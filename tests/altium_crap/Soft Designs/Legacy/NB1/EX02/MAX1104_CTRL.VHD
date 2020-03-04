------------------------------------------------------------
-- SubModule MAX1104_CTRL.Vhd
-- Created   14/11/2003 2:31:41 PM
-- Author    JR (MR)
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.std_logic_unsigned.all ;

entity MAX1104_CTRL is port
   (
     CLK           : in    std_logic;
     RESET         : in    std_logic;

     -- Parallel Interface

     DATAIN        : in    std_logic_vector(7 downto 0);  -- parallel data in
     DATAOUT       : out   std_logic_vector(7 downto 0);  -- parallel data out
     CS            : in    std_logic;                     -- Active High Chip Select
     RD            : in    std_logic;                     -- Active High Read
     WR            : in    std_logic;                     -- Active High Write
     A0            : in    std_logic;                     -- Function Select 0 = data, 1 = control
     BUSY          : out   std_logic;                     -- Active High Busy Signal

     -- SPI Interface

     SPI_DOUT      : out   std_logic;
     SPI_SCLK      : out   std_logic;
     SPI_nCS       : out   std_logic;
     SPI_DIN       : in    std_logic;

     -- TEST

     SPI_STATE     : out   std_logic_vector(3 downto 0)   -- For testing only
   );
end MAX1104_CTRL;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of MAX1104_CTRL is

-- Processor state
subtype  STATE_TYPE is std_logic_vector (3 downto 0);
signal   State        :  STATE_TYPE;
signal   Next_State   :  STATE_TYPE;
signal   Return1      :  STATE_TYPE;      --Return address for subroutine
signal   Return2      :  STATE_TYPE;

constant READY                           : STATE_TYPE := "0000";
constant WRITE_DATA                      : STATE_TYPE := "0001";
constant DELAY                           : STATE_TYPE := "0010";
constant WRITE_SPI                       : STATE_TYPE := "0011";
constant WRITE_SPI_END                   : STATE_TYPE := "0100";
constant WRITE_SPI_DONE                  : STATE_TYPE := "0101";
constant READ_DATA                       : STATE_TYPE := "0110";
constant SPI_DELAY                       : STATE_TYPE := "0111";
constant SPI_sendReceive                 : STATE_TYPE := "1000";
constant SPI_sendBit_out                 : STATE_TYPE := "1001";
constant SPI_sendBit_in                  : STATE_TYPE := "1010";
constant AWAKE                           : STATE_TYPE := "1011";


signal   countReg     : integer range 0 to 7;          -- Bit send/receive counter
signal   spiReg       : std_logic_vector (7 downto 0); -- Bit in/out shift register
signal   ctrlReg      : std_logic_vector (7 downto 0); -- Bit in/out shift register
--// SPI Control Register Structure
--// Bit 0        - Busy Flag (Read Only) 1 = Busy
--// Bit 1        - SPI CS                1 = Enable SPI device ( 0 to SPI hardware SPI_nCS)
--// Bit [7..2]   - SPI Clock Divisor     0 .. 63



signal   outReg       : std_logic_vector (7 downto 0); -- output register

signal   SCLK_buffer  : std_logic;                   -- Internal MEM_ClK signal;
signal   DOUT_buffer  : std_logic;                   -- Internal DATA signal;

--subtype  SCALER_TYPE is integer range 0 to 63;
--signal   SPI_Scaler      : SCALER_TYPE;  -- for generating SPI clock rate
signal   SPI_Scaler      : integer range 0 to 63 ;
--constant SPI_Rate        : SCALER_TYPE   := 0;   -- need to work out correct values

--signal   counter         : SCALER_TYPE;  -- General purpose counter (timeouts, delays etc)
signal counter : integer range 0 to 63 ;
--constant PRE_SPI_DELAY   : SCALER_TYPE   := 0;   -- need to work out correct values
--constant POST_SPI_DELAY  : SCALER_TYPE   := 0;

begin

    -- Concurrent
    DATAOUT        <= outReg;
    SPI_SCLK       <= SCLK_buffer;
    SPI_DOUT       <= DOUT_buffer;
    SPI_nCS        <= not ctrlReg(1);
    BUSY           <= ctrlReg(0);
    SPI_STATE      <= State;

    -- Main SPI controller FSM
    FSM: process (State,WR,RD,CS,A0,CLK)
    begin
      if (CLK'event and CLK='0') then

            if ((RD='1') and (CS='1')) then
                if (A0 = '0') then
                   outReg <= spiReg;
                else
                   outReg <= ctrlReg;
                end if;
            end if;

            case State is
             ---------------------------------------------------------------------------------
             --   Main program                                                              --
             ---------------------------------------------------------------------------------

             when READY=>
                   SCLK_buffer <= '0';
                   DOUT_buffer <= '0';
                   spiReg  <= "00000000";
                   ctrlReg <= "00000000";       -- initial value runs SPI at maximum rate
                   outReg  <= "00000000";
                   Next_State <= AWAKE;


             when AWAKE=>                        -- waiting for a Write Signal from the host
                  ctrlReg(0) <= '0';             -- Clear the BUSY Flag
                  if (WR = '1') then             -- we've got an input from host
                      if (CS = '1') then         -- device selected
                         Next_State <= READ_DATA;
                      end if;
                  end if;

             when READ_DATA=>
                   if (WR = '0') then       -- we've got an input from host
                      if (CS = '1') then          -- device selected
                         ctrlReg(0) <= '1';              -- Raise the BUSY Flag
                         Next_State <= DELAY;
                         if (A0 = '0') then               -- and it's data for the MAX1104 Codec DAC
                            spiReg <= DATAIN;
                            --counter <= SCALER_TYPE(ctrlReg(7 downto 2));  -- Wait this long before serial data is sent
                            counter <= CONV_INTEGER(ctrlReg(7 downto 2)) ;
                            Return1 <= WRITE_SPI;
                        else
                            ctrlReg(7 downto 1) <= DATAIN(7 downto 1);     -- it is ctrl data
                            --counter <= SCALER_TYPE(ctrlReg(7 downto 2));  -- Wait this long before serial data is sent
                            counter <= CONV_INTEGER(ctrlReg(7 downto 2)) ;
                            Return1 <= AWAKE;
                         end if;
                      end if;
                   end if;

             when WRITE_SPI =>          -- time to send the SPI data and clock, and receive as well
                  Return1 <= WRITE_SPI_DONE;
                  Next_State <= SPI_sendReceive;

             when WRITE_SPI_DONE =>           -- execute end delay
                  --counter <= SCALER_TYPE(ctrlReg(7 downto 2));  -- Wait this long before serial data is sent
                  counter <= CONV_INTEGER(ctrlReg(7 downto 2)) ;
                  Return1 <= WRITE_SPI_END;
                  Next_State <= DELAY;

             when WRITE_SPI_END =>      -- terminate busy signal and go back to awake
                   ctrlReg(0) <= '0';   -- clear Busy Flag
                   Next_State <= AWAKE;

             ---------------------------------------------------------------------------------
             --   Subroutine:  DELAY                                                        --
             --   Only Called from above states                                             --
             ---------------------------------------------------------------------------------
             when DELAY =>
                  if (counter=0) then
                     Next_State <= Return1;
                  else
                     counter <= counter-1;
                  end if;

             ---------------------------------------------------------------------------------
             --   Subroutine:  SPI_SendReceive (spiReg) : spiReg                            --
             ---------------------------------------------------------------------------------

             when SPI_sendReceive =>       -- Entry point for next byte
                  CountReg <= 0;
                  DOUT_buffer <= spiReg(7);
                  --SPI_Scaler <= SCALER_TYPE(ctrlReg(7 downto 2));
                  SPI_Scaler <= CONV_INTEGER(ctrlReg(7 downto 2)) ;
                  Return2 <= SPI_sendBit_out;
                  Next_State <= SPI_DELAY;

             when SPI_sendBit_out =>
                  spiReg <= spiReg(6 downto 0) & SPI_DIN;  -- Read last input
                  SCLK_buffer <= '1';
                  CountReg <= CountReg-1;
                  --SPI_Scaler <= SCALER_TYPE(ctrlReg(7 downto 2));
                  SPI_Scaler <= CONV_INTEGER(ctrlReg(7 downto 2)) ;
                  Return2 <= SPI_sendBit_in;
                  Next_State <= SPI_DELAY;

             when SPI_sendBit_in =>
                  DOUT_buffer <= spiReg(7);                  -- Write next output
                  SCLK_buffer <= '0';
                  if (countReg=0) then
                      Next_State <= Return1;                 -- Exit Subroutine
                  else
                      --SPI_Scaler <= SCALER_TYPE(ctrlReg(7 downto 2));
                      SPI_Scaler <= CONV_INTEGER(ctrlReg(7 downto 2)) ;
                      Return2 <= SPI_sendBit_out;
                      Next_State <= SPI_DELAY;
                  end if;

             ---------------------------------------------------------------------------------
             --   Subroutine:  SPI_DELAY                                                    --
             --   only called from SPI States                                               --
             ---------------------------------------------------------------------------------
             when SPI_DELAY =>
                  if (SPI_Scaler = 0) then
                      Next_State <= Return2;
                  else
                      SPI_Scaler <= SPI_Scaler-1;
                  end if;

             when OTHERS =>
                  Next_State <= READY;
             end case;
      end if;

    end process;

    clock_reset: process (RESET,CLK)
    begin
       if RESET = '1' then
          State <= READY;
       else
          if (CLK'event and CLK='1') then
             State <= Next_State;
          end if;
       end if;
    end process;


end structure;
------------------------------------------------------------
