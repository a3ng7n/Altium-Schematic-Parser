------------------------------------------------------------
-- SubModule RECEIVER.VHD
--
-- Uses: RIO_COMMON.Vhd
--
-- byte aligns and synchronises on the incoming data stream
-- then writes incoming data to memory starting ad address 0
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

use work.RIO_COMMON.all;                                        -- common constants for transmitter and receiver module

entity RECEIVER is port
   (
     IS_COMMA           : in    std_logic_vector(1 downto 0);   -- used for byte alignment
     DATA_IN            : in    std_logic_vector(15 downto 0);  -- received data, may need byte alignment
     LOSS_OF_SYNC       : in    std_logic_vector(1 downto 0);   -- currently unused
     COMMA_DET          : in    std_logic;                      -- not used
     REC_CLK            : in    std_logic;
     START_IN           : in    std_logic;                      -- start receive block input

     DATA_OUT           : out   std_logic_vector(15 downto 0);  -- SRAM ADDRESS BUS
     ADDR_OUT           : out   std_logic_vector(15 downto 0);  -- SRAM DATA BUS
     WR_OUT             : out   std_logic;                      -- SRAM WRITE ENABLE SIGNAL
     DONE_OUT           : out   std_logic;                      -- DONE FLAG OUTPUT
     LED_OUT            : out   std_logic_vector(7 downto 0)    -- LED port for debugging
   );
end RECEIVER;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of RECEIVER is

-- Component Declarations

-- Signal Declarations
signal S_SWAP_BYTES : std_logic;                                -- this is true if input bytes must be swapped for output word
signal S_RX_DATA    : std_logic_vector(15 downto 0);            -- correctly byteswapped data

--- FSM Signals
subtype state_type is std_logic_vector(2 downto 0);
constant S_idle:        state_type := "001";
constant S_rxheader:    state_type := "010";
constant S_receive:     state_type := "011";
constant S_done:        state_type := "100";
--signal STATE, NEXT_STATE: state_type;
signal STATE: state_type;

-- Address counter signals
signal CURRENT_COUNT, NEXT_COUNT :std_logic_vector(15 downto 0);
--signal S_RESET_COUNTER_FLAG: std_logic;
--signal S_RESET_COUNTER:   std_logic;

-- SRAM output signals
signal S_ADDR_OUT : std_logic_vector(15 downto 0);
signal S_DATA_OUT : std_logic_vector(15 downto 0);
signal S_WR_OUT   : std_logic;
signal S_DONE_OUT : std_logic;

-- timeout counters
signal S_HEADER_COUNTER : std_logic_vector(7 downto 0);         -- counter for header characters  received in a row
signal S_HEADER_TIMEOUT_COUNTER : std_logic_vector(7 downto 0); -- counter for header characters  received in a row
signal S_TIMEOUT_COUNTER: std_logic_vector(7 downto 0);         -- timeout counter for header characters

-- Byte alignment storage register
signal S_ALIGNMENT_REG : std_logic_vector(31 downto 0);         -- intermediate storage for incoming data

-- reads next data byte from Rocket I/O port, byte aligns it
-- and sticks it into S_DATA_OUT
procedure GetNext_S_DATA_OUT(signal S_ALIGNMENT_REG : in std_logic_vector(31 downto 0);
                             signal S_SWAP_BYTES : in std_logic;
                             signal S_DATA_OUT : out std_logic_vector(15 downto 0)
                             ) is
begin
  if(S_SWAP_BYTES = '0') then
      S_DATA_OUT <= S_ALIGNMENT_REG(15 downto 0);
    else
      S_DATA_OUT <= S_ALIGNMENT_REG(23 downto 8);
    end if;
end procedure GetNext_S_DATA_OUT;


begin

-- rs flip-flop to remember which order the bytes are in the data word
BYTESWAP_RS_FF: process(IS_COMMA, S_SWAP_BYTES) is
begin
  if(IS_COMMA(0) = '1') then
    S_SWAP_BYTES <= '0';
  elsif(IS_COMMA(1) = '1') then
    S_SWAP_BYTES <= '1';
  else
    S_SWAP_BYTES <= S_SWAP_BYTES;
  end if;
end process BYTESWAP_RS_FF;

-- receiver state machine
  FSM: process is
  begin
    wait until rising_edge(REC_CLK);                            -- REC_CLK = '0' and REC_CLK'event;
    S_ADDR_OUT <= S_ADDR_OUT + 1;                               -- increment address counter
    case state is
      when S_idle =>
        S_WR_OUT   <= '1';                                      -- write line inactive
        S_DONE_OUT <= '0';                                      -- clear done flag
        S_ALIGNMENT_REG <= (others => '0' );                    -- clear alignment register
        if(IS_COMMA = "00") then                                -- wait for end of comma character stream
          S_HEADER_COUNTER <= (others => '0');                  -- start header character timeout counter
          S_HEADER_TIMEOUT_COUNTER <= (others => '0');
          S_ADDR_OUT <= C_RAM_START_ADDRESS;                    -- reset SRAM address counter
          S_ALIGNMENT_REG <= S_ALIGNMENT_REG(15 downto 0) & DATA_IN;  -- byte align data stream
          GetNext_S_DATA_OUT(S_ALIGNMENT_REG, S_SWAP_BYTES, S_DATA_OUT); -- read next value

          STATE <= S_rxheader;
--          STATE <= S_receive;                                 -- for debugging header recognition code
        else
          STATE <= S_idle;
        end if;
      when S_rxheader =>                                        -- wait for header character sequence

        S_ALIGNMENT_REG <= S_ALIGNMENT_REG(15 downto 0) & DATA_IN;
         GetNext_S_DATA_OUT(S_ALIGNMENT_REG, S_SWAP_BYTES, S_DATA_OUT); -- read next value

        S_WR_OUT   <= '0';                                      -- write line inactive
        S_DONE_OUT <= '0';                                      -- clear done flag
        S_HEADER_TIMEOUT_COUNTER <= S_HEADER_TIMEOUT_COUNTER + 1; -- increment timeout counter
        if (S_DATA_OUT = C_HEADERBODY_INT) then                 -- count successive header characters
          S_HEADER_COUNTER <= S_HEADER_COUNTER + 1;
        else
           S_HEADER_COUNTER <= (others => '0');
        end if;
        if(S_HEADER_TIMEOUT_COUNTER >= (5 * C_NUMBEROF_HEADERCHARS))  then  -- timeout?
          STATE <= S_done;                                                  -- go to done
        elsif (S_DATA_OUT = C_HEADEREND_INT)  and (S_HEADER_COUNTER >= (C_NUMBEROF_HEADERCHARS / 2)) then -- found end of header
          S_ADDR_OUT <= (C_RAM_START_ADDRESS);                  --write data from start address onwards
          S_WR_OUT <= '0';                                      -- enable write
          STATE <= S_receive;                                   -- start receiving characters
        else
          STATE <= S_rxheader;
        end if;
      when S_receive =>
         S_WR_OUT <= '0';                                       -- start writing the 1st word at address 0
         S_DONE_OUT <= '0';

         S_ALIGNMENT_REG <= S_ALIGNMENT_REG(15 downto 0) & DATA_IN;
         GetNext_S_DATA_OUT(S_ALIGNMENT_REG, S_SWAP_BYTES, S_DATA_OUT); -- read next value

         if(S_ADDR_OUT = C_RAM_END_ADDRESS)                     -- or not (IS_COMMA = "00") -- memory full or comma character received?
         then
           STATE <= S_done;
           S_WR_OUT   <= '1';                                   -- write line inactive
         end if;
      when S_done =>
         S_WR_OUT    <= '1';                                    -- write line inactive
         S_DONE_OUT  <= '1';                                    -- raise done flag
         if(START_IN = '1') then
            STATE <= S_IDLE;                                    -- go to idle
         end if;
      when others =>
         STATE <= S_done;
    end case;
  end process FSM;

-- Output Assignments
  S_RX_DATA <= S_DATA_OUT;
  DONE_OUT  <= S_DONE_OUT;
  DATA_OUT  <= S_RX_DATA;
  WR_OUT    <= S_WR_OUT or (not(REC_CLK));
  ADDR_OUT  <= S_ADDR_OUT;
  LED_OUT   <= REC_CLK & (S_WR_OUT or (not(REC_CLK)))& IS_COMMA &  S_DONE_OUT  & STATE;
end structure;
------------------------------------------------------------
