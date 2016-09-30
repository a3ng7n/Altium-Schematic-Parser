-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
entity max1104_codecdriver_dac is port
   (
     clk        : in  std_logic;
     rst        : in  std_logic;
     d          : in  std_logic_vector(7 downto 0);
     codec_dout : out std_logic;
     codec_sclk : out std_logic;
     codec_ncs  : out std_logic;
     codec_din  : in  std_logic
   );
end max1104_codecdriver_dac;
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
architecture rtl of max1104_codecdriver_dac is

-------------------------------------------------------------------------------
subtype  state_type is std_logic_vector (2 downto 0);
signal   state   :  state_type;

constant configure                       : state_type := "000";
constant configure_start                 : state_type := "001";
constant configure_sendconfigurationbyte : state_type := "010";
constant loadspireg                      : state_type := "011";
constant processbyte                     : state_type := "100";
constant spi_sendreceive                 : state_type := "101";
constant spi_sendbit_out                 : state_type := "110";
constant spi_sendbit_in                  : state_type := "111";
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
subtype  byte is std_logic_vector (7 downto 0);
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
constant caudio_start                   : byte := x"80";   --start of control byte
constant caudio_dac_addressed           : byte := x"40";   --a1  dac_addressed - the control byte configures the dac
constant caudio_adc_addressed           : byte := x"20";   --a0  adc_addressed - the control byte configures the adc
constant caudio_adc_inputc1             : byte := x"10";   --c1  adc_input to vdd/2 to measure supply voltage (max1102-max1103 only)
constant caudio_continuousconversion    : byte := x"08";   --c0  mode_continuousconversion
constant caudio_enablereferencevoltage  : byte := x"04";   --e2  enablereferencevoltage - don't care for max1104 so zero
constant caudio_adc_enable              : byte := x"02";   --e1
constant caudio_dac_enable              : byte := x"01";   --e0
constant caudio_continuousdac           : byte := caudio_start                or
                                                  caudio_dac_addressed        or
                                                  caudio_continuousconversion or
                                                  caudio_dac_enable;
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
signal countreg          : integer range 0 to 7;            -- bit send/receive counter
signal spireg            : std_logic_vector (7 downto 0);   -- bit in/out shift register

signal codec_sclk_buffer : std_logic;                       -- internal codec_clk signal;
signal codec_din_buffer  : std_logic;                       -- internal data in signal;
signal codec_dout_buffer : std_logic;                       -- internal data out signal;
signal codec_cs_buffer   : std_logic;
signal d_buffer          : std_logic_vector (7 downto 0);   -- buffered 8-bit input
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
begin
    codec_sclk       <= codec_sclk_buffer;
    codec_dout       <= codec_dout_buffer;
    codec_din_buffer <= codec_din;
    codec_ncs        <= not codec_cs_buffer;

    -- main state machine to control spi
    fsm: process (state,rst,clk)
    begin
       if rst = '1' then
          state             <= configure;
          spireg            <= "00000000";
          countreg          <= 0;
          codec_cs_buffer   <= '0';
          codec_dout_buffer <= '0';
          codec_sclk_buffer <= '0';
       elsif (clk'event) and (clk = '1') then
           case state is
               when configure =>
                    codec_cs_buffer <= '0';
                    state            <= configure_start;

               when configure_start=>
                    codec_cs_buffer <= '1';
                    state           <= configure_sendconfigurationbyte;

               when configure_sendconfigurationbyte=>
                    spireg    <= caudio_continuousdac;
                    state     <= spi_sendreceive;

               when loadspireg =>
                    spireg    <= d;
                    state     <= processbyte;

               when processbyte =>
                    state     <= spi_sendreceive;

               -------------------------------------------------
               -- subroutine:  spi_sendreceive (spireg) : spireg
               -- postcondition codec_sclk_buffer = '0'
               -------------------------------------------------
               when spi_sendreceive =>
                    countreg          <= 0;
                    codec_dout_buffer <= spireg(7);
                    state             <= spi_sendbit_out;                -- send msb to dout

               when spi_sendbit_out =>
                    spireg            <= spireg(6 downto 0) & codec_din;  -- shift spireg left and put din into lsb
                    codec_sclk_buffer <= '1';
                    countreg          <= countreg-1;
                    state             <= spi_sendbit_in;

               when spi_sendbit_in =>
                    codec_dout_buffer <= spireg(7);                      -- next bit ready
                    codec_sclk_buffer <= '0';
                    if countreg = 0
                       then state <= loadspireg;                         -- exit subroutine
                       else state <= spi_sendbit_out;
                    end if;
               -------------------------------------------------

               when others =>
                   state <= configure;
           end case;
       end if;
    end process;
end rtl;
-------------------------------------------------------------------------------

