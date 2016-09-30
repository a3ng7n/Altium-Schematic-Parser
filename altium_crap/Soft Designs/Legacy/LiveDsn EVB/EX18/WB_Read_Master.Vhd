--------------------------------------------------------------------------------
-- SubModule WB_READ_MASTER
-- Created   22/11/2004 3:36:38 PM
--------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.all;

entity WB_READ_MASTER is port
   (

     CLK_I           : in    std_logic;
     RST_I           : in    std_logic;
     RD_I            : in    std_logic;
     ADR_I           : in    std_logic_vector(31 downto 0);

     WB_ACK_I        : in    std_logic;
     WB_DAT_I        : in    std_logic_vector(31 downto 0);

     WB_STB_O        : out   std_logic;
     WB_CYC_O        : out   std_logic;
     WB_ADR_O        : out   std_logic_vector(20 downto 0);      -- was 31 downto 0
     WB_DAT_O        : out   std_logic_vector(31 downto 0);
     WB_SEL_O        : out   std_logic_vector(3 downto 0);
     WB_WE_O         : out   std_logic;
     WB_CLK_O        : out   std_logic;
     WB_RST_O        : out   std_logic;

     BUSY            : out   std_logic;
     DAT32_O         : out   std_logic_vector(31 downto 0);
     DAT8_O          : out   std_logic_vector(7 downto 0)
   );
end WB_READ_MASTER;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
architecture structure of WB_READ_MASTER is

constant TIMEOUT_ACK : integer := 255;                           -- maximum number of clock cycles
                                                                 -- spent waiting for ACK
type SEQ_STATE_TYPE is (STOP, ARM, START, WAIT_ACK);

signal SEQ_STATE: SEQ_STATE_TYPE;

signal AckTimeOut : std_logic_vector(7 downto 0);

begin

-- static connections

WB_ADR_O <= ADR_I(20 downto 0) AND "111111111111111111100" ;     -- X"FFFFFFFC";
WB_RST_O <= RST_I;
WB_CLK_O <= CLK_I;
WB_WE_O  <= '0';                                                 -- 'WE' is always inactive since we never write
WB_DAT_O <= (others => '0');                                     -- we never write, so write data is irrelevant
WB_SEL_O <= (others => '1');                                     -- read full 32 bits

-- indicates read is in progress
BUSY <= '0' when (SEQ_STATE = ARM) OR (SEQ_STATE = STOP) else '1';

-- for debugging only
process (SEQ_STATE) is
begin
  case SEQ_STATE is
    when STOP =>
      DAT32_O <= X"00000001";
    when ARM  =>
      DAT32_O <= X"00000002";
    when START =>
      DAT32_O <= X"00000003";
    when WAIT_ACK =>
      DAT32_O <= X"00000004";
    when others =>
      DAT32_O <= X"00000005";
  end case;
end process;

-- read state machine
-- waits for rising edge on RD_I
  SEQUENCER_FSM: process (Clk_I, RD_I, RST_I) is
  begin
    if(RST_I = '1') then
      SEQ_STATE <= STOP;
    else
    if(rising_edge(CLK_I)) then
      case SEQ_STATE is
        when STOP =>
          WB_CYC_O <= '0';
          WB_STB_O <= '0';
          if(RD_I = '0') then
            SEQ_STATE <= ARM;
          end if;
        when ARM   =>
          WB_CYC_O <= '0';
          WB_STB_O <= '0';
          if(RD_I = '1') then
            SEQ_STATE <= START;
          end if;
        when START =>
          WB_CYC_O <= '1';
          WB_STB_O <= '1';
          AckTimeOut <= X"00";
          SEQ_STATE <= WAIT_ACK;
        when WAIT_ACK =>
          AckTimeOut <= AckTimeOut + X"01";
          if (WB_ACK_I = '1') then
            case ADR_I(1 downto 0) is
              when "11" =>
                DAT8_O <= WB_DAT_I(7 downto 0);
              when "10" =>
                DAT8_O <= WB_DAT_I(15 downto 8);
              when "01" =>
                DAT8_O <= WB_DAT_I(23 downto 16);
              when "00" =>
                DAT8_O <= WB_DAT_I(31 downto 24);
              when others => NULL;
            end case;
            WB_CYC_O <= '0';
            WB_STB_O <= '0';
            SEQ_STATE <= STOP;
          elsif (AckTimeOut >= TIMEOUT_ACK) then
              SEQ_STATE <= STOP;
          end if;

        when others =>
          SEQ_STATE <= STOP;
      end case;
    end if;
    end if;
  end process SEQUENCER_FSM;


end structure;
--------------------------------------------------------------------------------
