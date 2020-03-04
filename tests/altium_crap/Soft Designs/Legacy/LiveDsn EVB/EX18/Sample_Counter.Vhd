--------------------------------------------------------------------------------
-- SubModule SAMPLE_COUNTER
-- Created   24/11/2004 10:16:24 AM
--------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.all;

entity SAMPLE_COUNTER is port
   (
     RD_O                    : out   std_logic;
     ADR_O                   : out   std_logic_vector(31 downto 0);
     CLK_I                   : in    std_logic;
     L_START_ADR_I           : in    std_logic_vector(31 downto 0);
     L_END_ADR_I             : in    std_logic_vector(31 downto 0);
     L_START_REPEAT_I        : in    std_logic_vector(31 downto 0);
     L_END_REPEAT_I          : in    std_logic_vector(31 downto 0);
     CONTROL_I               : in    std_logic_vector(31 downto 0);
     START_I                 : in    std_logic;
     CLK_SAMPLE              : in    std_logic
   );
end SAMPLE_COUNTER;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

------------------------------------------------------------
architecture structure of SAMPLE_COUNTER is

type SEQ_STATE_TYPE is (STOP, RUN_START, RUN_1ST, RPT_RUN);

signal SEQ_STATE : SEQ_STATE_TYPE;

signal DoStart : std_logic;
signal OldStart: std_logic;

signal ADDR_CNT : std_logic_vector(31 downto 0);

begin

  SEQUENCER_FSM: process (CLK_SAMPLE, DoStart, L_END_ADR_I, L_START_ADR_I) is
  begin
    if(rising_edge(CLK_SAMPLE)) then
      case SEQ_STATE is
        when STOP =>
          ADDR_CNT <= L_START_ADR_I;                                            -- restart signal
          if(DoStart = '1') then
            SEQ_STATE <= RUN_START;
          end if;
        when RUN_START =>
          ADDR_CNT <= L_START_ADR_I;
          SEQ_STATE <= RUN_1ST;
        when RUN_1ST =>
          if(DoStart = '1') then                                                -- restart signal
            SEQ_STATE <= RUN_START;
          elsif  (L_END_REPEAT_I >= 1) AND (ADDR_CNT >= L_START_REPEAT_I) then  -- repeat active?
            SEQ_STATE <= RPT_RUN;
          elsif (ADDR_CNT >= L_END_ADR_I) then                                  -- end of sample
            SEQ_STATE <= STOP;
          elsif (ADDR_CNT < L_START_ADR_I) then
            ADDR_CNT <= L_START_ADR_I;
          else
            ADDR_CNT <= ADDR_CNT + "1";                                         -- increment address counter
          end if;
        when RPT_RUN =>
          if(DoStart = '1') then                                                -- restart signal
            SEQ_STATE <= RUN_START;
          elsif (ADDR_CNT >= L_END_REPEAT_I) then                               -- end of repeat section?
            ADDR_CNT <= L_START_REPEAT_I;
          else
            ADDR_CNT <= ADDR_CNT + "1";                                         -- increment address counter
          end if;
        when others =>
          SEQ_STATE <= STOP;
       end case;
    end if;
  end process SEQUENCER_FSM;

  StartFlagHandler: process (CLK_I, START_I)  is begin
     if(rising_edge(CLK_I)) then
       if(SEQ_STATE = RUN_START) then
         DoStart <= '0';
       end if;
       if(START_I /= OldStart) then
         OldStart <= START_I;
         if(START_I = '0') then                                                 -- Data gets latched on rising edge, so we start opn falling edge
           DoStart <= '1';
         end if;
       end if;
     end if;
  end process  StartFlagHandler;

ADR_O <= ADDR_CNT;
RD_O  <= CLK_SAMPLE;

end structure;
------------------------------------------------------------













