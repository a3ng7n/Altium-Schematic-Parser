------------------------------------------------------------
-- SubModule TX_DATA_GEN.VHD        
-- Copyright (c) 2004 Altium
--
-- Uses: RIO_COMMON.Vhd
--
-- transmits sync characters until START is active
-- if start is high, a string of header characters followed by
-- a header end character is transmitted to indicate the start
-- of the data sequence and allow the receiver to synchronise.
-- data consists of 64k of integers couting up from 0
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

use work.RIO_COMMON.all;

entity TX_DATA_GEN is port
   (
     TXD_OUT             : out   std_logic_vector(15 downto 0);
     TX_CLK              : in    std_logic;
     SYNC_ACTIVE         : out   std_logic;
     SYNC_CHAR_IN        : in    std_logic_vector(15 downto 0);
     START               : in    std_logic;
     LED_OUT             : out   std_logic_vector(7 downto 0);
     DONE                : out   std_logic
   );
end entity;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of TX_DATA_GEN is

-- Component Declarations

-- Signal Declarations
subtype state_type is std_logic_vector(2 downto 0);
constant S_idle:     state_type := "001";
constant S_txheaderbody: state_type := "011";
constant S_txheaderend: state_type := "010";
constant S_transmit: state_type := "110";
constant S_done:     state_type := "101";
signal STATE, NEXT_STATE: state_type;

-- data header signals
signal S_HEADER_COUNT: std_logic_vector(7 downto 0);
-- address counter signals
signal CURRENT_COUNT :std_logic_vector(15 downto 0);

signal DONE_STATE : std_logic;
signal OUTPUT_DATA : std_logic_vector(15 downto 0);

begin

  FSM: process is
  begin
    wait until rising_edge(TX_CLK);
    case state is
      when S_idle =>
         DONE_STATE <= '1';                                -- set done flag
         SYNC_ACTIVE <= '1';
         CURRENT_COUNT <= (others => '0');                 -- keep counter reset
         OUTPUT_DATA <= SYNC_CHAR_IN;                      -- when idle transmit sync characters
         if(START = '1')  then                             -- start counting
--           STATE <= S_transmit;                          -- start transmitting
           SYNC_ACTIVE <= '0';                             -- remove sync flag
           STATE <= S_txheaderbody;                        -- transmit header body characters
         end if;
      when S_txheaderbody =>
         CURRENT_COUNT <= CURRENT_COUNT + 1;
         SYNC_ACTIVE <= '0';                               -- clear sync input
         DONE_STATE  <= '0';                               -- clear done flag while counting
         OUTPUT_DATA <= C_HEADERBODY_INT;                  -- output header character
         if(CURRENT_COUNT >= C_NUMBEROF_HEADERCHARS)       -- transmitted enough header body characters?
         then
           CURRENT_COUNT <= (others => '0');               -- counter reset
           STATE <= S_txheaderend;
         end if;
      when S_txheaderend  =>
         SYNC_ACTIVE <= '0';                               -- clear sync input
         DONE_STATE  <= '0';                               -- clear done flag while counting
         CURRENT_COUNT <= (others => '0');                 -- reset counter
         OUTPUT_DATA <= C_HEADEREND_INT;                   -- output single end header character
         STATE <= S_transmit;
      when S_transmit =>
         CURRENT_COUNT <= CURRENT_COUNT + 1;
         SYNC_ACTIVE <= '0';                               -- clear sync input
         DONE_STATE  <= '0';                               -- clear done flag while counting
         OUTPUT_DATA <= CURRENT_COUNT;                     -- output counter value
         if(CURRENT_COUNT = x"FFFF")                       -- done transmitting?
         then
           STATE <= S_done;
           DONE_STATE <= '1';                              -- set done flag
         end if;
      when S_done =>
         SYNC_ACTIVE <= '1';
         OUTPUT_DATA <= SYNC_CHAR_IN;                      -- when idle transmit sync characters
         DONE_STATE <= '1';                                -- set done flag
         if(START = '0') then                              -- do not autorepeat
           STATE <= S_IDLE;                                -- go to idle
         end if;
       when others =>
         STATE <= S_IDLE;
    end case;
  end process FSM;

DONE <= DONE_STATE;

TXD_OUT <= OUTPUT_DATA;

LED_OUT(2 downto 0) <= STATE;
LED_OUT(4)          <= DONE_STATE;
LED_OUT(5)          <= START;
LED_OUT(6)          <= '0';
LED_OUT(7)          <= CURRENT_COUNT(15);

end structure;
------------------------------------------------------------



