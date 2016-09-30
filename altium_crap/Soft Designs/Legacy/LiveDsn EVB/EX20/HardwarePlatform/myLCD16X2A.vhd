--  freq MHz | enable_lcd | trigger_read | trigger_lcd | end_wait | end_wait_long
--*******************************************************************************
--  40       | 2          | 5            | 12          | 22       | 77            40 Spartan2E 300K slices
--  39       | 2          | 5            | 11          | 22       | 75
--  38       | 2          | 5            | 11          | 21       | 73
--  37       | 2          | 5            | 11          | 21       | 71
--  36       | 2          | 5            | 11          | 20       | 69
--  35       | 2          | 5            | 11          | 20       | 67
--  34       | 2          | 5            | 10          | 19       | 65
--  33       | 2          | 4            | 10          | 19       | 64
--  32       | 2          | 4            | 10          | 18       | 62
--  31       | 2          | 4            | 10          | 18       | 60
--  30       | 2          | 4            | 9           | 17       | 58
--  29       | 2          | 4            | 9           | 17       | 56
--  28       | 2          | 4            | 9           | 16       | 54
--  27       | 2          | 4            | 9           | 16       | 53
--  26       | 2          | 4            | 8           | 15       | 51
--  25       | 1          | 3            | 7           | 14       | 48
--  24       | 1          | 3            | 7           | 13       | 46
--  23       | 1          | 3            | 7           | 13       | 44
--  22       | 1          | 3            | 7           | 12       | 42
--  21       | 1          | 3            | 6           | 12       | 40
--  20       | 1          | 3            | 6           | 11       | 39
--  19       | 1          | 3            | 6           | 11       | 37
--  18       | 1          | 3            | 6           | 10       | 35
--  17       | 1          | 3            | 5           | 10       | 33
--  16       | 1          | 2            | 5           | 9        | 31
--  15       | 1          | 2            | 5           | 9        | 29
--  14       | 1          | 2            | 5           | 8        | 27
--  13       | 1          | 2            | 4           | 8        | 26
--  12       | 1          | 2            | 4           | 7        | 24
--  11       | 1          | 2            | 4           | 7        | 22
--  10       | 1          | 2            | 4           | 6        | 20
--  9        | 1          | 2            | 4           | 6        | 18
--  8        | 1          | 2            | 3           | 5        | 16
--  7        | 1          | 2            | 3           | 5        | 14
--  6        | 1          | 2            | 3           | 4        | 13
--  5        | 1          | 2            | 3           | 4        | 11
--  4        | 1          | 2            | 3           | 4        | 9
--  3        | 1          | 2            | 3           | 4        | 7
--  2        | 1          | 2            | 3           | 4        | 5
--  1        | 1          | 2            | 3           | 4        | 5             33 Spartan2E slices




library ieee;
use ieee.std_logic_1164.all;

package nanoPACKAGE is 

    constant enable_lcd              : integer := 2 ;
    constant trigger_read            : integer := 4 ;                                        
    constant trigger_lcd             : integer := 9 ;
    constant end_wait                : integer := 17 ;
    constant end_wait_long           : integer := 58 ;


    constant lcd_busy_flag           : std_logic := '1' ;
    constant lcd_not_busy_flag       : std_logic := '0' ;
    
    constant strobe_active : std_logic := '0' ;

    -- FSM state machine    
    type INIT_FSM_TYPE is ( FUNCTION_SET_STATE, 
                            DISPLAY_ON_OFF_STATE, 
                            DISPLAY_CLEAR_STATE, 
                            ENTRY_MODE_STATE, 
                            SET_ADDRESS_STATE, 
                            SHOW_CHAR_STATE, 
                            WAIT_FOR_LCD_STATE, 
                            WAIT_ON_DATA_STATE
                            );

    -- active signals level
    constant rst_active              : std_logic := '1' ;
    
    constant busy_active             : std_logic := '0' ;
    constant busy_inactive           : std_logic := '1' ;
    
    -- LINE MODE SETUP
    --  '0' 1-line mode
    --  '1' 2-line mode
    constant line_mode               : std_logic := '1' ;

    -- Display ON/OFF setup
    -- '0' display off
    -- '1' display on 
    constant display_on_off          : std_logic := '1' ;
    
    -- Cursor ON/OFF
    -- '0' cursor off
    -- '1' cursor on
    constant cursor_on_off           : std_logic := '0' ;
    
    -- Blink Setup
    -- '0' blink off
    -- '1' blink on
    constant blink_on_off            : std_logic := '0' ;
    
    -- Increment/Decrement Mode setup
    -- '0' decrement
    -- '1' increment
    constant inc_dec                 : std_logic := '0' ;
    
    -- Entire shift ON/OFF
    -- '0' entire shift off
    -- '1' entire shift on
    constant entire_shift_on_off     : std_logic := '0' ;
    
end nanoPACKAGE ;


-- ****************************************************************************
--      Design : NanoLCD ready to use component
--      Author : 
-- Date :
-- Version
-- Description :
--      The LCD driver. generates proper timings and takes control over the LCD.
--              
-- Revision :
-- ****************************************************************************
library ieee;
use ieee.std_logic_1164.all ;
library work ;
use work.nanopackage.all ;

entity LCD16X2A is
port (
    CLK         : in     std_logic ;
    RST         : in     std_logic ;
    -- data and address
    DATA        : in     std_logic_vector(7 downto 0) ;
    -- '0' means 1st line, '1' means 2nd line    
    LINE        : in     std_logic;
    -- '0' means 1st line, '1' means 2nd line
    ADDR        : in     std_logic_vector(3 downto 0) ;  -- position in the line - 0h means first position on the left side, Fh means first position on the right side
    -- strobe - makes data and addr valid
    STROBE      : in     std_logic ;
    -- busy flag
    BUSY        : out    std_logic ;

    -- The LCD control pins         
    LCD_DATA    : inout  std_logic_vector(7 downto 0) ;
    LCD_E       : out    std_logic ;
    LCD_RS      : out    std_logic ;
    LCD_RW      : out    std_logic 
);
end ;

architecture rtl of LCD16X2A is

    signal addr_int : std_logic_vector(3 downto 0) ;
    signal data_int : std_logic_vector(7 downto 0) ;
    signal line_int : std_logic ;

    signal fsm_state : INIT_FSM_TYPE ;
        
    signal from_function_set_state   : std_logic ;
    signal from_display_on_off_state : std_logic ;
    signal from_display_clear_state  : std_logic ;
    signal from_entry_mode_state     : std_logic ;
    signal from_set_address_state    : std_logic ;
    signal from_show_char_state      : std_logic ;
                                  
    signal lcd_busy                  : std_logic ;
        
        
begin


INIT_FSM : process(clk, rst)

variable cnt : integer range 0 to end_wait_long ;

begin
        -- asynchronous reset logic
    if rst = rst_active then
       fsm_state                       <= FUNCTION_SET_STATE ;
       busy                            <= busy_active ;
       LCD_E                           <= '0' ;
       LCD_RS                          <= '0' ;
       LCD_RW                          <= '0' ;
       LCD_data                        <= (others => 'Z');
       cnt                             := 0 ;
       from_function_set_state         <= '0' ;
       from_display_on_off_state       <= '0' ;
       from_display_clear_state        <= '0' ;
       from_entry_mode_state           <= '0' ;
       from_set_address_state          <= '0' ;
       from_show_char_state            <= '0' ;
       lcd_busy                        <= lcd_not_busy_flag ;
                
        -- synchronous logic
    elsif clk'event and clk = '1' then
       case fsm_state is
          when FUNCTION_SET_STATE   =>
                                        LCD_RS                                           <= '0' ;
                                        LCD_RW                                           <= '0' ;
                                        LCD_data                                         <= "0011" & line_mode & display_on_off & "--" ;
                                                                        
                                        if cnt = enable_lcd then
                                           LCD_E                                         <= '1' ;
                                           fsm_state                                     <= FUNCTION_SET_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = trigger_lcd then
                                           LCD_E                                         <= '0' ;
                                           fsm_state                                     <= FUNCTION_SET_STATE ;
                                           cnt                                           := cnt + 1  ;
                                        elsif cnt = end_wait then                        
                                           fsm_state                                     <= WAIT_FOR_LCD_STATE ;
                                           from_function_set_state                       <= '1' ;
                                           cnt                                           := 0 ;
                                        else
                                           cnt                                           := cnt + 1 ;
                                           fsm_state                                     <= FUNCTION_SET_STATE ;
                                        end if ;
          when DISPLAY_ON_OFF_STATE =>
                                        LCD_RS                                           <= '0' ;
                                        LCD_RW                                           <= '0' ;
                                        LCD_data                                         <= "00001" & display_on_off & cursor_on_off & blink_on_off ;
                                                                    
                                        if cnt = enable_lcd then
                                           LCD_E                                         <= '1' ;
                                           fsm_state                                     <= DISPLAY_ON_OFF_STATE ;
                                           cnt                                                                             := cnt + 1 ;
                                        elsif cnt = trigger_lcd then
                                           LCD_E                                         <= '0' ;
                                           fsm_state                                     <= DISPLAY_ON_OFF_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = end_wait then
                                           fsm_state                                     <= WAIT_FOR_LCD_STATE ;
                                           from_display_on_off_state                     <= '1' ;
                                           cnt                                           := 0 ;
                                        else
                                           cnt                                           := cnt + 1 ;
                                           fsm_state                                     <= DISPLAY_ON_OFF_STATE ;
                                        end if ;
                        
          when DISPLAY_CLEAR_STATE  =>
                                        LCD_RS                                           <= '0' ;
                                        LCD_RW                                           <= '0' ;
                                        LCD_data                                         <= "00000001" ;
                                        
                                        if cnt = enable_lcd then
                                           LCD_E                                         <= '1' ;
                                           fsm_state                                     <= DISPLAY_CLEAR_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = trigger_lcd then
                                           LCD_E                                         <= '0' ;
                                           fsm_state                                     <= DISPLAY_CLEAR_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = end_wait then
                                           fsm_state                                     <= WAIT_FOR_LCD_STATE ;
                                           from_display_clear_state                      <= '1' ;
                                           cnt                                           := 0 ;
                                        else                                             
                                           cnt                                           := cnt + 1 ;
                                           fsm_state                                     <= DISPLAY_CLEAR_STATE ;
                                        end if ;
                        
          when ENTRY_MODE_STATE     =>
                                        LCD_RS                                           <= '0' ;
                                        LCD_RW                                           <= '0' ;
                                        LCD_data                                         <= "000001" & inc_dec & entire_shift_on_off ;
                                                                              
                                        if cnt = enable_lcd then                         
                                           LCD_E                                         <= '1' ;
                                           fsm_state                                     <= ENTRY_MODE_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = trigger_lcd then
                                           LCD_E                                         <= '0' ;
                                           fsm_state                                     <= ENTRY_MODE_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = end_wait then
                                           fsm_state                                     <= WAIT_FOR_LCD_STATE ;
                                           from_entry_mode_state                         <= '1' ;
                                           cnt                                           := 0 ;
                                        else                                             
                                           cnt                                           := cnt + 1 ;
                                           fsm_state                                     <= ENTRY_MODE_STATE ;
                                        end if ;
          when WAIT_ON_DATA_STATE   =>
                                        if strobe = strobe_active then
                                           fsm_state                                     <= SET_ADDRESS_STATE ;
                                           busy                                          <= busy_active ;
                                        else
                                           fsm_state                                     <= WAIT_ON_DATA_STATE ;
                                           busy                                          <= busy_inactive ;
                                        end if ;
                        
          when SET_ADDRESS_STATE    =>
                                        LCD_RS                                           <= '0' ;
                                        LCD_RW                                           <= '0' ;
                                        LCD_data                                         <= '1'& line_int & "00" & addr_int ;
                                                                                      
                                        if cnt = enable_lcd then
                                           LCD_E                                         <= '1' ;
                                           fsm_state                                     <= SET_ADDRESS_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = trigger_lcd then
                                           LCD_E                                         <= '0' ;
                                           fsm_state                                     <= SET_ADDRESS_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = end_wait then
                                           fsm_state                                     <= WAIT_FOR_LCD_STATE ;
                                           from_set_address_state                        <= '1' ;
                                           cnt                                           := 0 ;
                                        else
                                           cnt                                           := cnt + 1 ;
                                           fsm_state                                     <= SET_ADDRESS_STATE ;
                                        end if ;
                                                                                                        
                        
          when SHOW_CHAR_STATE      =>
                                        LCD_RS                                           <= '1' ;
                                        LCD_RW                                           <= '0' ;
                                        LCD_data                                         <= data_int ;
                                        
                                        if cnt = enable_lcd then
                                           LCD_E                                         <= '1' ;
                                           fsm_state                                     <= SHOW_CHAR_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = trigger_lcd then
                                           LCD_E                                         <= '0' ;
                                           fsm_state                                     <= SHOW_CHAR_STATE ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = end_wait then
                                           fsm_state                                     <= WAIT_FOR_LCD_STATE ;
                                           from_show_char_state                          <= '1' ;
                                           cnt                                           := 0 ;
                                        else
                                           cnt                                           := cnt + 1 ;
                                           fsm_state                                     <= SHOW_CHAR_STATE ;
                                        end if ;
                                                                                                         
                        
          when WAIT_FOR_LCD_STATE   =>
                                        LCD_RS                                           <= '0' ;
                                        LCD_RW                                           <= '1' ;
                                        
                                        if cnt = enable_lcd then
                                           LCD_E                                         <= '1' ;
                                           cnt                                           := cnt + 1 ;
                                           fsm_state                                     <= WAIT_FOR_LCD_STATE ;
                                        elsif cnt = trigger_read then
                                           lcd_busy                                      <= LCD_data(7) ;
                                           cnt                                           := cnt + 1 ;
                                           fsm_state                                     <= WAIT_FOR_LCD_STATE ;
                                        elsif cnt = trigger_lcd then
                                           LCD_E                                         <= '0' ;
                                           cnt                                           := cnt + 1 ;
                                        elsif cnt = end_wait_long then                   
                                           cnt                                           := 0 ;
                                           if lcd_busy = lcd_busy_flag then
                                              fsm_state                                  <= WAIT_FOR_LCD_STATE ;
                                           else
                                              if from_function_set_state = '1' then
                                                 fsm_state                               <= DISPLAY_ON_OFF_STATE ;
                                                 from_function_set_state <= '0' ;
                                              elsif from_display_on_off_state = '1' then 
                                                 fsm_state                               <= DISPLAY_CLEAR_STATE ;
                                                 from_display_on_off_state       <= '0' ;
                                              elsif from_display_clear_state = '1' then
                                                 fsm_state                               <= ENTRY_MODE_STATE ;
                                                 from_display_clear_state        <= '0' ;
                                              elsif from_entry_mode_state = '1' then
                                                 fsm_state                               <= WAIT_ON_DATA_STATE ;
                                                 from_entry_mode_state           <= '0' ;
                                              elsif from_set_address_state = '1' then
                                                 fsm_state                               <= SHOW_CHAR_STATE ;
                                                 from_set_address_state          <= '0' ;
                                              elsif from_show_char_state = '1' then
                                                 fsm_state                               <= WAIT_ON_DATA_STATE ;
                                                 from_show_char_state                    <= '0' ;
                                              end if ;
                                           end if ;
                                        else
                                           cnt                                           := cnt + 1 ;
                                           fsm_state                                     <= WAIT_FOR_LCD_STATE ;
                                        end if ;

          when others =>
                                        null ;
       end case ;
    end if ;
end process ;


LATCH_ADDR : process (strobe)
begin
if strobe = strobe_active then
    addr_int <= addr;
end if ;
end process;

LATCH_DATA : process(strobe)
begin
if strobe = strobe_active then
  data_int <= data ;
end if ;
end process ;

process (strobe)
begin
if strobe = strobe_active then
line_int <= line ;
end if ;
end process;
end rtl ;
