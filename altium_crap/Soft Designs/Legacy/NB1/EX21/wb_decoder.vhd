library ieee;
use ieee.std_logic_1164.all ;

entity wb_decoder is
port (
   -- master interface
   wb_adr : in  std_logic_vector(7 downto 0) ;
   wb_we  : in  std_logic ;
   wb_stb : in  std_logic ;
   wb_cyc : in  std_logic ;
   wb_dat : out std_logic_vector(7 downto 0) ;
   wb_ack : out std_logic ;

   -- lcd interface
   lcd_dat : in  std_logic_vector(7 downto 0) ;
   lcd_adr : out std_logic ;
   lcd_cyc : out std_logic ;
   lcd_stb : out std_logic ;
   lcd_we  : out std_logic ;
   lcd_ack : in  std_logic ;
   -- ps2 interface
   ps2_dat : in  std_logic_vector(7 downto 0) ;
   ps2_adr : out std_logic ;
   ps2_cyc : out std_logic ;
   ps2_stb : out std_logic ;
   ps2_we  : out std_logic ;
   ps2_ack : in  std_logic

);
end ;

architecture rtl of wb_decoder is

constant LCD_DAT_REG  : std_logic_vector(7 downto 0) := "01110111" ;
constant LCD_CTRL_REG : std_logic_vector(7 downto 0) := "01111111" ;
constant PS2_DAT_REG  : std_logic_vector(7 downto 0) := "01100111" ;
constant PS2_CTRL_REG : std_logic_vector(7 downto 0) := "01101111" ;


begin


process(lcd_dat, lcd_ack, wb_adr, wb_we, wb_stb, wb_cyc, ps2_dat, ps2_ack)
begin

   case wb_adr is
      when LCD_DAT_REG  =>
         lcd_adr <= '0';
         lcd_cyc <= wb_cyc ;
         lcd_stb <= wb_stb ;
         lcd_we  <= wb_we ;
         ps2_adr <= '0' ;
         ps2_cyc <= '0' ;
         ps2_stb <= '0' ;
         ps2_we  <= '0' ;
         wb_dat  <= lcd_dat ;
         wb_ack  <= lcd_ack ;
      when PS2_DAT_REG  =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         ps2_adr <= '1' ;
         ps2_cyc <= wb_cyc ;
         ps2_stb <= wb_stb ;
         ps2_we  <= wb_we ;
         wb_dat  <= ps2_dat ;
         wb_ack  <= ps2_ack ;

      when LCD_CTRL_REG =>
         lcd_adr <= '1' ;
         lcd_cyc <= wb_cyc ;
         lcd_stb <= wb_stb ;
         lcd_we  <= wb_we ;
         ps2_adr <= '0' ;
         ps2_cyc <= '0' ;
         ps2_stb <= '0' ;
         ps2_we  <= '0' ;
         wb_dat  <= lcd_dat ;
         wb_ack  <= lcd_ack ;
      when PS2_CTRL_REG =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         ps2_adr <= '0' ;
         ps2_cyc <= wb_cyc ;
         ps2_stb <= wb_stb ;
         ps2_we  <= wb_we ;
         wb_dat  <= ps2_dat ;
         wb_ack  <= ps2_ack ;
      when others       =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         ps2_adr <= '0' ;
         ps2_cyc <= '0' ;
         ps2_stb <= '0' ;
         ps2_we  <= '0' ;
         wb_dat  <= (others=>'0') ;
         wb_ack  <= '1' ;
   end case ;

end process;

end rtl ;
