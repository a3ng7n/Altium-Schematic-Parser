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
   -- can interface
   i2c_dat : in  std_logic_vector(7 downto 0) ;
   i2c_adr : out std_logic_vector(2 downto 0) ;
   i2c_cyc : out std_logic ;
   i2c_stb : out std_logic ;
   i2c_we  : out std_logic ;
   i2c_ack : in  std_logic ;
   -- keypad interface
   keypad_dat : in  std_logic_vector(7 downto 0) ;
   keypad_cyc : out std_logic ;
   keypad_stb : out std_logic ;
   keypad_we  : out std_logic ;
   keypad_ack : in  std_logic
);
end ;

architecture rtl of wb_decoder is
-- LCD sfr mappings
constant LCD_DAT_REG  : std_logic_vector(7 downto 0) := "01110111" ; -- SFR @ 0xF7
constant LCD_CTRL_REG : std_logic_vector(7 downto 0) := "01111111" ; -- SFR @ 0xFF
-- KEYPAD DATA Register
constant KEYPAD_DAT_REG   : std_logic_vector(7 downto 0) := "00110111" ; -- SFR @ 0xB7
-- I2C sfr mappings
constant I2C_CTRL_REG : std_logic_vector(7 downto 0) := "01100111" ; -- SFR @ 0xE7
constant I2C_STAT_REG : std_logic_vector(7 downto 0) := "01101111" ; -- SFR @ 0xEF
constant I2C_CLK0_REG : std_logic_vector(7 downto 0) := "01010111" ; -- SFR @ 0xD7
constant I2C_CLK1_REG : std_logic_vector(7 downto 0) := "01011111" ; -- SFR @ 0xDF
constant I2C_WRIT_REG : std_logic_vector(7 downto 0) := "01000111" ; -- SFR @ 0xC7
constant I2C_READ_REG : std_logic_vector(7 downto 0) := "01001111" ; -- SFR @ 0xCF
-- I2C internal register
constant i2c_ctrl  : std_logic_vector(2 downto 0) := "000" ;
constant i2c_stat  : std_logic_vector(2 downto 0) := "001" ;
constant i2c_clk0  : std_logic_vector(2 downto 0) := "010" ;
constant i2c_clk1  : std_logic_vector(2 downto 0) := "011" ;
constant i2c_writ  : std_logic_vector(2 downto 0) := "100" ;
constant i2c_read  : std_logic_vector(2 downto 0) := "101" ;
constant i2c_idle  : std_logic_vector(2 downto 0) := "111" ;


begin


process(lcd_dat, lcd_ack, wb_adr, wb_we, wb_stb, wb_cyc, i2c_dat, i2c_ack, keypad_dat, keypad_ack)
begin

   case wb_adr is
      when LCD_DAT_REG  =>
         lcd_adr <= '0';
         lcd_cyc <= wb_cyc ;
         lcd_stb <= wb_stb ;
         lcd_we  <= wb_we ;
         i2c_adr <= i2c_idle ;
         i2c_cyc <= '0' ;
         i2c_stb <= '0' ;
         i2c_we  <= '0' ;
         keypad_cyc <= '0' ;
         keypad_stb <= '0' ;
         keypad_we  <= '0' ;
         wb_dat  <= lcd_dat ;
         wb_ack  <= lcd_ack ;

      when LCD_CTRL_REG =>
         lcd_adr <= '1' ;
         lcd_cyc <= wb_cyc ;
         lcd_stb <= wb_stb ;
         lcd_we  <= wb_we ;
         i2c_adr <= i2c_idle ;
         i2c_cyc <= '0' ;
         i2c_stb <= '0' ;
         i2c_we  <= '0' ;
         keypad_cyc <= '0' ;
         keypad_stb <= '0' ;
         keypad_we  <= '0' ;
         wb_dat  <= lcd_dat ;
         wb_ack  <= lcd_ack ;

      when I2C_CTRL_REG =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         i2c_adr <= i2c_ctrl ;
         i2c_cyc <= wb_cyc ;
         i2c_stb <= wb_stb ;
         i2c_we  <= wb_we ;
         keypad_cyc <= '0' ;
         keypad_stb <= '0' ;
         keypad_we  <= '0' ;
         wb_dat  <= i2c_dat ;
         wb_ack  <= i2c_ack ;

      when I2C_STAT_REG =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         i2c_adr <= i2c_stat ;
         i2c_cyc <= wb_cyc ;
         i2c_stb <= wb_stb ;
         i2c_we  <= wb_we ;
         keypad_cyc <= '0' ;
         keypad_stb <= '0' ;
         keypad_we  <= '0' ;
         wb_dat  <= i2c_dat ;
         wb_ack  <= i2c_ack ;

      when I2C_CLK0_REG =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         i2c_adr <= i2c_clk0 ;
         i2c_cyc <= wb_cyc ;
         i2c_stb <= wb_stb ;
         i2c_we  <= wb_we ;
         keypad_cyc <= '0' ;
         keypad_stb <= '0' ;
         keypad_we  <= '0' ;
         wb_dat  <= i2c_dat ;
         wb_ack  <= i2c_ack ;

      when I2C_CLK1_REG =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         i2c_adr <= i2c_clk1 ;
         i2c_cyc <= wb_cyc ;
         i2c_stb <= wb_stb ;
         i2c_we  <= wb_we ;
         keypad_cyc <= '0' ;
         keypad_stb <= '0' ;
         keypad_we  <= '0' ;
         wb_dat  <= i2c_dat ;
         wb_ack  <= i2c_ack ;

      when I2C_WRIT_REG =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         i2c_adr <= i2c_writ ;
         i2c_cyc <= wb_cyc ;
         i2c_stb <= wb_stb ;
         i2c_we  <= wb_we ;
         keypad_cyc <= '0' ;
         keypad_stb <= '0' ;
         keypad_we  <= '0' ;
         wb_dat  <= i2c_dat ;
         wb_ack  <= i2c_ack ;

      when I2C_READ_REG =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         i2c_adr <= i2c_read ;
         i2c_cyc <= wb_cyc ;
         i2c_stb <= wb_stb ;
         i2c_we  <= wb_we ;
         keypad_cyc <= '0' ;
         keypad_stb <= '0' ;
         keypad_we  <= '0' ;
         wb_dat  <= i2c_dat ;
         wb_ack  <= i2c_ack ;

      when KEYPAD_DAT_REG =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         i2c_adr <= i2c_idle ;
         i2c_cyc <= '0' ;
         i2c_stb <= '0' ;
         i2c_we  <= '0' ;
         keypad_cyc <= wb_cyc ;
         keypad_stb <= wb_stb ;
         keypad_we  <= wb_we ;
         wb_dat  <= keypad_dat ;
         wb_ack  <= keypad_ack ;

      when others       =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         i2c_adr <= i2c_idle ;
         i2c_cyc <= '0' ;
         i2c_stb <= '0' ;
         i2c_we  <= '0' ;
         keypad_cyc <= '0' ;
         keypad_stb <= '0' ;
         keypad_we  <= '0' ;
         wb_dat  <= (others=>'0') ;
         wb_ack  <= '1' ;
   end case ;

end process;

end rtl ;
