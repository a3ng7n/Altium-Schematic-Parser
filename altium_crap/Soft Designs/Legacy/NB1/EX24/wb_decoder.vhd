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

   -- Serial interface
   serial_dat : in  std_logic_vector(7 downto 0) ;
   serial_adr : out std_logic_vector(3 downto 0) ;
   serial_cyc : out std_logic ;
   serial_stb : out std_logic ;
   serial_we  : out std_logic ;
   serial_ack : in  std_logic ;

   -- Keypad interface
   kpad_dat : in  std_logic_vector(7 downto 0) ;
   kpad_cyc : out std_logic ;
   kpad_stb : out std_logic ;
   kpad_we  : out std_logic ;
   kpad_ack : in  std_logic
);
end ;

architecture rtl of wb_decoder is

--                                                                          SFR
constant LCD_DAT_REG      : std_logic_vector(7 downto 0) := "01110111" ; -- 0xF7
constant LCD_CTRL_REG     : std_logic_vector(7 downto 0) := "01111111" ; -- 0xFF

constant KPAD_DAT_REG     : std_logic_vector(7 downto 0) := "01101111" ; -- 0xEF

constant SERIAL_PCON_REG  : std_logic_vector(7 downto 0) := "01100111" ; -- 0xE7
constant SERIAL_SCON_REG  : std_logic_vector(7 downto 0) := "01011111" ; -- 0xDf
constant SERIAL_SBUF_REG  : std_logic_vector(7 downto 0) := "01010111" ; -- 0xD7
constant SERIAL_SRELL_REG : std_logic_vector(7 downto 0) := "01001111" ; -- 0xCF
constant SERIAL_SRELH_REG : std_logic_vector(7 downto 0) := "01000111" ; -- 0xC7
constant SERIAL_TCON_REG  : std_logic_vector(7 downto 0) := "00111111" ; -- 0xBF
constant SERIAL_TL_REG    : std_logic_vector(7 downto 0) := "00110111" ; -- 0xB7
constant SERIAL_TH_REG    : std_logic_vector(7 downto 0) := "00101111" ; -- 0xAF
constant SERIAL_ADCON_REG : std_logic_vector(7 downto 0) := "00100111" ; -- 0xA7


begin


process( lcd_dat,
         lcd_ack,
         wb_adr,
         wb_we,
         wb_stb,
         wb_cyc,
         kpad_dat,
         kpad_ack,
         serial_dat,
         serial_ack
         )
begin

   case wb_adr is
      when LCD_DAT_REG  =>
         lcd_adr    <= '0';
         lcd_cyc    <= wb_cyc ;
         lcd_stb    <= wb_stb ;
         lcd_we     <= wb_we ;
         serial_adr <= (others=>'0') ;
         serial_cyc <= '0' ;
         serial_stb <= '0' ;
         serial_we  <= '0' ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= lcd_dat ;
         wb_ack     <= lcd_ack ;

      when LCD_CTRL_REG =>
         lcd_adr <= '1' ;
         lcd_cyc <= wb_cyc ;
         lcd_stb <= wb_stb ;
         lcd_we  <= wb_we ;
         serial_adr <= (others=>'0') ;
         serial_cyc <= '0' ;
         serial_stb <= '0' ;
         serial_we  <= '0' ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat  <= lcd_dat ;
         wb_ack  <= lcd_ack ;

      when KPAD_DAT_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= (others=>'0') ;
         serial_cyc <= '0' ;
         serial_stb <= '0' ;
         serial_we  <= '0' ;
         kpad_cyc   <= wb_cyc ;
         kpad_stb   <= wb_stb ;
         kpad_we    <= wb_we ;
         wb_dat     <= kpad_dat ;
         wb_ack     <= kpad_ack ;

      when SERIAL_PCON_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= "0000" ;
         serial_cyc <= wb_cyc ;
         serial_stb <= wb_stb ;
         serial_we  <= wb_we ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= serial_dat ;
         wb_ack     <= serial_ack ;

      when SERIAL_SCON_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= "0001" ;
         serial_cyc <= wb_cyc ;
         serial_stb <= wb_stb ;
         serial_we  <= wb_we ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= serial_dat ;
         wb_ack     <= serial_ack ;

      when SERIAL_SBUF_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= "0010" ;
         serial_cyc <= wb_cyc ;
         serial_stb <= wb_stb ;
         serial_we  <= wb_we ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= serial_dat ;
         wb_ack     <= serial_ack ;

      when SERIAL_SRELL_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= "0011" ;
         serial_cyc <= wb_cyc ;
         serial_stb <= wb_stb ;
         serial_we  <= wb_we ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= serial_dat ;
         wb_ack     <= serial_ack ;

      when SERIAL_SRELH_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= "0100" ;
         serial_cyc <= wb_cyc ;
         serial_stb <= wb_stb ;
         serial_we  <= wb_we ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= serial_dat ;
         wb_ack     <= serial_ack ;

      when SERIAL_TCON_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= "0101" ;
         serial_cyc <= wb_cyc ;
         serial_stb <= wb_stb ;
         serial_we  <= wb_we ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= serial_dat ;
         wb_ack     <= serial_ack ;

      when SERIAL_TL_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= "0110" ;
         serial_cyc <= wb_cyc ;
         serial_stb <= wb_stb ;
         serial_we  <= wb_we ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= serial_dat ;
         wb_ack     <= serial_ack ;

      when SERIAL_TH_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= "0111" ;
         serial_cyc <= wb_cyc ;
         serial_stb <= wb_stb ;
         serial_we  <= wb_we ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= serial_dat ;
         wb_ack     <= serial_ack ;

      when SERIAL_ADCON_REG  =>
         lcd_adr    <= '0' ;
         lcd_cyc    <= '0' ;
         lcd_stb    <= '0' ;
         lcd_we     <= '0' ;
         serial_adr <= "1000" ;
         serial_cyc <= wb_cyc ;
         serial_stb <= wb_stb ;
         serial_we  <= wb_we ;
         kpad_cyc   <= '0' ;
         kpad_stb   <= '0' ;
         kpad_we    <= '0' ;
         wb_dat     <= serial_dat ;
         wb_ack     <= serial_ack ;

      when others       =>
         lcd_adr <= '0' ;
         lcd_cyc <= '0' ;
         lcd_stb <= '0' ;
         lcd_we  <= '0' ;
         serial_adr <= (others=>'0') ;
         serial_cyc <= '0' ;
         serial_stb <= '0' ;
         serial_we  <= '0' ;
         kpad_cyc <= '0' ;
         kpad_stb <= '0' ;
         kpad_we  <= '0' ;
         wb_dat  <= (others=>'0') ;
         wb_ack  <= '1' ;
   end case ;

end process;

end rtl ;
