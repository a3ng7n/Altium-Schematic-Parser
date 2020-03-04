library ieee;
use ieee.std_logic_1164.all ;

entity wb_dec is
port (
   -- master interface
   wb_adr : in  std_logic_vector(7 downto 0) ;
   wb_we  : in  std_logic ;
   wb_stb : in  std_logic ;
   wb_cyc : in  std_logic ;
   wb_dat : out std_logic_vector(7 downto 0) ;
   wb_ack : out std_logic ;

   -- emac interface
   emac_dat : in  std_logic_vector(7 downto 0) ;
   emac_adr : out std_logic_vector(1 downto 0) ;
   emac_cyc : out std_logic ;
   emac_stb : out std_logic ;
   emac_we  : out std_logic ;
   emac_ack : in  std_logic
);
end ;

architecture rtl of wb_dec is

--                                                                           SFR
    constant EMAC_DAT_REG      : std_logic_vector(7 downto 0) := "01111101" ; -- 0xFD
    constant EMAC_ADDR_H_REG   : std_logic_vector(7 downto 0) := "01111111" ; -- 0xFF
    constant EMAC_ADDR_L_REG   : std_logic_vector(7 downto 0) := "01111110" ; -- 0xFE
    signal selected : std_logic;

begin

--    selected  <= '1' when wb_adr(7 downto 2) = "011111" else '0';
--
--    emac_adr  <= wb_adr(1 downto 0);
--
--    emac_cyc  <= wb_cyc and selected;
--    emac_stb  <= wb_stb and selected;
--    emac_we   <= wb_we and selected;
--    wb_ack    <= emac_ack and selected;
--
--
--    wb_dat    <= emac_dat when selected = '1' else "00000000";

process( emac_dat,
         emac_ack,
         wb_adr,
         wb_we,
         wb_stb,
         wb_cyc
         )
begin

   case wb_adr is
      when EMAC_DAT_REG  =>
         emac_adr    <= "00";
         emac_cyc    <= wb_cyc ;
         emac_stb    <= wb_stb ;
         emac_we     <= wb_we ;
         wb_dat     <= emac_dat ;
         wb_ack     <= emac_ack ;

      when EMAC_ADDR_H_REG =>
         emac_adr <= "11" ;
         emac_cyc <= wb_cyc ;
         emac_stb <= wb_stb ;
         emac_we  <= wb_we ;
         wb_dat  <= emac_dat ;
         wb_ack  <= emac_ack ;

      when EMAC_ADDR_L_REG =>
         emac_adr <= "10" ;
         emac_cyc <= wb_cyc ;
         emac_stb <= wb_stb ;
         emac_we  <= wb_we ;
         wb_dat  <= emac_dat ;
         wb_ack  <= emac_ack ;

      when others       =>
         emac_adr <= "00" ;
         emac_cyc <= '0' ;
         emac_stb <= '0' ;
         emac_we  <= '0' ;
         wb_dat  <= (others=>'0') ;
         wb_ack  <= '1' ;
   end case ;

end process;

end rtl ;
