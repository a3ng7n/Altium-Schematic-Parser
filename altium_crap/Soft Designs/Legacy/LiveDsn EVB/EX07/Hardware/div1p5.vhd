------------------------------------------------------------
Library ieee;
Use ieee.std_logic_1164.all;

Entity div1p5 Is Port
   (
      CLK       :In    std_logic := '0';
      CLKOUT    :OUT   std_logic
   );
End div1p5;
------------------------------------------------------------

------------------------------------------------------------
Architecture Structure Of div1p5 Is

signal Clock_cycle: natural;

   signal asig : std_logic ;  
   signal bsig : std_logic ;
   signal g1, g2, g3 : std_logic ;
  
Begin

CLKOUT <= g3 or Asig ;
g1 <= (not clk)  and Bsig ;
g2 <= g3 and clk ;
g3 <= g1 or g2 ;

process(clk)
begin
if rising_edge(clk) then
      Asig <= (not Asig) and (not Bsig) ;
end if ;
end process;

process(clk)
begin
if rising_edge(clk) then
      Bsig <= Asig;
end if ;
end process;


 
End Structure;
------------------------------------------------------------
