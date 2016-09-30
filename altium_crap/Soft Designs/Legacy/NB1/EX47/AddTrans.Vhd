Library Ieee;
use ieee.std_logic_1164.all;


  entity addTrans is
  port (  
     sel : in std_logic;
     inp   : in std_logic_vector(15 downto 0);
     outp  : out std_logic_vector(15 downto 0);
     io    : inout std_logic_vector(15 downto 0)
     );
  end entity;

  architecture rtl of addTrans is
  begin
      outp <= io when sel = '0' else (others=>'Z');
      io  <= inp when sel = '1' else (others=>'Z');
  end architecture;









