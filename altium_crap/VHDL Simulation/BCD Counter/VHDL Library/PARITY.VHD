Library IEEE;
Use     IEEE.std_logic_1164.all;
Use     WORK.utility.all;

entity PARITYC is
  port
  (
    L   : in STD_LOGIC_VECTOR(3 downto 0);
    U   : in STD_LOGIC_VECTOR(3 downto 0);
    P     : out STD_LOGIC
  );
end PARITYC;

architecture PARITY_Arch of PARITYC is

signal VTC : STD_LOGIC_VECTOR(7 downto 0);

begin
    VTC(7 downto 4) <= U;
    VTC(3 downto 0) <= L;

    P <= fparity(VTC);

end PARITY_Arch;