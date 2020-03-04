LIBRARY ieee;
    USE ieee.std_logic_1164.ALL;

-------------------------------------------------------------------
ENTITY BrightnessPalette IS
    PORT( D     : IN  std_logic_vector(3 downto 0);
          Q     : OUT std_logic_vector(7 downto 0)
        );
end BrightnessPalette;
-------------------------------------------------------------------

-------------------------------------------------------------------
Architecture RTL OF BrightnessPalette IS
Begin
   Process(D)
   Begin
       Case D Is
          when X"0"   => Q <= X"00";
          when X"1"   => Q <= X"01";
          when X"2"   => Q <= X"02";
          when X"3"   => Q <= X"03";
          when X"4"   => Q <= X"05";
          when X"5"   => Q <= X"07";
          when X"6"   => Q <= X"0B";
          when X"7"   => Q <= X"10";
          when X"8"   => Q <= X"17";
          when X"9"   => Q <= X"20";
          when X"A"   => Q <= X"2E";
          when X"B"   => Q <= X"41";
          when X"C"   => Q <= X"5C";
          when X"D"   => Q <= X"81";
          when X"E"   => Q <= X"B6";
          when others => Q <= X"FF";
       End Case;
   End Process;
End RTL;
-------------------------------------------------------------------
