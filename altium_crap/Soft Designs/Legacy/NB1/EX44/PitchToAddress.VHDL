-------------------------------------------------------------------------------
Library IEEE;
Use IEEE.Std_Logic_1164.all;
Use ieee.std_logic_unsigned.all;

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
Entity PitchToAddress Is Port
   (
     Pitch  : In  Std_Logic_Vector(7 downto 0);
     Octave : Out Std_Logic_Vector(3 downto 0);
     Note   : Out Std_Logic_Vector(3 downto 0)
   );
End PitchToAddress;
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
Architecture RTL Of PitchToAddress Is
    Signal Num : Std_Logic_Vector (7 downto 0);
Begin
    Process (Pitch)
    Begin
           If Pitch >= X"6C" Then Num <= Pitch + X"24";
        ElsIf Pitch >= X"60" Then Num <= Pitch + X"20";
        ElsIf Pitch >= X"54" Then Num <= Pitch + X"1C";
        ElsIf Pitch >= X"48" Then Num <= Pitch + X"18";
        ElsIf Pitch >= X"3C" Then Num <= Pitch + X"14";
        ElsIf Pitch >= X"30" Then Num <= Pitch + X"10";
        ElsIf Pitch >= X"24" Then Num <= Pitch + X"0C";
        ElsIf Pitch >= X"18" Then Num <= Pitch + X"08";
        ElsIf Pitch >= X"0C" Then Num <= Pitch + X"04";
        Else                      Num <= Pitch;
        End If;
    End Process;
    Note   <= Num(3 DownTo 0);
    Octave <= Num(7 DownTo 4);
End RTL;
-------------------------------------------------------------------------------


