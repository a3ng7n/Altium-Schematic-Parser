--------------------------------------------------------------------------------
Library IEEE;
Use IEEE.Std_Logic_1164.all;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
Entity ScalePatternGenerator Is
  Port(
        Clock  : In  Std_Logic;
        Reset  : In  Std_Logic;
        DOUT   : Out Std_Logic_Vector(2 DownTo 0)
      );
End ScalePatternGenerator;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
Architecture RTL Of ScalePatternGenerator Is
    Subtype  TState Is Std_Logic_Vector (3 DownTo 0);
    Signal   State    : TState;
    Constant State_0  : TState := X"0";
    Constant State_1  : TState := X"1";
    Constant State_2  : TState := X"2";
    Constant State_3  : TState := X"3";
    Constant State_4  : TState := X"4";
    Constant State_5  : TState := X"5";
    Constant State_6  : TState := X"6";
    Constant State_7  : TState := X"7";
    Constant State_8  : TState := X"8";
    Constant State_9  : TState := X"9";
    Constant State_10 : TState := X"A";
    Constant State_11 : TState := X"B";
    Constant State_12 : TState := X"C";
    Constant State_13 : TState := X"D";
    Constant State_14 : TState := X"E";
    Constant State_15 : TState := X"F";
Begin
    FSM: Process (State,Reset,Clock)
    Begin
       If Reset = '1' Then
          State <= State_0;
          DOUT  <= "000";
       Elsif Rising_Edge(Clock) Then
           Case State Is
              When State_0  => DOUT <= "000"; State <= State_1 ;
              When State_1  => DOUT <= "001"; State <= State_2 ;
              When State_2  => DOUT <= "010"; State <= State_3 ;
              When State_3  => DOUT <= "011"; State <= State_4 ;
              When State_4  => DOUT <= "100"; State <= State_5 ;
              When State_5  => DOUT <= "101"; State <= State_6 ;
              When State_6  => DOUT <= "110"; State <= State_7 ;
              When State_7  => DOUT <= "111"; State <= State_8 ;
              When State_8  => DOUT <= "111"; State <= State_9 ;
              When State_9  => DOUT <= "110"; State <= State_10;
              When State_10 => DOUT <= "101"; State <= State_11;
              When State_11 => DOUT <= "100"; State <= State_12;
              When State_12 => DOUT <= "011"; State <= State_13;
              When State_13 => DOUT <= "010"; State <= State_14;
              When State_14 => DOUT <= "001"; State <= State_15;
              When State_15 => DOUT <= "000"; State <= State_0 ;
              When Others   =>                State <= State_0 ;
           End Case;
       End If;
    End Process;
End RTL;
--------------------------------------------------------------------------------

