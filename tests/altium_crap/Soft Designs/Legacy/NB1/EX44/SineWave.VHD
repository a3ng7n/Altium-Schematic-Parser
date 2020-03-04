--------------------------------------------------------------------------------
Library IEEE;
Use IEEE.Std_Logic_1164.all;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
Entity SineWaveGenerator Is
  Port(
        Clock  : In  Std_Logic;
        Reset  : In  Std_Logic;
        DOUT   : Out Std_Logic_Vector(7 DownTo 0)
      );
End SineWaveGenerator;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
Architecture RTL Of SineWaveGenerator Is
    Subtype  TState Is Std_Logic_Vector (5 DownTo 0);
    Signal   State    : TState;
    Constant State_0  : TState := "000000";
    Constant State_1  : TState := "000001";
    Constant State_2  : TState := "000010";
    Constant State_3  : TState := "000011";
    Constant State_4  : TState := "000100";
    Constant State_5  : TState := "000101";
    Constant State_6  : TState := "000110";
    Constant State_7  : TState := "000111";
    Constant State_8  : TState := "001000";
    Constant State_9  : TState := "001001";
    Constant State_10 : TState := "001010";
    Constant State_11 : TState := "001011";
    Constant State_12 : TState := "001100";
    Constant State_13 : TState := "001101";
    Constant State_14 : TState := "001110";
    Constant State_15 : TState := "001111";
    Constant State_16 : TState := "010000";
    Constant State_17 : TState := "010001";
    Constant State_18 : TState := "010010";
    Constant State_19 : TState := "010011";
    Constant State_20 : TState := "010100";
    Constant State_21 : TState := "010101";
    Constant State_22 : TState := "010110";
    Constant State_23 : TState := "010111";
    Constant State_24 : TState := "011000";
    Constant State_25 : TState := "011001";
    Constant State_26 : TState := "011010";
    Constant State_27 : TState := "011011";
    Constant State_28 : TState := "011100";
    Constant State_29 : TState := "011101";
    Constant State_30 : TState := "011110";
    Constant State_31 : TState := "011111";
    Constant State_32 : TState := "100000";
    Constant State_33 : TState := "100001";
    Constant State_34 : TState := "100010";
    Constant State_35 : TState := "100011";
    Constant State_36 : TState := "100100";
    Constant State_37 : TState := "100101";
    Constant State_38 : TState := "100110";
    Constant State_39 : TState := "100111";
    Constant State_40 : TState := "101000";
    Constant State_41 : TState := "101001";
    Constant State_42 : TState := "101010";
    Constant State_43 : TState := "101011";
    Constant State_44 : TState := "101100";
    Constant State_45 : TState := "101101";
    Constant State_46 : TState := "101110";
    Constant State_47 : TState := "101111";
    Constant State_48 : TState := "110000";
    Constant State_49 : TState := "110001";
    Constant State_50 : TState := "110010";
    Constant State_51 : TState := "110011";
    Constant State_52 : TState := "110100";
    Constant State_53 : TState := "110101";
    Constant State_54 : TState := "110110";
    Constant State_55 : TState := "110111";
    Constant State_56 : TState := "111000";
    Constant State_57 : TState := "111001";
    Constant State_58 : TState := "111010";
    Constant State_59 : TState := "111011";
    Constant State_60 : TState := "111100";
    Constant State_61 : TState := "111101";
    Constant State_62 : TState := "111110";
    Constant State_63 : TState := "111111";
Begin
    FSM: Process (State,Reset,Clock)
    Begin
       If Reset = '1' Then
          State <= State_0;
          DOUT  <= X"00";
       Elsif Rising_Edge(Clock) Then
           Case State Is
              When State_0  => DOUT <= X"80"; State <= State_1 ;
              When State_1  => DOUT <= X"8C"; State <= State_2 ;
              When State_2  => DOUT <= X"98"; State <= State_3 ;
              When State_3  => DOUT <= X"A5"; State <= State_4 ;
              When State_4  => DOUT <= X"B0"; State <= State_5 ;
              When State_5  => DOUT <= X"BC"; State <= State_6 ;
              When State_6  => DOUT <= X"C7"; State <= State_7 ;
              When State_7  => DOUT <= X"D1"; State <= State_8 ;
              When State_8  => DOUT <= X"DA"; State <= State_9 ;
              When State_9  => DOUT <= X"E2"; State <= State_10;
              When State_10 => DOUT <= X"EA"; State <= State_11;
              When State_11 => DOUT <= X"F0"; State <= State_12;
              When State_12 => DOUT <= X"F6"; State <= State_13;
              When State_13 => DOUT <= X"FA"; State <= State_14;
              When State_14 => DOUT <= X"FD"; State <= State_15;
              When State_15 => DOUT <= X"FF"; State <= State_16;
              When State_16 => DOUT <= X"FF"; State <= State_17;
              When State_17 => DOUT <= X"FF"; State <= State_18;
              When State_18 => DOUT <= X"FD"; State <= State_19;
              When State_19 => DOUT <= X"FA"; State <= State_20;
              When State_20 => DOUT <= X"F6"; State <= State_21;
              When State_21 => DOUT <= X"F0"; State <= State_22;
              When State_22 => DOUT <= X"EA"; State <= State_23;
              When State_23 => DOUT <= X"E2"; State <= State_24;
              When State_24 => DOUT <= X"DA"; State <= State_25;
              When State_25 => DOUT <= X"D1"; State <= State_26;
              When State_26 => DOUT <= X"C7"; State <= State_27;
              When State_27 => DOUT <= X"BC"; State <= State_28;
              When State_28 => DOUT <= X"B0"; State <= State_29;
              When State_29 => DOUT <= X"A5"; State <= State_30;
              When State_30 => DOUT <= X"98"; State <= State_31;
              When State_31 => DOUT <= X"8C"; State <= State_32;
              When State_32 => DOUT <= X"80"; State <= State_33;
              When State_33 => DOUT <= X"74"; State <= State_34;
              When State_34 => DOUT <= X"68"; State <= State_35;
              When State_35 => DOUT <= X"5B"; State <= State_36;
              When State_36 => DOUT <= X"50"; State <= State_37;
              When State_37 => DOUT <= X"44"; State <= State_38;
              When State_38 => DOUT <= X"39"; State <= State_39;
              When State_39 => DOUT <= X"2F"; State <= State_40;
              When State_40 => DOUT <= X"26"; State <= State_41;
              When State_41 => DOUT <= X"1E"; State <= State_42;
              When State_42 => DOUT <= X"16"; State <= State_43;
              When State_43 => DOUT <= X"10"; State <= State_44;
              When State_44 => DOUT <= X"0A"; State <= State_45;
              When State_45 => DOUT <= X"06"; State <= State_46;
              When State_46 => DOUT <= X"03"; State <= State_47;
              When State_47 => DOUT <= X"01"; State <= State_48;
              When State_48 => DOUT <= X"01"; State <= State_49;
              When State_49 => DOUT <= X"01"; State <= State_50;
              When State_50 => DOUT <= X"03"; State <= State_51;
              When State_51 => DOUT <= X"06"; State <= State_52;
              When State_52 => DOUT <= X"0A"; State <= State_53;
              When State_53 => DOUT <= X"10"; State <= State_54;
              When State_54 => DOUT <= X"16"; State <= State_55;
              When State_55 => DOUT <= X"1E"; State <= State_56;
              When State_56 => DOUT <= X"26"; State <= State_57;
              When State_57 => DOUT <= X"2F"; State <= State_58;
              When State_58 => DOUT <= X"39"; State <= State_59;
              When State_59 => DOUT <= X"44"; State <= State_60;
              When State_60 => DOUT <= X"50"; State <= State_61;
              When State_61 => DOUT <= X"5B"; State <= State_62;
              When State_62 => DOUT <= X"68"; State <= State_63;
              When State_63 => DOUT <= X"74"; State <= State_0 ;
              When Others   =>                State <= State_0 ;
           End Case;
       End If;
    End Process;
End RTL;
--------------------------------------------------------------------------------

