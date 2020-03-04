------------------------------------------------------------
-- SubModule NoteDividerTable
-- Created   20/08/2004 3:14:22 PM
------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
entity NoteDividerTable is port
   (
     DIVIDERRATIO        : out   std_logic_vector(15 downto 0);
     NoteN               : in    std_logic_vector(7 downto 0) ;
     Invalid             : out   std_Logic
   );
end NoteDividerTable;
------------------------------------------------------------

------------------------------------------------------------
architecture structure of NoteDividerTable is

begin
  DividerRatio <=
                 x"6EF9" when NoteN = x"00" else -- Note # 0
                 x"68BF" when NoteN = x"01" else -- Note # 1
                 x"62DE" when NoteN = x"02" else -- Note # 2
                 x"5D51" when NoteN = x"03" else -- Note # 3
                 x"5814" when NoteN = x"04" else -- Note # 4
                 x"5323" when NoteN = x"05" else -- Note # 5
                 x"4E78" when NoteN = x"06" else -- Note # 6
                 x"4A11" when NoteN = x"07" else -- Note # 7
                 x"45E9" when NoteN = x"08" else -- Note # 8
                 x"41FC" when NoteN = x"09" else -- Note # 9
                 x"3E48" when NoteN = x"0A" else -- Note #10
                 x"3AC9" when NoteN = x"0B" else -- Note #11
                 x"377D" when NoteN = x"0C" else -- Note #12
                 x"345F" when NoteN = x"0D" else -- Note #13
                 x"316F" when NoteN = x"0E" else -- Note #14
                 x"2EA9" when NoteN = x"0F" else -- Note #15
                 x"2C0A" when NoteN = x"10" else -- Note #16
                 x"2991" when NoteN = x"11" else -- Note #17
                 x"273C" when NoteN = x"12" else -- Note #18
                 x"2508" when NoteN = x"13" else -- Note #19
                 x"22F4" when NoteN = x"14" else -- Note #20
                 x"20FE" when NoteN = x"15" else -- Note #21
                 x"1F24" when NoteN = x"16" else -- Note #22
                 x"1D65" when NoteN = x"17" else -- Note #23
                 x"1BBE" when NoteN = x"18" else -- Note #24
                 x"1A30" when NoteN = x"19" else -- Note #25
                 x"18B7" when NoteN = x"1A" else -- Note #26
                 x"1754" when NoteN = x"1B" else -- Note #27
                 x"1605" when NoteN = x"1C" else -- Note #28
                 x"14C9" when NoteN = x"1D" else -- Note #29
                 x"139E" when NoteN = x"1E" else -- Note #30
                 x"1284" when NoteN = x"1F" else -- Note #31
                 x"117A" when NoteN = x"20" else -- Note #32
                 x"107F" when NoteN = x"21" else -- Note #33
                 x"0F92" when NoteN = x"22" else -- Note #34
                 x"0EB2" when NoteN = x"23" else -- Note #35
                 x"0DDF" when NoteN = x"24" else -- Note #36
                 x"0D18" when NoteN = x"25" else -- Note #37
                 x"0C5C" when NoteN = x"26" else -- Note #38
                 x"0BAA" when NoteN = x"27" else -- Note #39
                 x"0B03" when NoteN = x"28" else -- Note #40
                 x"0A64" when NoteN = x"29" else -- Note #41
                 x"09CF" when NoteN = x"2A" else -- Note #42
                 x"0942" when NoteN = x"2B" else -- Note #43
                 x"08BD" when NoteN = x"2C" else -- Note #44
                 x"0840" when NoteN = x"2D" else -- Note #45
                 x"07C9" when NoteN = x"2E" else -- Note #46
                 x"0759" when NoteN = x"2F" else -- Note #47
                 x"06F0" when NoteN = x"30" else -- Note #48
                 x"068C" when NoteN = x"31" else -- Note #49
                 x"062E" when NoteN = x"32" else -- Note #50
                 x"05D5" when NoteN = x"33" else -- Note #51
                 x"0581" when NoteN = x"34" else -- Note #52
                 x"0532" when NoteN = x"35" else -- Note #53
                 x"04E8" when NoteN = x"36" else -- Note #54
                 x"04A1" when NoteN = x"37" else -- Note #55
                 x"045F" when NoteN = x"38" else -- Note #56
                 x"0420" when NoteN = x"39" else -- Note #57
                 x"03E5" when NoteN = x"3A" else -- Note #58
                 x"03AD" when NoteN = x"3B" else -- Note #59
                 x"0378" when NoteN = x"3C" else -- Note #60
                 x"0346" when NoteN = x"3D" else -- Note #61
                 x"0317" when NoteN = x"3E" else -- Note #62
                 x"02EB" when NoteN = x"3F" else -- Note #63
                 x"02C1" when NoteN = x"40" else -- Note #64
                 x"0299" when NoteN = x"41" else -- Note #65
                 x"0274" when NoteN = x"42" else -- Note #66
                 x"0251" when NoteN = x"43" else -- Note #67
                 x"022F" when NoteN = x"44" else -- Note #68
                 x"0210" when NoteN = x"45" else -- Note #69
                 x"01F2" when NoteN = x"46" else -- Note #70
                 x"01D6" when NoteN = x"47" else -- Note #71
                 x"01BC" when NoteN = x"48" else -- Note #72
                 x"01A3" when NoteN = x"49" else -- Note #73
                 x"018B" when NoteN = x"4A" else -- Note #74
                 x"0175" when NoteN = x"4B" else -- Note #75
                 x"0160" when NoteN = x"4C" else -- Note #76
                 x"014D" when NoteN = x"4D" else -- Note #77
                 x"013A" when NoteN = x"4E" else -- Note #78
                 x"0128" when NoteN = x"4F" else -- Note #79
                 x"0118" when NoteN = x"50" else -- Note #80
                 x"0108" when NoteN = x"51" else -- Note #81
                 x"00F9" when NoteN = x"52" else -- Note #82
                 x"00EB" when NoteN = x"53" else -- Note #83
                 x"00DE" when NoteN = x"54" else -- Note #84
                 x"00D1" when NoteN = x"55" else -- Note #85
                 x"00C6" when NoteN = x"56" else -- Note #86
                 x"00BB" when NoteN = x"57" else -- Note #87
                 x"00B0" when NoteN = x"58" else -- Note #88
                 x"00A6" when NoteN = x"59" else -- Note #89
                 x"009D" when NoteN = x"5A" else -- Note #90
                 x"0094" when NoteN = x"5B" else -- Note #91
                 x"008C" when NoteN = x"5C" else -- Note #92
                 x"0084" when NoteN = x"5D" else -- Note #93
                 x"007D" when NoteN = x"5E" else -- Note #94
                 x"0076" when NoteN = x"5F" else -- Note #95
                 x"06F0";
  Invalid <= '0' when (NoteN < x"60") else '1';
 end structure;
------------------------------------------------------------
