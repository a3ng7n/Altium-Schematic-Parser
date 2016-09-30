--------------------------------------------------------------------------------
Library IEEE;
Use IEEE.Std_Logic_1164.All;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
Entity Flash_Fix Is Port
   (
     DATA_PROC : InOut   Std_Logic_Vector(31 DownTo 0);
     DATA_FPGA : InOut   Std_Logic_Vector(31 DownTo 0);
     PER_OE    : In      Std_Logic;
     FPGA_CS   : In      Std_Logic_Vector(3 DownTo 0);
     PER_RNW    : in    std_logic
   );
End Flash_Fix;
--------------------------------------------------------------------------------        

--------------------------------------------------------------------------------
Architecture RTL of Flash_Fix is
   Signal   IsFlash  : Std_Logic;
Begin
   IsFlash <= FPGA_CS(0) And FPGA_CS(1) And FPGA_CS(2) And FPGA_CS(3) And (Not PER_OE Or Not PER_RNW);


   -----------------------------------------------------------------------------
   FlipSignal:
   Process (IsFlash,
            PER_OE,
            PER_RNW,
            DATA_FPGA,
            DATA_PROC)
   Begin
       DATA_PROC <= (Others => 'Z');
       DATA_FPGA <= (Others => 'Z');
       If IsFlash = '1' Then
           If PER_OE = '0' Then
               DATA_PROC(31 downto 16) <= DATA_PROC(15 downto 0);
               DATA_FPGA               <= (Others => 'Z');
           ElsIf PER_RNW = '0' Then
               DATA_PROC(15 downto 0)  <= DATA_PROC(31 downto 16);
               DATA_FPGA               <= (Others => 'Z');
           End If;
       Else
           If PER_OE = '0' Then
               DATA_PROC <= DATA_FPGA;
               DATA_FPGA <= (Others => 'Z');
           ElsIf PER_RNW = '0' Then
               DATA_PROC <= (Others => 'Z');
               DATA_FPGA <= DATA_PROC;
           End If;
       End If;
   End Process;
   -----------------------------------------------------------------------------   

End Architecture;

