const
   ScanArrayLimit = 7;
   ScanShapeLimit = 6;

var
   HSVUpdate, RGBUpdate : Boolean;
   ScanArray : array[0..ScanArrayLimit] of Integer;
   ScanShape : array[0..ScanShapeLimit] of Integer;
   LEDScanIndex : Integer;
   LEDScanIndexModifier : Integer;

procedure TxMainForm.OnCreate(Sender: TObject);
begin
     ScanShape[0] := 0;
     ScanShape[1] := 10;
     ScanShape[2] := 64;
     ScanShape[3] := 255;
     ScanShape[4] := 64;
     ScanShape[5] := 10;
     ScanShape[6] := 0;

     LEDScanIndex := 0;
     LEDScanIndexModifier := 1;
end;

//Scanning lights
procedure TxMainForm.TimerTick(Sender: TObject);
var
   i : integer;
   PreScanValue : integer;

begin

     PreScanValue := 100;

     for i := 0 to ScanArrayLimit do
     begin
         if ScanArray[i]  = PreScanValue then
            ScanArray[i] := 255
         else
            ScanArray[i] := ScanArray[i] * 0.5;
     end;

     //current item = 100
     ScanArray[LedScanIndex] := PreScanValue;

     //modify the index on this clock tick
     LEDScanIndex := LEDScanIndex + LEDScanIndexModifier;

     //when it gets to the end, send it back the other way
     if (LEDScanIndex = ScanArrayLimit) or (LEDScanIndex = 0) then
        LEDScanIndexModifier := LEDScanIndexModifier * -1;

     xLEDSSlider0.Value := ScanArray[0];
     xLEDSSlider1.Value := ScanArray[1];
     xLEDSSlider2.Value := ScanArray[2];
     xLEDSSlider3.Value := ScanArray[3];
     xLEDSSlider4.Value := ScanArray[4];
     xLEDSSlider5.Value := ScanArray[5];
     xLEDSSlider6.Value := ScanArray[6];
     xLEDSSlider7.Value := ScanArray[7];

end;


