var HSVUpdate, RGBUpdate : Boolean;

procedure TxMainForm.xClearButtonClick(Sender: TObject);
begin
  RED_O.Value := 0;
  GREEN_O.Value := 0;
  BLUE_O.Value := 0;
end;

function Module (X, Y, Z : Real) : Real;
begin
     Result := Sqrt(x * x + y * y + z * z);
end;

function Arg(xa, ya : Real) : Real;
begin
     Result := 0;
     if (xa = 0) and (ya = 0) then
     begin
          Result := 0;
          Exit;
     end;

     if (xa = 0) and (ya >= 0) then
     begin
          Result := Pi / 2;
          Exit;
     end;

     if (ya = 0) and (xa < 0) then
     begin
          Result := Pi;
          Exit;
     end;

     if (xa = 0) and (ya < 0) then
     begin
          Result := -Pi / 2;
          Exit;
     end;

     if (xa > 0) then
     begin
          Result := ArcTan(ya / xa);
          Exit;
     end;

     if (xa < 0) and (ya >= 0) then Result := Pi - ArcTan(-ya / xa);

     if (xa < 0) and (ya < 0) then Result := -Pi + ArcTan(-ya / -xa);
end;

procedure RGBtoHSV();
var  Temp, xa, ya, Hue, Saturation, Value : Real;

begin
     Temp := (xRedSlider.Value + xGreenSlider.Value + xBlueSlider.Value) / 3;
     xa := (xGreenSlider.Value - xRedSlider.Value) / sqrt(2);
     ya := (2 * xBlueSlider.Value - xRedSlider.Value - xGreenSlider.Value) / sqrt(6);
     Hue := Arg(xa, ya) * 180 / Pi + 150;
     Saturation := Arg(Temp, Module(xRedSlider.Value - Temp, xGreenSlider.Value - Temp, xBlueSlider.Value - Temp)) * 100 / ArcTan(Sqrt(6));
     Value := Temp / 2.55;

     if (Saturation = 0) or (Value = 0) then Hue := 0;
     if (Hue < 0) then Hue := Hue + 360;
     if (Hue >= 360) then Hue := Hue - 360;

     xHueSlider.Value := Hue;
     xSaturationSlider.Value := Saturation;
     xValueSlider.Value := Value;
end;

procedure HSVtoRGB();
var  Angle, Ur, Radius, Vr, Wr : Real;
     Red, Green Blue : Real;
     Rdim : Real;

begin
     Angle := (xHueSlider.Value - 150) * Pi / 180;
     Ur := xValueSlider.Value * 2.55;
     Radius := Ur * Tan(xSaturationSlider.Value * ArcTan(Sqrt(6)/100));
     Vr := Radius * Cos(Angle) / Sqrt(2);
     Wr := Radius * Sin(Angle) / Sqrt(6);

     Red   := Ur - Vr - Wr;
     Green := Ur + Vr - Wr;
     Blue  := Ur + Wr + Wr;

     if Red < 0 then
     begin
          Rdim  := Ur / (Vr + Wr);
          Red   := 0;
          Green := Ur + (Vr - Wr) * Rdim;
          Blue  := Ur + 2 * Wr * Rdim;
     end;

     if Green < 0 then
     begin
          Rdim  := -Ur / (Vr - Wr);
          Red   := Ur - (Vr + Wr) * Rdim;
          Green := 0;
          Blue  := Ur + 2 * Wr * Rdim;
     end;

     if Blue < 0 then
     begin
          Rdim  := -Ur / (Wr + Wr);
          Red   := Ur - (Vr + Wr) * Rdim;
          Green := Ur + (Vr - Wr) * Rdim;
          Blue  := 0;
     end;

     if Red > 255 then
     begin
          Rdim  := (Ur - 255) / (Vr + Wr);
          Red   := 255;
          Green := Ur + (Vr - Wr) * Rdim;
          Blue  := Ur + 2 * Wr * Rdim;
     end;

     if Green > 255 then
     begin
          Rdim  := (255 - Ur) / (Vr - Wr);
          Red   := Ur - (Vr + Wr) * Rdim;
          Green := 255;
          Blue  := Ur + 2 * Wr * Rdim;
     end;

     if Blue > 255 then
     begin
          Rdim  := (255 - Ur) / (Wr + Wr);
          Red   := Ur - (Vr + Wr) * Rdim;
          Green := Ur + (Vr - Wr) * Rdim;
          Blue  := 255;
     end;

     xRedSlider.Value    := Red;
     xGreenSlider.Value  := Green;
     xBlueSlider.Value   := Blue;
end;

procedure TxMainForm.RGBChange(Sender: TObject);
begin
     //Don't do anything if in the middle of an HSV change
     if HSVUpdate = True then Exit;
     RGBUpdate := True;
     RGBtoHSV();
     RGBUpdate := False;
end;

procedure TxMainForm.HSVChange(Sender: TObject);
begin
     //Don't do anything if in the middle of an RGB change
     if RGBUpdate = True then Exit;
     HSVUpdate := True;
     HSVtoRGB();
     HSVUpdate := False;
end;

procedure TxMainForm.FormEnter(Sender: TObject);
begin
      HSVUpdate := False;
      RGBUpdate := False;
end;




