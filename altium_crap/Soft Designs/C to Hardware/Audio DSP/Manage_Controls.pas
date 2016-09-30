const
    cGain_ExpFactor = 0.840896415;
    cGain_ScaledOne_4_12 = $1000;
    cGain_ScaledOne_0_8 = $FF;

procedure UpdateFromTrackbar_4_12(aLink : ISignalLink; aTrackbar : TInstrumentTrackBar; aLabel : TInstrumentLabel);
var
    Gain      : Double;
    Scaled    : Cardinal;
    DB        : Double;
begin
    Gain := Power(cGain_ExpFactor, (aTrackBar.MaxValue div 2) - aTrackBar.Value);
    Scaled := Round(cGain_ScaledOne_4_12 * Gain);
    aLink.Value := Scaled;

    DB := 20 * Log10(Gain);
    aLabel.Caption := FloatToStrF(DB, ffFixed, 4, 0) + ' dB';
end;

procedure UpdateFromTrackbar_0_8(aLink : ISignalLink; aTrackbar : TInstrumentTrackBar; aLabel : TInstrumentLabel);
var
    Gain      : Double;
    Scaled    : Cardinal;
    DB        : Double;
begin
    Gain := Power(cGain_ExpFactor, (aTrackBar.MaxValue) - aTrackBar.Value);
    Scaled := Round(cGain_ScaledOne_0_8 * Gain);
    aLink.Value := Scaled;

    DB := 20 * Log10(Gain/2.0);
    aLabel.Caption := FloatToStrF(DB, ffFixed, 4, 0) + ' dB';
end;

// widths <= 32 supported
function ValueFromSignedSignal(aLink : SignalLink; aWidth : Integer) : Integer;
var
    ShiftedUnsigned32Bit : Cardinal;
begin
    ShiftedUnsigned32Bit := aLink.Value Shl (32 - aWidth);
    Result := ShiftedUnsigned32Bit;
    Result := Result div Power(2, (32 - aWidth));
end;

function ValueFromSignedFixedPointSignal(aLink : SignalLink; aWidth : Integer; aFractionBits : Integer) : Double;
begin
    Result := ValueFromSignedSignal(aLink, aWidth);
    Result := Result / Power(2, (aFractionBits));
end;
