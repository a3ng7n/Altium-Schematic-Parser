procedure TIOForm.TrackBar_InputGainChange(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_InputGain, TrackBar_InputGain, Label_InputGain);
end;

procedure TIOForm.TrackBar_OutputGainChange(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_OutputGain, TrackBar_OutputGain, Label_OutputGain);
end;

procedure UpdateDelayValue(aLink : ISignalLink; aTrackbar : TInstrumentTrackBar; aLabel : TInstrumentLabel);
begin
    aLink.Value := aTrackBar.Value;
    aLabel.Caption := FloatToStrF((aTrackBar.Value / 255.0 * 65536 / 44000), ffFixed, 4, 1) + ' s';      // Delay value 64K buffer 44KHz sample rate
end;

procedure TIOForm.TrackBar_EchoDelayChange(Sender: TObject);
begin
    UpdateDelayValue(Link_EchoDelay, TrackBar_EchoDelay, Label_EchoDelay);
end;

procedure TIOForm.TrackBar_EchoLevelChange(Sender: TObject);
begin
    UpdateFromTrackbar_0_8(Link_EchoLevel, TrackBar_EchoLevel, Label_EchoLevel);
end;

procedure TIOForm.TrackBar_DelayDelayChange(Sender: TObject);
begin
    UpdateDelayValue(Link_DelayDelay, TrackBar_DelayDelay, Label_DelayDelay);
end;

procedure TIOForm.TrackBar_DelayLevelChange(Sender: TObject);
begin
    UpdateFromTrackbar_0_8(Link_DelayLevel, TrackBar_DelayLevel, Label_DelayLevel);
end;

procedure TIOForm.IOFormShow(Sender: TObject);
begin
    TrackBar_InputGain .Value := 10;
    TrackBar_OutputGain.Value := 10;
end;

procedure TIOForm.InstrumentButton1Click(Sender: TObject);
begin
    EqualizerForm.Show;
end;

procedure TIOForm.InstrumentButton2Click(Sender: TObject);
begin
    StatusForm.Show;
end;

procedure TIOForm.InstrumentButton3Click(Sender: TObject);
begin
    FilterDebugForm.Show;
end;


