procedure TEqualizerForm.TrackBar_Ch00Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh00, TrackBar_Ch00, Label_GainCh00);
end;

procedure TEqualizerForm.TrackBar_Ch01Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh01, TrackBar_Ch01, Label_GainCh01);
end;

procedure TEqualizerForm.TrackBar_Ch02Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh02, TrackBar_Ch02, Label_GainCh02);
end;

procedure TEqualizerForm.TrackBar_Ch03Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh03, TrackBar_Ch03, Label_GainCh03);
end;

procedure TEqualizerForm.TrackBar_Ch04Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh04, TrackBar_Ch04, Label_GainCh04);
end;

procedure TEqualizerForm.TrackBar_Ch05Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh05, TrackBar_Ch05, Label_GainCh05);
end;

procedure TEqualizerForm.TrackBar_Ch06Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh06, TrackBar_Ch06, Label_GainCh06);
end;

procedure TEqualizerForm.TrackBar_Ch07Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh07, TrackBar_Ch07, Label_GainCh07);
end;

procedure TEqualizerForm.TrackBar_Ch08Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh08, TrackBar_Ch08, Label_GainCh08);
end;

procedure TEqualizerForm.TrackBar_Ch09Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh09, TrackBar_Ch09, Label_GainCh09);
end;

procedure TEqualizerForm.TrackBar_Ch10Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh10, TrackBar_Ch10, Label_GainCh10);
end;

procedure TEqualizerForm.TrackBar_Ch11Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh11, TrackBar_Ch11, Label_GainCh11);
end;

procedure TEqualizerForm.TrackBar_Ch12Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh12, TrackBar_Ch12, Label_GainCh12);
end;

procedure TEqualizerForm.TrackBar_Ch13Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh13, TrackBar_Ch13, Label_GainCh13);
end;

procedure TEqualizerForm.TrackBar_Ch14Change(Sender: TObject);
begin
    UpdateFromTrackbar_4_12(Link_EQGainCh14, TrackBar_Ch14, Label_GainCh14);
end;

procedure TEqualizerForm.AudioProcessorFormShow(Sender: TObject);
begin
    TrackBar_Ch00  .Value := 10;
    TrackBar_Ch01  .Value := 10;
    TrackBar_Ch02  .Value := 10;
    TrackBar_Ch03  .Value := 10;
    TrackBar_Ch04  .Value := 10;
    TrackBar_Ch05  .Value := 10;
    TrackBar_Ch06  .Value := 10;
    TrackBar_Ch07  .Value := 10;
    TrackBar_Ch08  .Value := 10;
    TrackBar_Ch09  .Value := 10;
    TrackBar_Ch10  .Value := 10;
    TrackBar_Ch11  .Value := 10;
    TrackBar_Ch12  .Value := 10;
    TrackBar_Ch13  .Value := 10;
    TrackBar_Ch14  .Value := 10;
    Check_EQEnable.Checked := True;
    Check_EQDeltaOnly.Checked := False;
end;

procedure TEqualizerForm.Check_EQEnableClick(Sender: TObject);
begin
    Check_EQDeltaOnly.Enabled := Check_EQEnable.Checked;
end;


