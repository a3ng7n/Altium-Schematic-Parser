////////// INSTRUMENTS //////////

var
    MI      : IMemoryInstrument;
    Probe   : IMemoryInstrument;
    FreqGen : IFrequencyGeneratorInstrument;
    FreqCnt : IFrequencyCounterInstrument;
    Term    : ITerminalInstrument;
    Switch  : ICrosspointSwitchInstrument;

    TermChar : Char;

function MemoryInstrument : IMemoryInstrument;
begin
    if MI = nil then
        MI := MemoryInstrumentManager.GetMemoryInstrumentByDesignator('MI1');
    Result := MI;
end;

function ProbeInstrument : IMemoryInstrument;
begin
    if Probe = nil then
        Probe := MemoryInstrumentManager.GetMemoryInstrumentByDesignator('PROBE1');
    Result := Probe;
end;

function FreqGenInstrument : IFrequencyGeneratorInstrument;
begin
    if FreqGen = nil then
        FreqGen := FrequencyGeneratorInstrumentManager.GetInstrumentByDesignator('FREQGEN1');
    Result := FreqGen;
end;

function FreqCntInstrument : IFrequencyCounterInstrument;
begin
    if FreqCnt = nil then
        FreqCnt := FrequencyCounterInstrumentManager.GetInstrumentByDesignator('FREQCNT1');
    Result := FreqCnt;
end;

function TerminalInstrument : ITerminalInstrument;
begin
    if Term = nil then
        Term := TerminalInstrumentManager.GetInstrumentByDesignator('T1');
    Result := Term;
end;

function SwitchInstrument : ICrosspointSwitchInstrument;
begin
    if Switch = nil then
        Switch := CrosspointSwitchInstrumentManager.GetInstrumentByDesignator('CPS1');
    Result := Switch;
end;

////////// COMMON //////////

function GetBaseFolder : string;
begin
    Result := ExtractFilePath(GetRunningScriptProjectName);
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
    MI      := nil;
    Probe   := nil;
    FreqGen := nil;
    FreqCnt := nil;
    Term    := nil;
    Switch  := nil;

    TermChar := #33;
end;

procedure TForm1.SignalLinkManager1Poll(Sender: TObject);
var
    S          : string;
    InputName  : widestring;
    OutputName : widestring;
    i          : integer;
begin
    Label_FreqGen_CurFreq    .Caption := IntToStr (FreqGenInstrument.Frequency);
    Label_FreqGen_CurTimeBase.Caption := IntToStr (FreqGenInstrument.TimeBase);
    Label_FreqGen_Suspended  .Caption := BoolToStr(FreqGenInstrument.Suspended, True);

    Label_FreqCnt_CurTimeBase.Caption := IntToStr (FreqCntInstrument.TimeBase);
    if FreqCntInstrument.ChannelA.EdgePolarity = eEdgeRising then
        Label_FreqCntA_EdgePolarity.Caption := 'Rising'
    else
        Label_FreqCntA_EdgePolarity.Caption := 'Falling';
    if FreqCntInstrument.ChannelB.EdgePolarity = eEdgeRising then
        Label_FreqCntB_EdgePolarity.Caption := 'Rising'
    else
        Label_FreqCntB_EdgePolarity.Caption := 'Falling';
    case FreqCntInstrument.ChannelA.MeasureMode of
        eMeasureMode_Frequency : Label_FreqCntA_MeasureMode.Caption := 'Frequency';
        eMeasureMode_Period    : Label_FreqCntA_MeasureMode.Caption := 'Period';
        eMeasureMode_Count     : Label_FreqCntA_MeasureMode.Caption := 'Count';
    end;
    case FreqCntInstrument.ChannelB.MeasureMode of
        eMeasureMode_Frequency : Label_FreqCntB_MeasureMode.Caption := 'Frequency';
        eMeasureMode_Period    : Label_FreqCntB_MeasureMode.Caption := 'Period';
        eMeasureMode_Count     : Label_FreqCntB_MeasureMode.Caption := 'Count';
    end;
    Label_FreqCntA_Gating    .Caption := FloatToStr (FreqCntInstrument.ChannelA.Gating);
    Label_FreqCntB_Gating    .Caption := FloatToStr (FreqCntInstrument.ChannelB.Gating);
    Label_FreqCntA_Suspended .Caption := BoolToStr  (FreqCntInstrument.ChannelA.Suspended, True);
    Label_FreqCntB_Suspended .Caption := BoolToStr  (FreqCntInstrument.ChannelB.Suspended, True);
    Label_FreqCntA_Measured  .Caption := FloatToStr (FreqCntInstrument.ChannelA.Measured);
    Label_FreqCntB_Measured  .Caption := FloatToStr (FreqCntInstrument.ChannelB.Measured);

    S := '';
    InputName  := '';
    OutputName := '';
    For i := 0 To SwitchInstrument.GetConnectionCount - 1 Do
    Begin
        If S <> '' Then
            S := S + ', ';
        SwitchInstrument.GetConnection(i, InputName, OutputName);
        S := S + InputName + ' -> ' +  OutputName;
    End;
    Label_CPS_Connectivity.Caption := S;
end;

////////// MEMORY UTILS //////////

procedure TestMemoryWrite(const MI : IMemoryInstrument);
var
    Buffer : PByteArray;
    i : integer;
begin
    if MI = nil then exit;
    Buffer := AllocateByteArray(26);
    For i := 0 To 25 Do
        SetByteArrayElement(Buffer, i, Ord('A') + i);
    MI.Write(Buffer, 0, 26);
    FreeByteArray(Buffer);
end;

procedure TestMemoryRead(const MI : IMemoryInstrument);
var
    Buffer : PByteArray;
    i : integer;
    S : String;
    V : Byte;
begin
    if MI = nil then exit;
    Buffer := AllocateByteArray(16);
    MI.Read(Buffer, 0, 16);
    S := 'Data:'#13#10;
    For i := 0 To 15 Do
    Begin
        V := GetByteArrayElement(Buffer, i);
        S := S + #13#10 + IntToStr(i) + ': ' + IntToStr(V);
    End;
    FreeByteArray(Buffer);
    ShowInfo(S);
end;

procedure TestMemoryLoad(const MI : IMemoryInstrument);
begin
    if MI = nil then exit;
    MI.LoadFromFile(GetBaseFolder + 'NBIMain.pas', 0, 64);
end;

procedure TestMemorySave(const MI : IMemoryInstrument);
begin
    if MI = nil then exit;
    MI.SaveToFile(GetBaseFolder + 'mem.bin', 0, 64);
end;

////////// MEMORY INSTRUMENT //////////

procedure TForm1.InstrumentButton1Click(Sender: TObject);
begin
    TestMemoryRead(MemoryInstrument);
end;

procedure TForm1.InstrumentButton2Click(Sender: TObject);
begin
    TestMemoryWrite(MemoryInstrument);
end;

procedure TForm1.InstrumentButton3Click(Sender: TObject);
begin
    TestMemoryLoad(MemoryInstrument);
end;

procedure TForm1.InstrumentButton4Click(Sender: TObject);
begin
    TestMemorySave(MemoryInstrument);
end;

////////// WB_PROBE //////////

procedure TForm1.InstrumentButton5Click(Sender: TObject);
begin
    TestMemoryRead(ProbeInstrument);
end;

procedure TForm1.InstrumentButton6Click(Sender: TObject);
begin
    TestMemoryWrite(ProbeInstrument);
end;

procedure TForm1.InstrumentButton7Click(Sender: TObject);
begin
    TestMemoryLoad(ProbeInstrument);
end;

procedure TForm1.InstrumentButton8Click(Sender: TObject);
begin
    TestMemorySave(ProbeInstrument);
end;

////////// FREQUENCY GENERATOR //////////

procedure TForm1.InstrumentButton9Click(Sender: TObject);
begin
    FreqGenInstrument.Suspended := not FreqGenInstrument.Suspended;
    if FreqGenInstrument.Suspended then
        InstrumentButton9.Caption := 'Resume'
    else
        InstrumentButton9.Caption := 'Suspend';
end;

procedure TForm1.InstrumentButton10Click(Sender: TObject);
begin
    if FreqGenInstrument.TimeBase = 50000000 then
        FreqGenInstrument.TimeBase := 25000000
    else
        FreqGenInstrument.TimeBase := 50000000;
end;

procedure TForm1.InstrumentButton11Click(Sender: TObject);
begin
    if FreqGenInstrument.Frequency = 1000000 then
        FreqGenInstrument.Frequency := 1
    else
        FreqGenInstrument.Frequency := 1000000;
end;

////////// FREQUENCY COUNTER //////////

procedure TForm1.InstrumentButton12Click(Sender: TObject);
begin
    if FreqCntInstrument.TimeBase = 50000000 then
        FreqCntInstrument.TimeBase := 25000000
    else
        FreqCntInstrument.TimeBase := 50000000;
end;

////////// TERMINAL //////////

procedure TForm1.InstrumentButton13Click(Sender: TObject);
begin
    TerminalInstrument.PutChar(TermChar);
    TermChar := Chr(Ord(TermChar) + 1);
end;

procedure TForm1.InstrumentButton14Click(Sender: TObject);
begin
    TerminalInstrument.PutString('This line is added from the script');
end;

procedure TForm1.InstrumentButton15Click(Sender: TObject);
begin
    TerminalInstrument.SaveContent(GetBaseFolder + 'terminal.txt');
end;

procedure TForm1.InstrumentButton16Click(Sender: TObject);
begin
    ShowMessage(TerminalInstrument.GetContent);
end;

////////// CROSSPOINT SWITCH //////////

procedure TForm1.InstrumentButton17Click(Sender: TObject);
begin
    SwitchInstrument.BeginReconnect;
    if SwitchInstrument.ConnectionExists('BRIGHTNESS', 'DEBUG') then
    begin
        SwitchInstrument.RemoveConnection('BRIGHTNESS', 'DEBUG');
        SwitchInstrument.AddConnection   ('CONTRAST'  , 'DEBUG');
    end
    else
    begin
        SwitchInstrument.RemoveConnection('CONTRAST'  , 'DEBUG');
        SwitchInstrument.AddConnection   ('BRIGHTNESS', 'DEBUG');
    end;
    SwitchInstrument.EndReconnect;
end;

