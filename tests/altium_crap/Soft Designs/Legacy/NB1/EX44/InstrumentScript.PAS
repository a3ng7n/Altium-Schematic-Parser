Unit Form;

Interface

Uses
    Windows,
    System,
    StrUtils,
    Registry,
    Messages,
    StrUtils,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    Utility,
    ComCtrls,
    StdCtrls,
    ExtCtrls;

Type
{..........................................................................................................}
TAutoTester = Class(TForm)
    EditBox                : TEdit;
    Button_Close           : TButton;
    Bevel1                 : TBevel;

    CheckBox_Voice1        : TCheckBox;
    CheckBox_Voice2        : TCheckBox;
    CheckBox_Voice3        : TCheckBox;
    CheckBox_Voice5        : TCheckBox;
    CheckBox_Voice4        : TCheckBox;
    CheckBox_Voice6        : TCheckBox;
    CheckBox_Voice7        : TCheckBox;
    CheckBox_Voice8        : TCheckBox;

    GroupBox_Scale         : TGroupBox;
    RadioGroup_ScaleId     : TRadioGroup;
    RadioGroup_ScaleNote   : TRadioGroup;
    RadioGroup_ScaleOctave : TRadioGroup;
    Edit_ScaleMidi         : TEdit;
    UpDown_ScaleMidi       : TUpDown;
    UpDown_ScaleRate       : TUpDown;
    Edit_ScaleRate         : TEdit;

    GroupBox_Chord         : TGroupBox;
    RadioGroup_ChordId     : TRadioGroup;
    RadioGroup_ChordNote   : TRadioGroup;
    RadioGroup_ChordOctave : TRadioGroup;
    Button_ChordOn         : TButton;
    Button_Off             : TButton;

    Label_Chord            : TLabel;
    Label_Rate             : TLabel;

    Edit_ScaleMidi         : TEdit;
    UpDown_ScaleMidi       : TUpDown;
    TrackBar_ScaleRate     : TTrackBar;

    GroupBox_V1            : TGroupBox;
    RadioGroup_Octave_V1   : TRadioGroup;
    RadioGroup_Note_V1     : TRadioGroup;
    Label_V1               : TLabel;
    Edit_V1                : TEdit;
    UpDown_V1              : TUpDown;
    TrackBar_V1            : TTrackBar;

    GroupBox_V2            : TGroupBox;
    RadioGroup_Octave_V2   : TRadioGroup;
    RadioGroup_Note_V2     : TRadioGroup;
    Label_V2               : TLabel;
    Edit_V2                : TEdit;
    UpDown_V2              : TUpDown;
    TrackBar_V2            : TTrackBar;

    GroupBox_V3            : TGroupBox;
    RadioGroup_Octave_V3   : TRadioGroup;
    RadioGroup_Note_V3     : TRadioGroup;
    Label_V3               : TLabel;
    Edit_V3                : TEdit;
    UpDown_V3              : TUpDown;
    TrackBar_V3            : TTrackBar;

    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TrackBar_Volume_Scale: TTrackBar;
    TrackBar_Volume_Chord: TTrackBar;
    TrackBar_Volume_Voice1: TTrackBar;
    TrackBar_Volume_Voice2: TTrackBar;
    TrackBar_Volume_Voice3: TTrackBar;
    Procedure Button_CloseClick  (Sender : TObject);
    Procedure AutoTesterMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y: Integer);
    Procedure AutoTesterMouseUp  (Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    Procedure AutoTesterMouseMove(Sender : TObject; Shift                        : TShiftState; X, Y : Integer);

    Procedure AutoTesterShow     (Sender : TObject);

    Procedure CheckBox_VoiceClick(Sender: TObject);
    Procedure Edit_ScaleRateChange(Sender: TObject);
    Procedure UpDown_ScaleRateClick(Sender: TObject; Button: TUDBtnType);

    Procedure RadioGroup_ScaleIdClick    (Sender: TObject);
    Procedure RadioGroup_ScaleNoteClick  (Sender: TObject);
    Procedure RadioGroup_ScaleOctaveClick(Sender: TObject);

    Procedure RadioGroup_ChordIdClick    (Sender: TObject);
    Procedure RadioGroup_ChordNoteClick  (Sender: TObject);
    Procedure RadioGroup_ChordOctaveClick(Sender: TObject);
    Procedure TrackBar_ScaleRateChange   (Sender: TObject);

    Procedure Button_ChordOnClick        (Sender: TObject);
    Procedure Button_ChordOffClick       (Sender: TObject);

    Procedure RadioGroup_Note_V1Click    (Sender: TObject);
    Procedure RadioGroup_Octave_V1Click  (Sender: TObject);
    Procedure UpDown_V1Click             (Sender: TObject; Button: TUDBtnType);
    Procedure Edit_V1Change              (Sender: TObject);
    Procedure TrackBar_V1Change          (Sender: TObject);

    Procedure RadioGroup_Note_V2Click    (Sender: TObject);
    Procedure RadioGroup_Octave_V2Click  (Sender: TObject);
    Procedure UpDown_V2Click             (Sender: TObject; Button: TUDBtnType);
    Procedure Edit_V2Change              (Sender: TObject);
    Procedure TrackBar_V2Change          (Sender: TObject);

    Procedure RadioGroup_Note_V3Click    (Sender: TObject);
    Procedure RadioGroup_Octave_V3Click  (Sender: TObject);
    Procedure UpDown_V3Click             (Sender: TObject; Button: TUDBtnType);
    Procedure Edit_V3Change              (Sender: TObject);
    Procedure TrackBar_V3Change          (Sender: TObject);

    Procedure TrackBar_Volume_ScaleChange(Sender: TObject);
    Procedure TrackBar_Volume_VoiceChange(Sender: TObject);
Private
    Function  SetChannelEnable(ChannelNo : Integer; Enable : Boolean) : Boolean;
    Function  SetChannelVolume(ChannelNo : Integer; Value  : Byte   ) : Boolean;
    Function  SetChordRoot    (Value     : Integer) : Boolean;
    Function  SetChordId      (Value     : Integer) : Boolean;
    Function  SetScaleRoot    (Value     : Integer) : Boolean;
    Function  SetScaleId      (Value     : Integer) : Boolean;
    Function  SetScaleRate    (Value     : Integer) : Boolean;
    Function  SetScaleVolume  (Value     : Integer) : Boolean;
    Function  SetVoice        (Voice     : Integer;
                               Value     : Integer) : Boolean;
    Procedure UpdateVoice1    (Value     : Integer);
    Procedure UpdateVoice2    (Value     : Integer);
    Procedure UpdateVoice3    (Value     : Integer);

    Procedure UpdateScaleFromForm;
    Procedure UpdateChordFromForm;

    FDeviceLink : Pointer;
    FDragging   : Boolean;
    FLastX      : Integer;
    FLastY      : Integer;
    Changing    : Boolean;
End;
{..........................................................................................................}

Var
{..........................................................................................................}
AutoTester: TAutoTester;
{..........................................................................................................}

Implementation

Const
{..........................................................................................................}
cInstrumentDesignator_Control       = 'OTHER_CTRL';
cInstrumentDesignator_Voices        = 'VOICE_CTRL';
cInstrumentDesignator_Chords        = 'CHORD_CTRL';
cInstrumentDesignator_Scales        = 'SCALE_CTRL';
cInstrumentDesignator_Volume        = 'VOICE_VOLUME';
{..........................................................................................................}

{..........................................................................................................}
cInstrumentChannel_Enables          = 0;
cInstrumentChannel_Volume           = 1;
{..........................................................................................................}

{..........................................................................................................}
cInstrumentChannel_VoiceEnable1     = 0;
cInstrumentChannel_VoiceEnable2     = 1;
cInstrumentChannel_VoiceEnable3     = 2;
cInstrumentChannel_VoiceEnable4     = 3;
cInstrumentChannel_VoiceEnable5     = 4;
cInstrumentChannel_VoiceEnable6     = 5;
cInstrumentChannel_VoiceEnable7     = 6;
cInstrumentChannel_VoiceEnable8     = 7;
{..........................................................................................................}

{..........................................................................................................}
cInstrumentChannel_ScaleRoot        = 0;
cInstrumentChannel_ScaleRate        = 1;
cInstrumentChannel_ScaleId          = 2;
cInstrumentChannel_ScaleVolume      = 3;
{..........................................................................................................}

{..........................................................................................................}
cInstrumentChannel_ChordRoot        = 0;
cInstrumentChannel_ChordId          = 1;
{..........................................................................................................}

{..........................................................................................................}
Function  SoftDeviceCount                             : Integer; Forward;
Function  GetDeviceName       (AIndex   : Integer    ): String ; Forward;
Procedure RunInstrumentScript;                                   Forward;
Procedure OpenDevicesPage;                                       Forward;
Procedure ShowAllInstruments;                                    Forward;
Procedure ShowInstrument      (AIndex    : Integer    );         Forward;
{..........................................................................................................}

{$R *.DFM}

{..........................................................................................................}
Procedure TAutoTester.AutoTesterShow(Sender : TObject);
Begin
    Changing := False;
End;
{..........................................................................................................}

{..........................................................................................................}
Function ReadWriteDeviceValue(ChannelNo : Integer;
                              DeviceNo  : Integer;
                              DoWrite   : Boolean;
                              Value     : LongWord): LongWord;
Var
    NexusWorkBench    : INexusWorkbench;
    NexusDevice       : INexusDevice;
    DeviceLink        : IDeviceLink;
    ProcessorRegister : IProcessorRegister;
Begin
    Result := 0;

    NexusWorkBench := GetNexusWorkbench;
    DeviceLink     := NexusWorkBench.GetSoftDevice(DeviceNo);
    NexusDevice    := DeviceLink.GetNexusDevice;

    ProcessorRegister := NexusDevice.GetProcessorRegister(ChannelNo);
    If ProcessorRegister <> Nil Then
    Begin
        Try
            NexusDevice.TemporarySuspend;
            If DoWrite Then           ProcessorRegister.SetValue(Value)
                       Else Result := ProcessorRegister.GetValue;
            NexusDevice.ContinueFromTemporarySuspend;  //If the device is a processor then reading this value will pause the device
        Finally
        End;
    End;
End;
{..........................................................................................................}

{..........................................................................................................}
Function ReadValueFromDevice(ChannelNo : Integer; DeviceNo: Integer): LongWord;
Begin
    Result := ReadWriteDeviceValue(ChannelNo,DeviceNo,False,0);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure WriteValueToDevice(ChannelNo : Integer; DeviceNo : Integer; Value : LongWord);
Begin
    ReadWriteDeviceValue(ChannelNo,DeviceNo,True,Value);
End;
{..........................................................................................................}

{..........................................................................................................}
Function SoftDeviceCount : Integer;
Var
    NexusWorkBench : INexusWorkbench;
Begin
    NexusWorkBench := GetNexusWorkbench;
    If NexusWorkBench.GetSoftDeviceCount > 0 Then Result := NexusWorkBench.GetSoftDeviceCount
                                             Else Result := 0;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.Button_CloseClick(Sender: TObject);
Begin
    Close;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.AutoTesterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
    //Allows form to be moved by clicking anywhere on the form
    FDragging := True;
    FLastx := Mouse.CursorPos.X;
    FLastY := Mouse.CursorPos.Y;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.AutoTesterMouseMove (Sender : TObject; Shift : TShiftState; X, Y : Integer);
Begin
    //Allows form to be moved by clicking anywhere on the form
    If Not FDragging Then Exit;
    AutoTester.Left := AutoTester.Left + (Mouse.CursorPos.X - FLastX);
    AutoTester.Top  := AutoTester.Top  + (Mouse.CursorPos.Y - FLastY);

    FLastx := Mouse.CursorPos.X;
    FLastY := Mouse.CursorPos.Y;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.AutoTesterMouseUp (Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
Begin
    //Allows form to be moved by clicking anywhere on the form
    FDragging := False;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure OpenDevicesPage;
Begin
    ResetParameters;

    AddStringParameter('ObjectKind','ControlPanel');
    AddStringParameter('Index', '1');

    RunProcess('WorkspaceManager:OpenObject');
End;
{..........................................................................................................}

{..........................................................................................................}
Function GetDeviceName(AIndex : Integer) : String;
Var
    NexusWorkBench : INexusWorkbench;
    DeviceLink     : IDeviceLink;
Begin
    NexusWorkBench := GetNexusWorkbench;
    DeviceLink     := NexusWorkBench.GetSoftDevice(AIndex);
    Result         := DeviceLink.GetDeviceName;
End;
{..........................................................................................................}

{..........................................................................................................}
Function GetDeviceDesignator(AIndex : Integer) : String;
Var
    NexusWorkBench    : INexusWorkbench;
    DeviceLink        : IDeviceLink;
    NexusCore         : INexusCore;
Begin
    NexusWorkBench := GetNexusWorkbench;
    DeviceLink     := NexusWorkBench.GetSoftDevice(AIndex);
    NexusCore      := DeviceLink.GetNexusCore;
    Result         := NexusCore.GetComponentDesignator;
End;
{..........................................................................................................}

{..........................................................................................................}
Function DeviceLink(AIndex : Integer) : String;
Begin
    Result := GetNexusWorkbench.GetSoftDevice(AIndex);
End;
{..........................................................................................................}

{..........................................................................................................}
Function GetDeviceIndexFromDesignator(InstrumentDesignator : String) : Integer;
Var
    i : Integer;
Begin
    Result := Nil;
    For i := 0 To SoftDeviceCount - 1 Do
    Begin
        If SameText(InstrumentDesignator,GetDeviceDesignator(i)) Then
        Begin
            Result := i;
            Exit;
        End;
    End;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure ShowInstrument(AIndex : Integer);
Begin
    ResetParameters;

    AddStringParameter('Target', 'SoftDevice'       );
    AddStringParameter('Action', 'ShowViewer'       );
    AddStringParameter('Index' ,  IntToStr(AIndex+1));

    RunProcess('FPGAFlow:DeviceAction');
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure ShowAllInstruments;
Var
    DeviceIndex : Integer;
Begin
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Control);
    If DeviceIndex >= 0 Then ShowInstrument(DeviceIndex);

    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Voices);
    If DeviceIndex >= 0 Then ShowInstrument(DeviceIndex);

    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Scales);
    If DeviceIndex >= 0 Then ShowInstrument(DeviceIndex);

    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Volume);
    If DeviceIndex >= 0 Then ShowInstrument(DeviceIndex);

    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Chords);
    If DeviceIndex >= 0 Then ShowInstrument(DeviceIndex);
End;
{..........................................................................................................}

{..........................................................................................................}
Function TAutoTester.SetChannelEnable(ChannelNo : Integer; Enable : Boolean) : Boolean;
Var
    N               : Integer;
    DeviceIndex     : Integer;
    VoiceEnableMask : LongWord;
Begin
    Result      := False;
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Control);
    If DeviceIndex < 0 Then Exit;

    Result          := True;
    VoiceEnableMask := ReadValueFromDevice(cInstrumentChannel_Enables,DeviceIndex);

    N := 1 Shl ChannelNo;
    If Enable Then VoiceEnableMask := VoiceEnableMask Or N
              Else VoiceEnableMask := VoiceEnableMask And (Not N);

    WriteValueToDevice(cInstrumentChannel_Enables,DeviceIndex,VoiceEnableMask);
End;
{..........................................................................................................}

{..........................................................................................................}
Function TAutoTester.SetChordRoot(Value : Integer) : Boolean;
Var
    DeviceIndex : Integer;
Begin
    Result      := False;
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Chords);
    If DeviceIndex < 0 Then Exit;

    Result := True;
    WriteValueToDevice(cInstrumentChannel_ChordRoot,DeviceIndex,Value);
End;
{..........................................................................................................}

{..........................................................................................................}
Function TAutoTester.SetChordId(Value : Integer) : Boolean;
Var
    DeviceIndex : Integer;
Begin
    Result      := False;
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Chords);
    If DeviceIndex < 0 Then Exit;

    Result := True;
    WriteValueToDevice(cInstrumentChannel_ChordId,DeviceIndex,Value);
End;
{..........................................................................................................}

{..........................................................................................................}
Function TAutoTester.SetScaleRoot(Value : Integer) : Boolean;
Var
    DeviceIndex : Integer;
Begin
    Result      := False;
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Scales);
    If DeviceIndex < 0 Then Exit;

    Result := True;
    WriteValueToDevice(cInstrumentChannel_ScaleRoot,DeviceIndex,Value);
End;
{..........................................................................................................}

{..........................................................................................................}
Function TAutoTester.SetScaleId(Value : Integer) : Boolean;
Var
    DeviceIndex : Integer;
Begin
    Result      := False;
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Scales);
    If DeviceIndex < 0 Then Exit;

    Result := True;
    WriteValueToDevice(cInstrumentChannel_ScaleId,DeviceIndex,Value);
End;
{..........................................................................................................}

{..........................................................................................................}
Function TAutoTester.SetScaleRate(Value : Integer) : Boolean;
Var
    DeviceIndex : Integer;
Begin
    Result      := False;
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Scales);
    If DeviceIndex < 0 Then Exit;

    Result := True;
    WriteValueToDevice(cInstrumentChannel_ScaleRate,DeviceIndex,Value);
End;
{..........................................................................................................}

{..........................................................................................................}
Function TAutoTester.SetScaleVolume(Value : Integer) : Boolean;
Var
    DeviceIndex : Integer;
Begin
    Result      := False;
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Scales);
    If DeviceIndex < 0 Then Exit;

    Result := True;
    WriteValueToDevice(cInstrumentChannel_ScaleVolume,DeviceIndex,Value);
End;
{..........................................................................................................}

{..........................................................................................................}
Function TAutoTester.SetVoice(Voice : Integer; Value : Integer) : Boolean;
Var
    DeviceIndex : Integer;
Begin
    Result      := False;
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Voices);
    If DeviceIndex < 0 Then Exit;

    Result := True;
    WriteValueToDevice(Voice,DeviceIndex,Value);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.CheckBox_VoiceClick(Sender: TObject);
Var
    CheckBox   : TCheckBox;
Begin
    CheckBox := Sender;
    SetChannelEnable(CheckBox.Tag - 1,CheckBox.Checked);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure RunInstrumentScript;
Begin
    OpenDevicesPage;
    ShowAllInstruments;
    AutoTester.ShowModal;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.Edit_ScaleRateChange(Sender: TObject);
Begin
    UpdateScaleFromForm;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_ScaleIdClick(Sender: TObject);
Begin
    UpdateScaleFromForm;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.UpdateScaleFromForm;
Begin
    //Set rate between 30 and 255 based on TrackBar between 1 and 100
    SetScaleRate(((100-TrackBar_ScaleRate.Position) * 2.25) + 30);
    SetScaleId  (RadioGroup_ScaleId.ItemIndex);
    SetScaleRoot(RadioGroup_ScaleNote.ItemIndex + (RadioGroup_ScaleOctave.ItemIndex * 12));
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.UpdateChordFromForm;
Begin
    SetChordId  (RadioGroup_ChordId  .ItemIndex);
    SetChordRoot(RadioGroup_ChordNote.ItemIndex + (RadioGroup_ChordOctave.ItemIndex * 12));
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_ScaleNoteClick(Sender: TObject);
Begin
    UpdateScaleFromForm;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_ScaleOctaveClick(Sender: TObject);
Begin
    UpdateScaleFromForm;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_ChordNoteClick(Sender: TObject);
Begin
    UpdateChordFromForm;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_ChordOctaveClick(Sender: TObject);
Begin
    UpdateChordFromForm;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_ChordIdClick(Sender: TObject);
Begin
    UpdateChordFromForm;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.TrackBar_ScaleRateChange(Sender: TObject);
Begin
    Edit_ScaleRate.Text := IntToStr(TrackBar_ScaleRate.Position);
    UpdateScaleFromForm;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.Button_ChordOnClick(Sender: TObject);
Begin
    CheckBox_Voice4.Checked := True;
    CheckBox_Voice5.Checked := True;
    CheckBox_Voice6.Checked := True;
    CheckBox_Voice7.Checked := True;
    CheckBox_VoiceClick(CheckBox_Voice4);
    CheckBox_VoiceClick(CheckBox_Voice5);
    CheckBox_VoiceClick(CheckBox_Voice6);
    CheckBox_VoiceClick(CheckBox_Voice7);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.Button_ChordOffClick(Sender: TObject);
Begin
    CheckBox_Voice4.Checked := False;
    CheckBox_Voice5.Checked := False;
    CheckBox_Voice6.Checked := False;
    CheckBox_Voice7.Checked := False;
    CheckBox_VoiceClick(CheckBox_Voice4);
    CheckBox_VoiceClick(CheckBox_Voice5);
    CheckBox_VoiceClick(CheckBox_Voice6);
    CheckBox_VoiceClick(CheckBox_Voice7);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.UpdateVoice1(Value : Integer);
Begin
    Changing := True;

    Edit_V1             .Text      := IntToStr(Value);
    UpDown_V1           .Position  := Value;
    TrackBar_V1         .Position  := Value;
    RadioGroup_Note_V1  .ItemIndex := Value Mod 12;
    RadioGroup_Octave_V1.ItemIndex := Value Div 12;

    SetVoice(0,Value);
    Changing := False;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_Note_V1Click(Sender: TObject);
Begin
    If Changing Then Exit;
    UpdateVoice1(RadioGroup_Note_V1.ItemIndex + (RadioGroup_Octave_V1.ItemIndex * 12));
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_Octave_V1Click(Sender: TObject);
Begin
    If Changing Then Exit;
    UpdateVoice1(RadioGroup_Note_V1.ItemIndex + (RadioGroup_Octave_V1.ItemIndex * 12));
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.UpDown_V1Click(Sender: TObject; Button: TUDBtnType);
Begin
    If Changing Then Exit;
    UpdateVoice1(UpDown_V1.Position);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.Edit_V1Change(Sender: TObject);
Var
    N : Integer;
Begin
    If Changing Then Exit;
    N := StrToIntDef(Edit_V1.Text,-1);
    If N > 0 Then
       UpdateVoice1(N);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.TrackBar_V1Change(Sender: TObject);
Begin
    If Changing Then Exit;
    UpdateVoice1(TrackBar_V1.Position);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.UpdateVoice2(Value : Integer);
Begin
    Changing := True;

    Edit_V2             .Text      := IntToStr(Value);
    UpDown_V2           .Position  := Value;
    TrackBar_V2         .Position  := Value;
    RadioGroup_Note_V2  .ItemIndex := Value Mod 12;
    RadioGroup_Octave_V2.ItemIndex := Value Div 12;

    SetVoice(1,Value);
    Changing := False;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_Note_V2Click(Sender: TObject);
Begin
    If Changing Then Exit;
    UpdateVoice2(RadioGroup_Note_V2.ItemIndex + (RadioGroup_Octave_V2.ItemIndex * 12));
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_Octave_V2Click(Sender: TObject);
Begin
    If Changing Then Exit;
    UpdateVoice2(RadioGroup_Note_V2.ItemIndex + (RadioGroup_Octave_V2.ItemIndex * 12));
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.UpDown_V2Click(Sender: TObject; Button: TUDBtnType);
Begin
    If Changing Then Exit;
    UpdateVoice2(UpDown_V2.Position);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.Edit_V2Change(Sender: TObject);
Var
    N : Integer;
Begin
    If Changing Then Exit;
    N := StrToIntDef(Edit_V2.Text,-1);
    If N > 0 Then
       UpdateVoice2(N);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.TrackBar_V2Change(Sender: TObject);
Begin
    If Changing Then Exit;
    UpdateVoice2(TrackBar_V2.Position);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.UpdateVoice3(Value : Integer);
Begin
    Changing := True;

    Edit_V3             .Text      := IntToStr(Value);
    UpDown_V3           .Position  := Value;
    TrackBar_V3         .Position  := Value;
    RadioGroup_Note_V3  .ItemIndex := Value Mod 12;
    RadioGroup_Octave_V3.ItemIndex := Value Div 12;

    SetVoice(2,Value);
    Changing := False;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_Note_V3Click(Sender: TObject);
Begin
    If Changing Then Exit;
    UpdateVoice3(RadioGroup_Note_V3.ItemIndex + (RadioGroup_Octave_V3.ItemIndex * 12));
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.RadioGroup_Octave_V3Click(Sender: TObject);
Begin
    If Changing Then Exit;
    UpdateVoice3(RadioGroup_Note_V3.ItemIndex + (RadioGroup_Octave_V3.ItemIndex * 12));
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.UpDown_V3Click(Sender: TObject; Button: TUDBtnType);
Begin
    If Changing Then Exit;
    UpdateVoice3(UpDown_V3.Position);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.Edit_V3Change(Sender: TObject);
Var
    N : Integer;
Begin
    If Changing Then Exit;
    N := StrToIntDef(Edit_V3.Text,-1);
    If N > 0 Then
       UpdateVoice3(N);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.TrackBar_V3Change(Sender: TObject);
Begin
    If Changing Then Exit;
    UpdateVoice3(TrackBar_V3.Position);
End;
{..........................................................................................................}

{..........................................................................................................}
Function TAutoTester.SetChannelVolume(ChannelNo : Integer; Value : Byte) : Boolean;
Var
    DeviceIndex : Integer;
Begin
    Result      := False;
    DeviceIndex := GetDeviceIndexFromDesignator(cInstrumentDesignator_Volume);
    If DeviceIndex < 0 Then Exit;

    Result := True;
    WriteValueToDevice(ChannelNo,DeviceIndex,Value);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.TrackBar_Volume_VoiceChange(Sender: TObject);
Var
    TrackBar : TTrackBar;
Begin
    If Changing Then Exit;
    TrackBar := Sender;
    SetChannelVolume(TrackBar.Tag - 1,TrackBar.Position);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.TrackBar_Volume_ScaleChange(Sender: TObject);
Var
    TrackBar : TTrackBar;
Begin
    If Changing Then Exit;
    TrackBar := Sender;
    SetScaleVolume(TrackBar.Position);
End;
{..........................................................................................................}

End.


