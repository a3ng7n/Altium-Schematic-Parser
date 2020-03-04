object AutoTester: TAutoTester
  Left = 79
  Top = 56
  BorderStyle = bsToolWindow
  Caption = 'Music Hardware Controller'
  ClientHeight = 531
  ClientWidth = 856
  Color = clBtnFace
  TransparentColor = True
  TransparentColorValue = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = AutoTesterClose
  OnMouseDown = AutoTesterMouseDown
  OnMouseMove = AutoTesterMouseMove
  OnMouseUp = AutoTesterMouseUp
  OnShow = AutoTesterShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel_Bottom: TBevel
    Left = 0
    Top = 485
    Width = 856
    Height = 5
    Align = alBottom
    Shape = bsTopLine
  end
  object Panel_Buttons: TPanel
    Left = 0
    Top = 490
    Width = 856
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      856
      41)
    object Button_Close: TButton
      Left = 769
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Close'
      Default = True
      TabOrder = 0
      OnClick = Button_CloseClick
    end
  end
  object RadioGroup_ScaleNote: TRadioGroup
    Left = 8
    Top = 255
    Width = 127
    Height = 216
    ItemIndex = 0
    Items.Strings = (
      'C'
      'C#/Db'
      'D'
      'D#/Eb'
      'E'
      'F'
      'F#/Gb'
      'G'
      'G#/Ab'
      'A'
      'A#/Bb'
      'B')
    TabOrder = 4
    OnClick = RadioGroup_ScaleNoteClick
  end
  object RadioGroup_ScaleOctave: TRadioGroup
    Left = 132
    Top = 255
    Width = 123
    Height = 216
    ItemIndex = 4
    Items.Strings = (
      'Octave 0'
      'Octave 1'
      'Octave 2'
      'Octave 3'
      'Octave 4'
      'Octave 5'
      'Octave 6'
      'Octave 7'
      'Octave 8'
      'Octave 9')
    TabOrder = 6
    OnClick = RadioGroup_ScaleOctaveClick
  end
  object GroupBox_Scale: TGroupBox
    Left = 8
    Top = 184
    Width = 247
    Height = 78
    TabOrder = 5
    object Label_Rate: TLabel
      Left = 8
      Top = 46
      Width = 23
      Height = 13
      Alignment = taRightJustify
      Caption = 'Rate'
    end
    object CheckBox_Voice8: TCheckBox
      Tag = 8
      Left = 8
      Top = 16
      Width = 51
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Enable'
      TabOrder = 0
      OnClick = CheckBox_VoiceClick
    end
    object TrackBar_ScaleRate: TTrackBar
      Left = 38
      Top = 41
      Width = 154
      Height = 27
      Max = 100
      Min = 1
      Orientation = trHorizontal
      PageSize = 1
      Frequency = 10
      Position = 50
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar_ScaleRateChange
    end
    object Edit_ScaleRate: TEdit
      Left = 203
      Top = 41
      Width = 29
      Height = 21
      TabOrder = 1
      Text = '50'
      OnChange = Edit_ScaleRateChange
    end
  end
  object RadioGroup_ScaleId: TRadioGroup
    Left = 8
    Top = 8
    Width = 247
    Height = 184
    Caption = 'Scale'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Major'
      'Minor (Natural)'
      'Harmonic Minor'
      'Melodic Minor (Asc)'
      'Melodic Minor (Desc)'
      'Enigmatic'
      'Diminished'
      'Whole Tone'
      'Pentatonic Major'
      'Pentatonic Minor'
      '3 semitone'
      '4 semitone'
      'Bluesy R&R'
      'Indian')
    TabOrder = 2
    OnClick = RadioGroup_ScaleIdClick
  end
  object RadioGroup_ChordNote: TRadioGroup
    Left = 263
    Top = 254
    Width = 75
    Height = 217
    ItemIndex = 0
    Items.Strings = (
      'C'
      'C#/Db'
      'D'
      'D#/Eb'
      'E'
      'F'
      'F#/Gb'
      'G'
      'G#/Ab'
      'A'
      'A#/Bb'
      'B')
    TabOrder = 7
    OnClick = RadioGroup_ChordNoteClick
  end
  object RadioGroup_ChordOctave: TRadioGroup
    Left = 334
    Top = 255
    Width = 77
    Height = 216
    ItemIndex = 4
    Items.Strings = (
      'Octave 0'
      'Octave 1'
      'Octave 2'
      'Octave 3'
      'Octave 4'
      'Octave 5'
      'Octave 6'
      'Octave 7'
      'Octave 8'
      'Octave 9')
    TabOrder = 8
    OnClick = RadioGroup_ChordOctaveClick
  end
  object GroupBox_Chord: TGroupBox
    Left = 263
    Top = 184
    Width = 148
    Height = 78
    TabOrder = 3
    object Label_ChordNote: TLabel
      Left = 9
      Top = 15
      Width = 59
      Height = 13
      Alignment = taRightJustify
      Caption = 'Chrod Notes'
    end
    object CheckBox_Voice5: TCheckBox
      Tag = 5
      Left = 39
      Top = 32
      Width = 24
      Height = 17
      Alignment = taLeftJustify
      Caption = '2'
      TabOrder = 0
      OnClick = CheckBox_VoiceClick
    end
    object CheckBox_Voice4: TCheckBox
      Tag = 4
      Left = 9
      Top = 32
      Width = 24
      Height = 17
      Alignment = taLeftJustify
      Caption = '1'
      TabOrder = 1
      OnClick = CheckBox_VoiceClick
    end
    object CheckBox_Voice6: TCheckBox
      Tag = 6
      Left = 9
      Top = 51
      Width = 24
      Height = 17
      Alignment = taLeftJustify
      Caption = '3'
      TabOrder = 2
      OnClick = CheckBox_VoiceClick
    end
    object CheckBox_Voice7: TCheckBox
      Tag = 7
      Left = 39
      Top = 51
      Width = 24
      Height = 17
      Alignment = taLeftJustify
      Caption = '4'
      TabOrder = 3
      OnClick = CheckBox_VoiceClick
    end
    object Button_ChordOn: TButton
      Left = 79
      Top = 15
      Width = 61
      Height = 25
      Caption = 'All On'
      TabOrder = 4
      OnClick = Button_ChordOnClick
    end
    object Button_ChordOff: TButton
      Left = 79
      Top = 43
      Width = 61
      Height = 25
      Caption = 'All Off'
      TabOrder = 5
      OnClick = Button_ChordOffClick
    end
  end
  object RadioGroup_ChordId: TRadioGroup
    Left = 263
    Top = 8
    Width = 148
    Height = 184
    Caption = 'Chord'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Major'
      'Sixth'
      'Major7'
      'Major9'
      'Dom7'
      'Ninth'
      'Eleventh'
      'Aug'
      'Minor'
      'Minor6'
      'Minor7'
      'Minor9'
      'Minor11'
      'Sus2'
      'Sus4'
      'Dim')
    TabOrder = 1
    OnClick = RadioGroup_ChordIdClick
  end
  object RadioGroup_Octave_V1: TRadioGroup
    Left = 479
    Top = 255
    Width = 77
    Height = 216
    ItemIndex = 4
    Items.Strings = (
      'Octave 0'
      'Octave 1'
      'Octave 2'
      'Octave 3'
      'Octave 4'
      'Octave 5'
      'Octave 6'
      'Octave 7'
      'Octave 8'
      'Octave 9')
    TabOrder = 9
    OnClick = RadioGroup_Octave_V1Click
  end
  object RadioGroup_Note_V1: TRadioGroup
    Left = 420
    Top = 255
    Width = 63
    Height = 216
    ItemIndex = 0
    Items.Strings = (
      'C'
      'C#/Db'
      'D'
      'D#/Eb'
      'E'
      'F'
      'F#/Gb'
      'G'
      'G#/Ab'
      'A'
      'A#/Bb'
      'B')
    TabOrder = 10
    OnClick = RadioGroup_Note_V1Click
  end
  object GroupBox_V1: TGroupBox
    Left = 420
    Top = 158
    Width = 136
    Height = 104
    TabOrder = 11
    object Label_V1: TLabel
      Left = 8
      Top = 44
      Width = 43
      Height = 13
      Caption = 'MIDI No.'
    end
    object Edit_V1: TEdit
      Left = 58
      Top = 40
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '48'
      OnChange = Edit_V1Change
    end
    object UpDown_V1: TUpDown
      Left = 108
      Top = 40
      Width = 16
      Height = 21
      Associate = Edit_V1
      Min = 1
      Max = 119
      Position = 48
      TabOrder = 1
      Wrap = False
      OnClick = UpDown_V1Click
    end
    object TrackBar_V1: TTrackBar
      Left = 3
      Top = 67
      Width = 130
      Height = 32
      Max = 119
      Min = 1
      Orientation = trHorizontal
      Frequency = 10
      Position = 48
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar_V1Change
    end
    object CheckBox_Voice1: TCheckBox
      Tag = 1
      Left = 8
      Top = 14
      Width = 64
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Voice 1'
      TabOrder = 3
      OnClick = CheckBox_VoiceClick
    end
  end
  object RadioGroup_Octave_V2: TRadioGroup
    Left = 623
    Top = 255
    Width = 77
    Height = 216
    ItemIndex = 4
    Items.Strings = (
      'Octave 0'
      'Octave 1'
      'Octave 2'
      'Octave 3'
      'Octave 4'
      'Octave 5'
      'Octave 6'
      'Octave 7'
      'Octave 8'
      'Octave 9')
    TabOrder = 12
    OnClick = RadioGroup_Octave_V2Click
  end
  object RadioGroup_Note_V2: TRadioGroup
    Left = 564
    Top = 255
    Width = 63
    Height = 216
    ItemIndex = 0
    Items.Strings = (
      'C'
      'C#/Db'
      'D'
      'D#/Eb'
      'E'
      'F'
      'F#/Gb'
      'G'
      'G#/Ab'
      'A'
      'A#/Bb'
      'B')
    TabOrder = 13
    OnClick = RadioGroup_Note_V2Click
  end
  object GroupBox_V2: TGroupBox
    Left = 564
    Top = 158
    Width = 136
    Height = 104
    TabOrder = 14
    object Label_V2: TLabel
      Left = 8
      Top = 44
      Width = 43
      Height = 13
      Caption = 'MIDI No.'
    end
    object Edit_V2: TEdit
      Left = 58
      Top = 40
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '48'
      OnChange = Edit_V2Change
    end
    object UpDown_V2: TUpDown
      Left = 108
      Top = 40
      Width = 16
      Height = 21
      Associate = Edit_V2
      Min = 1
      Max = 119
      Position = 48
      TabOrder = 1
      Wrap = False
      OnClick = UpDown_V2Click
    end
    object TrackBar_V2: TTrackBar
      Left = 3
      Top = 67
      Width = 130
      Height = 32
      Max = 119
      Min = 1
      Orientation = trHorizontal
      Frequency = 10
      Position = 48
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar_V2Change
    end
    object CheckBox_Voice2: TCheckBox
      Tag = 2
      Left = 8
      Top = 16
      Width = 64
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Voice 2'
      TabOrder = 3
      OnClick = CheckBox_VoiceClick
    end
  end
  object RadioGroup_Octave_V3: TRadioGroup
    Left = 767
    Top = 255
    Width = 77
    Height = 216
    ItemIndex = 4
    Items.Strings = (
      'Octave 0'
      'Octave 1'
      'Octave 2'
      'Octave 3'
      'Octave 4'
      'Octave 5'
      'Octave 6'
      'Octave 7'
      'Octave 8'
      'Octave 9')
    TabOrder = 15
    OnClick = RadioGroup_Octave_V3Click
  end
  object RadioGroup_Note_V3: TRadioGroup
    Left = 708
    Top = 255
    Width = 63
    Height = 216
    ItemIndex = 0
    Items.Strings = (
      'C'
      'C#/Db'
      'D'
      'D#/Eb'
      'E'
      'F'
      'F#/Gb'
      'G'
      'G#/Ab'
      'A'
      'A#/Bb'
      'B')
    TabOrder = 16
    OnClick = RadioGroup_Note_V3Click
  end
  object GroupBox_V3: TGroupBox
    Left = 708
    Top = 158
    Width = 136
    Height = 104
    TabOrder = 17
    object Label_V3: TLabel
      Left = 8
      Top = 44
      Width = 43
      Height = 13
      Caption = 'MIDI No.'
    end
    object Edit_V3: TEdit
      Left = 58
      Top = 40
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '48'
      OnChange = Edit_V3Change
    end
    object UpDown_V3: TUpDown
      Left = 108
      Top = 40
      Width = 16
      Height = 21
      Associate = Edit_V3
      Min = 1
      Max = 119
      Position = 48
      TabOrder = 1
      Wrap = False
      OnClick = UpDown_V3Click
    end
    object TrackBar_V3: TTrackBar
      Left = 3
      Top = 67
      Width = 130
      Height = 32
      Max = 119
      Min = 1
      Orientation = trHorizontal
      Frequency = 10
      Position = 48
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar_V3Change
    end
    object CheckBox_Voice3: TCheckBox
      Tag = 3
      Left = 8
      Top = 16
      Width = 64
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Voice 3'
      TabOrder = 3
      OnClick = CheckBox_VoiceClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 420
    Top = 8
    Width = 424
    Height = 147
    Caption = 'Mixer'
    TabOrder = 18
    object Label1: TLabel
      Left = 14
      Top = 125
      Width = 27
      Height = 13
      Caption = 'Scale'
    end
    object Label2: TLabel
      Left = 104
      Top = 125
      Width = 28
      Height = 13
      Caption = 'Chord'
    end
    object Label3: TLabel
      Left = 195
      Top = 125
      Width = 36
      Height = 13
      Caption = 'Voice 1'
    end
    object Label4: TLabel
      Left = 276
      Top = 125
      Width = 36
      Height = 13
      Caption = 'Voice 2'
    end
    object Label5: TLabel
      Left = 356
      Top = 125
      Width = 36
      Height = 13
      Caption = 'Voice 3'
    end
    object TrackBar_Volume_Scale: TTrackBar
      Left = 16
      Top = 12
      Width = 45
      Height = 115
      Max = 255
      Orientation = trVertical
      Frequency = 10
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 0
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar_Volume_ScaleChange
    end
    object TrackBar_Volume_Chord: TTrackBar
      Tag = 4
      Left = 108
      Top = 12
      Width = 45
      Height = 115
      Max = 255
      Orientation = trVertical
      Frequency = 10
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 1
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar_Volume_VoiceChange
    end
    object TrackBar_Volume_Voice1: TTrackBar
      Tag = 1
      Left = 203
      Top = 12
      Width = 45
      Height = 115
      Max = 255
      Orientation = trVertical
      Frequency = 10
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar_Volume_VoiceChange
    end
    object TrackBar_Volume_Voice2: TTrackBar
      Tag = 2
      Left = 283
      Top = 12
      Width = 45
      Height = 115
      Max = 255
      Orientation = trVertical
      Frequency = 10
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 3
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar_Volume_VoiceChange
    end
    object TrackBar_Volume_Voice3: TTrackBar
      Tag = 3
      Left = 363
      Top = 12
      Width = 45
      Height = 115
      Max = 255
      Orientation = trVertical
      Frequency = 10
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 4
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar_Volume_VoiceChange
    end
  end
end
