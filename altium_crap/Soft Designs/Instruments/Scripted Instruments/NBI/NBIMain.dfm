object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Scripted Instruments'
  ClientHeight = 560
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = Form1Create
  FormKind = fkNormal
  PixelsPerInch = 96
  TextHeight = 13
  object InstrumentBackgroundPanel1: TInstrumentBackgroundPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 560
    PortID = -1
    ExplicitWidth = 640
    ExplicitHeight = 480
    DesignSize = (
      512
      560)
    object InstrumentCaption1: TInstrumentCaption
      Left = 48
      Top = 8
      Width = 400
      Height = 14
      Caption = 'Memory Instrument MI1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
    end
    object InstrumentCaption2: TInstrumentCaption
      Left = 48
      Top = 72
      Width = 400
      Height = 14
      Caption = 'Wishbone Probe PROBE1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
    end
    object InstrumentCaption3: TInstrumentCaption
      Left = 48
      Top = 304
      Width = 400
      Height = 14
      Caption = 'Frequency Generator FREQGEN'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
    end
    object InstrumentLabel1: TInstrumentLabel
      Left = 48
      Top = 328
      Width = 112
      Height = 17
      Caption = 'Current Frequency:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqGen_CurFreq: TInstrumentLabel
      Left = 168
      Top = 328
      Width = 136
      Height = 17
      Caption = 'Label_FreqGen_CurFreq'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel2: TInstrumentLabel
      Left = 48
      Top = 344
      Width = 108
      Height = 17
      Caption = 'Current TimeBase:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqGen_CurTimeBase: TInstrumentLabel
      Left = 168
      Top = 344
      Width = 166
      Height = 17
      Caption = 'Label_FreqGen_CurTimeBase'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel3: TInstrumentLabel
      Left = 48
      Top = 360
      Width = 66
      Height = 17
      Caption = 'Suspended:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqGen_Suspended: TInstrumentLabel
      Left = 168
      Top = 360
      Width = 152
      Height = 17
      Caption = 'Label_FreqGen_Suspended'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentCaption4: TInstrumentCaption
      Left = 48
      Top = 136
      Width = 400
      Height = 14
      Caption = 'Frequency Counter FREQCNT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
    end
    object InstrumentCaption5: TInstrumentCaption
      Left = 56
      Top = 192
      Width = 192
      Height = 14
      Caption = 'Channel A'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
    end
    object InstrumentCaption6: TInstrumentCaption
      Left = 256
      Top = 192
      Width = 192
      Height = 14
      Caption = 'Channel B'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
    end
    object InstrumentLabel4: TInstrumentLabel
      Left = 152
      Top = 160
      Width = 108
      Height = 17
      Caption = 'Current TimeBase:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCnt_CurTimeBase: TInstrumentLabel
      Left = 272
      Top = 160
      Width = 164
      Height = 17
      Caption = 'Label_FreqCnt_CurTimeBase'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel5: TInstrumentLabel
      Left = 56
      Top = 216
      Width = 81
      Height = 17
      Caption = 'Edge Polarity:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntA_EdgePolarity: TInstrumentLabel
      Left = 144
      Top = 216
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntA_EdgePolarity'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel6: TInstrumentLabel
      Left = 256
      Top = 216
      Width = 81
      Height = 17
      Caption = 'Edge Polarity:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntB_EdgePolarity: TInstrumentLabel
      Left = 344
      Top = 216
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntB_EdgePolarity'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel7: TInstrumentLabel
      Left = 56
      Top = 232
      Width = 86
      Height = 17
      Caption = 'Measure Mode:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntA_MeasureMode: TInstrumentLabel
      Left = 144
      Top = 232
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntA_MeasureMode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel9: TInstrumentLabel
      Left = 256
      Top = 232
      Width = 86
      Height = 17
      Caption = 'Measure Mode:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntB_MeasureMode: TInstrumentLabel
      Left = 344
      Top = 232
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntB_MeasureMode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel8: TInstrumentLabel
      Left = 56
      Top = 248
      Width = 42
      Height = 17
      Caption = 'Gating:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntA_Gating: TInstrumentLabel
      Left = 144
      Top = 248
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntA_Gating'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel11: TInstrumentLabel
      Left = 256
      Top = 248
      Width = 42
      Height = 17
      Caption = 'Gating:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntB_Gating: TInstrumentLabel
      Left = 344
      Top = 248
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntB_Gating'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel10: TInstrumentLabel
      Left = 56
      Top = 264
      Width = 66
      Height = 17
      Caption = 'Suspended:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntA_Suspended: TInstrumentLabel
      Left = 144
      Top = 264
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntA_Suspended'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntB_Suspended: TInstrumentLabel
      Left = 344
      Top = 264
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntB_Suspended'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel13: TInstrumentLabel
      Left = 256
      Top = 264
      Width = 66
      Height = 17
      Caption = 'Suspended:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel12: TInstrumentLabel
      Left = 56
      Top = 280
      Width = 59
      Height = 17
      Caption = 'Measured:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntA_Measured: TInstrumentLabel
      Left = 144
      Top = 280
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntA_Measured'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentLabel15: TInstrumentLabel
      Left = 256
      Top = 280
      Width = 59
      Height = 17
      Caption = 'Measured:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_FreqCntB_Measured: TInstrumentLabel
      Left = 344
      Top = 280
      Width = 100
      Height = 17
      Caption = 'Label_FreqCntB_Measured'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentCaption7: TInstrumentCaption
      Left = 48
      Top = 488
      Width = 400
      Height = 14
      Caption = 'Terminal T1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
    end
    object InstrumentCaption8: TInstrumentCaption
      Left = 48
      Top = 424
      Width = 400
      Height = 14
      Caption = 'CrossPoint Switch CPS1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
    end
    object InstrumentLabel14: TInstrumentLabel
      Left = 152
      Top = 448
      Width = 77
      Height = 17
      Caption = 'Connectivity:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Label_CPS_Connectivity: TInstrumentLabel
      Left = 248
      Top = 448
      Width = 138
      Height = 17
      Caption = 'Label_CPS_Connectivity'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object InstrumentButton1: TInstrumentButton
      Left = 48
      Top = 32
      Width = 80
      Height = 24
      Caption = 'Read Memory'
      OnClick = InstrumentButton1Click
    end
    object InstrumentButton2: TInstrumentButton
      Left = 136
      Top = 32
      Width = 80
      Height = 24
      Caption = 'Write Memory'
      OnClick = InstrumentButton2Click
    end
    object InstrumentButton3: TInstrumentButton
      Left = 224
      Top = 32
      Width = 80
      Height = 24
      Caption = 'Load Memory'
      OnClick = InstrumentButton3Click
    end
    object InstrumentButton4: TInstrumentButton
      Left = 312
      Top = 32
      Width = 80
      Height = 24
      Caption = 'Save Memory'
      OnClick = InstrumentButton4Click
    end
    object InstrumentButton5: TInstrumentButton
      Left = 48
      Top = 96
      Width = 80
      Height = 24
      Caption = 'Read Probe'
      OnClick = InstrumentButton5Click
    end
    object InstrumentButton6: TInstrumentButton
      Left = 136
      Top = 96
      Width = 80
      Height = 24
      Caption = 'Write Probe'
      OnClick = InstrumentButton6Click
    end
    object InstrumentButton7: TInstrumentButton
      Left = 224
      Top = 96
      Width = 80
      Height = 24
      Caption = 'Load Probe'
      OnClick = InstrumentButton7Click
    end
    object InstrumentButton8: TInstrumentButton
      Left = 312
      Top = 96
      Width = 80
      Height = 24
      Caption = 'Save Probe'
      OnClick = InstrumentButton8Click
    end
    object InstrumentButton9: TInstrumentButton
      Left = 256
      Top = 384
      Width = 80
      Height = 24
      Caption = 'Suspend'
      OnClick = InstrumentButton9Click
    end
    object InstrumentButton10: TInstrumentButton
      Left = 152
      Top = 384
      Width = 96
      Height = 24
      Caption = 'Toggle TimeBase'
      OnClick = InstrumentButton10Click
    end
    object InstrumentButton11: TInstrumentButton
      Left = 48
      Top = 384
      Width = 96
      Height = 24
      Caption = 'Toggle Frequency'
      OnClick = InstrumentButton11Click
    end
    object InstrumentButton12: TInstrumentButton
      Left = 48
      Top = 160
      Width = 96
      Height = 24
      Caption = 'Toggle TimeBase'
      OnClick = InstrumentButton12Click
    end
    object InstrumentButton13: TInstrumentButton
      Left = 48
      Top = 512
      Width = 72
      Height = 24
      Caption = 'Put Next Char'
      OnClick = InstrumentButton13Click
    end
    object InstrumentButton14: TInstrumentButton
      Left = 128
      Top = 512
      Width = 56
      Height = 24
      Caption = 'Put String'
      OnClick = InstrumentButton14Click
    end
    object InstrumentButton15: TInstrumentButton
      Left = 192
      Top = 512
      Width = 64
      Height = 24
      Caption = 'Save To File'
      OnClick = InstrumentButton15Click
    end
    object InstrumentButton16: TInstrumentButton
      Left = 264
      Top = 512
      Width = 72
      Height = 24
      Caption = 'Show Content'
      OnClick = InstrumentButton16Click
    end
    object InstrumentButton17: TInstrumentButton
      Left = 48
      Top = 448
      Width = 96
      Height = 24
      Caption = 'Toggle Connection'
      OnClick = InstrumentButton17Click
    end
  end
  object SignalLinkManager1: TSignalLinkManager
    AutoPolling = True
    PollingInterval = 250
    OnPoll = SignalLinkManager1Poll
    Left = 8
    Top = 24
  end
end
