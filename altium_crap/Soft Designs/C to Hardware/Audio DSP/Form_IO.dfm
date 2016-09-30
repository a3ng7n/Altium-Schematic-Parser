object IOForm: TIOForm
  Left = 379
  Top = 68
  BorderStyle = bsDialog
  Caption = 'InOut'
  ClientHeight = 270
  ClientWidth = 454
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = IOFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object InstrumentBackgroundPanel1: TInstrumentBackgroundPanel
    Left = 0
    Top = 0
    Width = 454
    Height = 270
    PortID = -1
    ShowGrooves = False
    ShowMountingHoles = False
    DesignSize = (
      454
      270)
    object InstrumentSilkScreen1: TInstrumentSilkScreen
      Left = 8
      Top = 8
      Width = 106
      Height = 256
      object Label_InputGain: TInstrumentLabel
        Left = 3
        Top = 220
        Width = 50
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = '0 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object Label_OutputGain: TInstrumentLabel
        Left = 54
        Top = 220
        Width = 50
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = '0 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentLabel1: TInstrumentLabel
        Left = 11
        Top = 20
        Width = 39
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'In'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentLabel2: TInstrumentLabel
        Left = 62
        Top = 20
        Width = 39
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Out'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentCheckBox2: TInstrumentCheckBox
        Left = 28
        Top = 232
        Width = 17
        Height = 17
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        GlyphStyle = icsRectAmber
        ParentFont = False
        ReadOnly = True
        SignalLink = Link_ClipInputGain
      end
      object InstrumentCheckBox3: TInstrumentCheckBox
        Left = 68
        Top = 232
        Width = 17
        Height = 17
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        GlyphStyle = icsRectAmber
        ParentFont = False
        ReadOnly = True
        SignalLink = Link_ClipOutputGain
      end
      object InstrumentLabel3: TInstrumentLabel
        Left = 37
        Top = 4
        Width = 39
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Gain'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object TrackBar_InputGain: TInstrumentTrackBar
        Left = 11
        Top = 32
        Width = 31
        Height = 184
        Discrete = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold, fsItalic]
        LargeTicks.Visible = True
        LargeTicks.Step = 5.000000000000000000
        MaxValue = 20.000000000000000000
        SmallTicks.Step = 1.000000000000000000
        Value = 20.000000000000000000
        OnChange = TrackBar_InputGainChange
      end
      object TrackBar_OutputGain: TInstrumentTrackBar
        Left = 62
        Top = 32
        Width = 31
        Height = 184
        Discrete = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold, fsItalic]
        LargeTicks.Visible = True
        LargeTicks.Step = 5.000000000000000000
        MaxValue = 20.000000000000000000
        SmallTicks.Step = 1.000000000000000000
        Value = 20.000000000000000000
        OnChange = TrackBar_OutputGainChange
      end
    end
    object InstrumentSilkScreen2: TInstrumentSilkScreen
      Left = 340
      Top = 8
      Width = 106
      Height = 256
      object InstrumentLabel4: TInstrumentLabel
        Left = 0
        Top = 232
        Width = 106
        Height = 14
        AutoSize = False
        Caption = '<= Clipping'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentButton1: TInstrumentButton
        Left = 3
        Top = 8
        Width = 100
        Height = 24
        Caption = 'Equalizer'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = [fsItalic]
        OnClick = InstrumentButton1Click
      end
    end
    object InstrumentSilkScreen3: TInstrumentSilkScreen
      Left = 120
      Top = 8
      Width = 104
      Height = 256
      object Label_EchoDelay: TInstrumentLabel
        Left = 3
        Top = 220
        Width = 50
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = '0 s'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object Label_EchoLevel: TInstrumentLabel
        Left = 54
        Top = 220
        Width = 50
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = '-36 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentLabel5: TInstrumentLabel
        Left = 11
        Top = 20
        Width = 39
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Delay'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object Label_LevelHeader: TInstrumentLabel
        Left = 62
        Top = 20
        Width = 39
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Level'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentLabel11: TInstrumentLabel
        Left = 37
        Top = 4
        Width = 39
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Echo'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentCheckBox5: TInstrumentCheckBox
        Left = 44
        Top = 232
        Width = 17
        Height = 17
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        GlyphStyle = icsRectAmber
        ParentFont = False
        ReadOnly = True
        SignalLink = Link_ClipEcho
      end
      object Trackbar_EchoDelay: TInstrumentTrackBar
        Left = 11
        Top = 32
        Width = 31
        Height = 184
        Discrete = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold, fsItalic]
        LargeTicks.Visible = True
        LargeTicks.Step = 172.000000000000000000
        MaxValue = 255.000000000000000000
        SmallTicks.Step = 43.000000000000000000
        OnChange = TrackBar_EchoDelayChange
      end
      object Trackbar_EchoLevel: TInstrumentTrackBar
        Left = 62
        Top = 32
        Width = 31
        Height = 184
        Discrete = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold, fsItalic]
        LargeTicks.Visible = True
        LargeTicks.Step = 5.000000000000000000
        MaxValue = 20.000000000000000000
        SmallTicks.Step = 1.000000000000000000
        OnChange = TrackBar_EchoLevelChange
      end
    end
    object InstrumentSilkScreen4: TInstrumentSilkScreen
      Left = 230
      Top = 8
      Width = 104
      Height = 256
      object Label_DelayDelay: TInstrumentLabel
        Left = 3
        Top = 220
        Width = 50
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = '0 s'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentLabel8: TInstrumentLabel
        Left = 11
        Top = 20
        Width = 39
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Delay'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentLabel6: TInstrumentLabel
        Left = 62
        Top = 20
        Width = 39
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Level'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentLabel10: TInstrumentLabel
        Left = 29
        Top = 4
        Width = 39
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = 'Delay'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object Label_DelayLevel: TInstrumentLabel
        Left = 54
        Top = 220
        Width = 50
        Height = 14
        Alignment = taCenter
        AutoSize = False
        Caption = '-36 dB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        ParentFont = False
        Transparent = True
      end
      object InstrumentCheckBox6: TInstrumentCheckBox
        Left = 44
        Top = 232
        Width = 17
        Height = 17
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial Black'
        Font.Style = []
        GlyphStyle = icsRectAmber
        ParentFont = False
        ReadOnly = True
        SignalLink = Link_ClipDelay
      end
      object Trackbar_DelayDelay: TInstrumentTrackBar
        Left = 11
        Top = 32
        Width = 31
        Height = 184
        Discrete = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold, fsItalic]
        LargeTicks.Visible = True
        LargeTicks.Step = 172.000000000000000000
        MaxValue = 255.000000000000000000
        SmallTicks.Step = 43.000000000000000000
        OnChange = TrackBar_DelayDelayChange
      end
      object Trackbar_DelayLevel: TInstrumentTrackBar
        Left = 62
        Top = 32
        Width = 31
        Height = 184
        Discrete = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold, fsItalic]
        LargeTicks.Visible = True
        LargeTicks.Step = 5.000000000000000000
        MaxValue = 20.000000000000000000
        SmallTicks.Step = 1.000000000000000000
        OnChange = TrackBar_DelayLevelChange
      end
    end
  end
  object SignalInstrumentManagerWrapper1: TSignalLinkManager
    AutoPolling = True
    PollingInterval = 250
    object Link_InputGain: TOutputIOSignalLink
      CompoundSignalName = 'SETTINGS:INPUT_GAIN[15..0]'
      Value = 4096
    end
    object Link_OutputGain: TOutputIOSignalLink
      CompoundSignalName = 'SETTINGS:OUTPUT_GAIN[15..0]'
      Value = 4096
    end
    object Link_ClipInput: TInputIOSignalLink
      CompoundSignalName = 'STATUS:CLIP_ERROR_INPUT[0]'
    end
    object Link_ClipInputGain: TInputIOSignalLink
      CompoundSignalName = 'STATUS:CLIP_ERROR_GAIN[0]'
    end
    object Link_ClipOutputGain: TInputIOSignalLink
      CompoundSignalName = 'STATUS:CLIP_ERROR_OUTPUT_GAIN[0]'
    end
    object Link_ClipOutput: TInputIOSignalLink
      CompoundSignalName = 'STATUS:CLIP_ERROR_OUTPUT[0]'
    end
    object Link_DelayDelay: TOutputIOSignalLink
      CompoundSignalName = 'SETTINGS:DELAY_DELAY[7..0]'
      Value = 0
    end
    object Link_EchoDelay: TOutputIOSignalLink
      CompoundSignalName = 'SETTINGS:ECHO_DELAY[7..0]'
      Value = 0
    end
    object Link_EchoLevel: TOutputIOSignalLink
      CompoundSignalName = 'SETTINGS:ECHO_LEVEL[7..0]'
      Value = 0
    end
    object Link_EchoFactor: TOutputIOSignalLink
      CompoundSignalName = 'SETTINGS:ECHO_FACTOR[7..0]'
      Value = 0
    end
    object Link_DelayLevel: TOutputIOSignalLink
      CompoundSignalName = 'SETTINGS:DELAY_LEVEL[7..0]'
      Value = 0
    end
    object Link_ClipDelay: TInputIOSignalLink
      CompoundSignalName = 'STATUS:CLIP_ERROR_DELAY[0]'
    end
    object Link_ClipEcho: TInputIOSignalLink
      CompoundSignalName = 'STATUS:CLIP_ERROR_ECHO[0]'
    end
  end
end
