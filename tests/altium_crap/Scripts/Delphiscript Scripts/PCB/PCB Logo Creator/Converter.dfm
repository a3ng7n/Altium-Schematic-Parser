object ConverterForm: TConverterForm
  Left = 18
  Top = 9
  BorderStyle = bsDialog
  Caption = 'PCB Logo Creator'
  ClientHeight = 236
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = ConverterFormCreate
  DesignSize = (
    520
    236)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 200
    Height = 200
    Center = True
    Proportional = True
    Stretch = True
  end
  object XStatusBar1: TXStatusBar
    Left = 0
    Top = 211
    Width = 520
    Height = 25
    Enabled = False
    Panels = <>
    ParentShowHint = False
    ShowHint = False
    SimplePanel = True
    SimpleText = '   Ready...'
    SizeGrip = False
  end
  object XPProgressBar1: TXPProgressBar
    Left = 76
    Top = 217
    Width = 435
    Height = 15
    Position = 0
    Smooth = True
    Step = 0
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 215
    Top = 5
    Width = 209
    Height = 201
    Caption = ' Converter Options '
    TabOrder = 1
    object lImageSize: TLabel
      Left = 72
      Top = 72
      Width = 46
      Height = 13
      Caption = ' 0 x 0 mils'
    end
    object ImageSizeLabel: TLabel
      Left = 13
      Top = 72
      Width = 59
      Height = 13
      Caption = 'Image size : '
    end
    object lScalingFactor: TLabel
      Left = 13
      Top = 101
      Width = 77
      Height = 13
      Caption = 'Scaling Factor : '
    end
    object Label2: TLabel
      Left = 13
      Top = 21
      Width = 66
      Height = 13
      Caption = 'Board Layer : '
    end
    object Label1: TLabel
      Left = 144
      Top = 101
      Width = 54
      Height = 13
      Caption = '(mils/pixels)'
    end
    object cbNegative: TCheckBox
      Left = 13
      Top = 127
      Width = 97
      Height = 21
      Caption = 'Negative'
      TabOrder = 0
    end
    object cbMirrorX: TCheckBox
      Left = 13
      Top = 148
      Width = 58
      Height = 21
      Caption = 'Mirror X'
      TabOrder = 1
    end
    object cbMirrorY: TCheckBox
      Left = 13
      Top = 169
      Width = 56
      Height = 21
      Caption = 'Mirror Y'
      TabOrder = 2
    end
    object UpDown1: TUpDown
      Left = 122
      Top = 97
      Width = 15
      Height = 21
      Associate = eScalingFactor
      Position = 1
      TabOrder = 3
    end
    object eScalingFactor: TEdit
      Left = 91
      Top = 97
      Width = 31
      Height = 21
      TabOrder = 4
      Text = '1'
      OnChange = eScalingFactorChange
    end
    object ComboBoxLayers: TComboBox
      Left = 13
      Top = 36
      Width = 188
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
    end
  end
  object exitbutton: TButton
    Left = 432
    Top = 66
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Exit'
    TabOrder = 3
    OnClick = exitbuttonClick
  end
  object convertbutton: TButton
    Left = 432
    Top = 38
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Convert'
    Enabled = False
    TabOrder = 4
    OnClick = convertbuttonClick
  end
  object loadbutton: TButton
    Left = 432
    Top = 10
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    TabOrder = 5
    OnClick = loadbuttonClick
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 43
    Top = 105
  end
end
