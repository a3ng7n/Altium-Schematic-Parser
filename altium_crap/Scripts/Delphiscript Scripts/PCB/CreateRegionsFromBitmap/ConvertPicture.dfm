object ConverterForm: TConverterForm
  Left = 18
  Top = 9
  Caption = 'PCB Logo Creator'
  ClientHeight = 533
  ClientWidth = 809
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    809
    533)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 470
    Height = 496
    Anchors = [akLeft, akTop, akRight, akBottom]
    Center = True
    Proportional = True
    Stretch = True
  end
  object XStatusBar1: TXStatusBar
    Left = 0
    Top = 508
    Width = 809
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
    Left = 230
    Top = 514
    Width = 571
    Height = 15
    Position = 0
    Smooth = True
    Step = 0
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object exitbutton: TButton
    Left = 726
    Top = 65
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Exit'
    TabOrder = 2
    OnClick = exitbuttonClick
  end
  object convertbutton: TButton
    Left = 726
    Top = 37
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Convert'
    TabOrder = 3
    OnClick = convertbuttonClick
  end
  object loadbutton: TButton
    Left = 726
    Top = 9
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    Default = True
    TabOrder = 4
    OnClick = loadbuttonClick
  end
  object ScaleGroupBox: TGroupBox
    Left = 487
    Top = 382
    Width = 232
    Height = 115
    Anchors = [akTop, akRight]
    Caption = 'Scale'
    TabOrder = 5
    object Label10: TLabel
      Left = 23
      Top = 25
      Width = 43
      Height = 13
      Caption = '1 Pixel = '
    end
    object Label11: TLabel
      Left = 14
      Top = 54
      Width = 52
      Height = 13
      Caption = 'PCB Width'
    end
    object Label9: TLabel
      Left = 11
      Top = 83
      Width = 55
      Height = 13
      Caption = 'PCB Height'
    end
    object PixelSizeEdit: TEdit
      Left = 71
      Top = 22
      Width = 132
      Height = 21
      TabOrder = 0
      OnChange = PixelSizeEditChange
    end
    object WidthEdit: TEdit
      Left = 71
      Top = 51
      Width = 132
      Height = 21
      TabOrder = 1
      OnChange = WidthEditChange
    end
    object HeightEdit: TEdit
      Left = 71
      Top = 80
      Width = 132
      Height = 21
      TabOrder = 2
      OnChange = HeightEditChange
    end
  end
  object BoardGroupBox: TGroupBox
    Left = 487
    Top = 11
    Width = 232
    Height = 157
    Anchors = [akTop, akRight]
    Caption = 'Logo'
    TabOrder = 6
    object Label14: TLabel
      Left = 25
      Top = 25
      Width = 7
      Height = 13
      Caption = 'X'
    end
    object Label15: TLabel
      Left = 25
      Top = 54
      Width = 7
      Height = 13
      Caption = 'Y'
    end
    object Label1: TLabel
      Left = 6
      Top = 83
      Width = 26
      Height = 13
      Caption = 'Layer'
    end
    object XEdit: TEdit
      Left = 39
      Top = 22
      Width = 120
      Height = 21
      TabOrder = 0
      OnChange = XEditChange
    end
    object YEdit: TEdit
      Left = 39
      Top = 51
      Width = 120
      Height = 21
      TabOrder = 1
      OnChange = YEditChange
    end
    object SelectButton: TButton
      Left = 167
      Top = 49
      Width = 55
      Height = 25
      Caption = 'Select'
      TabOrder = 2
      OnClick = SelectButtonClick
    end
    object ComboBoxLayers: TComboBox
      Left = 39
      Top = 80
      Width = 182
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
    object CreateComponentRadioButton: TRadioButton
      Left = 39
      Top = 110
      Width = 132
      Height = 17
      Caption = 'Create As Component'
      Checked = True
      TabOrder = 4
      TabStop = True
    end
    object CreateFreeRadioButton: TRadioButton
      Left = 39
      Top = 132
      Width = 145
      Height = 17
      Caption = 'Create As Free Primitives'
      TabOrder = 5
    end
  end
  object ImageGroupBox: TGroupBox
    Left = 487
    Top = 251
    Width = 232
    Height = 125
    Anchors = [akTop, akRight]
    Caption = 'Image'
    TabOrder = 7
    object Label3: TLabel
      Left = 12
      Top = 22
      Width = 55
      Height = 13
      Caption = 'Size (pixels)'
    end
    object ImageSizeLabel: TLabel
      Left = 77
      Top = 22
      Width = 26
      Height = 13
      Caption = ' 0 x 0'
    end
    object NegativeCheckBox: TCheckBox
      Left = 13
      Top = 47
      Width = 97
      Height = 17
      Caption = 'Negative'
      TabOrder = 0
    end
    object FlipHorCheckBox: TCheckBox
      Left = 13
      Top = 72
      Width = 113
      Height = 17
      Caption = 'Mirror Horizontally'
      TabOrder = 1
    end
    object FlipVertCheckBox: TCheckBox
      Left = 13
      Top = 97
      Width = 97
      Height = 17
      Caption = 'Mirror Vertically'
      TabOrder = 2
    end
  end
  object GroupBox1: TGroupBox
    Left = 487
    Top = 174
    Width = 232
    Height = 71
    Anchors = [akTop, akRight]
    Caption = 'Generated Regions'
    TabOrder = 8
    object DoUnionRadioButton: TRadioButton
      Left = 38
      Top = 23
      Width = 132
      Height = 17
      Caption = 'Union (slower)'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object DontUnionRadioButton: TRadioButton
      Left = 38
      Top = 45
      Width = 145
      Height = 17
      Caption = 'Don'#39't Union (more regions)'
      TabOrder = 1
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmap Files (*.bmp)|*.BMP'
    Left = 43
    Top = 105
  end
end
