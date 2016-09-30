object formNetClassGenerator: TformNetClassGenerator
  Left = 0
  Top = 0
  Caption = 'Specify a Net Class'
  ClientHeight = 138
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object labelInstructions: TXPLabel
    Left = 16
    Top = 24
    Width = 273
    Height = 13
    UnicodeText = 'Enter a name for the Net Class or select one from the list'
  end
  object comboBoxClassList: TXPComboBox
    Left = 20
    Top = 54
    Width = 263
    Height = 21
    HotTrack = False
    Sorted = True
    Style = csDropDown
    TabOrder = 0
  end
  object buttonOK: TXPButton
    Left = 110
    Top = 91
    Width = 80
    Height = 25
    Caption = '&OK'
    Default = True
    ParentColor = False
    TabOrder = 1
    TabStop = False
    OnClick = buttonOKClick
  end
  object buttonCancel: TXPButton
    Left = 201
    Top = 91
    Width = 80
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ParentColor = False
    TabOrder = 2
    TabStop = False
  end
end
