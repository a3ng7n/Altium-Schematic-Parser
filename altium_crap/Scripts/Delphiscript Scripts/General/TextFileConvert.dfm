object TextForm: TTextForm
  Left = 98
  Top = 113
  Width = 369
  Height = 241
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 9
    Top = 168
    Width = 344
    Height = 2
  end
  object rgConvert: TRadioGroup
    Left = 15
    Top = 13
    Width = 185
    Height = 91
    Caption = ' Conversion options '
    ItemIndex = 0
    Items.Strings = (
      'Uppercase'
      'Remove symbols')
    TabOrder = 0
  end
  object bOpen: TButton
    Left = 280
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 1
    OnClick = bOpenClick
  end
  object bSave: TButton
    Left = 280
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 2
    OnClick = bSaveClick
  end
  object eOriginalFile: TEdit
    Left = 16
    Top = 112
    Width = 248
    Height = 21
    TabOrder = 3
  end
  object eConvertedFile: TEdit
    Left = 16
    Top = 136
    Width = 248
    Height = 21
    TabOrder = 4
  end
  object bConvert: TButton
    Left = 280
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 5
    OnClick = bConvertClick
  end
  object bCancel: TButton
    Left = 192
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = bCancelClick
  end
  object OpenDialog1: TOpenDialog
    Left = 320
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 328
    Top = 56
  end
end
