object Form_UpdatePath: TForm_UpdatePath
  Left = 24
  Top = 14
  BorderStyle = bsDialog
  Caption = 'Update PCB Model Paths...'
  ClientHeight = 199
  ClientWidth = 367
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 11
    Top = 12
    Width = 120
    Height = 13
    Caption = 'Choose a Footprint Name'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 160
    Width = 352
    Height = 2
  end
  object Label2: TLabel
    Left = 192
    Top = 16
    Width = 134
    Height = 13
    Caption = 'Footprint with different Paths'
  end
  object lFootPrintLocation: TLabel
    Left = 192
    Top = 136
    Width = 53
    Height = 13
    Caption = 'New Path: '
  end
  object XPBitBtnOK: TXPBitBtn
    Left = 205
    Top = 168
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = XPBitBtnOKClick
  end
  object XPBitBtnCancel: TXPBitBtn
    Left = 284
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = XPBitBtnCancelClick
  end
  object ComboBox_FootprintNames: TComboBox
    Left = 8
    Top = 32
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    OnClick = ComboBox_FootprintNamesClick
  end
  object ListBox_DiffPaths: TListBox
    Left = 192
    Top = 32
    Width = 160
    Height = 97
    ItemHeight = 13
    TabOrder = 3
    OnClick = ListBox_DiffPathsClick
  end
end
