object GetNetClass: TGetNetClass
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Choose Net Classes to Unroute'
  ClientHeight = 174
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 227
    Top = 145
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object Classes: TXPListBox
    Left = 5
    Top = 4
    Width = 297
    Height = 138
    MultiSelect = True
    TabOrder = 1
    OnDblClick = ClassesDblClick
  end
  object Button2: TButton
    Left = 146
    Top = 145
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
