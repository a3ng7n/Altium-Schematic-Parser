object FormShowModalEg: TFormShowModalEg
  Left = 61
  Top = 18
  Width = 320
  Height = 240
  Caption = 'ShowModal Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object bOk: TButton
    Left = 216
    Top = 160
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = bOkClick
  end
  object bCancel: TButton
    Left = 128
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = bCancelClick
  end
end
