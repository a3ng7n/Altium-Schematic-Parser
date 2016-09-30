object HelloWorldForm: THelloWorldForm
  Left = 5
  Top = 4
  Width = 247
  Height = 233
  Caption = 'Hello World!'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object bDisplay: TButton
    Left = 64
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Display'
    TabOrder = 0
    OnClick = bDisplayClick
  end
  object bClose: TButton
    Left = 152
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = bCloseClick
  end
end
