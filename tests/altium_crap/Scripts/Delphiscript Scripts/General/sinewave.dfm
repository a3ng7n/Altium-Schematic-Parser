object SineWaveForm: TSineWaveForm
  Left = 11
  Top = 34
  BorderStyle = bsDialog
  Caption = 'SineWave Generator'
  ClientHeight = 251
  ClientWidth = 366
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
  object bDraw: TButton
    Left = 201
    Top = 220
    Width = 75
    Height = 25
    Caption = 'Draw'
    TabOrder = 0
    OnClick = bDrawClick
  end
  object bClose: TButton
    Left = 280
    Top = 220
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = bCloseClick
  end
end
