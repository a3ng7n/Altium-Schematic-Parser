object MandelBrotForm1: TMandelBrotForm1
  Left = 65
  Top = 86
  BorderStyle = bsDialog
  Caption = 'MandelBrot'
  ClientHeight = 238
  ClientWidth = 223
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
    Left = 48
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Draw'
    TabOrder = 0
    OnClick = bDrawClick
  end
  object bQuit: TButton
    Left = 128
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Quit'
    TabOrder = 1
    OnClick = bQuitClick
  end
end
