object MMDemo: TMMDemo
  Left = 42
  Top = 72
  Width = 815
  Height = 633
  Caption = 'MMDemo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = MMDemoCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MediaPlayer1: TMediaPlayer
    Left = 272
    Top = 558
    Width = 253
    Height = 30
    AutoEnable = False
    Display = Animate1
    TabOrder = 0
  end
  object Animate1: TAnimate
    Left = 2
    Top = 3
    Width = 793
    Height = 545
    Active = False
    StopFrame = 12
  end
end
