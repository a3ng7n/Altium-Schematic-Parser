object TimerForm: TTimerForm
  Left = 41
  Top = 136
  BorderStyle = bsToolWindow
  Caption = '  Time Teller'
  ClientHeight = 108
  ClientWidth = 286
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
  object Label1: TLabel
    Left = 104
    Top = 24
    Width = 42
    Height = 20
    Caption = 'Time: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 24
  end
end
