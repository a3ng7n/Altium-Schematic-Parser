object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Align Sch Component Pins'
  ClientHeight = 196
  ClientWidth = 263
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyUp = Form1KeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Button_OK: TButton
    Left = 56
    Top = 168
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 144
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object RadioGroup_AlignVertically: TRadioGroup
    Left = 24
    Top = 8
    Width = 220
    Height = 72
    Caption = 'Make Vertical Column'
    ItemIndex = 0
    Items.Strings = (
      'Pin hot-spot points &right (0 degrees)'
      'Pin hot-spot points &left (180 degrees)')
    TabOrder = 2
    OnClick = RadioGroup_AlignHorizontallyClick
  end
  object RadioGroup_AlignHorizontally: TRadioGroup
    Left = 24
    Top = 88
    Width = 220
    Height = 72
    Caption = 'Make Horizontal Row'
    Items.Strings = (
      'Pin hot-spot points &up (90 degrees)'
      'Pin hot-spot points &down (270 degrees)')
    TabOrder = 3
    OnClick = RadioGroup_AlignHorizontallyClick
  end
end
