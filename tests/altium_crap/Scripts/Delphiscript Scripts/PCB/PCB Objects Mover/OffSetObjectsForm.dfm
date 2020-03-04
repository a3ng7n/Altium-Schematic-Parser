object formMovePCBObjects: TformMovePCBObjects
  Left = 53
  Top = 100
  BorderStyle = bsSingle
  Caption = 'Move PCB Objects'
  ClientHeight = 221
  ClientWidth = 237
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  FormKind = fkServerPanel
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 107
    Width = 45
    Height = 13
    Caption = 'X Value'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object Label2: TLabel
    Left = 8
    Top = 128
    Width = 45
    Height = 13
    Caption = 'Y Value'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object Bevel1: TBevel
    Left = 4
    Top = 157
    Width = 230
    Height = 2
  end
  object Label3: TLabel
    Left = 8
    Top = 172
    Width = 51
    Height = 13
    Caption = 'Distance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object Label4: TLabel
    Left = 8
    Top = 193
    Width = 33
    Height = 13
    Caption = 'Angle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object Label5: TLabel
    Left = 10
    Top = 8
    Width = 148
    Height = 13
    Caption = 'Choose which objects to move:'
  end
  object Bevel2: TBevel
    Left = 4
    Top = 90
    Width = 230
    Height = 2
  end
  object buttonMove1: TButton
    Left = 147
    Top = 100
    Width = 80
    Height = 46
    Caption = 'Move 1'
    TabOrder = 0
    OnClick = buttonMove1Click
  end
  object buttonMove2: TButton
    Left = 147
    Top = 167
    Width = 80
    Height = 46
    Caption = 'Move 2'
    TabOrder = 1
    OnClick = buttonMove2Click
  end
  object X: TEdit
    Left = 64
    Top = 99
    Width = 60
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object Y: TEdit
    Left = 64
    Top = 125
    Width = 60
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object Distance: TEdit
    Left = 64
    Top = 168
    Width = 60
    Height = 21
    TabOrder = 4
    Text = '0'
  end
  object Angle: TMaskEdit
    Left = 64
    Top = 192
    Width = 60
    Height = 21
    MaxLength = 3
    TabOrder = 5
    Text = '0'
  end
  object cbDesignObjects: TCheckListBox
    Left = 9
    Top = 22
    Width = 218
    Height = 58
    Columns = 2
    ItemHeight = 13
    Items.Strings = (
      'Arc'
      'Track'
      'Fill'
      'Text'
      'Component'
      'Pad'
      'Via')
    TabOrder = 6
  end
end
