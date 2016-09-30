object HoleSizeEditorForm: THoleSizeEditorForm
  Left = 162
  Top = 85
  BorderStyle = bsDialog
  Caption = 'Hole Size Editor'
  ClientHeight = 271
  ClientWidth = 240
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
  object lHoleSizeCount: TLabel
    Left = 64
    Top = 210
    Width = 80
    Height = 13
    Caption = 'Pads : 0  Vias : 0'
  end
  object Label2: TLabel
    Left = 14
    Top = 7
    Width = 80
    Height = 13
    Caption = 'Hole Sizes found'
  end
  object ListBox1: TListBox
    Left = 13
    Top = 22
    Width = 121
    Height = 173
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
  end
  object GroupBox1: TGroupBox
    Left = 143
    Top = 17
    Width = 91
    Height = 88
    Caption = ' Include '
    TabOrder = 1
    object cbPads: TCheckBox
      Left = 12
      Top = 26
      Width = 52
      Height = 17
      Caption = ' Pads'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbPadsClick
    end
    object cbVias: TCheckBox
      Left = 12
      Top = 48
      Width = 40
      Height = 17
      Caption = ' Vias'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbViasClick
    end
  end
  object rgUnit: TRadioGroup
    Left = 143
    Top = 115
    Width = 90
    Height = 80
    Caption = ' Unit '
    ItemIndex = 0
    Items.Strings = (
      'Imperial'
      'Metric')
    TabOrder = 2
    OnClick = rgUnitClick
  end
  object bOK: TButton
    Left = 79
    Top = 242
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 160
    Top = 242
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = bCancelClick
  end
  object bEdit: TButton
    Left = 13
    Top = 202
    Width = 40
    Height = 25
    Caption = 'Edit...'
    TabOrder = 5
    OnClick = bEditClick
  end
  object Panel1: TPanel
    Left = 3
    Top = 234
    Width = 234
    Height = 2
    BevelOuter = bvLowered
    TabOrder = 6
  end
end
