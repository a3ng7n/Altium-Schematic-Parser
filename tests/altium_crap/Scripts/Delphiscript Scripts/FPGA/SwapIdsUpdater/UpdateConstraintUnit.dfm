object fUpdate: TfUpdate
  Left = 25
  Top = 11
  BorderStyle = bsDialog
  Caption = 'Update SwapIDs in the constraint file'
  ClientHeight = 270
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 150
    Top = 85
    Width = 73
    Height = 13
    Caption = 'Group Swap ID'
  end
  object Label2: TLabel
    Left = 10
    Top = 81
    Width = 72
    Height = 13
    Caption = 'Selected Pads:'
  end
  object Bevel1: TBevel
    Left = 2
    Top = 236
    Width = 320
    Height = 2
  end
  object Label3: TLabel
    Left = 10
    Top = 33
    Width = 162
    Height = 13
    Caption = 'Choose a Constraint File to update'
  end
  object labelTargetDeviceName: TLabel
    Left = 10
    Top = 11
    Width = 74
    Height = 13
    Caption = 'Target Device: '
  end
  object editSwapID: TEdit
    Left = 149
    Top = 100
    Width = 34
    Height = 21
    TabOrder = 0
    Text = '1'
  end
  object bOK: TButton
    Left = 246
    Top = 241
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = bOKClick
  end
  object Button2: TButton
    Left = 169
    Top = 241
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object UpDown1: TUpDown
    Left = 183
    Top = 100
    Width = 16
    Height = 21
    Associate = editSwapID
    Min = 0
    Position = 1
    TabOrder = 3
    Wrap = False
    OnChanging = UpDown1Changing
  end
  object cbConstraintFiles: TComboBox
    Left = 10
    Top = 48
    Width = 309
    Height = 21
    ItemHeight = 13
    TabOrder = 4
    OnClick = cbConstraintFilesClick
  end
  object clbPads: TCheckListBox
    Left = 10
    Top = 96
    Width = 120
    Height = 116
    ItemHeight = 13
    TabOrder = 5
  end
  object bEnableAll: TButton
    Left = 10
    Top = 212
    Width = 60
    Height = 18
    Caption = 'Enable all'
    TabOrder = 6
    OnClick = bEnableAllClick
  end
  object bDisableAll: TButton
    Left = 70
    Top = 212
    Width = 60
    Height = 18
    Caption = 'Disable all'
    TabOrder = 7
    OnClick = bDisableAllClick
  end
end
