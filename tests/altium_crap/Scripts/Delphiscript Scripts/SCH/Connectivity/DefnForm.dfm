object FormParamMaker: TFormParamMaker
  Left = 78
  Top = 31
  BorderStyle = bsDialog
  Caption = 'Generation of Parameters Options'
  ClientHeight = 300
  ClientWidth = 419
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormParamMakerCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 10
    Top = 10
    Width = 237
    Height = 242
    Caption = ' Document Scope '
    TabOrder = 6
    object ButtonClearAll: TButton
      Left = 14
      Top = 210
      Width = 106
      Height = 25
      Caption = 'Clear All'
      TabOrder = 0
      OnClick = ButtonClearAllClick
    end
    object ButtonSelectAll: TButton
      Left = 118
      Top = 210
      Width = 106
      Height = 25
      Caption = 'Select All'
      TabOrder = 1
      OnClick = ButtonSelectAllClick
    end
    object CheckListBoxDocuments: TCheckListBox
      Left = 12
      Top = 98
      Width = 212
      Height = 113
      ItemHeight = 13
      TabOrder = 2
    end
    object RadioGroupScope: TRadioGroup
      Left = 12
      Top = 19
      Width = 212
      Height = 69
      ItemIndex = 0
      Items.Strings = (
        'Focussed sch document only'
        'Selected sch documents')
      TabOrder = 3
      OnClick = RadioGroupScopeClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 256
    Top = 10
    Width = 154
    Height = 242
    Caption = ' Parameter Attributes '
    TabOrder = 0
    object Label2: TLabel
      Left = 12
      Top = 93
      Width = 123
      Height = 13
      Caption = 'Offset (DXP Default Units)'
    end
    object Label3: TLabel
      Left = 12
      Top = 29
      Width = 81
      Height = 13
      Caption = ' Parameter Color '
    end
    object ColorShape: TShape
      Left = 67
      Top = 46
      Width = 24
      Height = 24
      Brush.Color = clBlack
    end
    object Label1: TLabel
      Left = 13
      Top = 154
      Width = 82
      Height = 13
      Caption = 'Parameter Name:'
    end
    object XPBitBtnColor: TXPBitBtn
      Left = 13
      Top = 47
      Width = 45
      Height = 25
      Caption = 'Color'
      TabOrder = 0
      OnClick = XPBitBtnColorClick
    end
    object EditBoxOffset: TEdit
      Left = 12
      Top = 110
      Width = 42
      Height = 21
      TabOrder = 1
      Text = '40'
    end
    object UpDownParameterOffset: TUpDown
      Left = 54
      Top = 110
      Width = 17
      Height = 21
      Associate = EditBoxOffset
      Min = 0
      Max = 200
      Increment = 5
      Position = 40
      TabOrder = 2
      Wrap = False
      OnClick = UpDownParameterOffsetClick
    end
    object EditParameterName: TEdit
      Left = 13
      Top = 170
      Width = 117
      Height = 21
      TabOrder = 3
      Text = 'Net'
    end
    object CheckBoxParameterNameVisibility: TCheckBox
      Left = 13
      Top = 195
      Width = 97
      Height = 17
      Caption = 'Visible Name'
      TabOrder = 4
    end
    object CheckBoxAssignParamToPin: TCheckBox
      Left = 13
      Top = 219
      Width = 131
      Height = 17
      Caption = 'Assign Parameter to Pin'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = CheckBoxAssignParamToPinClick
    end
  end
  object XPGenerate: TXPBitBtn
    Left = 335
    Top = 268
    Width = 75
    Height = 25
    Caption = 'Generate'
    TabOrder = 1
    OnClick = XPGenerateClick
  end
  object XPCancel: TXPBitBtn
    Left = 256
    Top = 268
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = XPCancelClick
  end
  object XPButton1: TXPButton
    Left = 32
    Top = 158
    Width = 0
    Height = 0
    ParentColor = False
    Caption = 'XPButton1'
  end
  object XPButton2: TXPButton
    Left = 44
    Top = 172
    Width = 0
    Height = 0
    ParentColor = False
    Caption = 'XPButton2'
  end
  object Panel1: TPanel
    Left = 7
    Top = 260
    Width = 403
    Height = 2
    BevelOuter = bvLowered
    TabOrder = 3
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Left = 354
    Top = 62
  end
end
