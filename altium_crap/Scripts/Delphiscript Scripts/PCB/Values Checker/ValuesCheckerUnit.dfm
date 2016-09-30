object formObjectValueChecker: TformObjectValueChecker
  Left = 57
  Top = 78
  BorderStyle = bsDialog
  Caption = 'Simple PCB Check'
  ClientHeight = 219
  ClientWidth = 246
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
  object XPButton1: TXPButton
    Left = 257
    Top = 174
    Width = 0
    Height = 0
    ParentColor = False
    Caption = 'XPButton1'
  end
  object XPGroupBox1: TXPGroupBox
    Left = 12
    Top = 8
    Width = 222
    Height = 169
    Caption = 'Object Value Check'
    TabOrder = 1
    object Label1: TLabel
      Left = 174
      Top = 92
      Width = 17
      Height = 13
      Caption = 'mils'
    end
    object Label2: TLabel
      Left = 175
      Top = 114
      Width = 17
      Height = 13
      Caption = 'mils'
    end
    object XPcbZeroRadiiArcs: TXPCheckBox
      Left = 22
      Top = 23
      Width = 113
      Height = 17
      Caption = ' Zero arc radii'
      TabOrder = 0
      State = cbChecked
    end
    object XPcbZeroDiameterPads: TXPCheckBox
      Left = 22
      Top = 47
      Width = 129
      Height = 17
      Caption = ' Zero pad diameters'
      TabOrder = 1
    end
    object XPcbZeroDiameterVias: TXPCheckBox
      Left = 22
      Top = 70
      Width = 126
      Height = 17
      Caption = ' Zero via diameters'
      TabOrder = 2
    end
    object XPcbNegativeValues: TXPCheckBox
      Left = 22
      Top = 141
      Width = 161
      Height = 17
      Caption = ' Objects with negative values'
      TabOrder = 3
    end
    object XPcbPadDiameters: TXPCheckBox
      Left = 22
      Top = 94
      Width = 90
      Height = 17
      Caption = ' Pad Diameters'
      TabOrder = 4
    end
    object XPcbViaDiameters: TXPCheckBox
      Left = 22
      Top = 117
      Width = 86
      Height = 17
      Caption = ' Via Diameters'
      TabOrder = 5
    end
    object MEPadDiameter: TMaskEdit
      Left = 137
      Top = 89
      Width = 30
      Height = 21
      TabOrder = 6
      Text = '5'
    end
    object MEViaDiameter: TMaskEdit
      Left = 137
      Top = 111
      Width = 30
      Height = 21
      TabOrder = 7
      Text = '5'
    end
  end
  object bCancel: TButton
    Left = 159
    Top = 189
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = bCancelClick
  end
  object bOK: TButton
    Left = 78
    Top = 189
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = bOKClick
  end
end
