object FilterForm: TFilterForm
  Left = 16
  Top = 20
  BorderStyle = bsDialog
  Caption = 'Filter Wizard'
  ClientHeight = 80
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = False
  OnCreate = Form1Create
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 18
    Width = 63
    Height = 13
    Caption = 'Capacitance:'
  end
  object Label2: TLabel
    Left = 10
    Top = 50
    Width = 56
    Height = 13
    Caption = 'Resistance:'
  end
  object txtCapacitance: TEdit
    Left = 80
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object txtResistance: TEdit
    Left = 80
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 216
    Top = 16
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 216
    Top = 48
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
end
