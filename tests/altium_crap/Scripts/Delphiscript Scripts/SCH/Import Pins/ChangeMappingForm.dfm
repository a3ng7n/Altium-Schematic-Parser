object FormChangeMapping: TFormChangeMapping
  Left = 104
  Top = 81
  BorderStyle = bsDialog
  Caption = 'Change Mapping'
  ClientHeight = 169
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormChangeMappingCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelSelect: TLabel
    Left = 8
    Top = 8
    Width = 177
    Height = 13
    Caption = 'Select a Pin Property for the text field:'
  end
  object LabelTextField: TLabel
    Left = 8
    Top = 24
    Width = 69
    Height = 13
    Caption = 'LabelTextField'
  end
  object ComboBox: TComboBox
    Left = 8
    Top = 48
    Width = 280
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'Display Name'
    Items.Strings = (
      'Display Name'
      'Designator'
      'Electrical Type')
  end
  object CheckBox: TCheckBox
    Left = 8
    Top = 112
    Width = 97
    Height = 17
    Caption = 'Import this field'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object ButtonOk: TButton
    Left = 136
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 216
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
