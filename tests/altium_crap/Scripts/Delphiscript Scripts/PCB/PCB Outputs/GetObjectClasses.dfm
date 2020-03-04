object form_GetClass: Tform_GetClass
  Left = 40
  Top = 77
  BorderStyle = bsDialog
  Caption = 'Get Object Classes'
  ClientHeight = 275
  ClientWidth = 314
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
    Left = 10
    Top = 7
    Width = 92
    Height = 13
    Caption = 'Object Class Type: '
  end
  object Label2: TLabel
    Left = 10
    Top = 62
    Width = 166
    Height = 13
    Caption = 'Members of this Object Class Type:'
  end
  object bOK: TButton
    Left = 154
    Top = 245
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
  end
  object bCancel: TButton
    Left = 231
    Top = 245
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = bCancelClick
  end
  object Classes: TXPListBox
    Left = 10
    Top = 77
    Width = 297
    Height = 160
    TabOrder = 2
    OnDblClick = ClassesDblClick
  end
  object cb_ClassMemberKind: TComboBox
    Left = 10
    Top = 26
    Width = 297
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 3
    Text = 'Net'
    OnChange = cb_ClassMemberKindChange
    Items.Strings = (
      'Net'
      'Component'
      'FromTo'
      'Pad'
      'Layer'
      'DesignChannel')
  end
end
