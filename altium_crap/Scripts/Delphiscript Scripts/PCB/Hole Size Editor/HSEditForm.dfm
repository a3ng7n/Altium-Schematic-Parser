object EditHoleSizeForm: TEditHoleSizeForm
  Left = 401
  Top = 428
  BorderStyle = bsDialog
  Caption = 'Edit Hole Size'
  ClientHeight = 107
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 224
    Height = 64
    Caption = ' Hole Size '
    TabOrder = 0
    object eEditHoleSize: TEdit
      Left = 8
      Top = 24
      Width = 208
      Height = 21
      TabOrder = 0
    end
  end
  object bOk: TButton
    Left = 76
    Top = 77
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = bOkClick
  end
  object bCancel: TButton
    Left = 156
    Top = 77
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = bCancelClick
  end
end
