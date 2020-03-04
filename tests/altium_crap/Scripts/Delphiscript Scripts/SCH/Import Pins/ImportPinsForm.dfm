object ImportPinsForm: TImportPinsForm
  Left = 117
  Top = 90
  BorderStyle = bsDialog
  Caption = 'Pins Importer'
  ClientHeight = 329
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelImport: TLabel
    Left = 8
    Top = 8
    Width = 121
    Height = 13
    Caption = 'CSV Format File to import:'
  end
  object LabelMapFields: TLabel
    Left = 8
    Top = 56
    Width = 156
    Height = 13
    Caption = 'Map the fields you wish to import:'
  end
  object Edit: TEdit
    Left = 8
    Top = 24
    Width = 337
    Height = 21
    TabOrder = 0
  end
  object ButtonBrowse: TButton
    Left = 352
    Top = 22
    Width = 65
    Height = 26
    Caption = 'Browse ...'
    TabOrder = 1
    OnClick = ButtonBrowseClick
  end
  object ListView: TListView
    Left = 8
    Top = 72
    Width = 409
    Height = 217
    Checkboxes = True
    Columns = <
      item
        Caption = 'Text Field'
        Width = 192
      end
      item
        Caption = 'Pin Property'
        Width = 192
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object ButtonChangeMapping: TButton
    Left = 144
    Top = 296
    Width = 128
    Height = 25
    Caption = 'Change Mapping ...'
    TabOrder = 3
    OnClick = ButtonChangeMappingClick
  end
  object ButtonUpdateMapping: TButton
    Left = 8
    Top = 296
    Width = 128
    Height = 25
    Caption = 'Update Mapping'
    TabOrder = 4
    OnClick = ButtonUpdateMappingClick
  end
  object ButtonImport: TButton
    Left = 344
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 5
    OnClick = ButtonImportClick
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'Comma Separated Values (*.csv)|*.CSV|Text files (*.txt)|*.TXT|Al' +
      'l Files (*.*)|*.*'
    Title = 'Open'
    Left = 344
    Top = 144
  end
end
