object TranslatorDialog: TTranslatorDialog
  Left = 10
  Top = 0
  Width = 318
  Height = 284
  Caption = 'CirCad Translator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = OnFormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 22
    Top = 22
    Width = 203
    Height = 13
    Caption = 'Enter the Circad Schematic file to translate:'
  end
  object Label2: TLabel
    Left = 23
    Top = 83
    Width = 59
    Height = 13
    Caption = 'Output path:'
  end
  object LabelStatus: TLabel
    Left = 24
    Top = 146
    Width = 33
    Height = 13
    Caption = 'Status:'
  end
  object OutputDirCtrl: TXPDirectoryEdit
    Left = 22
    Top = 100
    Width = 267
    Height = 21
    HotTrack = False
    TabOrder = 0
    Text = 'OutputDirCtrl'
  end
  object ButtonTranslate: TButton
    Left = 215
    Top = 213
    Width = 75
    Height = 25
    Caption = 'Translate'
    TabOrder = 1
    OnClick = ButtonTranslateClick
  end
  object ButtonCancel: TButton
    Left = 123
    Top = 214
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object ProgressBarStatus: TProgressBar
    Left = 24
    Top = 172
    Width = 265
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 3
  end
  object InputFileCtrl: TEdit
    Left = 21
    Top = 39
    Width = 230
    Height = 21
    TabOrder = 4
    Text = 'InputFileCtrl'
  end
  object InputFileBrowse: TButton
    Left = 259
    Top = 39
    Width = 27
    Height = 21
    Caption = '...'
    TabOrder = 5
    OnClick = InputFileBrowseClick
  end
  object OpenDialogCtrl: TOpenDialog
    Left = 260
    Top = 67
  end
end
