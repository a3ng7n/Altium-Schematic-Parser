object formZipper: TformZipper
  Left = 75
  Top = 34
  BorderStyle = bsDialog
  Caption = 'Design Project Zipper'
  ClientHeight = 230
  ClientWidth = 414
  Color = clBtnFace
  DefaultMonitor = dmDesktop
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
    Left = 14
    Top = 11
    Width = 114
    Height = 13
    Caption = 'Choose a project to zip: '
  end
  object Label2: TLabel
    Left = 205
    Top = 10
    Width = 126
    Height = 13
    Caption = 'Choose project files to zip: '
  end
  object Label3: TLabel
    Left = 15
    Top = 143
    Width = 100
    Height = 13
    Caption = 'Choose zip filename: '
  end
  object Bevel1: TBevel
    Left = 4
    Top = 193
    Width = 402
    Height = 2
  end
  object bClose: TButton
    Left = 328
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 0
    OnClick = bCloseClick
  end
  object Zip: TButton
    Left = 242
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Zip'
    TabOrder = 1
    OnClick = ZipClick
  end
  object CheckListBoxProjectFiles: TCheckListBox
    Left = 204
    Top = 27
    Width = 200
    Height = 139
    ItemHeight = 13
    TabOrder = 2
  end
  object bEnableAll: TButton
    Left = 204
    Top = 166
    Width = 100
    Height = 18
    Caption = 'Enable All'
    TabOrder = 3
    OnClick = bEnableAllClick
  end
  object bClearAll: TButton
    Left = 304
    Top = 166
    Width = 100
    Height = 18
    Caption = 'Clear All'
    TabOrder = 4
    OnClick = bClearAllClick
  end
  object XPProjectFileNameEdit: TXPFileNameEdit
    Left = 14
    Top = 26
    Width = 170
    Height = 21
    FilterIndex = 0
    HotTrack = False
    TabOrder = 5
    Title = 'Choose a design project to archive...'
    OnChange = XPProjectFileNameEditChange
  end
  object XPZipFileNameEdit: TXPFileNameEdit
    Left = 14
    Top = 161
    Width = 170
    Height = 21
    FilterIndex = 0
    HotTrack = False
    TabOrder = 6
  end
  object cbIncludeProjectFile: TCheckBox
    Left = 14
    Top = 60
    Width = 151
    Height = 17
    Caption = 'Include Project file in zip file'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object cbIncludeGeneratedFiles: TCheckBox
    Left = 14
    Top = 84
    Width = 175
    Height = 17
    Caption = 'Include generated files in zip file'
    TabOrder = 8
  end
  object cbIncludePathsInZipFile: TCheckBox
    Left = 14
    Top = 108
    Width = 131
    Height = 17
    Caption = 'Include paths in zip file'
    TabOrder = 9
  end
end
