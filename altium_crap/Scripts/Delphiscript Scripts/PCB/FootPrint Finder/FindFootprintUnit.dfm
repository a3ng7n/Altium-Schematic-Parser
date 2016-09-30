object FormFootprintFinder: TFormFootprintFinder
  Left = 34
  Top = 50
  BorderStyle = bsDialog
  Caption = 'Footprint Finder...'
  ClientHeight = 269
  ClientWidth = 310
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
  object Label1: TLabel
    Left = 12
    Top = 13
    Width = 75
    Height = 13
    Caption = 'Footprint Name:'
  end
  object Label2: TLabel
    Left = 8
    Top = 82
    Width = 99
    Height = 13
    Caption = 'PCB Libraries found :'
  end
  object Bevel1: TBevel
    Left = 7
    Top = 228
    Width = 300
    Height = 2
  end
  object lFootprint: TLabel
    Left = 9
    Top = 206
    Width = 3
    Height = 13
  end
  object lNumberOfLibs: TLabel
    Left = 9
    Top = 242
    Width = 51
    Height = 13
    Caption = '0 of 0 Files'
  end
  object eFootprintName: TEdit
    Left = 174
    Top = 10
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'DIODE-0.4'
  end
  object lbFootprintLibraries: TListBox
    Left = 7
    Top = 100
    Width = 294
    Height = 97
    ItemHeight = 13
    TabOrder = 1
  end
  object bSearch: TButton
    Left = 148
    Top = 237
    Width = 75
    Height = 25
    Caption = 'Search'
    TabOrder = 2
    OnClick = bSearchClick
  end
  object bCancel: TButton
    Left = 226
    Top = 237
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 3
    OnClick = bCancelClick
  end
  object XPDirectoryEdit1: TXPDirectoryEdit
    Left = 9
    Top = 48
    Width = 289
    Height = 21
    HotTrack = False
    InitialDir = 'C:\Program Files\Altium Designer 6\Library\Pcb'
    TabOrder = 4
    Text = 'C:\Program Files\Altium Designer 6\Library\Pcb'
  end
end
