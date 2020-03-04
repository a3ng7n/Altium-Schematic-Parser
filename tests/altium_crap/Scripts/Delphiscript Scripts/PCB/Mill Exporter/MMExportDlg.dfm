object MMExportForm: TMMExportForm
  Left = 43
  Top = 13
  Caption = 'Export to Milling Machine'
  ClientHeight = 220
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = UserCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox4: TGroupBox
    Left = 230
    Top = 8
    Width = 219
    Height = 156
    Caption = 'Export Options'
    TabOrder = 1
    object Label2: TLabel
      Left = 7
      Top = 112
      Width = 107
      Height = 13
      Caption = 'Machine Configuration'
    end
    object EnableTopLayer: TCheckBox
      Left = 6
      Top = 17
      Width = 66
      Height = 17
      Hint = 'Enable/Disable the Top Copper Layer for export'
      Caption = 'Top Layer'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 0
    end
    object EnableBottomLayer: TCheckBox
      Left = 6
      Top = 41
      Width = 83
      Height = 17
      Hint = 'Enable/Disable the Bottom Copper Layer for export'
      Caption = 'Bottom Layer'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
    end
    object EnableDrill: TCheckBox
      Left = 6
      Top = 65
      Width = 38
      Height = 17
      Hint = 'Enable/Disable Drilling data to be exported'
      BiDiMode = bdLeftToRight
      Caption = 'Drill'
      Checked = True
      ParentBiDiMode = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 2
      OnClick = EnableDrillClick
    end
    object DrillOption: TComboBox
      Left = 19
      Top = 84
      Width = 100
      Height = 21
      Hint = 
        'spot drill holes as part of top layer export or export full dril' +
        'l files for all hole sizes'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object MMName: TComboBox
      Left = 6
      Top = 126
      Width = 185
      Height = 21
      Hint = 'Export milling data using this machine'#39's configuration settings'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      Sorted = True
      TabOrder = 4
    end
    object btnMachineConfig: TButton
      Left = 194
      Top = 127
      Width = 19
      Height = 19
      Hint = 'Add/Delete/Edit machine configuration settings'
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnMachineConfigClick
    end
  end
  object btnExport: TButton
    Left = 294
    Top = 174
    Width = 75
    Height = 25
    Caption = 'Export'
    TabOrder = 2
    OnClick = btnExportClick
  end
  object btnExit: TButton
    Left = 374
    Top = 174
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 3
    OnClick = btnExitClick
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 8
    Width = 219
    Height = 200
    Caption = 'Tool Settings'
    TabOrder = 0
    object Label6: TLabel
      Left = 8
      Top = 19
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'Cutter Diameter'
    end
    object Label7: TLabel
      Left = 170
      Top = 19
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Label1: TLabel
      Left = 21
      Top = 40
      Width = 63
      Height = 13
      Alignment = taRightJustify
      Caption = 'Cutter Depth'
    end
    object Label26: TLabel
      Left = 170
      Top = 41
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Label27: TLabel
      Left = 25
      Top = 106
      Width = 60
      Height = 13
      Alignment = taRightJustify
      Caption = 'XY-Feedrate'
    end
    object Label28: TLabel
      Left = 170
      Top = 107
      Width = 36
      Height = 13
      Caption = 'mm/sec'
    end
    object Label29: TLabel
      Left = 31
      Top = 128
      Width = 54
      Height = 13
      Alignment = taRightJustify
      Caption = 'Z-Feedrate'
    end
    object Label30: TLabel
      Left = 170
      Top = 129
      Width = 36
      Height = 13
      Caption = 'mm/sec'
    end
    object Label31: TLabel
      Left = 28
      Top = 84
      Width = 56
      Height = 13
      Alignment = taRightJustify
      Caption = 'Pass Height'
    end
    object Label32: TLabel
      Left = 170
      Top = 85
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Label3: TLabel
      Left = 35
      Top = 62
      Width = 49
      Height = 13
      Alignment = taRightJustify
      Caption = 'Drill Depth'
    end
    object Label4: TLabel
      Left = 170
      Top = 63
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Label5: TLabel
      Left = 47
      Top = 151
      Width = 38
      Height = 13
      Alignment = taRightJustify
      Caption = 'Mill RPM'
    end
    object Label8: TLabel
      Left = 170
      Top = 152
      Width = 21
      Height = 13
      Caption = 'RPM'
    end
    object Label9: TLabel
      Left = 35
      Top = 174
      Width = 50
      Height = 13
      Alignment = taRightJustify
      Caption = 'Drill Speed'
    end
    object Label10: TLabel
      Left = 170
      Top = 175
      Width = 21
      Height = 13
      Caption = 'RPM'
    end
    object CutterDiameter: TEdit
      Left = 87
      Top = 15
      Width = 81
      Height = 21
      Hint = 'Width of cutting tools sharpest point'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'CutterDiameter'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object CutterDepth: TEdit
      Left = 87
      Top = 37
      Width = 81
      Height = 21
      Hint = 'Depth to mill the PCB to'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'CutterDepth'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object XYFeedrate: TEdit
      Left = 87
      Top = 103
      Width = 81
      Height = 21
      Hint = 'Tool travel speed in the XY (horizontal) plane'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'XYFeedrate'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object ZFeedrate: TEdit
      Left = 87
      Top = 125
      Width = 81
      Height = 21
      Hint = 'Tool travel speed in the Z plane'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'ZFeedrate'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object PassHeight: TEdit
      Left = 87
      Top = 81
      Width = 81
      Height = 21
      Hint = 'Height tool will move to in the '#39'Pen Up'#39' position'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'PassHeight'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object editDrillDepth: TEdit
      Left = 87
      Top = 59
      Width = 81
      Height = 21
      Hint = 'Depth to drill PCB holes/vias to'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'editDrillDepth'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object editMillRPM: TEdit
      Left = 87
      Top = 148
      Width = 81
      Height = 21
      Hint = 'Rotational speed at which milling operations will be performed'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Text = 'editMillRPM'
      OnKeyPress = TEdit_PosInt_OnKeyPress
    end
    object editDrillRPM: TEdit
      Left = 87
      Top = 171
      Width = 81
      Height = 21
      Hint = 'Rotational speed at which drilling operations will be performed'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Text = 'editDrillRPM'
      OnKeyPress = TEdit_PosInt_OnKeyPress
    end
  end
end
