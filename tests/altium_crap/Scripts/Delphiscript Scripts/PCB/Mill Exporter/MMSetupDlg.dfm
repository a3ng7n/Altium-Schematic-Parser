object MMSetupForm: TMMSetupForm
  Left = 64
  Top = 11
  Caption = 'Setup Milling Machine'
  ClientHeight = 296
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 21
    Top = 19
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object MMName: TComboBox
    Left = 53
    Top = 16
    Width = 223
    Height = 21
    Hint = 'The name of the current machine configuration'
    Style = csDropDownList
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    Sorted = True
    TabOrder = 0
    OnChange = MMNameChange
  end
  object New: TButton
    Left = 282
    Top = 14
    Width = 75
    Height = 25
    Hint = 'Create a new machine configuration'
    Caption = 'New'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = NewClick
  end
  object Delete: TButton
    Left = 363
    Top = 14
    Width = 75
    Height = 25
    Hint = 'Delete the current machine configuration'
    Caption = 'Delete'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = DeleteClick
  end
  object ToolRangeGroup: TGroupBox
    Left = 12
    Top = 147
    Width = 112
    Height = 104
    Caption = 'XYZ-axis travel'
    TabOrder = 2
    object Label8: TLabel
      Left = 8
      Top = 30
      Width = 6
      Height = 13
      Caption = 'X'
    end
    object Label9: TLabel
      Left = 8
      Top = 52
      Width = 6
      Height = 13
      Caption = 'Y'
    end
    object Label10: TLabel
      Left = 8
      Top = 75
      Width = 6
      Height = 13
      Caption = 'Z'
    end
    object Label11: TLabel
      Left = 85
      Top = 33
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Label12: TLabel
      Left = 85
      Top = 53
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Label13: TLabel
      Left = 85
      Top = 73
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object MaxX: TEdit
      Left = 19
      Top = 27
      Width = 62
      Height = 21
      Hint = 'Maximum accessible width of the milling head'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'MaxX'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object MaxY: TEdit
      Left = 19
      Top = 49
      Width = 62
      Height = 21
      Hint = 'Maximum accessible depth of the milling head'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'MaxY'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object MaxZ: TEdit
      Left = 19
      Top = 71
      Width = 62
      Height = 21
      Hint = 'Maximum accessible height of the milling head'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'MaxZ'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
  end
  object GroupBox2: TGroupBox
    Left = 126
    Top = 147
    Width = 201
    Height = 104
    Caption = 'Operating Speed'
    TabOrder = 3
    object Label18: TLabel
      Left = 63
      Top = 16
      Width = 16
      Height = 13
      Caption = 'Min'
    end
    object Label19: TLabel
      Left = 6
      Top = 33
      Width = 35
      Height = 13
      Caption = 'XY-axis'
    end
    object Label20: TLabel
      Left = 155
      Top = 32
      Width = 36
      Height = 13
      Caption = 'mm/sec'
    end
    object Label21: TLabel
      Left = 117
      Top = 15
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object Label22: TLabel
      Left = 11
      Top = 54
      Width = 29
      Height = 13
      Caption = 'Z-axis'
    end
    object Label23: TLabel
      Left = 155
      Top = 53
      Width = 36
      Height = 13
      Caption = 'mm/sec'
    end
    object Label24: TLabel
      Left = 6
      Top = 75
      Width = 34
      Height = 13
      Caption = 'Spindle'
    end
    object Label25: TLabel
      Left = 155
      Top = 74
      Width = 18
      Height = 13
      Caption = 'rpm'
    end
    object MinXYSpeed: TEdit
      Left = 43
      Top = 29
      Width = 54
      Height = 21
      Hint = 'Minimum travel speed in the XY (horizontal) direction'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'MinXYSpeed'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object MaxXYSpeed: TEdit
      Left = 98
      Top = 29
      Width = 54
      Height = 21
      Hint = 'Maximum travel speed in the XY (horizontal) direction'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'MaxXYSpeed'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object MinZSpeed: TEdit
      Left = 43
      Top = 50
      Width = 54
      Height = 21
      Hint = 'Minimum travel speed in the Z (vertical) direction'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'MinZSpeed'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object MaxZSpeed: TEdit
      Left = 98
      Top = 50
      Width = 54
      Height = 21
      Hint = 'Maximum travel speed in the Z (vertical) direction'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'MaxZSpeed'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
    object MinSpindleSpeed: TEdit
      Left = 43
      Top = 71
      Width = 54
      Height = 21
      Hint = 'Minimum rotational speed of milling head'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'MinSpindleSpeed'
      OnKeyPress = TEdit_PosInt_OnKeyPress
    end
    object MaxSpindleSpeed: TEdit
      Left = 98
      Top = 71
      Width = 54
      Height = 21
      Hint = 'Maximum rotational speed of milling head'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'MaxSpindleSpeed'
      OnKeyPress = TEdit_PosInt_OnKeyPress
    end
  end
  object GroupBox1: TGroupBox
    Left = 13
    Top = 43
    Width = 426
    Height = 103
    Caption = 'General Settings'
    TabOrder = 1
    object Label2: TLabel
      Left = 28
      Top = 57
      Width = 53
      Height = 13
      Caption = 'Description'
    end
    object Label3: TLabel
      Left = 162
      Top = 16
      Width = 65
      Height = 13
      Caption = 'Manufacturer'
    end
    object Label4: TLabel
      Left = 6
      Top = 16
      Width = 28
      Height = 13
      Caption = 'Model'
    end
    object Label5: TLabel
      Left = 304
      Top = 15
      Width = 66
      Height = 13
      Caption = 'Command Set'
    end
    object Label6: TLabel
      Left = 4
      Top = 80
      Width = 77
      Height = 13
      Caption = 'Cutter Diameter'
    end
    object Label7: TLabel
      Left = 165
      Top = 81
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Description: TEdit
      Left = 82
      Top = 54
      Width = 335
      Height = 21
      Hint = 'General description of this milling machine'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'Description'
    end
    object Manufacturer: TEdit
      Left = 160
      Top = 31
      Width = 142
      Height = 21
      Hint = 'Milling machine manufacturer'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'Manufacturer'
    end
    object Model: TEdit
      Left = 5
      Top = 31
      Width = 154
      Height = 21
      Hint = 'Manufacturer'#39's model'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'Model'
    end
    object CommandSet: TComboBox
      Left = 304
      Top = 31
      Width = 116
      Height = 21
      Hint = 'Milling Machine command language'
      Enabled = False
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      Sorted = True
      TabOrder = 2
    end
    object CutterDiameter: TEdit
      Left = 82
      Top = 77
      Width = 81
      Height = 21
      Hint = 'The diameter of the cutting blade tip'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'CutterDiameter'
      OnKeyPress = TEdit_PosReal_OnKeyPress
    end
  end
  object Save: TButton
    Left = 283
    Top = 261
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = SaveClick
  end
  object bnExit: TButton
    Left = 362
    Top = 261
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 5
    OnClick = bnExitClick
  end
  object GroupBox3: TGroupBox
    Left = 329
    Top = 147
    Width = 110
    Height = 104
    Caption = 'Margin'
    TabOrder = 8
    object Label14: TLabel
      Left = 9
      Top = 33
      Width = 6
      Height = 13
      Caption = 'X'
    end
    object Label15: TLabel
      Left = 85
      Top = 34
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object Label16: TLabel
      Left = 9
      Top = 56
      Width = 6
      Height = 13
      Caption = 'Y'
    end
    object Label17: TLabel
      Left = 85
      Top = 57
      Width = 16
      Height = 13
      Caption = 'mm'
    end
    object XMargin: TEdit
      Left = 19
      Top = 30
      Width = 62
      Height = 21
      Hint = 
        'X-offset from the machine'#39's 0,0 location that milling will begin' +
        ' from'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'XMargin'
    end
    object YMargin: TEdit
      Left = 19
      Top = 53
      Width = 62
      Height = 21
      Hint = 
        'Y-offset from the machine'#39's 0,0 location that milling will begin' +
        ' from'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'YMargin'
    end
  end
end
