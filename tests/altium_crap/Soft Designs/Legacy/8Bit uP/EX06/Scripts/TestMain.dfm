object AutoTester: TAutoTester
  Left = 53
  Top = 111
  BorderStyle = bsToolWindow
  Caption = 'Automated Testing...'
  ClientHeight = 442
  ClientWidth = 638
  Color = clBtnFace
  TransparentColor = True
  TransparentColorValue = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = AutoTesterClose
  OnMouseDown = AutoTesterMouseDown
  OnMouseMove = AutoTesterMouseMove
  OnMouseUp = AutoTesterMouseUp
  OnShow = AutoTesterShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel_Main: TPanel
    Left = 0
    Top = 0
    Width = 638
    Height = 426
    BevelOuter = bvNone
    Caption = 'Panel_Main'
    Ctl3D = False
    FullRepaint = False
    ParentCtl3D = False
    TabOrder = 1
    OnMouseDown = AutoTesterMouseDown
    OnMouseMove = AutoTesterMouseMove
    OnMouseUp = AutoTesterMouseUp
    object Button_Go: TButton
      Left = 554
      Top = 392
      Width = 75
      Height = 25
      Caption = '&Go'
      Default = True
      TabOrder = 0
      OnClick = Button_GoClick
    end
    object Button_Close: TButton
      Left = 473
      Top = 392
      Width = 75
      Height = 25
      Caption = '&Close'
      Default = True
      TabOrder = 1
      OnClick = Button_CloseClick
    end
    object PageControl1: TPageControl
      Left = 8
      Top = 9
      Width = 622
      Height = 378
      ActivePage = TabSheet1
      TabIndex = 0
      TabOrder = 2
      object TabSheet1: TTabSheet
        Caption = 'Log File'
        object Memo_LogFile: TMemo
          Left = 0
          Top = 0
          Width = 614
          Height = 350
          BorderStyle = bsNone
          Ctl3D = False
          ParentCtl3D = False
          ScrollBars = ssVertical
          TabOrder = 0
          OnMouseDown = AutoTesterMouseDown
          OnMouseMove = AutoTesterMouseMove
          OnMouseUp = AutoTesterMouseUp
        end
      end
    end
    object Panel3: TPanel
      Left = 683
      Top = 183
      Width = 10
      Height = 121
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'Panel2'
      Color = 16744703
      TabOrder = 3
    end
    object Panel2: TPanel
      Left = 683
      Top = 304
      Width = 185
      Height = 10
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'Panel2'
      Color = 16744703
      TabOrder = 4
    end
    object Panel5: TPanel
      Left = 8
      Top = 29
      Width = 620
      Height = 1
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'Panel2'
      Color = 16744703
      TabOrder = 5
    end
    object Panel6: TPanel
      Left = 8
      Top = 384
      Width = 620
      Height = 1
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'Panel2'
      Color = 16744703
      TabOrder = 6
    end
    object Panel4: TPanel
      Left = 8
      Top = 29
      Width = 1
      Height = 356
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'Panel2'
      Color = 16744703
      TabOrder = 7
    end
    object Panel1: TPanel
      Left = 627
      Top = 29
      Width = 1
      Height = 356
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'Panel2'
      Color = 16744703
      TabOrder = 8
    end
  end
  object Panel_ProgressBar: TPanel
    Left = -2
    Top = 426
    Width = 642
    Height = 17
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Panel_ProgressBar'
    Color = 8240895
    Ctl3D = False
    FullRepaint = False
    ParentCtl3D = False
    TabOrder = 0
    object ProgressBar: TProgressBar
      Left = -2
      Top = -2
      Width = 643
      Height = 20
      Min = 0
      Max = 100
      TabOrder = 0
      OnMouseDown = AutoTesterMouseDown
      OnMouseMove = AutoTesterMouseMove
      OnMouseUp = AutoTesterMouseUp
    end
  end
end
