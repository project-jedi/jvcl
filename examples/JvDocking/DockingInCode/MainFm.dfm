object MainForm: TMainForm
  Left = 290
  Top = 259
  Width = 965
  Height = 678
  Caption = 'Controlling Docking from Code'
  Color = 14734247
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvSplitter1: TJvSplitter
    Left = 0
    Top = 473
    Width = 949
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 265
    Height = 473
    Align = alLeft
    Color = 15399897
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 200
      Width = 61
      Height = 13
      Caption = 'Grabber Size'
    end
    object ButtonSibDock: TButton
      Left = 6
      Top = 6
      Width = 99
      Height = 25
      Hint = 'Create new dockable document window '
      Caption = 'Sibling Dock'
      TabOrder = 0
      OnClick = ButtonSibDockClick
    end
    object Button2: TButton
      Left = 6
      Top = 164
      Width = 99
      Height = 25
      Hint = 'Display number of windows docked in the Custom Docking Area.'
      Caption = 'View Dock Tree'
      TabOrder = 5
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 6
      Top = 101
      Width = 99
      Height = 25
      Caption = 'Save Layout'
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 6
      Top = 132
      Width = 99
      Height = 25
      Caption = 'Load Layout'
      TabOrder = 4
      OnClick = Button4Click
    end
    object ButtonCreateTabDock: TButton
      Left = 6
      Top = 72
      Width = 99
      Height = 25
      Hint = 'Create a tabbed multtiple document  window'
      Caption = 'Tab Dock'
      TabOrder = 2
      OnClick = ButtonCreateTabDockClick
    end
    object ButtonCreateConjoin: TButton
      Left = 6
      Top = 39
      Width = 99
      Height = 25
      Hint = 'Create a tabbed multtiple document  window'
      Caption = 'Conjoined Dock'
      TabOrder = 1
      OnClick = ButtonCreateConjoinClick
    end
    object SpinEdit1: TSpinEdit
      Left = 8
      Top = 224
      Width = 89
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 6
      Value = 18
      OnChange = SpinEdit1Change
    end
    object tbDockRightSide: TCheckBox
      Left = 8
      Top = 270
      Width = 233
      Height = 17
      Caption = 'Dock in tabs to right side (reproduce bug)'
      TabOrder = 7
      OnClick = tbDockRightSideClick
    end
    object cbWorkaround: TCheckBox
      Left = 8
      Top = 296
      Width = 193
      Height = 17
      Caption = 'Tab-Docking workaround mode'
      Checked = True
      State = cbChecked
      TabOrder = 8
      Visible = False
    end
  end
  object Panel2: TPanel
    Left = 265
    Top = 0
    Width = 684
    Height = 473
    Align = alClient
    Caption = 'Custom Docking Area'
    Color = 4227200
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 476
    Width = 949
    Height = 164
    Align = alBottom
    Caption = 'PanelBottom'
    TabOrder = 2
    object MemoTrace: TMemo
      Left = 1
      Top = 1
      Width = 947
      Height = 162
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'Trace Messages go here.')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object dockServer: TJvDockServer
    LeftSplitterStyle.Color = clGreen
    LeftSplitterStyle.Cursor = crHSplit
    LeftSplitterStyle.ParentColor = False
    RightSplitterStyle.Color = clBlue
    RightSplitterStyle.Cursor = crHSplit
    RightSplitterStyle.ParentColor = False
    RightSplitterStyle.MinSize = 500
    TopSplitterStyle.Color = clYellow
    TopSplitterStyle.Cursor = crVSplit
    TopSplitterStyle.ParentColor = False
    BottomSplitterStyle.Color = clRed
    BottomSplitterStyle.Cursor = crVSplit
    BottomSplitterStyle.ParentColor = False
    LeftDock = False
    TopDock = False
    BottomDock = False
    DockStyle = JvDockVIDStyle1
    OnGetClientAlignSize = dockServerGetClientAlignSize
    OnFinishSetDockPanelSize = dockServerFinishSetDockPanelSize
    OnCustomPanel = dockServerCustomPanel
    Left = 288
    Top = 136
  end
  object JvDockVIDStyle1: TJvDockVIDStyle
    AlwaysShowGrabber = True
    ConjoinServerOption.GrabbersSize = 20
    ConjoinServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -13
    ConjoinServerOption.ActiveFont.Name = 'Trebuchet MS'
    ConjoinServerOption.ActiveFont.Style = [fsBold]
    ConjoinServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    ConjoinServerOption.InactiveFont.Color = 16311512
    ConjoinServerOption.InactiveFont.Height = -13
    ConjoinServerOption.InactiveFont.Name = 'Trebuchet MS'
    ConjoinServerOption.InactiveFont.Style = [fsBold]
    ConjoinServerOption.ActiveTitleStartColor = 14898176
    ConjoinServerOption.ActiveTitleEndColor = 16749885
    ConjoinServerOption.InactiveTitleStartColor = 14653050
    ConjoinServerOption.InactiveTitleEndColor = 15448477
    ConjoinServerOption.SystemInfo = False
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Sans Serif'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = clWhite
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Sans Serif'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.ShowCloseButtonOnTabs = False
    Left = 306
    Top = 232
  end
  object DockIniStorage: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    SubStorages = <>
    Left = 422
    Top = 306
  end
end
