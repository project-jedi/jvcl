object MainForm: TMainForm
  Left = 228
  Top = 269
  Width = 586
  Height = 341
  Anchors = []
  Caption = 'Main Window'
  Color = clGray
  DefaultMonitor = dmMainForm
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 578
    Height = 26
    Anchors = []
    AutoSize = True
    ButtonWidth = 80
    Caption = 'ToolBar1'
    Color = clBtnFace
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    Flat = True
    ParentColor = False
    ShowCaptions = True
    TabOrder = 0
    object btnDelphi: TToolButton
      Left = 0
      Top = 0
      Action = actNewWindowDelphiStyle
      AllowAllUp = True
      AutoSize = True
    end
    object btnVC: TToolButton
      Left = 75
      Top = 0
      Action = actNewWindowVCStyle
      AllowAllUp = True
      AutoSize = True
    end
    object btnVID: TToolButton
      Left = 148
      Top = 0
      Action = actNewWindowVIDStyle
      AllowAllUp = True
      AutoSize = True
    end
    object btnVSNet: TToolButton
      Left = 210
      Top = 0
      Action = actNewWindowVSNetStyle
    end
    object ToolButton2: TToolButton
      Left = 290
      Top = 0
      Action = actNewWindowVIDVCStyle
    end
    object ToolButton1: TToolButton
      Left = 370
      Top = 0
      Caption = 'Close All'
      ImageIndex = 4
      OnClick = CloseAllClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 273
    Width = 578
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 26
    Width = 578
    Height = 247
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Lines.Strings = (
      'This demo show you how use the JVCL docking components.'
      ''
      'There are four buttons in the toolbar of the application: '
      '  '#39'Delphi Style'#39', '#39'VC++ Style'#39', '#39'VID Style'#39' and '#39'VS.NET Style'#39'.'
      ''
      
        'If you click '#39'Delphi Style'#39' button, the application will create ' +
        'a window that looks and behaves similar to Delphi'#39's standard doc' +
        'k style.'
      
        'If you click '#39'VC++ Style'#39' button, the application will create a ' +
        'window that looks and behaves similar to Visual C++'#39's standard d' +
        'ock style.'
      
        'If you click '#39'VID Style'#39' button, the application will create a w' +
        'indow looks and behaves similar to Visual InterDev'#39's standard do' +
        'ck style.'
      
        'If you click '#39'VS.NET Style'#39' button, the application will create ' +
        'a window looks and behaves similar to Visual Studio.Net'#39's standa' +
        'rd dock style.'
      ''
      
        'Since the docking is controlled by the TJvDockserver component (' +
        'on this form), you have to set it'#39's DockStyle property to the ty' +
        'pe of docking you want to see before running the'
      
        'demo. That is, if you want Delphi style docking, set lbDockServe' +
        'r1.DockStyle to JvDockDelphiStyle1. If you want VS.net style doc' +
        'king, set lbDockServer1.DockStyle JvDockVSNetStyle1 etc.'
      ''
      
        'Only one type of docking can be active at the time. You can stil' +
        'l create the other types of docking forms but their docking beha' +
        'vior will be disabled.'
      ''
      
        'When you have the correct type of docking behavior activated, yo' +
        'u can drag the corresponding dock form to any edge of this form ' +
        'to dock it. You can create as many forms you like of each type.')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Left = 64
    Top = 48
    object DockForm_Menu: TMenuItem
      Caption = 'Dockable Window'
      object DelphiStyle: TMenuItem
        Action = actNewWindowDelphiStyle
      end
      object VCStyle: TMenuItem
        Action = actNewWindowVCStyle
      end
      object VIDStyle: TMenuItem
        Action = actNewWindowVIDStyle
      end
      object VSNETStyle: TMenuItem
        Action = actNewWindowVSNetStyle
      end
      object VIDVCStyle1: TMenuItem
        Action = actNewWindowVIDVCStyle
      end
    end
    object ShowWindow_Menu: TMenuItem
      Caption = 'Show Window'
    end
    object DockInfo_Menu: TMenuItem
      Caption = 'Dock Information'
      object SaveToIniFile: TMenuItem
        Caption = 'Save To Ini File'
        OnClick = SaveToIniFileClick
      end
      object LoadFromIniFile: TMenuItem
        Caption = 'Load From Ini File'
        OnClick = LoadFromIniFileClick
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object SaveToReg: TMenuItem
        Caption = 'Save To Registry'
        OnClick = SaveToRegClick
      end
      object LoadFromReg: TMenuItem
        Caption = 'Load From Registry'
        OnClick = LoadFromRegClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object SaveToXmlFile: TMenuItem
        Caption = 'Save To Xml File'
        OnClick = SaveToXmlFileClick
      end
      object LoadFromXmlFile: TMenuItem
        Caption = 'Load From Xml File'
        OnClick = LoadFromXmlFileClick
      end
    end
    object DockOption_Menu: TMenuItem
      Caption = 'Dock Option'
      object TopDocked: TMenuItem
        Caption = 'Top Dockable'
        OnClick = TopDockedClick
      end
      object BottomDocked: TMenuItem
        Caption = 'Bottom Dockable'
        OnClick = BottomDockedClick
      end
      object LeftDocked: TMenuItem
        Caption = 'Left Dockable'
        OnClick = LeftDockedClick
      end
      object RightDocked: TMenuItem
        Caption = 'Right Dockable'
        OnClick = RightDockedClick
      end
      object N31: TMenuItem
        Caption = '-'
      end
      object AllDocked: TMenuItem
        Caption = 'All Dockable'
        OnClick = AllDockedClick
      end
    end
    object ServerStyle_Menu: TMenuItem
      Caption = 'Server Style'
      object ServerDelphiStyle: TMenuItem
        Action = actServerStyleDelphi
      end
      object ServerVisualCStyle: TMenuItem
        Action = actServerStyleVC
      end
      object ServerVisualInterDevStyle: TMenuItem
        Action = actServerStyleVID
      end
      object ServerVisualStudionetStyle: TMenuItem
        Action = actServerStyleVSNet
      end
      object VisualInterDevCStyle1: TMenuItem
        Action = actServerStyleVIDVC
      end
    end
  end
  object lbDockServer1: TJvDockServer
    LeftSplitterStyle.Cursor = crHSplit
    LeftSplitterStyle.ParentColor = False
    LeftSplitterStyle.Size = 4
    TopSplitterStyle.Cursor = crVSplit
    TopSplitterStyle.ParentColor = False
    TopSplitterStyle.Size = 4
    RightSplitterStyle.Cursor = crHSplit
    RightSplitterStyle.ParentColor = False
    RightSplitterStyle.Size = 4
    BottomSplitterStyle.Cursor = crVSplit
    BottomSplitterStyle.ParentColor = False
    BottomSplitterStyle.Size = 4
    DockStyle = JvDockVIDVCStyle1
    Left = 32
    Top = 48
  end
  object JvDockDelphiStyle1: TJvDockDelphiStyle
    ConjoinServerOption.GrabbersSize = 12
    ConjoinServerOption.SplitterWidth = 4
    Left = 32
    Top = 112
  end
  object JvDockVCStyle1: TJvDockVCStyle
    ConjoinServerOption.GrabbersSize = 15
    ConjoinServerOption.SplitterWidth = 4
    Left = 64
    Top = 112
  end
  object JvDockVIDStyle1: TJvDockVIDStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = ANSI_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = [fsBold]
    ConjoinServerOption.InactiveFont.Charset = ANSI_CHARSET
    ConjoinServerOption.InactiveFont.Color = 13160660
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = [fsBold]
    ConjoinServerOption.TextAlignment = taLeftJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = clSkyBlue
    ConjoinServerOption.InactiveTitleStartColor = clGray
    ConjoinServerOption.InactiveTitleEndColor = clSilver
    ConjoinServerOption.TextEllipsis = True
    ConjoinServerOption.SystemInfo = True
    TabServerOption.TabPosition = tpBottom
    TabServerOption.ActiveSheetColor = clBtnFace
    TabServerOption.InactiveSheetColor = clBtnShadow
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Shell Dlg 2'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = clWhite
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Shell Dlg 2'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = False
    Left = 96
    Top = 112
  end
  object PopupMenu2: TPopupMenu
    OnPopup = PopupMenu2Popup
    Left = 136
    Top = 48
    object ClientTopDocked: TMenuItem
      Caption = 'Top Dockable'
      Checked = True
      OnClick = ClientTopDockedClick
    end
    object ClientBottomDocked: TMenuItem
      Caption = 'Bottom Dockable'
      Checked = True
      OnClick = ClientBottomDockedClick
    end
    object ClientLeftDocked: TMenuItem
      Caption = 'Left Dockable'
      Checked = True
      OnClick = ClientLeftDockedClick
    end
    object ClientRightDocked: TMenuItem
      Caption = 'Right Dockable'
      Checked = True
      OnClick = ClientRightDockedClick
    end
    object N20: TMenuItem
      Caption = '-'
    end
    object ClientEachOtherDocked: TMenuItem
      Caption = 'Each Other Dockable'
      Checked = True
      OnClick = ClientEachOtherDockedClick
    end
    object ClientAllDocked: TMenuItem
      Caption = 'All Dockable'
      Checked = True
      OnClick = ClientAllDockedClick
    end
    object N21: TMenuItem
      Caption = '-'
    end
    object ClientDockorFloat: TMenuItem
      Caption = 'Dock'
      OnClick = ClientDockorFloatClick
    end
    object ClientHide: TMenuItem
      Caption = 'Hide'
      OnClick = ClientHideClick
    end
  end
  object JvDockVSNetStyle1: TJvDockVSNetStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = ANSI_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = []
    ConjoinServerOption.InactiveFont.Charset = ANSI_CHARSET
    ConjoinServerOption.InactiveFont.Color = clBlack
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = []
    ConjoinServerOption.TextAlignment = taLeftJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = 6956042
    ConjoinServerOption.InactiveTitleStartColor = clBtnFace
    ConjoinServerOption.InactiveTitleEndColor = clBtnFace
    ConjoinServerOption.TextEllipsis = True
    ConjoinServerOption.SystemInfo = True
    TabServerOption.TabPosition = tpBottom
    TabServerOption.ActiveSheetColor = clBtnFace
    TabServerOption.InactiveSheetColor = clInfoBk
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Shell Dlg 2'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = 5395794
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Shell Dlg 2'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = True
    ChannelOption.ActivePaneSize = 150
    ChannelOption.ShowImage = True
    ChannelOption.HideHoldTime = 1000
    Left = 128
    Top = 112
  end
  object JvDockVIDVCStyle1: TJvDockVIDVCStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = ANSI_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = [fsBold]
    ConjoinServerOption.InactiveFont.Charset = ANSI_CHARSET
    ConjoinServerOption.InactiveFont.Color = 13160660
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = [fsBold]
    ConjoinServerOption.TextAlignment = taRightJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = clSkyBlue
    ConjoinServerOption.InactiveTitleStartColor = clGray
    ConjoinServerOption.InactiveTitleEndColor = clSilver
    ConjoinServerOption.TextEllipsis = True
    ConjoinServerOption.SystemInfo = False
    TabServerOption.HotTrack = True
    TabServerOption.TabPosition = tpBottom
    TabServerOption.ActiveSheetColor = clBtnFace
    TabServerOption.InactiveSheetColor = clBtnShadow
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
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = True
    Left = 216
    Top = 112
  end
  object ActionList1: TActionList
    Left = 192
    Top = 48
    object actServerStyleDelphi: TAction
      Category = 'ServerStyle'
      Caption = 'Delphi Style'
      OnExecute = actServerStyleExecute
      OnUpdate = actServerStyleUpdate
    end
    object actServerStyleVC: TAction
      Tag = 1
      Category = 'ServerStyle'
      Caption = 'Visual C++ Style'
      OnExecute = actServerStyleExecute
      OnUpdate = actServerStyleUpdate
    end
    object actServerStyleVID: TAction
      Tag = 2
      Category = 'ServerStyle'
      Caption = 'Visual InterDev Style'
      OnExecute = actServerStyleExecute
      OnUpdate = actServerStyleUpdate
    end
    object actServerStyleVSNet: TAction
      Tag = 3
      Category = 'ServerStyle'
      Caption = 'Visual Studio.net Style'
      OnExecute = actServerStyleExecute
      OnUpdate = actServerStyleUpdate
    end
    object actServerStyleVIDVC: TAction
      Tag = 4
      Category = 'ServerStyle'
      Caption = 'Visual InterDev C++ Style'
      OnExecute = actServerStyleExecute
      OnUpdate = actServerStyleUpdate
    end
    object actNewWindowDelphiStyle: TAction
      Category = 'New Window'
      Caption = 'Delphi Style'
      OnExecute = NewWindowExecute
    end
    object actNewWindowVCStyle: TAction
      Tag = 1
      Category = 'New Window'
      Caption = 'VC++ Style'
      OnExecute = NewWindowExecute
    end
    object actNewWindowVIDStyle: TAction
      Tag = 2
      Category = 'New Window'
      Caption = 'VID Style'
      OnExecute = NewWindowExecute
    end
    object actNewWindowVSNetStyle: TAction
      Tag = 3
      Category = 'New Window'
      Caption = 'VS.NET Style'
      OnExecute = NewWindowExecute
    end
    object actNewWindowVIDVCStyle: TAction
      Tag = 4
      Category = 'New Window'
      Caption = 'VIDVC Style'
      OnExecute = NewWindowExecute
    end
  end
end
