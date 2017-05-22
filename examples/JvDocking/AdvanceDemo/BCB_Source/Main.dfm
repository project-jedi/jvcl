object MainForm: TMainForm
  Left = 287
  Top = 215
  Width = 311
  Height = 300
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 25
    Width = 303
    Height = 211
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = '����'
    Font.Style = []
    Lines.Strings = (
      
        '��ӭ���ʹ��DockManager�ؼ������Demo����������ʾ�ؼ���ʹ�÷�' +
        '����'
      
        '�����ڳ���Ĺ������������ĸ���ť��'#39'Delphi���'#39','#39'VC++���'#39','#39'VID��' +
        '��'#39'��'#39'VS.NET���'#39'��'
      #39'Delphi���'#39'��������Delphiͣ�����Ĵ��塣'
      #39'VC++���'#39'��������Visual C++ͣ�����Ĵ��塣'
      #39'VID���'#39'��������Visual InterDevͣ�����Ĵ��塣'
      #39'VS.NET���'#39'��������Visual Studio.NETͣ�����Ĵ��塣'
      
        '����û�ϣ������Delphi��ͣ�������������ڵ�ʱ��������������' +
        #39'lbDockServer1'#39'�ؼ���DockStyle�������ó�JvDockDelphiStyle1;Ȼ��' +
        '�������ڵ��'#39'Delphi���'#39'��ť����ͣ������,�����ͣ�������϶�����' +
        '���帽���Ϳ���ʵ��Delphi��ͣ�����'
      
        '����û�ϣ������Visual C++��ͣ�������������ڵ�ʱ�����������' +
        '���'#39'lbDockServer1'#39'�ؼ���DockStyle�������ó�JvDockVCStyle1;Ȼ��' +
        '�������ڵ��'#39'VC++���'#39'��ť����ͣ����,�����ͣ�������϶���������' +
        '�����Ϳ���ʵ��Visual C++��ͣ�����'
      
        '����û�ϣ������Visual InterDev��ͣ�������������ڵ�ʱ�����' +
        '���������'#39'lbDockServer1'#39'�ؼ���DockStyle�������ó�JvDockVIDStyle' +
        '1;Ȼ���������ڵ��'#39'VID���'#39'��ť����ͣ������,�����ͣ�������϶���' +
        '�����帽���Ϳ���ʵ��Visual InterDev��ͣ�����'
      
        '����û�ϣ������Visual Studio.NET��ͣ�������������ڵ�ʱ���' +
        '�����������'#39'lbDockServer1'#39'�ؼ���DockStyle�������ó�lbVSNETDockS' +
        'tyle1;Ȼ���������ڵ��'#39'VSNET���'#39'��ť����ͣ������,�����ͣ������' +
        '�϶��������帽���Ϳ���ʵ��Visual Studio.NET��ͣ�����')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 303
    Height = 25
    Anchors = []
    AutoSize = True
    ButtonHeight = 21
    ButtonWidth = 72
    Caption = 'ToolBar1'
    Color = clBtnFace
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    Flat = True
    ParentColor = False
    ShowCaptions = True
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      AllowAllUp = True
      AutoSize = True
      Caption = 'Delphi Style'
      ImageIndex = 0
      OnClick = DelphiStyleClick
    end
    object ToolButton2: TToolButton
      Left = 67
      Top = 0
      AllowAllUp = True
      AutoSize = True
      Caption = 'VC++ Style'
      ImageIndex = 1
      OnClick = VCStyleClick
    end
    object ToolButton3: TToolButton
      Left = 130
      Top = 0
      AllowAllUp = True
      AutoSize = True
      Caption = 'VID Style'
      ImageIndex = 2
      OnClick = VIDStyleClick
    end
    object ToolButton4: TToolButton
      Left = 185
      Top = 0
      Caption = 'VS.NET Style'
      ImageIndex = 3
      OnClick = VSNETStyleClick
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
    DockStyle = JvDockVSNetStyle1
    Left = 32
    Top = 48
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Left = 112
    Top = 48
    object DockForm_Menu: TMenuItem
      Caption = 'Dockable Window'
      object DelphiStyle: TMenuItem
        Caption = 'Delphi Style'
        OnClick = DelphiStyleClick
      end
      object VCStyle: TMenuItem
        Caption = 'Visual C++ Style'
        OnClick = VCStyleClick
      end
      object VIDStyle: TMenuItem
        Caption = 'Visual InterDev Style'
        OnClick = VIDStyleClick
      end
      object VSNETStyle: TMenuItem
        Caption = 'Visual Studio.net Style'
        OnClick = VSNETStyleClick
      end
    end
    object ShowWindow_Menu: TMenuItem
      Caption = 'Show Widnow'
    end
    object DockInfo_Menu: TMenuItem
      Caption = 'Dock Information'
      object SaveToFile: TMenuItem
        Caption = 'Save To Ini File'
        OnClick = SaveToFileClick
      end
      object LoadFromFile: TMenuItem
        Caption = 'Load From Ini File'
        OnClick = LoadFromFileClick
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
  end
  object JvDockVIDStyle1: TJvDockVIDStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = [fsBold]
    ConjoinServerOption.InactiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.InactiveFont.Color = 13160660
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = [fsBold]
    ConjoinServerOption.TextAlignment = taLeftJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = 15780518
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
    TabServerOption.ActiveFont.Name = 'MS Sans Serif'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = clWhite
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Sans Serif'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = False
    Left = 192
    Top = 112
  end
  object JvDockVCStyle1: TJvDockVCStyle
    ConjoinServerOption.GrabbersSize = 15
    ConjoinServerOption.SplitterWidth = 4
    Left = 112
    Top = 112
  end
  object JvDockDelphiStyle1: TJvDockDelphiStyle
    ConjoinServerOption.GrabbersSize = 12
    ConjoinServerOption.SplitterWidth = 4
    Left = 32
    Top = 112
  end
  object JvDockVSNetStyle1: TJvDockVSNetStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = []
    ConjoinServerOption.InactiveFont.Charset = GB2312_CHARSET
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
    TabServerOption.InactiveSheetColor = 15725559
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Sans Serif'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = 5395794
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Sans Serif'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = True
    ChannelOption.ActivePaneSize = 100
    ChannelOption.ShowImage = True
    ChannelOption.MouseleaveHide = True
    ChannelOption.HideHoldTime = 1000
    Left = 32
    Top = 176
  end
  object PopupMenu2: TPopupMenu
    OnPopup = PopupMenu2Popup
    Left = 192
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
end
