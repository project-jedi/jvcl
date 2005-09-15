object Mainform: TMainform
  Left = 113
  Top = 0
  Width = 908
  Height = 804
  Caption = 
    'JVCL3 - MegaDemo                                                ' +
    '                                                                ' +
    '                                                                ' +
    '                                      '
  Color = clBtnFace
  Constraints.MinHeight = 492
  Constraints.MinWidth = 652
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  ShowHint = True
  WindowState = wsMaximized
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 225
    Top = 181
    Height = 577
    Cursor = crHSplit
    Hint = 'click me to have more '#13#10'space for the current demo'
    Align = alLeft
    MinSize = 10
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
  end
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 239
    Top = 187
    Width = 513
    Height = 401
    Buttons = []
    CaptionPosition = dpTop
    CaptionFont.Charset = ANSI_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'MS Sans Serif'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 0
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 1
    object JvNavPanelButton2: TJvNavPanelButton
      Tag = 65
      Left = 0
      Top = 0
      Width = 112
      Height = 40
      Caption = 'Info'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = [fsBold]
      ParentFont = False
      StyleManager = JvNavPaneStyleMan
      ParentStyleManager = False
      ImageIndex = 3
      Images = LargeImages
      OnClick = CompClick
    end
    object JvNavPanelButton3: TJvNavPanelButton
      Tag = 8
      Left = 88
      Top = 0
      Width = 112
      Height = 40
      Hint = 'shows a special Seach form'#13#10'where one can search for'#13#10'Demos'
      Caption = 'Search'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = [fsBold]
      ParentFont = False
      StyleManager = JvNavPaneStyleMan
      ParentStyleManager = False
      ImageIndex = 12
      Images = LargeImages
      OnClick = CompClick
    end
    object JvNavPanelBtnIdePageCtrl: TJvNavPanelButton
      Tag = 8
      Left = 200
      Top = 0
      Width = 112
      Height = 40
      Caption = 'IDE View'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = [fsBold]
      ParentFont = False
      StyleManager = JvNavPaneStyleMan
      ParentStyleManager = False
      ImageIndex = 2
      Images = LargeImages
      OnClick = JvNavPanelBtnIdePageCtrlClick
    end
    object JvNavPanelBtnLoadFormInIDE: TJvNavPanelButton
      Left = 312
      Top = 0
      Width = 112
      Height = 40
      Hint = 
        'Loads the MainForm of the'#13#10'current Demo in the Delphi IDE.'#13#10'This' +
        ' feature uses JclDebug from the'#13#10'JEDI Code Library. To get it wo' +
        'rk'#13#10'the executable has to be compiled'#13#10'with a Map file.'
      Caption = 'Load in IDE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = [fsBold]
      ParentFont = False
      StyleManager = JvNavPaneStyleMan
      ParentStyleManager = False
      ImageIndex = 20
      Images = LargeImages
      OnClick = btnLoadIdeClick
    end
    object JvNavPanelBtnJumpHelp: TJvNavPanelButton
      Left = 424
      Top = 0
      Width = 112
      Height = 40
      Hint = 'Jump to the Help Topic of the'#13#10'currently seen JVCL Component'
      Caption = 'Jump to Help'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = [fsBold]
      ParentFont = False
      StyleManager = JvNavPaneStyleMan
      ParentStyleManager = False
      ImageIndex = 16
      Images = LargeImages
      OnClick = JvNavPanelBtnJumpHelpClick
    end
    object JvNavPanelBtnExit: TJvNavPanelButton
      Left = 536
      Top = 0
      Width = 112
      Height = 40
      Hint = 'Leave the Demo. Don'#180't know '#13#10'why you want this, but anyway'
      Caption = 'Exit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = [fsBold]
      ParentFont = False
      StyleManager = JvNavPaneStyleMan
      ParentStyleManager = False
      ImageIndex = 1
      Images = LargeImages
      OnClick = JvNavPanelBtnExitClick
    end
    object JvNavPanelButton1: TJvNavPanelButton
      Left = 648
      Top = 0
      Width = 112
      Height = 40
      Hint = 'Leave the Demo. Don'#180't know '#13#10'why you want this, but anyway'
      Caption = 'Exception!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = [fsBold]
      ParentFont = False
      StyleManager = JvNavPaneStyleMan
      ParentStyleManager = False
      ImageIndex = -1
      Images = LargeImages
      OnClick = JvNavPanelButton1Click
    end
  end
  object aJvImgBtn: TJvImgBtn
    Left = 512
    Top = 128
    Width = 26
    Height = 25
    TabOrder = 2
    Visible = False
    Flat = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Images = LargeImages
    ImageIndex = 2
    Layout = blImageRight
    Spacing = 0
  end
  object sbxWinXPBar: TScrollBox
    Left = 0
    Top = 181
    Width = 225
    Height = 577
    HorzScrollBar.Smooth = True
    HorzScrollBar.Style = ssFlat
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Style = ssFlat
    VertScrollBar.Tracking = True
    Align = alLeft
    BorderStyle = bsNone
    TabOrder = 3
    object JvSplitter1: TJvSplitter
      Left = 0
      Top = 139
      Width = 225
      Height = 6
      Cursor = crVSplit
      Align = alTop
      Beveled = True
    end
    object JvXPContainer1: TJvXPContainer
      Left = 0
      Top = 145
      Width = 225
      Height = 432
      AutoSize = True
      BorderWidth = 4
      Caption = 'JvXPContainer1'
      Align = alClient
      object JvXPBarBrowseDemos: TJvXPBar
        Left = 4
        Top = 109
        Width = 217
        Height = 35
        Caption = 'Browse All Demo Stuff'
        Collapsed = True
        Colors.BorderColor = clBlack
        Colors.BodyColor = clWindow
        Colors.GradientFrom = 16048865
        Colors.GradientTo = 11572372
        Colors.SeparatorColor = clWhite
        Items = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 17877
        Font.Height = -13
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        HeaderFont.Charset = ANSI_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -13
        HeaderFont.Name = 'MS Sans Serif'
        HeaderFont.Style = [fsBold]
        HotTrackColor = clBlack
        OwnerDraw = False
        RollStep = 5
        ShowItemFrame = False
        RoundedItemFrame = 0
        AfterCollapsedChange = JvXPBarBrowseDemosAfterCollapsedChange
        BeforeCollapsedChange = WinXPBarEnsureOnlyOneExpanded
        Align = alTop
        ParentFont = False
        object Panel4: TPanel
          Left = 4
          Top = 36
          Width = 209
          Height = 45
          Align = alClient
          BevelOuter = bvNone
          Color = clInfoBk
          TabOrder = 0
          object JvLabel1: TJvLabel
            Left = 8
            Top = 8
            Width = 161
            Height = 13
            Caption = 'Here are all availible demos listed:'
            AutoOpenURL = False
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Sans Serif'
            HotTrackFont.Style = []
          end
          object JvListBoxAllDemos: TJvListBox
            Left = 4
            Top = 24
            Width = 199
            Height = 19
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clInfoBk
            ItemHeight = 13
            Background.FillMode = bfmTile
            Background.Visible = False
            ScrollBars = ssVertical
            Sorted = True
            TabOrder = 0
            OnClick = CompClick
          end
        end
      end
      object jvXPBarSettings: TJvXPBar
        Left = 4
        Top = 39
        Width = 217
        Height = 35
        Hint = 'This is a hint'
        Caption = 'Settings'
        Collapsed = True
        Colors.BorderColor = clGray
        Colors.BodyColor = clInfoBk
        Colors.GradientFrom = 16048865
        Colors.GradientTo = 11572372
        Colors.SeparatorColor = clSilver
        Items = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        HeaderFont.Charset = ANSI_CHARSET
        HeaderFont.Color = clBlack
        HeaderFont.Height = -13
        HeaderFont.Name = 'MS Sans Serif'
        HeaderFont.Style = [fsBold]
        HotTrackColor = clBlack
        OwnerDraw = False
        Icon.Data = {
          0000010001002020000001000800A80800001600000028000000200000004000
          0000010008000000000080040000000000000000000000000000000000000000
          000000549900005497000054950000539500004F9000004E8E0000579B000055
          9A002491DC0063C4FF0046B5F80000509100026CB7000A91E5000473C100004D
          8B00004E8A00147AC4004FB5F40075CBFF004ABBFF003CB5FF0045AEEF000052
          92000E97EC000F9FF7000C98F0000472BF00004B880049AFF10064C5FF0055C0
          FF0040B8FF0036B3FF003DA3E5000E90E10012A2FA0010A2FA000D9FF600004A
          87002192E20047BAFF003FB7FF0038B5FF002DB0FF0028AFFF000E7BC5001FAA
          FF001AA9FE000C9DF6000B8CDF00004479003BB5FF0036B4FF0031B2FF0027B0
          FF0022ADFF0019A9FE0015A8FE000BA1FB000980CE00003D6E00004780000055
          9C0000569900004E8D00004881002B95E30049BBFF0044B9FF003AB7FF0035B4
          FF0030B1FF002BB0FF001EACFF0011A7FF0000488200026BB6000780D2000046
          7F0000589C003CADF6006FC7FE003FAAF2001D87D9000064B90043B1F6004DBD
          FF0043B8FF0039ABF100309BDE001CAAFE0017A8FE0014A7FE000F97EB000791
          E700099EF7000A8CE10000457C00BEE6FF0099D9FF0082D0FF0056BFFF0051BE
          FF00318BC90000508F001B75B10010A5FF000EA1FB0000447A003690D1005EC3
          FF005BC1FF002673AB000067B300007CD6000079D1000075C900005FA5001F7C
          BA001BABFE0013A7FE000581D70000437800ACE0FF0068C5FF000077CD00007F
          DA000080DD00007DD9000072CB000065B5000DA2FB00098FE7000042770070C7
          FE006AC7FF0067C5FF0061C4FF0000599F000073C8000078D40016A7FE000074
          CC001D9DF40048B6FB007BCDFF006FC8FF00004A84000065B300006DC5000068
          BE000057A0000066B60000417600006AC20079CDFF000058A200005FB100005B
          A6000C90E4000065B60000427500006EC400005DA8000A8FEC00297BB6000041
          730015A6FC000896EE000882D8000062B0008FD5FF006DC8FF004AA1DA00003E
          6F0025AEFF0021ACFF000C6DB2009AD8FF0085D1FF0071CAFF003181B9000056
          970029B0FF00003F72002283C30081D0FF00DFF3FF0032B1FF001AAAFD00FFFF
          FF0051A7DE000D65A70017A7FC00A1DCFF004EBDFF0054BFFF001D8EE1002194
          E600249AEA002BA6F400A2DBFF00B0E1FF0049B9FD001D90E1007DCFFF005CC1
          FF0058C0FF0053BFFF0050BDFF000061B100037CD700003F70000068B600199B
          F2002FA5F2009BD9FF0060C3FF0056C1FF0043ADF400036DBF000061B200ACDF
          FF0068C7FF0066C6FF0089D3FF002394E3000B7BD00077CDFF0073CAFF002DA4
          F20070C9FF003DB6FF002999E4000067B400BCE6FF0097D8FF000B82DA000E90
          EC00B9E5FF005BBDFA001484D9000061B400B7E5FF000389E8000774CC00027F
          DB000073CD000063AC000D609B000070C1000078CF0000407100000000000000
          00000000000000000000006E347C870000000000000000000000000000000000
          000000000000000050636377FCFDFD8700000000BAFED6000000000000000000
          0000000000000043FC8181818181817C000000A88183A0D6B000000000000000
          000000000000004DFA818181818181FB8700A374819C565604B0000000000000
          000000000000004DB8F8818181A691787C87F9818198565656A0B00000000000
          0000001D000000004D8EA6CDB5EDF1F5E3568181F6D9C7F75656D60000000000
          000010EC1D00000095E5B564ED65EEE314EF81F020F1C4F2F3F4D60000000000
          00428181831D1D004DE564DAB4ADE614E7C6E8E9E3E3E3E3EAEBD60000000000
          6A8181818183564D18CEE0DA669DB61FE1E1E1E1E1E1E2E3E4DED6D60000000C
          D781818181D8CE56B8D9DA669D891FDBD0DCC668DCC61FDDDE049EDFD6000018
          8181818191CBCCCDCECF66AE891F8BD0D1D2C5C5C6C6D3C799D497D5D6000018
          81818191C4BDB4B4669394898A5A5B5B5B5B5AC5C6C636C7C8C9CACA6DBA0000
          188191B5C064B4AD939DB6C1725063A37C3E3EC21636BE49BEBEBE39C3BA0000
          0018BBBCBDB4AD939DB6B74D4300000000003E3E78BEBEBEBEBFA9A96DBA0000
          000018B364B4B59DB6B7B8000000000000003E063E5AB9B2B2A9A96D61BA0000
          03049CA591AD9DAEAF070000000000000000B0993E5BB1B25CA96D6161A80102
          A48E9CA5A69D9489A70700000000000000A899A07C5B395CA96DAAABACA34181
          818E9C91939D891F8C00000000000063349E9FA07C5B4B3AA1A22828A3000881
          81919293947E8A8B8C0642111D95435196979899435A3A3B851B9A069B000008
          88656666898A8B708C738D828081818E8383844378318F4C6D6D1B9087000007
          657D66667E0A7071696A737F8081818283846A78795D7A856D6D618687000007
          6F6465660A70712068726A73747576776A6A78795D7A7A6D6D61617B7C000000
          07646566660A6768582A696A6A0642116B5B5C5D5E6C6D6D6161626E00000000
          515253545556575845592B5A5B5B5B5A395C5D5E5F4E60616162630000000000
          0040074142434445462B4748494A38394B3A3B4C1C3E4D4E4F50000000000000
          0000000007082020213536372D3839303A3B263C3D3E003F3F00000000000000
          00000000072920202A2B2C2D2E2F303126262632333400000000000000000000
          00000000071E1F1F202122222305242526271A1A0E2800000000000000000000
          00000000071213140A151617180005191A1A1B1C1D1D00000000000000000000
          0000000000070801090A0B0400000C0D0E0F1011000000000000000000000000
          0000000000000000010203040000000506060000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000FFF8
          7FFFFFC03C7FFF80381FFF80100FFF800007FBC00007F1C00007E0400007C000
          0003800000018000000180000000C0000000E001F000F007F000C00FF000000F
          E000001F800100000001800000018000000180000001C0000003C0000007E000
          000FFC00009FFC0000FFFC0000FFFC0100FFFE0303FFFFC38FFFFFFFFFFF}
        ImageList = LargeImages
        RollStep = 5
        ShowItemFrame = False
        RoundedItemFrame = 0
        BeforeCollapsedChange = WinXPBarEnsureOnlyOneExpanded
        Align = alTop
        ParentFont = False
        object Panel1: TPanel
          Left = 4
          Top = 36
          Width = 209
          Height = 20
          Align = alClient
          BevelOuter = bvNone
          Color = clInfoBk
          TabOrder = 0
          object JvLabel3: TJvLabel
            Left = 2
            Top = 30
            Width = 189
            Height = 13
            Cursor = crHandPoint
            Hint = 'JvMegaDemoConfig.ini'
            Caption = 'open config file JvMegaDemoConfig.ini '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = ShellExecHint
            HotTrack = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clBlue
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Sans Serif'
            HotTrackFont.Style = [fsUnderline]
          end
          object JvLabel4: TJvLabel
            Left = 2
            Top = 48
            Width = 226
            Height = 13
            Cursor = crHandPoint
            Hint = 'JvMegaDemoAllDemoForms.ini '
            Caption = 'open config file JvMegaDemoAllDemoForms.ini '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = ShellExecHint
            HotTrack = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clBlue
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Sans Serif'
            HotTrackFont.Style = [fsUnderline]
          end
          object JvLabel5: TJvLabel
            Left = 2
            Top = 68
            Width = 212
            Height = 13
            Cursor = crHandPoint
            Hint = 'JvMegaDemoAllDemoForms.ini '
            Caption = 'open config file JvMegaDemoCompsTabs.ini'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = ShellExecHint
            HotTrack = True
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clBlue
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Sans Serif'
            HotTrackFont.Style = [fsUnderline]
          end
          object JvCheckBoxAllowOnlyOneExpanded: TJvCheckBox
            Left = 3
            Top = 10
            Width = 168
            Height = 17
            Caption = 'allow only one expanded group'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            State = cbChecked
            TabOrder = 0
            LinkedControls = <>
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Sans Serif'
            HotTrackFont.Style = []
          end
        end
      end
      object JvXPBarInformation: TJvXPBar
        Left = 4
        Top = 4
        Width = 217
        Height = 35
        Hint = 'This is a hint'
        Caption = 'Information'
        Collapsed = True
        Colors.BorderColor = clGray
        Colors.BodyColor = clInfoBk
        Colors.GradientFrom = 16048865
        Colors.GradientTo = 11572372
        Colors.SeparatorColor = clWindowText
        Items = <
          item
            Caption = 'visit JEDI website'
            Hint = 'http://www.delphi-jedi.org/'
            ImageIndex = 4
            OnClick = ShellExecHint
          end
          item
            Caption = 'visit JCL website'
            Hint = 'http://homepages.borland.com/jedi/jcl/'
            ImageIndex = 4
            OnClick = ShellExecHint
          end
          item
            Caption = 'visit JVCL website'
            Hint = 'http://homepages.borland.com/jedi/jvcl/'
            ImageIndex = 4
            OnClick = ShellExecHint
          end
          item
            Caption = 'drop author of MegaDemo a line'
            Hint = 'mailto:RalfGSpam@gmx.de?subject=JVCL MegaDemo'
            ImageIndex = 0
            OnClick = ShellExecHint
          end
          item
            Caption = 'view Standard JVCL About Dialog'
            ImageIndex = 3
            OnClick = ShowJVCLAboutDlg
          end
          item
            Caption = 'Login to report a bug (preferred)'
            Hint = 'http://homepages.borland.com/jedi/issuetracker'
            OnClick = ShellExecHint
          end
          item
            Caption = 'Report a bug anonymously (anonymous user)'
            Hint = 'http://homepages.borland.com/jedi/issuetracker/login_anon.php'
            OnClick = ShellExecHint
          end>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = []
        HeaderFont.Charset = ANSI_CHARSET
        HeaderFont.Color = clBlack
        HeaderFont.Height = -13
        HeaderFont.Name = 'MS Sans Serif'
        HeaderFont.Style = [fsBold]
        HotTrackColor = clBlack
        OwnerDraw = False
        Icon.Data = {
          0000010001002020040000000000E80200001600000028000000200000004000
          0000010004000000000000020000000000000000000000000000000000000000
          0000000080000080000000808000800000008000800080800000C0C0C0008080
          80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
          0000000000000008800000000000000000000000000000888000000000000000
          000000000000000880000000000000000000000000000F088000000000000000
          000000000000FF08800000000000000000000000080FFF088000000000000000
          00000088880FFF08888800000000000000008880007FFF088888880000000000
          00080007FFFFFF70008888800000000000007FFFFFFFFFFFF700888800000000
          007FFFFFFFFFFFFFFFF70888800000000FFFFFFFFFFFFFFFFFFFF08888000000
          FFFFFFFFFFFFFFFFFFFFFF088880008FFFFFFFFCCCCCCCCCFFFFFFF08880087F
          FFFFFFFFFCCCCCFFFFFFFFF7088808FFFFFFFFFFFCCCCCFFFFFFFFFF088887FF
          FFFFFFFFFCCCCCFFFFFFFFFF70888FFFFFFFFFFFFCCCCCFFFFFFFFFFF0888FFF
          FFFFFFFFFCCCCCFFFFFFFFFFF0888FFFFFFFFFFFFCCCCCFFFFFFFFFFF0888FFF
          FFFFFFFFFCCCCCFFFFFFFFFFF0888FFFFFFFFFFCCCCCCCFFFFFFFFFFF08087FF
          FFFFFFFFFFFFFFFFFFFFFFFF708008FFFFFFFFFFFFFFFFFFFFFFFFFF0800087F
          FFFFFFFF7CCCC7FFFFFFFFF70000008FFFFFFFFFCCCCCCFFFFFFFFF000000008
          FFFFFFFFCCCCCCFFFFFFFF00000000008FFFFFFF7CCCC7FFFFFFF00000000000
          087FFFFFFFFFFFFFFFF780000000000000887FFFFFFFFFFFF788000000000000
          00008887FFFFFF7888000000000000000000000888888880000000000000FFFF
          E7FFFFFFC7FFFFFF87FFFFFF07FFFFFE07FFFFF807FFFFC000FFFF00003FFE00
          001FFC00000FF8000007F0000003E0000001C000000180000000800000000000
          0000000000000000000000000000000000000000000100000001800000038000
          0007C000000FE000001FF000003FF800007FFC0000FFFF0003FFFFE01FFF}
        ImageList = LargeImages
        RollStep = 5
        ShowItemFrame = False
        RoundedItemFrame = 0
        AfterCollapsedChange = JvXPBarBrowseDemosAfterCollapsedChange
        BeforeCollapsedChange = WinXPBarEnsureOnlyOneExpanded
        Align = alTop
        ParentFont = False
      end
      object JvXPBarSearchByCompName: TJvXPBar
        Left = 4
        Top = 74
        Width = 217
        Height = 35
        Caption = 'Search by Component'
        Collapsed = True
        Colors.BorderColor = clBlack
        Colors.BodyColor = clWindow
        Colors.GradientFrom = 16048865
        Colors.GradientTo = 11572372
        Colors.SeparatorColor = clWhite
        Items = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWhite
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        HeaderFont.Charset = ANSI_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -13
        HeaderFont.Name = 'MS Sans Serif'
        HeaderFont.Style = [fsBold]
        HotTrackColor = clBlack
        OwnerDraw = False
        RollStep = 5
        ShowItemFrame = True
        RoundedItemFrame = 0
        AfterCollapsedChange = JvXPBarSearchByCompNameAfterCollapsedChange
        BeforeCollapsedChange = WinXPBarEnsureOnlyOneExpanded
        Align = alTop
        Anchors = [akLeft, akTop, akRight, akBottom]
        ParentFont = False
        object Panel2: TPanel
          Left = 4
          Top = 36
          Width = 209
          Height = 153
          Align = alClient
          BevelOuter = bvNone
          Color = clInfoBk
          TabOrder = 0
          object JvLabel2: TJvLabel
            Left = 8
            Top = 8
            Width = 86
            Height = 13
            Caption = 'type first letters in:'
            AutoOpenURL = False
            HotTrackFont.Charset = DEFAULT_CHARSET
            HotTrackFont.Color = clWindowText
            HotTrackFont.Height = -11
            HotTrackFont.Name = 'MS Sans Serif'
            HotTrackFont.Style = []
          end
          object JvListBoxDemosCompNameSorted: TJvListBox
            Left = 7
            Top = 48
            Width = 188
            Height = 124
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clInfoBk
            ItemHeight = 13
            Background.FillMode = bfmTile
            Background.Visible = False
            ScrollBars = ssVertical
            Sorted = True
            TabOrder = 0
            OnClick = CompClick
          end
          object JvEdtCompSearch: TJvEdit
            Left = 8
            Top = 24
            Width = 177
            Height = 19
            Flat = True
            ParentCtl3D = False
            Modified = False
            Color = clInfoBk
            TabOrder = 1
          end
        end
      end
    end
    object JvRichEditHints: TJvRichEdit
      Left = 0
      Top = 0
      Width = 225
      Height = 139
      Align = alTop
      Color = clInfoBk
      TabOrder = 1
      OnURLClick = JvRichEditHintsURLClick
    end
  end
  object JvXPBarIDE: TJvXPBar
    Left = 0
    Top = 48
    Width = 900
    Height = 133
    Caption = 'Components as in the Delphi IDE'
    Colors.BorderColor = 9845
    Colors.BodyColor = clWindow
    Colors.GradientTo = clGray
    Colors.SeparatorColor = 4227327
    Items = <
      item
      end
      item
      end
      item
      end
      item
      end
      item
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 17877
    Font.Height = -13
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -13
    HeaderFont.Name = 'MS Shell Dlg 2'
    HeaderFont.Style = [fsBold]
    HeaderHeight = 18
    HotTrackColor = clBlack
    OwnerDraw = False
    Icon.Data = {
      0000010001001020000001000800680500001600000028000000100000002000
      0000010008000000000040010000000000000000000000000000000000000000
      0000000080000080000000808000800000008000800080800000C0C0C000C0DC
      C000F0CAA6000020400000206000002080000020A0000020C0000020E0000040
      0000004020000040400000406000004080000040A0000040C0000040E0000060
      0000006020000060400000606000006080000060A0000060C0000060E0000080
      0000008020000080400000806000008080000080A0000080C0000080E00000A0
      000000A0200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0
      000000C0200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0
      000000E0200000E0400000E0600000E0800000E0A00000E0C00000E0E0004000
      0000400020004000400040006000400080004000A0004000C0004000E0004020
      0000402020004020400040206000402080004020A0004020C0004020E0004040
      0000404020004040400040406000404080004040A0004040C0004040E0004060
      0000406020004060400040606000406080004060A0004060C0004060E0004080
      0000408020004080400040806000408080004080A0004080C0004080E00040A0
      000040A0200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0
      000040C0200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0
      000040E0200040E0400040E0600040E0800040E0A00040E0C00040E0E0008000
      0000800020008000400080006000800080008000A0008000C0008000E0008020
      0000802020008020400080206000802080008020A0008020C0008020E0008040
      0000804020008040400080406000804080008040A0008040C0008040E0008060
      0000806020008060400080606000806080008060A0008060C0008060E0008080
      0000808020008080400080806000808080008080A0008080C0008080E00080A0
      000080A0200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0
      000080C0200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0
      000080E0200080E0400080E0600080E0800080E0A00080E0C00080E0E000C000
      0000C0002000C0004000C0006000C0008000C000A000C000C000C000E000C020
      0000C0202000C0204000C0206000C0208000C020A000C020C000C020E000C040
      0000C0402000C0404000C0406000C0408000C040A000C040C000C040E000C060
      0000C0602000C0604000C0606000C0608000C060A000C060C000C060E000C080
      0000C0802000C0804000C0806000C0808000C080A000C080C000C080E000C0A0
      0000C0A02000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C0
      0000C0C02000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A0008080
      80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
      771C1C1C1C1C1C1C000000000000000077BFB7665D661D1C0000000000000000
      0077B76F66661C0000000000000000000077BF772F1C1C008989890000000000
      000077771C5DA4890606EB89A4000000000077772F5D8906EB6006EB89000000
      000000BF771C60EB0661EB06EB89000000000077BF2F1CF3F36060EB06890000
      00000077BF775C61606060600689000000000000BF771CAE6060606089070000
      0000000077772F1CF3F3F389A40000000000000000BF775D6060610700000000
      0000000000BF77661C000000000000006F665D5D5D662F665D00000000000000
      BF6F6F6F6F7777776600000000000000BFBFBFBFBFBFBFBF770000000000C03F
      0000C03F0000E07F0000E0470000F0010000F0010000F8000000F8000000F800
      0000FC000000FC010000FE030000FE1F0000C01F0000C01F0000C01F0000}
    RollStep = 5
    ShowItemFrame = False
    RoundedItemFrame = 0
    OnCollapsedChange = JvXPBarIDECollapsedChange
    Align = alTop
    ParentFont = False
    object JvPageControlComps: TJvPageControl
      Left = 7
      Top = 31
      Width = 886
      Height = 94
      Anchors = [akLeft, akTop, akRight, akBottom]
      MultiLine = True
      TabOrder = 0
    end
  end
  object aJvBitBtn: TJvBitBtn
    Left = 832
    Top = 384
    Width = 25
    Height = 25
    TabOrder = 5
    Visible = False
    OnClick = CompClick
    Glyph.Data = {
      76060000424D7606000000000000360400002800000018000000180000000100
      08000000000040020000120B0000120B00000001000000010000000000000000
      80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
      A6000020400000206000002080000020A0000020C0000020E000004000000040
      20000040400000406000004080000040A0000040C0000040E000006000000060
      20000060400000606000006080000060A0000060C0000060E000008000000080
      20000080400000806000008080000080A0000080C0000080E00000A0000000A0
      200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
      200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
      200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
      20004000400040006000400080004000A0004000C0004000E000402000004020
      20004020400040206000402080004020A0004020C0004020E000404000004040
      20004040400040406000404080004040A0004040C0004040E000406000004060
      20004060400040606000406080004060A0004060C0004060E000408000004080
      20004080400040806000408080004080A0004080C0004080E00040A0000040A0
      200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
      200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
      200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
      20008000400080006000800080008000A0008000C0008000E000802000008020
      20008020400080206000802080008020A0008020C0008020E000804000008040
      20008040400080406000804080008040A0008040C0008040E000806000008060
      20008060400080606000806080008060A0008060C0008060E000808000008080
      20008080400080806000808080008080A0008080C0008080E00080A0000080A0
      200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
      200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
      200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
      2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
      2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
      2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
      2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
      2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
      2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
      2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00C73E3E3E3E3E
      3E3E3E3E3E3E3E3D3D3DFEFCFCFCFCFCFCFC3E3E3E3E3E3E3E3E3E3E3E3E3E3E
      3D3DFEFCFCFBFBFCFCFC3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3DFEFCFBFCFCFB
      FCFC3F3E3E3E3E3E3E3E3E3E3E3E3E3E3E3EFEFCFBFCFCFBFCFC3F3F3E3E3E3E
      3E3E3E3E3E3E3E3E3E3EFEFCFCFCFCFBFCFC3F3F3F3E3E3E3E3E3E3E3E3E3E3E
      3E3EFEFCFCFCFCFBFCFC3F3F3F3F3E3E3E3E3E3E3E3E3E3E3E3EFEFCFCFCFCFB
      FCFC3F3F3F3F3F3E3E3E3E3E3E3E3E3E3E3EFEFCFCFCFCFCFCFCA4A4A4A4A4A4
      A4A4A4A4A4A4A4A4A4A4FEFEFEFEFEFEFEFEFF07070707070707070707070707
      070707070707070707A4FF07070707070707070707070707A4FF070707070707
      07A4FF07070707070707070707070707A4FF07070707070707A4FF0707070707
      0707070707070707A4FF07070707070707A4FF07070707070707070707070707
      A4FF07070700070707A4FF07070707070707070707070707A4FF070700000007
      07A4FF07070707070707070707070707A4FF07000000000007A4FF0707070707
      0707070707070707A4FF07070707070707A4FF07070707070707070707070707
      A4FF07070707070707A4FF07070707070707070707070707A4FF070707070707
      07A4FF07070707070707070707070707070707070707070707A4FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF373F3F3F3F3F3F3F3F3F3F3F3F3F
      3F3F3F3F3F3E3E3E3E3E37373F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3E3E
      3E3E3737373F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3E3E3E}
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object StatusBar: TJvStatusBar
    Left = 0
    Top = 758
    Width = 900
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object JvNavPaneStyleMan: TJvNavPaneStyleManager
    Theme = nptXPSilver
    Left = 832
    Top = 424
  end
  object LargeImages: TImageList
    Height = 20
    Width = 24
    Left = 828
    Top = 312
    Bitmap = {
      494C010115001800040018001400FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      00000000000036000000280000006000000078000000010020000000000000B4
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000003250000062960000629600006296000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000005050006E6E
      6E0048B8FF006BC6FF00626262006E6E6E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000003250005656
      5600808080008ED4FF006E6E6E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000626262006262
      6200000000000000000000000000000000000000000000000000323232003E3E
      3E006E6E6E008ED4FF006E6E6E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000929292003E3E
      3E006E6E6E006E6E6E0000000000808080000000000000000000323232003E3E
      3E006E6E6E008ED4FF006E6E6E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000262626000000000000000000323232003E3E
      3E006E6E6E008ED4FF006E6E6E00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000929292009292
      9200000000000000000000000000000000000000000000000000323232003E3E
      3E006E6E6E008ED4FF0080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003E3E3E000E0E
      0E00808080000000000000000000000000000000000000000000323232003E3E
      3E006E6E6E008ED4FF0080808000000000009E9E9E0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000626262006E6E6E0000000000323232000000000000000000323232003E3E
      3E006E6E6E008ED4FF008080800000000000000000003E3E3E00000000003232
      3200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000626262006262
      6200000000000000000000000000000000000000000000000000323232003E3E
      3E006E6E6E008ED4FF006E6E6E00000000000000000000000000000000006262
      6200808080003E3E3E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003E3E3E002626
      26009E9E9E000000000000000000000000000000000000000000323232003E3E
      3E006E6E6E008ED4FF006E6E6E00000000003E3E3E0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000092929200565656006E6E6E00000000000000000000000000003250003E3E
      3E006E6E6E008ED4FF006E6E6E0000000000000000003E3E3E00000000006262
      6200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009E9E9E00000000000000000092929200003250003E3E
      3E006E6E6E008ED4FF006E6E6E00000000008080800080808000000000000000
      0000808080003E3E3E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003E3E3E003232
      3200000000000000000000000000000000006E6E6E000E0E0E00001950000062
      96004873FF004873FF004873FF00003196000000000000000000000000000000
      0000000000003E3E3E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009292
      9200808080006E6E6E00000000003E3E3E000000000000000000000000000000
      000092929200000000000000000000000000000000003E3E3E00000000009292
      9200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009E9E9E000E0E0E003232320062626200000000006E6E6E000000
      0000000000000000000000000000000000000000000056565600000000000000
      0000929292006E6E6E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000929292000E0E0E000000
      0000000000000E0E0E0062626200000000000000000000000000000000000000
      0000000000006E6E6E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009292
      92003E3E3E0000000000000000009E9E9E003E3E3E0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003232320056565600000000000E0E
      0E009E9E9E009292920000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009E9E
      9E00929292003E3E3E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000A0E0004080C00000406000004060000040600000406000004060000040
      6000004060000040600000206000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080E0E00080E0E00080E0E00040C0E00040A0C0004080A0000060A0004080
      A0004080C0004080C00000408000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000040A0C00040A0C0004080C0004080A0000060A000006080000060
      A0000060A0000040600000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00EFE7DE00B5B58C008C94
      52009C844200C6946300D6A58C00F7EFEF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005A73520084634200844229004A6B39007B8463007B846300847352008463
      4200846342008452390073311800733118005A42290084735200844229000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000004080C0004080C0004080C0000060A0000060
      A0004060A0004040800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840084000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009CAD73009C5221007B8C420084AD5A0084AD
      5A00A5732900A5732900A5520000AD735A00FFFFF70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CEE7C600BDBD7B00D65A290084AD5A00ADE7A500BDD6AD00BDBD9C00BDA5
      7300B5946B00B5845200AD6B42008C42290084634200DEAD6300A54A29000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000040C0E00040C0E00040C0E0004080C0004080
      C00000408000000000008020200080202000C0A0600080800000C0A060008020
      2000802020000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084008400840084000000
      0000FFFFFF00C6C6C600FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000ADC68C009C8442006B6B290094CE8400ADD6
      84009C945200A57329009C5221007B420000C68C4A0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000ADE7A500CE7B52009C844200ADE7A500CEE7C600BDBD9C00B5AD
      8400B5946B00AD845A00AD6B42007B423100A5734A00E79452007B5239000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080E0E00080E0E00040C0E0004080C0004080
      C0000040800080202000C0A060008080000080800000C0A0600080800000C0A0
      6000808000008020200000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840084008400840084000000
      000084848400FFFFFF00C6C6C600FFFFFF00C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000EFE7DE009CBD7B00ADA55200ADD68400ADDE
      AD009CAD73009C8442007B4A18007B420000AD733900ADA55200DEAD6300F7EF
      EF00000000000000000000000000000000000000000000000000000000000000
      000000000000ADE7A500D6D6A500AD843900ADE7A500C6F7CE00BDD6AD00B5B5
      8C00AD9C7B00AD845A00946B420073422900E79452009C522100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000040C0E00040C0E00040C0E0000080C0004080
      C0008080A000C0A06000C0C06000C0A0600080800000C0A0600080800000C0A0
      600080800000C0A0600000000000000000000000000000000000000000000000
      000000000000000000000000000084008400840084008400840084008400C6C6
      C600000000000000000084848400C6C6C600FFFFFF00C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EFE7DE007BB56B00ADE7A500ADEF
      C6008CAD7B006B6B2900522910007B4A18008C9C7300ADA55200AD840800D6A5
      8C00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000BDF7B500D6D6A500ADD68400C6F7CE00CEE7C600ADBD
      9C00AD9C7B0094845A007B5239009C5A4200BD7B42004A312100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080E0E00080E0E00080E0E00000A0E0004080
      C0008080A000C0C06000C0A06000C0C06000C0C0600040800000408000008080
      0000C0A060008080000080202000000000000000000000000000000000000000
      0000000000000000000084008400840084008400840084008400C6C6C6008400
      84008400840084008400000000000000000084848400FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFF700D6EFD600A5CE9C00A5CE9C007BB56B0094D6
      B5006BA57B004A5A31005A634A0094A58400A5CE9C00B5B56B00AD840800A552
      0000CEAD5A000000000000000000000000000000000000000000000000000000
      00000000000000000000CEE7C600BDF7CE00ADD68400BDF7CE00B5D6CE00ADBD
      9C00A5A5840094845A0073523900946B42006352390000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000040C0E00040C0E00000C0E0004080
      C0004080C000C0C06000C0C06000C0C060004080000040800000408000008080
      0000C0A060008080000080202000000000000000000000000000000000000000
      00000000000084008400840084008400840084008400C6C6C600840084008400
      8400840084008400840084008400840084000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B5DEB500B5B56B00ADA55200A5EFC600ADDEAD009CAD73004A5A
      31004A31210084735200C6FFDE00CEFFE700B5DEB500ADBD7300AD840800A552
      0000CEAD5A000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E7E7E70094CE8400ADE7A500BDF7CE009CBD
      AD0094A58400847352005A422900525242000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080C0E00040C0E00000A0
      E0004080A0008080A000C0C06000C0C060004060400040800000C0C060004080
      0000406040004060400080202000000000000000000000000000000000000000
      000000000000840084008400840084008400C6C6C60084008400840084008400
      8400840084008400840084008400840084008400840084008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A5CE9C00BDE7C6008CB55A00C6FFDE00C6F7CE008C9C73005252
      1000944210009C391800CEDEC600E7FFFF00BDF7CE009CBD7B007B4200009C52
      2100F7F7F7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000094CE8400A5EFC60094D6
      B5007BA58400525A3900848C8400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080E0E00040C0E00000C0
      E0004080C0008080A000FFFFFF00C0C06000C0C0600040604000408000004080
      0000408000004060400080202000000000000000000000000000000000000000
      0000000000008400840084008400C6C6C6008400840084008400840084008400
      8400840084008400840084008400840084008400840084008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D6EFD600A5CE9C00BDE7C600C6FFDE00CEFFE70084AD5A00ADD6
      8400ADA55200A552000094845A00CEEFFF00A5EFDE006BA57B007B7B5200EFEF
      E700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000639463007394
      73005A735200848C840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000040C0E00040C0
      E00000A0E0004060A0008080A000C0C06000C0C0600040604000408000004080
      0000408020004060400000000000000000000000000000000000000000000000
      00000000000084008400C6C6C600840084008400840084008400840084008400
      8400840084008400840084008400840084008400840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000D6EFD600A5CE9C00CEFFE700E7FFFF009CBD7B0094DE
      9C008CAD7B0031291800BDB5AD005A735200735239009C9C8C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000052524200633921008442
      2900A54A29009C39180000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080E0E00080C0
      E00040C0E0004080C0008080A00040802000C0C06000C0C06000406040004080
      0000406040008020200000000000000000000000000000000000000000000000
      000000000000C6C6C60084008400840084008400840084008400840084008400
      8400840084008400840084008400840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D6EFD600A5CE9C00CEFFE700A5EFDE006BA5
      7B004A845A009C9C8C00B5CEA50073943100AD840800C6630800EFE7DE000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000848C6B006B6B29009C844200AD73
      3900CE6B3900D65A290094421000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080E0E00080E0
      E00040C0E0004080C0004060A0008080A0004060400040604000406040000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840084008400840084008400840084008400
      8400840084008400840084008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EFF7EF006B7B6B00734229009C9C
      8C000000000000000000ADDEAD00C6FFDE00BDD6AD00AD840800D6A58C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B8C4200ADBD7300BDA57300C68C
      4A00DE6B3900DE6B3900AD4A2100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000080E0
      E00080E0E00000C0E0004080C0004060A0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084008400840084008400
      8400840084008400840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5CEA50073943100AD840800C663
      0800EFE7DE0000000000BDE7C600CEFFE700B5EFD6006B6B2900DEBDBD000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000094B56300CEEFA500D6D6A500BDA5
      7300BD7B4200B55A3100944A2100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000040C0E00000C0E0004080C0004060A0000040600000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      8400840084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ADDEAD00C6FFDE00BDD6AD00AD84
      0800D6A58C000000000000000000B5D6CE0073CEB5009CBDAD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008CB55A00BDF7B500C6F7CE00ADBD
      9C0094845A008452390052291000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000A0E0004080C0004060A0004060A0004060A0004060A0004060A0004080
      C0004080C00000A0E00000A0E0004080C0004060A00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDE7C600CEFFE700B5EFD6006B6B
      2900DEBDBD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007BB56B0094DEC6007BB5
      A5006B7B6B0031291800736B6B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000040C0E00000A0E0000080C0000080C0000080C0000080C0000080E00000A0
      E00000A0E00040C0E00000C0E00000A0E0004060A00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5D6CE0073CEB5009CBD
      AD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007BA584005A8C
      73004A6B5A00A5A5A50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000040C0E00080E0E00080E0E00080E0E00080E0E00080E0E00080E0E00080E0
      E00080E0E00080E0E00080E0E00040C0E0004080C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000E0C00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0
      C00000E0C00000E0A00000E0A00000E0A000FFFF000080800000808000008080
      0000808000008080000080800000808000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000E0C00000E0C00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0
      C00000E0C00000E0C00000E0A00000E0A000FFFF0000808000008080000000FF
      FF0000FFFF008080000080800000808000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018181800181818001818
      1800181818001818180018181800000000000000000000000000000000000000
      00000000000000000000000000000000000000E0C00000E0C00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0A000FFFF00008080000000FFFF008080
      00008080000000FFFF0080800000808000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000181818001818180018181800181818001818
      1800181818001818180018181800181818001818180000000000000000000000
      00000000000000000000000000000000000000E0E00000E0E00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C000FFFF000080800000808000008080
      00008080000000FFFF0080800000808000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000181818001818180018181800CECECE00CECECE00CECE
      CE00CECECE00ADADAD00ADADAD00181818001818180018181800000000000000
      00000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C000FFFF000080800000808000008080
      00008080000000FFFF0080800000808000000000000000000000000000000000
      0000000000000000000000000000000000000000000084008400840084008400
      8400840084008400840084008400840084008400840084008400840084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000008400000084000084000000FF0000008400
      000084000000840000008400000000008400000000000000FF00000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000181818001818180018181800CECECE00CECECE00CECECE000000
      C6000000C600CECECE00CECECE00CECECE001818180018181800181818000000
      00000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00000E0C00000E0C00000E0C00000E0C00000E0C00080808000808080008080
      8000808080008080800000E0C00000E0C000FFFF000080800000808000008080
      00008080000000FFFF0080800000808000000000000000000000000000000000
      00000000000000000000000000000000000084008400FFFFFF00000000008484
      840084848400C6C6C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00840084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000848484008484840000FF00000084000000840000000000008484
      8400000000000000000084848400C6C6C6000000FF0000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001818180018181800D6D6D600CECECE00CECECE00CECECE000000
      C6000000C600CECECE00CECECE00CECECE00CECECE0018181800181818000000
      00000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00000E0E00000E0C00000E0C0008080800080808000FFFF0000FFFF00000080
      000080808000FFFF00008080800080808000FFFF000080800000808000008080
      0000808000008080000080800000808000000000000000000000000000000000
      00000000000000000000000000008400840084008400FFFFFF0000000000C6C6
      C600FFFFFF008484840084848400C6C6C600FFFFFF00FFFFFF00840084000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400848484000084000000FF00000084000084848400C6C6
      C600FFFF00008484840000000000848484008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000181818001818180021212100E7E7E700CECECE00CECECE00CECECE00CECE
      CE00CECECE00CECECE00CECECE00CECECE00CECECE00ADADAD00181818001818
      18000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00000E0E00000E0E00080808000FFFF0000FFFF0000FFFF0000008000008080
      80000080000080808000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000
      000000000000000000008400840084008400FFFFFF00FFFFFF0000000000C6C6
      C600FFFFFF00FFFFFF00FFFFFF0084848400C6C6C600FFFFFF00840084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400C6C6C6008484840000FF00000084000084848400C6C6C600FFFF
      0000C6C6C600FFFF000084848400000000000084000000840000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001818180018181800D6D6D600EFEFEF00CECECE00CECECE00CECECE000000
      C6000000C600C6C6CE00CECECE00CECECE00CECECE00CECECE00181818001818
      18000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00000E0E00080808000FFFF0000FFFF0000FFFF000080808000008000008080
      8000008000008080800000800000FFFF0000FFFF00008080800000E0C00000E0
      C00000E0C00000E0C00000E0A00000E0A0000000000000000000000000000000
      0000000000000000000084008400FFFFFF00FFFFFF00FFFFFF0000000000C6C6
      C600FFFFFF0000FFFF00FFFFFF00FFFFFF0084848400FFFFFF00840084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF00848484008484000084000000C6C6C600FFFFFF00C6C6
      C600FFFF0000C6C6C600FFFF0000000000000084000000840000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001818180018181800EFEFEF00EFEFEF00D6D6D600D6D6D600D6D6D6003939
      C6000000C6006B6BC600CECECE00CECECE00CECECE00CECECE00181818001818
      18000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00080808000FFFF0000FFFF0000FFFF0000FFFF000000800000808080000080
      0000808080000080000080808000FFFF0000FFFF0000FFFF00008080800000E0
      C00000E0C00000E0C00000E0C00000E0A0000000000000000000000000000000
      0000000000000000000084008400FFFFFF00FFFFFF00FFFFFF0000000000C6C6
      C600FFFFFF00FFFFFF00FFFFFF0000FFFF0084848400FFFFFF00840084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400008400008484000084840000FF000000C6C6C600FFFFFF00FFFF
      0000C6C6C600FFFF000084848400000000000084000000840000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001818180018181800EFEFEF00F7F7F700DEDEDE00D6D6D600D6D6D600CECE
      D6003939C6000000C6007373C600CECECE00CECECE00CECECE00181818001818
      18000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00080808000FFFF0000FFFF0000808080000080000080808000008000008080
      800000800000808080000080000080808000FFFF0000FFFF00008080800000E0
      C00000E0C00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000084008400FFFFFF00FFFFFF00FFFFFF0000000000C6C6
      C600FFFFFF0000FFFF00FFFFFF00FFFFFF0084848400FFFFFF00840084000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008484840084000000FF000000FF000000FF00000084848400C6C6C600C6C6
      C600FFFF0000C6C6C60084848400848484000084000000840000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001818180018181800F7F7F700FFFFFF00EFEFEF00DEDEDE00DEDEDE00D6D6
      D600CECED6000000C6000000C600CECECE00CECECE00CECECE00181818001818
      18000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00080808000FFFF0000FFFF0000008000008080800000800000808080000080
      000080808000008000008080800000800000FFFF0000008000008080800000E0
      C00000E0C00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000084008400FFFFFF00FFFFFF00FFFFFF0000000000C6C6
      C600FFFFFF00FFFFFF00FFFFFF0000FFFF0084848400FFFFFF00840084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400FF000000840000008484000084000000FF000000848484008484
      8400848484008484840084000000848484008484840000840000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000018181800181818004A4A4A00FFFFFF00EFEFEF002121C6000000C600A5A5
      D6009C9CCE000000C6003131C600D6D6D600CECECE00A5A5A500181818001818
      18000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00080808000FFFF0000FFFF0000808080000080000080808000008000008080
      80000080000080808000008000008080800000800000808080008080800000E0
      C00000E0C00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000084008400FFFFFF00FFFFFF00FFFFFF0000000000C6C6
      C600FFFFFF0000FFFF00FFFFFF00FFFFFF0084848400FFFFFF00840084000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084840000008400000084000084840000FF000000FF0000008400
      0000840000008400000084000000840000000084000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001818180039393900F7F7F700FFFFFF00CECEEF003939CE000000
      C6000000C6004242CE00C6C6D600D6D6D600D6D6D60018181800181818000000
      00000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00000E0E00080808000FFFF0000FFFF00000080000080808000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF000080808000008000008080800000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000084008400FFFFFF00FFFFFF00C6C6C600848484008484
      8400FFFFFF00FFFFFF00FFFFFF0000FFFF008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000848484000084000000FF0000008400008484000084000000FF00
      00000084000084000000FF000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000018181800313131004A4A4A00F7F7F700FFFFFF00FFFFFF00F7F7
      F700EFEFEF00EFEFEF00E7E7E700D6D6D6001818180018181800181818000000
      00000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00000E0E00080808000FFFF0000008000008080800000800000808080000080
      000080808000008000008080800000800000808080008080800000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000084008400FFFFFF00FFFFFF0084848400000000000000
      00008484840084848400FFFFFF00FFFFFF008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400C6C6C60000FF000000840000008400008484
      0000008400000084000084000000FF0000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001818180031313100393939004A4A4A00F7F7F700EFEF
      EF00EFEFEF00D6D6D60021212100181818001818180018181800000000000000
      00000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00000E0E00000E0E00080808000FFFF0000FFFF000080808000008000008080
      8000008000008080800000800000808080008080800000E0C00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000084008400FFFFFF00C6C6C60084848400000000000000
      0000000000000000000084848400848484008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C6C60084848400C6C6C600848484000084
      0000008400000084000000840000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000181818001818180018181800181818001818
      1800181818001818180018181800181818001818180000000000000000000000
      00000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00000E0E00000E0E00000E0E0008080800080808000FFFF0000808080000080
      00008080800000800000808080008080800000E0E00000E0C00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000084848400C6C6C6008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C6008484
      8400848484000084000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018181800181818001818
      1800181818001818180018181800000000000000000000000000000000000000
      00000000000000000000000000000000000000E0E00000E0E00000E0E00000E0
      E00000E0E00000E0E00000E0E00000E0E00000E0E00080808000808080008080
      8000808080008080800000E0E00000E0E00000E0E00000E0E00000E0C00000E0
      C00000E0C00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000C0E00000C0E00000E0E00000E0
      E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0
      E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0
      E00000E0C00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000C0E00000C0E00000C0E00000E0
      E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0
      E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0E00000E0
      E00000E0E00000E0C00000E0C00000E0C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7CEAD00EFBD9C00EFBD9C00EFBD
      9C00EFBD9C00EFBD9C00F7C6AD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7CEAD00EFBD9C00EFBD9C00EFBD
      9C00EFBD9C00EFBD9C00F7C6AD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00EFEFEF00D6D6D600BDBDBD00ADA5
      AD00B5B5B500C6C6C600DEDEDE00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00EFEFEF00D6D6D600BDBDBD00ADA5
      AD00B5B5B500C6C6C600DEDEDE00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000EFBD9C00F7DECE00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F7DECE00EFBD9C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000EFBD9C00F7DECE00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F7DECE00EFBD9C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F7F7F700DEDEDE00CECECE00C6C6C600C6C6C600C6C6
      C600BDBDBD00B5B5B500ADA5AD00C6C6C600EFEFEF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F7F7F700DEDEDE00CECECE00C6C6C600C6C6C600C6C6
      C600BDBDBD00B5B5B500ADA5AD00C6C6C600EFEFEF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00E7E7E700DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDE
      DE00DEDEDE00CECECE00C6C6C600B5B5B500BDBDBD00EFEFEF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00E7E7E700DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDE
      DE00DEDEDE00CECECE00C6C6C600B5B5B500BDBDBD00EFEFEF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00F7F7F700EFEFEF00EFEFEF00EFEFEF00EFEFEF00F7F7F700F7F7
      F700EFEFEF00E7E7E700DEDEDE00C6C6C600B5B5B500C6C6C600FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00F7F7F700EFEFEF00EFEFEF00EFEFEF00EFEFEF00F7F7F700C684
      6300EFEFEF00E7E7E700DEDEDE00C6C6C600B5B5B500C6C6C600FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7CEAD00F7DECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00840000008400
      000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7DECE00F7C6A5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7CEAD00F7DECE00FFFFFF00FFFFFF008400000084000000FFFFFF00FFFF
      FF00FFFFFF008400000084000000FFFFFF00FFFFFF00F7DECE00F7C6A5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00F7F7F700F7F7F700F7F7F700C6846300C6846300F7F7F700F7F7
      F700F7F7F700C6846300BD7B5A00DEDEDE00C6C6C600B5B5B500E7E7E7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700C6846300C684
      6300C6846300F7F7F700F7F7F700DEDEDE00C6C6C600B5B5B500E7E7E7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400000084000000FFFF
      FF008400000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400000084000000FFFF
      FF008400000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700C6846300C6846300F7F7
      F700C6846300C6846300F7F7F700EFEFEF00D6D6D600C6C6C600D6D6D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700C6846300C6846300F7F7
      F700C6846300C6846300F7F7F700EFEFEF00D6D6D600C6C6C600D6D6D6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF008400000084000000FFFFFF00FFFF
      FF00FFFFFF008400000084000000FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00840000008400
      000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700C6846300C684
      6300C6846300F7F7F700F7F7F700F7F7F700E7E7E700CECECE00CECECE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00F7F7F700F7F7F700C6846300C6846300F7F7F700F7F7
      F700F7F7F700C6846300C6846300F7F7F700E7E7E700CECECE00CECECE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700C684
      6300F7F7F700F7F7F700F7F7F700F7F7F700EFEFEF00D6D6D600CECECE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700C684
      6300F7F7F700F7F7F700F7F7F700F7F7F700EFEFEF00D6D6D600CECECE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00840000008400
      000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF008400000084000000FFFFFF00FFFF
      FF00FFFFFF008400000084000000FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00F7F7F700C6846300C6846300F7F7F700F7F7
      F700F7F7F700C6846300C6846300F7F7F700EFEFEF00DEDEDE00DEDEDE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00F7F7F700F7F7F700F7F7F700C6846300C684
      6300C6846300F7F7F700F7F7F700F7F7F700EFEFEF00DEDEDE00DEDEDE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400000084000000FFFF
      FF008400000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400000084000000FFFF
      FF008400000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00F7F7F700F7F7F700C6846300C6846300F7F7
      F700C6846300C6846300F7F7F700F7F7F700EFEFEF00E7E7E700EFEFEF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00F7F7F700F7F7F700C6846300C6846300F7F7
      F700C6846300C6846300F7F7F700F7F7F700EFEFEF00E7E7E700EFEFEF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7CEAD00F7DECE00FFFFFF00FFFFFF008400000084000000FFFFFF00FFFF
      FF00FFFFFF008400000084000000FFFFFF00FFFFFF00F7DECE00F7C6A5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7CEAD00F7DECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00840000008400
      000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7DECE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7F700F7F7F700C6846300C684
      6300C6846300F7F7F700F7F7F700F7F7F700F7F7F700EFEFEF00F7F7F7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00C6846300C6846300F7F7F700F7F7
      F700F7F7F700C6846300BD7B5A00F7F7F700F7F7F700EFEFEF00F7F7F7000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7F700C684
      6300F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7F700F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000EFBD9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFBD9C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000EFBD9C00F7DECE00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F7DECE00EFBD9C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000EFBD9C00F7DECE00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F7DECE00EFBD9C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7CEAD00EFBD9C00EFBD9C00EFBD
      9C00EFBD9C00EFBD9C00F7C6AD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7CEAD00EFBD9C00EFBD9C00EFBD
      9C00EFBD9C00EFBD9C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000522900004A3100005229
      0000522900005231000052290000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000006B3900006339000084420000C6630000BD5A
      00008C5A08009C5A000073420000522900005229000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B
      5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B
      5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006B31000063521000EF730000FF7B0000FF8400007BA5
      3900399C31004A9C31006B7B2100AD5A00007B42000052290000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A53800000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7
      DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7
      DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00EFE7DE00B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000073310000527321008CA52900E77B0000FF8C0000F7A5180031C6
      4A0029B5290031B5310039A53100637B2100B56300008C4A0000522900000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A538
      0000A5380000A538000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006B31000052C652005ABD4200948C1000F78C0000CED67B0084EF
      B50031BD290029B5210018C652004AA53900BD630000C66300004A3100000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5380000A538
      000000000000A5380000A5380000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000844200008C6329004AE7630039DE63009C941000DE840000FFA50000FFE7
      9C00D6FFF7004AD65200B5AD2900EF941000EF630000BD6300007B4A00005229
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5380000A53800000000
      00000000000000000000A5380000A53800000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE7007B52
      2900F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084420000C6AD5A0042EF7B004ABD1800ADB50800FFAD0000FFA50000FFA5
      0000E7941000FFAD1000FF8C0000FF840000EF7B0000CE6B0000BD6300005231
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A53800000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE7007B5229007B52
      29007B522900F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE7007B522900F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE7007B522900F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084420000CEBD7B0084E77B00CEB50000FFBD0000FFBD0000FFBD0000B5C6
      08005A940800C66B0000FF9C0000FF940000FF840000C65A0000945200005229
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A538
      0000A5380000A538000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE7007B5229007B5229007B52
      29007B5229007B522900F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE7007B5229007B5229007B522900F7EFE700F7EF
      E700F7EFE7007B5229007B5229007B522900F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084420000E7A53100FFCE2100FFC60000FFD60000FFE70800F7DE08005AFF
      7B0021EF210063941800E77B0000FF8C0000E76B000084942100637B18005229
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A5380000A538
      000000000000A5380000A5380000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE7007B5229007B5229007B522900F7EF
      E7007B5229007B5229007B522900F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE7007B5229007B5229007B522900F7EF
      E7007B5229007B5229007B522900F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000844200009C8C0000C69C0000DEAD0000FFE71000FFEF5A00FFDE0800E7FF
      FF0094FF94005ADE4200B56B0000CE730000739C210042AD3100B56B00005229
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5380000A53800000000
      00000000000000000000A5380000A53800000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE7007B5229007B5229007B522900F7EFE700F7EF
      E700F7EFE7007B5229007B5229007B522900F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE7007B5229007B5229007B52
      29007B5229007B522900F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000844200009463000084DE3900DE9C0000F7D60800FFEF5A00FFD60000FFC6
      0000FFC629008CEF63004AB521004ABD290021BD390094841000844A00005229
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE7007B522900F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE7007B522900F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE7007B5229007B52
      29007B522900F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084390000A5FFB50084CE1800D6A50000FFD60000F7DE0000BD8C
      0000D6941000E7FFEF0039D6390031C6310052AD2900BD7300006B3900000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE7007B52
      2900F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000844200006B733900DEFFF700ADD61000DEA50000F7D6080063D6
      5A00CE730000DEE7840031D6420031C631004AB531007B420000733900000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084420000637B29007BE76B00C6940000FFB50000ADCE
      4A009CAD1000DEDE6B0084F7940063CE5200526B180073310000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00FFFFFF00F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008442000084390000A55A0000FFA50800BDD6
      7B007BBD3900E79C10009C5A00006B3100007331000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00F7EFE700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B56B5A00F7EFE700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7EFE700B56B5A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084420000844200008442
      0000844200008442000084420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B
      5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B
      5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00B56B5A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004A42
      31004A4231004A4231004A4231004A4231004A4231004A4231004A4231004A42
      31004A4231004A4231004A4231004A4231004A4231004A4231004A4231004A42
      31004A4231004A42310000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000029313900394A
      5200293139000000000000000000000000001008100010101000080810001010
      100008081000101010000808100010081000FFFF000080800000808000008080
      0000808000008080000080800000808000000000000000000000CE9C7B009C7B
      6B009C7B6B009C7B6B009C7B6B009C7B6B009C7B6B009C7B6B009C7B6B009C7B
      6B009C7B6B009C7B6B009C7B6B009C7B6B009C7B6B009C7B6B009C7B6B009C7B
      6B009C7B6B004A42310000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000008081000101821002121
      2900101821000000000000000000181018001810210010101000101010001010
      100010081000101010001008100010101000FFFF0000808000008080000000FF
      FF0000FFFF008080000080800000808000000000000000000000C69C7B00FFDE
      C600EFEFEF00FFDEC600F7D6BD00FFCE9C00F7C68C00E7B59400F7AD8C00F7B5
      8C00F7A57300EFA57B00F7A57300E79C6300E7946300E79C6B00E7946300CE7B
      4A009C7B6B004A42310000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000808100010101000080810002121
      290000000000000000004A525A00212931002921290000000000000000000000
      000000000000101010001010100018181800FFFF00008080000000FFFF008080
      00008080000000FFFF0080800000808000000000000000000000AD948400C6B5
      9C00F7F7EF00F7EFEF00EFF7EF00FFDEC600F7D6BD00F7D6BD00F7D6B500F7D6
      BD00F7CEAD00FFCE9C00FFCEA500F7BD8C00E7B59400F7A57300E7946B009C7B
      6B009C7B6B004A42310000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000008400000084000000
      8400000084000000840084848400848484008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000800080008081000181818000808
      1000292129005A4A52002921290029182100312931004A394200292129002118
      210021182100211818003129310029182100FFFF000080800000808000008080
      00008080000000FFFF0080800000808000000000000000000000AD948400F7EF
      EF00C6AD9C00EFEFEF00F7EFEF00EFEFEF00F7F7EF00F7DEC600FFDECE00F7DE
      C600F7D6C600F7D6B500F7D6BD00FFCE9C00F7C68C00FFAD8C009C7B6B00E794
      6B009C7B6B004A42310000000000000000000000000000000000000000000000
      000000000000000000000000000000008400000084000000FF000000FF000000
      FF000000FF000000FF0000008400000084008484840084848400000000000000
      00000000000000000000000000000000000000000000FFFFFF00808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000000000000800080008080800101010001008
      1000181018004A4242007B63630042292900392929007B636300422931003929
      310031292900312129005A42420029181800FFFF000080800000808000008080
      00008080000000FFFF0080800000808000000000000000000000AD948C00F7F7
      EF00F7EFEF00CEA58400F7F7EF00F7EFEF00EFEFEF00F7F7E700EFEFEF00F7F7
      EF00FFDEC600F7D6C600F7DEBD00F7D6B500F7C68C009C7B6B00F7B58C00F7A5
      73009C7B6B004A42310000000000000000000000000000000000000000000000
      00000000000000000000000084000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000840084848400848484000000
      00000000000000000000000000000000000000000000FFFFFF00C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000000000000800080008081000101010001010
      100018101000291821007B5A5A00A5847B005A4239008C6B6B00735A52006342
      42004A393900634242006B4A4A0039292900FFFF000080800000808000008080
      00008080000000FFFF0080800000808000000000000000000000AD948400F7EF
      EF00EFF7EF00F7EFEF00D6A58400EFF7EF00F7EFEF00EFEFEF00F7EFEF00EFEF
      EF00EFEFEF00FFE7C600FFDECE00FFCEA5009C7B6B00FFCE9C00FFCE9C00F7C6
      8C009C7B6B004A42310000000000000000000000000000000000000000000000
      000000000000000084000000FF000000FF00C6C6C6000000FF000000FF000000
      FF000000FF000000FF00C6C6C6000000FF000000FF0000008400848484000000
      00000000000000000000000000000000000000000000FFFFFF00C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000000000000800080010101000101010001810
      210021181800291821004A393900A5847B00B5948C00845A5200BD949400946B
      6B007B5252009C736B00634A4200634A5200FFFF000080800000808000008080
      0000808000008080000080800000808000000000000000000000CE9C7B00FFFF
      FF00EFEFEF00F7F7EF00C6AD9C00D6A58C00FFE7CE00EFEFEF00F7F7EF00F7EF
      EF00F7F7EF00EFEFEF00F7D6B5009C7B6B00AD948C00EFB59400F7D6BD00FFCE
      9C009C7B6B004A42310000000000000000000000000000000000000000000000
      000000000000000084000000FF00FFFFFF00FFFFFF00C6C6C6000000FF000000
      FF000000FF00FFFFFF00FFFFFF00C6C6C6000000FF0000008400848484008484
      84000000000000000000000000000000000000000000FFFFFF00C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000000000001008100000000000212129002110
      1800312931004A3942004A3939007B4A4A00CEA59C00CEA59C00D6ADA500BD8C
      7B00B5736B00BD8C7B00A56B6B0084635A00FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF00000000000000000000D6A58C00FFFF
      FF00FFE7CE00C6AD9C00FFFFFF00FFFFFF00CEA58400D6A58C00D6A58400AD94
      8C00AD948C00AD948C00AD948C00F7EFEF00F7F7EF009C736B00E7BD9400FFCE
      A5009C7B6B004A42310000000000000000000000000000000000000000000000
      0000000084000000FF000000FF00C6C6C600FFFFFF00FFFFFF00C6C6C6000000
      FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000FF00000084008484
      84000000000000000000000000000000000000000000FFFFFF00C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000000000001008100021101800211818003118
      210039292900523939006B4A42009C5A5A00D69C8C00E7CEBD00E7DED600EFDE
      DE00E7DED600E7DED600E7CEC600DEAD9400A56B63007B4A4A005A3939003929
      29005A4A4A003129310018181800100810000000000000000000D6AD8400FFDE
      CE00C6AD9C00FFFFFF00F7F7EF00D6CECE00DED6CE00D6D6CE00D6D6CE00DED6
      CE00D6D6CE00DEBDAD00DEC6AD00E7C6AD00D6CECE00F7F7EF009C7B6B00E7AD
      8C009C7B6B004A42310000000000000000000000000000000000000000000000
      0000000084000000FF000000FF000000FF00C6C6C600FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF00000084008484
      84000000000000000000000000000000000000000000FFFFFF00C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C0008080800000000000524242006B52520084636300946B
      6B00A5847B00B58C8400C69C9400D6AD9C00DEBDAD00E7D6C600E7DED600E7E7
      E700E7E7E700E7DED600E7D6C600E7BDAD00DEADA500C69C9400B5948C00A57B
      7B00946B6B007B5A5A00634A52004A3939000000000000000000CEADA500C6AD
      9C00FFDEC600FFFFFF00F7DECE00FFDEC600F7F7EF00FFDECE00FFDEC600FFDE
      CE00F7DEC600F7F7EF00F7EFEF00EFF7EF00F7EFEF00EFEFEF00D6BDAD00A57B
      6B009C7B6B004A42310000000000000000000000000000000000000000000000
      0000000084000000FF000000FF000000FF000000FF00C6C6C600FFFFFF00FFFF
      FF00FFFFFF000000FF000000FF000000FF000000FF000000FF00000084008484
      84000000000000000000000000000000000000000000FFFFFF00C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000000000001010100021101800291821003121
      21004229290052393900734A4A009C5A5A00C68C8400E7CEBD00E7DED600E7E7
      DE00E7E7E700E7DED600DEBDAD00C6847300A5635A0084524A006B4242005231
      3100422929003921210039212100291818000000000000000000D6AD84007BD6
      F700F7F7EF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F7EFEF00EFEFEF00F7EFE700EFEFEF00F7F7EF00F7EFEF00DEC6AD0031A5
      F7009C7B6B004A42310000000000000000000000000000000000000000000000
      0000000084000000FF000000FF000000FF000000FF00C6C6C600FFFFFF00FFFF
      FF00FFFFFF00C6C6C6000000FF000000FF000000FF000000FF00000084008484
      84000000000000000000000000000000000000000000FFFFFF00C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000000000001008100000000000292129002118
      1800312129005231310084636300CEA5A500CEA59C00D6A59400E7C6B500E7CE
      BD00E7D6C600E7DED600D6AD9C00D69C8C00B57B7300734A42004A2929003121
      2900291018001818180018101800100810000000000000000000CEAD9C0063C6
      F700F7EFEF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F7F7E700F7EFEF00F7F7EF00F7EFEF00ADE7F700ADE7EF0094C6D60031AD
      EF009C7B6B000000000000000000000000000000000000000000000000000000
      0000000084000000FF000000FF000000FF00C6C6C600FFFFFF00C6C6C600C6C6
      C600FFFFFF00FFFFFF00C6C6C6000000FF000000FF000000FF00000084000000
      00000000000000000000000000000000000000000000FFFFFF00C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080808000000000001008100010081000181018002118
      2900524242008C737300947B7300735252008C5A5A00BD847B00D69C8C00D68C
      7B00D6AD9C00DEB5AD00EFCEBD00B5736B007B525200735A52005A4242003118
      2100181018001810180010101000100810000000000000000000C6AD9C00C6F7
      FF00ADE7F700A5E7F70094E7F7007BD6F7006BC6F70063CEF70052B5EF004ABD
      F7004AB5F70052BDF70031B5DE0039ADF70031A5EF0021A5EF0039A5EF0039AD
      F700A57B6B000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF00C6C6C600FFFFFF00C6C6C6000000FF000000
      FF00C6C6C600FFFFFF00FFFFFF00C6C6C6000000FF0000008400848484000000
      00000000000000000000000000000000000000000000FFFFFF0080808000C0C0
      C000000000000000000000000000C0C0C00000000000C0C0C00080808000C0C0
      C0000000000000000000C0C0C000C0C0C00080808000C0C0C000000000000000
      0000C0C0C000C0C0C00080808000000000001010100021212900292931002929
      3100181021002110180029212900422931004229310084635A00734A4A006B4A
      42007B4A4A00AD847B00845A5A00946B6300BD948C0052313100311821002118
      180018101000101010001008100010101000000000000000000000000000CEAD
      9C00C6F7FF00C6F7FF00C6EFFF00C6F7FF00A5E7F70094E7FF007BD6F7007BD6
      F70084D6F7007BD6F7006BC6F7006BCEF7004AB5EF0052B5F7004ABDF700AD94
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000084000000FF00C6C6C600C6C6C6000000FF000000FF000000
      FF000000FF00C6C6C600FFFFFF000000FF000000FF0000008400000000000000
      00000000000000000000000000000000000000000000FFFFFF0080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000808080008080
      8000808080008080800080808000C0C0C0008080800080808000808080008080
      800080808000C0C0C00080808000000000000800100008081000101010000000
      000000000000181010002118210029182100392129006B525200422931004A39
      39004A313100634A4A007B5A5A00523931008C6363008C636300312121001810
      2100292129001008080010101000080810000000000000000000000000000000
      0000CEAD9C00DED6CE00C6EFFF00C6F7FF00C6EFFF00C6F7FF00C6EFFF00ADE7
      F7008CE7F70084D6F70063CEF7006BC6F7004ABDF700BDBDBD00849CA5000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000840000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00808080008080
      80008080800080808000808080008080800080808000C0C0C00080808000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000800080008001000080810001010
      1000101010001008100018101800292129003929310039293100291821003121
      29003121290039212900634A5200312929003921210073525200523942001810
      1800181018001008100008080800080810000000000000000000000000000000
      000000000000BDBDBD00CEBDB500C6F7FF00C6EFFF00C6F7FF00C6F7FF00C6EF
      FF008CE7FF007BDEF7006BC6F7006BCEF700AD94840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000FF000000FF000000FF000000
      FF000000FF000000FF0000008400000084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000800080008000800080008000808
      1000101010001008100010101000211821003929310021182100211818002118
      2100211818002118210042313900292129002110180029182100524242002921
      2900000000001008100008080800080808000000000000000000000000000000
      0000000000000000000000000000CEAD9C00DED6CE00C6EFFF00C6EFFF00C6F7
      FF00ADE7F7007BD6F7008CC6D600AD9484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000084000000
      8400000084000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000080008000800080008001008
      1000101010000808080010101000181018001810210018101800101010001818
      1800181018001810180021181800292129000000000018101000211018003931
      3900101010001008100008080800080008000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD00E7C6B500C6F7FF00C6F7
      FF00ADE7F700C6AD9C00CEAD9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000080000000800080008000800
      0800080008000808100008081000292129001010100010101000101010002121
      2900000000001008100010101000100818001010100010081000101010001008
      1000080810000808100008000800080008000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CEB59C00C6AD
      9C00CEAD9C00CEAD9C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000080000000800000008000800
      0800080008000800080008081000080810000808080008081000080810001010
      1000080810001008100010101000000000000000000010101000080810000808
      100008081000181818000808100008000800424D3E000000000000003E000000
      2800000060000000780000000100010000000000A00500000000000000000000
      000000000000000000000000FFFFFF00FF80FF000000000000000000FF80FF00
      0000000000000000FFC1FF000000000000000000CFC1FF000000000000000000
      C0C1FF000000000000000000F8C1FF000000000000000000CFC1FF0000000000
      00000000C7C17F000000000000000000F0C12F000000000000000000CFC1E300
      0000000000000000C7C17F000000000000000000F1C12F000000000000000000
      FD8123000000000000000000CF00FB000000000000000000E0F7AF0000000000
      00000000F81FA3000000000000000000FF81FB000000000000000000FFE07F00
      0000000000000000FFFF23000000000000000000FFFFE3000000000000000000
      FFFFFFFFFFFFFFFFFFF001FFFFFFFFFFFFFFFFFFFFF001FFFFFFFFFFFFFFFFFF
      FFF803FFFFC7FFFF00FFF0001FFE03FFFF81FFFE007FF0001FFE0407FF007FFE
      007FF8001FFE0003FE001FFE000FF8003FFE0003FC000FFF0007FC003FFE0001
      F8001FFC0007FC007FFF0001F0001FF80007FE00FFFF8001F0000FF80007FF81
      FFFF8001F0001FF8000FFFC3FFFFC003F0003FFC003FFF83FFFFC003F0007FFE
      001FFF01FFFFC01FF800FFFF0C1FFF01FFFFE0FFFE01FFFF041FFF01FFFFF07F
      FF83FFFF063FFF01FFF0007FFFE7FFFF07FFFF81FFF0007FFFFFFFFF8FFFFFC3
      FFF0007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800000FFFFFFFFFFFFFF
      FFFF000000FFFFFFFFFFFFFF81FF000000FFFFFFFF839FFE007F000000FF800F
      FE005FFC003F000000FF000FFC009FF8001F000000FE000FF8003FF8001F0000
      00FC000FF8003FF0000F000000F8000FF0001FF0000F000000F8000FF0001FF0
      000F000000F8000FF0001FF0000F000000F8000FF0001FF0000F000000F8000F
      F0001FF0000F000000F8000FF8003FF8001F000000F8001FF8003FF8001F0000
      00F8307FFC007FFC003F000000F83C7FFE00FFFE007F000000FC7FFFFF83FFFF
      81FF000000FEFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFF01FFFF01FFFE00FFFE00FFFE00FFFE00FFFC007FFC007FFC007FFC
      007FF8003FF8003FF8003FF8003FF0001FF0001FF0001FF0001FF0001FF0001F
      F0001FF0001FF0001FF0001FF0001FF0001FF0001FF0001FF0001FF0001FF000
      1FF0001FF0001FF0001FF0001FF0001FF0001FF0001FF0001FF0001FF0001FF0
      003FF0001FF0001FF8003FF8003FF0001FF0001FFC007FFC007FF8003FF8003F
      FE00FFFE00FFFC007FFC007FFF01FFFF03FFFE00FFFE00FFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFF81FFFFFFFFFFFFFFFFFFFFFE007FFFFFFFF8003FF8003F
      FC003FFFF7FFF0001FF0001FF8001FFFE3FFF0001FF0001FF8001FFFC9FFF000
      1FF0001FF0000FFF9CFFF0001FF0001FF0000FFFF7FFF0001FF0001FF0000FFF
      E3FFF0001FF0001FF0000FFFC9FFF0001FF0001FF0000FFF9CFFF0001FF0001F
      F0000FFFFFFFF0001FF0001FF8001FFFFFFFF0001FF0001FF8001FFFFFFFF000
      1FF0001FFC003FFFFFFFF0001FF0001FFE007FFFFFFFF0001FF0001FFF81FFFF
      FFFFF8003FF8003FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      E00003FFFFFFFFFFFF800000C00003FFFFFFFFFFFF020000C00003FFC1FFFFFF
      FF040000C00003FF007FFFFFFF000000C00003FE003F800001000000C00003FC
      001F800001000000C00003F8001F800001000000C00003F8000F800001400000
      C00003F0000F800001000000C00003F0000F800001000000C00003F0000F8000
      01000000C00003F0000F800001000000C00007F0001F800001000000C00007F8
      001F800001000000E0000FF8003F800001000000F0001FFC007F800083000000
      F8007FFE00FFC03FFF000000FE00FFFF83FFFFFFFF000000FF01FFFFFFFFFFFF
      FF000800FFC3FFFFFFFFFFFFFF00008000000000000000000000000000000000
      000000000000}
  end
  object JvLookupAutoCompl: TJvLookupAutoComplete
    Edit = JvEdtCompSearch
    ListBox = JvListBoxDemosCompNameSorted
    Strings.Strings = (
      'aaa'
      'abb'
      'bbb'
      'ccc')
    Left = 824
    Top = 213
  end
  object JvJVCLAboutComp: TJvJVCLAboutComponent
    Left = 825
    Top = 264
  end
end
