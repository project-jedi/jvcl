object JvPageListTreeViewMainFrm: TJvPageListTreeViewMainFrm
  Left = 362
  Top = 148
  ActiveControl = JvPagedTreeView1
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'VS.Net Options Dialog Look-Alike'
  ClientHeight = 375
  ClientWidth = 597
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 180
    Top = 0
    Width = 4
    Height = 322
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 356
    Width = 597
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 180
    Height = 322
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = ' '
    TabOrder = 1
    object JvPagedTreeView1: TJvSettingsTreeView
      Left = 4
      Top = 4
      Width = 172
      Height = 314
      PageDefault = 0
      PageList = JvPageList1
      Align = alClient
      Images = ImageList1
      Indent = 19
      TabOrder = 0
      Items.Data = {
        0A00000024000000010000000100000000000000FFFFFFFF000000000A000000
        0B456E7669726F6E6D656E7420000000FFFFFFFFFFFFFFFF01000000FFFFFFFF
        01000000000000000747656E6572616C22000000FFFFFFFFFFFFFFFF01000000
        FFFFFFFF020000000000000009446F63756D656E747325000000FFFFFFFFFFFF
        FFFF02000000FFFFFFFF03000000000000000C44796E616D69632048656C7029
        000000FFFFFFFFFFFFFFFF03000000FFFFFFFF040000000000000010466F6E74
        7320616E6420436F6C6F72731D000000FFFFFFFFFFFFFFFF02000000FFFFFFFF
        05000000000000000448656C702F000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF060000000000000016496E7465726E6174696F6E616C2053657474696E6773
        21000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0700000000000000084B6579
        626F6172642F000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF08000000000000
        001650726F6A6563747320616E6420536F6C7574696F6E7322000000FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF0900000000000000095461736B204C6973742400
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF09000000000000000B5765622042
        726F77736572270000000100000001000000FFFFFFFFFFFFFFFF000000000200
        00000E536F7572636520436F6E74726F6C20000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFF00000000000000000747656E6572616C25000000FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF00000000000000000C5343432050726F76696465722400
        00000100000001000000FFFFFFFFFFFFFFFF00000000090000000B5465787420
        456469746F7220000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        00000747656E6572616C260000000100000001000000FFFFFFFFFFFFFFFF0000
        0000020000000D416C6C204C616E67756167657320000000FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF00000000000000000747656E6572616C1D000000FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000004546162731E000000010000
        0001000000FFFFFFFFFFFFFFFF000000000300000005426173696320000000FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000747656E6572616C1D
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000454616273
        23000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000A466F72
        6D617474696E671B0000000000000000000000FFFFFFFFFFFFFFFF0000000003
        00000002432320000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        00000747656E6572616C1D000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
        000000000000045461627323000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        000000000000000A466F726D617474696E671E0000000000000000000000FFFF
        FFFFFFFFFFFF000000000300000005432F432B2B20000000FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF00000000000000000747656E6572616C1D000000FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF0000000000000000045461627323000000FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000A466F726D617474696E67
        1C0000000000000000000000FFFFFFFFFFFFFFFF000000000100000003435353
        20000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000747656E
        6572616C210000000000000000000000FFFFFFFFFFFFFFFF0000000001000000
        0848544D4C2F584D4C20000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        00000000000747656E6572616C1F0000000000000000000000FFFFFFFFFFFFFF
        FF000000000100000006504C2F53514C20000000FFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF00000000000000000747656E6572616C230000000000000000000000
        FFFFFFFFFFFFFFFF00000000010000000A506C61696E205465787420000000FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000747656E6572616C21
        0000000000000000000000FFFFFFFFFFFFFFFF000000000100000008416E616C
        797A657220000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000
        0747656E6572616C270000000000000000000000FFFFFFFFFFFFFFFF00000000
        040000000E446174616261736520546F6F6C732A000000FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFF00000000000000001144617461626173652050726F6A656374
        732A0000000000000000000000FFFFFFFFFFFFFFFF0000000003000000114461
        7461626173652044657369676E657220000000FFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF00000000000000000747656E6572616C1F000000FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFF0000000000000000064F7261636C6523000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF00000000000000000A53514C205365727665722C0000
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000013517565727920
        566965772044657369676E657228000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF00000000000000000F536572766572204578706C6F72657222000000000000
        0000000000FFFFFFFFFFFFFFFF000000000400000009446562756767696E6720
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000747656E65
        72616C2A000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000011
        4564697420616E6420436F6E74696E756525000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFF00000000000000000C4A7573742D496E2D54696D651F000000FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000000000064E6174697665260000
        000000000000000000FFFFFFFFFFFFFFFF00000000020000000D48544D4C2044
        657369676E657220000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000
        0000000747656E6572616C20000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000007446973706C6179210000000000000000000000FFFFFFFFFF
        FFFFFF00000000030000000850726F6A6563747323000000FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF00000000000000000A56432B2B204275696C6429000000FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000001056432B2B20446972
        6563746F7269657325000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
        000000000C5765622053657474696E67732E0000000000000000000000FFFFFF
        FFFFFFFFFF00000000010000001557696E646F777320466F726D204465736967
        6E657220000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000007
        47656E6572616C250000000000000000000000FFFFFFFFFFFFFFFF0000000001
        0000000C584D4C2044657369676E6572200000000200000002000000FFFFFFFF
        FFFFFFFF00000000000000000747656E6572616C}
      Items.Links = {
        4100000001000000010000000200000003000000040000000500000006000000
        0700000008000000090000000900000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000}
    end
  end
  object JvStandardPage2: TJvStandardPage
    Left = 184
    Top = 0
    Width = 413
    Height = 322
    Caption = 'JvStandardPage2'
  end
  object JvStandardPage4: TJvStandardPage
    Left = 184
    Top = 0
    Width = 413
    Height = 322
    Caption = 'JvStandardPage4'
  end
  object JvStandardPage1: TJvStandardPage
    Left = 184
    Top = 0
    Width = 413
    Height = 322
    Caption = 'JvStandardPage1'
  end
  object JvStandardPage5: TJvStandardPage
    Left = 184
    Top = 0
    Width = 413
    Height = 322
    Caption = 'JvStandardPage5'
  end
  object JvFooter1: TJvFooter
    Left = 0
    Top = 322
    Width = 597
    Height = 34
    Align = alBottom
    object JvFooterBtn2: TJvFooterBtn
      Left = 354
      Top = 5
      Width = 75
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = JvFooterBtn2Click
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ButtonIndex = 0
      SpaceInterval = 6
    end
    object JvFooterBtn3: TJvFooterBtn
      Left = 435
      Top = 5
      Width = 75
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = JvFooterBtn2Click
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ButtonIndex = 1
      SpaceInterval = 6
    end
    object JvFooterBtn1: TJvFooterBtn
      Left = 514
      Top = 5
      Width = 75
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Help'
      TabOrder = 0
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ButtonIndex = 2
      SpaceInterval = 6
    end
  end
  object JvPageList1: TJvPageList
    Left = 184
    Top = 0
    Width = 413
    Height = 322
    ActivePage = pgEnvironmentGeneral
    PropagateEnable = True
    Align = alClient
    object pgEnvironmentGeneral: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - General'
      object Label1: TLabel
        Left = 248
        Top = 8
        Width = 53
        Height = 13
        Caption = 'At startup:'
      end
      object Label2: TLabel
        Left = 48
        Top = 120
        Width = 30
        Height = 13
        Caption = 'Speed'
      end
      object Label3: TLabel
        Left = 16
        Top = 168
        Width = 34
        Height = 13
        Caption = 'Display'
      end
      object Label4: TLabel
        Left = 16
        Top = 200
        Width = 34
        Height = 13
        Caption = 'Display'
      end
      object Label5: TLabel
        Left = 104
        Top = 168
        Width = 104
        Height = 13
        Caption = 'items in window menu'
      end
      object Label6: TLabel
        Left = 104
        Top = 200
        Width = 151
        Height = 13
        Caption = 'items in most recently used lists'
      end
      object JvGroupHeader3: TJvGroupHeader
        Left = 0
        Top = 232
        Width = 409
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Docked Tool Window Behaviour'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
      end
      object RadioButton1: TRadioButton
        Left = 16
        Top = 8
        Width = 113
        Height = 17
        Caption = 'Tabbed documents'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object RadioButton2: TRadioButton
        Left = 16
        Top = 24
        Width = 113
        Height = 17
        Caption = 'MDI environment'
        TabOrder = 1
      end
      object Button1: TButton
        Left = 16
        Top = 48
        Width = 153
        Height = 22
        Caption = 'Reset Window Layout'
        TabOrder = 2
      end
      object ComboBox1: TComboBox
        Left = 248
        Top = 24
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 3
        Text = 'Show Start Page'
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 88
        Width = 121
        Height = 17
        Caption = 'Show status bar'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object CheckBox2: TCheckBox
        Left = 16
        Top = 104
        Width = 201
        Height = 17
        Caption = 'Animate environment tools'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object TrackBar1: TTrackBar
        Left = 88
        Top = 120
        Width = 73
        Height = 15
        Position = 5
        TabOrder = 6
        ThumbLength = 10
        TickMarks = tmBoth
        TickStyle = tsNone
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 136
        Width = 241
        Height = 17
        Caption = 'Enable Command Window autocompletion'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object Edit1: TEdit
        Left = 64
        Top = 164
        Width = 33
        Height = 21
        TabOrder = 8
        Text = '  10'
      end
      object Edit2: TEdit
        Left = 64
        Top = 196
        Width = 33
        Height = 21
        TabOrder = 9
        Text = '  4'
      end
      object CheckBox4: TCheckBox
        Left = 32
        Top = 256
        Width = 193
        Height = 17
        Caption = 'Close button affects active tab only'
        Checked = True
        State = cbChecked
        TabOrder = 10
      end
      object CheckBox5: TCheckBox
        Left = 32
        Top = 272
        Width = 217
        Height = 17
        Caption = 'Auto Hide button affects active tab only'
        Checked = True
        State = cbChecked
        TabOrder = 11
      end
    end
    object pgDocuments: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - Documents'
      object Label7: TLabel
        Left = 32
        Top = 112
        Width = 175
        Height = 13
        Caption = 'Miscellaneous files project saves last'
      end
      object Label8: TLabel
        Left = 272
        Top = 112
        Width = 25
        Height = 13
        Caption = 'items'
      end
      object JvGroupHeader4: TJvGroupHeader
        Left = 0
        Top = 168
        Width = 409
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Find and Replace'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelSpace = 4
      end
      object CheckBox6: TCheckBox
        Left = 8
        Top = 8
        Width = 369
        Height = 17
        Caption = 'Reuse current window if saved'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object CheckBox7: TCheckBox
        Left = 8
        Top = 24
        Width = 369
        Height = 17
        Caption = 'Detect when file is changed outside the environment'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object CheckBox8: TCheckBox
        Left = 24
        Top = 40
        Width = 369
        Height = 17
        Caption = 
          'Autoload changes (if not currently modified inside the environme' +
          'nt)'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object CheckBox9: TCheckBox
        Left = 8
        Top = 56
        Width = 369
        Height = 17
        Caption = 'Allow editing of read-only files, warn when attempt to save'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object CheckBox10: TCheckBox
        Left = 8
        Top = 72
        Width = 369
        Height = 17
        Caption = 'Open file using directory of currently active document'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object CheckBox11: TCheckBox
        Left = 8
        Top = 88
        Width = 369
        Height = 17
        Caption = 'Show miscellaneous files in Solution Explorer'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object Edit3: TEdit
        Left = 219
        Top = 109
        Width = 38
        Height = 21
        TabOrder = 6
        Text = '  0'
      end
      object CheckBox12: TCheckBox
        Left = 8
        Top = 184
        Width = 161
        Height = 17
        Caption = 'Show message boxes'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object CheckBox13: TCheckBox
        Left = 8
        Top = 200
        Width = 161
        Height = 17
        Caption = 'Initialize Find Text from editor'
        Checked = True
        State = cbChecked
        TabOrder = 8
      end
    end
    object pgDynamicHelp: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - Dynamic Help'
      object Label9: TLabel
        Left = 8
        Top = 8
        Width = 56
        Height = 13
        Caption = 'Categories:'
      end
      object Label10: TLabel
        Left = 192
        Top = 8
        Width = 59
        Height = 13
        Caption = 'Topic types:'
      end
      object Label11: TLabel
        Left = 16
        Top = 184
        Width = 70
        Height = 13
        Caption = 'Show links for:'
      end
      object ListView1: TListView
        Left = 8
        Top = 24
        Width = 177
        Height = 105
        Checkboxes = True
        Columns = <>
        Items.Data = {
          BC0000000600000000000000FFFFFFFFFFFFFFFF00000000000000000448656C
          7000000000FFFFFFFFFFFFFFFF000000000000000007416374696F6E73000000
          00FFFFFFFFFFFFFFFF00000000000000000D4D697363656C6C616E656F757300
          000000FFFFFFFFFFFFFFFF00000000000000000753616D706C657300000000FF
          FFFFFFFFFFFFFF00000000000000000F47657474696E67205374617274656400
          000000FFFFFFFFFFFFFFFF000000000000000008547261696E696E67}
        TabOrder = 0
        ViewStyle = vsList
      end
      object ListView2: TListView
        Left = 192
        Top = 24
        Width = 201
        Height = 105
        Checkboxes = True
        Columns = <>
        Items.Data = {
          B60000000600000000000000FFFFFFFFFFFFFFFF000000000000000007417274
          69636C6500000000FFFFFFFFFFFFFFFF00000000000000000950726F63656475
          726500000000FFFFFFFFFFFFFFFF00000000000000000B4F7269656E74617469
          6F6E00000000FFFFFFFFFFFFFFFF0000000000000000095265666572656E6365
          00000000FFFFFFFFFFFFFFFF00000000000000000653616D706C6500000000FF
          FFFFFFFFFFFFFF00000000000000000653796E746178}
        TabOrder = 1
        ViewStyle = vsList
      end
      object Button2: TButton
        Left = 16
        Top = 136
        Width = 75
        Height = 22
        Caption = 'Move Up'
        TabOrder = 2
      end
      object Button3: TButton
        Left = 96
        Top = 136
        Width = 75
        Height = 22
        Caption = 'Move Down'
        TabOrder = 3
      end
      object RadioButton3: TRadioButton
        Left = 24
        Top = 200
        Width = 113
        Height = 17
        Caption = 'Selection only'
        TabOrder = 4
      end
      object RadioButton4: TRadioButton
        Left = 24
        Top = 216
        Width = 113
        Height = 17
        Caption = 'Active UI element'
        TabOrder = 5
      end
      object RadioButton5: TRadioButton
        Left = 24
        Top = 232
        Width = 113
        Height = 17
        Caption = 'Show all links'
        Checked = True
        TabOrder = 6
        TabStop = True
      end
      object CheckBox14: TCheckBox
        Left = 176
        Top = 184
        Width = 217
        Height = 17
        Caption = 'Limit numbers of links per category:'
        TabOrder = 7
      end
      object Edit4: TEdit
        Left = 200
        Top = 208
        Width = 41
        Height = 21
        Color = clBtnFace
        TabOrder = 8
        Text = '  10'
      end
    end
    object pgFontsColors: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - Fonts and Colors'
      object Label12: TLabel
        Left = 8
        Top = 8
        Width = 88
        Height = 13
        Caption = 'Show settings for:'
      end
      object Label13: TLabel
        Left = 8
        Top = 56
        Width = 217
        Height = 13
        Caption = 'Fonts (bold type indicates fixed-width fonts):'
      end
      object Label14: TLabel
        Left = 296
        Top = 56
        Width = 23
        Height = 13
        Caption = 'Size:'
      end
      object Label15: TLabel
        Left = 8
        Top = 104
        Width = 68
        Height = 13
        Caption = 'Display Items:'
      end
      object Label16: TLabel
        Left = 176
        Top = 104
        Width = 85
        Height = 13
        Caption = 'Item Foreground:'
      end
      object Label17: TLabel
        Left = 176
        Top = 152
        Width = 85
        Height = 13
        Caption = 'Item Background:'
      end
      object Label18: TLabel
        Left = 8
        Top = 232
        Width = 38
        Height = 13
        Caption = 'Sample:'
      end
      object Shape1: TShape
        Left = 8
        Top = 248
        Width = 377
        Height = 41
      end
      object Label19: TLabel
        Left = 152
        Top = 260
        Width = 96
        Height = 16
        Caption = 'AaBbCcXxYyZz'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
      end
      object ComboBox2: TComboBox
        Left = 8
        Top = 24
        Width = 281
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'Text Editor'
      end
      object Button4: TButton
        Left = 296
        Top = 24
        Width = 90
        Height = 22
        Caption = 'Use Defaults'
        TabOrder = 1
      end
      object ComboBox3: TComboBox
        Left = 8
        Top = 72
        Width = 281
        Height = 21
        ItemHeight = 13
        TabOrder = 2
        Text = 'Courier New'
      end
      object ComboBox4: TComboBox
        Left = 296
        Top = 72
        Width = 90
        Height = 21
        ItemHeight = 13
        TabOrder = 3
        Text = ' 10'
      end
      object ListBox1: TListBox
        Left = 8
        Top = 120
        Width = 161
        Height = 97
        ItemHeight = 13
        Items.Strings = (
          'Text'
          'Selected Text'
          'Inactive Selected Text'
          'Indicator Margin'
          'Line Numbers'
          'Visible White Space'
          'Brace Matching'
          'Bookmark'
          'Breakpoint (Disabled)'
          'Breakpoint (Enabled)'
          'Breakpoint (Error)'
          'Breakpoint  (Warning)'
          'Call Return'
          'Collapsible Text'
          'Comment'
          'Compiler Error')
        TabOrder = 4
      end
      object JvColorComboBox1: TJvColorComboBox
        Left = 176
        Top = 120
        Width = 113
        Height = 20
        ColorNameMap.Strings = (
          'clBlack=Black'
          'clMaroon=Maroon'
          'clGreen=Green'
          'clOlive=Olive'
          'clNavy=Navy'
          'clPurple=Purple'
          'clTeal=Teal'
          'clGray=Gray'
          'clSilver=Silver'
          'clRed=Red'
          'clLime=Lime'
          'clYellow=Yellow'
          'clBlue=Blue'
          'clFuchsia=Fuchsia'
          'clAqua=Aqua'
          'clLtGray=Light Gray'
          'clDkGray=Dark Gray'
          'clWhite=White'
          'clMoneyGreen=Money Green'
          'clSkyBlue=Sky Blue'
          'clCream=Cream'
          'clMedGray=Medium Gray'
          'clScrollBar=ScrollBar'
          'clBackground=Background'
          'clActiveCaption=Active Caption'
          'clInactiveCaption=Inactive Caption'
          'clMenu=Menu'
          'clWindow=Window'
          'clWindowFrame=Window Frame'
          'clMenuText=Menu Text'
          'clWindowText=Window Text'
          'clCaptionText=Caption Text'
          'clActiveBorder=Active Border'
          'clInactiveBorder=Inactive Border'
          'clAppWorkSpace=Application Workspace'
          'clHighlight=Highlight'
          'clHighlightText=Highlight Text'
          'clBtnFace=Button Face'
          'clBtnShadow=Button Shadow'
          'clGrayText=Gray Text'
          'clBtnText=Button Text'
          'clInactiveCaptionText=Inactive Caption Text'
          'clBtnHighlight=Button Highlight'
          'cl3DDkShadow=3D Dark Shadow'
          'cl3DLight=3D Light'
          'clInfoText=Info Text'
          'clInfoBk=Info Background'
          'clHotLight=Hot Light'
          'clGradientActiveCaption=Gradient Active Caption'
          'clGradientInactiveCaption=Gradient Inactive Caption'
          'clMenuHighlight=Menu Highlight'
          'clMenuBar=MenuBar'
          'clNone=None'
          'clDefault=Default')
        ColorDialogText = 'Custom...'
        DroppedDownWidth = 113
        NewColorText = 'Custom'
        Options = [coText, coSysColors]
        TabOrder = 5
      end
      object JvColorComboBox2: TJvColorComboBox
        Left = 176
        Top = 168
        Width = 113
        Height = 20
        ColorNameMap.Strings = (
          'clBlack=Black'
          'clMaroon=Maroon'
          'clGreen=Green'
          'clOlive=Olive'
          'clNavy=Navy'
          'clPurple=Purple'
          'clTeal=Teal'
          'clGray=Gray'
          'clSilver=Silver'
          'clRed=Red'
          'clLime=Lime'
          'clYellow=Yellow'
          'clBlue=Blue'
          'clFuchsia=Fuchsia'
          'clAqua=Aqua'
          'clLtGray=Light Gray'
          'clDkGray=Dark Gray'
          'clWhite=White'
          'clMoneyGreen=Money Green'
          'clSkyBlue=Sky Blue'
          'clCream=Cream'
          'clMedGray=Medium Gray'
          'clScrollBar=ScrollBar'
          'clBackground=Background'
          'clActiveCaption=Active Caption'
          'clInactiveCaption=Inactive Caption'
          'clMenu=Menu'
          'clWindow=Window'
          'clWindowFrame=Window Frame'
          'clMenuText=Menu Text'
          'clWindowText=Window Text'
          'clCaptionText=Caption Text'
          'clActiveBorder=Active Border'
          'clInactiveBorder=Inactive Border'
          'clAppWorkSpace=Application Workspace'
          'clHighlight=Highlight'
          'clHighlightText=Highlight Text'
          'clBtnFace=Button Face'
          'clBtnShadow=Button Shadow'
          'clGrayText=Gray Text'
          'clBtnText=Button Text'
          'clInactiveCaptionText=Inactive Caption Text'
          'clBtnHighlight=Button Highlight'
          'cl3DDkShadow=3D Dark Shadow'
          'cl3DLight=3D Light'
          'clInfoText=Info Text'
          'clInfoBk=Info Background'
          'clHotLight=Hot Light'
          'clGradientActiveCaption=Gradient Active Caption'
          'clGradientInactiveCaption=Gradient Inactive Caption'
          'clMenuHighlight=Menu Highlight'
          'clMenuBar=MenuBar'
          'clNone=None'
          'clDefault=Default')
        ColorDialogText = 'Custom...'
        DroppedDownWidth = 113
        NewColorText = 'Custom'
        TabOrder = 6
      end
      object CheckBox15: TCheckBox
        Left = 176
        Top = 200
        Width = 97
        Height = 17
        Caption = 'Bold'
        TabOrder = 7
      end
      object Button5: TButton
        Left = 296
        Top = 120
        Width = 90
        Height = 22
        Caption = 'Custom...'
        TabOrder = 8
      end
      object Button6: TButton
        Left = 296
        Top = 168
        Width = 90
        Height = 22
        Caption = 'Custom...'
        TabOrder = 9
      end
    end
    object pgHelp: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - Help'
    end
    object pgInternational: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - International Settings'
    end
    object pgKeyboard: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - Keyboard'
    end
    object pgProjSolutions: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - Projects and Solutions'
    end
    object pgTaskList: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - Task List'
    end
    object pgWebBrowser: TJvStandardPage
      Left = 0
      Top = 17
      Width = 413
      Height = 303
      Caption = 'Environment - Web Browser'
    end
    object JvGroupHeader1: TJvGroupHeader
      Left = 0
      Top = 320
      Width = 413
      Height = 2
      Align = alBottom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      BevelSpace = 4
    end
    object JvGroupHeader2: TJvGroupHeader
      Left = 0
      Top = 0
      Width = 413
      Height = 17
      Align = alTop
      Caption = 'Settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      BevelSpace = 4
    end
  end
  object ImageList1: TImageList
    Left = 72
    Top = 104
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
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
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000000000C6C6
      C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6
      C60000FFFF0084848400000000000000000000000000848484000000000000FF
      FF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000848484000000000000FF
      FF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FF
      FF00C6C6C600848484000000000000000000000000008484840000000000C6C6
      C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF008484
      8400000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000000000C6C6
      C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6
      C60000FFFF008484840000000000000000008484840000000000C6C6C60000FF
      FF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C6000000
      0000848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000848484000000000000FF
      FF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FF
      FF00C6C6C6008484840000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF008484840000000000000000000000000000000000000000000000
      0000848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000000000C6C6
      C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6
      C60000FFFF008484840000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400C6C6C6008484840000000000000000000000000000000000000000000000
      00008484840084848400C6C6C60084848400C6C6C60084848400C6C6C6008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000848484000000000000FF
      FF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FF
      FF00C6C6C600848484000000000000000000000000008484840000000000C6C6
      C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6
      C60000FFFF008484840000000000000000000000000000000000000000000000
      000084848400C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400000000000000000000000000848484000000000000FF
      FF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      000084848400848484008484840084848400C6C6C600C6C6C600848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400C6C6C60000FF
      FF00C6C6C60000FFFF00C6C6C60000FFFF00C6C6C60084848400848484008484
      8400848484008484840000000000000000000000000084848400000000000000
      000000FFFF00C6C6C60000FFFF00000000000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      000000000000000000000000000084848400C6C6C60084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C60000FFFF00C6C6C60000FFFF00C6C6C6008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
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
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      C001E001FFFF00008001C001FEFF0000A001A001FE7F0000A001A001FE3F0000
      A0014001F01F0000A0017FE1F00F0000A0010001F0070000A001A001F00F0000
      BFF9A079F01F00008003B183FE3F0000C07FDF7FFE7F0000E0FFE0FFFEFF0000
      FFFFFFFFFFFF0000FFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 104
    Top = 104
  end
  object ImageList2: TImageList
    Left = 80
    Top = 176
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
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
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000084000000C6C6C60000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000084848400FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000840000008400000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6008484840000000000000000000000000084848400FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000840000008400000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6008484840000000000000000000000000084848400FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000840000008400000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600000000000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C6008484840000000000000000000000000084848400FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000840000008400000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400C6C6C6000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C6008484840000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C6C6C6008400000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400FFFFFF000000000000000000FFFFFF00000000000000000000000000FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400C6C6C6000000000000000000C6C6C600000000000000000000000000C6C6
      C600C6C6C60084848400000000000000000084848400C6C6C600FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000FFFFFF008484840000000000000000000000000000000000000000008484
      8400C6C6C60000000000C6C6C600C6C6C600C6C6C60000000000000000000000
      0000C6C6C60084848400000000000000000084848400C6C6C60084848400C6C6
      C600C6C6C600C6C6C6000000000000FF0000FFFF0000C6C6C600C6C6C6000000
      0000000000008484840000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000FFFFFF008484840000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600000000000000
      0000C6C6C600848484000000000000000000000000008484840084848400C6C6
      C600C6C6C60000FF0000FFFFFF0000000000C6C6C600C6C6C600C6C6C6000000
      0000848484000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF008484840000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000C6C6C600848484000000000000000000000000000000000084848400C6C6
      C60000FF00008484000084840000C6C6C600FFFFFF00C6C6C600C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008484840000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6008484840000000000000000000000000000000000C6C6C60000FF
      0000FFFF0000FFFF000000FFFF00C6C6C600C6C6C600FFFFFF0000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000008484
      84008484000000FFFF0000FFFF00C6C6C600C6C6C6008484840000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400848484008484840000000000848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000008007FFFFFFFFFFFF8003FFFFFFFFFFFF
      8001FFFFFFFFFFFF8001E003E003E0038001E003E003E0038001E003E003E003
      8001E003E003E0038001E003E003E0038001E003E003E0030001E003E003E003
      0000E003E003E0038001E003E003E003C007E003E003E003C007E003E003E003
      E007FFFFFFFFFFFFF007FFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end
