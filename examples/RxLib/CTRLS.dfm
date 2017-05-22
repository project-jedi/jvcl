object ControlsForm: TControlsForm
  Left = 231
  Top = 142
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'RX Controls'
  ClientHeight = 313
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = True
  Position = poDefaultPosOnly
  ShowHint = True
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabbedNotebook1: TTabbedNotebook
    Left = 0
    Top = 0
    Width = 504
    Height = 313
    Align = alClient
    PageIndex = 3
    TabsPerRow = 4
    TabFont.Charset = DEFAULT_CHARSET
    TabFont.Color = clBtnText
    TabFont.Height = -11
    TabFont.Name = 'MS Sans Serif'
    TabFont.Style = []
    TabOrder = 0
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Misc'
      object GroupBox1: TGroupBox
        Left = 4
        Top = 0
        Width = 237
        Height = 105
        Caption = ' ColorComboBox '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 16
          Width = 225
          Height = 13
          AutoSize = False
          Caption = 'Allows to choose color from a drop-down list'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Shape1: TShape
          Left = 8
          Top = 64
          Width = 221
          Height = 33
          Brush.Color = clLime
          Shape = stRoundRect
        end
        object CheckBox4: TCheckBox
          Left = 143
          Top = 38
          Width = 86
          Height = 17
          Caption = 'Show names'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          State = cbChecked
          TabOrder = 0
          OnClick = CheckBox4Click
        end
        object ColorComboBox1: TJvColorComboBox
          Left = 8
          Top = 36
          Width = 129
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
          ColorValue = clLime
          ColorDialogText = '(Other...)'
          DroppedDownWidth = 129
          NewColorText = 'Custom'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnChange = ColorComboBox1Change
        end
      end
      object GroupBox2: TGroupBox
        Left = 244
        Top = 0
        Width = 245
        Height = 105
        Caption = ' FontComboBox '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object Label2: TLabel
          Left = 8
          Top = 16
          Width = 229
          Height = 13
          AutoSize = False
          Caption = 'Allows to choose font from a drop-down list'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label3: TLabel
          Left = 8
          Top = 64
          Width = 41
          Height = 13
          AutoSize = False
          Caption = 'Show:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object FontComboBox1: TJvFontComboBox
          Left = 8
          Top = 36
          Width = 145
          Height = 22
          DroppedDownWidth = 145
          MaxMRUCount = 0
          FontName = 'Courier'
          Device = fdPrinter
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemIndex = 0
          ParentFont = False
          Sorted = False
          TabOrder = 0
          OnChange = FontComboBox1Change
        end
        object CheckBox1: TCheckBox
          Left = 8
          Top = 80
          Width = 105
          Height = 17
          Caption = '&TrueType only'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = CheckBox1Click
        end
        object ComboBox1: TComboBox
          Left = 128
          Top = 76
          Width = 109
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 2
          OnChange = ComboBox1Change
          Items.Strings = (
            'Screen Fonts'
            'Printer Fonts'
            'All Fonts')
        end
      end
      object GroupBox3: TGroupBox
        Left = 4
        Top = 108
        Width = 237
        Height = 169
        Caption = ' JvxSlider '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        object ADHRuler: TImage
          Left = 24
          Top = 104
          Width = 16
          Height = 6
          AutoSize = True
          Picture.Data = {
            07544269746D6170A6000000424DA60000000000000076000000280000001000
            0000060000000100040000000000300000000000000000000000100000000000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00DDDDDDDDDDDDDDDD00000000000000000DDDDDDDDDDDDDD00DDDDDDDDDDD
            DDD00000000000000000DDDDDDDDDDDDDDDD}
          Visible = False
        end
        object Label4: TLabel
          Left = 8
          Top = 16
          Width = 217
          Height = 13
          AutoSize = False
          Caption = 'Slider with customizable look and feel'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label5: TLabel
          Left = 104
          Top = 32
          Width = 73
          Height = 13
          AutoSize = False
          Caption = '&Orientation:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 12
          Top = 144
          Width = 69
          Height = 13
          AutoSize = False
          Caption = 'Value: 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label7: TLabel
          Left = 104
          Top = 116
          Width = 53
          Height = 13
          AutoSize = False
          Caption = 'O&ptions:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 104
          Top = 76
          Width = 77
          Height = 13
          AutoSize = False
          Caption = '&Looks like in:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object ADHThumb: TImage
          Left = 28
          Top = 98
          Width = 10
          Height = 16
          AutoSize = True
          Picture.Data = {
            07544269746D6170F6040000424DF60400000000000036040000280000000A00
            0000100000000100080000000000C00000000000000000000000000100000000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000C0DCC000F0CAA60000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000F0FBFF00A4A0
            A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF0003000000000000000003000000F8F8F8F8F8F8F8F800000000FF0707FF07
            F8070700000000FF0707FF07F8070700000000FF0707FF07F8070700000000FF
            0707FF07F8070700000000FF0707FF07F8070700000000FF0707FF07F8070700
            000000FF0707FF07F8070700000000FF0707FF07F8070700000000FF0707FF07
            F8070700000000FF0707FF07F8070700000000FF0707FF07F8070700000000FF
            0707FF07F8070700000000FFFFFFFFFFFFFFFF00000003000000000000000003
            0000}
          Visible = False
        end
        object ADVThumb: TImage
          Left = 48
          Top = 100
          Width = 16
          Height = 10
          AutoSize = True
          Picture.Data = {
            07544269746D6170D6040000424DD60400000000000036040000280000001000
            00000A0000000100080000000000A00000000000000000000000000100000000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000C0DCC000F0CAA60000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000F0FBFF00A4A0
            A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF000300000000000000000000000000000300FF070707070707070707070707
            FF0000FF070707070707070707070707FF0000FFF8F8F8F8F8F8F8F8F8F8F8F8
            FF0000FF070707070707070707070707FF0000FFFFFFFFFFFFFFFFFFFFFFFFFF
            FF0000FF070707070707070707070707FF0000FF070707070707070707070707
            FF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF000300000000000000000000000000
            0003}
          Visible = False
        end
        object ADVRuler: TImage
          Left = 52
          Top = 96
          Width = 6
          Height = 16
          AutoSize = True
          Picture.Data = {
            07544269746D6170B6000000424DB60000000000000076000000280000000600
            0000100000000100040000000000400000000000000000000000100000000000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00D0000D00D0DD0D00D0DD0D00D0DD0D00D0DD0D00D0DD0D00D0DD0D00D0DD
            0D00D0DD0D00D0DD0D00D0DD0D00D0DD0D00D0DD0D00D0DD0D00D0DD0D00D000
            0D00}
          Visible = False
        end
        object rxSlider1: TJvxSlider
          Left = 4
          Top = 36
          Width = 97
          Height = 33
          MaxValue = 40
          TabOrder = 0
          OnChange = rxSlider1Change
        end
        object ComboBox2: TComboBox
          Left = 104
          Top = 48
          Width = 125
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 1
          OnChange = ComboBox2Change
          Items.Strings = (
            'Horizontal'
            'Vertical')
        end
        object CheckBox2: TCheckBox
          Left = 104
          Top = 132
          Width = 129
          Height = 17
          Caption = 'Smooth'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          State = cbChecked
          TabOrder = 3
          OnClick = CheckBox2Click
        end
        object CheckBox3: TCheckBox
          Left = 104
          Top = 148
          Width = 125
          Height = 17
          Caption = 'Show Tick Marks'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          State = cbChecked
          TabOrder = 4
          OnClick = CheckBox3Click
        end
        object ComboBox3: TComboBox
          Left = 104
          Top = 92
          Width = 125
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 2
          OnChange = ComboBox3Change
          Items.Strings = (
            'Windows 95'
            'After Dark')
        end
      end
      object GroupBox4: TGroupBox
        Left = 244
        Top = 108
        Width = 245
        Height = 169
        Caption = ' JvSwitch '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        object Label9: TLabel
          Left = 8
          Top = 16
          Width = 225
          Height = 13
          AutoSize = False
          Caption = 'Switch control with customizable look'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label10: TLabel
          Left = 112
          Top = 32
          Width = 37
          Height = 13
          AutoSize = False
          Caption = 'Te&xt:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label11: TLabel
          Left = 112
          Top = 76
          Width = 73
          Height = 13
          AutoSize = False
          Caption = 'Looks like in:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object SwOff: TImage
          Left = 36
          Top = 120
          Width = 22
          Height = 34
          AutoSize = True
          Picture.Data = {
            07544269746D617066070000424D660700000000000036040000280000001600
            0000220000000100080000000000300300000000000000000000000100000000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000C0DCC000F0CAA60000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000F0FBFF00A4A0
            A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00030303030303030303030303030303030303030303030000030303030303
            0303030303030303030303030303030300000303000000000000000000000000
            00000000000003030000030300F8070707070707070707070707070707000303
            0000030300F807F8000000000000000000000000070003030000030300F807F8
            F8F8F8F8F8F8F8F8F8F8F800070003030000030300F807F8FFF8F8F8F8F8F8F8
            F8F8F800070003030000030300F807F8FF0707070707070707F8F80007000303
            0000030300F807F8FF0707070707070707F8F800070003030000030300F807F8
            FF0707070707070707F8F800070003030000030300F807F8FF07070707070707
            07F8F800070003030000030300F807F8FF0707070707070707F8F80007000303
            0000030300F807F8FF0707070707070707F8F800070003030000030300F807F8
            FF0707070707070707F8F800070003030000030300F807F8FF07070707070707
            07F8F800070003030000030300F807F8FFFFFFFFFFFFFFFFFFFFF80007000303
            0000030300F807F8F8F8F8F8F8F8F8F8F8F8F8F8070003030000030300F80707
            070707070707070707070707070003030000030300F807070707070707070707
            07070707070003030000030300F8070707070707070707070707070707000303
            0000030300F80707070707070707070707070707070003030000030300F80707
            070707070707070707070707070003030000030300F807070707070707070707
            07070707070003030000030300F8070707070707070707070707070707000303
            0000030300F80707070707070707070707070707070003030000030300F80707
            070707070707070707070707070003030000030300F807070707070707070707
            07070707070003030000030300F8070707070707070707070707070707000303
            0000030300F80707070707070707070707070707070003030000030300F8F8F8
            F8F8F8F8F8F8F8F8F8F8F8F8F800030300000303000000000000000000000000
            0000000000000303000003030000000000000000000000000000000000000303
            0000030303030303030303030303030303030303030303030000030303030303
            030303030303030303030303030303030000}
          Visible = False
        end
        object SwOn: TImage
          Left = 16
          Top = 120
          Width = 22
          Height = 34
          AutoSize = True
          Picture.Data = {
            07544269746D617066070000424D660700000000000036040000280000001600
            0000220000000100080000000000300300000000000000000000000100000000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000C0DCC000F0CAA60000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000F0FBFF00A4A0
            A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00030303030303030303030303030303030303030303030000030303030303
            0303030303030303030303030303030300000303000000000000000000000000
            00000000000003030000030300F8070707070707070707070707070707000303
            0000030300F80707070707070707070707070707070003030000030300F80707
            070707070707070707070707070003030000030300F807070707070707070707
            07070707070003030000030300F8070707070707070707070707070707000303
            0000030300F80707070707070707070707070707070003030000030300F80707
            070707070707070707070707070003030000030300F807070707070707070707
            07070707070003030000030300F8070707070707070707070707070707000303
            0000030300F80707070707070707070707070707070003030000030300F80707
            070707070707070707070707070003030000030300F807070707070707070707
            07070707070003030000030300F8070707070707070707070707070707000303
            0000030300F807F8000000000000000000000000070003030000030300F807F8
            F8F8F8F8F8F8F8F8F8F8F800070003030000030300F807F8FFF8F8F8F8F8F8F8
            F8F8F800070003030000030300F807F8FF0707070707070707F8F80007000303
            0000030300F807F8FF0707070707070707F8F800070003030000030300F807F8
            FF0707070707070707F8F800070003030000030300F807F8FF07070707070707
            07F8F800070003030000030300F807F8FF0707070707070707F8F80007000303
            0000030300F807F8FF0707070707070707F8F800070003030000030300F807F8
            FF0707070707070707F8F800070003030000030300F807F8FFFFFFFFFFFFFFFF
            FFFFF800070003030000030300F807F8F8F8F8F8F8F8F8F8F8F8F8F807000303
            0000030300F80707070707070707070707070707070003030000030300F8F8F8
            F8F8F8F8F8F8F8F8F8F8F8F8F800030300000303000000000000000000000000
            0000000000000303000003030000000000000000000000000000000000000303
            0000030303030303030303030303030303030303030303030000030303030303
            030303030303030303030303030303030000}
          Visible = False
        end
        object rxSwitch1: TJvSwitch
          Left = 12
          Top = 44
          Width = 77
          Height = 77
          Caption = 'On'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          StateOn = True
          TabOrder = 0
          TextPosition = tpRight
          OnOn = rxSwitchOn
          OnOff = rxSwitch1Off
        end
        object ComboBox4: TComboBox
          Left = 112
          Top = 48
          Width = 125
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 1
          OnChange = ComboBox4Change
          Items.Strings = (
            'None'
            'Left'
            'Right'
            'Above'
            'Below')
        end
        object ComboBox5: TComboBox
          Left = 112
          Top = 92
          Width = 125
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 2
          OnChange = ComboBox5Change
          Items.Strings = (
            'Switch.VBX'
            'After Dark')
        end
        object CheckBox5: TCheckBox
          Left = 112
          Top = 132
          Width = 97
          Height = 17
          Caption = 'Show Border'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = CheckBox5Click
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Edits'
      object GroupBox5: TGroupBox
        Left = 4
        Top = 0
        Width = 241
        Height = 60
        Caption = ' ComboEdit '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label13: TLabel
          Left = 9
          Top = 14
          Width = 197
          Height = 13
          Caption = 'Edit Box with a button (for special actions)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object ComboEdit1: TJvComboEdit
          Left = 8
          Top = 33
          Width = 225
          Height = 21
          ButtonFlat = False
          ButtonHint = 'Special action...|'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ImageKind = ikEllipsis
          ParentFont = False
          TabOrder = 0
          OnButtonClick = ComboEdit1ButtonClick
        end
      end
      object GroupBox6: TGroupBox
        Left = 248
        Top = 0
        Width = 241
        Height = 77
        Caption = ' DateEdit and CalcEdit '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object Label14: TLabel
          Left = 8
          Top = 16
          Width = 225
          Height = 33
          AutoSize = False
          Caption = 
            'Provide masked typing and brings up calendar or calculator on bu' +
            'tton click'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object DateEdit1: TJvDateEdit
          Left = 8
          Top = 48
          Width = 93
          Height = 21
          ButtonHint = 'Calendar|'
          ButtonFlat = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object RxCalcEdit1: TJvCalcEdit
          Left = 112
          Top = 48
          Width = 121
          Height = 21
          AutoSize = False
          ButtonHint = 'Calculator|'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
      end
      object GroupBox7: TGroupBox
        Left = 4
        Top = 60
        Width = 241
        Height = 61
        Caption = ' FileNameEdit '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        object Label15: TLabel
          Left = 8
          Top = 14
          Width = 161
          Height = 13
          Caption = 'Direct typing or call to OpenDialog'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object FilenameEdit1: TJvFilenameEdit
          Left = 8
          Top = 33
          Width = 225
          Height = 21
          AddQuotes = False
          Filter = 'Pascal sources (*.pas)|*.pas|All files (*.*)|*.*'
          ButtonHint = 'Browse...|'
          ButtonFlat = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
      end
      object GroupBox8: TGroupBox
        Left = 248
        Top = 80
        Width = 241
        Height = 65
        Caption = ' DirectoryEdit '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        object Label16: TLabel
          Left = 8
          Top = 16
          Width = 165
          Height = 13
          AutoSize = False
          Caption = 'Direct typing or Browse'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object DirectoryEdit1: TJvDirectoryEdit
          Left = 8
          Top = 36
          Width = 224
          Height = 21
          ButtonHint = 'Browse...|'
          ButtonFlat = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
      end
      object GroupBox9: TGroupBox
        Left = 4
        Top = 121
        Width = 241
        Height = 79
        Caption = ' CurrencyEdit '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        object Label17: TLabel
          Left = 8
          Top = 14
          Width = 157
          Height = 13
          Caption = 'Edit Box for moneys and numbers'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object Label22: TLabel
          Left = 116
          Top = 37
          Width = 45
          Height = 13
          AutoSize = False
          Caption = 'Format:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object CurrencyEdit1: TJvValidateEdit
          Left = 8
          Top = 33
          Width = 97
          Height = 21
          AutoSize = False
          CheckChars = '01234567890'
          CriticalPoints.CheckPoints = cpNone
          CriticalPoints.ColorAbove = clBlue
          CriticalPoints.ColorBelow = clRed
          EditText = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          PasswordChar = #0
          TabOrder = 0
          Text = '0'
          Value = 0
        end
        object Edit1: TEdit
          Left = 164
          Top = 33
          Width = 69
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Text = ',0.00;-,0.00'
          Visible = False
          OnChange = Edit1Change
        end
        object CheckBox7: TCheckBox
          Left = 8
          Top = 56
          Width = 109
          Height = 17
          Caption = ' Format on editing '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          Visible = False
          OnClick = CheckBox7Click
        end
      end
      object GroupBox10: TGroupBox
        Left = 248
        Top = 148
        Width = 241
        Height = 129
        Caption = ' RxLabel '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        object Label18: TLabel
          Left = 8
          Top = 16
          Width = 109
          Height = 13
          AutoSize = False
          Caption = 'Label with shadow'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object rxLabel1: TJvLabel
          Left = 8
          Top = 36
          Width = 82
          Height = 13
          Caption = 'Shadowed Label'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          AutoOpenURL = False
          HotTrackFont.Charset = DEFAULT_CHARSET
          HotTrackFont.Color = clWindowText
          HotTrackFont.Height = -11
          HotTrackFont.Name = 'MS Sans Serif'
          HotTrackFont.Style = []
          ImageIndex = 0
        end
        object Label23: TLabel
          Left = 120
          Top = 12
          Width = 101
          Height = 13
          AutoSize = False
          Caption = 'Shadow Color:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label24: TLabel
          Left = 120
          Top = 52
          Width = 117
          Height = 13
          AutoSize = False
          Caption = 'Shadow Position:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label25: TLabel
          Left = 120
          Top = 100
          Width = 81
          Height = 13
          AutoSize = False
          Caption = 'Shadow Size:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object ComboBox6: TComboBox
          Left = 120
          Top = 68
          Width = 113
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 0
          OnChange = ComboBox6Change
          Items.Strings = (
            'Left-Top'
            'Left-Bottom'
            'Right-Bottom'
            'Right-Top')
        end
        object SpinEdit2: TSpinEdit
          Left = 200
          Top = 96
          Width = 33
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxValue = 15
          MinValue = 0
          ParentFont = False
          TabOrder = 1
          Value = 1
          OnChange = SpinEdit2Change
        end
        object Button1: TButton
          Left = 8
          Top = 88
          Width = 97
          Height = 25
          Caption = 'Font...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = Button1Click
        end
        object ColorComboBox2: TJvColorComboBox
          Left = 120
          Top = 29
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
          ColorValue = clWhite
          ColorDialogText = '(Other...)'
          DroppedDownWidth = 113
          NewColorText = 'Custom'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnChange = ColorComboBox2Change
        end
      end
      object GroupBox16: TGroupBox
        Left = 4
        Top = 200
        Width = 241
        Height = 77
        Caption = ' RxSpinEdit '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        object Label19: TLabel
          Left = 8
          Top = 14
          Width = 168
          Height = 13
          Caption = 'Edit Box with Up and Down buttons'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object Label20: TLabel
          Left = 100
          Top = 30
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'ValueType:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label21: TLabel
          Left = 178
          Top = 30
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Increment:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object rxSpinEdit1: TJvSpinEdit
          Left = 8
          Top = 47
          Width = 81
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object ComboBox9: TComboBox
          Left = 100
          Top = 47
          Width = 65
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 1
          OnChange = ComboBox9Change
          Items.Strings = (
            'Integer'
            'Float')
        end
        object SpinEdit1: TSpinEdit
          Left = 179
          Top = 47
          Width = 49
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxValue = 100
          MinValue = 0
          ParentFont = False
          TabOrder = 2
          Value = 1
          OnChange = SpinEdit1Change
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Splitter'
      object GroupBox11: TGroupBox
        Left = 0
        Top = 0
        Width = 496
        Height = 285
        Align = alClient
        Caption = ' RxSplitter '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label26: TLabel
          Left = 8
          Top = 16
          Width = 481
          Height = 33
          AutoSize = False
          Caption = 
            'Line or bar separating two controls. Controls can be horizontall' +
            'y or vertically resized by dragging the splitter. Two splitters ' +
            'are placed between Directory List, File List and  Quick View.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object Label27: TLabel
          Left = 4
          Top = 53
          Width = 29
          Height = 13
          AutoSize = False
          Caption = 'Disk:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label28: TLabel
          Left = 196
          Top = 56
          Width = 77
          Height = 13
          AutoSize = False
          Caption = 'Quick View:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Panel1: TPanel
          Left = 2
          Top = 75
          Width = 492
          Height = 208
          Align = alBottom
          BevelOuter = bvNone
          Caption = 'Panel1'
          TabOrder = 0
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 185
            Height = 208
            Align = alLeft
            BevelOuter = bvNone
            Caption = 'Panel2'
            TabOrder = 0
            object DirectoryListBox1: TDirectoryListBox
              Left = 0
              Top = 0
              Width = 185
              Height = 97
              Align = alTop
              FileList = FileListBox1
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ItemHeight = 16
              ParentFont = False
              TabOrder = 0
            end
            object rxSplitter2: TJvxSplitter
              Left = 0
              Top = 97
              Width = 185
              Height = 3
              ControlFirst = DirectoryListBox1
              ControlSecond = FileListBox1
              Align = alTop
              BevelOuter = bvLowered
              TopLeftLimit = 16
              BottomRightLimit = 16
            end
            object FileListBox1: TFileListBox
              Left = 0
              Top = 100
              Width = 185
              Height = 108
              Align = alClient
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ItemHeight = 13
              ParentFont = False
              TabOrder = 2
              OnChange = FileListBox1Change
            end
          end
          object Memo1: TMemo
            Left = 188
            Top = 0
            Width = 304
            Height = 208
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Lines.Strings = (
              '')
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
          end
          object rxSplitter1: TJvxSplitter
            Left = 185
            Top = 0
            Width = 3
            Height = 208
            ControlFirst = Panel2
            ControlSecond = Memo1
            Align = alLeft
            BevelInner = bvLowered
            BevelOuter = bvNone
          end
        end
        object DriveComboBox1: TDriveComboBox
          Left = 36
          Top = 48
          Width = 151
          Height = 19
          DirList = DirectoryListBox1
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Gadgets'
      object GroupBox12: TGroupBox
        Left = 4
        Top = 0
        Width = 233
        Height = 125
        Caption = 'AnimatedImage'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label30: TLabel
          Left = 8
          Top = 16
          Width = 61
          Height = 13
          AutoSize = False
          Caption = 'Animation'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object RunnerImage: TImage
          Left = 64
          Top = 39
          Width = 33
          Height = 37
          Picture.Data = {
            07544269746D617076180000424D761800000000000076000000280000008001
            0000200000000100040000000000001800000000000000000000100000001000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777707777777777777777700000777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777770000077777
            7777777777777700000077777777777777777777777700077777777777777777
            7777777777000777777777777777777777777777777777777777777777777777
            7777777777777777777777777777700007777777777777777777777770000007
            7777777777777777777777777777777777777777770000777777777777777777
            777777770077777777777777770F700777777777770777777777777777777777
            7777777707777777777777777700000777777777777777777777772270777777
            7777777777777707FF07777777777777777777777700F0777777777777777777
            7777777777007777777777777777777777777777077777777777777777770000
            77777777777777777777777777700F7077777777777777777777777772FF0777
            7777777777777777777777777777777777777777707707777777777777777777
            7777777707077777777777777742277777777777770077777777777777000077
            777777770077777777777777770F700077777777777777777777772227777777
            7777777777777777227777777777777777777777770F07777777777777777777
            77777777770F277777777777770000777777777700777777777777777700F707
            7777777777777777777777777772220777777777777777777777777772227777
            777777777777777770000777777777777777777770F277777777777777777777
            777777770F222229777777777222277777777777770F27777777777700F70777
            7777777707077777777777777722277777777777777777777777772227777777
            777777777777777222B777777777777777777777770222777770077777777777
            77777777770F22277777777700FF077777777777070777777777777777022277
            7777777777777777777777777772227777777777777777777777777772227777
            77777777777777700FF077777777777777777777702227777700077777777777
            777777770022229B9B7777777B92277777777777770722797777777702227777
            777777770F2222297777777777222777777777777777777777777B9B97777777
            77777777777707772F977777777777777777777777722229770F077777777777
            777777777702222977777777022277777777777770F222777777777777722277
            7777777777777777777777777772227777777777777777777777777772227777
            7777777777777770FF2777777777777777777777772222277070777777777777
            7777777777772279F977777779F97777777777777700222B9B97777772222777
            777777770022222F97777777779227777777777777777777777779F977777777
            777777777777007700B9B97777777777777777777777229F00F0777777777777
            777777777777722B9B9B7777722227777777777770F2222B9B77777777722277
            777777770777777797977777779797777777777777777777779B97779B9B7777
            7777777777777777222277777777777777777777777222970022777777777777
            777777777777777B9B9B7777779B97777777777777777229F9F9777777222777
            77777777777722B9B9B7777779B9B77777777777777777779B977B9B97777777
            7777777777770F02229F9F977777777777777777777772B90002277777777777
            7777777777777779F9F97777772227777777777777022229F97977777779F977
            7777777700777729B9B9777777B9B977777777777777777779F9777779F97777
            77777777777777777222777777777777777777777777B9B9BF22227777777777
            777777777777777779F9777779F97777777777777777777B9B9B9777779B9B77
            77777777777777779F977777779F97777777777777077779F977777977777777
            77777777777770F222B9B9B777777777777777777777779790022F9777777777
            777777777777777B9B9B9B77779B9B77777777777777722B9B9B977777779B97
            777777770702222F9F977777779F977777777777777077779B9B97777B9B7777
            7777777777777700779B977777777777777777777777779F9FF2229777777777
            77777777777777777B9B999B9B9B9777777777777777777779F999777779F977
            7777777777777777B9B9B777B9B9B777777777777700722B9B909B9B97777777
            77777777777770722F9F9777777777777777777777777777B9B229B9B7777777
            777777777777777779F9F9777779F977777777777777777779F9F9777939F977
            7777777770F22222B9B9B779B9B9B9777777777777700772F9F9977979F97777
            777777777777700772F9F977777777777777777777777779B9BF29B9B7777777
            777777777777777779FC9999F97977777777777777777777779C99199B9B9B77
            7777777777777777779F99979F9F97777777777777070222F9F9F9F977777777
            777777777777700FF999B9777777777777777777777777779F9FFF9F97777777
            7777777777777777779B9B779B9B9B777777777777777777779B99999B9B9B77
            7777777777000777779999999F9F9777777777777770F2222299999B9B977777
            77777777777770F2229F9B977777777777777777777777799C999F9F97777777
            77777777777777777799C9999977777777777777777777777799C99999F9F777
            777777777777777779BC9999B9B97777777777777707F2299B999B9777777777
            777777777777777999CC9777777777777777777777777777999999C9B9777777
            77777777777777777779F999F9F97777777777777777777777799999F9F97777
            7777777777777777799999CCC9B97777777777777770FF222F999C99F9777777
            77777777777770F222F9F9F97777777777777777777777999C99B9B977777777
            777777777777777777999C9997777777777777777777777777999C99999B7777
            77777777777777777799C9999977777777777777777000779C99997777777777
            77777777777777999C000777777777777777777777777779999CCC9797777777
            7777777777777777779999CC9B9B77777777777777777777777999CCCB9B7777
            777777777777777779999C99997777777777777777770FF07999C99997777777
            7777777777777000FF999B9777777777777777777777779999C9979777777777
            777777777777777777799C999777777777777777777777777779993399777777
            777777777777777777999C999777777777777777777777799C99977777777777
            77777777777777999F9F977777777777777777777777777999B9999777777777
            777777777777777777999C9999777777777777777777777777799CC999777777
            77777777777777777999B9999777777777777777777770077999F9F9F9777777
            7777777777777777999C997777777777777777777777779999C9977777777777
            777777777777777777773B39F9777777777777777777777777733BB397777777
            777777777777777777799C999777777777777777777777799C99F97977777777
            7777777777777799B9B9B9777777777777777777777777779FB99F9777777777
            777777777777777777999C9997777777777777777777777777779C9997777777
            7777777777777777779797BB379777777777777777777777799B9B999B977777
            777777777777777999C99B9B7777777777777777777777999BBBB77777777777
            777777777777777B9777BBBB9B9B777777777777777777777977BBB9F9777777
            7777777777777777777733B9B9B777777777777777777779B39B9B9B97777777
            77777777777777779F9B97777777777777777777777777B9B9BBB9B9B9777777
            777777777777777779FBBBBB977977777777777777777777777793BBB3797777
            777777777777777777B9B9BBB9B9B777777777777777777779F9F97779777777
            777777777777777999F9F9F977777777777777777777B9BBBBB9B9B9B9777777
            77777777777777797773BBF9F9F97777777777777777777B9B77BBBB9B9B7777
            77777777777777779777BBBF9F9777777777777777777777BBF9F97977777777
            7777777777777777B9BBB77777777777777777777777779F9FBBBF9F97777777
            77777777777777779B9BBBBBBB9B97777777777777777777779B73BBBB339B77
            7777777777777777779F97BFB39F97777777777777777777779B9BBB77777777
            77777777777777799B9B9B9B977777777777777777779F9FBB9F9F979F977777
            777777777777779B977BBB9B9F9B97777777777777777779F973BBB9F9797777
            77777777777777B9B773BBB9B9B9B777777777777777777BBB9B9B7B97777777
            7777777777777777BF9FBB7777777777777777777777B9B9BBBBBB79B7777777
            7777777777777779F9FBBBBBB9F97777777777777777777779F973BBBB39F977
            777777777777777779B93BBBBB79B977777777777777777779F9BBBBB7777777
            777777777777777739F93B7777777777777777777777B9BFBBB9BB77B9B77777
            7777777777777779777BBBF9F3797777777777777777777B9773BBBB9B7B9B77
            777777777777779F977BBB9F9F9F97777777777777777779BBF9FB7777777777
            7777777777777779B9B9BB77777777777777777777779F9FBBBBBB7777777777
            777777777777777B9FFBBBBBBB9B9B7777777777777777777B9773BBBBB39B9B
            77777777777777779F9773BFBB77777777777777777777777B9B9BBB9B777777
            7777777777777777BB9BBBB7777777777777777777779F9FBB9F9B7777777777
            777777777777779B9B9BBB9B9B7777777777777777777779F973BBF9F979F977
            77777777777777B9777BBBB9B379B7777777777777777B9BBB9B9B9777777777
            7777777777777777BB9F9F9777777777777777777777B9B9B9BBBBB777777777
            7777777777777779F9F9F9BBBB79F977777777777777777739B7773BBBBB79F9
            7777777777777777B9B9B9BBB377777777777777777777777779F9BBF9777777
            7777777777777777BBF9FBBB77777777777777777777B9B9BBB9BBB777777777
            777777777777777979FBBBF9BB777777777777777777777B9B9BBB9B9C779B77
            777777777777779F979BBB9F9B77777777777777777779F9BBF9FB7777777777
            7777777777777777BBB9B9B977777777777777777777779F9F9FBB9777777777
            777777777777779B9B9B9B9BBB7B9B7777777777777777779B97773BBB937B9B
            7777777777777777779F9F9BB79777777777777777777777777B9B9B9B9B7777
            77777777777777777BBB9BBBB7777777777777777777779F9B9F9BB777777777
            77777777777777777B9BBBBBB39B7777777777777777777779BBBBB9FC777777
            7777777777777779B9BBBBB9BBB77777777777777777779B9BBB939B97777777
            77777777777777777BBB0F9F977777777777777777777779B9B9BBB9B7777777
            77777777777777797979F9FBB9777777777777777777777739B9B9F9FBF97777
            77777777777777777779B9BB39B9377777777777777777777777797BF9F97777
            77777777777777777BB9F9B9777777777777777777777779BBB9BBB777777777
            7777777777777777777777B309F9777777777777777777777777BBB3BB977777
            77777777777777777797BBBB3F977777777777777777777777BBB0F9F9777777
            7777777777777777777700B9B777777777777777777777777797BF9F97777777
            7777777777777777777B9BBBBB9B777777777777777777779B9B9B9B9B9B9377
            77777777777777777777779B979F9777777777777777777777777777009B9777
            777777777777777777BB9BB39B77777777777777777777777BBBBF9777777777
            777777777777777777777777009B9777777777777777777777777770F9F97777
            7777777777777777777777BB09B9B77777777777777777777777700B9B777777
            77777777777777777777000007777777777777777777777777777009B9777777
            77777777777777777777777779F97777777777777777777779393977C9F9F977
            7777777777777777777777777089B97777777777777777777777777700000777
            7777777777777777777BBBB9F977777777777777777777777770B9B977777777
            777777777777777777777777000007777777777777777777777777700B9B7777
            777777777777777777777777009F977777777777777777777777700000777777
            7777777777777777777770007777777777777777777777777777700000777777
            777777777777777777777777009B977777777777777777777777777777009B97
            7777777777777777777777777088807777777777777777777777777770007777
            7777777777777777777777009B977777777777777777777777700F9777777777
            7777777777777777777777777000777777777777777777777777777000007777
            7777777777777777777777770000077777777777777777777777770007777777
            7777777777777777777777777777777777777777777777777777770007777777
            7777777777777777777777770000077777777777777777777777777777000007
            7777777777777777777777777700077777777777777777777777777777777777
            7777777777777777777777000007777777777777777777777770000077777777
            7777777777777777777777777777777777777777777777777777777700077777
            7777777777777777777777777000777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777000777777777777777777777777777777700077
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777700077777777777777777777777777000777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777}
          Visible = False
        end
        object Label31: TLabel
          Left = 104
          Top = 40
          Width = 53
          Height = 13
          AutoSize = False
          Caption = 'Picture:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label32: TLabel
          Left = 104
          Top = 80
          Width = 49
          Height = 13
          AutoSize = False
          Caption = 'Interval:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label33: TLabel
          Left = 164
          Top = 80
          Width = 61
          Height = 13
          AutoSize = False
          Caption = 'Picture #:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object FlagImage: TImage
          Left = 64
          Top = 39
          Width = 33
          Height = 37
          Picture.Data = {
            07544269746D61703A090000424D3A0900000000000076000000280000008400
            0000210000000100040000000000C40800000000000000000000100000000000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00EEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEE
            EE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEE
            EEEE870E3546EEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEE
            EEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEE
            EEEEEEEEEEEE870ECBC0EEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEE
            EEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEE
            EEEEEEEEEEEEEEEEEEEE870EAF56EEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEE
            EEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EE
            EEEEEEEEEEEEEEEEEEEEEEEEEEEE870EFA83EEEEEEEEEEEEEEEEEEEEEEEEEEEE
            E870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEE
            EEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870E0808EEEEEEEEEEEEEEEEEEEE
            EEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEE
            EEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870E6020EEEEEEEEEEEE
            EEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEE
            EEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870EB888EEEE
            EEEEEEEEEEEEEEEEEEEEEEEEE870EEEEEEEEEEEEEEEEEEEEEE888FFFEE870EEE
            EEEEEEEEEEEEEEE888888FFFEEE870EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE870E
            CEE8EEEEEE88888FEEEEEEEEEEEEEE888870EEEE88FEEEEEEEEEEEEEE880000F
            FF870EEFFEEEEEEEEEEEE8880000000FFFE870EEEEEEEEEE888888FFFFFFFEEE
            E888870E2622EEE888808800FFFFFFEEEEEE88808870EE8880FFFFEEEEEEEEE8
            880000000F870EEFFFEEEEEEEEE88800000000000FF870EEFFFEEEE880088000
            0000FFFE8808870E3222E888088008000F000FFFFF8880008870EE88800F0FFF
            FFF88888000BBBB00F870EEF0FFFEEE888880000CC00BBB000F870EEFFFFE888
            80008000000000F88008870EC000E888008808800F000000000000008870EE80
            880F00F00FF00000000BBBB00F870EEF0F0FFFF8008800CCCC00BBBB00F870EE
            F0FF88088800800CCCC000000008870E8CB8E8088088888FFF00C00000000B00
            8870EE8088FFF0F000F00000C00BBBB00F870EEFFF00F0F8000800CCCC00BBBB
            00F870EEF0F088008888800CCCC00B000008870E6080E808888C88CCFF00CCCC
            00BBBB008870EE888CFFFFFF00F00CCCC00BBBB00F870EEFFFF0F008800800CC
            CC00BBBB00F870EEFFF008808CC8800CCCC00BBBB008870E2000E888C88CC8CC
            CF00CCCC00BBBB008870EE888CCFCFFFFFF00CCCC00BBBB00F870EEFCFFFFF08
            888800CCCC00BBBB00F870EEFFFF08888CCC800CCCC00BBBB008870E2222E888
            CC88C88CCF00CCCC00BBBB008870EE8C88CFCCFCCFF00CCCC00BBBB00F870EEF
            CFCFFFF8CC8800CCCC00BBBB00F870EEFCFF88C888CC800CCCC00BBBB008870E
            3000E8C88C88888FFF00CCCC00BBBB008870EE8C88FFFCFCCCF00CCCC0000000
            0F870EEFFFCCFCF8CCC800CC0000000B00F870EEFCFC88CC8888800CCCC00BBB
            B008870E8008E8C888808800FF00CCCC00BBBB008870EE8880FFFFFFCCF00CCC
            C00000000F870EEFFFFCFCC88CC800000000000000F870EEFFFCC88C8008800C
            CCC00BBBB008870E1048E888088008000F00CCCC00BBB0008870EE88800F0FFF
            FFF00CCCC00222200F870EEF0FFFFFC8888800009900222000F870EEFFFFC888
            80008000000000BBB008870E2400E888008808800F000CCC000000008870EE80
            880F00F00FF00CCC000222200F870EEF0F0FFFF8008800999900222200F870EE
            F0FF880888008000000000000008870E5844E8088088888FFF00000000000200
            8870EE8088FFF0F000F00000000222200F870EEFFF00F0F80008009999002222
            00F870EEF0F0880088888009999002000008870EB088E80888898899FF009000
            002222008870EE8889FFFFFF00F00000900222200F870EEFFFF0F00880080099
            9900222200F870EEFFF0088089988009999002222008870E8888E88898899899
            9F009999002222008870EE88899F9FFFFFF00999900222200F870EEF9FFFFF08
            888800999900222200F870EEFFFF088889998009999002222008870ED100E888
            998898899F009999002222008870EE89889F99F99FF00999900222200F870EEF
            9F9FFFF8998800999900222200F870EEF9FF889888998009999002222008870E
            1D11E8988988888FFF009999002222008870EE8988FFF9F999F0099990022220
            0F870EEFFF99F9F8999800990000000200F870EEF9F988998888800999900222
            2008870E9D99E89888808800FF009999002222008870EE8880FFFFFF99F00999
            900000000F870EEFFFF9F998899800000000000000F870EEFFF9988980088009
            999002222008870E5911E888088008000F009999002222008870EE88800F0FFF
            FFF00999900000000F870EEF0FFFFF988888000088888FF000F870EEFFFF9888
            80008000000002222008870E0F00E888008808800F000999002220008870EE80
            880F00F00FF0099900088FF00F870EEF0F0FFFF8008800888EEEEEFFF0F870EE
            F0FF880888008000000000222008870EAB88E8088088888FFF00000000000008
            8880EE8088FFF0F000F000000088EEFFFF880EEFFF00F0F80008888EEEEEEEEE
            FFF880EEF0F08800888888FFFFFF00000088880E9000E808888EEEEEEFFFF000
            00000888E000EE888EEEFFFF00F00000888EEEEEEE000EEEEFF0F00880088EEE
            EEEEEEEEEEE000EEFFF0088088EEEEEEEEEFFF00088E000EFCD8E888EEEEEEEE
            EEEEFFFFFF8888EEE870EEEEEEEEEEEFFFF888888EEEEEEEEE870EEEEEFFFF08
            8888EEEEEEEEEEEEEEE870EEEEFF08888EEEEEEEEEEEEFF888EE870E2041EEEE
            EEEEEEEEEEEEEEEEEEEEEEEEE880EEEEEEEEEEEEEEEEEEEEEEEEEEEEEE880EEE
            EEEEEFF8EEEEEEEEEEEEEEEEEEE880EEEEEF888EEEEEEEEEEEEEEEEEEEEE880E
            C800EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
            EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
            EEEEEEEEEC10}
          Visible = False
        end
        object SearchImage: TImage
          Left = 64
          Top = 39
          Width = 33
          Height = 37
          Picture.Data = {
            07544269746D617076360000424D763600000000000076000000280000004002
            0000300000000100040000000000003600000000000000000000100000001000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777700077777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            77777777777777777777777777777777777774CEC07777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777000777
            7777777777777777777777777777777777774CEEC07777777777777777777777
            7777777777777777770007777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777774CEC077
            777777777777777777777777777777777774CEECC07777777777777777777777
            777777777777777774CEC0777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            777777777777777777777777777777777777777777777777777777774CEEC077
            77777777777777777777777777777777774CEECC077777777777777777777777
            77777777777777774CEEC0777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            77777777777777777777777777777777777777777777777777777774CEECC077
            7777777777777777777777777777777774CEECC0777777777777777777777777
            7777777777777774CEECC0777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777770007777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777000777777777777777777777777777777777777774CEECC0777
            777777777777777777777777777777774CEECC07777777777777777777777777
            777777777777774CEECC07777777777777777777777777777777777777777700
            0777777777777777777777777777777777777777777777777777777777777777
            777777777777777777777777777774CEC0777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777774CEC07777777777777777777777777777777777774CEECC07777
            777777777777777777777777777777787FECC077777777777777777777777777
            77777777777774CEECC0777777777777777777777777777777777777777774CE
            C077777777777777777777777777777777777777777777777777777777777777
            77777777777777777777777777774CEEC0777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            777777777774CEEC0777777777777777777777777777777777774CEECC077777
            77777777777777777777777700000087F78C0777777777777777777777777777
            7777777777774CEECC0777777777777777777777777777777777777777774CEE
            C077777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777774CEECC0777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            77777777774CEECC0777777777777777777777777777777777787FECC0777777
            7777777777777777777777006666660078807777777777777777777777777777
            7777777777787FECC0777777777777777777777777777777777777777774CEEC
            C077777777777777777777777777777777777777777777777777777777777777
            777777777777777777777777774CEECC07777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777774CEECC0777777777777777777777777777700000087F78C07777777
            77777777777777777777787EFEFEFE6608077777777777777777777777777777
            777700000087F78C0777777777777777777777777777777777777777774CEECC
            0777777777777777777777777777777777777777777000777777777777777777
            77777777777777777777777774CEECC077777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            777777774CEECC07777777777777777777777777770066666600788077777777
            7777777777777777777787EFEFEFEFE660777777777777777777777777777777
            7700666666007880777777777777777777777777777777777777777774CEECC0
            7777777777777777777777777777777777777777774CEC077777777777777777
            7777777777777777777777778CEECC0777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            77777778CEECC077777777777777777777777777788888888866080777777777
            7777777777777777777788888888888860777777777777777777777777777777
            788888888866080777777777777777777777777777777777777777774CEECC07
            777777777777777777777777777777777777777774CEEC077777777777777777
            7777777777777777777777787FECC07777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            77777787FECC0777777777777777777777777777877777777776608007777777
            7777777777777777777877777777777776000000077777777777777777777777
            877777777776600000000000077777777777777777777777000000087FECC000
            00000000077777777777777777777770000000004CEECC000000000077777777
            777777777777777700000087F78C077777777777777777777777777777777777
            7777777777777700077777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777000777777777777777
            7777777777777777777777777777770007777777777777777777777777777770
            0000087F78C077777777777777777777777777778EFEBEFEBEFE608707777777
            77777777777777777778BEFEBEFEBEFEB6087777077777777777777777777777
            8EFEBEFEBEFE60877777777707777777777777777777777700000087F78C0877
            7777777707777777777777777777777877777774CEECC0877777777077777777
            7777777777777700666666007880777777777777777777777777777777777777
            77777777777774CEC07777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777774CEC077777777777777
            777777777777777777777777777774CEC0777777777777777777777777777006
            6666600788077777777777777777777777777778EFEFEFEFEFEFE60807777777
            77777777777777777778EFEFEFEFEFEFE608FBF7077777777777777777777778
            EFEFEFEFEFEFE608FBFFFBF70777777777777777777777006666660078808BFF
            FBFFFBF7077777777777777777777778BFFFBF4CEECC08FFBFFFBF7077777777
            777777777777787EFEFEFE660807777777777777777777777777777777777777
            7777777777774CEEC07777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            777777777777777777777777777777777777777777774CEEC077777777777777
            77777777777777777777777777774CEEC07777777777777777777777777787EB
            EFEBE66080777777777777777777777777777778BEFEBEFEBEFEB60807777777
            77777777777777777778BEFEBEFEBEFEB608FFF7077777777777777777777778
            BEFEBEFEBEFEB608FFFBFFF7077777777777777777777877BEFEBE660808FFFF
            FFFBFFF7077777777777777777777778FFBFF8CEECC08FBFFFBFFF7077777777
            77777777777787EFEFEFEFE66077777777777777777777777777777777777777
            777777777774CEECC07777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            77777777777777777777777777777777777777777774CEECC077777777777777
            7777777777777777777777777774CEECC0777777777777777777777777788888
            8EFEF766077777777777777777777777777777788888888888EFE60807777777
            777777777777777777788888888888888608FBF7077777777777777777777778
            EFE8888888888608FBFFFBF70777777777777777777787E7EFE88886608FFBFF
            FBFFFBF7077777777777777777777778BFFF87FECC08BFFFBFFFBF7077777777
            7777777777778EFEFEFEFEFE6077777777777777777777777777777777777777
            77777777774CEECC077777777777777777777777777777777777777777777777
            0007777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777774CEECC0777777777777777
            777777777777777777777777774CEECC0777777777777777777777777778EFEB
            EFEBE78607777777777777777777777777777778BEFEBEFEBEFEB60807777777
            777777777777777777778EFEBEFEBEFEB08BFFF7077777777777777777777778
            BEFEBEFEBEFEB608777BFFF7077777777777777777778EF7BEFEBEFE60877777
            777BFFF707777777777777777777700000087F78C087777777BFFF7077777777
            777777777778EFEFEFEFEFEFE607777777777777777777777777777777777777
            7777777774CEECC0777777777777777777777777777777777777777777777774
            CEC0777777777777777777777777777777777777777777777777777777777777
            777777777777777777777777777777777777777774CEECC07777777777777777
            77777777777777777777777774CEECC077777777777777777777777777888888
            8EFEF78E607777777777777777777777777777788888888888EFE60807777777
            77777777777777777777888888888888608FFBF7077777777777777777777778
            EFE8888888888608FBFFFBF707777777777777777778EFE7EFE888888608FBFF
            FBFFFBF70777777777777777777006666660078808FFBFFFBFFFBF7077777777
            777777777778FEFEFEFEFEFEF607777777777777777777777777777777777777
            777777778CEECC0777777777777777777777777777777777777777777777774C
            EEC0777777777777777777777777777777777777777777777777700077777777
            77777777777777777777777777777777777777774CEECC077777777777777777
            7777777777777777777777774CEECC07777777777777777777777777778BEFEB
            EFEBE78F607777777777777777777777777777778EFEBEFEBEFEB08707777777
            7777777777777777777778FEBEFEBEF608FBFFF7077777777777777777777777
            8EFEBEFEBEFEB087777BFFF707777777777777777778FEF7BEFEBEFEB6087777
            777BFFF707777777777777777787EF7BEFE660808777777777BFFF7077777777
            777777777778EFEFEFEFEFEFE607777777777777777777777777777777777777
            777777787FECC0777777777777777777777777777777777777777777777774CE
            ECC07777777777777777777777777777777777777777777777774CEC07777777
            77777777777777777777777777777777777777787FECC0777777777777777777
            7777777777777777777777787FECC07777777777777777777777777777888888
            8EFEF78E607777777777777777777777777777778888888888EF608707777777
            777777777777777777777788EFEFEF008BFFFBF7077777777777777777777777
            8FE888888888608FFBFFFBF707777777777777777778EFE7EFE888888608FBFF
            FBFFFBF70777777777777777787EFE7EFE886608BFFFBFFFBFFFBF7077777777
            777777777778FEFEFEFEFEFEF607777777777777777777777777777777777777
            00000087F78C0777777777777777777777777777777777777777777777774CEE
            CC07777777777777777777777777777777777777777777777774CEEC07777777
            7777777777777777777777777777777700000087F78C07777777777777777777
            777777777777777700000087F78C0777777777777777777777777777778BEFEB
            EFEBE78F6077777777777777777777777777777778FEBEFEBEF608F707777777
            77777777777777777777777788888087777BFFF7077777777777777777777777
            78FEBEFEBEF60877777BFFF707777777777777777778FEF7BEFEBEFEB6087777
            777BFFF7077777777777777778EFEF7BEFEBE6087777777777BFFF7077777777
            777777777778EFEFEFEFEFEFE607777777777777777777777777777777777700
            666666007880777777777777777777777777777777777777777777777774CEEC
            C07777777777777777777777777777777777777777777777774CEECC07777777
            7777777777777777777777777777770066666600788077777777777777777777
            7777777777777700666666007880777777777777777777777777777777888888
            8EFEF78E607777777777777777777777777777777788EFEFEF008BF707777777
            77777777777777777777FBFFFBFFFBFFFBFFFBF7077777777777777777777777
            7788EFEFEF008BFFFBFFFBF707777777777777777778EFE7EFE888888608FBFF
            FBFFFBF707777777777777778EFEFE7EFE8888608FFFBFFFBFFFBF7077777777
            7777777777778EFEFEFEFEFEF07777777777777777777777777777777777787E
            7EFEFE6608077777777777777777777777777777777777778FFFBFFFBF4CEECC
            077777777777777777777777777777778FFBFFFBFFFBFFFBF4CEECC077777777
            77777777777777778FFB77777777787EBEFEBE66080777777777777777777777
            8FFB77777777787EBEFEBE66080777777777777777777778FFB777777778EFEB
            EFEBE78F0777777777777777777777778FFB777777778888808FFFF707777777
            77777777777777778FFB777777777777777BFFF7077777777777777777777777
            8FFF888880877777777BFFF7077777777777777777778EF7BEFEBEFEB0877777
            777BFFF707777777777777778FEFEF7BEFEBEF608777777777BFFF7077777777
            7777777777778FEFEFEFEFEF60777777777777777777777777777777777787EF
            7FE7888660777777777777777777777777777777777777778FBFFFBFF87FECC0
            777777777777777777777777777777778BFF7777777777774CEECC0777777777
            77777777777777778BFFFBFFFBFF888888EFEFE6607777777777777777777777
            8BFFFBFFFBFF888888EFEFE6607777777777777777777778BFFFBFFFBFF88888
            8EFEF7860777777777777777777777778BFFFBFFFBFFFBFFFBFFFBF707777777
            77777777777777778BFFFBFFFBFFFBFFFBFFFBF7077777777777777777777777
            8BFFFBFFFBFFFBFFFBFFFBF7077777777777777777778FE7EFE88888608FFBFF
            FBFFFBF707777777777777778EFEFE7EFE8888608FFFBFFFBFFFBF7077777777
            77777777777778FEFEFEFEF60777777777777777777777777777777777778EFE
            7EFEFEFE60777777777777777777777777777777777777778F00000087F78C07
            777777777777777777777777777777778FFBFFFBFFFBFFF8CEECC07777777777
            77777777777777778FFB777777778EFEBEFEBEF7607777777777777777777777
            8FFB777777778EFEBEFEBEF7607777777777777777777778FFB7777777778FEB
            EFEBE7607777777777777777777777778FFB777777777777777BFFF707777777
            77777777777777778FFB777777777777777BFFF7077777777777777777777777
            8FFB777777777777777BFFF70777777777777777777778F7BEFEBEF608777777
            777BFFF707777777777777778FEFEF7BEFEBEF608777777777BFFF7077777777
            7777777777777788FFEFEF00777777777777777777777777777777777778EFEF
            7FEB888886077777777777777777777777777777777777770066666600788077
            777777777777777777777777777777778BFF777777777787FECC077777777777
            77777777777777778BFFFBFFFBF8888888EFEFE7860777777777777777777777
            8BFFFBFFFBF8888888EFEFE7860777777777777777777778BFFFBFFFBFFFB88E
            FEFEF0007777777777777777777777778BFFFBFFFBFFFBFFFBFFFBF707777777
            77777777777777778BFFFBFFFBFFFBFFFBFFFBF7077777777777777777777777
            8BFFFBFFFBFFFBFFFBFFFBF7077777777777777777777788EFE888008FFFFBFF
            FBFFFBF707777777777777778EFEFE7EFE8888608FFFBFFFBFFFBF7077777777
            777777777777777788888077777777777777777777777777777777777778FEFE
            7EFEFEFEF6077777777777777777777777777777777777787EFEBEFE66080777
            777777777777777777777777777777778FFBFFF00000087F78C0777777777777
            77777777777777778FFB77777778BEFEBEFEBEF7860777777777777777777777
            8FFB77777778BEFEBEFEBEF7860777777777777777777778FFB7777777777778
            888808707777777777777777777777778FFB777777777777777BFFF707777777
            77777777777777778FFB777777777777777BFFF7077777777777777777777777
            8FFB777777777777777BFFF70777777777777777777777778888808777777777
            777BFFF7077777777777777778EFEF7BEFEBEF087777777777BFFF7077777777
            777777777777777777777777777777777777777777777777777777777778EFEF
            7FEB88888607777777777777777777777777777777777787EFE8888886607777
            777777777777777777777777777777778BFF7006666660078807777777777777
            77777777777777778BFFFBFFFBF8888888EFEFE7860777777777777777777777
            8BFFFBFFFBF8888888EFEFE7860777777777777777777778BFFFBFFFBFFFBFFF
            BFFFBF707777777777777777777777778BFFFBFFFBFFFBFFFBFFFBF707777777
            77777777777777778BFFFBFFFBFFFBFFFBFFFBF7077777777777777777777777
            8BFFFBFFFBFFFBFFFBFFFBF70777777777777777777777778BFFFBFFFBFFFBFF
            FBFFFBF7077777777777777778FEFE7EFE888608BFFFBFFFBFFFBF7077777777
            777777777777777777777777777777777777777777777777777777777778FEFF
            7EFEFEFEF607777777777777777777777777777777777787BEFEBEFEBE607777
            777777777777777777777777777777778FFB87EBEFEBE6608077777777777777
            77777777777777778FFB77777778BEFEBEFEBEF7860777777777777777777777
            8FFB77777778BEFEBEFEBEF7860777777777777777777778FFB7777777777777
            77BFFF707777777777777777777777778FFB777777777777777BFFF707777777
            77777777777777778FFB777777777777777BFFF7077777777777777777777777
            8FFB777777777777777BFFF70777777777777777777777778FFB777777777777
            777BFFF70777777777777777778FEF7BEFEB60877777777777BFFF7077777777
            777777777777777777777777777777777777777777777777777777777778EFEF
            7FEBEFEBE6077777777777777777777777777777777778E7EFEFEFEFEFE60777
            777777777777777777777777777777778BF87EFEFEFEFE660777777777777777
            77777777777777778BFFFBFFFBF8EFEFEFEFEFE7860777777777777777777777
            8BFFFBFFFBF8EFEFEFEFEFE7860777777777777777777778BFFFBFFFBFFFBFFF
            BFFFBF707777777777777777777777778BFFFBFFFBFFFBFFFBFFFBF707777777
            77777777777777778BFFFBFFFBFFFBFFFBFFFBF7077777777777777777777777
            8BFFFBFFFBFFFBFFFBFFFBF70777777777777777777777778BFFFBFFFBFFFBFF
            FBFFFBF7077777777777777777788F7EFEF008FFBFFFBFFFBFFFBF7077777777
            7777777777777777777777777777777777777777777777777777777777778EFF
            7EFEFEFEF0777777777777777777777777777777777778F7BEFEBEFEBEF60777
            777777777777777777777777777777778FF8EFEBEFEBEFE60777777777777777
            77777777777777778FFBFFFBFFFB8EFEBEFEBEF7607777777777777777777777
            8FFBFFFBFFFB8EFEBEFEBEF7607777777777777777777778FFBFFFBFFFBFFFBF
            FFBFFF707777777777777777777777778FFBFFFBFFFBFFFBFFFBFFF707777777
            77777777777777778FFBFFFBFFFBFFFBFFFBFFF7077777777777777777777777
            8FFBFFFBFFFBFFFBFFFBFFF70777777777777777777777778FFBFFFBFFFBFFFB
            FFFBFFF707777777777777777777788888087FBFFFBFFFBFFFBFFF7077777777
            7777777777777777777777777777777777777777777777777777777777778FEF
            7FEBEFEB60777777777777777777777777777777777778E7EFE8888888860777
            777777777777777777777777777777778B88888888888EFE6077777777777777
            77777777777777778BFFFBFFFBFF8FEFEFEFEFE7607777777777777777777777
            8BFFFBFFFBFF8FEFEFEFEFE7607777777777777777777778BFFFBFFFBFFFBFFF
            BFFFBF707777777777777777777777778BFFFBFFFBFFFBFFFBFFFBF707777777
            77777777777777778BFFFBFFFBFFFBFFFBFFFBF7077777777777777777777777
            8BFFFBFFFBFFFBFFFBFFFBF70777777777777777777777778BFFFBFFFBFFFBFF
            FBFFFBF7077777777777777777777778BFFFBFFFBFFFBFFFBFFFBF7077777777
            77777777777777777777777777777777777777777777777777777777777778FE
            7EFEFEF607777777777777777777777777777777777778F7BEFEBEFEBEF60777
            777777777777777777777777777777778F8BEFEBEFEBEFEB6077777777777777
            77777777777777778FFB7777777778FE88888886077777777777777777777777
            8FFB7777777778FE88888886077777777777777777777778FFB7777777777FBF
            FFBFFF707777777777777777777777778FFB7777777777FBFFFBFFF707777777
            77777777777777778FFB7777777777FBFFFBFFF7077777777777777777777777
            8FFB7777777777FBFFFBFFF70777777777777777777777778FFB7777777777FB
            FFFBFFF7077777777777777777777778FFB7777777777FBFFFBFFF7077777777
            7777777777777777777777777777777777777777777777777777777777777788
            7FEBEF0077777777777777777777777777777777777778E7EFEFEFEFEFE60777
            777777777777777777777777777777778B8EFEFEFEFEFEFE6077777777777777
            77777777777777778BFFFBFFFBFFFF888FEFE700777777777777777777777777
            8BFFFBFFFBFFFF888FEFE700777777777777777777777778BFFFBFFFBFFFBFFF
            800000007777777777777777777777778BFFFBFFFBFFFBFFF800000007777777
            77777777777777778BFFFBFFFBFFFBFFF8000000077777777777777777777777
            8BFFFBFFFBFFFBFFF80000000777777777777777777777778BFFFBFFFBFFFBFF
            F8000000077777777777777777777778BFFFBFFFBFFFBFFF8000000077777777
            7777777777777777777777777777777777777777777777777777777777777777
            888880777777777777777777777777777777777777777787BEFEBEFEBEF07777
            777777777777777777777777777777778F8BEFEBEFEBEFEB6077777777777777
            77777777777777778FFBFFFBFFFBFFFB88888087777777777777777777777777
            8FFBFFFBFFFBFFFB88888087777777777777777777777778FFBFFFBFFFBFFFBF
            8FFF78077777777777777777777777778FFBFFFBFFFBFFFBF8FFF78077777777
            77777777777777778FFBFFFBFFFBFFFBF8FFF780777777777777777777777777
            8FFBFFFBFFFBFFFBF8FFF7807777777777777777777777778FFBFFFBFFFBFFFB
            F8FFF780777777777777777777777778FFBFFFBFFFBFFFBF8FFF780777777777
            7777777777777777777777777777777777777777777777777777777777777777
            777777777777777777777777777777777777777777777787EFEFEFEFEF607777
            777777777777777777777777777777778B8EFEFEFEFEFEFE6077777777777777
            77777777777777778BFFFBFFFBFFFBFFF8FF7807777777777777777777777777
            8BFFFBFFFBFFFBFFF8FF7807777777777777777777777778BFFFBFFFBFFFBFFF
            8FF780777777777777777777777777778BFFFBFFFBFFFBFFF8FF780777777777
            77777777777777778BFFFBFFFBFFFBFFF8FF7807777777777777777777777777
            8BFFFBFFFBFFFBFFF8FF78077777777777777777777777778BFFFBFFFBFFFBFF
            F8FF7807777777777777777777777778BFFFBFFFBFFFBFFF8FF7807777777777
            7777777777777777777777777777777777777777777777777777777777777777
            777777777777777777777777777777777777777777777778BEFEBEFEB6077777
            777777777777777777777777777777778FF8EFEBEFEBEFEB0777777777777777
            77777777777777778FFBFFFBFFFBFFFBF8F78077777777777777777777777777
            8FFBFFFBFFFBFFFBF8F78077777777777777777777777778FFBFFFBFFFBFFFBF
            8F7807777777777777777777777777778FFBFFFBFFFBFFFBF8F7807777777777
            77777777777777778FFBFFFBFFFBFFFBF8F78077777777777777777777777777
            8FFBFFFBFFFBFFFBF8F780777777777777777777777777778FFBFFFBFFFBFFFB
            F8F78077777777777777777777777778FFBFFFBFFFBFFFBF8F78077777777777
            7777777777777777777777777777777777777777777777777777777777777777
            77777777777777777777777777777777777777777777777788EFEFEF00777777
            777777777777777777777777777777778BF8FEFEFEFEFEF60777777777777777
            77777777777777778BFFFBFFFBFFFBFFF8780777777777777777777777777777
            8BFFFBFFFBFFFBFFF8780777777777777777777777777778BFFFBFFFBFFFBFFF
            878077777777777777777777777777778BFFFBFFFBFFFBFFF878077777777777
            77777777777777778BFFFBFFFBFFFBFFF8780777777777777777777777777777
            8BFFFBFFFBFFFBFFF87807777777777777777777777777778BFFFBFFFBFFFBFF
            F8780777777777777777777777777778BFFFBFFFBFFFBFFF8780777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777788888077777777
            777777777777777777777777777777778FFB8FEBEFEBEF607777777777777777
            77777777777777778FFBFFFBFFFBFFFBF8807777777777777777777777777777
            8FFBFFFBFFFBFFFBF8807777777777777777777777777778FFBFFFBFFFBFFFBF
            880777777777777777777777777777778FFBFFFBFFFBFFFBF880777777777777
            77777777777777778FFBFFFBFFFBFFFBF8807777777777777777777777777777
            8FFBFFFBFFFBFFFBF88077777777777777777777777777778FFBFFFBFFFBFFFB
            F8807777777777777777777777777778FFBFFFBFFFBFFFBF8807777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            777777777777777777777777777777778BFFF88EFEFEF0077777777777777777
            77777777777777778BFFFBFFFBFFFBFFF8077777777777777777777777777777
            8BFFFBFFFBFFFBFFF8077777777777777777777777777778BFFFBFFFBFFFBFFF
            807777777777777777777777777777778BFFFBFFFBFFFBFFF807777777777777
            77777777777777778BFFFBFFFBFFFBFFF8077777777777777777777777777777
            8BFFFBFFFBFFFBFFF80777777777777777777777777777778BFFFBFFFBFFFBFF
            F8077777777777777777777777777778BFFFBFFFBFFFBFFF8077777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777788888888888807777777777777777777
            7777777777777777888888888888888888777777777777777777777777777777
            8888888888888888887777777777777777777777777777788888888888888888
            8777777777777777777777777777777788888888888888888877777777777777
            7777777777777777888888888888888888777777777777777777777777777777
            8888888888888888887777777777777777777777777777778888888888888888
            8877777777777777777777777777777888888888888888888777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777}
          Visible = False
        end
        object BookImage: TImage
          Left = 64
          Top = 39
          Width = 33
          Height = 36
          Picture.Data = {
            07544269746D6170660D0000424D660D0000000000007600000028000000B400
            0000240000000100040000000000F00C00000000000000000000100000000000
            000000000000000080000080000000808000800000008000800080800000C0C0
            C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00333333333330000003333333333333333333333333333330000003333333
            3333333333333333333333300000033333333333333333333333333333300000
            0333333333333333333333333333333000000333333333333333333300003333
            3333333055555000000000000000003333333333333055555000000000000000
            0033333333333330555550000000000000000033333333333330555550000000
            0000000000333333333333305555500000000000000000330000333333333305
            5000555555555555555550333333333333055000555555555555555550333333
            3333330550005555555555555555503333333333330550005555555555555555
            5033333333333305500055555555555555555033000033333333305587075555
            5555555555555033333333333055870755555555555555555033333333333055
            8807855555555555555550333333333330558708855555555555555550333333
            3333305558088555555555555555503300003333333305558707788888888888
            8888503333333333055587077888888888888888503333333333055587078888
            8888888888885033333333330555770788888888888888885033333333330555
            87078888888888888888503300003333333055587F0778877777777777785033
            3333333055587F07F78777777777777850333333333055587707877777777777
            77785033333333305557870FF77777777777777850333333333055587F07F777
            777777777778503300003333330555887F07F7788877FFFFF778503333333305
            55887F07FF7877777777F778503333333305558877078F7777777777F7785033
            333333055587870FFFF777777777F77850333333330555887F07FFF777777777
            F77850330000333330555887FF07FFF77788888FF7785033333330555887FF07
            FFF78FFFFFFFF7785033333330555887F70F8FFFFFFFFFFFF778503333333055
            5878F70FFFFFFFFFFFFFF7785033333330555887FF07FFFFFFFFFFFFF7785033
            0000333305558877FF07FFFFFF77777888785033333305558877FF07FFFF78FF
            FFFFF7785033333305558877F70F8FFFFFFFFFFFF7785033333305558778F70F
            FFFFFFFFFFFFF7785033333305558877FF07FFFFFFFFFFFFF778503300003330
            5558877FFF07FFFFFFFFFF777878503333305558877FFF07FFFFF78FFFFFF778
            503333305558877FF70F77FFFFFFFFFFF77850333330555887F8F70FFFFFFFFF
            FFFFF778503333305558877FFF07FFFFFFFFFFFFF778503300003305558877FF
            FF07FFFFFFFFFFFFF87850333305558877FFFF07FFFFFF78FFFFF77850333305
            558877FFF70FF8FFFFFFFFFFF7785033330555887F8FF70FFFFFFFFFFFFFF778
            50333305558877FFFF07FFFFFFFFFFFFF77850330000305558777FFFFF07FFFF
            FFFFFFFFF8785033305558777FFFFF07FFFFFFF78FFFF7785033305558777FFF
            F70FF8FFFFFFFFFFF778503330555877FF8FF70FFFFFFFFFFFFFF77850333055
            58777FFFFF07FFFFFFFFFFFFF7785033000005558777FFFFFF07F5FFFFFFFFFF
            F878503305558777FFFFFF07F5FFFFFF78FFF778503305558777FFFFF70FF8FF
            FFFFFFFFF77850330555877FFF8FF70FFFFFFFFFFFFFF778503305558777FFFF
            FF07FFFFFFFFFFFFF77850330000058777FFFFFFFF07FF55FFFFFFFFF8785033
            058777FFFFFFFF07FF5FFFFFF78FF7785033058777FFFFFFF70FF77FFFFFFFFF
            F7785033058777FFF8FFF70FFFFFFFFFFFFFF7785033058777FFFFFFFF07FFFF
            FFFFFFFFF778503300000587F7FFFFFFFF07F5FF555FFFFFF87850330587F7FF
            FFFFFF07F5F5FFFFF78FF77850330587F7FFFFFFF70F5F8FFFFFFFFFF7785033
            0587F7FFF8FFF70FFFFFFFFFFFFFF77850330587F7FFFFFFFF07FFFFFFFFFFFF
            F778503300000587F7FFFFFFFF07FF55FFF555FFF87850330587F7FFFFFFFF07
            FF5F5FFFF78FF77850330587F7FFFFFFF70F5F8FFFFFFFFFF77850330587F7FF
            F8FFF70FFFFFFFFFFFFFF77850330587F7FFFFFFFF07FFFFFFFFFFFFF7785033
            00000587F7FFFFFFFF07F5FF555FFFFFF87850330587F7FFFFFFFF07F5F5F5FF
            F78FF77850330587F7FFFFFFF70F8F8FFFFFFFFFF77850330587F7FFF8FFF70F
            FFFFFFFFFFFFF77850330587F7FFFFFFFF07FFFFFFFFFFFFF778503300000587
            F7FFFFFFFF07FF55FFF555FFF87850330587F7FFFFFFFF07FF5F5F5FF78FF778
            50330587F7FFFFFFF70FF58FFFFFFFFFF77850330587F7FFF8FFF70FFFFFFFFF
            FFFFF77850330587F7FFFFFFFF07FFFFFFFFFFFFF778503300000587F7FFFFFF
            FF07F5FF555FFFFFF87850330587F7FFFFFFFF07F5F5F5FFF78FF77850330587
            F7FFFFFFF70FF58FFFFFFFFFF77850330587F7FFF8FFF70FFFFFFFFFFFFFF778
            50330587F7FFFFFFFF07FFFFFFFFFFFFF778503300000587F7FFFFFFFF07FF55
            FFF555FFF87850330587F7FFFFFFFF07FF5F5F5FF78FF77850330587F7FFFFFF
            F70FF88FFFFFFFFFF77850330587F7FFF8FFF70FFFFFFFFFFFFFF77850330587
            F7FFFFFFFF07FFFFFFFFFFFFF778503300000587F7FFFFFFFF07FFFF555FFFFF
            F87850330587F7FFFFFFFF07FFF5F5FFF78FF77850330587F7FFFFFFF70F5F8F
            FFFFFFFFF77850330587F7FFF8FFF70FFFFFFFFFFFFFF77850330587F7FFFFFF
            FF07FFFFFFFFFFFFF778503300000587F7FFFFFFFF07FFFFFFF555FFF8785033
            0587F7FFFFFFFF07FFFF5F5FF78FF77850330587F7FFFFFFF70F5F8FFFFFFFFF
            F77850330587F7FFF8FFF70FFFFFFFFFFFFFF77850330587F7FFFFFFFF07FFFF
            FFFFFFFFF778503300000587F7FFFFFFFF07FFFFFFFFFFFFF87803330587F7FF
            FFFFF707FFFFF5FFF78FF77003330587F7FFFFFFF70F8F8FFFFFFFFFF7700333
            0587F7FFF8FFF80FFFFFFFFFFFFFF77003330587F7FFFFFFFF07FFFFFFFFFFFF
            F770033300000587F7FFFFFFF8388FFFFFFFFFFFF87833330587F7FFFFFFF833
            8FFFFF5FF78FF78333330587F7FFFFFFF88FF588FFFFFFFF788333330587F7FF
            F8FFF8377FFFFFFFFFFF788333330587F7FFFFFFF8387FFFFFFFFFFF78833333
            00000587F7FFFFFF8333388FFFFFFFFFF87833330587F7FFFFFF833338FFFFFF
            F788873333330587F7FFFFFF8338F58388888888733333330587F7FFF8FF8333
            787FFFFFFF78733333330587F7FFFFFF8333787FFFFFFF787333333300000587
            F7FFFFF73333333888FFFFFFF88333330587F7FFFFF73333338FFFFFF7833333
            33330587F7FFFFF73338F88333333333333333330587F7FFF8FF833333788888
            8873333333330587F7FFFFF733333378888888733333333300000587F7FFFF77
            33333333338888FFF83333330587F7FFFF7733333338FFFFF783333333330587
            F7FFFF773338FF8333333333333333330587F7FFF8F773333333333333333333
            33330587F7FFFF7733333333333333333333333300000587F7FFFF7333333333
            33333388883333330587F7FFFF73333333338FFFF783333333330587F7FFFF83
            33338F8333333333333333330587F7FFF8F83333333333333333333333330587
            F7FFFF7333333333333333333333333300000587F7FFF8333333333333333333
            333333330587F7FFF8333333333338FFF783333333330587F7FFF83333338F83
            33333333333333330587F7FFF8773333333333333333333333330587F7FFF833
            33333333333333333333333300000587F7FF8333333333333333333333333333
            0587F7FF833333333333338FF783333333330587F7FF833333338F8333333333
            333333330587F7FF88733333333333333333333333330587F7FF833333333333
            333333333333333300000587F7F833333333333333333333333333330587F7F8
            3333333333333338F783333333330587F7F83333333377833333333333333333
            0587F7F838733333333333333333333333330587F7F833333333333333333333
            3333333300003087878333333333333333333333333333333087878333333333
            3333333387833333333330878783333333333883333333333333333330878783
            3833333333333333333333333333308787833333333333333333333333333333
            0000333838333333333333333333333333333333333838333333333333333333
            3883333333333338383333333333378333333333333333333338383333333333
            3333333333333333333333383833333333333333333333333333333300003333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333833333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333330000333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333383333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333000033333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            333333333333333333333333333333330000}
          Visible = False
        end
        object AnimatedImage1: TJvAnimatedImage
          Left = 32
          Top = 62
          Width = 50
          Height = 50
          TransparentColor = clNone
        end
        object ComboBox7: TComboBox
          Left = 104
          Top = 56
          Width = 121
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 2
          OnChange = ComboBox7Change
          Items.Strings = (
            'Running Man'
            'Windows Flag'
            'Search'
            'Book')
        end
        object Button2: TButton
          Left = 8
          Top = 36
          Width = 49
          Height = 21
          Caption = 'Start'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = Button2Click
        end
        object SpinEdit3: TSpinEdit
          Left = 104
          Top = 96
          Width = 53
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Increment = 10
          MaxValue = 0
          MinValue = 0
          ParentFont = False
          TabOrder = 3
          Value = 100
          OnChange = SpinEdit3Change
        end
        object SpinEdit4: TSpinEdit
          Left = 164
          Top = 96
          Width = 61
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxValue = 0
          MinValue = 0
          ParentFont = False
          TabOrder = 4
          Value = 0
          OnChange = SpinEdit4Change
        end
        object Button6: TButton
          Left = 104
          Top = 12
          Width = 120
          Height = 25
          Caption = 'Load from ANI...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = Button6Click
        end
      end
      object GroupBox13: TGroupBox
        Left = 240
        Top = 0
        Width = 249
        Height = 125
        Caption = 'TextListBox'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object Label34: TLabel
          Left = 8
          Top = 16
          Width = 233
          Height = 13
          AutoSize = False
          Caption = 'ListBox with horizontal scrollbar if necessary'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object TextListBox1: TJvTextListBox
          Left = 16
          Top = 36
          Width = 217
          Height = 57
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          Items.Strings = (
            'Short Item'
            'Short Item'
            'Long Item Long Item ')
          ParentFont = False
          TabOrder = 0
        end
        object Button5: TButton
          Left = 16
          Top = 96
          Width = 85
          Height = 23
          Caption = 'Show'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = Button5Click
        end
      end
      object GroupBox14: TGroupBox
        Left = 4
        Top = 128
        Width = 233
        Height = 149
        Caption = 'JvClock'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        object Label35: TLabel
          Left = 8
          Top = 16
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Clock'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label36: TLabel
          Left = 128
          Top = 12
          Width = 41
          Height = 13
          AutoSize = False
          Caption = 'Style:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label37: TLabel
          Left = 128
          Top = 84
          Width = 45
          Height = 13
          AutoSize = False
          Caption = 'Alarm:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object ComboBox8: TComboBox
          Left = 128
          Top = 28
          Width = 99
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 0
          OnChange = ComboBox8Change
          Items.Strings = (
            'Digital'
            'Analog')
        end
        object Button3: TButton
          Left = 128
          Top = 54
          Width = 99
          Height = 25
          Caption = 'Font...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = Button3Click
        end
        object CheckBox6: TCheckBox
          Left = 128
          Top = 100
          Width = 101
          Height = 17
          Caption = 'On'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = CheckBox6Click
        end
        object SpinEdit5: TSpinEdit
          Left = 128
          Top = 120
          Width = 35
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxValue = 24
          MinValue = 0
          ParentFont = False
          TabOrder = 3
          Value = 12
          OnChange = SpinEdit5Change
        end
        object SpinEdit6: TSpinEdit
          Left = 162
          Top = 120
          Width = 35
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxValue = 59
          MinValue = 0
          ParentFont = False
          TabOrder = 4
          Value = 0
          OnChange = SpinEdit6Change
        end
        object SpinEdit7: TSpinEdit
          Left = 196
          Top = 120
          Width = 35
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxValue = 59
          MinValue = 0
          ParentFont = False
          TabOrder = 5
          Value = 0
          OnChange = SpinEdit7Change
        end
        object Panel3: TPanel
          Left = 6
          Top = 32
          Width = 110
          Height = 110
          BevelOuter = bvNone
          TabOrder = 6
          object JvClock1: TJvClock
            Left = 0
            Top = 0
            Width = 110
            Height = 110
            AlarmHour = 12
            AutoSize = True
            BevelInner = bvNone
            BevelOuter = bvNone
            ShowMode = scAnalog
            Align = alClient
            BorderWidth = 1
            BorderStyle = bsSingle
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -17
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = True
            ParentFont = False
            OnAlarm = JvClock1Alarm
          end
        end
      end
      object GroupBox15: TGroupBox
        Left = 240
        Top = 128
        Width = 249
        Height = 149
        Caption = 'JvDice'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        object Label38: TLabel
          Left = 8
          Top = 16
          Width = 65
          Height = 13
          AutoSize = False
          Caption = 'Dices'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
        end
        object Label39: TLabel
          Left = 120
          Top = 104
          Width = 53
          Height = 13
          AutoSize = False
          Caption = 'Interval:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label40: TLabel
          Left = 180
          Top = 104
          Width = 57
          Height = 13
          AutoSize = False
          Caption = 'Value:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object JvDice: TJvDice
          Left = 12
          Top = 72
          Width = 66
          Height = 66
          Rotate = False
          ShowFocus = False
          TabOrder = 0
          OnChange = JvDiceChange
        end
        object Button4: TButton
          Left = 12
          Top = 40
          Width = 49
          Height = 21
          Caption = 'Start'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = Button4Click
        end
        object SpinEdit8: TSpinEdit
          Left = 120
          Top = 120
          Width = 53
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Increment = 10
          MaxValue = 0
          MinValue = 0
          ParentFont = False
          TabOrder = 2
          Value = 60
          OnChange = SpinEdit8Change
        end
        object SpinEdit9: TSpinEdit
          Left = 180
          Top = 120
          Width = 61
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxValue = 6
          MinValue = 1
          ParentFont = False
          TabOrder = 3
          Value = 1
          OnChange = SpinEdit9Change
        end
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 438
    Top = 1
  end
  object FormStorage1: TJvFormStorage
    AppStorage = MainForm.JvAppRegistryStorage
    AppStoragePath = 'Controls\'
    StoredProps.Strings = (
      'TabbedNotebook1.PageIndex'
      'rxLabel1.Font'
      'rxSlider1.Value'
      'CheckBox4.Checked'
      'CheckBox7.Checked'
      'Edit1.Text'
      'Panel2.Width'
      'DirectoryListBox1.Height'
      'OpenDialog.InitialDir')
    StoredValues = <>
    Left = 466
    Top = 1
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ani'
    Filter = 'Animated cursors (*.ani)|*.ani|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist]
    Left = 410
    Top = 1
  end
end
