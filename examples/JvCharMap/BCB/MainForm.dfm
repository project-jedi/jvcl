object frmMain: TfrmMain
  Left = 192
  Top = 107
  Width = 647
  Height = 430
  Caption = 'JvCharMap Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblChars: TLabel
    Left = 211
    Top = 8
    Width = 54
    Height = 13
    Anchors = [akTop]
    Caption = 'C&haracters:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 247
    Width = 639
    Height = 156
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 54
      Height = 13
      Caption = '&Start value:'
      FocusControl = edStart
    end
    object Label2: TLabel
      Left = 96
      Top = 8
      Width = 51
      Height = 13
      Caption = '&End value:'
      FocusControl = edEnd
    end
    object Label3: TLabel
      Left = 192
      Top = 8
      Width = 43
      Height = 13
      Caption = '&Columns:'
      FocusControl = edCols
    end
    object lblFilter: TLabel
      Left = 8
      Top = 73
      Width = 25
      Height = 13
      Hint = 
        'Specifies the Unicode subrange to display. Set to "ufUndefined" ' +
        'to show StartChar/EndChar range.'
      Caption = '&Filter:'
      FocusControl = cbFilter
    end
    object Label4: TLabel
      Left = 161
      Top = 73
      Width = 35
      Height = 13
      Hint = 'This combo is only enabled on non-NT OS'#39'es (Win95/98/Me).'
      Caption = '&Locale:'
    end
    object btnFont: TButton
      Left = 216
      Top = 126
      Width = 75
      Height = 25
      Caption = '&Font...'
      TabOrder = 13
      OnClick = btnFontClick
    end
    object chkZoomPanel: TCheckBox
      Left = 8
      Top = 52
      Width = 113
      Height = 17
      Caption = 'Show &Zoom Panel'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = chkZoomPanelClick
    end
    object edStart: TEdit
      Left = 8
      Top = 24
      Width = 57
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edStartChange
    end
    object udStart: TUpDown
      Left = 65
      Top = 24
      Width = 15
      Height = 21
      Associate = edStart
      Min = 0
      Max = 32767
      Position = 0
      TabOrder = 1
      Wrap = False
    end
    object edEnd: TEdit
      Left = 96
      Top = 24
      Width = 65
      Height = 21
      TabOrder = 2
      Text = '0'
      OnChange = edEndChange
    end
    object udEnd: TUpDown
      Left = 161
      Top = 24
      Width = 15
      Height = 21
      Associate = edEnd
      Min = 0
      Max = 32767
      Position = 0
      TabOrder = 3
      Wrap = False
    end
    object edCols: TEdit
      Left = 192
      Top = 24
      Width = 65
      Height = 21
      TabOrder = 4
      Text = '1'
      OnChange = edColsChange
    end
    object udColumns: TUpDown
      Left = 257
      Top = 24
      Width = 15
      Height = 21
      Associate = edCols
      Min = 0
      Max = 32767
      Position = 1
      TabOrder = 5
      Wrap = False
    end
    object cbColor: TJvColorComboBox
      Left = 8
      Top = 126
      Width = 100
      Height = 22
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
      DroppedDownWidth = 100
      NewColorText = 'Custom'
      Options = [coText, coCustomColors]
      TabOrder = 11
    end
    object cbFont: TJvFontComboBox
      Left = 112
      Top = 126
      Width = 97
      Height = 22
      DroppedDownWidth = 97
      FontName = 'System'
      ItemIndex = 0
      Sorted = False
      TabOrder = 12
    end
    object chkUnicode: TCheckBox
      Left = 192
      Top = 52
      Width = 59
      Height = 17
      Caption = '&Unicode'
      TabOrder = 8
      OnClick = chkUnicodeClick
    end
    object reInfo: TRichEdit
      Left = 354
      Top = 52
      Width = 272
      Height = 96
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 15
      WordWrap = False
    end
    object btnSelect: TButton
      Left = 548
      Top = 13
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Se&lect'
      TabOrder = 14
      OnClick = btnSelectClick
    end
    object cbFilter: TComboBox
      Left = 8
      Top = 90
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 9
      OnClick = cbFilterClick
    end
    object cbLocales: TComboBox
      Left = 161
      Top = 90
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 10
      OnClick = cbLocalesClick
    end
    object chkShadow: TCheckBox
      Left = 120
      Top = 52
      Width = 73
      Height = 17
      Hint = 
        'Shows and hides the drop shadow.'#13#10'Also, when checking/unchecking' +
        ', the shadow is offset randomly from the zoom panel.'#13#10' Default o' +
        'ffset value is 2px.'
      Caption = 'Shado&w'
      TabOrder = 7
      OnClick = chkShadowClick
    end
    object chkDisplayAll: TCheckBox
      Left = 270
      Top = 54
      Width = 79
      Height = 17
      Caption = '&Display all'
      TabOrder = 16
      OnClick = chkDisplayAllClick
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 128
    Top = 192
    object Copy1: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = Copy1Click
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 48
    Top = 192
  end
end
