object JvColorComboDemoMainForm: TJvColorComboDemoMainForm
  Left = 459
  Top = 135
  Width = 476
  Height = 454
  Caption = 'JvColorCombo Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Colors:'
  end
  object Label2: TLabel
    Left = 168
    Top = 8
    Width = 99
    Height = 13
    Caption = 'Custom Color Prefix:'
  end
  object Label3: TLabel
    Left = 304
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Display:'
  end
  object JvColorComboBox1: TJvColorComboBox
    Left = 16
    Top = 24
    Width = 145
    Height = 20
    AutoSave.Registry.Key = 'DefaultColor'
    AutoSave.Registry.Path = '\Software\JEDI\ColorTest'
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
    ColorValue = clNavy
    ColorDialogText = '(Other...)'
    NewColorText = 'Custom Color %d'
    Options = [coText, coSysColors, coCustomColors]
    DroppedDownWidth = 145
    OnNewColor = JvColorComboBox1NewColor
    TabOrder = 0
  end
  object memInfo: TMemo
    Left = 24
    Top = 96
    Width = 420
    Height = 321
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnColorNames: TButton
    Left = 24
    Top = 64
    Width = 90
    Height = 25
    Caption = 'Get Color Map:'
    TabOrder = 2
    OnClick = btnColorNamesClick
  end
  object edNameTemplate: TEdit
    Left = 168
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Custom Color %d'
  end
  object chkAllowCustom: TCheckBox
    Left = 168
    Top = 64
    Width = 121
    Height = 17
    Caption = 'Display Color Dialog'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = cbDisplayStyleChange
  end
  object btnCustColors: TButton
    Left = 352
    Top = 64
    Width = 89
    Height = 25
    Caption = 'Custom Colors'
    TabOrder = 5
    OnClick = btnCustColorsClick
  end
  object cbDisplayStyle: TComboBox
    Left = 304
    Top = 23
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    OnChange = cbDisplayStyleChange
    Items.Strings = (
      '(none)'
      'Text'
      'Hex'
      'RGB')
  end
end
