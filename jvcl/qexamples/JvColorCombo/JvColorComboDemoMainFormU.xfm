object JvColorComboDemoMainForm: TJvColorComboDemoMainForm
  Left = 459
  Top = 135
  Width = 476
  Height = 454
  Caption = 'JvColorCombo Demo'
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 476
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
    Left = 172
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
  object Label4: TLabel
    Left = 16
    Top = 368
    Width = 73
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Custom Colors:'
  end
  object Bevel1: TBevel
    Left = 96
    Top = 376
    Width = 361
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object JvColorComboBox1: TJvColorComboBox
    Left = 16
    Top = 24
    Width = 145
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
    ColorDialogText = '(Other...)'
    DroppedDownWidth = 145
    NewColorText = 'Custom Color %d'
    Options = [coText, coSysColors, coCustomColors]
    OnNewColor = JvColorComboBox1NewColor
    TabOrder = 0
    OnChange = JvColorComboBox1Change
  end
  object memInfo: TMemo
    Left = 16
    Top = 96
    Width = 433
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnColorNames: TButton
    Left = 16
    Top = 64
    Width = 90
    Height = 25
    Caption = 'Get Color Map:'
    TabOrder = 2
    OnClick = btnColorNamesClick
  end
  object edNameTemplate: TEdit
    Left = 172
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Custom Color %d'
  end
  object chkAllowCustom: TCheckBox
    Left = 173
    Top = 66
    Width = 113
    Height = 17
    Caption = 'Display Color Dialog'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = cbDisplayStyleChange
  end
  object btnViewCustom: TButton
    Left = 184
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'View'
    TabOrder = 5
    OnClick = btnViewCustomClick
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
  object btnSaveCustom: TButton
    Left = 24
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 7
    OnClick = btnSaveCustomClick
  end
  object btnLoadCustom: TButton
    Left = 104
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load'
    TabOrder = 8
    OnClick = btnLoadCustomClick
  end
  object btnClearCustom: TButton
    Left = 264
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 9
    OnClick = btnClearCustomClick
  end
end
