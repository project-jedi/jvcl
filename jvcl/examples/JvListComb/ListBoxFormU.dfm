object fmListBox: TfmListBox
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'TJvListBox'
  ClientHeight = 490
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object pnl1: TPanel
    Left = 403
    Top = 0
    Width = 145
    Height = 490
    Align = alRight
    TabOrder = 0
    DesignSize = (
      145
      490)
    object btnTextGen: TButton
      Left = 27
      Top = 443
      Width = 92
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akRight, akBottom]
      Caption = 'Random...'
      TabOrder = 0
      OnClick = btnTextGenClick
    end
    object chkWW: TCheckBox
      Left = 16
      Top = 19
      Width = 108
      Height = 17
      Caption = 'Wrap words'
      TabOrder = 1
      OnClick = chkWWClick
    end
    object cbcMain: TJvColorComboBox
      Left = 7
      Top = 81
      Width = 130
      Height = 23
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      ColorNameMap.Strings = (
        'clBlack=Black'
        'clMaroon=Maroon'
        'clGreen=Green'
        'clOlive=Olive green'
        'clNavy=Navy blue'
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
        'clWhite=White'
        'clMoneyGreen=Money green'
        'clSkyBlue=Sky blue'
        'clCream=Cream'
        'clMedGray=Medium gray'
        'clScrollBar=Scrollbar'
        'clBackground=Desktop background'
        'clActiveCaption=Active window title bar'
        'clInactiveCaption=Inactive window title bar'
        'clMenu=Menu background'
        'clWindow=Window background'
        'clWindowFrame=Window frame'
        'clMenuText=Menu text'
        'clWindowText=Window text'
        'clCaptionText=Active window title bar text'
        'clActiveBorder=Active window border'
        'clInactiveBorder=Inactive window border'
        'clAppWorkSpace=Application workspace'
        'clHighlight=Selection background'
        'clHighlightText=Selection text'
        'clBtnFace=Button face'
        'clBtnShadow=Button shadow'
        'clGrayText=Dimmed text'
        'clBtnText=Button text'
        'clInactiveCaptionText=Inactive window title bar text'
        'clBtnHighlight=Button highlight'
        'cl3DDkShadow=Dark shadow 3D elements'
        'cl3DLight=Highlight 3D elements'
        'clInfoText=Tooltip text'
        'clInfoBk=Tooltip background'
        'clGradientActiveCaption=Gradient Active Caption'
        'clGradientInactiveCaption=Gradient Inactive Caption'
        'clHotLight=Hot Light'
        'clMenuBar=Menu Bar'
        'clMenuHighlight=Menu Highlight')
      ColorValue = clWhite
      ColorDialogText = '(Custom...)'
      DroppedDownWidth = 130
      NewColorText = 'New Color '
      Options = [coText, coCustomColors]
      TabOrder = 2
      OnChange = cbcMainChange
    end
    object cbcAlt: TJvColorComboBox
      Left = 7
      Top = 126
      Width = 130
      Height = 23
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      ColorNameMap.Strings = (
        'clBlack=Black'
        'clMaroon=Maroon'
        'clGreen=Green'
        'clOlive=Olive green'
        'clNavy=Navy blue'
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
        'clWhite=White'
        'clMoneyGreen=Money green'
        'clSkyBlue=Sky blue'
        'clCream=Cream'
        'clMedGray=Medium gray'
        'clScrollBar=Scrollbar'
        'clBackground=Desktop background'
        'clActiveCaption=Active window title bar'
        'clInactiveCaption=Inactive window title bar'
        'clMenu=Menu background'
        'clWindow=Window background'
        'clWindowFrame=Window frame'
        'clMenuText=Menu text'
        'clWindowText=Window text'
        'clCaptionText=Active window title bar text'
        'clActiveBorder=Active window border'
        'clInactiveBorder=Inactive window border'
        'clAppWorkSpace=Application workspace'
        'clHighlight=Selection background'
        'clHighlightText=Selection text'
        'clBtnFace=Button face'
        'clBtnShadow=Button shadow'
        'clGrayText=Dimmed text'
        'clBtnText=Button text'
        'clInactiveCaptionText=Inactive window title bar text'
        'clBtnHighlight=Button highlight'
        'cl3DDkShadow=Dark shadow 3D elements'
        'cl3DLight=Highlight 3D elements'
        'clInfoText=Tooltip text'
        'clInfoBk=Tooltip background'
        'clGradientActiveCaption=Gradient Active Caption'
        'clGradientInactiveCaption=Gradient Inactive Caption'
        'clHotLight=Hot Light'
        'clMenuBar=Menu Bar'
        'clMenuHighlight=Menu Highlight')
      ColorValue = clWhite
      ColorDialogText = '(Custom...)'
      DroppedDownWidth = 130
      NewColorText = 'New Color '
      Options = [coText, coCustomColors]
      TabOrder = 3
      OnChange = cbcAltChange
    end
    object chkAlt: TCheckBox
      Left = 40
      Top = 155
      Width = 97
      Height = 17
      Caption = 'Alternate'
      TabOrder = 4
      OnClick = chkAltClick
    end
    object chkOutline: TCheckBox
      Left = 16
      Top = 224
      Width = 97
      Height = 17
      Caption = 'chkOutline'
      TabOrder = 5
      OnClick = chkOutlineClick
    end
  end
  object pgcLBs: TPageControl
    Left = 0
    Top = 0
    Width = 403
    Height = 490
    ActivePage = tsJVCL
    Align = alClient
    TabOrder = 1
    TabPosition = tpBottom
    object tsJVCL: TTabSheet
      Caption = 'JediVCL Listbox'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lst1: TJvListBox
        Left = 0
        Top = 0
        Width = 395
        Height = 461
        Align = alClient
        IntegralHeight = True
        Background.FillMode = bfmTile
        Background.Visible = False
        TabOrder = 0
      end
    end
    object tsVCL: TTabSheet
      Caption = 'Standard ListBox'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lst2: TListBox
        Left = 0
        Top = 0
        Width = 395
        Height = 461
        Align = alClient
        TabOrder = 0
      end
    end
  end
end
