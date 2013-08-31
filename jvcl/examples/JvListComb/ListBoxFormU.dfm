object fmListBox: TfmListBox
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'TJvListBox'
  ClientHeight = 443
  ClientWidth = 545
  Color = clBtnFace
  Constraints.MinHeight = 294
  Constraints.MinWidth = 414
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object pnl1: TPanel
    Left = 436
    Top = 0
    Width = 109
    Height = 443
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alRight
    TabOrder = 0
    ExplicitLeft = 302
    ExplicitHeight = 368
    DesignSize = (
      109
      443)
    object btnTextGen: TButton
      Left = 20
      Top = 407
      Width = 69
      Height = 24
      Anchors = [akRight, akBottom]
      Caption = 'Random...'
      TabOrder = 0
      OnClick = btnTextGenClick
      ExplicitTop = 332
    end
    object chkWW: TCheckBox
      Left = 12
      Top = 14
      Width = 81
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Wrap words'
      TabOrder = 1
      OnClick = chkWWClick
    end
    object cbcMain: TJvColorComboBox
      Left = 5
      Top = 61
      Width = 98
      Height = 19
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
      DroppedDownWidth = 98
      NewColorText = 'New Color '
      Options = [coText, coCustomColors]
      TabOrder = 2
      OnChange = cbcMainChange
    end
    object cbcAlt: TJvColorComboBox
      Left = 5
      Top = 95
      Width = 98
      Height = 19
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
      DroppedDownWidth = 98
      NewColorText = 'New Color '
      Options = [coText, coCustomColors]
      TabOrder = 3
      OnChange = cbcAltChange
    end
    object chkAlt: TCheckBox
      Left = 30
      Top = 116
      Width = 73
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Alternate'
      TabOrder = 4
      OnClick = chkAltClick
    end
    object chkOutline: TCheckBox
      Left = 12
      Top = 168
      Width = 73
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Outline Lines'
      TabOrder = 5
      OnClick = chkOutlineClick
    end
  end
  object pgcLBs: TPageControl
    Left = 0
    Top = 0
    Width = 436
    Height = 443
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ActivePage = tsJVCL
    Align = alClient
    TabOrder = 1
    TabPosition = tpBottom
    ExplicitWidth = 302
    ExplicitHeight = 368
    object tsJVCL: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'JediVCL Listbox'
      ExplicitWidth = 294
      ExplicitHeight = 343
      object lst1: TJvListBox
        Left = 0
        Top = 0
        Width = 428
        Height = 418
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        ItemHeight = 12
        Background.FillMode = bfmTile
        Background.Visible = False
        TabOrder = 0
        ExplicitWidth = 294
        ExplicitHeight = 343
      end
    end
    object tsVCL: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Standard ListBox'
      ImageIndex = 1
      ExplicitWidth = 294
      ExplicitHeight = 343
      object lst2: TListBox
        Left = 0
        Top = 0
        Width = 428
        Height = 418
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        ItemHeight = 12
        TabOrder = 0
        ExplicitWidth = 294
        ExplicitHeight = 343
      end
    end
  end
end
