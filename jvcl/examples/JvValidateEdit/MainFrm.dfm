object frmValidateEditDemo: TfrmValidateEditDemo
  Left = 304
  Top = 186
  BorderStyle = bsDialog
  Caption = 'JvValidateEdit Demo'
  ClientHeight = 402
  ClientWidth = 384
  Color = clBtnFace
  Constraints.MinHeight = 170
  Constraints.MinWidth = 325
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
  object Label2: TLabel
    Left = 8
    Top = 248
    Width = 63
    Height = 13
    Caption = 'Validate Edit:'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 369
    Height = 233
    Caption = 'Properties'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 75
      Height = 13
      Caption = 'Display Format:'
    end
    object Label3: TLabel
      Left = 8
      Top = 112
      Width = 64
      Height = 13
      Caption = 'Check Chars:'
    end
    object Label4: TLabel
      Left = 232
      Top = 88
      Width = 69
      Height = 13
      Caption = 'Decimal Places'
    end
    object Label6: TLabel
      Left = 8
      Top = 64
      Width = 65
      Height = 13
      Caption = 'Display Prefix'
    end
    object Label7: TLabel
      Left = 8
      Top = 88
      Width = 65
      Height = 13
      Caption = 'Display Suffix'
    end
    object cbDisplayFormat: TComboBox
      Left = 8
      Top = 32
      Width = 193
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbDisplayFormatChange
      OnKeyPress = cbDisplayFormatKeyPress
    end
    object chkHasMaxValue: TCheckBox
      Left = 216
      Top = 40
      Width = 89
      Height = 17
      Caption = 'Has Maximum'
      TabOrder = 1
      OnClick = chkHasMaxValueClick
    end
    object chkHasMinValue: TCheckBox
      Left = 216
      Top = 64
      Width = 89
      Height = 17
      Caption = 'Has Minimum'
      TabOrder = 2
      OnClick = chkHasMinValueClick
    end
    object seDecimalPlaces: TSpinEdit
      Left = 312
      Top = 88
      Width = 49
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = seDecimalPlacesChange
    end
    object chkZeroEmpty: TCheckBox
      Left = 216
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Empty if Zero?'
      TabOrder = 4
      OnClick = chkZeroEmptyClick
    end
    object edCheckChars: TEdit
      Left = 8
      Top = 128
      Width = 297
      Height = 21
      TabOrder = 5
    end
    object btnCheckChars: TButton
      Left = 312
      Top = 128
      Width = 49
      Height = 21
      Caption = 'Set'
      TabOrder = 6
      OnClick = btnCheckCharsClick
    end
    object edDisplayPrefix: TEdit
      Left = 88
      Top = 64
      Width = 57
      Height = 21
      TabOrder = 7
    end
    object edDisplaySuffix: TEdit
      Left = 88
      Top = 88
      Width = 57
      Height = 21
      TabOrder = 8
    end
    object btnSetDisplayPrefix: TButton
      Left = 152
      Top = 64
      Width = 47
      Height = 21
      Caption = 'Set'
      TabOrder = 9
      OnClick = btnSetDisplayPrefixClick
    end
    object btnSetDisplaySuffix: TButton
      Left = 152
      Top = 88
      Width = 47
      Height = 21
      Caption = 'Set'
      TabOrder = 10
      OnClick = btnSetDisplaySuffixClick
    end
    object edMaxValue: TEdit
      Left = 312
      Top = 40
      Width = 49
      Height = 21
      TabOrder = 11
      OnExit = edMaxValueExit
    end
    object edMinValue: TEdit
      Left = 312
      Top = 64
      Width = 49
      Height = 21
      TabOrder = 12
      OnExit = edMinValueExit
    end
    object GroupBox3: TGroupBox
      Left = 8
      Top = 152
      Width = 353
      Height = 73
      Caption = 'Critical Points'
      TabOrder = 13
      object Label8: TLabel
        Left = 104
        Top = 16
        Width = 31
        Height = 13
        Caption = 'Above'
      end
      object Label9: TLabel
        Left = 200
        Top = 16
        Width = 31
        Height = 13
        Caption = 'Colour'
      end
      object Label10: TLabel
        Left = 104
        Top = 40
        Width = 28
        Height = 13
        Caption = 'Below'
      end
      object Label11: TLabel
        Left = 200
        Top = 40
        Width = 31
        Height = 13
        Caption = 'Colour'
      end
      object Label12: TLabel
        Left = 8
        Top = 24
        Width = 65
        Height = 13
        Caption = 'Check Points:'
      end
      object edCPMaxValue: TEdit
        Left = 144
        Top = 16
        Width = 49
        Height = 21
        TabOrder = 0
        OnExit = edCPMaxValueExit
      end
      object colCPAbove: TJvColorComboBox
        Left = 240
        Top = 16
        Width = 105
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
        DroppedDownWidth = 105
        NewColorText = 'Custom'
        TabOrder = 1
      end
      object edCPMinValue: TEdit
        Left = 144
        Top = 40
        Width = 49
        Height = 21
        TabOrder = 2
        OnExit = edCPMinValueExit
      end
      object colCPBelow: TJvColorComboBox
        Left = 240
        Top = 40
        Width = 105
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
        DroppedDownWidth = 105
        NewColorText = 'Custom'
        TabOrder = 3
      end
      object cbCPCheckPoints: TComboBox
        Left = 8
        Top = 40
        Width = 89
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = cbCPCheckPointsChange
      end
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 296
    Width = 369
    Height = 97
    Caption = 'Set To'
    TabOrder = 1
    object Label5: TLabel
      Left = 8
      Top = 16
      Width = 26
      Height = 13
      Caption = 'Value'
    end
    object edSetTo: TEdit
      Left = 48
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object rgSetToType: TRadioGroup
      Left = 8
      Top = 40
      Width = 353
      Height = 49
      Caption = 'as Type'
      Columns = 4
      ItemIndex = 3
      Items.Strings = (
        'Currency'
        'Float'
        'Integer'
        'Text')
      TabOrder = 1
    end
    object btnSetTo: TButton
      Left = 176
      Top = 16
      Width = 75
      Height = 21
      Caption = 'Set'
      Default = True
      TabOrder = 2
      OnClick = btnSetToClick
    end
    object chkAsVariant: TCheckBox
      Left = 256
      Top = 16
      Width = 97
      Height = 17
      Caption = 'as Variant'
      TabOrder = 3
    end
  end
  object chkValueChanged: TCheckBox
    Left = 216
    Top = 248
    Width = 153
    Height = 17
    Caption = 'Message on Value Change?'
    TabOrder = 2
  end
  object JvValidateEdit: TJvValidateEdit
    Left = 8
    Top = 264
    Width = 369
    Height = 21
    CheckChars = '01234567890'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    PasswordChar = #0
    TabOrder = 3
    Text = '0'
    Value = 0
    OnCustomValidate = JvValidateEditCustomValidate
    OnValueChanged = JvValidateEditValueChanged
  end
end
