object JvChoosersFrm: TJvChoosersFrm
  Left = 0
  Top = 0
  Width = 578
  Height = 526
  TabOrder = 0
  object JvLabel1: TJvLabel
    Left = 16
    Top = 43
    Width = 82
    Height = 16
    Caption = 'JvColorbutton'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvLabel2: TJvLabel
    Left = 336
    Top = 8
    Width = 74
    Height = 13
    Caption = 'JvGammaPanel'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvColorSquare1: TJvColorSquare
    Left = 336
    Top = 72
    Width = 73
    Height = 65
    BorderStyle = bsSingle
  end
  object JvLabel3: TJvLabel
    Left = 336
    Top = 48
    Width = 69
    Height = 13
    Caption = 'JvColorSquare'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Label5: TLabel
    Left = 16
    Top = 120
    Width = 107
    Height = 16
    Caption = 'JvFontComboBox'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object JvLabel4: TJvLabel
    Left = 16
    Top = 81
    Width = 113
    Height = 16
    Caption = 'JvColorComboBox'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvArrow1: TJvArrow
    Left = 352
    Top = 144
    Width = 65
    Height = 25
    Shape = atDownLeft
    ArrowSize = 5
    ArrowWidth = 5
  end
  object JvxLabel1: TJvxLabel
    Left = 41
    Top = 156
    Width = 187
    Height = 13
    Caption = 'change the font in the FontComboBox !'
  end
  object JvColorButton1: TJvColorButton
    Left = 144
    Top = 40
    Width = 81
    Height = 21
    OtherCaption = '&Other...'
    Options = []
  end
  object JvFontCombobox1: TJvFontComboBox
    Left = 144
    Top = 117
    Width = 145
    Height = 22
    FontName = 'Arial'
    ItemIndex = 0
    Options = [foWysiWyg]
    Sorted = True
    TabOrder = 1
    OnChange = JvFontCombobox1Change
  end
  object JvColorComboBox1: TJvColorComboBox
    Left = 144
    Top = 79
    Width = 145
    Height = 22
    ColorDialogText = '(Other...)'
    NewColorText = 'New Color '
    Options = [coText, coCustomColors]
    DroppedDownWidth = 145
    TabOrder = 2
  end
  object JvGammaPanel1: TJvGammaPanel
    Left = 424
    Top = 8
    Width = 65
    Height = 250
    AutoSize = True
    OnChangeColor = JvGammaPanel1ChangeColor
  end
end
