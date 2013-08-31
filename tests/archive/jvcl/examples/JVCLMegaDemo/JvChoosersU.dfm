object JvChoosersFrm: TJvChoosersFrm
  Left = 296
  Top = 80
  Width = 578
  Height = 526
  Caption = 'various chooser'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object JvLabel1: TJvLabel
    Left = 16
    Top = 43
    Width = 86
    Height = 16
    Caption = 'JvColorButton:'
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
    Width = 77
    Height = 13
    Caption = 'JvGammaPanel:'
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
    Width = 72
    Height = 13
    Caption = 'JvColorSquare:'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Label5: TLabel
    Left = 16
    Top = 120
    Width = 110
    Height = 16
    Caption = 'JvFontComboBox:'
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
    Width = 116
    Height = 16
    Caption = 'JvColorComboBox:'
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
  end
  object JvxLabel1: TJvLabel
    Left = 41
    Top = 156
    Width = 187
    Height = 13
    Caption = 'change the font in the FontComboBox !'
  end
  object Label2: TLabel
    Left = 24
    Top = 200
    Width = 116
    Height = 16
    Caption = 'JvColorComboBox:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label1: TLabel
    Left = 24
    Top = 360
    Width = 214
    Height = 16
    Caption = 'click me to Execute a JvColorDialog'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
    OnClick = Label1Click
  end
  object JvColorButton1: TJvColorButton
    Left = 144
    Top = 40
    Width = 81
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
    Left = 432
    Top = 8
    Width = 65
    Height = 250
    AutoSize = True
    OnChangeColor = JvGammaPanel1ChangeColor
  end
  object JvColorBox1: TJvColorBox
    Left = 48
    Top = 224
    Width = 78
    Height = 96
    BorderStyle = bsSingle
  end
  object JvColorDialog1: TJvColorDialog
    Left = 256
    Top = 208
  end
end
