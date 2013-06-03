object Form1: TForm1
  Left = 408
  Top = 144
  Width = 848
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvFillListBox1: TJvFillListBox
    Left = 16
    Top = 32
    Width = 132
    Height = 97
    Items = JvFontFiller1
    ItemHeight = 13
    TabOrder = 0
    FillerOptionsClass = 'TFontFillerOptions'
    FillerOptions.UseFontNames = True
  end
  object JvFillListBox2: TJvFillListBox
    Left = 175
    Top = 30
    Width = 132
    Height = 97
    Items = JvFontFiller1
    ItemHeight = 13
    TabOrder = 1
    FillerOptionsClass = 'TFontFillerOptions'
    FillerOptions.UseFontNames = False
  end
  object JvFontFiller1: TJvFontFiller
    Left = 384
    Top = 144
  end
end
