object Form1: TForm1
  Left = 310
  Top = 275
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 144
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object JvFilenameBox1: TJvFilenameEdit
    Left = 18
    Top = 8
    Width = 307
    Height = 21
    ButtonFlat = False
    NumGlyphs = 1
    TabOrder = 0
  end
  object JvDirectoryBox1: TJvDirectoryEdit
    Left = 18
    Top = 30
    Width = 307
    Height = 21
    ButtonFlat = False
    NumGlyphs = 1
    TabOrder = 1
  end
  object JvButtonBox1: TJvComboEdit
    Left = 18
    Top = 74
    Width = 307
    Height = 21
    ButtonFlat = False
    GlyphKind = gkEllipsis
    NumGlyphs = 1
    TabOrder = 3
    OnButtonClick = JvButtonBox1ButtonClick
  end
  object JvDateEdit1: TJvDateEdit
    Left = 18
    Top = 53
    Width = 307
    Height = 21
    ButtonFlat = False
    NumGlyphs = 2
    TabOrder = 2
  end
  object JvCalcEdit1: TJvCalcEdit
    Left = 18
    Top = 96
    Width = 307
    Height = 21
    AutoSize = False
    NumGlyphs = 2
    TabOrder = 4
  end
end
