object Form1: TForm1
  Left = 387
  Top = 227
  Width = 548
  Height = 439
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object JvFillLabel1: TJvFillLabel
    Left = 20
    Top = 135
    Width = 82
    Height = 13
    Caption = 'JvFillLabel1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Filler = JvFontFiller1
    Index = 7
  end
  object JvFillListBox1: TJvFillListBox
    Left = 20
    Top = 5
    Width = 116
    Height = 121
    Filler = JvFontFiller1
    ItemHeight = 13
    Style = lbOwnerDrawVariable
    TabOrder = 0
  end
  object JvFillListBox2: TJvFillListBox
    Left = 155
    Top = 10
    Width = 116
    Height = 116
    Filler = JvStringsFiller1
    ItemHeight = 13
    TabOrder = 1
  end
  object btnEditStrings: TButton
    Left = 160
    Top = 135
    Width = 75
    Height = 25
    Caption = 'Edit strings'
    TabOrder = 2
    OnClick = btnEditStringsClick
  end
  object btnEditTree: TButton
    Left = 160
    Top = 165
    Width = 75
    Height = 25
    Caption = 'Edit tree'
    TabOrder = 3
    OnClick = btnEditTreeClick
  end
  object JvFontFiller1: TJvFontFiller
    Left = 300
    Top = 20
  end
  object JvStringsFiller1: TJvStringsFiller
    Strings.Strings = (
      'Line 1'
      'Line 2'
      'Line 3'
      'Line 4'
      'Line 5'
      'Line 6'
      'Line 7'
      'Line 8')
    Left = 300
    Top = 75
  end
end
