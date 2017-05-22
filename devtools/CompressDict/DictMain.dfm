object Form1: TForm1
  Left = 271
  Top = 227
  BorderStyle = bsSingle
  Caption = 'Compress dictionaries'
  ClientHeight = 78
  ClientWidth = 268
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Scaled = false
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Compress: TButton
    Left = 88
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Compress'
    TabOrder = 0
    OnClick = CompressClick
  end
  object Uncompress: TButton
    Left = 88
    Top = 48
    Width = 113
    Height = 25
    Caption = 'Uncompress'
    TabOrder = 1
    OnClick = UncompressClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dic'
    Left = 24
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'dic'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 224
    Top = 16
  end
end
