object frmMain: TfrmMain
  Left = 213
  Top = 107
  Width = 619
  Height = 533
  BorderWidth = 2
  Caption = 'JvRichEditParser demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 271
    Top = 0
    Width = 5
    Height = 461
    Cursor = crHSplit
  end
  object reOriginal: TRichEdit
    Left = 0
    Top = 0
    Width = 271
    Height = 461
    Align = alLeft
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object reParsed: TRichEdit
    Left = 276
    Top = 0
    Width = 331
    Height = 461
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 461
    Width = 607
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnParse: TButton
      Left = 102
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Parse'
      TabOrder = 0
      OnClick = btnParseClick
    end
    object btnLoad: TButton
      Left = 18
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Load...'
      TabOrder = 1
      OnClick = btnLoadClick
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'RTF files|*.rtf'
    InitialDir = '.'
    Left = 84
    Top = 426
  end
end
