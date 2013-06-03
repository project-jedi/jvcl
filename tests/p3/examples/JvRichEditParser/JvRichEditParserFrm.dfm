object frmMain: TfrmMain
  Left = 347
  Top = 126
  Width = 619
  Height = 641
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
    Left = 0
    Top = 271
    Width = 607
    Height = 5
    Cursor = crVSplit
    Align = alTop
  end
  object reOriginal: TRichEdit
    Left = 0
    Top = 0
    Width = 607
    Height = 271
    Align = alTop
    HideScrollBars = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object reParsed: TRichEdit
    Left = 0
    Top = 276
    Width = 607
    Height = 274
    Align = alClient
    HideScrollBars = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 550
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
      Caption = '&Parse'
      TabOrder = 0
      OnClick = btnParseClick
    end
    object btnLoad: TButton
      Left = 18
      Top = 10
      Width = 75
      Height = 25
      Caption = 'L&oad...'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnRecreate: TButton
      Left = 192
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Recreate'
      TabOrder = 2
      OnClick = btnRecreateClick
    end
    object chkWordwrap: TCheckBox
      Left = 282
      Top = 14
      Width = 97
      Height = 17
      Caption = '&Wordwrap'
      TabOrder = 3
      OnClick = chkWordwrapClick
    end
    object chkNoWrite: TCheckBox
      Left = 402
      Top = 12
      Width = 121
      Height = 17
      Caption = 'Don'#39't write back'
      TabOrder = 4
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 591
    Width = 607
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object OpenDialog1: TOpenDialog
    Filter = 'RTF files|*.rtf'
    InitialDir = '.'
    Left = 84
    Top = 426
  end
end
