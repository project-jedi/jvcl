object JvDebugLog: TJvDebugLog
  Left = 335
  Top = 166
  Width = 406
  Height = 287
  Caption = 'Debug log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object memDebug: TJvHLEditor
    Left = 0
    Top = 33
    Width = 398
    Height = 224
    Cursor = crIBeam
    GutterWidth = 0
    RightMarginColor = clSilver
    Completion.ItemHeight = 13
    Completion.Interval = 800
    Completion.ListBoxStyle = lbStandard
    Completion.CaretChar = '|'
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '3 5'
    SelForeColor = clHighlightText
    SelBackColor = clHighlight
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabStop = True
    UseDockManager = False
    Colors.Comment.Style = [fsItalic]
    Colors.Comment.ForeColor = clOlive
    Colors.Number.ForeColor = clNavy
    Colors.Strings.ForeColor = clPurple
    Colors.Symbol.ForeColor = clBlue
    Colors.Reserved.Style = [fsBold]
    Colors.Preproc.ForeColor = clGreen
    Colors.Statement.Style = [fsBold]
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 398
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object cbDebug: TCheckBox
      Left = 16
      Top = 8
      Width = 121
      Height = 17
      Caption = 'Enable Debug log'
      TabOrder = 0
    end
  end
end
