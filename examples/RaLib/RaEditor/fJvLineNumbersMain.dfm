object JvLineNumbersMain: TJvLineNumbersMain
  Left = 291
  Top = 217
  Width = 558
  Height = 375
  Caption = 'LineNumbersMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 550
    Height = 41
    Align = alTop
    Alignment = taRightJustify
    BevelOuter = bvNone
    Caption = 'This demo shows how to paint line numbers on the gutter     '
    TabOrder = 0
    object GutterFont: TLabel
      Left = 8
      Top = 16
      Width = 51
      Height = 14
      Caption = 'GutterFont'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object Button1: TButton
      Left = 10
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Open...'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object JvHLEditor1: TJvEditor
    Left = 0
    Top = 41
    Width = 550
    Height = 307
    Cursor = crIBeam
    GutterWidth = 32
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
    OnPaintGutter = JvHLEditor1PaintGutter
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
  end
  object OpenDialog1: TOpenDialog
    Left = 78
    Top = 78
  end
end
