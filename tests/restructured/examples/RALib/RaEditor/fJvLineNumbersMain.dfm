object LineNumbersMain: TJvLineNumbersMain 
  Left = 156
  Top = 127
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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RAHLEditor1: TJvEditor
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
    OnPaintGutter = RAHLEditor1PaintGutter
    Align = alClient
    Ctl3D = True
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 550
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'This demo shows how to paint line numbers on the gutter'
    TabOrder = 1
    object GutterFont: TLabel
      Left = 8
      Top = 16
      Width = 60
      Height = 13
      Caption = 'GutterFont'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
  end
end
