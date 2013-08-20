object DebugLog: TJvDebugLog 
  Left = 182
  Top = 170
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
    Height = 227
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
    Colors.Comment.Style = [fsItalic]
    Colors.Comment.ForeColor = clOlive
    Colors.Comment.BackColor = clWindow
    Colors.Number.Style = []
    Colors.Number.ForeColor = clNavy
    Colors.Number.BackColor = clWindow
    Colors.Strings.Style = []
    Colors.Strings.ForeColor = clPurple
    Colors.Strings.BackColor = clWindow
    Colors.Symbol.Style = []
    Colors.Symbol.ForeColor = clBlue
    Colors.Symbol.BackColor = clWindow
    Colors.Reserved.Style = [fsBold]
    Colors.Reserved.ForeColor = clWindowText
    Colors.Reserved.BackColor = clWindow
    Colors.Identifer.Style = []
    Colors.Identifer.ForeColor = clWindowText
    Colors.Identifer.BackColor = clWindow
    Colors.Preproc.Style = []
    Colors.Preproc.ForeColor = clGreen
    Colors.Preproc.BackColor = clWindow
    Colors.FunctionCall.Style = []
    Colors.FunctionCall.ForeColor = clWindowText
    Colors.FunctionCall.BackColor = clWindow
    Colors.Declaration.Style = []
    Colors.Declaration.ForeColor = clWindowText
    Colors.Declaration.BackColor = clWindow
    Colors.Statement.Style = [fsBold]
    Colors.Statement.ForeColor = clWindowText
    Colors.Statement.BackColor = clWindow
    Colors.PlainText.Style = []
    Colors.PlainText.ForeColor = clWindowText
    Colors.PlainText.BackColor = clWindow
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
  object RegAuto1: TJvRegAuto
    RegPath = 'Software\JVCL\Pas2JvInterpreter'
    IniFile = '$HOME/.NONAME'
    Props.Strings = (
      'cbDebug.Checked')
    SaveWindowPlace = True
    Left = 264
    Top = 64
  end
end
