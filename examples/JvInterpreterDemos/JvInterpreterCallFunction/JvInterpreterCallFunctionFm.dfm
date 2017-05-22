object Form1: TForm1
  Left = 192
  Top = 66
  Width = 965
  Height = 690
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    949
    654)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 476
    Top = 20
    Width = 33
    Height = 13
    Caption = 'Result:'
  end
  object JvHLEditor1: TJvHLEditor
    Left = 24
    Top = 40
    Width = 913
    Height = 605
    Cursor = crIBeam
    Lines.Strings = (
      'unit UserFunctions;'
      ''
      
        '// sample of a user-created-library of jvinterpreter functions t' +
        'hat your compiled'
      '// program might access:'
      ''
      ''
      
        '// notice that there is no interface/implementation section in t' +
        'his '
      '// interpreter-only unit.'
      ''
      'function MyFunction(B:String):Integer;'
      'begin'
      '  result := Length(B);'
      'end;'
      ''
      'function MyFunction2(A,B:Integer):Integer;'
      'begin'
      '  result := A+B;'
      'end;'
      ''
      ''
      'end.')
    GutterWidth = 0
    RightMarginColor = clSilver
    ReadOnly = True
    Completion.ItemHeight = 13
    Completion.Interval = 800
    Completion.ListBoxStyle = lbStandard
    Completion.CaretChar = '|'
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '3 5'
    BracketHighlighting.StringEscape = #39#39
    SelForeColor = clHighlightText
    SelBackColor = clHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabStop = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    UseDockManager = False
    Colors.Comment.Style = [fsItalic]
    Colors.Comment.ForeColor = clOlive
    Colors.Comment.BackColor = clWindow
    Colors.Number.ForeColor = clNavy
    Colors.Number.BackColor = clWindow
    Colors.Strings.ForeColor = clPurple
    Colors.Strings.BackColor = clWindow
    Colors.Symbol.ForeColor = clBlue
    Colors.Symbol.BackColor = clWindow
    Colors.Reserved.Style = [fsBold]
    Colors.Reserved.ForeColor = clWindowText
    Colors.Reserved.BackColor = clWindow
    Colors.Identifier.ForeColor = clWindowText
    Colors.Identifier.BackColor = clWindow
    Colors.Preproc.ForeColor = clGreen
    Colors.Preproc.BackColor = clWindow
    Colors.FunctionCall.ForeColor = clWindowText
    Colors.FunctionCall.BackColor = clWindow
    Colors.Declaration.ForeColor = clWindowText
    Colors.Declaration.BackColor = clWindow
    Colors.Statement.Style = [fsBold]
    Colors.Statement.ForeColor = clWindowText
    Colors.Statement.BackColor = clWindow
    Colors.PlainText.ForeColor = clWindowText
    Colors.PlainText.BackColor = clWindow
  end
  object Button1: TButton
    Left = 36
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Call MyFunction'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 164
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Call MyFunction2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 520
    Top = 16
    Width = 121
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
  object JvInterpreterProgram1: TJvInterpreterProgram
    OnGetValue = JvInterpreterProgram1GetValue
    Left = 344
    Top = 8
  end
end
