object Test: TTest
  Left = 320
  Top = 121
  Width = 576
  Height = 447
  HelpContext = 999
  Caption = 'JvI2 demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 568
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 0
    OnResize = Panel1Resize
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 34
      Height = 13
      Caption = 'Source'
    end
    object Label3: TLabel
      Left = 8
      Top = 16
      Width = 72
      Height = 13
      Caption = 'Select example'
    end
    object ComboBox1: TComboBox
      Left = 88
      Top = 8
      Width = 473
      Height = 21
      Style = csDropDownList
      DropDownCount = 25
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBox1Change
      OnDropDown = ComboBox1DropDown
    end
  end
  object Memo1: TJvHLEditor
    Left = 0
    Top = 57
    Width = 360
    Height = 343
    Cursor = crIBeam
    GutterWidth = 0
    RightMarginVisible = False
    RightMargin = 0
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
    OnKeyDown = Memo1KeyDown
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
    Colors.Reserved.ForeColor = clBlack
    Colors.Identifier.ForeColor = clBlack
    Colors.Preproc.ForeColor = clGreen
    Colors.Statement.Style = [fsBold]
  end
  object Panel2: TPanel
    Left = 360
    Top = 57
    Width = 208
    Height = 343
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = ' '
    TabOrder = 2
    object Notebook1: TNotebook
      Left = 3
      Top = 240
      Width = 202
      Height = 100
      Align = alBottom
      PageIndex = 3
      TabOrder = 0
      object TPage
        Left = 0
        Top = 0
        Caption = 'Default'
        object Button1: TButton
          Left = 16
          Top = 16
          Width = 75
          Height = 23
          Caption = 'Run'
          TabOrder = 0
          OnClick = Button1Click
        end
        object Button5: TButton
          Left = 109
          Top = 16
          Width = 75
          Height = 23
          Caption = 'Compile'
          TabOrder = 1
          OnClick = Button1Click
        end
        object pnlTime: TPanel
          Left = 119
          Top = 60
          Width = 65
          Height = 18
          Alignment = taLeftJustify
          BevelOuter = bvLowered
          BorderWidth = 3
          Caption = ' '
          TabOrder = 2
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'ExternalForm'
        object bRunForm: TButton
          Left = 16
          Top = 16
          Width = 75
          Height = 23
          Caption = 'Run form...'
          TabOrder = 0
          OnClick = bRunFormClick
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'ExternalReport'
        object bRunReport: TButton
          Left = 16
          Top = 16
          Width = 75
          Height = 23
          Caption = 'Run report...'
          TabOrder = 0
          OnClick = bRunReportClick
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Event'
        object Button2: TButton
          Left = 16
          Top = 16
          Width = 75
          Height = 23
          Caption = 'Run'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Empty'
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'ExternalUnit'
        object Button3: TButton
          Left = 16
          Top = 16
          Width = 75
          Height = 23
          Caption = 'Run unit...'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
    object Memo2: TMemo
      Left = 3
      Top = 3
      Width = 202
      Height = 237
      Align = alClient
      Lines.Strings = (
        'Memo2')
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 400
    Width = 568
    Height = 20
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 2
    Caption = ' '
    TabOrder = 3
    object pnlResult: TPanel
      Left = 2
      Top = 2
      Width = 564
      Height = 16
      Align = alClient
      BevelOuter = bvLowered
      Caption = ' '
      TabOrder = 0
    end
  end
  object RegAuto1: TJvFormStorage
    StoredValues = <>
    Left = 248
    Top = 136
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Delphi units (*.pas)|*.pas|Delphi forms (*.dfm)|*.dfm|All files ' +
      '(*.*)|*.*'
    Title = 'Select delphi form'
    Left = 480
    Top = 104
  end
  object Table1: TTable
    DatabaseName = 'DBDemos'
    TableName = 'EMPLOYEE.DB'
    Left = 340
    Top = 209
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 340
    Top = 265
  end
  object JvInterpreterProgram1: TJvInterpreterFm
    OnGetValue = JvInterpreterProgram1GetValue
    OnGetUnitSource = JvInterpreterProgram1GetUnitSource
    OnStatement = JvInterpreterProgram1Statement
    OnGetDfmFileName = JvInterpreterProgram1GetDfmFileName
    Left = 260
    Top = 89
  end
  object RegAuto2: TJvFormStorage
    StoredValues = <>
    Left = 248
    Top = 208
  end
end
