object CsvDataSourceForm: TCsvDataSourceForm
  Left = 237
  Top = 144
  Caption = 'CSV DataSet Demo in Delphi 2009 (Unicode)'
  ClientHeight = 621
  ClientWidth = 802
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    802
    621)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 344
    Top = 109
    Width = 299
    Height = 13
    Caption = 'Date Display Format in Grid (does not affect saved CSV format):'
  end
  object DBNavigator1: TDBNavigator
    Left = 24
    Top = 103
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 9
    Top = 134
    Width = 785
    Height = 223
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'NAME'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ADDRESS'
        Width = 151
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ADDRESS2'
        Width = 140
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TELEPHONE'
        Width = 140
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'AGE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LASTPHONECALL'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PRIVATENUMBER'
        Visible = True
      end>
  end
  object Button1: TButton
    Left = 8
    Top = 363
    Width = 265
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'JvCsvDataSet1.AssignToStrings(Memo1.Lines)'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 553
    Top = 363
    Width = 173
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save Now. (aka Flush)'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 9
    Top = 394
    Width = 785
    Height = 168
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '<Click the VIEW button to view the results as a CSV ascii file.>')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 7
  end
  object Button3: TButton
    Left = 280
    Top = 363
    Width = 265
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'JvCsvDataSet1.AssignFromStrings(Memo1.Lines)'
    TabOrder = 4
    OnClick = Button3Click
  end
  object ComboBox1: TComboBox
    Left = 649
    Top = 106
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnChange = ComboBox1Change
    Items.Strings = (
      'yyyy-mm-dd hh:nn:ss'
      'yyyy-mm-dd hh:nn:ss am/pm'
      'mmm dd, yyyy hh:nn:ss am/pm'
      'mm/ddd/yyyy hh:nn:ss'
      'ddd mmm dd, yyyy hh:nn:ss'
      'ddd mmm dd, yyyy hh:nn:ss am/pm')
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 0
    Width = 802
    Height = 89
    TabStop = False
    Align = alTop
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Lines.Strings = (
      
        'This is the Unicode-Capable  (Delphi 2009) version of the CSV De' +
        'mo.')
    ParentFont = False
    ReadOnly = True
    TabOrder = 8
  end
  object Button4: TButton
    Left = 734
    Top = 363
    Width = 59
    Height = 25
    Caption = 'Load...'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 96
    Top = 568
    Width = 75
    Height = 25
    Caption = 'Font'
    TabOrder = 9
    OnClick = Button5Click
  end
  object DataSource1: TDataSource
    DataSet = JvCsvDataSet1
    Left = 56
    Top = 216
  end
  object JvCsvDataSet1: TJvCsvDataSet
    FileName = 'PhoneListUnicode.csv'
    Changed = False
    CsvFieldDef = 
      'NAME:~,ADDRESS:~,ADDRESS2:~,TELEPHONE:~,AGE:%,LASTPHONECALL:@,PR' +
      'IVATENUMBER:!'
    CsvUniqueKeys = False
    ExtendedHeaderInfo = False
    CaseInsensitive = False
    AutoBackupCount = 0
    StoreDefs = True
    AlwaysEnquoteStrings = False
    AppendOnly = False
    Left = 128
    Top = 216
    object JvCsvDataSet1NAME: TWideStringField
      FieldName = 'NAME'
      Size = 80
    end
    object JvCsvDataSet1ADDRESS: TWideStringField
      FieldName = 'ADDRESS'
      Size = 80
    end
    object JvCsvDataSet1ADDRESS2: TWideStringField
      FieldName = 'ADDRESS2'
      Size = 80
    end
    object JvCsvDataSet1TELEPHONE: TWideStringField
      FieldName = 'TELEPHONE'
      Size = 80
    end
    object JvCsvDataSet1AGE: TIntegerField
      FieldName = 'AGE'
    end
    object JvCsvDataSet1LASTPHONECALL: TDateTimeField
      FieldName = 'LASTPHONECALL'
    end
    object JvCsvDataSet1PRIVATENUMBER: TBooleanField
      FieldName = 'PRIVATENUMBER'
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'csv'
    Filter = 'CSV files (*.csv)|*.csv|all files (*.*)|*.*'
    Left = 200
    Top = 216
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 32
    Top = 568
  end
end
