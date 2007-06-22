object Form1: TForm1
  Left = 361
  Top = 113
  Width = 544
  Height = 375
  Caption = 'JEDI Date Time Picker Demo www.delphi-jedi.org'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 50
    Top = 110
    Width = 437
    Height = 227
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 50
    Top = 82
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 0
  end
  object JvDBDateTimePicker1: TJvDBDateTimePicker
    Left = 50
    Top = 28
    Width = 186
    Height = 21
    CalAlignment = dtaLeft
    Date = 32035.6837020255
    Time = 32035.6837020255
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkDate
    ParseInput = False
    TabOrder = 2
    DataField = 'PUR_DATE'
    DataSource = DataSource1
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 378
    Top = 34
  end
  object Table1: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    TableName = 'holdings.dbf'
    Left = 322
    Top = 22
  end
end
