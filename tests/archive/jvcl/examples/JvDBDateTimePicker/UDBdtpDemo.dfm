object Form1: TForm1
  Left = 250
  Top = 183
  Width = 534
  Height = 375
  Caption = 'Jedi Date Time Picker Demo www.delphi-jedi.org'
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
    Left = 10
    Top = 118
    Width = 503
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
    Left = 10
    Top = 28
    Width = 186
    Height = 21
    CalAlignment = dtaLeft
    Date = 35231.6837020255
    Time = 35231.6837020255
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkDate
    ParseInput = False
    TabOrder = 2
    DataField = 'Event_Date'
    DataSource = DataSource1
  end
  object JvDBDateTimePicker2: TJvDBDateTimePicker
    Left = 202
    Top = 28
    Width = 186
    Height = 21
    CalAlignment = dtaLeft
    Date = 2.68370202550068
    Time = 2.68370202550068
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkTime
    ParseInput = False
    TabOrder = 3
    DataField = 'Event_Time'
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
    TableName = 'events.db'
    Left = 458
    Top = 22
  end
end
