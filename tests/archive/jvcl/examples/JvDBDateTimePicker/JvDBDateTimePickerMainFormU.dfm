object JvDBDateTimePickerMainForm: TJvDBDateTimePickerMainForm
  Left = 285
  Top = 137
  Width = 582
  Height = 409
  Caption = 'JEDI Date Time Picker Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 27
    Width = 111
    Height = 26
    Caption = 'a JvDBDateTimePicker'#13#10'with kind dtkDate:'
  end
  object Label2: TLabel
    Left = 240
    Top = 28
    Width = 111
    Height = 26
    Caption = 'a JvDBDateTimePicker'#13#10'with kind dtkTime:'
  end
  object DBGrid1: TDBGrid
    Left = 42
    Top = 142
    Width = 503
    Height = 227
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 82
    Top = 98
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object JvDBDateTimePicker1: TJvDBDateTimePicker
    Left = 42
    Top = 60
    Width = 186
    Height = 21
    Date = 35231.569609768500000000
    Time = 35231.569609768500000000
    TabOrder = 2
    DropDownDate = 37579.000000000000000000
    NullText = '(none)'
    DataField = 'Event_Date'
    DataSource = DataSource1
  end
  object JvDBDateTimePicker2: TJvDBDateTimePicker
    Left = 234
    Top = 60
    Width = 186
    Height = 21
    Date = 2.569609768521330000
    Time = 2.569609768521330000
    Kind = dtkTime
    TabOrder = 3
    DropDownDate = 37579.000000000000000000
    NullText = '(none)'
    DataField = 'Event_Time'
    DataSource = DataSource1
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 426
    Top = 18
  end
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'events.db'
    Left = 458
    Top = 22
  end
end
