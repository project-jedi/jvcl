object uilPlugin1: TuilPlugin1
  OldCreateOrder = False
  OnCreate = uilPlugin1Create
  Author = 'Tim Sullivan'
  Commands = <>
  Description = 'Sample Data-Aware plugin'
  Copyright = '(c) 1999, Unlimited Intelligence Limited'
  PluginID = 'UIL.Data Aware Sample'
  OnInitialize = uilPlugin1Initialize
  Left = 227
  Top = 147
  Height = 150
  Width = 215
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'biolife.db'
    Left = 28
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 104
    Top = 8
  end
end
