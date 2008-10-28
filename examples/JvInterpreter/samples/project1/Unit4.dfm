object DataModule1: TDataModule1
  OldCreateOrder = False
  Left = 130
  Top = 269
  Height = 203
  Width = 185
  object Table1: TTable
    DatabaseName = 'DBDemos'
    TableName = 'EMPLOYEE.DB'
    Left = 24
    Top = 24
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 24
    Top = 80
  end
end
