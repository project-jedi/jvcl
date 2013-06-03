object DataModule1: TDataModule1
  OldCreateOrder = True
  OnCreate = DataModuleCreate
  Left = 146
  Top = 110
  Height = 150
  Width = 171
  object Table1: TTable
    Active = True
    DatabaseName = 'DBDemos'
    TableName = 'EMPLOYEE.DB'
    Left = 25
    Top = 16
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 26
    Top = 68
  end
end
