object MainForm: TMainForm
  Left = 198
  Top = 183
  Width = 253
  Height = 135
  Caption = 'Data Pump example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object go: TButton
    Left = 80
    Top = 40
    Width = 75
    Height = 25
    Caption = 'go'
    TabOrder = 0
    OnClick = goClick
  end
  object DataBase: TJvUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE'
      'password=masterkey'
      'user_name=SYSDBA')
    DatabaseName = 'D:\EMPLOYEE.DB'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 8
    Top = 8
  end
  object Transaction: TJvUIBTransaction
    DataBase = DataBase
    Left = 40
    Top = 8
  end
  object Query: TJvUIBQuery
    SQL.Strings = (
      'INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (?, ?)')
    Transaction = Transaction
    CachedFetch = False
    Left = 72
    Top = 8
  end
end
