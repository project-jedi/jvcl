object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 578
  ClientWidth = 918
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 918
    Height = 41
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 10
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 91
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Disonnect'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 205
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Open Query'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 286
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Close Query'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 485
      Top = 10
      Width = 91
      Height = 25
      Caption = 'Change Password'
      TabOrder = 4
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 136
    Width = 918
    Height = 374
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel2: TPanel
    Left = 0
    Top = 510
    Width = 918
    Height = 68
    Align = alBottom
    TabOrder = 2
    object BitBtn1: TBitBtn
      Left = 5
      Top = 6
      Width = 121
      Height = 25
      Action = JvDatabaseFirstAction1
      Caption = 'First Record'
      TabOrder = 0
    end
    object BitBtn2: TBitBtn
      Left = 123
      Top = 6
      Width = 121
      Height = 25
      Action = JvDatabasePriorAction1
      Caption = 'Prior Record'
      TabOrder = 1
    end
    object BitBtn3: TBitBtn
      Left = 365
      Top = 6
      Width = 121
      Height = 25
      Action = JvDatabaseLastAction1
      Caption = 'Last Record'
      TabOrder = 2
    end
    object BitBtn4: TBitBtn
      Left = 245
      Top = 6
      Width = 121
      Height = 25
      Action = JvDatabaseNextAction1
      Caption = 'Next Record'
      TabOrder = 3
    end
    object BitBtn5: TBitBtn
      Left = 485
      Top = 6
      Width = 91
      Height = 25
      Action = JvDatabasePositionAction1
      Caption = '0/0'
      TabOrder = 4
    end
    object BitBtn6: TBitBtn
      Left = 708
      Top = 6
      Width = 121
      Height = 25
      Action = JvDatabaseSingleRecordWindowAction1
      Caption = 'Single Record Window'
      TabOrder = 5
    end
    object BitBtn7: TBitBtn
      Left = 588
      Top = 37
      Width = 121
      Height = 25
      Action = JvDatabaseOpenAction1
      Caption = 'Open'
      TabOrder = 6
    end
    object BitBtn8: TBitBtn
      Left = 708
      Top = 37
      Width = 121
      Height = 25
      Action = JvDatabaseCloseAction1
      Caption = 'Close'
      TabOrder = 7
    end
    object BitBtn9: TBitBtn
      Left = 365
      Top = 37
      Width = 121
      Height = 25
      Action = JvDatabaseModifyAllAction1
      Caption = 'Modify All'
      TabOrder = 8
    end
    object BitBtn10: TBitBtn
      Left = 588
      Top = 6
      Width = 121
      Height = 25
      Action = JvDatabaseShowSQLStatementAction1
      Caption = 'Show SQL'
      TabOrder = 9
    end
    object BitBtn11: TBitBtn
      Left = 245
      Top = 37
      Width = 121
      Height = 25
      Action = JvDatabaseEditAction1
      Caption = 'Edit Record'
      TabOrder = 10
    end
    object BitBtn12: TBitBtn
      Left = 123
      Top = 37
      Width = 121
      Height = 25
      Action = JvDatabaseCopyAction1
      Caption = 'Copy Record'
      TabOrder = 11
    end
    object BitBtn13: TBitBtn
      Left = 5
      Top = 37
      Width = 121
      Height = 25
      Action = JvDatabaseInsertAction1
      Caption = 'Insert Record'
      TabOrder = 12
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 41
    Width = 918
    Height = 95
    Align = alTop
    Caption = 'Panel3'
    TabOrder = 3
    object SQLMemo: TMemo
      Left = 1
      Top = 1
      Width = 916
      Height = 93
      Align = alClient
      Lines.Strings = (
        'Select * from user_objects')
      TabOrder = 0
    end
  end
  object DataSource1: TDataSource
    DataSet = JvUniDacUniQuery1
    Left = 520
    Top = 110
  end
  object JvDatabaseActionList1: TJvDatabaseActionList
    DataComponent = DataSource1
    Left = 285
    Top = 170
    object JvDatabaseSingleRecordWindowAction1: TJvDatabaseSingleRecordWindowAction
      Category = 'JVCL-DB'
      Caption = 'Single Record Window'
      ImageIndex = 0
      DataComponent = DataSource1
      Options.PostButtonCaption = '&Post'
      Options.CancelButtonCaption = '&Cancel'
      Options.CloseButtonCaption = 'C&lose'
      Options.ArrangeConstraints.MaxHeight = 480
      Options.ArrangeConstraints.MaxWidth = 640
      Options.ArrangeSettings.BorderLeft = 3
      Options.ArrangeSettings.BorderTop = 3
      Options.ArrangeSettings.DistanceVertical = 3
      Options.ArrangeSettings.DistanceHorizontal = 3
      Options.ArrangeSettings.AutoSize = asBoth
      Options.FieldCreateOptions.UseFieldSizeForWidth = False
    end
    object JvDatabaseFirstAction1: TJvDatabaseFirstAction
      Category = 'JVCL-DB'
      Caption = 'First Record'
      ImageIndex = 8
      DataComponent = DataSource1
    end
    object JvDatabaseLastAction1: TJvDatabaseLastAction
      Category = 'JVCL-DB'
      Caption = 'Last Record'
      ImageIndex = 12
      DataComponent = DataSource1
    end
    object JvDatabaseNextAction1: TJvDatabaseNextAction
      Category = 'JVCL-DB'
      Caption = 'Next Record'
      ImageIndex = 13
      DataComponent = DataSource1
    end
    object JvDatabasePriorAction1: TJvDatabasePriorAction
      Category = 'JVCL-DB'
      Caption = 'Prior Record'
      ImageIndex = 16
      DataComponent = DataSource1
    end
    object JvDatabaseNextBlockAction1: TJvDatabaseNextBlockAction
      Category = 'JVCL-DB'
      Caption = 'Next Record Block'
      ImageIndex = 14
      DataComponent = DataSource1
    end
    object JvDatabasePriorBlockAction1: TJvDatabasePriorBlockAction
      Category = 'JVCL-DB'
      Caption = 'Prior Record Block'
      ImageIndex = 17
      DataComponent = DataSource1
    end
    object JvDatabasePositionAction1: TJvDatabasePositionAction
      Category = 'JVCL-DB'
      Caption = '0/0'
      DataComponent = DataSource1
    end
    object JvDatabaseRefreshAction1: TJvDatabaseRefreshAction
      Category = 'JVCL-DB'
      Caption = 'Refresh'
      ImageIndex = 18
      DataComponent = DataSource1
    end
    object JvDatabaseInsertAction1: TJvDatabaseInsertAction
      Category = 'JVCL-DB'
      Caption = 'Insert Record'
      DataComponent = DataSource1
      SingleRecordWindowAction = JvDatabaseSingleRecordWindowAction1
    end
    object JvDatabaseCopyAction1: TJvDatabaseCopyAction
      Category = 'JVCL-DB'
      Caption = 'Copy Record'
      DataComponent = DataSource1
      SingleRecordWindowAction = JvDatabaseSingleRecordWindowAction1
    end
    object JvDatabaseEditAction1: TJvDatabaseEditAction
      Category = 'JVCL-DB'
      Caption = 'Edit Record'
      DataComponent = DataSource1
      SingleRecordWindowAction = JvDatabaseSingleRecordWindowAction1
    end
    object JvDatabaseDeleteAction1: TJvDatabaseDeleteAction
      Category = 'JVCL-DB'
      Caption = 'Delete Record'
      DataComponent = DataSource1
    end
    object JvDatabasePostAction1: TJvDatabasePostAction
      Category = 'JVCL-DB'
      Caption = 'Post Changes'
      DataComponent = DataSource1
    end
    object JvDatabaseCancelAction1: TJvDatabaseCancelAction
      Category = 'JVCL-DB'
      Caption = 'Cancel Changes'
      DataComponent = DataSource1
    end
    object JvDatabaseSimpleAction1: TJvDatabaseSimpleAction
      Category = 'JVCL-DB'
      Caption = 'JvDatabaseSimpleAction1'
      DataComponent = DataSource1
    end
    object JvDatabaseOpenAction1: TJvDatabaseOpenAction
      Category = 'JVCL-DB'
      Caption = 'Open'
      DataComponent = DataSource1
    end
    object JvDatabaseCloseAction1: TJvDatabaseCloseAction
      Category = 'JVCL-DB'
      Caption = 'Close'
      DataComponent = DataSource1
    end
    object JvDatabaseModifyAllAction1: TJvDatabaseModifyAllAction
      Category = 'JVCL-DB'
      Caption = 'Modify All'
      DataComponent = DataSource1
    end
    object JvDatabaseShowSQLStatementAction1: TJvDatabaseShowSQLStatementAction
      Category = 'JVCL-DB'
      Caption = 'JvDatabaseShowSQLStatementAction1'
      DataComponent = DataSource1
    end
    object JvDatabaseSimpleAction2: TJvDatabaseSimpleAction
      Category = 'JVCL-DB'
      Caption = 'Age > 40'
      Enabled = False
      DataComponent = DataSource1
    end
    object JvDatabaseEditAction2: TJvDatabaseEditAction
      Category = 'JVCL-DB'
      Caption = 'Edit Record'
      DataComponent = DataSource1
    end
  end
  object UniConnection1: TUniConnection
    DataTypeMap = <>
    ConnectDialog = JvDBUniDacConnectDialog1
    Left = 455
    Top = 230
  end
  object AccessUniProvider1: TAccessUniProvider
    Left = 690
    Top = 145
  end
  object AdvantageUniProvider1: TAdvantageUniProvider
    Left = 685
    Top = 195
  end
  object ASEUniProvider1: TASEUniProvider
    Left = 685
    Top = 240
  end
  object DB2UniProvider1: TDB2UniProvider
    Left = 685
    Top = 285
  end
  object DBFUniProvider1: TDBFUniProvider
    Left = 685
    Top = 330
  end
  object InterBaseUniProvider1: TInterBaseUniProvider
    Left = 680
    Top = 375
  end
  object MySQLUniProvider1: TMySQLUniProvider
    Left = 815
    Top = 150
  end
  object ODBCUniProvider1: TODBCUniProvider
    Left = 815
    Top = 240
  end
  object OracleUniProvider1: TOracleUniProvider
    Left = 815
    Top = 285
  end
  object PostgreSQLUniProvider1: TPostgreSQLUniProvider
    Left = 815
    Top = 330
  end
  object SQLServerUniProvider1: TSQLServerUniProvider
    Left = 810
    Top = 375
  end
  object SQLiteUniProvider1: TSQLiteUniProvider
    Left = 805
    Top = 420
  end
  object JvUniDacUniQuery1: TJvUniDacUniQuery
    DataTypeMap = <>
    Connection = UniConnection1
    DialogOptions.FormStyle = fsNormal
    EnhancedOptions.AllowedContinueRecordFetchOptions.All = True
    ThreadOptions.LastInThread = True
    ThreadOptions.OpenInThread = True
    ThreadOptions.RefreshInThread = True
    Left = 455
    Top = 295
  end
  object JvDBUniDacConnectDialog1: TJvDBUniDacConnectDialog
    AppStorage = JvAppRegistryStorage1
    AppStoragePath = 'LogonDialog'
    Options.UsernameCaseSensitive = True
    Options.ShowAlias = True
    Options.ShowColors = True
    Options.ShowSavePasswords = True
    Options.ShowOracleHome = True
    Left = 455
    Top = 370
  end
  object JvAppRegistryStorage1: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    Root = 'Software\JVCL\Examples\JVDBUniDacComponents'
    SubStorages = <>
    Left = 450
    Top = 420
  end
end
