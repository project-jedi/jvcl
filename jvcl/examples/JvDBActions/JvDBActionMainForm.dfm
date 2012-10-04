object JvDBActionMainFrm: TJvDBActionMainFrm
  Left = 349
  Top = 48
  Caption = 'JvDBActionMainFrm'
  ClientHeight = 701
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    862
    701)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 3
    Top = 30
    Width = 785
    Height = 318
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnEnter = DBGrid1Enter
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
  object DBNavigator1: TDBNavigator
    Left = 3
    Top = 6
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 15
    Top = 545
    Width = 121
    Height = 25
    Action = JvDatabaseFirstAction1
    Caption = 'First Record'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object BitBtn2: TBitBtn
    Left = 135
    Top = 545
    Width = 121
    Height = 25
    Action = JvDatabasePriorAction1
    Caption = 'Prior Record'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object BitBtn3: TBitBtn
    Left = 375
    Top = 545
    Width = 121
    Height = 25
    Action = JvDatabaseLastAction1
    Caption = 'Last Record'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 4
  end
  object BitBtn4: TBitBtn
    Left = 255
    Top = 545
    Width = 121
    Height = 25
    Action = JvDatabaseNextAction1
    Caption = 'Next Record'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 5
  end
  object BitBtn5: TBitBtn
    Left = 720
    Top = 570
    Width = 91
    Height = 25
    Action = JvDatabasePositionAction1
    Caption = '0/0'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 6
  end
  object BitBtn6: TBitBtn
    Left = 593
    Top = 545
    Width = 121
    Height = 25
    Action = JvDatabaseSingleRecordWindowAction1
    Caption = 'Single Record Window'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 7
  end
  object JvDBGrid1: TJvDBGrid
    Left = 8
    Top = 354
    Width = 590
    Height = 185
    DataSource = DataSource1
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit, dgMultiSelect, dgTitleClick, dgTitleHotTrack]
    TabOrder = 8
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnEnter = JvDBGrid1Enter
    SortMarker = smUp
    MultiSelect = True
    TitleButtons = True
    SelectColumn = scGrid
    SortedField = 'Name'
    TitleArrow = True
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.RealNamesOption = '[With the real field name]'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    EditControls = <>
    RowsHeight = 17
    TitleRowHeight = 17
  end
  object BitBtn7: TBitBtn
    Left = 15
    Top = 570
    Width = 121
    Height = 25
    Action = JvDatabaseOpenAction1
    Caption = 'Open'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 9
  end
  object BitBtn8: TBitBtn
    Left = 135
    Top = 570
    Width = 121
    Height = 25
    Action = JvDatabaseCloseAction1
    Caption = 'Close'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 10
  end
  object BitBtn9: TBitBtn
    Left = 255
    Top = 570
    Width = 121
    Height = 25
    Action = JvDatabaseModifyAllAction1
    Caption = 'Modify All'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 11
  end
  object BitBtn10: TBitBtn
    Left = 593
    Top = 570
    Width = 121
    Height = 25
    Action = JvDatabaseShowSQLStatementAction1
    Caption = 'Show SQL'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 15
  end
  object Button1: TButton
    Left = 720
    Top = 545
    Width = 91
    Height = 25
    Action = JvDatabaseSimpleAction2
    TabOrder = 16
  end
  object BitBtn11: TBitBtn
    Left = 255
    Top = 595
    Width = 121
    Height = 25
    Action = JvDatabaseEditAction1
    Caption = 'Edit Record'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 14
  end
  object BitBtn12: TBitBtn
    Left = 135
    Top = 595
    Width = 121
    Height = 25
    Action = JvDatabaseCopyAction1
    Caption = 'Copy Record'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 13
  end
  object BitBtn13: TBitBtn
    Left = 15
    Top = 595
    Width = 121
    Height = 25
    Action = JvDatabaseInsertAction1
    Caption = 'Insert Record'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 12
  end
  object BitBtn14: TBitBtn
    Left = 15
    Top = 645
    Width = 100
    Height = 25
    Action = JvControlCollapseAction1
    Caption = 'Collapse'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 17
  end
  object BitBtn15: TBitBtn
    Left = 113
    Top = 645
    Width = 100
    Height = 25
    Action = JvControlExpandAction1
    Caption = 'Expand'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 18
  end
  object BitBtn16: TBitBtn
    Left = 505
    Top = 645
    Width = 100
    Height = 25
    Action = JvControlCustomizeAction1
    Caption = 'Customize Control'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 22
  end
  object BitBtn17: TBitBtn
    Left = 604
    Top = 645
    Width = 100
    Height = 25
    Action = JvControlPrintAction1
    Caption = 'Print'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 23
  end
  object BitBtn18: TBitBtn
    Left = 407
    Top = 645
    Width = 100
    Height = 25
    Action = JvControlCustomizeColumnsAction1
    Caption = 'Customize Columns'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 21
  end
  object BitBtn19: TBitBtn
    Left = 212
    Top = 645
    Width = 100
    Height = 25
    Action = JvControlExportAction1
    Caption = 'Export Contents'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 19
  end
  object BitBtn20: TBitBtn
    Left = 309
    Top = 645
    Width = 100
    Height = 25
    Action = JvControlOptimizeColumnsAction1
    Caption = 'Optimize Columns'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 20
  end
  object JvCsvDataSet2: TJvCsvDataSet
    FieldDefs = <
      item
        Name = 'NAME'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'ADDRESS'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'ADDRESS2'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'TELEPHONE'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'AGE'
        DataType = ftInteger
      end
      item
        Name = 'LASTPHONECALL'
        DataType = ftDateTime
      end
      item
        Name = 'PRIVATENUMBER'
        DataType = ftBoolean
      end>
    Active = True
    FileName = 'PhoneList.csv'
    CsvFieldDef = 
      'NAME,ADDRESS,ADDRESS2,TELEPHONE,AGE:%,LASTPHONECALL:@,PRIVATENUM' +
      'BER:!'
    AutoBackupCount = 0
    StoreDefs = True
    Left = 123
    Top = 86
    object JvCsvDataSet1NAME: TStringField
      FieldName = 'NAME'
      Size = 80
    end
    object JvCsvDataSet1ADDRESS: TStringField
      FieldName = 'ADDRESS'
      Size = 80
    end
    object JvCsvDataSet1ADDRESS2: TStringField
      FieldName = 'ADDRESS2'
      ReadOnly = True
      Size = 80
    end
    object JvCsvDataSet1TELEPHONE: TStringField
      FieldName = 'TELEPHONE'
      Size = 40
    end
    object JvCsvDataSet1AGE: TIntegerField
      FieldName = 'AGE'
      ReadOnly = True
    end
    object JvCsvDataSet1LASTPHONECALL: TDateTimeField
      FieldName = 'LASTPHONECALL'
    end
    object JvCsvDataSet1PRIVATENUMBER: TBooleanField
      FieldName = 'PRIVATENUMBER'
    end
  end
  object DataSource1: TDataSource
    DataSet = JvCsvDataSet2
    Left = 51
    Top = 86
  end
  object JvDatabaseActionList1: TJvDatabaseActionList
    Left = 325
    Top = 420
    object JvDatabaseSingleRecordWindowAction1: TJvDatabaseSingleRecordWindowAction
      Category = 'JVCL-DB'
      Caption = 'Single Record Window'
      ImageIndex = 0
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
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseLastAction1: TJvDatabaseLastAction
      Category = 'JVCL-DB'
      Caption = 'Last Record'
      ImageIndex = 12
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseNextAction1: TJvDatabaseNextAction
      Category = 'JVCL-DB'
      Caption = 'Next Record'
      ImageIndex = 13
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabasePriorAction1: TJvDatabasePriorAction
      Category = 'JVCL-DB'
      Caption = 'Prior Record'
      ImageIndex = 16
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseNextBlockAction1: TJvDatabaseNextBlockAction
      Category = 'JVCL-DB'
      Caption = 'Next Record Block'
      ImageIndex = 14
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabasePriorBlockAction1: TJvDatabasePriorBlockAction
      Category = 'JVCL-DB'
      Caption = 'Prior Record Block'
      ImageIndex = 17
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabasePositionAction1: TJvDatabasePositionAction
      Category = 'JVCL-DB'
      Caption = '0/0'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseRefreshAction1: TJvDatabaseRefreshAction
      Category = 'JVCL-DB'
      Caption = 'Refresh'
      ImageIndex = 18
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseInsertAction1: TJvDatabaseInsertAction
      Category = 'JVCL-DB'
      Caption = 'Insert Record'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
      SingleRecordWindowAction = JvDatabaseSingleRecordWindowAction1
    end
    object JvDatabaseCopyAction1: TJvDatabaseCopyAction
      Category = 'JVCL-DB'
      Caption = 'Copy Record'
      OnExecute = JvDatabaseFirstAction1Execute
      SingleRecordWindowAction = JvDatabaseSingleRecordWindowAction1
    end
    object JvDatabaseEditAction1: TJvDatabaseEditAction
      Category = 'JVCL-DB'
      Caption = 'Edit Record'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
      SingleRecordWindowAction = JvDatabaseSingleRecordWindowAction1
    end
    object JvDatabaseDeleteAction1: TJvDatabaseDeleteAction
      Category = 'JVCL-DB'
      Caption = 'Delete Record'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabasePostAction1: TJvDatabasePostAction
      Category = 'JVCL-DB'
      Caption = 'Post Changes'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseCancelAction1: TJvDatabaseCancelAction
      Category = 'JVCL-DB'
      Caption = 'Cancel Changes'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseSimpleAction1: TJvDatabaseSimpleAction
      Category = 'JVCL-DB'
      Caption = 'JvDatabaseSimpleAction1'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseOpenAction1: TJvDatabaseOpenAction
      Category = 'JVCL-DB'
      Caption = 'Open'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseCloseAction1: TJvDatabaseCloseAction
      Category = 'JVCL-DB'
      Caption = 'Close'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseModifyAllAction1: TJvDatabaseModifyAllAction
      Category = 'JVCL-DB'
      Caption = 'Modify All'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseShowSQLStatementAction1: TJvDatabaseShowSQLStatementAction
      Category = 'JVCL-DB'
      Caption = 'JvDatabaseShowSQLStatementAction1'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseSimpleAction2: TJvDatabaseSimpleAction
      Category = 'JVCL-DB'
      Caption = 'Age > 40'
      Enabled = False
      OnExecute = JvDatabaseFirstAction1Execute
      OnCheckEnabled = JvDatabaseSimpleAction2CheckEnabled
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvDatabaseEditAction2: TJvDatabaseEditAction
      Category = 'JVCL-DB'
      Caption = 'Edit Record'
      OnExecute = JvDatabaseFirstAction1Execute
      AfterExecute = JvDatabaseFirstAction1AfterExecute
    end
    object JvControlCollapseAction1: TJvControlCollapseAction
      Category = 'JVCL-Control'
      Caption = 'Collapse'
    end
    object JvControlExpandAction1: TJvControlExpandAction
      Category = 'JVCL-Control'
      Caption = 'Expand'
    end
    object JvControlExportAction1: TJvControlExportAction
      Category = 'JVCL-Control'
      Caption = 'Export Contents'
    end
    object JvControlOptimizeColumnsAction1: TJvControlOptimizeColumnsAction
      Category = 'JVCL-Control'
      Caption = 'Optimize Columns'
    end
    object JvControlCustomizeColumnsAction1: TJvControlCustomizeColumnsAction
      Category = 'JVCL-Control'
      Caption = 'Customize Columns'
    end
    object JvControlPrintAction1: TJvControlPrintAction
      Category = 'JVCL-Control'
      Caption = 'Print'
    end
    object JvControlCustomizeAction1: TJvControlCustomizeAction
      Category = 'JVCL-Control'
      Caption = 'Customize Control'
      Hint = 'Customize the current control'
    end
  end
end
