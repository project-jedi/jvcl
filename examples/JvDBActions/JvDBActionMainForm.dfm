object JvDBActionMainFrm: TJvDBActionMainFrm
  Left = 349
  Top = 48
  Width = 870
  Height = 640
  Caption = 'JvDBActionMainFrm'
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
  object DBGrid1: TDBGrid
    Left = 3
    Top = 30
    Width = 785
    Height = 223
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
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
    TabOrder = 2
  end
  object BitBtn2: TBitBtn
    Left = 135
    Top = 545
    Width = 121
    Height = 25
    Action = JvDatabasePriorAction1
    Caption = 'Prior Record'
    TabOrder = 3
  end
  object BitBtn3: TBitBtn
    Left = 375
    Top = 545
    Width = 121
    Height = 25
    Action = JvDatabaseLastAction1
    Caption = 'Last Record'
    TabOrder = 4
  end
  object BitBtn4: TBitBtn
    Left = 255
    Top = 545
    Width = 121
    Height = 25
    Action = JvDatabaseNextAction1
    Caption = 'Next Record'
    TabOrder = 5
  end
  object BitBtn5: TBitBtn
    Left = 503
    Top = 545
    Width = 121
    Height = 25
    Action = JvDatabasePositionAction1
    Caption = '0/0'
    TabOrder = 6
  end
  object BitBtn6: TBitBtn
    Left = 15
    Top = 570
    Width = 121
    Height = 25
    Action = JvDatabaseSingleRecordWindowAction1
    Caption = 'Single Record Window'
    TabOrder = 7
  end
  object JvDBGrid1: TJvDBGrid
    Left = 10
    Top = 260
    Width = 766
    Height = 261
    DataSource = DataSource1
    TabOrder = 8
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnEnter = JvDBGrid1Enter
    SortMarker = smUp
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
    Left = 135
    Top = 570
    Width = 121
    Height = 25
    Action = JvDatabaseOpenAction1
    Caption = 'Open'
    TabOrder = 9
  end
  object BitBtn8: TBitBtn
    Left = 255
    Top = 570
    Width = 121
    Height = 25
    Action = JvDatabaseCloseAction1
    Caption = 'Close'
    TabOrder = 10
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
    Changed = False
    CsvFieldDef = 
      'NAME,ADDRESS,ADDRESS2,TELEPHONE,AGE:%,LASTPHONECALL:@,PRIVATENUM' +
      'BER:!'
    CsvUniqueKeys = False
    ExtendedHeaderInfo = False
    CaseInsensitive = False
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
    Left = 690
    Top = 540
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
    end
    object JvDatabaseLastAction1: TJvDatabaseLastAction
      Category = 'JVCL-DB'
      Caption = 'Last Record'
      ImageIndex = 12
    end
    object JvDatabaseNextAction1: TJvDatabaseNextAction
      Category = 'JVCL-DB'
      Caption = 'Next Record'
      ImageIndex = 13
    end
    object JvDatabasePriorAction1: TJvDatabasePriorAction
      Category = 'JVCL-DB'
      Caption = 'Prior Record'
      ImageIndex = 16
    end
    object JvDatabaseNextBlockAction1: TJvDatabaseNextBlockAction
      Category = 'JVCL-DB'
      Caption = 'Next Record Block'
      ImageIndex = 14
    end
    object JvDatabasePriorBlockAction1: TJvDatabasePriorBlockAction
      Category = 'JVCL-DB'
      Caption = 'Prior Record Block'
      ImageIndex = 17
    end
    object JvDatabasePositionAction1: TJvDatabasePositionAction
      Category = 'JVCL-DB'
      Caption = '0/0'
    end
    object JvDatabaseRefreshAction1: TJvDatabaseRefreshAction
      Category = 'JVCL-DB'
      Caption = 'Refresh'
      ImageIndex = 18
    end
    object JvDatabaseInsertAction1: TJvDatabaseInsertAction
      Category = 'JVCL-DB'
      Caption = 'Insert Record'
    end
    object JvDatabaseCopyAction1: TJvDatabaseCopyAction
      Category = 'JVCL-DB'
      Caption = 'Copy Record'
    end
    object JvDatabaseEditAction1: TJvDatabaseEditAction
      Category = 'JVCL-DB'
      Caption = 'Edit Record'
    end
    object JvDatabaseDeleteAction1: TJvDatabaseDeleteAction
      Category = 'JVCL-DB'
      Caption = 'Delete Record'
    end
    object JvDatabasePostAction1: TJvDatabasePostAction
      Category = 'JVCL-DB'
      Caption = 'Post Changes'
    end
    object JvDatabaseCancelAction1: TJvDatabaseCancelAction
      Category = 'JVCL-DB'
      Caption = 'Cancel Changes'
    end
    object JvDatabaseSimpleAction1: TJvDatabaseSimpleAction
      Category = 'JVCL-DB'
      Caption = 'JvDatabaseSimpleAction1'
    end
    object JvDatabaseOpenAction1: TJvDatabaseOpenAction
      Category = 'JVCL-DB'
      Caption = 'Open'
    end
    object JvDatabaseCloseAction1: TJvDatabaseCloseAction
      Category = 'JVCL-DB'
      Caption = 'Close'
    end
  end
end
