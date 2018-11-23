object JvDBActions: TJvDBActions
  OldCreateOrder = False
  Height = 150
  Width = 215
  object ActionList1: TActionList
    Left = 72
    Top = 8
    object JvDatabaseInsertAction1: TJvDatabaseInsertAction
      Category = 'JVCL DB'
      Caption = 'Insert Record'
      Enabled = False
      Hint = 'Insert a new record'
    end
    object JvDatabaseCopyAction1: TJvDatabaseCopyAction
      Category = 'JVCL DB'
      Caption = 'Copy Record'
      Enabled = False
      Hint = 'Copy the data of the current record into a new record'
    end
    object JvDatabaseEditAction1: TJvDatabaseEditAction
      Category = 'JVCL DB'
      Caption = 'Edit Record'
      Enabled = False
      Hint = 'Edit the current record'
    end
    object JvDatabaseDeleteAction1: TJvDatabaseDeleteAction
      Category = 'JVCL DB'
      Caption = 'Delete Record'
      Enabled = False
      Hint = 'Delete the current record'
    end
    object JvDatabasePostAction1: TJvDatabasePostAction
      Category = 'JVCL DB'
      Caption = 'Post Changes'
      Enabled = False
      Hint = 'Post the changes of the current record to the database'
    end
    object JvDatabaseCancelAction1: TJvDatabaseCancelAction
      Category = 'JVCL DB'
      Caption = 'Cancel Changes'
      Enabled = False
      Hint = 'Cancel the changes of the current record'
    end
    object JvDatabaseSingleRecordWindowAction1: TJvDatabaseSingleRecordWindowAction
      Category = 'JVCL DB'
      Caption = 'Single Record Window'
      Enabled = False
      Hint = 'Show a Single Record Window with fields of the result set'
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
    end
    object JvDatabaseFirstAction1: TJvDatabaseFirstAction
      Category = 'JVCL DB'
      Caption = 'First Record'
      Enabled = False
      Hint = 'Navigate to the first record of the result set'
      ImageIndex = 8
    end
    object JvDatabaseLastAction1: TJvDatabaseLastAction
      Category = 'JVCL DB'
      Caption = 'Last Record'
      Enabled = False
      Hint = 'Navigate to the last record of the result set'
      ImageIndex = 12
    end
    object JvDatabaseNextAction1: TJvDatabaseNextAction
      Category = 'JVCL DB'
      Caption = 'Next Record'
      Enabled = False
      Hint = 'Navigate to the next record of the result set'
      ImageIndex = 13
    end
    object JvDatabasePriorAction1: TJvDatabasePriorAction
      Category = 'JVCL DB'
      Caption = 'Prior Record'
      Enabled = False
      Hint = 'Navigate to the prior record of the result set'
      ImageIndex = 16
    end
    object JvDatabaseNextBlockAction1: TJvDatabaseNextBlockAction
      Category = 'JVCL DB'
      Caption = 'Next Record Block'
      Enabled = False
      Hint = 'Navigate to the next record block of the result set'
      ImageIndex = 14
    end
    object JvDatabasePriorBlockAction1: TJvDatabasePriorBlockAction
      Category = 'JVCL DB'
      Caption = 'Prior Record Block'
      Enabled = False
      Hint = 'Navigate to the prior record block of the result set'
      ImageIndex = 17
    end
    object JvDatabasePositionAction1: TJvDatabasePositionAction
      Category = 'JVCL DB'
      Caption = ' - / - '
      Enabled = False
    end
    object JvDatabaseRefreshAction1: TJvDatabaseRefreshAction
      Category = 'JVCL DB'
      Caption = 'Refresh'
      Enabled = False
      Hint = 'Refresh the result set'
      ImageIndex = 18
    end
    object JvDatabaseSimpleAction1: TJvDatabaseSimpleAction
      Category = 'JVCL DB'
      Caption = 'JvDatabaseSimpleAction1'
      Enabled = False
    end
    object JvDatabaseOpenAction1: TJvDatabaseOpenAction
      Category = 'JVCL DB'
      Caption = 'Open'
      Enabled = False
      Hint = 'Open the result set'
    end
    object JvDatabaseCloseAction1: TJvDatabaseCloseAction
      Category = 'JVCL DB'
      Caption = 'Close'
      Enabled = False
      Hint = 'Close the result set'
    end
    object JvDatabaseModifyAllAction1: TJvDatabaseModifyAllAction
      Category = 'JVCL DB'
      Caption = 'Modify All'
      Enabled = False
      Hint = 'Modify all records of the result set'
    end
    object JvDatabaseShowSQLStatementAction1: TJvDatabaseShowSQLStatementAction
      Category = 'JVCL DB'
      Caption = 'Show SQL Statement'
      Enabled = False
      Hint = 'Show the SQL statement of the result set'
    end
  end
end
