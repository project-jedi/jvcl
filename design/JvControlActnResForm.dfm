object JvControlActionsDM: TJvControlActionsDM
  OldCreateOrder = False
  Height = 150
  Width = 215
  object JvControlActionsList: TActionList
    Left = 90
    Top = 60
    object JvControlCollapseAction1: TJvControlCollapseAction
      Category = 'JVCL'
      Caption = 'Collapse'
    end
    object JvControlExpandAction1: TJvControlExpandAction
      Category = 'JVCL'
      Caption = 'Expand'
    end
    object JvControlExportAction1: TJvControlExportAction
      Category = 'JVCL'
      Caption = 'Export Contents'
    end
    object JvControlOptimizeColumnsAction1: TJvControlOptimizeColumnsAction
      Category = 'JVCL'
      Caption = 'Optimize Columns'
    end
    object JvControlCustomizeColumnsAction1: TJvControlCustomizeColumnsAction
      Category = 'JVCL'
      Caption = 'Customize Columns'
    end
    object JvControlPrintAction1: TJvControlPrintAction
      Category = 'JVCL'
      Caption = 'Print'
    end
    object JvControlCustomizeAction1: TJvControlCustomizeAction
      Category = 'JVCL'
      Caption = 'Customize Control'
      Hint = 'Customize the current control'
    end
  end
end
