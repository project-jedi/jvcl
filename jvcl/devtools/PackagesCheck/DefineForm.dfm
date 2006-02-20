object DefForm: TDefForm
  Left = 394
  Top = 294
  Caption = 'Define edition'
  ClientHeight = 443
  ClientWidth = 472
  Color = clBtnFace
  Constraints.MinHeight = 470
  Constraints.MinWidth = 464
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    472
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelSymbols: TLabel
    Left = 8
    Top = 40
    Width = 43
    Height = 13
    Caption = '&Symbols:'
    FocusControl = MemoSymbols
  end
  object LabelTarget: TLabel
    Left = 10
    Top = 11
    Width = 36
    Height = 13
    Caption = 'Target:'
    FocusControl = ComboBoxTargets
  end
  object LabelIncludeDirs: TLabel
    Left = 232
    Top = 40
    Width = 104
    Height = 13
    Caption = '&Directories to include:'
    FocusControl = MemoIncludeDirs
  end
  object LabelUnits: TLabel
    Left = 8
    Top = 199
    Width = 96
    Height = 13
    Caption = 'P&ackages and units:'
    FocusControl = JvTreeViewUnits
  end
  object MemoSymbols: TMemo
    Left = 8
    Top = 59
    Width = 218
    Height = 134
    Anchors = [akLeft, akTop, akRight]
    HideSelection = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object ComboBoxTargets: TComboBox
    Left = 66
    Top = 8
    Width = 139
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = ComboBoxTargetsChange
  end
  object MemoIncludeDirs: TMemo
    Left = 232
    Top = 59
    Width = 217
    Height = 134
    Anchors = [akTop, akRight]
    HideSelection = False
    ScrollBars = ssVertical
    TabOrder = 2
    WordWrap = False
  end
  object JvTreeViewUnits: TJvTreeView
    Left = 8
    Top = 218
    Width = 331
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 3
    LineColor = clScrollBar
  end
  object ButtonLoadPackage: TButton
    Left = 345
    Top = 218
    Width = 103
    Height = 25
    Action = ActionLoadPackage
    Anchors = [akTop, akRight]
    TabOrder = 4
  end
  object ButtonAddPackage: TButton
    Left = 345
    Top = 249
    Width = 103
    Height = 25
    Action = ActionAddPackage
    Anchors = [akTop, akRight]
    TabOrder = 5
  end
  object ButtonAddUnit: TButton
    Left = 345
    Top = 280
    Width = 103
    Height = 25
    Action = ActionAddUnit
    Anchors = [akTop, akRight]
    TabOrder = 6
  end
  object ButtonDelete: TButton
    Left = 345
    Top = 311
    Width = 103
    Height = 25
    Action = ActionDelete
    Anchors = [akTop, akRight]
    TabOrder = 7
  end
  object ButtonClose: TButton
    Left = 345
    Top = 410
    Width = 103
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Close'
    ModalResult = 1
    TabOrder = 8
  end
  object ButtonAddTarget: TButton
    Left = 211
    Top = 8
    Width = 75
    Height = 25
    Action = ActionAddTarget
    TabOrder = 9
  end
  object ButtonDeleteTarget: TButton
    Left = 373
    Top = 8
    Width = 75
    Height = 25
    Action = ActionDeleteTarget
    TabOrder = 10
  end
  object ButtonRenameTarget: TButton
    Left = 292
    Top = 8
    Width = 75
    Height = 25
    Action = ActionRenameTarget
    TabOrder = 11
  end
  object ButtonRename: TButton
    Left = 344
    Top = 344
    Width = 105
    Height = 25
    Action = ActionRename
    Anchors = [akTop, akRight]
    TabOrder = 12
  end
  object ActionList: TActionList
    Left = 360
    Top = 376
    object ActionAddPackage: TAction
      Caption = 'Add &Package'
      OnExecute = ActionAddPackageExecute
      OnUpdate = ActionAddPackageUpdate
    end
    object ActionAddUnit: TAction
      Caption = 'Add &Unit'
      OnExecute = ActionAddUnitExecute
      OnUpdate = ActionAddUnitUpdate
    end
    object ActionDelete: TAction
      Caption = '&Delete'
      OnExecute = ActionDeleteExecute
      OnUpdate = ActionDeleteUpdate
    end
    object ActionLoadPackage: TAction
      Caption = '&Load Package'
      OnExecute = ActionLoadPackageExecute
      OnUpdate = ActionLoadPackageUpdate
    end
    object ActionRenameTarget: TAction
      Caption = '&Rename'
      OnExecute = ActionRenameTargetExecute
      OnUpdate = ActionRenameTargetUpdate
    end
    object ActionAddTarget: TAction
      Caption = '&Add'
      OnExecute = ActionAddTargetExecute
      OnUpdate = ActionAddTargetUpdate
    end
    object ActionDeleteTarget: TAction
      Caption = 'D&elete'
      OnExecute = ActionDeleteTargetExecute
      OnUpdate = ActionDeleteTargetUpdate
    end
    object ActionRename: TAction
      Caption = 'R&ename'
      OnExecute = ActionRenameExecute
      OnUpdate = ActionRenameUpdate
    end
  end
  object JvOpenDialogBpl: TJvOpenDialog
    DefaultExt = 'bpl'
    Filter = 'BPL files (*.bpl)|*.bpl|All files (*.*)|*.*'
    Options = [ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Height = 0
    Width = 0
    Left = 400
    Top = 376
  end
end
