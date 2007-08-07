object frmMain: TfrmMain
  Left = 353
  Top = 154
  Width = 517
  Height = 494
  Caption = 'GenDtx'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 509
    Height = 273
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 48
      Width = 73
      Height = 13
      Caption = '&Available Units:'
      FocusControl = lsbSource
    end
    object Label2: TLabel
      Left = 272
      Top = 48
      Width = 68
      Height = 13
      Caption = '&Process Units:'
      FocusControl = lsbDest
    end
    object lsbSource: TListBox
      Left = 8
      Top = 64
      Width = 233
      Height = 169
      Style = lbOwnerDrawFixed
      ItemHeight = 13
      MultiSelect = True
      Sorted = True
      TabOrder = 1
      OnDrawItem = lsbSourceDrawItem
    end
    object btnInclude: TButton
      Left = 246
      Top = 143
      Width = 21
      Height = 28
      Action = actInclude
      TabOrder = 4
    end
    object btnIncludeAll: TButton
      Left = 246
      Top = 85
      Width = 21
      Height = 28
      Action = actIncludeAll
      TabOrder = 2
    end
    object btnExclude: TButton
      Left = 246
      Top = 114
      Width = 21
      Height = 28
      Action = actExclude
      TabOrder = 3
    end
    object btnExcludeAll: TButton
      Left = 246
      Top = 172
      Width = 21
      Height = 28
      Action = actExcludeAll
      TabOrder = 5
    end
    object lsbDest: TListBox
      Left = 271
      Top = 64
      Width = 226
      Height = 169
      Style = lbOwnerDrawFixed
      ItemHeight = 13
      MultiSelect = True
      Sorted = True
      TabOrder = 6
      OnDrawItem = lsbDestDrawItem
    end
    object Button2: TButton
      Left = 400
      Top = 240
      Width = 97
      Height = 25
      Action = actAddToIgnoreUnitList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
    end
    object Button3: TButton
      Left = 272
      Top = 240
      Width = 121
      Height = 25
      Action = actAddToCompletedList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
    end
    object ToolBar2: TToolBar
      Left = 1
      Top = 1
      Width = 507
      Height = 29
      ButtonHeight = 21
      ButtonWidth = 93
      Caption = 'ToolBar1'
      ShowCaptions = True
      TabOrder = 0
      object ToolButton4: TToolButton
        Left = 0
        Top = 2
        Action = actShowCompleted
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton5: TToolButton
        Left = 93
        Top = 2
        Action = actShowIgnored
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton9: TToolButton
        Left = 186
        Top = 2
        Action = actShowGenerated
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton6: TToolButton
        Left = 279
        Top = 2
        Action = actShowOther
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton7: TToolButton
        Left = 372
        Top = 2
        Width = 21
        Caption = 'ToolButton7'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ToolButton8: TToolButton
        Left = 393
        Top = 2
        Action = actRefresh
        ParentShowHint = False
        ShowHint = True
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 273
    Width = 509
    Height = 167
    Align = alClient
    BevelOuter = bvLowered
    Caption = 'Panel2'
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 8
      Width = 51
      Height = 13
      Caption = '&Messages:'
      FocusControl = lsbMessages
    end
    object lsbMessages: TListBox
      Left = 1
      Top = 24
      Width = 507
      Height = 101
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
    end
    object Button4: TButton
      Left = 8
      Top = 135
      Width = 75
      Height = 25
      Action = actClearMessages
      Anchors = [akLeft, akBottom]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object Button5: TButton
      Left = 96
      Top = 135
      Width = 75
      Height = 25
      Action = actSaveMessages
      Anchors = [akLeft, akBottom]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object Button1: TButton
      Left = 184
      Top = 135
      Width = 105
      Height = 25
      Action = actCopyToClipboard
      Anchors = [akLeft, akBottom]
      TabOrder = 3
    end
  end
  object ActionList1: TActionList
    Left = 128
    Top = 248
    object actIncludeAll: TAction
      Category = 'Listboxes'
      Caption = '>>'
      OnExecute = actIncludeAllExecute
      OnUpdate = actIncludeAllUpdate
    end
    object actExcludeAll: TAction
      Category = 'Listboxes'
      Caption = '<<'
      OnExecute = actExcludeAllExecute
      OnUpdate = actExcludeAllUpdate
    end
    object actInclude: TAction
      Category = 'Listboxes'
      Caption = '>'
      OnExecute = actIncludeExecute
      OnUpdate = actIncludeUpdate
    end
    object actExclude: TAction
      Category = 'Listboxes'
      Caption = '<'
      OnExecute = actExcludeExecute
      OnUpdate = actExcludeUpdate
    end
    object actSettings: TAction
      Category = 'Options'
      Caption = '&Settings'
      ShortCut = 123
      OnExecute = actSettingsExecute
    end
    object actGenerateDtxFiles: TAction
      Category = 'Generate'
      Caption = 'Generate &Dtx Files'
      Hint = 
        'Generates *.dtx files from the files listed'#13#10'in the right list b' +
        'ox and puts them in the'#13#10'generated *.dtx directory'
      ShortCut = 117
      OnExecute = actGenerateDtxFilesExecute
      OnUpdate = actGenerateDtxFilesUpdate
    end
    object actAddToIgnoreUnitList: TAction
      Category = 'Listboxes'
      Caption = 'Add to Ignore List'
      Hint = 'Marks the selected files in the right list box as '#39'ignored'#39
      OnExecute = actAddToIgnoreUnitListExecute
      OnUpdate = SelectedProcessFilesAvailable
    end
    object actAddToCompletedList: TAction
      Category = 'Listboxes'
      Caption = 'Add to Completed List'
      Hint = 'Marks the selected files in the right list box as '#39'completed'#39
      OnExecute = actAddToCompletedListExecute
      OnUpdate = SelectedProcessFilesAvailable
    end
    object actUnitStatus: TAction
      Category = 'View'
      Caption = 'Unit Status'
      Hint = 
        'View the status of the dtx files, and enables you to'#13#10'update the' +
        ' status files, such as '#39'Completed units.txt'#39' etc.'
      OnExecute = actUnitStatusExecute
    end
    object actShowCompleted: TAction
      Category = 'View'
      Caption = 'Show &Completed'
      Hint = 
        'Shows the files in the *.pas directory that are marked as '#39'compl' +
        'eted'#39#13#10'These files are listed in the '#39'Completed units.txt'#39' file'
      OnExecute = actShowCompletedExecute
      OnUpdate = actShowCompletedUpdate
    end
    object actShowIgnored: TAction
      Category = 'View'
      Caption = 'Show &Ignored'
      Hint = 
        'Shows the files in the *.pas directory that are marked as '#39'ignor' +
        'ed'#39#13#10'These files are listed in the '#39'Ignored units.txt'#39' file'
      OnExecute = actShowIgnoredExecute
      OnUpdate = actShowIgnoredUpdate
    end
    object actShowOther: TAction
      Category = 'View'
      Caption = 'Show &Other'
      Checked = True
      Hint = 
        'Shows the files in the *.pas directory that are marked as '#39'other' +
        #39#13#10'These files are listed in the '#39'Other units.txt'#39' file'
      OnExecute = actShowOtherExecute
      OnUpdate = actShowOtherUpdate
    end
    object actRefresh: TAction
      Category = 'View'
      Caption = '&Refresh'
      Hint = 'Rescans the directory'
      ShortCut = 116
      OnExecute = actRefreshExecute
    end
    object actShowGenerated: TAction
      Category = 'View'
      Caption = 'Show &Generated'
      OnExecute = actShowGeneratedExecute
      OnUpdate = actShowGeneratedUpdate
    end
    object actCheckDtxFiles: TAction
      Category = 'Check'
      Caption = 'Check &Dtx Files'
      Hint = 'Checks the files in the real *.dtx directory'
      ShortCut = 112
      OnExecute = actCheckDtxFilesExecute
      OnUpdate = actCheckDtxFilesUpdate
    end
    object actClearGeneratedDtxDir: TAction
      Category = 'Generate'
      Caption = 'Clear Generated *.dtx Dir'
      OnExecute = actClearGeneratedDtxDirExecute
    end
    object actClearMessages: TAction
      Category = 'Messages'
      Caption = 'Clear'
      Hint = 'Clears the above message list box'
      OnExecute = actClearMessagesExecute
    end
    object actSaveMessages: TAction
      Category = 'Messages'
      Caption = 'Save'
      Hint = 'Saves the messages in the above list box to a file'
      OnExecute = actSaveMessagesExecute
    end
    object actGeneratePackageList: TAction
      Category = 'Generate'
      Caption = 'Generate &Package List'
      Hint = 'Updates the '#39'Files In Packages.txt'#39' file'
      ShortCut = 118
      OnExecute = actGeneratePackageListExecute
      OnUpdate = actGeneratePackageListUpdate
    end
    object actGenerateRegisteredClasses: TAction
      Category = 'Generate'
      Caption = 'Generate Registered &Components'
      ShortCut = 119
      OnExecute = actGenerateRegisteredClassesExecute
      OnUpdate = actGenerateRegisteredClassesUpdate
    end
    object actCheckPasFiles: TAction
      Category = 'Check'
      Caption = 'Check &Pas Files'
      ShortCut = 113
      OnExecute = actCheckPasFilesExecute
      OnUpdate = actCheckPasFilesUpdate
    end
    object actCheckCasePasFiles: TAction
      Category = 'Check'
      Caption = 'Check &Casing in Pas Files'
      ShortCut = 114
      OnExecute = actCheckCasePasFilesExecute
      OnUpdate = actCheckCasePasFilesUpdate
    end
    object actDirectories: TAction
      Category = 'Options'
      Caption = '&Directories'
      OnExecute = actDirectoriesExecute
    end
    object actCheckCasePasFilesAll: TAction
      Category = 'Check'
      Caption = 'Check &Casing in Pas Files (All)'
      ShortCut = 8306
      OnExecute = actCheckCasePasFilesAllExecute
      OnUpdate = actCheckCasePasFilesUpdate
    end
    object actCheckDuplicateTypes: TAction
      Category = 'Check'
      Caption = 'Check Duplicate Types'
      ShortCut = 115
      OnExecute = actCheckDuplicateTypesExecute
      OnUpdate = actCheckDuplicateTypesUpdate
    end
    object actGenerateList: TAction
      Category = 'Generate'
      Caption = 'Generate List'
      OnExecute = actGenerateListExecute
      OnUpdate = actGenerateListUpdate
    end
    object actCopyToClipboard: TAction
      Category = 'Messages'
      Caption = 'Copy to Clipboard'
      OnExecute = actCopyToClipboardExecute
    end
    object actAddToIgnoreTokenList: TAction
      Category = 'Messages'
      Caption = 'Add to Ignore List'
      Hint = 'Marks the selected tokens in the message list box as '#39'ignored'#39
    end
    object actGenerateClassStructure: TAction
      Category = 'Generate'
      Caption = 'Generate Class Structure'
      OnExecute = actGenerateClassStructureExecute
      OnUpdate = actGenerateClassStructureUpdate
    end
    object actSortPas: TAction
      Category = 'Generate'
      Caption = 'Sort Impl. Pas'
      OnExecute = actSortPasExecute
      OnUpdate = actSortPasUpdate
    end
    object actCheckDtxFilesDialog: TAction
      Category = 'Check'
      Caption = 'Check &Dtx Files (Dialog)'
      OnExecute = actCheckDtxFilesDialogExecute
      OnUpdate = actCheckDtxFilesUpdate
    end
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 248
    object View1: TMenuItem
      Caption = 'View'
      object ShowCompleted1: TMenuItem
        Action = actShowCompleted
      end
      object ShowIgnored1: TMenuItem
        Action = actShowIgnored
      end
      object ShowGenerated1: TMenuItem
        Action = actShowGenerated
      end
      object ShowOther1: TMenuItem
        Action = actShowOther
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Refresh1: TMenuItem
        Action = actRefresh
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object UnitStatus1: TMenuItem
        Action = actUnitStatus
      end
    end
    object Check: TMenuItem
      Caption = 'Check'
      object CheckDtxfiles1: TMenuItem
        Action = actCheckDtxFiles
      end
      object CheckDtxFilesDialog1: TMenuItem
        Action = actCheckDtxFilesDialog
      end
      object Checkpasfiles1: TMenuItem
        Action = actCheckPasFiles
      end
      object Checkcasepasfiles1: TMenuItem
        Action = actCheckCasePasFiles
      end
      object CheckCasinginPasFilesAll1: TMenuItem
        Action = actCheckCasePasFilesAll
      end
      object CheckDuplicateTypes1: TMenuItem
        Action = actCheckDuplicateTypes
      end
    end
    object Generate1: TMenuItem
      Caption = 'Generate'
      object GenerateDtxfiles1: TMenuItem
        Action = actGenerateDtxFiles
      end
      object GeneratePackage1: TMenuItem
        Action = actGeneratePackageList
      end
      object GenerateRegisteredClasses1: TMenuItem
        Action = actGenerateRegisteredClasses
      end
      object GenerateClassStructure1: TMenuItem
        Action = actGenerateClassStructure
      end
      object GenerateList1: TMenuItem
        Action = actGenerateList
      end
      object SortImplPas1: TMenuItem
        Action = actSortPas
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object ClearGenerateddtxDir1: TMenuItem
        Action = actClearGeneratedDtxDir
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object Directories1: TMenuItem
        Action = actDirectories
        ShortCut = 122
      end
      object Settings1: TMenuItem
        Action = actSettings
      end
    end
  end
  object JvAppRegistryStore1: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    Root = 'Software\JVCL\GenDtx'
    SubStorages = <>
    Left = 336
    Top = 136
  end
  object JvFormStorage1: TJvFormStorage
    AppStorage = JvAppRegistryStore1
    AppStoragePath = 'Main\Placement\'
    OnSavePlacement = JvFormStorage1SavePlacement
    OnRestorePlacement = JvFormStorage1RestorePlacement
    StoredValues = <>
    Left = 336
    Top = 168
  end
end
