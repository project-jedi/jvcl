object Form1: TForm1
  Left = 318
  Top = 74
  Width = 503
  Height = 579
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lsbMessages: TListBox
    Left = 0
    Top = 413
    Width = 495
    Height = 91
    Align = alBottom
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 173
    Width = 495
    Height = 240
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 1
    object lsbSource: TListBox
      Left = 8
      Top = 40
      Width = 233
      Height = 161
      ItemHeight = 13
      MultiSelect = True
      Sorted = True
      TabOrder = 0
    end
    object btnInclude: TButton
      Left = 248
      Top = 124
      Width = 17
      Height = 25
      Action = actInclude
      TabOrder = 1
    end
    object btnIncludeAll: TButton
      Left = 248
      Top = 72
      Width = 17
      Height = 25
      Action = actIncludeAll
      TabOrder = 2
    end
    object btnExclude: TButton
      Left = 248
      Top = 98
      Width = 17
      Height = 25
      Action = actExclude
      TabOrder = 3
    end
    object btnExcludeAll: TButton
      Left = 248
      Top = 150
      Width = 17
      Height = 25
      Action = actExcludeAll
      TabOrder = 4
    end
    object lsbDest: TListBox
      Left = 271
      Top = 40
      Width = 217
      Height = 161
      ItemHeight = 13
      MultiSelect = True
      Sorted = True
      TabOrder = 5
    end
    object Button2: TButton
      Left = 400
      Top = 216
      Width = 97
      Height = 25
      Action = actAddToIgnoreList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object Button3: TButton
      Left = 272
      Top = 216
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
      Width = 493
      Height = 29
      ButtonHeight = 21
      ButtonWidth = 87
      Caption = 'ToolBar1'
      ShowCaptions = True
      TabOrder = 8
      object ToolButton4: TToolButton
        Left = 0
        Top = 2
        Action = actShowCompleted
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton5: TToolButton
        Left = 87
        Top = 2
        Action = actShowIgnored
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton9: TToolButton
        Left = 174
        Top = 2
        Action = actShowGenerated
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton6: TToolButton
        Left = 261
        Top = 2
        Action = actShowOther
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton7: TToolButton
        Left = 348
        Top = 2
        Width = 10
        Caption = 'ToolButton7'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ToolButton8: TToolButton
        Left = 358
        Top = 2
        Action = actRefresh
        ParentShowHint = False
        ShowHint = True
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 495
    Height = 57
    ButtonHeight = 21
    ButtonWidth = 144
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 2
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Action = actSettings
    end
    object ToolButton2: TToolButton
      Left = 144
      Top = 2
      Action = actUnitStatus
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton3: TToolButton
      Left = 288
      Top = 2
      Action = actGenerateDtxFiles
      ParentShowHint = False
      Wrap = True
      ShowHint = True
    end
    object ToolButton10: TToolButton
      Left = 0
      Top = 23
      Action = actCheckDtxFiles
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton11: TToolButton
      Left = 144
      Top = 23
      Action = actGeneratePackageList
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton12: TToolButton
      Left = 288
      Top = 23
      Action = actGenerateRegisteredClasses
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 57
    Width = 495
    Height = 116
    Align = alTop
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 3
    object lblInDirDesc: TLabel
      Left = 8
      Top = 8
      Width = 87
      Height = 13
      Caption = '*.pas directory:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblPasDir: TLabel
      Left = 8
      Top = 24
      Width = 41
      Height = 13
      Caption = 'lblPasDir'
    end
    object lblOutDirDesc: TLabel
      Left = 8
      Top = 41
      Width = 147
      Height = 13
      Caption = 'Generated *.dtx directory:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblGeneratedDtxDir: TLabel
      Left = 8
      Top = 57
      Width = 89
      Height = 13
      Caption = 'lblGeneratedDtxDir'
    end
    object Label1: TLabel
      Left = 8
      Top = 76
      Width = 114
      Height = 13
      Caption = 'Real *.dtx directory:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblRealDtxDir: TLabel
      Left = 8
      Top = 92
      Width = 40
      Height = 13
      Caption = 'lblOutDir'
    end
    object Button1: TButton
      Left = 336
      Top = 43
      Width = 153
      Height = 25
      Action = actClearGeneratedDtxDir
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 504
    Width = 495
    Height = 41
    Align = alBottom
    TabOrder = 4
    object Button4: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Action = actClearMessages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object Button5: TButton
      Left = 104
      Top = 8
      Width = 75
      Height = 25
      Action = actSaveMessages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 312
    Top = 24
    object actIncludeAll: TAction
      Caption = '>>'
      OnExecute = actIncludeAllExecute
      OnUpdate = actIncludeAllUpdate
    end
    object actExcludeAll: TAction
      Caption = '<<'
      OnExecute = actExcludeAllExecute
      OnUpdate = actExcludeAllUpdate
    end
    object actInclude: TAction
      Caption = '>'
      OnExecute = actIncludeExecute
      OnUpdate = actIncludeUpdate
    end
    object actExclude: TAction
      Caption = '<'
      OnExecute = actExcludeExecute
      OnUpdate = actExcludeUpdate
    end
    object actSettings: TAction
      Category = 'TopToolbar'
      Caption = 'Settings'
      OnExecute = actSettingsExecute
    end
    object actGenerateDtxFiles: TAction
      Category = 'TopToolbar'
      Caption = 'Generate Dtx files'
      Hint = 
        'Generates *.dtx files from the files listed'#13#10'in the right list b' +
        'ox and puts them in the'#13#10'generated *.dtx directory'
      OnExecute = actGenerateDtxFilesExecute
      OnUpdate = ProcessFilesAvailable
    end
    object actSave: TAction
      Caption = 'Save'
      OnExecute = actSaveExecute
    end
    object actAddToIgnoreList: TAction
      Caption = 'Add to Ignore List'
      Hint = 'Marks the selected files in the right list box as '#39'ignored'#39
      OnExecute = actAddToIgnoreListExecute
    end
    object actAddToCompletedList: TAction
      Caption = 'Add to Completed List'
      Hint = 'Marks the selected files in the right list box as '#39'completed'#39
      OnExecute = actAddToCompletedListExecute
    end
    object actUnitStatus: TAction
      Category = 'TopToolbar'
      Caption = 'Unit Status'
      Hint = 
        'View the status of the dtx files, and enables you to'#13#10'update the' +
        ' status files, such as '#39'Completed units.txt'#39' etc.'
      OnExecute = actUnitStatusExecute
    end
    object actShowCompleted: TAction
      Category = 'ShowToolbar'
      Caption = 'Show Completed'
      Hint = 
        'Shows the files in the *.pas directory that are marked as '#39'compl' +
        'eted'#39#13#10'These files are listed in the '#39'Completed units.txt'#39' file'
      OnExecute = actShowCompletedExecute
      OnUpdate = actShowCompletedUpdate
    end
    object actShowIgnored: TAction
      Category = 'ShowToolbar'
      Caption = 'Show Ignored'
      Hint = 
        'Shows the files in the *.pas directory that are marked as '#39'ignor' +
        'ed'#39#13#10'These files are listed in the '#39'Ignored units.txt'#39' file'
      OnExecute = actShowIgnoredExecute
      OnUpdate = actShowIgnoredUpdate
    end
    object actShowOther: TAction
      Category = 'ShowToolbar'
      Caption = 'Show Other'
      Checked = True
      Hint = 
        'Shows the files in the *.pas directory that are marked as '#39'other' +
        #39#13#10'These files are listed in the '#39'Other units.txt'#39' file'
      OnExecute = actShowOtherExecute
      OnUpdate = actShowOtherUpdate
    end
    object actRefresh: TAction
      Category = 'ShowToolbar'
      Caption = 'Refresh'
      Hint = 'Rescans the directory'
      OnExecute = actRefreshExecute
    end
    object actShowGenerated: TAction
      Category = 'ShowToolbar'
      Caption = 'Show Generated'
      OnExecute = actShowGeneratedExecute
      OnUpdate = actShowGeneratedUpdate
    end
    object actCheckDtxFiles: TAction
      Category = 'TopToolbar'
      Caption = 'Check Dtx files'
      Hint = 'Checks the files in the real *.dtx directory'
      OnExecute = actCheckDtxFilesExecute
      OnUpdate = ProcessFilesAvailable
    end
    object actClearGeneratedDtxDir: TAction
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
      Category = 'TopToolbar'
      Caption = 'Generate Package'
      Hint = 'Updates the '#39'Files In Packages.txt'#39' file'
      OnExecute = actGeneratePackageListExecute
    end
    object actGenerateRegisteredClasses: TAction
      Caption = 'Generate Registered Classes'
      OnExecute = actGenerateRegisteredClassesExecute
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 248
    Top = 24
  end
end
