object Form1: TForm1
  Left = 381
  Top = 199
  Width = 507
  Height = 472
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblInDirDesc: TLabel
    Left = 8
    Top = 48
    Width = 70
    Height = 13
    Caption = '*.pas directory:'
  end
  object lblOutDirDesc: TLabel
    Left = 8
    Top = 96
    Width = 67
    Height = 13
    Caption = '*.dtx directory:'
  end
  object lblInDir: TLabel
    Left = 8
    Top = 72
    Width = 32
    Height = 13
    Caption = 'lblInDir'
  end
  object lblOutDir: TLabel
    Left = 8
    Top = 120
    Width = 40
    Height = 13
    Caption = 'lblOutDir'
  end
  object lsbMessages: TListBox
    Left = 0
    Top = 320
    Width = 499
    Height = 118
    Align = alBottom
    ItemHeight = 13
    TabOrder = 8
  end
  object btnSettings: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Action = actSettings
    TabOrder = 0
  end
  object lsbSource: TListBox
    Left = 8
    Top = 144
    Width = 233
    Height = 137
    ItemHeight = 13
    MultiSelect = True
    Sorted = True
    TabOrder = 1
  end
  object lsbDest: TListBox
    Left = 272
    Top = 144
    Width = 217
    Height = 137
    ItemHeight = 13
    MultiSelect = True
    Sorted = True
    TabOrder = 6
  end
  object btnInclude: TButton
    Left = 248
    Top = 144
    Width = 17
    Height = 25
    Action = actInclude
    TabOrder = 2
  end
  object btnIncludeAll: TButton
    Left = 248
    Top = 176
    Width = 17
    Height = 25
    Action = actIncludeAll
    TabOrder = 3
  end
  object btnExclude: TButton
    Left = 248
    Top = 208
    Width = 17
    Height = 25
    Action = actExclude
    TabOrder = 4
  end
  object btnExcludeAll: TButton
    Left = 248
    Top = 240
    Width = 17
    Height = 25
    Action = actExcludeAll
    TabOrder = 5
  end
  object btnProcess: TButton
    Left = 8
    Top = 288
    Width = 75
    Height = 25
    Action = actProcess
    TabOrder = 7
  end
  object Button1: TButton
    Left = 272
    Top = 288
    Width = 75
    Height = 25
    Action = actSave
    TabOrder = 9
  end
  object chbDontIncludeIgnoredFiles: TCheckBox
    Left = 272
    Top = 120
    Width = 185
    Height = 17
    Caption = 'Do not Include Ignored Files'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnClick = chbDontIncludeIgnoredFilesClick
  end
  object Button2: TButton
    Left = 360
    Top = 288
    Width = 105
    Height = 25
    Action = actAddToIgnoreList
    TabOrder = 11
  end
  object ActionList1: TActionList
    Left = 312
    Top = 80
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
      Caption = 'Settings'
      OnExecute = actSettingsExecute
    end
    object actProcess: TAction
      Caption = 'Process'
      OnExecute = actProcessExecute
      OnUpdate = actProcessUpdate
    end
    object actSave: TAction
      Caption = 'Save'
      OnExecute = actSaveExecute
    end
    object actAddToIgnoreList: TAction
      Caption = 'Add to Ignore List'
      OnExecute = actAddToIgnoreListExecute
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 176
    Top = 64
  end
  object JvProgressComponent1: TJvProgressComponent
    ProgressMin = 0
    ProgressMax = 0
    ProgressStep = 0
    ProgressPosition = 0
    Left = 224
    Top = 64
  end
end
