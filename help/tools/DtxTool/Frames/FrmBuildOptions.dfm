object FrameBuildOptions: TFrameBuildOptions
  Left = 0
  Top = 0
  Width = 517
  Height = 359
  TabOrder = 0
  object Label2: TLabel
    Left = 48
    Top = 120
    Width = 230
    Height = 13
    Caption = 'Minimum % of topics that should be documented:'
  end
  object Label1: TLabel
    Left = 50
    Top = 64
    Width = 115
    Height = 13
    Caption = 'Max. nr. of files in output'
  end
  object Label3: TLabel
    Left = 16
    Top = 12
    Width = 38
    Height = 13
    Caption = 'Version:'
  end
  object spnMaxDtx: TJvSpinEdit
    Left = 176
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = spnMaxDtxChange
    OnClick = spnMaxDtxClick
  end
  object spnMinPerc: TJvSpinEdit
    Left = 49
    Top = 136
    Width = 121
    Height = 21
    MaxValue = 100
    TabOrder = 1
    OnChange = spnMinPercChange
    OnClick = spnMinPercClick
  end
  object rbtMaxDtx: TRadioButton
    Left = 16
    Top = 40
    Width = 161
    Height = 17
    Caption = 'Limit nr. of files in output'
    TabOrder = 2
    OnClick = HandleChanged
  end
  object rbtPercDtx: TRadioButton
    Left = 16
    Top = 96
    Width = 225
    Height = 17
    Caption = 'Limit files in output by documented %'
    TabOrder = 3
    OnClick = HandleChanged
  end
  object rbtSpecificDtx: TRadioButton
    Left = 16
    Top = 168
    Width = 185
    Height = 17
    Caption = 'Specific files in output'
    TabOrder = 4
    OnClick = HandleChanged
  end
  object lsbSpecificFiles: TListBox
    Left = 40
    Top = 192
    Width = 368
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Sorted = True
    TabOrder = 5
  end
  object btnAddDtx: TButton
    Left = 423
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Add'
    TabOrder = 6
    OnClick = btnAddDtxClick
  end
  object btnDeleteDtx: TButton
    Left = 423
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Delete'
    TabOrder = 7
    OnClick = btnDeleteDtxClick
  end
  object edtJVCLMajorVersion: TEdit
    Left = 72
    Top = 8
    Width = 49
    Height = 21
    TabOrder = 8
    OnChange = changeEvent
    OnExit = exitEvent
  end
  object edtJVCLMinorVersion: TEdit
    Left = 136
    Top = 8
    Width = 49
    Height = 21
    TabOrder = 9
    OnChange = changeEvent
    OnExit = exitEvent
  end
end
