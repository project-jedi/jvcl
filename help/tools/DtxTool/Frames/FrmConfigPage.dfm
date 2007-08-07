object FrameConfigPage: TFrameConfigPage
  Left = 0
  Top = 0
  Width = 518
  Height = 320
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 71
    Height = 13
    Caption = 'Help directory:'
  end
  object Label2: TLabel
    Left = 8
    Top = 112
    Width = 41
    Height = 13
    Caption = 'Options:'
  end
  object Label3: TLabel
    Left = 8
    Top = 64
    Width = 73
    Height = 13
    Caption = 'JVCL directory:'
  end
  object edtHelpDir: TJvDirectoryEdit
    Left = 8
    Top = 32
    Width = 497
    Height = 21
    DialogKind = dkWin32
    TabOrder = 0
    OnChange = edtHelpDirChange
    OnExit = edtHelpDirExit
  end
  object chlOptions: TCheckListBox
    Left = 8
    Top = 128
    Width = 497
    Height = 180
    OnClickCheck = chlOptionsClickCheck
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    ExplicitHeight = 145
  end
  object edtJVCLxxDir: TJvDirectoryEdit
    Left = 8
    Top = 80
    Width = 497
    Height = 21
    DialogKind = dkWin32
    TabOrder = 2
    OnChange = edtHelpDirChange
    OnExit = edtHelpDirExit
  end
end
