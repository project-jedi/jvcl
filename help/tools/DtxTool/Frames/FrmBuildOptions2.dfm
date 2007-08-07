object FrameBuildOptions2: TFrameBuildOptions2
  Left = 0
  Top = 0
  Width = 559
  Height = 374
  TabOrder = 0
  object Label3: TLabel
    Left = 16
    Top = 24
    Width = 262
    Height = 13
    Caption = 'Doc-O-Matic includes a command line tool (DMCC.exe):'
  end
  object edtDMCCFileName: TJvFilenameEdit
    Left = 33
    Top = 48
    Width = 465
    Height = 21
    AddQuotes = False
    TabOrder = 0
    OnExit = edtDMCCFileNameExit
  end
end
