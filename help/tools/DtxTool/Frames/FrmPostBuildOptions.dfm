object FramePostBuildOptions: TFramePostBuildOptions
  Left = 0
  Top = 0
  Width = 518
  Height = 335
  TabOrder = 0
  object Label3: TLabel
    Left = 16
    Top = 24
    Width = 147
    Height = 13
    Caption = 'HTML Help compiler (hhc.exe):'
  end
  object Label1: TLabel
    Left = 16
    Top = 80
    Width = 164
    Height = 13
    Caption = 'Windows Help compiler (hcrtf.exe):'
  end
  object edtHHCFileName: TJvFilenameEdit
    Left = 33
    Top = 48
    Width = 465
    Height = 21
    AddQuotes = False
    TabOrder = 0
    OnExit = edtHHCFileNameExit
  end
  object edtHCRTFFileName: TJvFilenameEdit
    Left = 33
    Top = 104
    Width = 465
    Height = 21
    AddQuotes = False
    TabOrder = 1
    OnExit = edtHCRTFFileNameExit
  end
end
