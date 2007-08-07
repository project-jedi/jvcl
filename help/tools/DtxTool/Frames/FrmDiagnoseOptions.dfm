object FrameDiagnoseOptions: TFrameDiagnoseOptions
  Left = 0
  Top = 0
  Width = 514
  Height = 335
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 112
    Width = 95
    Height = 13
    Caption = 'Dictionary file name:'
  end
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 90
    Height = 13
    Caption = 'All words file name:'
  end
  object Label3: TLabel
    Left = 8
    Top = 152
    Width = 110
    Height = 13
    Caption = 'Invalid words file name:'
  end
  object Label4: TLabel
    Left = 8
    Top = 56
    Width = 106
    Height = 13
    Caption = 'All identifiers file name:'
  end
  object Label5: TLabel
    Left = 8
    Top = 192
    Width = 140
    Height = 13
    Caption = 'Duplicate identifiers file name:'
  end
  object edtDictionary: TJvFilenameEdit
    Left = 8
    Top = 128
    Width = 497
    Height = 21
    AddQuotes = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnExit = EditExit
  end
  object edtAllWords: TJvFilenameEdit
    Left = 8
    Top = 32
    Width = 497
    Height = 21
    AddQuotes = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnExit = EditExit
  end
  object edtInvalidWords: TJvFilenameEdit
    Left = 8
    Top = 168
    Width = 497
    Height = 21
    AddQuotes = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnExit = EditExit
  end
  object edtAllIdentifiers: TJvFilenameEdit
    Left = 8
    Top = 72
    Width = 497
    Height = 21
    AddQuotes = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnExit = EditExit
  end
  object edtDupIdentifiers: TJvFilenameEdit
    Left = 8
    Top = 208
    Width = 497
    Height = 21
    AddQuotes = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnExit = EditExit
  end
end
