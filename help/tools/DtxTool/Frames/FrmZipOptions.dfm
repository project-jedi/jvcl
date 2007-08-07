object FrameZipOptions: TFrameZipOptions
  Left = 0
  Top = 0
  Width = 518
  Height = 335
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 64
    Width = 128
    Height = 13
    Caption = 'Zip executable (wzzip.exe):'
  end
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 119
    Height = 13
    Caption = 'Max. nr. of files in zip files'
  end
  object Label3: TLabel
    Left = 8
    Top = 112
    Width = 88
    Height = 13
    Caption = 'Add dtx files to zip:'
  end
  object edtZipExec: TJvFilenameEdit
    Left = 8
    Top = 80
    Width = 497
    Height = 21
    AddQuotes = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'JvFilenameEdit1'
    OnExit = ChangeEvent
  end
  object spnMaxInZip: TJvSpinEdit
    Left = 136
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = ChangeEvent
    OnClick = ChangeEvent
  end
  object edtDate: TJvDatePickerEdit
    Left = 160
    Top = 136
    Width = 121
    Height = 21
    AllowNoDate = True
    Checked = True
    TabOrder = 3
    OnChange = ChangeEvent
    OnClick = ChangeEvent
  end
  object edtTime: TDateTimePicker
    Left = 288
    Top = 136
    Width = 113
    Height = 21
    Date = 38364.9607981713
    Time = 38364.9607981713
    Kind = dtkTime
    TabOrder = 4
    OnClick = ChangeEvent
    OnChange = ChangeEvent
  end
  object rbtDtxFilterTimestamp: TRadioButton
    Left = 16
    Top = 136
    Width = 145
    Height = 17
    Caption = 'Dtx with timestamp after'
    TabOrder = 2
    OnClick = ChangeEvent
  end
  object rbtDtxFilterAll: TRadioButton
    Left = 16
    Top = 160
    Width = 113
    Height = 17
    Caption = 'All'
    TabOrder = 5
    OnClick = ChangeEvent
  end
end
