object JvAniMainForm: TJvAniMainForm
  Left = 291
  Top = 308
  Width = 496
  Height = 350
  ActiveControl = DriveComboBox1
  Caption = 'ANI Viewer'
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 160
    Top = 72
    Width = 29
    Height = 13
    Caption = 'Icons:'
  end
  object Label2: TLabel
    Left = 160
    Top = 139
    Width = 37
    Height = 13
    Caption = 'Frames:'
  end
  object FileListBox1: TJvFileListBox
    Left = 0
    Top = 191
    Width = 145
    Height = 115
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    Mask = '*.ani'
    TabOrder = 0
    OnClick = FileListBox1Click
    ForceFileExtensions = False
  end
  object DirectoryListBox1: TJvDirectoryListBox
    Left = 0
    Top = 34
    Width = 145
    Height = 155
    Directory = 'C:\'
    FileList = FileListBox1
    DriveCombo = DriveComboBox1
    ItemHeight = 16
    ScrollBars = ssBoth
    TabOrder = 1
    Anchors = [akLeft, akTop, akBottom]
  end
  object DriveComboBox1: TJvDriveCombo
    Left = 2
    Top = 12
    Width = 145
    Height = 22
    DriveTypes = [dtFixed, dtRemote, dtCDROM]
    Offset = 4
    ItemHeight = 16
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 160
    Top = 209
    Width = 320
    Height = 97
    Anchors = [akLeft, akRight, akBottom]
    Color = clBtnFace
    TabOrder = 3
  end
  object Save: TButton
    Left = 160
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = SaveClick
  end
  object Panel1: TPanel
    Left = 272
    Top = 16
    Width = 97
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvLowered
    TabOrder = 5
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 95
      Height = 55
      Align = alClient
      Center = True
      Transparent = True
    end
  end
  object Panel2: TPanel
    Left = 160
    Top = 85
    Width = 320
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvLowered
    TabOrder = 6
    object ImageIcons: TImage
      Left = 1
      Top = 1
      Width = 318
      Height = 47
      Align = alClient
    end
  end
  object Panel3: TPanel
    Left = 160
    Top = 152
    Width = 320
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvLowered
    TabOrder = 7
    object ImageFrames: TImage
      Left = 1
      Top = 1
      Width = 318
      Height = 47
      Align = alClient
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ani'
    Left = 440
    Top = 16
  end
end
