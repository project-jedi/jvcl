object frmMain: TfrmMain
  Left = 311
  Top = 151
  Width = 493
  Height = 437
  Caption = 'ANI Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 164
    Top = 94
    Width = 26
    Height = 13
    Caption = 'Icons'
  end
  object Frames: TLabel
    Left = 164
    Top = 182
    Width = 34
    Height = 13
    Caption = 'Frames'
  end
  object JvDriveCombo1: TJvDriveCombo
    Left = 4
    Top = 8
    Width = 145
    Height = 22
    DriveTypes = [dtFixed, dtRemote, dtCDROM]
    Offset = 4
    ImageSize = isSmall
    ItemHeight = 16
    TabOrder = 0
  end
  object JvDirectoryListBox1: TJvDirectoryListBox
    Left = 4
    Top = 32
    Width = 145
    Height = 209
    Directory = 'C:\Prog\CBuilder6\Projects'
    FileList = JvFileListBox1
    DriveCombo = JvDriveCombo1
    ItemHeight = 17
    ScrollBars = ssBoth
    TabOrder = 1
    Anchors = [akLeft, akTop, akBottom]
  end
  object JvFileListBox1: TJvFileListBox
    Left = 4
    Top = 244
    Width = 145
    Height = 161
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    Mask = '*.ani'
    TabOrder = 2
    OnClick = JvFileListBox1Click
    ForceFileExtensions = False
  end
  object btnSave: TButton
    Left = 160
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object Panel1: TPanel
    Left = 260
    Top = 8
    Width = 129
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvLowered
    TabOrder = 4
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 127
      Height = 71
      Align = alClient
    end
  end
  object Panel2: TPanel
    Left = 164
    Top = 108
    Width = 305
    Height = 61
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvLowered
    TabOrder = 5
    object ImageIcons: TImage
      Left = 1
      Top = 1
      Width = 303
      Height = 59
      Align = alClient
    end
  end
  object Panel3: TPanel
    Left = 164
    Top = 196
    Width = 305
    Height = 61
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvLowered
    TabOrder = 6
    object ImageFrames: TImage
      Left = 1
      Top = 1
      Width = 303
      Height = 59
      Align = alClient
    end
  end
  object Memo1: TMemo
    Left = 164
    Top = 272
    Width = 305
    Height = 133
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 7
  end
  object SaveDialog1: TSaveDialog
    Left = 448
    Top = 8
  end
end
