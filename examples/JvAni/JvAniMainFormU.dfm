object JvAniMainForm: TJvAniMainForm
  Left = 291
  Top = 308
  Width = 496
  Height = 350
  Caption = 'ANI Viewer'
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    488
    320)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 254
    Top = 8
    Width = 116
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Center = True
    Transparent = True
  end
  object Image2: TImage
    Left = 160
    Top = 96
    Width = 320
    Height = 105
    Anchors = [akLeft, akTop, akRight]
  end
  object FileListBox1: TJvFileListBox
    Left = 0
    Top = 137
    Width = 145
    Height = 177
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
    Height = 100
    Directory = 'E:\Daten\dev\JVCL3\examples\JvAni'
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
    ImageSize = isSmall
    ItemHeight = 16
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 160
    Top = 218
    Width = 320
    Height = 89
    Anchors = [akLeft, akRight, akBottom]
    Color = clBtnFace
    TabOrder = 3
  end
  object Save: TButton
    Left = 160
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Save'
    Enabled = False
    TabOrder = 4
    OnClick = SaveClick
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ani'
    Left = 448
    Top = 8
  end
end
