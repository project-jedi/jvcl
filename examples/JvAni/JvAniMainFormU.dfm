object JvAniMainForm: TJvAniMainForm
  Left = 291
  Top = 308
  Width = 497
  Height = 347
  Caption = 'ANI Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    489
    317)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 254
    Top = 8
    Width = 117
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Center = True
    Transparent = True
  end
  object Image2: TImage
    Left = 160
    Top = 96
    Width = 321
    Height = 105
    Anchors = [akLeft, akTop, akRight]
  end
  object FileListBox1: TJvFileListBox
    Left = 0
    Top = 134
    Width = 145
    Height = 177
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
    Height = 97
    Directory = 'E:\Daten\dev\JVCL3\examples\JvAni'
    FileList = FileListBox1
    DriveCombo = DriveComboBox1
    ItemHeight = 16
    ScrollBars = ssBoth
    TabOrder = 1
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
    Width = 321
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    TabOrder = 3
  end
end
