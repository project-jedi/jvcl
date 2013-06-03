object Form1: TForm1
  Left = 391
  Top = 230
  AutoScroll = False
  Caption = 'FileCtrls Demo'
  ClientHeight = 398
  ClientWidth = 595
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DriveCombo1: TJvDriveCombo
    Left = 2
    Top = 2
    Width = 591
    Height = 22
    BevelInner = bvNone
    DriveTypes = [dtFixed, dtRemote, dtCDROM]
    Offset = 4
    ImageSize = isSmall
    Ctl3D = True
    ItemHeight = 16
    ParentCtl3D = False
    TabOrder = 0
    Anchors = [akLeft, akTop, akRight]
  end
  object JvDirectoryListBox1: TJvDirectoryListBox
    Left = 2
    Top = 29
    Width = 215
    Height = 348
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 17
    Items.Strings = (
      'C:\'
      'C:\Program'
      'C:\Program\adaptec'
      'C:\Program\adaptec\shared'
      'C:\Program\adaptec\shared\ecdc engine')
    TabOrder = 1
    FileList = JvFileListBox1
    DriveCombo = DriveCombo1
    OnChange = JvDirectoryListBox1Change
  end
  object JvFileListBox1: TJvFileListBox
    Left = 221
    Top = 29
    Width = 371
    Height = 348
    Anchors = [akLeft, akTop, akRight, akBottom]
    FileType = [ftDirectory, ftNormal]
    ItemHeight = 16
    ShowGlyphs = True
    TabOrder = 2
    Columns = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 379
    Width = 595
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
end
