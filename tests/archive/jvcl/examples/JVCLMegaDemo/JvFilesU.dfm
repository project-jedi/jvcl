object JvFilesFrm: TJvFilesFrm
  Left = 0
  Top = 0
  Width = 548
  Height = 438
  TabOrder = 0
  object JvLabel6: TJvLabel
    Left = 8
    Top = 64
    Width = 87
    Height = 13
    Caption = 'JvDirectoryListBox'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvLabel7: TJvLabel
    Left = 8
    Top = 8
    Width = 69
    Height = 13
    Caption = 'JvDriveCombo'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvLabel8: TJvLabel
    Left = 344
    Top = 16
    Width = 52
    Height = 13
    Caption = 'JvDriveList'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvLabel9: TJvLabel
    Left = 8
    Top = 216
    Width = 61
    Height = 13
    Caption = 'JvFileListBox'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Label1: TLabel
    Left = 368
    Top = 304
    Width = 321
    Height = 13
    Caption = 'C:\...\Borland\Components\JEDI\JVCL\examples\JVCLMegaDemo'
  end
  object Label2: TLabel
    Left = 368
    Top = 272
    Width = 113
    Height = 26
    Caption = 'This label shows always'#13#10'the selected path:'
  end
  object JvDirectoryListBox1: TJvDirectoryListBox
    Left = 8
    Top = 80
    Width = 321
    Height = 129
    ItemHeight = 17
    Items.Strings = (
      'D:\'
      'D:\  new'
      'D:\ backup'
      'D:\barOne'
      'D:\BP'
      'D:\Home'
      'D:\Images'
      'D:\KALiV'
      'D:\mfssim'
      'D:\Oracle'
      'D:\Public'
      'D:\Ritz_CD_App'
      'D:\Test_Files')
    Style = lbOwnerDrawFixed
    TabOrder = 0
    Directory = 'C:\Program\Borland\Components\JEDI\JVCL\examples\JVCLMegaDemo'
    DirLabel = Label1
    FileList = JvFileListBox1
    DriveCombo = JvDriveCombo1
  end
  object JvDriveCombo1: TJvDriveCombo
    Left = 8
    Top = 32
    Width = 145
    Height = 22
    DriveTypes = [dtFixed, dtRemote, dtCDROM]
    Offset = 4
    ImageSize = isSmall
    ItemHeight = 16
    TabOrder = 1
  end
  object JvDriveList1: TJvDriveList
    Left = 344
    Top = 40
    Width = 177
    Height = 177
    ItemHeight = 37
    Items.Strings = (
      '3,5-tumsdiskett (A:)'
      'SYS (C:)'
      'CD-skiva (D:)'
      'CD-skiva (E:)'
      'timemetrics på eqplan (G:)'
      'log på kgb1__www\c$\Mercur (J:)'
      'BackupLog på Hobbe\c$\Script (K:)'
      'log$ på eqplan (L:)'
      'data på Eqplan (M:)'
      'eqdata på Eqplan (N:)'
      'ProAct på Eqplan (P:)'
      'Vss på Eqplan (V:)'
      'id106 på Kgb1__www\e-kgb\webhotel (W:)'
      'transfer på kalle (X:)'
      'peter3 på Kgb1__www\e-kgb\webhotel (Z:)')
    Style = lbOwnerDrawFixed
    TabOrder = 2
    DriveTypes = [dtFixed, dtRemote, dtCDROM]
    ImageSize = isSmall
  end
  object JvFileListBox1: TJvFileListBox
    Left = 8
    Top = 232
    Width = 329
    Height = 169
    ItemHeight = 16
    ShowGlyphs = True
    TabOrder = 3
  end
end
