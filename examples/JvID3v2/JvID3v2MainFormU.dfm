object JvID3v2MainForm: TJvID3v2MainForm
  Left = 442
  Top = 277
  Width = 573
  Height = 431
  Caption = 'JvID3v2 example'
  Color = clBtnFace
  Constraints.MinHeight = 150
  Constraints.MinWidth = 200
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100001001000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000BBBB0000000000BB000BB000000000BB0000B000000000B
    BB000BB00000000BBB000BB00000000000000BB00000000000000BB000000000
    00000BB00000000000000BB00000000000000BB00000000000000BB000000000
    00000BB0000000000000BBBB00000000000BBBBBB0000000000000000000FFFF
    0000F87F0000E73F0000E7BF0000E39F0000E39F0000FF9F0000FF9F0000FF9F
    0000FF9F0000FF9F0000FF9F0000FF9F0000FF0F0000FE070000FFFF0000}
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Width = 3
    Height = 401
    Cursor = crHSplit
  end
  object ListView1: TListView
    Left = 188
    Top = 0
    Width = 377
    Height = 401
    Align = alClient
    Columns = <
      item
        Caption = 'Type'
      end
      item
        Caption = 'File name'
        Width = 300
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 401
    Align = alLeft
    TabOrder = 1
    object JvDriveCombo1: TJvDriveCombo
      Left = 1
      Top = 1
      Width = 183
      Height = 22
      Align = alTop
      DriveTypes = [dtFixed, dtRemote, dtCDROM]
      Offset = 4
      ImageSize = isSmall
      ItemHeight = 16
      TabOrder = 0
    end
    object JvDirectoryListBox1: TJvDirectoryListBox
      Left = 1
      Top = 23
      Width = 183
      Height = 377
      Align = alClient
      Directory = 'C:\'
      DriveCombo = JvDriveCombo1
      ItemHeight = 17
      ScrollBars = ssBoth
      TabOrder = 1
      OnChange = JvDirectoryListBox1Change
    end
  end
  object JvSearchFiles1: TJvSearchFiles
    DirOption = doExcludeSubDirs
    Options = [soOwnerData, soSearchFiles]
    FileParams.SearchTypes = [stFileMask]
    FileParams.FileMasks.Strings = (
      '*.mp3')
    OnFindFile = JvSearchFiles1FindFile
    Left = 440
    Top = 8
  end
  object JvID3v21: TJvID3v2
    Active = False
    Left = 400
    Top = 8
  end
end
