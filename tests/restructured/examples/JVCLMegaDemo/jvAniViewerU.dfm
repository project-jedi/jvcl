object jvAniViewer: TjvAniViewer
  Left = 0
  Top = 0
  Width = 546
  Height = 368
  TabOrder = 0
  object Image1: TImage
    Left = 278
    Top = 112
    Width = 117
    Height = 111
    Center = True
    Transparent = True
  end
  object Label1: TLabel
    Left = 32
    Top = 16
    Width = 193
    Height = 24
    Caption = 'A simple ANI Viewer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object FileListBox1: TFileListBox
    Left = 56
    Top = 182
    Width = 145
    Height = 177
    ItemHeight = 13
    Mask = '*.ani'
    TabOrder = 0
    OnClick = FileListBox1Click
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 56
    Top = 82
    Width = 145
    Height = 97
    FileList = FileListBox1
    ItemHeight = 16
    TabOrder = 1
  end
  object DriveComboBox1: TDriveComboBox
    Left = 58
    Top = 60
    Width = 145
    Height = 19
    DirList = DirectoryListBox1
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 222
    Top = 256
    Width = 267
    Height = 99
    Color = clBtnFace
    TabOrder = 3
  end
end
