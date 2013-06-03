object Form1: TForm1
  Left = 298
  Top = 244
  BorderStyle = bsDialog
  Caption = 'ANI Viewer'
  ClientHeight = 317
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 262
    Top = 64
    Width = 117
    Height = 111
    Center = True
    Transparent = True
  end
  object FileListBox1: TFileListBox
    Left = 0
    Top = 134
    Width = 145
    Height = 177
    ItemHeight = 13
    Mask = '*.ani'
    TabOrder = 0
    OnClick = FileListBox1Click
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 0
    Top = 34
    Width = 145
    Height = 97
    FileList = FileListBox1
    ItemHeight = 16
    TabOrder = 1
  end
  object DriveComboBox1: TDriveComboBox
    Left = 2
    Top = 12
    Width = 145
    Height = 19
    DirList = DirectoryListBox1
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 182
    Top = 210
    Width = 273
    Height = 89
    Color = clBtnFace
    TabOrder = 3
  end
end
