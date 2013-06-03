object Form1: TForm1
  Left = 483
  Top = 232
  Width = 270
  Height = 438
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 48
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Favorites'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 48
    Top = 68
    Width = 75
    Height = 25
    Caption = 'Folder'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 48
    Top = 124
    Width = 75
    Height = 25
    Caption = 'Control Panel'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 48
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Applet'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 48
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Change Icon'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 136
    Top = 16
    Width = 75
    Height = 25
    Caption = 'About'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 136
    Top = 68
    Width = 75
    Height = 25
    Caption = 'Memory'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 136
    Top = 124
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 7
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 136
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Format'
    TabOrder = 8
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 136
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Computer'
    TabOrder = 9
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 48
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Properties'
    TabOrder = 10
    OnClick = Button11Click
  end
  object Button13: TButton
    Left = 48
    Top = 372
    Width = 75
    Height = 25
    Caption = 'New Shortcut'
    TabOrder = 11
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 136
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Add Hardware'
    TabOrder = 12
    OnClick = Button14Click
  end
  object OrganizeFavoritesDialog: TJvOrganizeFavoritesDialog
    Left = 16
    Top = 12
  end
  object BrowseFolderDialog: TJvBrowseFolder
    RootDirectory = fdRootFolder
    Title = 'Select Folder'
    Left = 16
    Top = 68
  end
  object ControlPanel: TJvControlPanelDialog
    Left = 16
    Top = 124
  end
  object AppletDialog: TJvAppletDialog
    AppletName = 'C:\WINNT\system32\timedate.cpl'
    Left = 16
    Top = 188
  end
  object ChangeIconDialog: TJvChangeIconDialog
    IconIndex = 0
    Left = 16
    Top = 252
  end
  object ShellAboutDialog: TJvShellAboutDialog
    Caption = 'About'
    OtherText = 'Other Text'
    Product = 'Product Name'
    Left = 216
    Top = 12
  end
  object OutOfMemoryDialog: TJvOutOfMemoryDialog
    Caption = 'Warning'
    Left = 220
    Top = 72
  end
  object RunDialog: TJvRunDialog
    Caption = 'Caption'
    Description = 'Description'
    Icon.Data = {
      0000010002002020100000000000E80200002600000010101000000000002801
      00000E0300002800000020000000400000000100040000000000800200000000
      0000000000000000000000000000000000000000800000800000008080008000
      0000800080008080000080808000C0C0C0000000FF0000FF000000FFFF00FF00
      0000FF00FF00FFFF0000FFFFFF00000000000000000000000000000000000000
      0000011111111111111000000000000000001111111111111111000000000000
      0001111111111111111110000000000000111111111111111111110000000000
      0111111111111111111111100000000011111111111111111111111100000001
      1111111111111111111111111000001111111111111111111111111111000111
      11111111111111111111111111100111111FF1111F1111FF111F111111100111
      11F11F111F111F11F11F11111110011111F11F111F111F11F11F111111100111
      11F11F111F111F11F11F11111110011111111F111F111F11F11F111111100111
      11111F111F111F11F11F111111100111111FF1111F111F11F11FFF1111100111
      11F111111F111F11F11F11F11110011111F111111F111F11F11F11F111100111
      11F11F111F111F11F11F11F11110011111F11F111F111F11F11F11F111100111
      111FF11FFFFF11FF111FFF111110011111111111111111111111111111100011
      1111111111111111111111111100000111111111111111111111111110000000
      1111111111111111111111110000000001111111111111111111111000000000
      0011111111111111111111000000000000011111111111111111100000000000
      0000111111111111111100000000000000000111111111111110000000000000
      0000000000000000000000000000FFFFFFFFFF8001FFFF0000FFFE00007FFC00
      003FF800001FF000000FE0000007C00000038000000180000001800000018000
      0001800000018000000180000001800000018000000180000001800000018000
      00018000000180000001C0000003E0000007F000000FF800001FFC00003FFE00
      007FFF0000FFFF8001FFFFFFFFFF280000001000000020000000010004000000
      0000C00000000000000000000000000000000000000000000000000080000080
      00000080800080000000800080008080000080808000C0C0C0000000FF0000FF
      000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000000000000000
      11111111100000011111111111000011111111111110011F11F11F11F11101F1
      F1F1F1F1F1110111F1F1F1F1F111011FF1F1F1F1FF1101FF11F1F1F1F1F101F1
      11F1F1F1F1F101F1F1F1F1F1F1F1011F1FFF1F11FF1100111111111111100001
      11111111110000001111111110000000000000000000FFFF0000F0070000E003
      0000C00100008000000080000000800000008000000080000000800000008000
      000080000000C0010000E0030000F0070000FFFF0000}
    Left = 216
    Top = 132
  end
  object FormatDialog: TJvFormatDrive
    FormatType = ftStandard
    Capacity = dcDefault
    Left = 220
    Top = 188
  end
  object ComputerNameDialog: TJvComputerNameDialog
    Left = 220
    Top = 248
  end
  object ObjectPropertiesDialog: TJvObjectPropertiesDialog
    ObjectName = 'My Computer'
    ObjectType = sdPathObject
    Left = 12
    Top = 320
  end
  object psvNewLinkDialog: TJvNewLinkDialog
    DestinationFolder = 'c:\'
    Left = 8
    Top = 372
  end
  object AddHardwareDialog: TJvAddHardwareDialog
    Left = 224
    Top = 312
  end
end
