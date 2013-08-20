object JvWinDialogs: TJvWinDialogs
  Left = 0
  Top = 0
  Width = 451
  Height = 397
  TabOrder = 0
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 175
    Height = 25
    Caption = 'Browse Folder'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 40
    Width = 175
    Height = 25
    Caption = 'Select Directory'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 72
    Width = 175
    Height = 25
    Caption = 'Organize Favorites'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 104
    Width = 175
    Height = 25
    Caption = 'Computer Name'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 136
    Width = 175
    Height = 25
    Caption = 'Controlpanel'
    TabOrder = 4
  end
  object Button6: TButton
    Left = 8
    Top = 168
    Width = 175
    Height = 25
    Caption = 'Appletdialog'
    TabOrder = 5
  end
  object Button7: TButton
    Left = 8
    Top = 200
    Width = 175
    Height = 25
    Caption = 'Change Icon'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 232
    Top = 8
    Width = 175
    Height = 25
    Caption = 'Add Hardware Dialog'
    TabOrder = 7
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 232
    Top = 200
    Width = 175
    Height = 25
    Caption = 'Disconnect Network'
    TabOrder = 8
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 232
    Top = 168
    Width = 175
    Height = 25
    Caption = 'Connect Network'
    TabOrder = 9
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 232
    Top = 136
    Width = 175
    Height = 25
    Caption = 'Out of Memory'
    TabOrder = 10
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 232
    Top = 104
    Width = 175
    Height = 25
    Caption = 'Exit Windows'
    TabOrder = 11
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 232
    Top = 72
    Width = 175
    Height = 25
    Caption = 'Disk Full Dialog'
    TabOrder = 12
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 232
    Top = 40
    Width = 175
    Height = 25
    Caption = 'Open with...'
    TabOrder = 13
    OnClick = Button14Click
  end
  object Button15: TButton
    Left = 8
    Top = 232
    Width = 175
    Height = 25
    Caption = 'About Dialog'
    TabOrder = 14
    OnClick = Button15Click
  end
  object Button16: TButton
    Left = 232
    Top = 296
    Width = 175
    Height = 25
    Caption = 'Add Printer'
    TabOrder = 15
    OnClick = Button16Click
  end
  object Button17: TButton
    Left = 232
    Top = 264
    Width = 175
    Height = 25
    Caption = 'Save Dialog 2000'
    TabOrder = 16
  end
  object Button18: TButton
    Left = 232
    Top = 232
    Width = 175
    Height = 25
    Caption = 'Open Dialog 2000'
    TabOrder = 17
  end
  object Button19: TButton
    Left = 8
    Top = 328
    Width = 175
    Height = 25
    Caption = 'New Link'
    TabOrder = 18
    OnClick = Button19Click
  end
  object Button20: TButton
    Left = 8
    Top = 296
    Width = 175
    Height = 25
    Caption = 'Objectpropertys'
    TabOrder = 19
    OnClick = Button20Click
  end
  object Button21: TButton
    Left = 8
    Top = 264
    Width = 175
    Height = 25
    Caption = 'Run Dialog'
    TabOrder = 20
    OnClick = Button21Click
  end
  object JvBrowseFolder1: TJvBrowseForFolderDialog
    RootDirectory = fdRootFolder
    Title = 'Browser thru folders'
    StatusText = 'Status of operation'
    Left = 192
    Top = 8
  end
  object JvSelectDirectory1: TJvSelectDirectory
    Title = 'Select a Directory'
    Left = 192
    Top = 40
  end
  object JvOrganizeFavoritesDialog1: TJvOrganizeFavoritesDialog
    Left = 192
    Top = 72
  end
  object JvComputerNameDialog1: TJvComputerNameDialog
    Caption = 'Computername'
    Left = 192
    Top = 104
  end
  object JvAppletDialog1: TJvAppletDialog
    Left = 192
    Top = 168
  end
  object JvChangeIconDialog1: TJvChangeIconDialog
    IconIndex = 0
    Left = 192
    Top = 200
  end
  object JvShellAboutDialog1: TJvShellAboutDialog
    Caption = 'The Caption'
    Icon.Data = {
      0000010001002020100000000000E80200001600000028000000200000004000
      0000010004000000000080020000000000000000000000000000000000001D18
      150016939300202ABD00545E5400E5DE6F0039345000AC2C2C009F6B61004237
      2A0015F4F100A87898001DB1B0001A60E400B3987100E2D6D200C3AEB8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D333
      A37333777DFFDDDD777A00000000500000000000377D78888008000000003110
      103101383308D7388838000000001998959919B87000573888D800000000B995
      9399191850188D7387E300000000BB93939BB90350130A7383D300000000BB93
      939B190305118DD78838000000001B91939B190752B18FD78888000000001393
      919119855C9B7FE78888000000003091B19809830C9B6EED8808000000005091
      9190890302367EED8888000000003091919889030206AEED8808000000003091
      BB9BB90302067EE7808800000000509BBB9B190382067EF7880800000000309B
      BB99190850008ED3808800000000509BBB99190830003D7880080000000050BB
      311B5B003800D7388008000000003000000000008787D3888808000000005000
      0000000083773880000800000000FF303444444444444444444400000000AA53
      34DDDD7DD777DDDDD44400000000AF3A3477777D77737D77744400000000EF50
      3477D77D76373D77744400000000E4D3D4DDD44D444444D44444000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000}
    OtherText = 'More Text to fill'
    Product = 'Productname'
    Left = 192
    Top = 232
  end
  object JvRunDialog1: TJvRunDialog
    Caption = 'The Caption'
    Description = 'Description'
    Icon.Data = {
      0000010001002020100000000000E80200001600000028000000200000004000
      0000010004000000000080020000000000000000000000000000000000001D18
      150016939300202ABD00545E5400E5DE6F0039345000AC2C2C009F6B61004237
      2A0015F4F100A87898001DB1B0001A60E400B3987100E2D6D200C3AEB8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D333
      A37333777DFFDDDD777A00000000500000000000377D78888008000000003110
      103101383308D7388838000000001998959919B87000573888D800000000B995
      9399191850188D7387E300000000BB93939BB90350130A7383D300000000BB93
      939B190305118DD78838000000001B91939B190752B18FD78888000000001393
      919119855C9B7FE78888000000003091B19809830C9B6EED8808000000005091
      9190890302367EED8888000000003091919889030206AEED8808000000003091
      BB9BB90302067EE7808800000000509BBB9B190382067EF7880800000000309B
      BB99190850008ED3808800000000509BBB99190830003D7880080000000050BB
      311B5B003800D7388008000000003000000000008787D3888808000000005000
      0000000083773880000800000000FF303444444444444444444400000000AA53
      34DDDD7DD777DDDDD44400000000AF3A3477777D77737D77744400000000EF50
      3477D77D76373D77744400000000E4D3D4DDD44D444444D44444000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000}
    Left = 192
    Top = 264
  end
  object JvObjectPropertiesDialog1: TJvObjectPropertiesDialog
    ObjectType = sdPathObject
    Left = 192
    Top = 296
  end
  object JvNewLinkDialog1: TJvNewLinkDialog
    DestinationFolder = 'C:\Temp'
    Left = 192
    Top = 328
  end
  object JvAddHardwareDialog1: TJvAddHardwareDialog
    Left = 416
    Top = 8
  end
  object JvOpenWithDialog1: TJvOpenWithDialog
    Left = 416
    Top = 40
  end
  object JvDiskFullDialog1: TJvDiskFullDialog
    Left = 416
    Top = 72
  end
  object JvExitWindowsDialog1: TJvExitWindowsDialog
    Left = 416
    Top = 104
  end
  object JvOutOfMemoryDialog1: TJvOutOfMemoryDialog
    Caption = 'Ups....'
    Left = 416
    Top = 136
  end
  object JvConnectNetwork1: TJvConnectNetwork
    Left = 416
    Top = 168
  end
  object JvDisconnectNetwork1: TJvDisconnectNetwork
    Left = 416
    Top = 200
  end
  object JvAddPrinterDialog1: TJvAddPrinterDialog
    Left = 416
    Top = 296
  end
end
