object Form1: TForm1
  Left = 345
  Top = 154
  Width = 744
  Height = 638
  Caption = 'Dialogs Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 736
    Height = 611
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Windows Dialogs'
      object Button1: TButton
        Left = 6
        Top = 4
        Width = 115
        Height = 25
        Caption = 'Format Drive A:'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 6
        Top = 41
        Width = 115
        Height = 25
        Caption = 'Find Files'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button4: TButton
        Left = 6
        Top = 124
        Width = 115
        Height = 25
        Caption = 'Shell About'
        TabOrder = 2
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 6
        Top = 154
        Width = 115
        Height = 25
        Caption = 'Select directory'
        TabOrder = 3
        OnClick = Button5Click
      end
      object Button8: TButton
        Left = 6
        Top = 229
        Width = 115
        Height = 25
        Caption = 'Add a printer'
        TabOrder = 4
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 6
        Top = 266
        Width = 115
        Height = 25
        Caption = 'Connect Network'
        TabOrder = 5
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 6
        Top = 304
        Width = 115
        Height = 25
        Caption = 'Disconnect network'
        TabOrder = 6
        OnClick = Button10Click
      end
      object Button32: TButton
        Left = 140
        Top = 2
        Width = 115
        Height = 25
        Caption = 'Open Dialog'
        TabOrder = 7
        OnClick = Button32Click
      end
      object Button33: TButton
        Left = 140
        Top = 35
        Width = 115
        Height = 25
        Caption = 'Save Dialog'
        TabOrder = 8
        OnClick = Button33Click
      end
      object Button25: TButton
        Left = 140
        Top = 101
        Width = 115
        Height = 25
        Caption = 'Add Hardware'
        TabOrder = 9
        OnClick = Button25Click
      end
      object Button24: TButton
        Left = 140
        Top = 68
        Width = 115
        Height = 25
        Caption = 'Shutdown Dlg'
        TabOrder = 10
        OnClick = Button24Click
      end
      object Button26: TButton
        Left = 140
        Top = 134
        Width = 115
        Height = 25
        Caption = 'Choose Icon'
        TabOrder = 11
        OnClick = Button26Click
      end
      object Button27: TButton
        Left = 140
        Top = 168
        Width = 115
        Height = 25
        Caption = 'Run Dlg'
        TabOrder = 12
        OnClick = Button27Click
      end
      object Button28: TButton
        Left = 140
        Top = 201
        Width = 115
        Height = 25
        Caption = 'Find Computer'
        TabOrder = 13
        OnClick = Button28Click
      end
      object Button29: TButton
        Left = 140
        Top = 234
        Width = 115
        Height = 25
        Caption = 'Object Properties'
        TabOrder = 14
        OnClick = Button29Click
      end
      object Button30: TButton
        Left = 140
        Top = 267
        Width = 115
        Height = 25
        Caption = 'Out Of Memory Dlg'
        TabOrder = 15
        OnClick = Button30Click
      end
      object Button31: TButton
        Left = 140
        Top = 301
        Width = 115
        Height = 25
        Caption = 'Disk C:\ Full Dialog'
        TabOrder = 16
        OnClick = Button31Click
      end
      object Button36: TButton
        Left = 268
        Top = 109
        Width = 115
        Height = 25
        Caption = 'Control Panel Dialog'
        TabOrder = 17
        OnClick = Button36Click
      end
      object Button37: TButton
        Left = 268
        Top = 37
        Width = 115
        Height = 25
        Caption = 'New Shortcut Dialog'
        TabOrder = 18
        OnClick = Button37Click
      end
      object Button38: TButton
        Left = 268
        Top = 69
        Width = 115
        Height = 25
        Caption = 'Applet Dialog'
        TabOrder = 19
        OnClick = Button38Click
      end
      object Button39: TButton
        Left = 268
        Top = 141
        Width = 115
        Height = 25
        Caption = 'Favorites'
        TabOrder = 20
        OnClick = Button39Click
      end
      object Button40: TButton
        Left = 268
        Top = 5
        Width = 115
        Height = 25
        Caption = 'OpenWith Dialog'
        TabOrder = 21
        OnClick = Button40Click
      end
      object Button3: TButton
        Left = 6
        Top = 71
        Width = 115
        Height = 25
        Caption = 'Browse for folder'
        TabOrder = 22
        OnClick = Button3Click
      end
      object Button7: TButton
        Left = 268
        Top = 173
        Width = 115
        Height = 25
        Caption = 'Page Setup'
        TabOrder = 23
        OnClick = Button7Click
      end
      object Button41: TButton
        Left = 268
        Top = 205
        Width = 115
        Height = 25
        Caption = 'Page Setup Titled'
        TabOrder = 24
        OnClick = Button41Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Other Dialogs'
      ImageIndex = 1
      object Button11: TButton
        Left = 10
        Top = 12
        Width = 115
        Height = 25
        Caption = 'Password'
        TabOrder = 0
      end
      object Button14: TButton
        Left = 10
        Top = 48
        Width = 115
        Height = 25
        Caption = 'Exchange listboxes'
        TabOrder = 1
      end
      object Button16: TButton
        Left = 10
        Top = 83
        Width = 115
        Height = 25
        Caption = 'Login'
        TabOrder = 2
      end
      object Button17: TButton
        Left = 10
        Top = 115
        Width = 115
        Height = 25
        Caption = 'Serial'
        TabOrder = 3
      end
      object Button18: TButton
        Left = 10
        Top = 148
        Width = 115
        Height = 25
        Caption = 'Calculator'
        TabOrder = 4
        OnClick = Button18Click
      end
      object Button19: TButton
        Left = 10
        Top = 181
        Width = 115
        Height = 25
        Caption = 'Progress Dlg'
        TabOrder = 5
      end
      object Button20: TButton
        Left = 10
        Top = 215
        Width = 115
        Height = 25
        Caption = 'Disk Prompt'
        TabOrder = 6
        OnClick = Button20Click
      end
      object Button21: TButton
        Left = 138
        Top = 78
        Width = 115
        Height = 25
        Caption = 'Copy Error'
        TabOrder = 7
        OnClick = Button21Click
      end
      object Button22: TButton
        Left = 138
        Top = 10
        Width = 115
        Height = 25
        Caption = 'Delete Error'
        TabOrder = 8
        OnClick = Button22Click
      end
      object Button23: TButton
        Left = 140
        Top = 43
        Width = 115
        Height = 25
        Caption = 'Rename Error'
        TabOrder = 9
        OnClick = Button23Click
      end
      object Button34: TButton
        Left = 486
        Top = 374
        Width = 115
        Height = 25
        Caption = 'Fatal exit'
        TabOrder = 10
      end
    end
  end
  object JvFormatDriveDialog1: TJvFormatDriveDialog
    FormatType = ftQuick
    Capacity = dcDefault
    Left = 104
    Top = 474
  end
  object JvFindFiles1: TJvFindFilesDialog
    SpecialFolder = sfRecycleBin
    UseSpecialFolder = False
    Left = 612
    Top = 44
  end
  object JvBrowseFolder1: TJvBrowseForFolderDialog
    RootDirectory = fdRootFolder
    Left = 196
    Top = 368
  end
  object JvSelectDirectory1: TJvSelectDirectory
    Options = []
    Left = 20
    Top = 422
  end
  object JvConnectNetwork1: TJvConnectNetwork
    Left = 110
    Top = 422
  end
  object JvDisconnectNetwork1: TJvDisconnectNetwork
    Left = 168
    Top = 422
  end
  object JvCalculator1: TJvCalculator
    Left = 462
    Top = 298
  end
  object JvDiskPrompt1: TJvDiskPrompt
    Style = []
    Left = 454
    Top = 194
  end
  object JvCopyError1: TJvCopyError
    Style = []
    Left = 454
    Top = 142
  end
  object JvDeleteError1: TJvDeleteError
    Style = []
    Left = 454
    Top = 90
  end
  object JvRenameError1: TJvRenameError
    Style = []
    Left = 454
    Top = 46
  end
  object JvShutdownDlg1: TJvExitWindowsDialog
    Left = 16
    Top = 366
  end
  object JvShellAboutDialog1: TJvShellAboutDialog
    Left = 32
    Top = 472
  end
  object JvAddHardwareDialog1: TJvAddHardwareDialog
    Left = 88
    Top = 368
  end
  object JvChooseIconDlg1: TJvChangeIconDialog
    IconIndex = 0
    Left = 180
    Top = 478
  end
  object JvRunDlg1: TJvRunDialog
    Left = 236
    Top = 468
  end
  object JvFindComputerDlg1: TJvComputerNameDialog
    Left = 36
    Top = 538
  end
  object JvObjectPropertiesDlg1: TJvObjectPropertiesDialog
    ObjectType = sdPathObject
    Left = 108
    Top = 534
  end
  object JvOutOfMemoryDlg1: TJvOutOfMemoryDialog
    Left = 182
    Top = 536
  end
  object JvOutOfSpaceDlg1: TJvDiskFullDialog
    Left = 258
    Top = 532
  end
  object JvPageSetupDialog1: TJvPageSetupDialog
    Left = 544
    Top = 320
  end
  object JvPageSetupTitledDialog1: TJvPageSetupTitledDialog
    Left = 616
    Top = 288
  end
  object JvBrowseFolder2: TJvBrowseForFolderDialog
    RootDirectory = fdRootFolder
    Left = 616
    Top = 336
  end
  object JvOrganizeFavoritesDialog1: TJvOrganizeFavoritesDialog
    Left = 448
    Top = 424
  end
  object JvAppletDialog1: TJvAppletDialog
    Left = 448
    Top = 480
  end
  object JvNewLinkDialog1: TJvNewLinkDialog
    DestinationFolder = 'C:\'
    Left = 536
    Top = 480
  end
  object JvOpenWithDialog1: TJvOpenWithDialog
    Left = 632
    Top = 424
  end
  object JvAddPrinterDialog1: TJvAddPrinterDialog
    Left = 312
    Top = 288
  end
  object JvOpenDialog1: TJvOpenDialog
    Height = 0
    Width = 0
    Left = 404
    Top = 280
  end
  object JvSaveDialog1: TJvSaveDialog
    Height = 0
    Width = 0
    Left = 384
    Top = 328
  end
end
