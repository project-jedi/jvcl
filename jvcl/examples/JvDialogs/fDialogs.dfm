object JvDialogsDemoFrm: TJvDialogsDemoFrm
  Left = 345
  Top = 154
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Dialogs Demo'
  ClientHeight = 501
  ClientWidth = 837
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 401
    Height = 377
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Windows Dialogs'
      object Label1: TLabel
        Left = 19
        Top = 8
        Width = 348
        Height = 13
        Caption = 'BE CAREFUL!  The fully functional system dialogs get called!'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Button1: TButton
        Left = 8
        Top = 32
        Width = 115
        Height = 25
        Caption = 'Format Drive A:'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 8
        Top = 64
        Width = 115
        Height = 25
        Caption = 'Find Files'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button4: TButton
        Left = 8
        Top = 160
        Width = 115
        Height = 25
        Caption = 'Shell About'
        TabOrder = 2
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 8
        Top = 192
        Width = 115
        Height = 25
        Caption = 'Select Directory'
        TabOrder = 3
        OnClick = Button5Click
      end
      object Button8: TButton
        Left = 8
        Top = 256
        Width = 115
        Height = 25
        Caption = 'Add Printer'
        TabOrder = 4
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 8
        Top = 288
        Width = 115
        Height = 25
        Caption = 'Connect Network'
        TabOrder = 5
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 8
        Top = 320
        Width = 115
        Height = 25
        Caption = 'Disconnect Network'
        TabOrder = 6
        OnClick = Button10Click
      end
      object Button32: TButton
        Left = 136
        Top = 32
        Width = 115
        Height = 25
        Caption = 'Open Dialog'
        TabOrder = 7
        OnClick = Button32Click
      end
      object Button33: TButton
        Left = 136
        Top = 64
        Width = 115
        Height = 25
        Caption = 'Save Dialog'
        TabOrder = 8
        OnClick = Button33Click
      end
      object Button25: TButton
        Left = 136
        Top = 128
        Width = 115
        Height = 25
        Caption = 'Add Hardware'
        TabOrder = 9
        OnClick = Button25Click
      end
      object Button24: TButton
        Left = 136
        Top = 96
        Width = 115
        Height = 25
        Caption = 'Shutdown Dialog'
        TabOrder = 10
        OnClick = Button24Click
      end
      object Button26: TButton
        Left = 136
        Top = 160
        Width = 115
        Height = 25
        Caption = 'Choose Icon'
        TabOrder = 11
        OnClick = Button26Click
      end
      object Button27: TButton
        Left = 136
        Top = 192
        Width = 115
        Height = 25
        Caption = 'Run Dialog'
        TabOrder = 12
        OnClick = Button27Click
      end
      object Button28: TButton
        Left = 136
        Top = 224
        Width = 115
        Height = 25
        Caption = 'Find Computer'
        TabOrder = 13
        OnClick = Button28Click
      end
      object Button29: TButton
        Left = 136
        Top = 256
        Width = 115
        Height = 25
        Caption = 'Object Properties'
        TabOrder = 14
        OnClick = Button29Click
      end
      object Button30: TButton
        Left = 136
        Top = 288
        Width = 115
        Height = 25
        Caption = 'Out Of Memory Dialog'
        TabOrder = 15
        OnClick = Button30Click
      end
      object Button31: TButton
        Left = 136
        Top = 320
        Width = 115
        Height = 25
        Caption = 'Disk C:\ Full Dialog'
        TabOrder = 16
        OnClick = Button31Click
      end
      object Button36: TButton
        Left = 264
        Top = 128
        Width = 115
        Height = 25
        Caption = 'Control Panel Dialog'
        TabOrder = 17
        OnClick = Button36Click
      end
      object Button37: TButton
        Left = 264
        Top = 64
        Width = 115
        Height = 25
        Caption = 'New Shortcut Dialog'
        TabOrder = 18
        OnClick = Button37Click
      end
      object Button38: TButton
        Left = 264
        Top = 96
        Width = 115
        Height = 25
        Caption = 'Applet Dialog'
        TabOrder = 19
        OnClick = Button38Click
      end
      object Button39: TButton
        Left = 264
        Top = 160
        Width = 115
        Height = 25
        Caption = 'Favorites'
        TabOrder = 20
        OnClick = Button39Click
      end
      object Button40: TButton
        Left = 264
        Top = 32
        Width = 115
        Height = 25
        Caption = 'OpenWith Dialog'
        TabOrder = 21
        OnClick = Button40Click
      end
      object Button3: TButton
        Left = 8
        Top = 128
        Width = 115
        Height = 25
        Caption = 'Browse for Folder'
        TabOrder = 22
        OnClick = Button3Click
      end
      object Button7: TButton
        Left = 264
        Top = 192
        Width = 115
        Height = 25
        Caption = 'Page Setup'
        TabOrder = 23
        OnClick = Button7Click
      end
      object Button41: TButton
        Left = 264
        Top = 224
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
      object Button18: TButton
        Left = 8
        Top = 16
        Width = 115
        Height = 25
        Caption = 'Calculator'
        TabOrder = 0
        OnClick = Button18Click
      end
      object Button20: TButton
        Left = 8
        Top = 48
        Width = 115
        Height = 25
        Caption = 'Disk Prompt'
        TabOrder = 1
        OnClick = Button20Click
      end
      object Button21: TButton
        Left = 8
        Top = 144
        Width = 115
        Height = 25
        Caption = 'Copy Error'
        TabOrder = 2
        OnClick = Button21Click
      end
      object Button22: TButton
        Left = 8
        Top = 80
        Width = 115
        Height = 25
        Caption = 'Delete Error'
        TabOrder = 3
        OnClick = Button22Click
      end
      object Button23: TButton
        Left = 8
        Top = 112
        Width = 115
        Height = 25
        Caption = 'Rename Error'
        TabOrder = 4
        OnClick = Button23Click
      end
    end
  end
  object JvFormatDriveDialog1: TJvFormatDriveDialog
    FormatType = ftQuick
    Capacity = dcDefault
    Left = 440
    Top = 16
  end
  object JvFindFiles1: TJvFindFilesDialog
    SpecialFolder = sfRecycleBin
    UseSpecialFolder = False
    Left = 440
    Top = 64
  end
  object JvBrowseFolder1: TJvBrowseForFolderDialog
    RootDirectory = fdRootFolder
    Left = 440
    Top = 112
  end
  object JvSelectDirectory1: TJvSelectDirectory
    Options = []
    Left = 440
    Top = 208
  end
  object JvConnectNetwork1: TJvConnectNetwork
    Left = 440
    Top = 304
  end
  object JvDisconnectNetwork1: TJvDisconnectNetwork
    Left = 440
    Top = 352
  end
  object JvCalculator1: TJvCalculator
    Left = 760
    Top = 16
  end
  object JvDiskPrompt1: TJvDiskPrompt
    Left = 760
    Top = 64
  end
  object JvCopyError1: TJvCopyError
    Left = 760
    Top = 208
  end
  object JvDeleteError1: TJvDeleteError
    Style = []
    Left = 760
    Top = 112
  end
  object JvRenameError1: TJvRenameError
    Left = 760
    Top = 160
  end
  object JvShutdownDlg1: TJvExitWindowsDialog
    Left = 552
    Top = 112
  end
  object JvShellAboutDialog1: TJvShellAboutDialog
    Left = 440
    Top = 160
  end
  object JvAddHardwareDialog1: TJvAddHardwareDialog
    Left = 552
    Top = 160
  end
  object JvChooseIconDlg1: TJvChangeIconDialog
    IconIndex = 0
    Left = 552
    Top = 208
  end
  object JvRunDlg1: TJvRunDialog
    Left = 552
    Top = 256
  end
  object JvFindComputerDlg1: TJvComputerNameDialog
    Left = 552
    Top = 304
  end
  object JvObjectPropertiesDlg1: TJvObjectPropertiesDialog
    ObjectType = sdPathObject
    Left = 552
    Top = 352
  end
  object JvOutOfMemoryDlg1: TJvOutOfMemoryDialog
    Left = 552
    Top = 400
  end
  object JvOutOfSpaceDlg1: TJvDiskFullDialog
    Left = 552
    Top = 456
  end
  object JvPageSetupDialog1: TJvPageSetupDialog
    Left = 672
    Top = 256
  end
  object JvPageSetupTitledDialog1: TJvPageSetupTitledDialog
    Left = 672
    Top = 304
  end
  object JvOrganizeFavoritesDialog1: TJvOrganizeFavoritesDialog
    Left = 672
    Top = 208
  end
  object JvAppletDialog1: TJvAppletDialog
    Left = 672
    Top = 136
  end
  object JvNewLinkDialog1: TJvNewLinkDialog
    DestinationFolder = 'C:\'
    Left = 672
    Top = 64
  end
  object JvOpenWithDialog1: TJvOpenWithDialog
    Left = 672
    Top = 16
  end
  object JvAddPrinterDialog1: TJvAddPrinterDialog
    Left = 440
    Top = 256
  end
  object JvOpenDialog1: TJvOpenDialog
    Height = 0
    Width = 0
    Left = 552
    Top = 16
  end
  object JvSaveDialog1: TJvSaveDialog
    Height = 0
    Width = 0
    Left = 552
    Top = 64
  end
end
