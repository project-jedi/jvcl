object frmMain: TfrmMain
  Left = 295
  Top = 159
  Width = 585
  Height = 448
  Caption = 'JvComputerInfoEx Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 383
    Width = 577
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 577
    Height = 383
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Computer Info'
      object reInfo: TJvRichEdit
        Left = 0
        Top = 0
        Width = 569
        Height = 355
        Align = alClient
        ReadOnly = True
        TabOrder = 0
        WantReturns = False
        WordWrap = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Events'
      ImageIndex = 1
      object lvEvents: TListView
        Left = 0
        Top = 0
        Width = 569
        Height = 355
        Align = alClient
        Columns = <
          item
            Caption = 'Date'
            Width = 120
          end
          item
            Caption = 'Event'
            Width = 300
          end
          item
            AutoSize = True
            Caption = 'Info'
          end>
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Modify'
      ImageIndex = 2
      object JvInspector1: TJvInspector
        Left = 0
        Top = 0
        Width = 569
        Height = 314
        Align = alClient
        BandWidth = 150
        BevelKind = bkTile
        BevelInner = bvNone
        RelativeDivider = True
        Divider = 45
        ItemHeight = 16
        Painter = JvInspectorBorlandPainter1
        ReadOnly = False
        UseBands = False
        WantTabs = False
      end
      object Panel1: TPanel
        Left = 0
        Top = 314
        Width = 569
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object chkReadOnly: TCheckBox
          Left = 16
          Top = 13
          Width = 97
          Height = 17
          Caption = 'Read Only'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = chkReadOnlyClick
        end
      end
    end
  end
  object JvComputerInfoEx1: TJvComputerInfoEx
    OnDeviceAdded = JvComputerInfoEx1DeviceAdded
    OnDeviceRemoved = JvComputerInfoEx1DeviceRemoved
    OnSettingChange = JvComputerInfoEx1SettingChange
    OnCompacting = JvComputerInfoEx1Compacting
    OnPowerBroadcast = JvComputerInfoEx1PowerBroadcast
    OnUserChanged = JvComputerInfoEx1UserChanged
    OnDeviceChange = JvComputerInfoEx1DeviceChange
    OnDeviceModeChange = JvComputerInfoEx1DeviceModeChange
    OnDisplayChange = JvComputerInfoEx1DisplayChange
    OnTimeChange = JvComputerInfoEx1TimeChange
    OnFontChange = JvComputerInfoEx1FontChange
    OnSysColorChange = JvComputerInfoEx1SysColorChange
    OnSpoolerStatusChange = JvComputerInfoEx1SpoolerStatusChange
    OnPaletteChanging = JvComputerInfoEx1PaletteChanging
    OnPaletteChanged = JvComputerInfoEx1PaletteChanged
    Left = 48
    Top = 24
  end
  object mmMain: TJvMainMenu
    Style = msXP
    ImageMargin.Left = 0
    ImageMargin.Top = 0
    ImageMargin.Right = 0
    ImageMargin.Bottom = 0
    ImageSize.Height = 0
    ImageSize.Width = 0
    Left = 16
    Top = 24
    object File1: TMenuItem
      Caption = 'File'
      object SaveAs1: TMenuItem
        Action = acSaveAs
      end
      object PrinterSetup1: TMenuItem
        Action = acPrinterSetup
      end
      object Print1: TMenuItem
        Action = acPrint
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = acExit
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Refresh1: TMenuItem
        Action = acRefresh
      end
      object ClearEvents1: TMenuItem
        Action = acClearEvents
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ReloadIcons1: TMenuItem
        Action = acReloadIcons
      end
      object ReloadCursors1: TMenuItem
        Action = acReloadCursors
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Action = acAbout
      end
    end
  end
  object alMain: TActionList
    Left = 80
    Top = 24
    object acSaveAs: TAction
      Caption = 'Save As...'
      ShortCut = 16467
      OnExecute = acSaveAsExecute
    end
    object acPrinterSetup: TAction
      Caption = 'Printer Setup...'
      ShortCut = 24656
      OnExecute = acPrinterSetupExecute
    end
    object acPrint: TAction
      Caption = 'Print...'
      ShortCut = 16464
      OnExecute = acPrintExecute
    end
    object acExit: TAction
      Caption = 'Exit'
      ShortCut = 32883
      OnExecute = acExitExecute
    end
    object acRefresh: TAction
      Caption = 'Refresh'
      ShortCut = 116
      OnExecute = acRefreshExecute
    end
    object acAbout: TAction
      Caption = 'About...'
      ShortCut = 112
      OnExecute = acAboutExecute
    end
    object acClearEvents: TAction
      Caption = 'Clear Events'
      ShortCut = 16430
      OnExecute = acClearEventsExecute
    end
    object acReloadIcons: TAction
      Caption = 'Reload Icons'
      OnExecute = acReloadIconsExecute
    end
    object acReloadCursors: TAction
      Caption = 'Reload Cursors'
      OnExecute = acReloadCursorsExecute
    end
  end
  object JvSaveDialog1: TJvSaveDialog
    DefaultExt = 'txt'
    InitialDir = '.'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    ActiveStyle = asReport
    Height = 0
    UseUserSize = True
    Width = 0
    Left = 16
    Top = 56
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 80
    Top = 56
  end
  object PrintDialog1: TPrintDialog
    Left = 48
    Top = 56
  end
  object JvInspectorBorlandPainter1: TJvInspectorBorlandPainter
    Left = 114
    Top = 24
  end
end
