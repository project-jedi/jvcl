object frmMain: TfrmMain
  Left = 353
  Top = 282
  Width = 546
  Height = 364
  Caption = 'Main test application for JvPlugin'
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 540
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 161
    Top = 0
    Width = 3
    Height = 337
    Cursor = crHSplit
  end
  object Memo1: TMemo
    Left = 164
    Top = 0
    Width = 374
    Height = 337
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 161
    Height = 337
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 1
    object butLoadPlugins: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Load plugins'
      TabOrder = 0
      OnClick = butLoadPluginsClick
    end
    object lstPlugins: TListBox
      Left = 8
      Top = 48
      Width = 137
      Height = 97
      ItemHeight = 13
      TabOrder = 1
    end
    object butShowPlug: TButton
      Left = 8
      Top = 152
      Width = 89
      Height = 25
      Caption = 'Show plugin'
      TabOrder = 2
      OnClick = butShowPlugClick
    end
    object butUnload: TButton
      Left = 8
      Top = 192
      Width = 89
      Height = 25
      Caption = 'Unload plugins'
      TabOrder = 3
      OnClick = butUnloadClick
    end
  end
  object JvPluginManager1: TJvPluginManager
    Extension = 'bpl'
    PluginKind = plgPackage
    Left = 184
    Top = 8
  end
end
