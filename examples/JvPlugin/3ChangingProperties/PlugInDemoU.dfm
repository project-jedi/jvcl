object Form1: TForm1
  Left = 451
  Top = 149
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Sample Plugin Host Application'
  ClientHeight = 191
  ClientWidth = 284
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 32
    Width = 31
    Height = 13
    Caption = 'Items :'
  end
  object Listbox1: TListBox
    Left = 4
    Top = 48
    Width = 133
    Height = 141
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 284
    Height = 29
    Align = alTop
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object Button1: TButton
    Left = 144
    Top = 48
    Width = 133
    Height = 37
    Caption = 'Button'
    TabOrder = 2
  end
  object uilPluginManager: TJvPluginManager
    Extension = 'dll'
    PluginKind = plgDLL
    OnNewCommand = uilPluginManagerNewCommand
    Left = 44
    Top = 56
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 56
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Plugin1: TMenuItem
      Caption = '&Plug-in'
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
end
