object Form1: TForm1
  Left = 353
  Top = 148
  Width = 435
  Height = 300
  Caption = 'Sample Plugin Host Application'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 435
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
  object Label4: TLabel
    Left = 4
    Top = 152
    Width = 60
    Height = 13
    Caption = 'Load Status:'
  end
  object Splitter1: TSplitter
    Left = 0
    Top = 29
    Width = 3
    Height = 217
    Cursor = crHSplit
  end
  object clbPlugins: TListBox
    Left = 4
    Top = 36
    Width = 229
    Height = 109
    ItemHeight = 13
    TabOrder = 0
    OnClick = clbPluginsClick
    OnDblClick = clbPluginsDblClick
  end
  object lbStatus: TListBox
    Left = 4
    Top = 168
    Width = 229
    Height = 81
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 427
    Height = 29
    Align = alTop
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object Panel2: TPanel
    Left = 240
    Top = 36
    Width = 179
    Height = 213
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvLowered
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 46
      Height = 13
      Caption = 'Author: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object labAuthor: TLabel
      Left = 52
      Top = 8
      Width = 119
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 69
      Height = 13
      Caption = 'Description:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object labDescription: TLabel
      Left = 8
      Top = 48
      Width = 163
      Height = 141
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      WordWrap = True
    end
    object Label3: TLabel
      Left = 1
      Top = 195
      Width = 177
      Height = 17
      Align = alBottom
      Alignment = taCenter
      AutoSize = False
      Caption = 'Double-click the plugin to configure.'
    end
  end
  object uilPluginManager: TJvPluginManager
    Extension = 'dll'
    PluginKind = plgDLL
    OnBeforeLoad = uilPluginManagerBeforeLoad
    OnNewCommand = uilPluginManagerNewCommand
    OnAfterLoad = uilPluginManagerAfterLoad
    Left = 44
    Top = 44
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 44
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Plugin1: TMenuItem
      Caption = '&Plug-in'
      object SendMessagetoPlugins1: TMenuItem
        Caption = '&Send Message to Plugins'
        OnClick = SendMessagetoPlugins1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
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
