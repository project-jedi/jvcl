object Form1: TForm1
  Left = 353
  Top = 148
  Width = 446
  Height = 308
  Caption = 'Sample Plugin Host Application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
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
    Height = 233
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
    Height = 89
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 438
    Height = 29
    Align = alTop
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object Panel2: TPanel
    Left = 240
    Top = 36
    Width = 190
    Height = 221
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
      Width = 130
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
      Width = 174
      Height = 149
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      WordWrap = True
    end
    object Label3: TLabel
      Left = 1
      Top = 203
      Width = 188
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
    OnAfterLoad = uilPluginManagerAfterLoad
    OnNewCommand = uilPluginManagerNewCommand
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
