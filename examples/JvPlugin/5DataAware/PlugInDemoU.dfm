object Form1: TForm1
  Left = 449
  Top = 148
  Width = 414
  Height = 315
  Caption = 'Sample Plugin Host Application'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 406
    Height = 261
    Align = alClient
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object uilPluginManager: TJvPluginManager
    Extension = 'dll'
    PluginKind = plgDLL
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
