object Form1: TForm1
  Left = 237
  Top = 149
  Width = 522
  Height = 388
  Caption = 'MDI Sample Program'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  WindowMenu = Window1
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object panToolbar: TPanel
    Left = 0
    Top = 0
    Width = 514
    Height = 29
    Align = alTop
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 315
    Width = 514
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object MainMenu1: TMainMenu
    Left = 20
    Top = 40
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Commands1: TMenuItem
      Caption = 'Commands'
    end
    object Window1: TMenuItem
      Caption = '&Window'
    end
  end
  object uilPluginManager1: TJvPluginManager
    Extension = 'dll'
    PluginKind = plgDLL
    OnNewCommand = uilPluginManager1NewCommand
    Left = 52
    Top = 40
  end
end
