object foMain: TfoMain
  Left = 502
  Top = 466
  BorderStyle = bsDialog
  Caption = 'Hopital'
  ClientHeight = 131
  ClientWidth = 128
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object IdHTTPServer1: TIdHTTPServer
    Active = True
    Bindings = <>
    DefaultPort = 8000
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 12
    Top = 48
  end
  object JvTrayIcon1: TJvTrayIcon
    Active = True
    IconIndex = -1
    Hint = 'Hospital'
    PopupMenu = MainMenu
    Visibility = [tvAutoHide]
    OnDblClick = JvTrayIcon1DblClick
    Left = 48
    Top = 48
  end
  object MainMenu: TPopupMenu
    Left = 12
    Top = 80
    object Homepage1: TMenuItem
      Action = actHome
      Default = True
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Fermer1: TMenuItem
      Action = actExit
    end
  end
  object ActionList1: TActionList
    Left = 50
    Top = 78
    object actHome: TAction
      Caption = '&Start Page'
      OnExecute = actHomeExecute
    end
    object actExit: TAction
      Caption = 'E&xit'
      OnExecute = actExitExecute
    end
  end
end
