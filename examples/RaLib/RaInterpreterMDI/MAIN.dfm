object MainForm: TMainForm
  Left = 391
  Top = 151
  Width = 376
  Height = 301
  Caption = 'MDI Application'
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDefault
  WindowMenu = Window1
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 233
    Width = 368
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 200
    object File1: TMenuItem
      Caption = '&File'
      Hint = 'File related commands'
      object FileNewItem: TMenuItem
        Caption = '&New'
        Hint = 'New|Create a new file'
        ShortCut = 16462
        OnClick = FileNew1Execute
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object FileExitItem: TMenuItem
        Caption = 'E&xit'
        Hint = 'Exit|Exit application'
        OnClick = FileExit1Execute
      end
    end
    object Window1: TMenuItem
      Caption = '&Window'
      Hint = 'Window related commands'
      object N2: TMenuItem
        Caption = '-'
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      Hint = 'Help topics'
      object HelpAboutItem: TMenuItem
        Caption = '&About...'
        Hint = 
          'About|Displays program information, version number, and copyrigh' +
          't'
        OnClick = HelpAbout1Execute
      end
    end
  end
  object JvInterpreterFm1: TJvInterpreterFm
    Left = 112
    Top = 200
  end
end
