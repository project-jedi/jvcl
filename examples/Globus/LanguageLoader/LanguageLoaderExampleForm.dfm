object Form1: TForm1
  Left = 200
  Top = 168
  Caption = 'Form1'
  ClientHeight = 434
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 32
    Top = 48
    Width = 104
    Height = 36
    Caption = 'English'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -32
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lbl2: TLabel
    Left = 32
    Top = 88
    Width = 84
    Height = 36
    Caption = 'Dutch'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -32
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lbl3: TLabel
    Left = 32
    Top = 128
    Width = 100
    Height = 36
    Caption = 'French'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -32
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblWarning: TLabel
    Left = 248
    Top = 36
    Width = 413
    Height = 178
    AutoSize = False
    Caption = 
      'WARNING: do not use this component for your localization, the re' +
      'commended method is to use DxGettext'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    WordWrap = True
  end
  object btnOK: TButton
    Left = 40
    Top = 184
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 152
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
  end
  object MainMenu: TJvMainMenu
    ImageMargin.Left = 0
    ImageMargin.Top = 0
    ImageMargin.Right = 0
    ImageMargin.Bottom = 0
    ImageSize.Height = 0
    ImageSize.Width = 0
    Left = 8
    Top = 8
    object MM_Bestand: TMenuItem
      Caption = '&File'
      HelpContext = 610
      object FileNew: TMenuItem
        Caption = '&New'
      end
      object FileOpen: TMenuItem
        Caption = '&Open'
      end
      object FileSave: TMenuItem
        Caption = '&Save'
      end
      object FileExit: TMenuItem
        Caption = 'E&xit'
        OnClick = FileExitClick
      end
    end
    object MM_Bewerken: TMenuItem
      Caption = '&Edit'
      HelpContext = 620
      object MM_EditUndo: TMenuItem
        Caption = '&Undo'
      end
      object Redo1: TMenuItem
        Caption = '&Redo'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Caption = 'Cu&t'
      end
      object Copy1: TMenuItem
        Caption = '&Copy'
      end
      object Paste1: TMenuItem
        Caption = '&Paste'
      end
    end
    object SelectLang: TMenuItem
      Caption = '&Language'
      object LangEnglish: TMenuItem
        Caption = '&English'
        GroupIndex = 3
        OnClick = LangEnglishClick
      end
      object LangDutch: TMenuItem
        Caption = '&Dutch'
        GroupIndex = 3
        OnClick = LangDutchClick
      end
      object LangFrench: TMenuItem
        Caption = '&French'
        GroupIndex = 3
        OnClick = LangFrenchClick
      end
    end
    object MM_Help: TMenuItem
      Caption = '&Help'
      HelpContext = 670
      object HelpShowHelp: TMenuItem
        Caption = '&Help'
      end
      object HelpAbout: TMenuItem
        Caption = '&About...'
      end
    end
  end
  object JvgLanguageLoader: TJvgLanguageLoader
    FormSection = 'Form1'
    StringsSection = 'Strings'
    Left = 40
    Top = 8
  end
end
