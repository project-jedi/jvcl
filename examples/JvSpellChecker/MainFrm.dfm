object frmMain: TfrmMain
  Left = 353
  Top = 156
  Width = 535
  Height = 398
  Caption = 'JvSpellChecker Demo'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 320
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object reText: TRichEdit
    Left = 0
    Top = 0
    Width = 527
    Height = 325
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    HideSelection = False
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WantTabs = True
    WordWrap = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 325
    Width = 527
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object mmMain: TMainMenu
    Left = 120
    Top = 64
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = 'Close'
        ShortCut = 32883
        OnClick = Close1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object SpellCheck1: TMenuItem
        Caption = 'Spell Check...'
        ShortCut = 118
        OnClick = SpellCheck1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Saveasimage1: TMenuItem
        Caption = 'Save as image...'
        OnClick = Saveasimage1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Text files|*.txt|RTF files|*.rtf|All files|*.*'
    Left = 168
    Top = 160
  end
  object SaveDialog1: TSaveDialog
    FileName = 'new.bmp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 272
    Top = 192
  end
end
