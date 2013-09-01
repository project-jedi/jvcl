object jvSpellerF: TJvSpellerF
  Left = 306
  Top = 251
  BorderStyle = bsDialog
  Caption = 'Spelling checker'
  ClientHeight = 153
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblcontext: TLabel
    Left = 0
    Top = 33
    Width = 371
    Height = 86
    Hint = 'look ahead box'
    Align = alClient
    Caption = 'lblcontext'
    ParentShowHint = False
    ShowHint = True
    WordWrap = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 371
    Height = 33
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object txtspell: TEdit
      Left = 7
      Top = 7
      Width = 293
      Height = 21
      TabOrder = 0
      Text = 'txtspell'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 119
    Width = 371
    Height = 34
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object btnSkip: TButton
      Left = 13
      Top = 7
      Width = 61
      Height = 20
      Hint = 'Skip this word'
      Caption = 'Skip'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object btnchange: TButton
      Left = 228
      Top = 7
      Width = 60
      Height = 20
      Hint = 'Change to corrected word'
      Caption = 'Change'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object btncancel: TButton
      Left = 299
      Top = 7
      Width = 61
      Height = 20
      Hint = 'Abort all changes'
      Caption = 'Cancel'
      ModalResult = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object btnAdd: TButton
      Left = 156
      Top = 7
      Width = 61
      Height = 20
      Hint = 'Add to user Dictionary'
      Caption = 'Add'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object btnskipall: TButton
      Left = 85
      Top = 7
      Width = 60
      Height = 20
      Hint = 'Skip all, update and finish'
      Caption = 'Skip All'
      ModalResult = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
  end
end
