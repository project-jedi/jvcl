object JvSpellerForm: TJvSpellerForm
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
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LblContext: TLabel
    Left = 0
    Top = 33
    Width = 371
    Height = 86
    Hint = 'look ahead box'
    Align = alClient
    Caption = 'LblContext'
    ParentShowHint = False
    ShowHint = True
    WordWrap = True
  end
  object TextPanel: TPanel
    Left = 0
    Top = 0
    Width = 371
    Height = 33
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object TxtSpell: TEdit
      Left = 7
      Top = 7
      Width = 293
      Height = 21
      TabOrder = 0
      Text = 'TxtSpell'
    end
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 119
    Width = 371
    Height = 34
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object BtnSkip: TButton
      Left = 13
      Top = 7
      Width = 61
      Height = 20
      Hint = 'Skip this word'
      Caption = '&Skip'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object BtnChange: TButton
      Left = 228
      Top = 7
      Width = 60
      Height = 20
      Hint = 'Change to corrected word'
      Caption = '&Change'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object BtnCancel: TButton
      Left = 299
      Top = 7
      Width = 61
      Height = 20
      Hint = 'Abort all changes'
      Caption = 'Cancel'
      ModalResult = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
    end
    object BtnAdd: TButton
      Left = 156
      Top = 7
      Width = 61
      Height = 20
      Hint = 'Add to user Dictionary'
      Caption = '&Add'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object BtnSkipAll: TButton
      Left = 85
      Top = 7
      Width = 60
      Height = 20
      Hint = 'Skip all, update and finish'
      Caption = 'S&kip All'
      ModalResult = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
  end
end
