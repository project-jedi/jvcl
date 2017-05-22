object frmSpellChecker: TfrmSpellChecker
  Left = 338
  Top = 177
  BorderStyle = bsDialog
  Caption = 'Spell check document'
  ClientHeight = 334
  ClientWidth = 384
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 72
    Height = 13
    Caption = 'Not in wordlist:'
  end
  object Label2: TLabel
    Left = 16
    Top = 102
    Width = 62
    Height = 13
    Caption = 'Suggestions:'
  end
  object lblNoSuggestions: TLabel
    Left = 179
    Top = 101
    Width = 93
    Height = 13
    Caption = '(nothing to display)'
    Visible = False
  end
  object Label3: TLabel
    Left = 16
    Top = 56
    Width = 65
    Height = 13
    Caption = 'Replace with:'
  end
  object edNewWord: TEdit
    Left = 16
    Top = 72
    Width = 254
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object lbSuggestions: TListBox
    Left = 16
    Top = 120
    Width = 254
    Height = 82
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
    OnClick = lbSuggestionsClick
  end
  object btnIgnore: TButton
    Left = 294
    Top = 32
    Width = 75
    Height = 25
    Action = acIgnore
    Anchors = [akTop, akRight]
    TabOrder = 3
  end
  object btnIgnoreAll: TButton
    Left = 294
    Top = 61
    Width = 75
    Height = 25
    Action = acIgnoreAll
    Anchors = [akTop, akRight]
    TabOrder = 4
  end
  object btnChange: TButton
    Left = 294
    Top = 120
    Width = 75
    Height = 25
    Action = acChange
    Anchors = [akTop, akRight]
    TabOrder = 5
  end
  object btnClose: TButton
    Left = 294
    Top = 286
    Width = 75
    Height = 25
    Action = acClose
    Anchors = [akRight, akBottom]
    Cancel = True
    TabOrder = 8
  end
  object btnAdd: TButton
    Left = 294
    Top = 149
    Width = 75
    Height = 25
    Action = acAdd
    Anchors = [akTop, akRight]
    TabOrder = 6
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 209
    Width = 257
    Height = 105
    Anchors = [akLeft, akRight, akBottom]
    Caption = ' Ignore: '
    TabOrder = 7
    object chkUpperCase: TCheckBox
      Left = 24
      Top = 23
      Width = 207
      Height = 17
      Caption = '&UPPERCASE words'
      TabOrder = 0
    end
    object chkNumber: TCheckBox
      Left = 24
      Top = 39
      Width = 207
      Height = 17
      Caption = 'Words with &numbers'
      TabOrder = 1
    end
    object chkURL: TCheckBox
      Left = 24
      Top = 55
      Width = 207
      Height = 17
      Caption = 'Internet and file &paths'
      TabOrder = 2
    end
    object chkHTML: TCheckBox
      Left = 24
      Top = 71
      Width = 207
      Height = 17
      Caption = 'HT&ML code'
      TabOrder = 3
    end
  end
  object edBadWord: TEdit
    Left = 16
    Top = 32
    Width = 254
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object alSpell: TActionList
    OnUpdate = alSpellUpdate
    Left = 296
    Top = 224
    object acIgnore: TAction
      Caption = '&Ignore'
      OnExecute = acIgnoreExecute
    end
    object acIgnoreAll: TAction
      Caption = 'Ignore &All'
      OnExecute = acIgnoreAllExecute
    end
    object acChange: TAction
      Caption = 'C&hange'
      OnExecute = acChangeExecute
    end
    object acAdd: TAction
      Caption = 'A&dd'
      OnExecute = acAddExecute
    end
    object acClose: TAction
      Caption = '&Close'
      ShortCut = 27
      OnExecute = acCloseExecute
    end
  end
end
