object FormMain: TFormMain
  Left = 294
  Top = 250
  BorderStyle = bsDialog
  Caption = 'dxgettext to Resourcestring'
  ClientHeight = 133
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = false
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LblStatus: TLabel
    Left = 8
    Top = 40
    Width = 44
    Height = 13
    Caption = 'LblStatus'
  end
  object LblDir: TLabel
    Left = 8
    Top = 12
    Width = 45
    Height = 13
    Caption = '&Directory:'
  end
  object BtnParse: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Parse'
    TabOrder = 6
    OnClick = BtnParseClick
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 56
    Width = 361
    Height = 17
    Min = 0
    Max = 100
    TabOrder = 0
  end
  object CheckBoxSingleResFile: TCheckBox
    Left = 8
    Top = 80
    Width = 161
    Height = 17
    Caption = 'Single Resourcestring file'
    TabOrder = 3
  end
  object BtnQuit: TButton
    Left = 296
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Quit'
    TabOrder = 7
    OnClick = BtnQuitClick
  end
  object EditDirectory: TEdit
    Left = 64
    Top = 8
    Width = 281
    Height = 21
    TabOrder = 1
  end
  object BtnBrowse: TButton
    Left = 344
    Top = 8
    Width = 21
    Height = 21
    Hint = 'Browse...'
    Caption = '...'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = BtnBrowseClick
  end
  object CheckBoxSilent: TCheckBox
    Left = 176
    Top = 80
    Width = 73
    Height = 17
    Caption = 'Silent'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object CheckBoxSubDirs: TCheckBox
    Left = 264
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Sub directories'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
end
