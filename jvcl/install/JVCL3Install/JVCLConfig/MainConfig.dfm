object FormMainConfig: TFormMainConfig
  Left = 140
  Top = 111
  BorderStyle = bsDialog
  Caption = 'JVCL Configuration'
  ClientHeight = 397
  ClientWidth = 744
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BevelBorder: TBevel
    Left = 278
    Top = 7
    Width = 462
    Height = 141
  end
  object CheckListBox: TCheckListBox
    Left = 0
    Top = 0
    Width = 273
    Height = 397
    OnClickCheck = CheckListBoxClickCheck
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = CheckListBoxClick
  end
  object ScrollBox: TScrollBox
    Left = 280
    Top = 8
    Width = 457
    Height = 137
    BorderStyle = bsNone
    TabOrder = 1
    object LblComment: TLabel
      Left = 8
      Top = 8
      Width = 58
      Height = 13
      Caption = 'LblComment'
    end
  end
  object BtnReload: TBitBtn
    Left = 280
    Top = 368
    Width = 113
    Height = 25
    Caption = '&Reload from file'
    TabOrder = 2
    OnClick = BtnReloadClick
  end
  object BtnQuit: TBitBtn
    Left = 664
    Top = 368
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Quit'
    TabOrder = 4
    OnClick = BtnQuitClick
  end
  object BtnSave: TBitBtn
    Left = 576
    Top = 368
    Width = 75
    Height = 25
    Caption = '&Save'
    TabOrder = 3
    OnClick = BtnSaveClick
  end
end
