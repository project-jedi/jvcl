object JvID3DefineDlg: TJvID3DefineDlg
  Left = 382
  Top = 194
  BorderStyle = bsDialog
  Caption = 'JvID3DefineDlg'
  ClientHeight = 88
  ClientWidth = 277
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblFrames: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = 'Frame Kind:'
  end
  object cmbFrames: TComboBox
    Left = 8
    Top = 24
    Width = 262
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
  end
  object OkBtn: TButton
    Left = 114
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 194
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
