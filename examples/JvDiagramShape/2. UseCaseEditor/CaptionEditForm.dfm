object CaptionEditDlg: TCaptionEditDlg
  Left = 236
  Top = 133
  Width = 345
  Height = 178
  Caption = 'Caption Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 60
    Height = 13
    Caption = 'New caption'
  end
  object OkBtn: TButton
    Left = 164
    Top = 116
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 244
    Top = 116
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object CaptionEdit: TMemo
    Left = 92
    Top = 16
    Width = 225
    Height = 89
    TabOrder = 0
  end
  object FontBtn: TButton
    Left = 16
    Top = 116
    Width = 75
    Height = 25
    Caption = 'Font...'
    TabOrder = 1
    OnClick = FontBtnClick
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 104
    Top = 116
  end
end
