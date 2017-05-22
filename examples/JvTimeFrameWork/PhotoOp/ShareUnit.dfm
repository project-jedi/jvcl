object Share: TShare
  Left = 738
  Top = 285
  Width = 271
  Height = 216
  Caption = 'Share'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object ResourcesCheckList: TCheckListBox
    Left = 16
    Top = 8
    Width = 225
    Height = 129
    ItemHeight = 16
    TabOrder = 0
  end
  object OKButton: TBitBtn
    Left = 40
    Top = 152
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object CancelButton: TBitBtn
    Left = 152
    Top = 152
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
