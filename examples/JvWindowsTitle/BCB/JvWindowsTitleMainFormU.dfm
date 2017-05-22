object JvWindowsTitleMainForm: TJvWindowsTitleMainForm
  Left = 139
  Top = 107
  Width = 377
  Height = 310
  Caption = 'Lister Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 124
    Top = 16
    Width = 75
    Height = 25
    Caption = '&Refresh'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 12
    Top = 50
    Width = 341
    Height = 227
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
end
