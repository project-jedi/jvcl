object Form1: TForm1
  Left = 353
  Top = 137
  Width = 395
  Height = 436
  Caption = 'JvCaptionButton Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnAdd: TButton
    Left = 16
    Top = 32
    Width = 75
    Height = 25
    Caption = '&Add'
    TabOrder = 0
    OnClick = btnAddClick
  end
  object btnDelete: TButton
    Left = 16
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Delete'
    TabOrder = 1
    OnClick = btnDeleteClick
  end
  object btnRecreateWnd: TButton
    Left = 16
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Recreate&Wnd'
    TabOrder = 2
    OnClick = btnRecreateWndClick
  end
  object lbButtons: TListBox
    Left = 128
    Top = 32
    Width = 233
    Height = 353
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
end
