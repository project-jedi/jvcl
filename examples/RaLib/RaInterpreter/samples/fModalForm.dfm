object MyModalForm: TMyModalForm
  Left = 200
  Top = 137
  Width = 259
  Height = 164
  Caption = 'Modal Form'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 32
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 144
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Hello: TButton
    Left = 88
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Hello'
    TabOrder = 2
    OnClick = HelloClick
  end
end
