object fPrintSetup: TfPrintSetup
  Left = 520
  Top = 121
  Width = 432
  Height = 89
  Caption = 'fPrintSetup'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object rgOrientation: TRadioGroup
    Left = 8
    Top = 0
    Width = 200
    Height = 56
    Caption = 'Ориентация'
    Items.Strings = (
      'Книжная'
      'Альбомная')
    TabOrder = 0
  end
  object RadioGroup2: TRadioGroup
    Left = 216
    Top = 0
    Width = 200
    Height = 56
    Caption = 'Печать'
    Items.Strings = (
      'Односторонняя'
      'Двусторонняя')
    TabOrder = 1
  end
end
