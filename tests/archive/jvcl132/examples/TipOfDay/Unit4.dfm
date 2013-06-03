object Form4: TForm4
  Left = 320
  Top = 278
  Width = 379
  Height = 332
  Caption = 'JVCL Tips Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 160
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Load Tips'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 56
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Show Tips'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 256
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Save Tips'
    TabOrder = 2
    OnClick = Button3Click
  end
  object JvTip: TJvTipWindow
    Caption = 'JVCL Tips Component'
    ShowTips = False
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -19
    TitleFont.Name = 'Arial'
    TitleFont.Style = [fsBold]
    TipFont.Charset = DEFAULT_CHARSET
    TipFont.Color = clWindowText
    TipFont.Height = -11
    TipFont.Name = 'Arial'
    TipFont.Style = []
    Left = 112
    Top = 88
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Tips (*.txt)|*.txt'
    Left = 64
    Top = 88
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Tips (*.txt)|*.txt'
    Left = 160
    Top = 88
  end
end
