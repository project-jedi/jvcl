object Form1: TForm1
  Left = 336
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Lister Demo'
  ClientHeight = 283
  ClientWidth = 222
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
  object Button1: TButton
    Left = 76
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
    Width = 203
    Height = 227
    ItemHeight = 13
    TabOrder = 1
  end
  object JvWindowsTitle1: TJvWindowsTitle
    OnBeforeList = JvWindowsTitle1BeforeList
    OnList = JvWindowsTitle1List
    Left = 22
    Top = 16
  end
end
