object JvWindowsLister: TJvWindowsLister
  Left = 336
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Lister Demo'
  ClientHeight = 490
  ClientWidth = 462
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
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 18
    Top = 21
    Width = 375
    Height = 412
    Buttons = [capClose, capHelp]
    Caption = 'demo for getting information of all windows (no comp)'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 0
    object ListBox1: TListBox
      Left = 44
      Top = 66
      Width = 301
      Height = 319
      ItemHeight = 13
      TabOrder = 0
    end
    object Button1: TButton
      Left = 140
      Top = 24
      Width = 75
      Height = 25
      Caption = '&Refresh'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
