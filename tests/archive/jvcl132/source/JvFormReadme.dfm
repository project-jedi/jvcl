object FormRead: TFormRead
  Left = 542
  Top = 363
  BorderStyle = bsDialog
  Caption = 'Informations'
  ClientHeight = 323
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 10
    Top = 16
    Width = 120
    Height = 260
  end
  object Bevel1: TBevel
    Left = 4
    Top = 280
    Width = 381
    Height = 3
  end
  object BUButton1: TJvSpeedButton
    Left = 150
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Previous'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton2: TJvSpeedButton
    Left = 230
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Next'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton3: TJvSpeedButton
    Left = 310
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Cancel'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Memo1: TMemo
    Left = 140
    Top = 16
    Width = 239
    Height = 260
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
