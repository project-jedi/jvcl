object FormSer: TFormSer
  Left = 423
  Top = 336
  BorderStyle = bsDialog
  Caption = 'Registration infos'
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
  object StaticText1: TStaticText
    Left = 136
    Top = 16
    Width = 247
    Height = 73
    AutoSize = False
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 134
    Top = 132
    Width = 253
    Height = 81
    Caption = '[ Username and serial ]'
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 22
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label2: TLabel
      Left = 22
      Top = 48
      Width = 26
      Height = 13
      Caption = 'Serial'
    end
    object Edit1: TEdit
      Left = 66
      Top = 18
      Width = 177
      Height = 21
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 66
      Top = 44
      Width = 177
      Height = 21
      TabOrder = 1
    end
  end
end
