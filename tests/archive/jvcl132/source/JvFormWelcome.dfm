object FormW: TFormW
  Left = 491
  Top = 338
  BorderStyle = bsDialog
  Caption = 'Welcome'
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
    Left = 146
    Top = 18
    Width = 235
    Height = 31
    AutoSize = False
    Caption = 
      'Welcome to the Setup program. This program '#13#10'will install the so' +
      'ftware on your computer.'
    TabOrder = 0
  end
  object StaticText2: TStaticText
    Left = 146
    Top = 60
    Width = 237
    Height = 105
    AutoSize = False
    Caption = 
      'It is strongly recommended that you exit all '#13#10'Windows programs ' +
      'before running this Setup '#13#10'program.'#13#10#13#10'Click Cancel to quit set' +
      'up and then close any '#13#10'programs you have running (or just use a' +
      'lt-tab). '#13#10'Click next to continue with the setup program.'
    TabOrder = 1
  end
  object StaticText3: TStaticText
    Left = 146
    Top = 178
    Width = 239
    Height = 99
    AutoSize = False
    Caption = 
      'Warning : This program is protected by copyright'#13#10'law and intern' +
      'ational treaties.'#13#10#13#10'Unauthorized reproduction or distribution o' +
      'f this '#13#10'program or any portion of it, may result in severe '#13#10'ci' +
      'vil and criminal penalties, and will be prosecuted '#13#10'to the maxi' +
      'mum extent possible under law.'
    TabOrder = 2
  end
end
