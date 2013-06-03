object FormCanc: TFormCanc
  Left = 398
  Top = 254
  BorderStyle = bsDialog
  Caption = 'Cancel Setup'
  ClientHeight = 165
  ClientWidth = 320
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
  object BUButton2: TJvSpeedButton
    Left = 50
    Top = 132
    Width = 75
    Height = 25
    Caption = '&Resume'
    OnClick = BUButton2Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton1: TJvSpeedButton
    Left = 196
    Top = 132
    Width = 75
    Height = 25
    Caption = '&Exit'
    OnClick = BUButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object StaticText1: TStaticText
    Left = 10
    Top = 10
    Width = 299
    Height = 39
    AutoSize = False
    Caption = 
      'Setup is not complete. If you quit the Setup program now, the pr' +
      'ogram will not be installed !!!'
    TabOrder = 0
  end
  object StaticText2: TStaticText
    Left = 10
    Top = 56
    Width = 287
    Height = 27
    AutoSize = False
    Caption = 'To cancel the installation, just click the button "Exit".'
    TabOrder = 1
  end
  object StaticText3: TStaticText
    Left = 10
    Top = 86
    Width = 285
    Height = 33
    AutoSize = False
    Caption = 
      'To continue the installation of this program, just click the "re' +
      'sume" button.'
    TabOrder = 2
    OnClick = BUButton2Click
  end
end
