object FormInst: TFormInst
  Left = 496
  Top = 284
  BorderStyle = bsDialog
  Caption = 'Installation'
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
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 136
    Top = 92
    Width = 247
    Height = 183
    Caption = '[ Progress ]'
    TabOrder = 1
    object GroupBox2: TGroupBox
      Left = 6
      Top = 14
      Width = 233
      Height = 87
      Caption = '[ Current File ]'
      TabOrder = 0
      object Label1: TLabel
        Left = 12
        Top = 18
        Width = 48
        Height = 13
        Caption = 'Filename :'
      end
      object Label3: TLabel
        Left = 12
        Top = 36
        Width = 46
        Height = 13
        Caption = 'Time left :'
      end
      object filename: TLabel
        Left = 71
        Top = 18
        Width = 151
        Height = 13
        AutoSize = False
      end
      object currentleft: TLabel
        Left = 71
        Top = 36
        Width = 151
        Height = 13
        AutoSize = False
      end
      object Gauge1: TProgressBar
        Left = 10
        Top = 60
        Width = 213
        Height = 17
        Min = 0
        Max = 100
        Smooth = True
        TabOrder = 0
      end
    end
    object GroupBox3: TGroupBox
      Left = 6
      Top = 106
      Width = 233
      Height = 73
      Caption = '[ Total ]'
      TabOrder = 1
      object Label2: TLabel
        Left = 12
        Top = 18
        Width = 46
        Height = 13
        Caption = 'Time left :'
      end
      object totalleft: TLabel
        Left = 70
        Top = 18
        Width = 151
        Height = 13
        AutoSize = False
      end
      object Gauge2: TProgressBar
        Left = 10
        Top = 46
        Width = 213
        Height = 17
        Min = 0
        Max = 100
        Smooth = True
        TabOrder = 0
      end
    end
  end
end
