object FormUnin: TFormUnin
  Left = 476
  Top = 341
  BorderStyle = bsDialog
  Caption = 'Uninstall'
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
  object BUButton2: TJvSpeedButton
    Left = 230
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Finish'
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
    Caption = 
      'This program is uninstalling the program, please'#13#10'wait during th' +
      'is process !. This operation is non'#13#10'reversible, all files are d' +
      'efinitively removed'#13#10'from your disk.'
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 136
    Top = 100
    Width = 247
    Height = 175
    Caption = '[ Uninstall ]'
    TabOrder = 1
    object GroupBox2: TGroupBox
      Left = 8
      Top = 14
      Width = 233
      Height = 73
      Caption = '[ Informations ]'
      TabOrder = 0
      object Label1: TLabel
        Left = 12
        Top = 20
        Width = 45
        Height = 13
        Caption = 'Program :'
      end
      object programname: TLabel
        Left = 66
        Top = 20
        Width = 160
        Height = 13
        AutoSize = False
      end
      object Path: TLabel
        Left = 66
        Top = 44
        Width = 160
        Height = 13
        AutoSize = False
      end
      object Label2: TLabel
        Left = 20
        Top = 44
        Width = 28
        Height = 13
        Caption = 'Path :'
      end
    end
    object GroupBox3: TGroupBox
      Left = 8
      Top = 92
      Width = 231
      Height = 77
      Caption = '[ Status ]'
      TabOrder = 1
      object Label3: TLabel
        Left = 8
        Top = 20
        Width = 61
        Height = 13
        Caption = 'Deleting file :'
      end
      object currfile: TStaticText
        Left = 8
        Top = 36
        Width = 217
        Height = 37
        AutoSize = False
        TabOrder = 0
      end
    end
  end
end
