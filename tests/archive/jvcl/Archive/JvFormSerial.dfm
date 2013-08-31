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
    Height = 252
  end
  object Bevel1: TBevel
    Left = 4
    Top = 280
    Width = 381
    Height = 3
  end
  object Button1: TButton
    Left = 150
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Previous'
    TabOrder = 2
  end
  object Button2: TButton
    Left = 230
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Next'
    TabOrder = 3
  end
  object Button3: TButton
    Left = 310
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Cancel'
    TabOrder = 4
  end
  object StaticText1: TStaticText
    Left = 136
    Top = 16
    Width = 247
    Height = 161
    AutoSize = False
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 134
    Top = 188
    Width = 253
    Height = 81
    Caption = 'Registration data'
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 22
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label2: TLabel
      Left = 32
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
