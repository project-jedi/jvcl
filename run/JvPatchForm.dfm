object FoPatch: TFoPatch
  Left = 419
  Top = 323
  Width = 300
  Height = 193
  Caption = 'Patcher Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 2
    Width = 283
    Height = 97
    TabOrder = 0
    object Label1: TLabel
      Left = 14
      Top = 18
      Width = 34
      Height = 13
      Caption = 'Source'
    end
    object Label2: TLabel
      Left = 6
      Top = 44
      Width = 53
      Height = 13
      Caption = 'Destination'
    end
    object Label3: TLabel
      Left = 8
      Top = 70
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Edit1: TEdit
      Left = 68
      Top = 66
      Width = 207
      Height = 21
      TabOrder = 2
    end
    object FileNameBox1: TJvFilenameEdit
      Left = 68
      Top = 14
      Width = 207
      Height = 21
      ButtonFlat = False
      NumGlyphs = 1
      TabOrder = 0
    end
    object FileNameBox2: TJvFilenameEdit
      Left = 68
      Top = 38
      Width = 207
      Height = 21
      ButtonFlat = False
      NumGlyphs = 1
      TabOrder = 1
    end
  end
  object OkBtn: TButton
    Left = 48
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 1
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 168
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
  end
end
