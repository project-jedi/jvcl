object Form1: TForm1
  Left = 234
  Top = 107
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clRed
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Animate1: TAnimate
    Left = 200
    Top = 128
    Width = 272
    Height = 60
    Active = True
    CommonAVI = aviCopyFile
    StopFrame = 26
  end
  object JvPanel1: TJvPanel
    Left = 464
    Top = 120
    Width = 185
    Height = 193
    Caption = 
      'JvPanel1 JvPanel1 JvPanel1 JvPanel1 JvPanel1 JvPanel1 JvPanel1 J' +
      'vPanel1 '
    TabOrder = 0
    Sizeable = True
    MultiLine = True
    object Label1: TLabel
      Left = 40
      Top = 128
      Width = 31
      Height = 13
      Caption = 'Label1'
      Transparent = True
    end
    object Edit1: TEdit
      Left = 24
      Top = 64
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object JvFilenameEdit1: TJvFilenameEdit
      Left = 24
      Top = 32
      Width = 121
      Height = 21
      ButtonFlat = False
      NumGlyphs = 1
      TabOrder = 1
      Text = 'JvFilenameEdit1'
    end
  end
  object CheckBox1: TCheckBox
    Left = 384
    Top = 360
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object JvFormStorage1: TJvFormStorage
    StoredValues = <>
    Left = 416
    Top = 296
  end
end
