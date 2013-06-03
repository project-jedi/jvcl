object Form1: TForm1
  Left = 316
  Top = 248
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 235
  ClientWidth = 226
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 205
    Height = 117
    Caption = 'Absolute positionner'
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 24
      Width = 18
      Height = 13
      Caption = 'Left'
    end
    object Label2: TLabel
      Left = 12
      Top = 50
      Width = 19
      Height = 13
      Caption = 'Top'
    end
    object SpinEdit1: TSpinEdit
      Left = 42
      Top = 20
      Width = 151
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object SpinEdit2: TSpinEdit
      Left = 42
      Top = 48
      Width = 151
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object Button7: TButton
      Left = 64
      Top = 76
      Width = 75
      Height = 25
      Caption = '&Go'
      TabOrder = 2
      OnClick = Button7Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 124
    Width = 205
    Height = 105
    Caption = 'Control positionner'
    TabOrder = 1
    object Button1: TButton
      Left = 124
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
    end
    object Button2: TButton
      Left = 124
      Top = 44
      Width = 75
      Height = 25
      Caption = 'Button2'
      TabOrder = 1
    end
    object Button3: TButton
      Left = 124
      Top = 74
      Width = 75
      Height = 25
      Caption = 'Button3'
      TabOrder = 2
    end
    object Button4: TButton
      Left = 8
      Top = 18
      Width = 75
      Height = 25
      Caption = 'Go to button1'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 8
      Top = 46
      Width = 75
      Height = 25
      Caption = 'Go to button2'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 8
      Top = 74
      Width = 75
      Height = 25
      Caption = 'Go to button3'
      TabOrder = 5
      OnClick = Button6Click
    end
  end
  object JvMousePositionner1: TJvMousePositionner
    Left = 32
    Top = 82
  end
end
