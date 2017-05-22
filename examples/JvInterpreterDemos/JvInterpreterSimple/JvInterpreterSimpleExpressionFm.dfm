object Form1: TForm1
  Left = 192
  Top = 66
  Width = 359
  Height = 177
  Caption = 'Simple Expressions in JvInterpreter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Expression'
  end
  object Label2: TLabel
    Left = 76
    Top = 64
    Width = 7
    Height = 13
    Caption = 'A'
  end
  object Label3: TLabel
    Left = 76
    Top = 88
    Width = 7
    Height = 13
    Caption = 'B'
  end
  object Label4: TLabel
    Left = 76
    Top = 112
    Width = 7
    Height = 13
    Caption = 'C'
  end
  object Label5: TLabel
    Left = 8
    Top = 32
    Width = 30
    Height = 13
    Caption = 'Result'
  end
  object Button1: TButton
    Left = 232
    Top = 4
    Width = 105
    Height = 25
    Caption = 'Evaluate Expression'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 72
    Top = 4
    Width = 153
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Text = 'A + MAX(B,C)'
  end
  object EditA: TJvValidateEdit
    Left = 92
    Top = 60
    Width = 73
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '100'
    TabOrder = 2
  end
  object EditB: TJvValidateEdit
    Left = 92
    Top = 84
    Width = 73
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '50'
    TabOrder = 3
  end
  object EditC: TJvValidateEdit
    Left = 92
    Top = 108
    Width = 73
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '60'
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 72
    Top = 28
    Width = 153
    Height = 22
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 5
  end
  object JvInterpreterProgram1: TJvInterpreterProgram
    OnGetValue = JvInterpreterProgram1GetValue
    OnSetValue = JvInterpreterProgram1SetValue
    Left = 244
    Top = 60
  end
end
