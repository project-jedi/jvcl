object Form1: TForm1
  Left = 336
  Top = 303
  Width = 426
  Height = 272
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 224
    Top = 136
    Width = 124
    Height = 13
    Caption = 'Choose button (hook only)'
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 401
    Height = 113
    BorderStyle = bsNone
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 128
    Width = 185
    Height = 105
    Caption = 'Detection'
    ItemIndex = 0
    Items.Strings = (
      'Form only'
      'All controls via AppEvents')
    TabOrder = 1
    OnClick = RadioGroup1Click
  end
  object ComboBox1: TComboBox
    Left = 224
    Top = 152
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'Right mouse button'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Left mouse button'
      'Middle mouse button'
      'Right mouse button')
  end
  object JvMouseGesture1: TJvMouseGesture
    TrailLimit = 1000
    TrailInterval = 2
    Grid = 15
    Delay = 500
    Active = False
    Left = 16
    Top = 88
  end
  object JvMouseGestureHook1: TJvMouseGestureHook
    Active = False
    ActivationMode = JvOnAppStart
    OnJvMouseGestureCustomInterpretation = JvMouseGestureHook1JvMouseGestureCustomInterpretation
    Left = 56
    Top = 88
  end
end
