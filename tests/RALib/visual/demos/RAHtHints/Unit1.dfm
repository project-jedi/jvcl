object Form1: TForm1
  Left = 200
  Top = 108
  Width = 360
  Height = 226
  Caption = 'HtHints test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 71
    Height = 13
    Caption = 'Hint for Button:'
  end
  object Button1: TButton
    Left = 8
    Top = 160
    Width = 75
    Height = 25
    Hint = 'Button1 <b>Bold Hint</b> not bold'
    Caption = 'Set hint'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 329
    Height = 105
    Lines.Strings = (
      'Button1 <b>Bold Hint</b> not bold'
      'Color red = <b><c:Red>Red')
    TabOrder = 1
  end
  object RegAuto1: TJvRegAuto
    RegPath = 'Software\JVCL\ColorHintsTest'
    IniFile = '$HOME/.ColorHintsTest'
    Props.Strings = (
      'Button1.Hint')
    Left = 136
    Top = 144
  end
end
