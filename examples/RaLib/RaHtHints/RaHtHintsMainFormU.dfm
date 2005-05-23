object RaHtHintsMainForm: TRaHtHintsMainForm
  Left = 380
  Top = 135
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
    Width = 169
    Height = 25
    Hint = 'Button1 <b>Bold Hint</b> not bold'
    Caption = 'Set colored hint for this button'
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
      'Color red = <b><font color=clRed>Red')
    TabOrder = 1
  end
  object RegAuto1: TJvFormStorage
    StoredValues = <>
    Left = 248
    Top = 96
  end
end
