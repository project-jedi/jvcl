object frmMain: TfrmMain
  Left = 240
  Top = 190
  Width = 427
  Height = 257
  Caption = 'Demo for the JvZoom component'
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
    Left = 288
    Top = 72
    Width = 103
    Height = 13
    Caption = 'Adjust the zoom level:'
  end
  object JvZoom1: TJvZoom
    Left = 18
    Top = 18
    Width = 233
    Height = 189
    Active = False
  end
  object CheckBox1: TCheckBox
    Left = 280
    Top = 20
    Width = 97
    Height = 17
    Caption = 'Active'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object Button1: TButton
    Left = 322
    Top = 98
    Width = 75
    Height = 25
    Caption = '+'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 321
    Top = 130
    Width = 75
    Height = 25
    Caption = '-'
    TabOrder = 3
    OnClick = Button2Click
  end
end
