object Form3: TForm3
  Left = 230
  Top = 104
  Width = 274
  Height = 270
  Caption = 'Form3'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 172
    Height = 13
    Caption = 'This form has been called from Unit2'
  end
  object Label2: TLabel
    Left = 24
    Top = 48
    Width = 228
    Height = 13
    Caption = 'Grid is linked with TTable from datamodule Unit4'
  end
  object Button1: TButton
    Left = 96
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 16
    Top = 72
    Width = 233
    Height = 113
    DataSource = DataModule1.DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
end
