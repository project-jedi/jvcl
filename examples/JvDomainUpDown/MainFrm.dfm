object Form1: TForm1
  Left = 252
  Top = 133
  BorderStyle = bsToolWindow
  Caption = 'JvDomainUpDown Demo'
  ClientHeight = 125
  ClientWidth = 459
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 60
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 9
    Top = 18
    Width = 44
    Height = 13
    Caption = 'Domains:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 35
    Width = 422
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'www.borland.com'
    OnChange = Edit1Change
  end
  object JvDomainUpDown1: TJvDomainUpDown
    Left = 434
    Top = 35
    Width = 15
    Height = 21
    Associate = Edit1
    Items.Strings = (
      'www.borland.com'
      'www.bdn.com'
      'www.delphi-jedi.org'
      'jvcl.sf.net'
      'jcl.sf.net')
    Position = 0
    Text = 'www.borland.com'
    Anchors = [akTop, akRight]
    TabOrder = 0
    Wrap = True
  end
  object Button1: TButton
    Left = 363
    Top = 84
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Go there'
    TabOrder = 2
    OnClick = Button1Click
  end
end
