object Form1: TForm1
  Left = 192
  Top = 116
  Width = 336
  Height = 429
  Caption = 'Mouse Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 208
    Top = 8
    Width = 47
    Height = 16
    Anchors = [akTop, akRight]
    Caption = 'Buttons:'
  end
  object Label2: TLabel
    Left = 208
    Top = 32
    Width = 43
    Height = 16
    Anchors = [akTop, akRight]
    Caption = 'DeltaX:'
  end
  object Label3: TLabel
    Left = 208
    Top = 56
    Width = 44
    Height = 16
    Anchors = [akTop, akRight]
    Caption = 'DeltaY:'
  end
  object Buttons: TLabel
    Left = 256
    Top = 8
    Width = 3
    Height = 16
  end
  object DeltaX: TLabel
    Left = 256
    Top = 32
    Width = 3
    Height = 16
  end
  object DeltaY: TLabel
    Left = 256
    Top = 56
    Width = 3
    Height = 16
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 201
    Height = 401
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 16
    TabOrder = 0
  end
  object HidCtl: TJvHidDeviceController
    OnDeviceChange = HidCtlDeviceChange
    Left = 296
  end
end
