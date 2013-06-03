object Form1: TForm1
  Left = 300
  Top = 121
  Width = 456
  Height = 472
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'SimpleHIDWrite'
  Color = clBtnFace
  Constraints.MinHeight = 380
  Constraints.MinWidth = 456
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 39
    Top = 256
    Width = 57
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Report ID'
  end
  object Label3: TLabel
    Left = 199
    Top = 251
    Width = 249
    Height = 16
    Anchors = [akRight, akBottom]
    AutoSize = False
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 448
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object WriteButton: TButton
    Left = 195
    Top = 416
    Width = 67
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Write'
    TabOrder = 6
    OnClick = WriteButtonClick
  end
  object ReportID: TEdit
    Left = 7
    Top = 256
    Width = 25
    Height = 24
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Text = '00'
  end
  object Edit1: TEdit
    Left = 7
    Top = 288
    Width = 25
    Height = 24
    Anchors = [akLeft, akBottom]
    TabOrder = 3
  end
  object ReadButton: TButton
    Left = 125
    Top = 416
    Width = 65
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Read'
    TabOrder = 5
    OnClick = ReadButtonClick
  end
  object InfoButton: TButton
    Left = 4
    Top = 416
    Width = 49
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Info'
    TabOrder = 4
    OnClick = InfoButtonClick
  end
  object GetFeatureButton: TButton
    Left = 275
    Top = 416
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Get Feature'
    TabOrder = 7
    OnClick = ReadButtonClick
  end
  object SetFeatureButton: TButton
    Left = 363
    Top = 416
    Width = 83
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Set Feature'
    TabOrder = 8
    OnClick = WriteButtonClick
  end
  object ListBox2: TListBox
    Left = 0
    Top = 96
    Width = 448
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 16
    TabOrder = 1
  end
  object HidCtl: TJvHidDeviceController
    OnEnumerate = HidCtlEnumerate
    OnDeviceChange = HidCtlDeviceChange
    Left = 416
    Top = 384
  end
end
