object frmMain: TfrmMain
  Left = 340
  Top = 169
  Width = 364
  Height = 333
  Caption = 'JvAppHotKey demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 336
    Height = 26
    Caption = 
      'Define a hot key and switch to another application.'#13#10'Press the h' +
      'otkey to see the effect of TJvApplicationHotKey.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 93
    Height = 13
    Caption = 'Application &hot key:'
  end
  object lbHotKeys: TListBox
    Left = 8
    Top = 104
    Width = 340
    Height = 186
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object HotKey1: THotKey
    Left = 8
    Top = 72
    Width = 257
    Height = 19
    Anchors = [akLeft, akTop, akRight]
    HotKey = 49217
    InvalidKeys = [hcNone, hcShift]
    Modifiers = [hkCtrl, hkAlt]
    TabOrder = 1
  end
  object btnAdd: TButton
    Left = 272
    Top = 70
    Width = 75
    Height = 24
    Anchors = [akTop, akRight]
    Caption = '&Add'
    Default = True
    TabOrder = 2
    OnClick = btnAddClick
  end
end
