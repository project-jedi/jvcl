object JvAppHotKeyDemoMainForm: TJvAppHotKeyDemoMainForm
  Left = 360
  Top = 165
  Width = 352
  Height = 422
  Caption = 'JvAppHotKey demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 95
    Height = 13
    Caption = 'Application &hot key:'
  end
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 333
    Height = 32
    Caption = 
      'Define a hot key and switch to another application '#13#10'and press t' +
      'he hotkey to see the effect of THotKey!'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object HotKey1: THotKey
    Left = 8
    Top = 72
    Width = 245
    Height = 19
    Anchors = [akLeft, akTop, akRight]
    HotKey = 49217
    Modifiers = [hkCtrl, hkAlt]
    TabOrder = 0
  end
  object btnAdd: TButton
    Left = 260
    Top = 70
    Width = 75
    Height = 24
    Anchors = [akTop, akRight]
    Caption = '&Add'
    Default = True
    TabOrder = 1
    OnClick = btnAddClick
  end
  object lbHotKeys: TListBox
    Left = 8
    Top = 104
    Width = 328
    Height = 275
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
end
