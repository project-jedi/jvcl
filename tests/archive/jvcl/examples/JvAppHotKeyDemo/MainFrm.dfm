object frmMain: TfrmMain
  Left = 240
  Top = 156
  Width = 277
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
  DesignSize = (
    269
    395)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 95
    Height = 13
    Caption = 'Application &hot key:'
  end
  object HotKey1: THotKey
    Left = 8
    Top = 24
    Width = 170
    Height = 19
    Anchors = [akLeft, akTop, akRight]
    HotKey = 49217
    InvalidKeys = [hcNone, hcShift]
    Modifiers = [hkCtrl, hkAlt]
    TabOrder = 0
  end
  object btnAdd: TButton
    Left = 185
    Top = 24
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add'
    Default = True
    TabOrder = 1
    OnClick = btnAddClick
  end
  object lbHotKeys: TListBox
    Left = 8
    Top = 56
    Width = 253
    Height = 323
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
end
