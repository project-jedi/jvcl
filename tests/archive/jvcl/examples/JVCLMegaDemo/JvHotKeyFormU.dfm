object JvHotKeyForm: TJvHotKeyForm
  Left = 316
  Top = 138
  Width = 600
  Height = 496
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
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 42
    Top = 21
    Width = 407
    Height = 412
    Buttons = [capClose, capHelp]
    Caption = 'HotKey components demo'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 0
    object Label1: TLabel
      Left = 40
      Top = 64
      Width = 95
      Height = 13
      Caption = 'Application &hot key:'
    end
    object Label2: TLabel
      Left = 40
      Top = 24
      Width = 333
      Height = 41
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
      Left = 40
      Top = 80
      Width = 241
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      HotKey = 49217
      Modifiers = [hkCtrl, hkAlt]
      TabOrder = 0
    end
    object btnAdd: TButton
      Left = 298
      Top = 77
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Add'
      Default = True
      TabOrder = 1
      OnClick = btnAddClick
    end
    object lbHotKeys: TListBox
      Left = 40
      Top = 112
      Width = 345
      Height = 281
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 2
    end
  end
end
