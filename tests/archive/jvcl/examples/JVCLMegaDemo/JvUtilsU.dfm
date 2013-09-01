object JvUtilsFrm: TJvUtilsFrm
  Left = 283
  Top = 87
  Width = 654
  Height = 485
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 304
    Top = 16
    Width = 329
    Height = 433
    TabOrder = 0
  end
  object Button1: TButton
    Left = 7
    Top = 153
    Width = 280
    Height = 25
    Caption = 'show me some system dirs there ->'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 7
    Top = 187
    Width = 280
    Height = 25
    Caption = 'turn the monitor off (click to get it back)'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 7
    Top = 221
    Width = 280
    Height = 25
    Caption = 'add this exe file to the recent docs '
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 7
    Top = 256
    Width = 280
    Height = 25
    Caption = 'launch the control panel unit  "desk"'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 7
    Top = 290
    Width = 280
    Height = 25
    Caption = 'let the keyboard lights flash'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 7
    Top = 325
    Width = 280
    Height = 25
    Caption = 'get the keyboard state'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button10: TButton
    Left = 7
    Top = 359
    Width = 280
    Height = 25
    Caption = 'Check if there is a disk in drive A'
    TabOrder = 7
    OnClick = Button10Click
  end
  object CheckBox2: TCheckBox
    Left = 7
    Top = 423
    Width = 150
    Height = 17
    Alignment = taLeftJustify
    Caption = 'hide Tray'
    TabOrder = 8
    OnClick = CheckBox2Click
  end
  object JvRecentMenuBtn1: TJvRecentMenuBtn
    Left = 7
    Top = 84
    Width = 280
    Height = 25
    Caption = 'get all recent docs with JvRecentMenuBtn'
    TabOrder = 9
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvStartMenuBtn1: TJvStartMenuBtn
    Left = 7
    Top = 49
    Width = 280
    Height = 25
    Caption = 'JvStartMenuBtn: I am a startmenu replacement'
    TabOrder = 10
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvFavoritesButton1: TJvFavoritesButton
    Left = 7
    Top = 15
    Width = 280
    Height = 25
    Caption = 'JvFavoritesButton: Click me to see all your favorites'
    TabOrder = 11
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvControlPanel1: TJvControlPanel
    Left = 7
    Top = 118
    Width = 280
    Height = 25
    Caption = 'all ControlPanel items (powered by JvControlPanel)'
    TabOrder = 12
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object CheckBox1: TCheckBox
    Left = 7
    Top = 407
    Width = 150
    Height = 17
    Alignment = taLeftJustify
    Caption = 'hide Startmenu'
    TabOrder = 13
    OnClick = CheckBox1Click
  end
  object JvDirectories1: TJvDirectories
    Left = 520
    Top = 160
  end
  object JvKeyboardStates1: TJvKeyboardStates
    Animation = anLeftRight
    NumLock = True
    ScrollLock = False
    CapsLock = False
    SystemKeysEnabled = True
    Left = 520
    Top = 216
  end
end
