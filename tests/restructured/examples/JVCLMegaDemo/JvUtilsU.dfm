object JvUtilsFrm: TJvUtilsFrm
  Left = 0
  Top = 0
  Width = 639
  Height = 452
  TabOrder = 0
  object Memo1: TMemo
    Left = 304
    Top = 16
    Width = 329
    Height = 433
    Lines.Strings = (
      'Memo1')
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
    Caption = 'turn the monitor off'
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
    Caption = 'launch a control panel unit  "desk"'
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
    SystemKeys_Enabled = True
    Left = 520
    Top = 216
  end
end
