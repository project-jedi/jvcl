object JvLabelsFrm: TJvLabelsFrm
  Left = 364
  Top = 51
  Width = 621
  Height = 464
  HelpContext = 6
  Caption = 'the labels from the '#39'Jv Labels'#39' tab'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object JvHotLink1: TJvLabel
    Left = 120
    Top = 48
    Width = 413
    Height = 13
    Cursor = crHandPoint
    Caption = 
      'I am a JvHotLink and take you to http://delphi-jedi.org   Where ' +
      'else do you want to go?'
    HotTrack = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clBlue
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = [fsUnderline]
    URL = 'http://delphi-jedi.org'
    AutoOpenURL = False
  end
  object JvBlinkingLabel1: TJvBehaviorLabel
    Left = 120
    Top = 72
    Width = 162
    Height = 13
    Behavior = 'Blinking'
    Caption = 'I am a JvBlinkingLabel     Irritated?'
    Visible = False
  end
  object JvScrollingLabel1: TJvBehaviorLabel
    Left = 120
    Top = 96
    Width = 201
    Height = 13
    Behavior = 'Scrolling'
    BehaviorOptions.Interval = 100
    BehaviorOptions.Direction = sdRightToLeft
    AutoSize = False
    Caption = 
      'I am a JvScrollongLable .... visit http://delphi-jedi.org  with ' +
      'the first label on this TabSheet'
  end
  object JvRealLabel1: TJvBehaviorLabel
    Left = 120
    Top = 184
    Width = 3
    Height = 13
    Behavior = 'Typing'
  end
  object JvSpecialLabel1: TJvBehaviorLabel
    Left = 120
    Top = 216
    Width = 3
    Height = 13
    Behavior = 'Special'
  end
  object JvAngleLabel1: TJvLabel
    Left = 128
    Top = 320
    Width = 125
    Height = 74
    Caption = 'I am a JvAngleLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Angle = 25
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    AutoOpenURL = False
  end
  object JvBouncingLabel1: TJvBehaviorLabel
    Left = 120
    Top = 218
    Width = 245
    Height = 13
    Behavior = 'Bouncing'
    Caption = 'I am a JvBouncingLabel. Hit me to do the bouncing.'
    OnClick = JvBouncingLabel1Click
  end
  object JvAppearingLabel1: TJvBehaviorLabel
    Left = 120
    Top = 248
    Width = 263
    Height = 13
    Behavior = 'Appearing'
    Caption = 'I am JvAppearingLabel click me to view my appearance'
    OnClick = JvAppearingLabel1Click
  end
  object Label11: TLabel
    Left = 120
    Top = 160
    Width = 128
    Height = 13
    Caption = 'This is a JvReversedLabel:'
  end
  object JvReversedLabel1: TJvLabel
    Left = 168
    Top = 176
    Width = 96
    Height = 16
    Caption = 'JvReversedLabel1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Angle = 180
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    AutoOpenURL = False
  end
  object Label12: TLabel
    Left = 32
    Top = 424
    Width = 346
    Height = 13
    Caption = 
      'BTW: all labels of the JVCL have OnMouseEnter/OnMouseLeave event' +
      's'
  end
end
