object JvLabelsFrm: TJvLabelsFrm
  Left = 0
  Top = 0
  Width = 621
  Height = 464
  HelpContext = 6
  TabOrder = 0
  object JvHotLink1: TJvHotLink
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
    Url = 'http://delphi-jedi.org'
  end
  object JvBlinkingLabel1: TJvBlinkingLabel
    Left = 120
    Top = 72
    Width = 162
    Height = 13
    Caption = 'I am a JvBlinkingLabel     Irritated?'
    Visible = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object JvScrollingLabel1: TJvScrollingLabel
    Left = 120
    Top = 96
    Width = 201
    Height = 13
    AutoSize = False
    ScrollInterval = 100
    Text = 
      'I am a JvScrollongLable .... visit http://delphi-jedi.org  with ' +
      'the first label on this TabSheet'
    ScrollDirection = sdRightToLeft
  end
  object JvRealLabel1: TJvRealLabel
    Left = 120
    Top = 184
    Width = 3
    Height = 13
  end
  object JvSpecialLabel1: TJvSpecialLabel
    Left = 120
    Top = 216
    Width = 3
    Height = 13
  end
  object JvAngleLabel1: TJvAngleLabel
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
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Angle = 25
  end
  object JvBouncingLabel1: TJvBouncingLabel
    Left = 120
    Top = 218
    Width = 245
    Height = 13
    Caption = 'I am a JvBouncingLabel. Hit me to do the bouncing.'
    OnClick = JvBouncingLabel1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Bouncing = False
  end
  object JvAppearingLabel1: TJvAppearingLabel
    Left = 120
    Top = 248
    Width = 263
    Height = 13
    Caption = 'I am JvAppearingLabel click me to view my appearance'
    OnClick = JvAppearingLabel1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    OnAppeared = JvAppearingLabel1Appeared
  end
  object Label11: TLabel
    Left = 120
    Top = 160
    Width = 128
    Height = 13
    Caption = 'This is a JvReversedLabel:'
  end
  object JvReversedLabel1: TJvReversedLabel
    Left = 168
    Top = 184
    Width = 89
    Height = 13
    Caption = 'JvReversedLabel1'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Label12: TLabel
    Left = 32
    Top = 424
    Width = 398
    Height = 13
    Caption = 
      'By the way: All Labels of the JVCL have OnMouseEnter and OnMouse' +
      'Leave Events'
  end
end
