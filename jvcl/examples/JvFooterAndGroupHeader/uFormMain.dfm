object Form1: TForm1
  Left = 393
  Top = 222
  Width = 502
  Height = 222
  Caption = 'JvGroupHeader and JvFooter demo'
  Color = clBtnFace
  Constraints.MinHeight = 150
  Constraints.MinWidth = 200
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvGroupHeader1: TJvGroupHeader
    Left = 16
    Top = 8
    Width = 225
    Height = 17
    Caption = 'JvGroupHeader1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object JvGroupHeader2: TJvGroupHeader
    Left = 16
    Top = 32
    Width = 225
    Height = 17
    Caption = 'JvGroupHeader2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    BevelOptions.Brush.Color = 11106843
    BevelOptions.Height = 3
    BevelOptions.Pen.Color = 14789952
    BevelOptions.Style = bsShape
  end
  object JvFooter1: TJvFooter
    Left = 0
    Top = 155
    Width = 494
    Height = 37
    Align = alBottom
    object JvFooterBtn1: TJvFooterBtn
      Left = 5
      Top = 7
      Width = 74
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'Help'
      TabOrder = 0
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      Alignment = taLeftJustify
      ButtonIndex = 0
      SpaceInterval = 6
    end
    object JvFooterBtn2: TJvFooterBtn
      Left = 334
      Top = 7
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 1
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ButtonIndex = 1
      SpaceInterval = 6
    end
    object JvFooterBtn3: TJvFooterBtn
      Left = 415
      Top = 7
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 2
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ButtonIndex = 2
      SpaceInterval = 6
    end
  end
  object JvFooter2: TJvFooter
    Left = 0
    Top = 118
    Width = 494
    Height = 37
    Align = alBottom
    object JvFooterBtn4: TJvFooterBtn
      Left = 260
      Top = 7
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Previous'
      TabOrder = 0
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ButtonIndex = 0
      SpaceInterval = 0
    end
    object JvFooterBtn5: TJvFooterBtn
      Left = 334
      Top = 7
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Next'
      Default = True
      TabOrder = 1
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ButtonIndex = 1
      SpaceInterval = 6
    end
    object JvFooterBtn6: TJvFooterBtn
      Left = 415
      Top = 7
      Width = 74
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Close'
      TabOrder = 2
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      ButtonIndex = 2
      SpaceInterval = 6
    end
  end
  object JvFooter3: TJvFooter
    Left = 0
    Top = 81
    Width = 494
    Height = 37
    Align = alBottom
    object JvFooterBtn7: TJvFooterBtn
      Left = 210
      Top = 7
      Width = 74
      Height = 23
      Anchors = [akBottom]
      Caption = '&Close'
      TabOrder = 0
      NumGlyphs = 2
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      Alignment = taCenter
      ButtonIndex = 0
      SpaceInterval = 0
    end
  end
end
