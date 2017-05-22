object FrameUninstall: TFrameUninstall
  Left = 0
  Top = 0
  Width = 518
  Height = 397
  TabOrder = 0
  TabStop = True
  object LblTarget: TLabel
    Left = 8
    Top = 138
    Width = 55
    Height = 13
    Caption = 'LblTarget'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LblFilename: TLabel
    Left = 8
    Top = 186
    Width = 68
    Height = 13
    Caption = 'LblFilename'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 154
    Width = 505
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object ProgressBarDelete: TProgressBar
    Left = 8
    Top = 202
    Width = 505
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
end
