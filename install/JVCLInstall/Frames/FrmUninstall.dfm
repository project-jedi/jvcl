object FrameUninstall: TFrameUninstall
  Left = 0
  Top = 0
  Width = 518
  Height = 348
  TabOrder = 0
  TabStop = True
  object LblTarget: TLabel
    Left = 8
    Top = 120
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
    Top = 168
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
    Top = 136
    Width = 505
    Height = 17
    TabOrder = 0
  end
  object ProgressBarDelete: TProgressBar
    Left = 8
    Top = 184
    Width = 505
    Height = 17
    TabOrder = 1
  end
end
