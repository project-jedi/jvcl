object FrameInstall: TFrameInstall
  Left = 0
  Top = 0
  Width = 518
  Height = 397
  TabOrder = 0
  TabStop = True
  object LblTarget: TLabel
    Left = 8
    Top = 16
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
  object LblInfo: TLabel
    Left = 8
    Top = 80
    Width = 33
    Height = 13
    Caption = 'LblInfo'
  end
  object LblOpenFile: TLabel
    Left = 8
    Top = 378
    Width = 505
    Height = 13
    Cursor = crHandPoint
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'LblOpenFile'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Visible = False
    OnClick = LblOpenFileClick
  end
  object ProgressBarTarget: TProgressBar
    Left = 8
    Top = 32
    Width = 505
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object ProgressBarCompile: TProgressBar
    Left = 8
    Top = 96
    Width = 505
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object RichEditLog: TRichEdit
    Left = 8
    Top = 128
    Width = 505
    Height = 247
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    Visible = False
    OnSelectionChange = RichEditLogSelectionChange
  end
  object BtnDetails: TButton
    Left = 432
    Top = 128
    Width = 81
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Details >>'
    TabOrder = 3
    OnClick = BtnDetailsClick
  end
end
