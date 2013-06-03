object JvGraphicalFrm: TJvGraphicalFrm
  Left = 0
  Top = 0
  Width = 541
  Height = 535
  TabOrder = 0
  OnEnter = FrameEnter
  object StatusBar1: TStatusBar
    Left = 0
    Top = 512
    Width = 541
    Height = 23
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 48
    Top = 64
    Width = 441
    Height = 313
    Buttons = [capClose, capHelp]
    Caption = 'TJvMonthCalendar2 demo'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    TabOrder = 1
    object chkMulti: TCheckBox
      Left = 275
      Top = 68
      Width = 119
      Height = 21
      Caption = '&MultiSelect'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkMultiClick
    end
    object chkShow: TCheckBox
      Left = 275
      Top = 97
      Width = 119
      Height = 21
      Caption = '&ShowToday'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkShowClick
    end
    object chkCircle: TCheckBox
      Left = 275
      Top = 127
      Width = 119
      Height = 21
      Caption = '&CircleToday'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = chkCircleClick
    end
    object chkWeeks: TCheckBox
      Left = 275
      Top = 156
      Width = 119
      Height = 21
      Caption = '&WeekNumbers'
      TabOrder = 3
      OnClick = chkWeeksClick
    end
    object mc: TJvMonthCalendar2
      Left = 72
      Top = 56
      Height = 153
      ParentColor = False
      TabStop = True
      TabOrder = 4
      AutoSize = True
      DateFirst = 37400
      DateLast = 37400
      MaxSelCount = 30
      MultiSelect = True
      Today = 37400.6206314468
      OnSelect = mcSelect
      OnSelChange = mcSelChange
    end
  end
end
