object MonthCalendarMainForm: TMonthCalendarMainForm
  Left = 322
  Top = 253
  Width = 413
  Height = 227
  Caption = 'TJvMonthCalendar2 demo'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 177
    Width = 405
    Height = 23
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object chkMulti: TCheckBox
    Left = 211
    Top = 20
    Width = 119
    Height = 21
    Caption = '&MultiSelect'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = chkMultiClick
  end
  object chkShow: TCheckBox
    Left = 211
    Top = 49
    Width = 119
    Height = 21
    Caption = '&ShowToday'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chkShowClick
  end
  object chkCircle: TCheckBox
    Left = 211
    Top = 79
    Width = 119
    Height = 21
    Caption = '&CircleToday'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chkCircleClick
  end
  object chkWeeks: TCheckBox
    Left = 211
    Top = 108
    Width = 119
    Height = 21
    Caption = '&WeekNumbers'
    TabOrder = 4
    OnClick = chkWeeksClick
  end
end
