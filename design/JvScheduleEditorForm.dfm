object FrmScheduleEditor: TFrmScheduleEditor
  Left = 257
  Top = 121
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Schedule Editor'
  ClientHeight = 407
  ClientWidth = 765
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100001001000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000BBBB0000000000BB000BB000000000BB0000B000000000B
    BB000BB00000000BBB000BB00000000000000BB00000000000000BB000000000
    00000BB00000000000000BB00000000000000BB00000000000000BB000000000
    00000BB0000000000000BBBB00000000000BBBBBB0000000000000000000FFFF
    0000F87F0000E73F0000E7BF0000E39F0000E39F0000FF9F0000FF9F0000FF9F
    0000FF9F0000FF9F0000FF9F0000FF9F0000FF0F0000FE070000FFFF0000}
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bvlDailyFreq: TBevel
    Left = 0
    Top = 170
    Width = 505
    Height = 2
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object pnlStartInfo: TPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 50
    BevelOuter = bvNone
    TabOrder = 0
    object lblStartDate: TLabel
      Left = 15
      Top = 22
      Width = 26
      Height = 13
      Caption = 'Date:'
    end
    object lblStartTime: TLabel
      Left = 155
      Top = 22
      Width = 26
      Height = 13
      Caption = 'Time:'
    end
    object lblStartCaption: TLabel
      Left = 0
      Top = 0
      Width = 28
      Height = 13
      Caption = 'Start'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object bvlScheduleType: TBevel
      Left = 0
      Top = 48
      Width = 505
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object dtpStartDate: TDateTimePicker
      Left = 50
      Top = 20
      Width = 96
      Height = 21
      CalAlignment = dtaLeft
      Date = 37437.430495081
      Time = 37437.430495081
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = False
      TabOrder = 0
    end
    object dtpStartTime: TDateTimePicker
      Left = 190
      Top = 20
      Width = 76
      Height = 21
      CalAlignment = dtaLeft
      Date = 37437
      Time = 37437
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkTime
      ParseInput = False
      TabOrder = 1
    end
  end
  object pnlEndInfo: TPanel
    Left = 0
    Top = 250
    Width = 505
    Height = 125
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 7
    object lblEndCaption: TLabel
      Left = 0
      Top = 0
      Width = 23
      Height = 13
      Caption = 'End'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblEndTime: TLabel
      Left = 205
      Top = 100
      Width = 26
      Height = 13
      Caption = 'Time:'
    end
    object Bevel2: TBevel
      Left = 0
      Top = 123
      Width = 505
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object rbInfinite: TRadioButton
      Left = 15
      Top = 20
      Width = 76
      Height = 17
      Caption = 'Never'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbTriggerCount: TRadioButton
      Left = 15
      Top = 45
      Width = 81
      Height = 17
      Caption = 'Event count'
      TabOrder = 1
    end
    object rbDayCount: TRadioButton
      Left = 15
      Top = 70
      Width = 81
      Height = 17
      Caption = 'Day count'
      TabOrder = 3
    end
    object rbDate: TRadioButton
      Left = 15
      Top = 95
      Width = 71
      Height = 17
      Caption = 'Date:'
      TabOrder = 5
    end
    object dtpEndDate: TDateTimePicker
      Left = 100
      Top = 95
      Width = 96
      Height = 21
      CalAlignment = dtaLeft
      Date = 37437.430495081
      Time = 37437.430495081
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = False
      TabOrder = 6
    end
    object dtpEndTime: TDateTimePicker
      Left = 235
      Top = 95
      Width = 76
      Height = 21
      CalAlignment = dtaLeft
      Date = 37437.9999884259
      Time = 37437.9999884259
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkTime
      ParseInput = False
      TabOrder = 7
    end
    object edEventCount: TEdit
      Left = 100
      Top = 45
      Width = 96
      Height = 21
      TabOrder = 2
      Text = '1'
    end
    object edDayCount: TEdit
      Left = 100
      Top = 70
      Width = 96
      Height = 21
      TabOrder = 4
      Text = '1'
    end
  end
  object pnlRecurringInfo: TPanel
    Left = 0
    Top = 50
    Width = 96
    Height = 120
    BevelOuter = bvNone
    TabOrder = 1
    object lblScheduleType: TLabel
      Left = 0
      Top = 0
      Width = 82
      Height = 13
      Caption = 'Schedule type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object bvlSeparation: TBevel
      Left = 91
      Top = 0
      Width = 5
      Height = 120
      Align = alRight
      Shape = bsLeftLine
    end
    object rbSingleShot: TRadioButton
      Left = 15
      Top = 20
      Width = 66
      Height = 17
      Caption = 'One-shot'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbDaily: TRadioButton
      Left = 15
      Top = 40
      Width = 66
      Height = 17
      Caption = 'Daily'
      TabOrder = 1
    end
    object rbWeekly: TRadioButton
      Left = 15
      Top = 60
      Width = 66
      Height = 17
      Caption = 'Weekly'
      TabOrder = 2
    end
    object rbMonthly: TRadioButton
      Left = 15
      Top = 80
      Width = 66
      Height = 17
      Caption = 'Monthly'
      TabOrder = 3
    end
    object rbYearly: TRadioButton
      Left = 15
      Top = 100
      Width = 66
      Height = 17
      Caption = 'Yearly'
      TabOrder = 4
    end
  end
  object pnlDailyFreq: TPanel
    Left = 0
    Top = 175
    Width = 505
    Height = 75
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 6
    object lblDailyFreq: TLabel
      Left = 0
      Top = 0
      Width = 89
      Height = 13
      Caption = 'Daily frequency'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblFreqFrom: TLabel
      Left = 245
      Top = 48
      Width = 41
      Height = 13
      Caption = 'between'
    end
    object lblFreqTo: TLabel
      Left = 370
      Top = 48
      Width = 18
      Height = 13
      Caption = 'and'
    end
    object Bevel1: TBevel
      Left = 0
      Top = 73
      Width = 505
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object rbFreqOneshot: TRadioButton
      Left = 15
      Top = 20
      Width = 96
      Height = 17
      Caption = 'Occurs once at'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object dtpDayFreqOneshot: TDateTimePicker
      Left = 115
      Top = 20
      Width = 76
      Height = 21
      CalAlignment = dtaLeft
      Date = 37437
      Time = 37437
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkTime
      ParseInput = False
      TabOrder = 1
    end
    object rbFreqInterval: TRadioButton
      Left = 15
      Top = 45
      Width = 96
      Height = 17
      Caption = 'Occurs every'
      TabOrder = 2
    end
    object edFreqInterval: TEdit
      Left = 115
      Top = 45
      Width = 36
      Height = 21
      TabOrder = 3
      Text = '1'
    end
    object cbFreqIntervalUnit: TComboBox
      Left = 155
      Top = 45
      Width = 86
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
      Items.Strings = (
        'Milliseconds'
        'Seconds'
        'Minutes'
        'Hours')
    end
    object dtpFreqFrom: TDateTimePicker
      Left = 290
      Top = 45
      Width = 76
      Height = 21
      CalAlignment = dtaLeft
      Date = 37437
      Time = 37437
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkTime
      ParseInput = False
      TabOrder = 5
    end
    object dtpFreqTo: TDateTimePicker
      Left = 390
      Top = 45
      Width = 76
      Height = 21
      CalAlignment = dtaLeft
      Date = 37437.9999884259
      Time = 37437.9999884259
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkTime
      ParseInput = False
      TabOrder = 6
    end
  end
  object pnlWeeklySchedule: TPanel
    Left = 95
    Top = 50
    Width = 410
    Height = 120
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    object lblWeeklyCaption: TLabel
      Left = 0
      Top = 0
      Width = 43
      Height = 13
      Caption = 'Weekly'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblWeeklyInterval: TLabel
      Left = 15
      Top = 22
      Width = 27
      Height = 13
      Caption = 'Every'
    end
    object lblWeeklyInterval2: TLabel
      Left = 90
      Top = 22
      Width = 55
      Height = 13
      Caption = 'week(s) on:'
    end
    object edWeeklyInterval: TEdit
      Left = 50
      Top = 20
      Width = 36
      Height = 21
      TabOrder = 0
      Text = '1'
    end
    object cbWeeklyMon: TCheckBox
      Left = 15
      Top = 45
      Width = 81
      Height = 17
      Caption = 'Monday'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object cbWeeklyTue: TCheckBox
      Left = 100
      Top = 45
      Width = 81
      Height = 17
      Caption = 'Tuesday'
      TabOrder = 2
    end
    object cbWeeklyWed: TCheckBox
      Left = 190
      Top = 45
      Width = 81
      Height = 17
      Caption = 'Wednesday'
      TabOrder = 3
    end
    object cbWeeklyThu: TCheckBox
      Left = 280
      Top = 45
      Width = 81
      Height = 17
      Caption = 'Thursday'
      TabOrder = 4
    end
    object cbWeeklyFri: TCheckBox
      Left = 15
      Top = 65
      Width = 81
      Height = 17
      Caption = 'Friday'
      TabOrder = 5
    end
    object cbWeeklySat: TCheckBox
      Left = 100
      Top = 65
      Width = 81
      Height = 17
      Caption = 'Saturday'
      TabOrder = 6
    end
    object cbWeeklySun: TCheckBox
      Left = 190
      Top = 65
      Width = 81
      Height = 17
      Caption = 'Sunday'
      TabOrder = 7
    end
  end
  object gbTestSettings: TGroupBox
    Left = 510
    Top = 0
    Width = 255
    Height = 407
    Align = alRight
    Caption = ' Test Settings '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    object cxStartToday: TCheckBox
      Left = 15
      Top = 20
      Width = 136
      Height = 17
      Caption = 'Start with current date'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 0
    end
    object cxCountMissedEvents: TCheckBox
      Left = 35
      Top = 40
      Width = 121
      Height = 17
      Caption = 'Count missed events'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 1
    end
    object btnTest: TButton
      Left = 16
      Top = 60
      Width = 75
      Height = 25
      Caption = 'Run'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnTestClick
    end
    object mmLog: TMemo
      Left = 15
      Top = 90
      Width = 225
      Height = 301
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 3
    end
  end
  object pnlYearlySchedule: TPanel
    Left = 95
    Top = 50
    Width = 410
    Height = 120
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 5
    Visible = False
    object lblYearlyCaption: TLabel
      Left = 0
      Top = 0
      Width = 36
      Height = 13
      Caption = 'Yearly'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblYearlyIntervalSuffix: TLabel
      Left = 288
      Top = 22
      Width = 31
      Height = 13
      Caption = 'year(s)'
    end
    object lblYearlyDateOf: TLabel
      Left = 98
      Top = 22
      Width = 9
      Height = 13
      Caption = 'of'
    end
    object lblYearlyIndexInterval: TLabel
      Left = 293
      Top = 47
      Width = 38
      Height = 13
      Caption = 'of every'
    end
    object lblYearlyIndexIntervalSuffix: TLabel
      Left = 373
      Top = 47
      Width = 31
      Height = 13
      Caption = 'year(s)'
    end
    object lblYearlyDateInterval: TLabel
      Left = 203
      Top = 22
      Width = 38
      Height = 13
      Caption = 'of every'
    end
    object lblYearlyIndexMonth: TLabel
      Left = 203
      Top = 47
      Width = 9
      Height = 13
      Caption = 'of'
    end
    object rbYearlyDate: TRadioButton
      Left = 15
      Top = 20
      Width = 41
      Height = 17
      Caption = 'On'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object edYearlyDateInterval: TEdit
      Left = 245
      Top = 20
      Width = 36
      Height = 21
      TabOrder = 3
      Text = '1'
    end
    object rbYearlyIndex: TRadioButton
      Left = 15
      Top = 45
      Width = 41
      Height = 17
      Caption = 'The'
      TabOrder = 4
    end
    object cbYearlyIndexValue: TComboBox
      Left = 60
      Top = 45
      Width = 51
      Height = 21
      ItemHeight = 13
      TabOrder = 5
      Text = '1st'
      Items.Strings = (
        '1st'
        '2nd'
        '3rd'
        '4th'
        'Last')
    end
    object cbYearlyIndexKind: TComboBox
      Left = 115
      Top = 45
      Width = 86
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
      Items.Strings = (
        'day'
        'weekday'
        'weekendday'
        'monday'
        'tuesday'
        'wednesday'
        'thursday'
        'friday'
        'saturday'
        'sunday')
    end
    object edYearlyDateDay: TEdit
      Left = 60
      Top = 20
      Width = 36
      Height = 21
      TabOrder = 1
      Text = '1'
    end
    object edYearlyIndexInterval: TEdit
      Left = 335
      Top = 45
      Width = 36
      Height = 21
      TabOrder = 8
      Text = '1'
    end
    object cbYearlyDateMonth: TComboBox
      Left = 115
      Top = 20
      Width = 86
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'januari'
        'february'
        'march'
        'april'
        'may'
        'june'
        'july'
        'august'
        'september'
        'october'
        'november'
        'december')
    end
    object cbYearlyIndexMonth: TComboBox
      Left = 215
      Top = 45
      Width = 76
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 7
      Items.Strings = (
        'januari'
        'february'
        'march'
        'april'
        'may'
        'june'
        'july'
        'august'
        'september'
        'october'
        'november'
        'december')
    end
  end
  object btnOk: TButton
    Left = 350
    Top = 380
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 9
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 430
    Top = 380
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object pnlDailySchedule: TPanel
    Left = 95
    Top = 50
    Width = 410
    Height = 120
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object lblDailyCaption: TLabel
      Left = 0
      Top = 0
      Width = 29
      Height = 13
      Caption = 'Daily'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblDailyIntervalUnit: TLabel
      Left = 110
      Top = 47
      Width = 28
      Height = 13
      Caption = 'day(s)'
    end
    object rbDailyEveryWeekDay: TRadioButton
      Left = 15
      Top = 20
      Width = 113
      Height = 17
      Caption = 'Every weekday'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbDailyInterval: TRadioButton
      Left = 15
      Top = 45
      Width = 56
      Height = 17
      Caption = 'Every'
      TabOrder = 1
    end
    object edDailyInterval: TEdit
      Left = 70
      Top = 45
      Width = 36
      Height = 21
      TabOrder = 2
      Text = '1'
    end
  end
  object pnlMonthlySchedule: TPanel
    Left = 95
    Top = 50
    Width = 410
    Height = 120
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object lblMonthlyCaption: TLabel
      Left = 0
      Top = 0
      Width = 45
      Height = 13
      Caption = 'Monthly'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblMonthlyDayIntervalSuffix: TLabel
      Left = 178
      Top = 22
      Width = 40
      Height = 13
      Caption = 'month(s)'
    end
    object lblMontlhyDayInterval: TLabel
      Left = 98
      Top = 22
      Width = 38
      Height = 13
      Caption = 'of every'
    end
    object lblMonthlyIndexInterval: TLabel
      Left = 208
      Top = 47
      Width = 38
      Height = 13
      Caption = 'of every'
    end
    object lblMonthlyIndexIntervalSuffix: TLabel
      Left = 288
      Top = 47
      Width = 40
      Height = 13
      Caption = 'month(s)'
    end
    object rbMonthlyDay: TRadioButton
      Left = 15
      Top = 20
      Width = 41
      Height = 17
      Caption = 'Day'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object edMonthlyEveryMonth: TEdit
      Left = 140
      Top = 20
      Width = 36
      Height = 21
      TabOrder = 2
      Text = '1'
    end
    object rbMonthlyEveryIndex: TRadioButton
      Left = 15
      Top = 45
      Width = 41
      Height = 17
      Caption = 'The'
      TabOrder = 3
    end
    object cbMonthlyIndexValue: TComboBox
      Left = 60
      Top = 45
      Width = 51
      Height = 21
      ItemHeight = 13
      TabOrder = 4
      Text = '1st'
      Items.Strings = (
        '1st'
        '2nd'
        '3rd'
        '4th'
        'Last')
    end
    object cbMonthlyIndexType: TComboBox
      Left = 115
      Top = 45
      Width = 91
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      Items.Strings = (
        'day'
        'weekday'
        'weekendday'
        'monday'
        'tuesday'
        'wednesday'
        'thursday'
        'friday'
        'saturday'
        'sunday')
    end
    object edMonthlyDay: TEdit
      Left = 60
      Top = 20
      Width = 36
      Height = 21
      TabOrder = 1
      Text = '1'
    end
    object edMonthlyIndexInterval: TEdit
      Left = 250
      Top = 45
      Width = 36
      Height = 21
      TabOrder = 6
      Text = '1'
    end
  end
  object AppEvents: TApplicationEvents
    OnIdle = AppEventsIdle
    Left = 620
    Top = 255
  end
end
