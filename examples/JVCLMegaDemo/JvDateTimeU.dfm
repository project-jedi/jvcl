object JvDateTimeFrm: TJvDateTimeFrm
  Left = 358
  Top = 107
  Width = 559
  Height = 481
  Caption = 'various date time components'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 115
    Height = 16
    Caption = 'JvDateTimePicker:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label3: TLabel
    Left = 344
    Top = 8
    Width = 105
    Height = 16
    Caption = 'JvMonthCalendar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label8: TLabel
    Left = 344
    Top = 200
    Width = 112
    Height = 16
    Caption = 'JvMonthCalendar2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 69
    Height = 16
    Caption = 'JvYearEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label4: TLabel
    Left = 8
    Top = 80
    Width = 69
    Height = 16
    Caption = 'JvDateEdit:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label6: TLabel
    Left = 32
    Top = 368
    Width = 51
    Height = 16
    Caption = 'JvClock:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label7: TLabel
    Left = 16
    Top = 184
    Width = 94
    Height = 16
    Caption = 'JvAnalogClock:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label5: TLabel
    Left = 24
    Top = 304
    Width = 57
    Height = 16
    Caption = 'JvxClock:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNone
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object JvDateTimePicker1: TJvDateTimePicker
    Left = 152
    Top = 16
    Width = 105
    Height = 21
    Date = 37421.492745266200000000
    Time = 37421.492745266200000000
    TabOrder = 0
    DropDownDate = 37494.000000000000000000
    NullText = '(none)'
  end
  object JvMonthCalendar1: TJvMonthCalendar
    Left = 344
    Top = 24
    Width = 191
    Height = 154
    Date = 37421.493829884260000000
    TabOrder = 1
  end
  object JvMonthCalendar21: TJvMonthCalendar2
    Left = 344
    Top = 224
    Width = 191
    Height = 154
    ParentColor = False
    TabStop = True
    TabOrder = 2
    DateFirst = 37421.000000000000000000
    Today = 37421.516127847200000000
  end
  object JvYearEdit1: TJvValidateEdit
    Left = 152
    Top = 48
    Width = 105
    Height = 21
    Alignment = taRightJustify
    ReadOnly = False
    TabOrder = 3
    Value = 2000
    MaxValue = 9999
    MinValue = 0
    HasMaxValue = True
    HasMinValue = True
    WindowsillYear = 71
  end
  object JvDateEdit1: TJvDateEdit
    Left = 152
    Top = 80
    Width = 105
    Height = 21
    ButtonFlat = False
    NumGlyphs = 2
    TabOrder = 4
  end
  object JvClock1: TJvClock
    Left = 136
    Top = 352
    Width = 145
    ClockStyle = csDateTime
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object JvAnalogClock1: TJvAnalogClock
    Left = 136
    Top = 128
    SpiderClock = True
    TabOrder = 6
  end
  object JvxClock1: TJvxClock
    Left = 128
    Top = 296
    Width = 185
    Height = 41
  end
  object ComboBox1: TComboBox
    Left = 32
    Top = 400
    Width = 105
    Height = 21
    ItemHeight = 13
    TabOrder = 8
    Text = 'Choose a stlye'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Time'
      'TimeDate'
      'DateTime;'
      'csDate')
  end
end
