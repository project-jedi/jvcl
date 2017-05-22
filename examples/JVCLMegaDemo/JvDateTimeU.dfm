object JvDateTimeFrm: TJvDateTimeFrm
  Left = 358
  Top = 107
  BorderStyle = bsDialog
  Caption = 'various date time components'
  ClientHeight = 444
  ClientWidth = 607
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
    Left = 400
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
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 127
    Height = 16
    Caption = 'JvValidateEdit (Year)'
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
    Top = 224
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
  object Label5: TLabel
    Left = 30
    Top = 148
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
    Left = 400
    Top = 24
    Width = 191
    Height = 154
    Date = 37421.079300000000000000
    TabOrder = 1
  end
  object JvYearEdit1: TJvValidateEdit
    Left = 152
    Top = 48
    Width = 105
    Height = 21
    CheckChars = '0123456789'
    CriticalPoints.CheckPoints = cpNone
    CriticalPoints.ColorAbove = clBlue
    CriticalPoints.ColorBelow = clRed
    DisplayFormat = dfYear
    EditText = '2003'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 4
    MaxValue = 9999.000000000000000000
    PasswordChar = #0
    ReadOnly = False
    TabOrder = 2
    Text = '2003'
    Value = 2003
  end
  object JvDateEdit1: TJvDateEdit
    Left = 152
    Top = 80
    Width = 105
    Height = 21
    ButtonFlat = False
    NumGlyphs = 2
    TabOrder = 3
  end
  object JvxClock1: TJvClock
    Left = 122
    Top = 134
    Width = 220
    Height = 41
    BevelInner = bvRaised
    BevelOuter = bvLowered
  end
  object JvClock1: TJvClock
    Left = 122
    Top = 218
    Width = 220
    Height = 215
    BevelInner = bvNone
    BevelOuter = bvLowered
    ShowMode = scAnalog
  end
end
