object FormAlarm: TFormAlarm
  Left = 438
  Top = 382
  BorderStyle = bsDialog
  Caption = 'Jv - Alarms editor'
  ClientHeight = 296
  ClientWidth = 285
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 112
    Width = 173
    Height = 175
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 4
    OnClick = ListBox1DblClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 6
    Width = 173
    Height = 97
    Caption = '[ Datas ]'
    Enabled = False
    TabOrder = 5
    object Label1: TLabel
      Left = 12
      Top = 20
      Width = 41
      Height = 13
      Caption = 'Keyword'
    end
    object Edit1: TEdit
      Left = 60
      Top = 16
      Width = 101
      Height = 21
      TabOrder = 0
      OnChange = Edit1Change
    end
    object DateTimePicker2: TDateTimePicker
      Left = 8
      Top = 42
      Width = 153
      Height = 21
      CalAlignment = dtaLeft
      Date = 36230.685934838
      Time = 36230.685934838
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkTime
      ParseInput = False
      TabOrder = 1
      OnChange = DateTimePicker2Change
    end
    object DateTimePicker1: TDateTimePicker
      Left = 8
      Top = 66
      Width = 153
      Height = 21
      CalAlignment = dtaLeft
      Date = 36230.685934838
      Time = 36230.685934838
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkDate
      MaxDate = 2958464.91192396
      ParseInput = False
      TabOrder = 2
      OnChange = DateTimePicker2Change
    end
  end
  object BUButton1: TJvButton
    Left = 200
    Top = 14
    Width = 75
    Height = 25
    Caption = '&Add'
    TabOrder = 2
    OnClick = BUButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton2: TJvButton
    Left = 200
    Top = 46
    Width = 75
    Height = 25
    Caption = '&Remove'
    TabOrder = 3
    OnClick = BUButton2Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton3: TJvButton
    Left = 200
    Top = 256
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = BUButton3Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton4: TJvButton
    Left = 200
    Top = 220
    Width = 75
    Height = 25
    Caption = '&Ok'
    Default = True
    TabOrder = 0
    OnClick = BUButton4Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
end
