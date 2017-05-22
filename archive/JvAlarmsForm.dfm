object FormAlarm: TFormAlarm
  Left = 400
  Top = 223
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'JvAlarms editor'
  ClientHeight = 296
  ClientWidth = 285
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
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object AlarmListBox: TListBox
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
    Caption = 'Data'
    Enabled = False
    TabOrder = 5
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 41
      Height = 13
      Caption = 'Keyword'
    end
    object NameEdit: TEdit
      Left = 60
      Top = 16
      Width = 101
      Height = 21
      TabOrder = 0
      OnChange = NameEditChange
    end
    object TimePick: TDateTimePicker
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
      OnChange = TimePickChange
    end
    object DatePick: TDateTimePicker
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
      OnChange = TimePickChange
    end
  end
  object AddBtn: TButton
    Left = 200
    Top = 14
    Width = 75
    Height = 25
    Caption = '&Add'
    TabOrder = 2
    OnClick = AddBtnClick
  end
  object RemoveBtn: TButton
    Left = 200
    Top = 46
    Width = 75
    Height = 25
    Caption = '&Remove'
    TabOrder = 3
    OnClick = RemoveBtnClick
  end
  object CancelBtn: TButton
    Left = 200
    Top = 256
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = CancelBtnClick
  end
  object OkBtn: TButton
    Left = 200
    Top = 220
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = OkBtnClick
  end
end
