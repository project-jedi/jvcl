object FormCalc: TFormCalc
  Left = 385
  Top = 326
  BorderStyle = bsToolWindow
  Caption = 'Calculator'
  ClientHeight = 119
  ClientWidth = 149
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BUSpeedButton1: TJvSpeedButton
    Left = 2
    Top = 28
    Width = 23
    Height = 22
    Caption = '1'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton2: TJvSpeedButton
    Left = 2
    Top = 50
    Width = 23
    Height = 22
    Caption = '4'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton3: TJvSpeedButton
    Left = 26
    Top = 50
    Width = 23
    Height = 22
    Caption = '5'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton4: TJvSpeedButton
    Left = 2
    Top = 72
    Width = 23
    Height = 22
    Caption = '7'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton5: TJvSpeedButton
    Left = 2
    Top = 94
    Width = 23
    Height = 22
    Caption = '0'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton6: TJvSpeedButton
    Left = 50
    Top = 50
    Width = 23
    Height = 22
    Caption = '6'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton7: TJvSpeedButton
    Left = 50
    Top = 72
    Width = 23
    Height = 22
    Caption = '9'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton8: TJvSpeedButton
    Left = 26
    Top = 28
    Width = 23
    Height = 22
    Caption = '2'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton9: TJvSpeedButton
    Left = 26
    Top = 72
    Width = 23
    Height = 22
    Caption = '8'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton10: TJvSpeedButton
    Left = 50
    Top = 28
    Width = 23
    Height = 22
    Caption = '3'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton11: TJvSpeedButton
    Left = 76
    Top = 28
    Width = 23
    Height = 22
    Hint = 'Clear ALL'
    Caption = 'AC'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = BUSpeedButton11Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton12: TJvSpeedButton
    Left = 50
    Top = 94
    Width = 23
    Height = 22
    Caption = '.'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton12Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton13: TJvSpeedButton
    Left = 100
    Top = 50
    Width = 23
    Height = 22
    Caption = '*'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton13Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton14: TJvSpeedButton
    Left = 76
    Top = 50
    Width = 23
    Height = 22
    Caption = '+'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton14Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton15: TJvSpeedButton
    Left = 100
    Top = 72
    Width = 23
    Height = 22
    Caption = '/'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton15Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton16: TJvSpeedButton
    Left = 76
    Top = 72
    Width = 23
    Height = 22
    Caption = '-'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton16Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton17: TJvSpeedButton
    Left = 124
    Top = 72
    Width = 23
    Height = 22
    Caption = '='
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton17Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton18: TJvSpeedButton
    Left = 26
    Top = 94
    Width = 23
    Height = 22
    Caption = '00'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton19: TJvSpeedButton
    Left = 124
    Top = 28
    Width = 23
    Height = 22
    Caption = 'M+'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton19Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton20: TJvSpeedButton
    Left = 124
    Top = 50
    Width = 23
    Height = 22
    Caption = 'M-'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton20Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton21: TJvSpeedButton
    Left = 100
    Top = 28
    Width = 23
    Height = 22
    Hint = 'Clear last'
    Caption = 'C'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = BUSpeedButton21Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Label1: TLabel
    Left = 130
    Top = 4
    Width = 12
    Height = 16
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
  end
  object BUSpeedButton22: TJvSpeedButton
    Left = 76
    Top = 94
    Width = 23
    Height = 22
    Hint = 'Change Sign'
    Caption = '+/-'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = BUSpeedButton22Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton23: TJvSpeedButton
    Left = 100
    Top = 94
    Width = 23
    Height = 22
    Caption = 'PI'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    OnClick = BUSpeedButton23Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUSpeedButton24: TJvSpeedButton
    Left = 124
    Top = 94
    Width = 23
    Height = 22
    Hint = 'Truncation'
    Caption = 'Int'
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = BUSpeedButton24Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object edit1: TJvEdit
    Left = 2
    Top = 2
    Width = 121
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 0
    OnKeyPress = Edit1KeyPress
    Modified = False
    MaxPixel.Font.Charset = DEFAULT_CHARSET
    MaxPixel.Font.Color = clWindowText
    MaxPixel.Font.Height = -11
    MaxPixel.Font.Name = 'MS Sans Serif'
    MaxPixel.Font.Style = []
    SelStart = 0
    SelLength = 0
  end
end
