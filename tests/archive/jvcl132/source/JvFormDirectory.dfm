object FormDir: TFormDir
  Left = 466
  Top = 346
  BorderStyle = bsDialog
  Caption = 'Choose destination directory'
  ClientHeight = 323
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 10
    Top = 16
    Width = 120
    Height = 260
  end
  object Bevel1: TBevel
    Left = 4
    Top = 280
    Width = 381
    Height = 3
  end
  object BUButton1: TJvSpeedButton
    Left = 150
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Previous'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton2: TJvSpeedButton
    Left = 230
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Next'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton3: TJvSpeedButton
    Left = 310
    Top = 288
    Width = 75
    Height = 25
    Caption = '&Cancel'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object GroupBox1: TGroupBox
    Left = 142
    Top = 224
    Width = 233
    Height = 47
    Caption = '[ Destination Directory ]'
    TabOrder = 0
    object BUDirectoryBox1: TJvDirectoryBox
      Left = 6
      Top = 16
      Width = 219
      Height = 20
      TabOrder = 0
      Edit.Font.Charset = DEFAULT_CHARSET
      Edit.Font.Color = clWindowText
      Edit.Font.Height = -11
      Edit.Font.Name = 'MS Sans Serif'
      Edit.Font.Style = []
      Button.Flat = True
      Button.Font.Charset = DEFAULT_CHARSET
      Button.Font.Color = clWindowText
      Button.Font.Height = -11
      Button.Font.Name = 'MS Sans Serif'
      Button.Font.Style = []
      Button.Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777777777777777777777788888888888877700000000000088703838383838
        380870F3B3B3B3B3B308703B3B3B3B3B380870F3B3B3B3B3B308703B3B3B3B3B
        380870F3F3F3F3B3B308770000003B3B308777777770F3B38087777777703F3F
        3077777777770000077777777777777777777777777777777777}
      DialogOptions.Options = []
      DialogOptions.HelpContext = 0
    end
  end
  object StaticText1: TStaticText
    Left = 136
    Top = 16
    Width = 247
    Height = 41
    AutoSize = False
    TabOrder = 1
  end
  object StaticText2: TStaticText
    Left = 136
    Top = 64
    Width = 247
    Height = 41
    AutoSize = False
    TabOrder = 2
  end
  object StaticText3: TStaticText
    Left = 136
    Top = 112
    Width = 247
    Height = 53
    AutoSize = False
    TabOrder = 3
  end
end
