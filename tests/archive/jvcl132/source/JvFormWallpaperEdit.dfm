object foWallpaperChooser: TfoWallpaperChooser
  Left = 479
  Top = 330
  BorderStyle = bsDialog
  Caption = 'Wallpaper Chooser'
  ClientHeight = 236
  ClientWidth = 317
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BUSpeedButton1: TJvSpeedButton
    Left = 6
    Top = 200
    Width = 75
    Height = 25
    Caption = '&Ok'
    Flat = True
    NumGlyphs = 2
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ModalResult = 1
  end
  object BUSpeedButton2: TJvSpeedButton
    Left = 102
    Top = 200
    Width = 75
    Height = 25
    Caption = '&Cancel'
    Flat = True
    NumGlyphs = 2
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ModalResult = 2
  end
  object BUSpeedButton3: TJvSpeedButton
    Left = 238
    Top = 198
    Width = 75
    Height = 25
    Caption = '&Clear'
    Flat = True
    NumGlyphs = 2
    OnClick = BUSpeedButton3Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object GroupBox1: TGroupBox
    Left = 6
    Top = 6
    Width = 307
    Height = 39
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 42
      Height = 13
      Caption = 'Directory'
    end
    object BUDirectoryBox1: TJvDirectoryBox
      Left = 58
      Top = 12
      Width = 243
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
      OnOpened = BUDirectoryBox1Opened
      DialogOptions.Options = []
      DialogOptions.HelpContext = 0
    end
  end
  object ScrollBox1: TScrollBox
    Left = 6
    Top = 48
    Width = 307
    Height = 141
    TabOrder = 1
  end
  object BUSearchFile1: TJvSearchFile
    Mask = '*.bmp'
    Recursive = False
    OnFound = BUSearchFile1Found
    Left = 188
    Top = 102
  end
end
