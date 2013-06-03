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
    object BUDirectoryBox1: TJvDirectoryEdit
      Left = 58
      Top = 12
      Width = 243
      Height = 20
      OnAfterDialog = BUDirectoryBox1AfterDialog
      NumGlyphs = 1
      TabOrder = 0
    end
  end
  object ScrollBox1: TScrollBox
    Left = 6
    Top = 48
    Width = 307
    Height = 141
    TabOrder = 1
  end
  object BUSearchFiles1: TJvSearchFiles
    DirOption = doExcludeSubDirs
    DirParams.LastChangeAfter = 29221
    DirParams.LastChangeBefore = 29221
    FileParams.SearchTypes = [stFileMask]
    FileParams.LastChangeAfter = 29221
    FileParams.LastChangeBefore = 29221
    FileParams.FileMasks.Strings = (
      '*.bmp')
    OnFindFile = BUSearchFile1Found
    Left = 112
    Top = 64
  end
end
