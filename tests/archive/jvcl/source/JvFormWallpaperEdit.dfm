object FoWallpaperChooser: TFoWallpaperChooser
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
  object Button1: TJvButton
    Left = 6
    Top = 200
    Width = 75
    Height = 25
    Caption = '&Ok'
    ModalResult = 1
    TabOrder = 2
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Button2: TJvButton
    Left = 102
    Top = 200
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Button3: TJvButton
    Left = 238
    Top = 198
    Width = 75
    Height = 25
    Caption = '&Clear'
    TabOrder = 4
    OnClick = Button3Click
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
    object DirectoryBox1: TJvDirectoryEdit
      Left = 58
      Top = 12
      Width = 243
      Height = 21
      OnAfterDialog = DirectoryBox1AfterDialog
      ButtonFlat = False
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
  object SearchFiles1: TJvSearchFiles
    DirOption = doExcludeSubDirs
    FileParams.SearchTypes = [stFileMask]
    FileParams.FileMasks.Strings = (
      '*.bmp')
    OnFindFile = SearchFile1Found
    Left = 112
    Top = 64
  end
end
