object FoWallpaperChooser: TFoWallpaperChooser
  Left = 401
  Top = 250
  Width = 400
  Height = 308
  BorderIcons = [biSystemMenu]
  Caption = 'Wallpaper Chooser'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 6
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 84
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Button3: TButton
    Left = 309
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    TabOrder = 4
    OnClick = Button3Click
  end
  object GroupBox1: TGroupBox
    Left = 6
    Top = 6
    Width = 382
    Height = 39
    Anchors = [akLeft, akTop, akRight]
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
      Width = 318
      Height = 21
      OnAfterDialog = DirectoryBox1AfterDialog
      DialogKind = dkWin32
      ButtonFlat = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object ScrollBox1: TScrollBox
    Left = 6
    Top = 48
    Width = 382
    Height = 186
    Anchors = [akLeft, akTop, akRight, akBottom]
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
