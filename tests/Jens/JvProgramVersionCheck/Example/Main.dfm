object Form1: TForm1
  Left = 0
  Top = 0
  Width = 476
  Height = 413
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 80
    Top = 85
    Width = 305
    Height = 29
    Caption = 'JVCL Program Version Check'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 120
    Top = 120
    Width = 198
    Height = 29
    Caption = 'Sample Application'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object JvAppIniFileStorageVersionCheck: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StorageOptions.SetAsString = True
    StorageOptions.FloatAsString = True
    StorageOptions.DefaultIfReadConvertError = True
    StorageOptions.ReplaceCRLF = True
    AutoFlush = True
    AutoReload = True
    FileName = 'versioninfolocal.ini'
    DefaultSection = 'Versioninfo'
    SubStorages = <>
    Left = 80
    Top = 15
  end
end
