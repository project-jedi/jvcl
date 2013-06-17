object Form1: TForm1
  Left = 357
  Top = 242
  Caption = 'Form1'
  ClientHeight = 502
  ClientWidth = 558
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
  object Button1: TButton
    Left = 205
    Top = 175
    Width = 101
    Height = 25
    Caption = 'Check Again'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 205
    Top = 235
    Width = 101
    Height = 25
    Caption = 'Edit History'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 205
    Top = 290
    Width = 101
    Height = 25
    Caption = 'Show History XML'
    TabOrder = 2
    OnClick = Button3Click
  end
  object JvAppIniFileStorageVersionCheck: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StorageOptions.SetAsString = True
    StorageOptions.FloatAsString = True
    StorageOptions.DefaultIfReadConvertError = True
    StorageOptions.UseOldItemNameFormat = False
    StorageOptions.ReplaceCRLF = True
    AutoFlush = True
    AutoReload = True
    FileName = 'versioninfolocal.ini'
    SubStorages = <>
    Left = 80
    Top = 15
  end
  object ProgramVersionCheck: TJvProgramVersionCheck
    AllowedReleaseType = prtAlpha
    AppStorage = JvAppIniFileStorageVersionCheck
    AppStoragePath = 'Local'
    CheckFrequency = 0
    LocalDirectory = 'Version Check\a\b\c'
    LocalVersionInfoFileName = 'ProgramVersionCheckLocal.xml'
    LocationDatabase = JvProgramVersionDatabaseLocation
    LocationFTP = JvProgramVersionFTPLocation
    LocationHTTP = JvProgramVersionHTTPLocation
    LocationNetwork = JvProgramVersionNetworkLocation
    LocationType = pvltNetwork
    UserOptions = [uoCheckFrequency, uoAllowedReleaseType, uoLocationNetwork, uoLocationHTTP, uoLocationFTP, uoLocationDatabase]
    VersionHistoryFileOptions.FileFormat = hffXML
    VersionHistoryFileOptions.INIOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    VersionHistoryFileOptions.INIOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    VersionHistoryFileOptions.INIOptions.SetAsString = True
    VersionHistoryFileOptions.INIOptions.FloatAsString = True
    VersionHistoryFileOptions.INIOptions.DefaultIfReadConvertError = True
    VersionHistoryFileOptions.INIOptions.UseOldItemNameFormat = False
    VersionHistoryFileOptions.XMLOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    VersionHistoryFileOptions.XMLOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    VersionHistoryFileOptions.XMLOptions.SetAsString = True
    VersionHistoryFileOptions.XMLOptions.FloatAsString = True
    VersionHistoryFileOptions.XMLOptions.DefaultIfReadConvertError = True
    VersionHistoryFileOptions.XMLOptions.UseOldItemNameFormat = False
    VersionHistoryFileOptions.XMLOptions.WhiteSpaceReplacement = '_'
    VersionHistoryFileOptions.XMLOptions.InvalidCharReplacement = '_'
    Left = 355
    Top = 205
  end
  object JvProgramVersionNetworkLocation: TJvProgramVersionNetworkLocation
    VersionInfoLocationPathList.Strings = (
      'Version Check\Remote')
    VersionInfoFileName = 'ProjectVersions.xml'
    Left = 105
    Top = 200
  end
  object JvProgramVersionHTTPLocation: TJvProgramVersionHTTPLocation
    VersionInfoFileName = 'ProjectVersions_http.ini'
    Left = 105
    Top = 240
  end
  object JvProgramVersionFTPLocation: TJvProgramVersionFTPLocation
    Left = 105
    Top = 280
  end
  object JvProgramVersionDatabaseLocation: TJvProgramVersionDatabaseLocation
    Left = 105
    Top = 320
  end
end
