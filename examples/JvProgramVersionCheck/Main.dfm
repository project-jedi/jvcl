object Form1: TForm1
  Left = 357
  Top = 242
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
    SubStorages = <>
    Left = 80
    Top = 15
  end
  object ProgramVersionCheck: TJvProgramVersionCheck
    AllowedReleaseType = prtBeta
    AppStorage = JvAppIniFileStorageVersionCheck
    AppStoragePath = 'Local'
    CheckFrequency = 0
    LocalDirectory = 'Version Check\a\b\c'
    LocalVersionInfoFileName = 'ProgramVersionCheckLocal.Ini'
    LocationDatabase = JvProgramVersionDatabaseLocation1
    LocationFTP = JvProgramVersionFTPLocation1
    LocationNetwork = JvProgramVersionNetworkLocation1
    LocationType = pvltNetwork
    UserOptions = [uoCheckFrequency, uoAllowedReleaseType, uoLocationNetwork, uoLocationHTTP, uoLocationFTP, uoLocationDatabase]
    Left = 355
    Top = 205
  end
  object JvProgramVersionNetworkLocation1: TJvProgramVersionNetworkLocation
    VersionInfoLocationPathList.Strings = (
      'Version Check\Remote')
    VersionInfoFileName = 'ProjektVersions.ini'
    Left = 105
    Top = 200
  end
  object JvProgramVersionHTTPLocation1: TJvProgramVersionHTTPLocation
    VersionInfoFileName = 'ProjektVersions_http.ini'
    Left = 105
    Top = 240
  end
  object JvProgramVersionFTPLocation1: TJvProgramVersionFTPLocation
    Left = 105
    Top = 280
  end
  object JvProgramVersionDatabaseLocation1: TJvProgramVersionDatabaseLocation
    Left = 105
    Top = 320
  end
  object JvProgramVersionHTTPLocationIndy1: TJvProgramVersionHTTPLocationIndy
    VersionInfoLocationPathList.Strings = (
      'htasdasd'
      'http://www.oratool.de/test/')
    VersionInfoFileName = 'ProjektVersions_http.ini'
    Left = 300
    Top = 320
  end
end
