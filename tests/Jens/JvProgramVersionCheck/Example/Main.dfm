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
    SubStorages = <>
    Left = 80
    Top = 15
  end
  object JvHttpUrlGrabber: TJvHttpUrlGrabber
    FileName = 'output.txt'
    Agent = 'JEDI-VCL'
    Port = 0
    Left = 75
    Top = 150
  end
  object IdHTTP: TIdHTTP
    AuthRetries = 0
    AuthProxyRetries = 0
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HTTPOptions = [hoForceEncodeParams]
    Left = 350
    Top = 260
  end
  object ProgramVersionCheck: TJvProgramVersionCheck
    AllowedReleaseType = prtBeta
    CheckFrequency = 0
    LocalDirectory = 'Version Check'
    LocalVersionInfoFileName = 'ProgramVersionCheckLocal.Ini'
    LocationTypeSelected = pvltNetwork
    LocationNetwork = JvProgramVersionNetworkLocation1
    LocationHTTP = JvProgramVersionHTTPLocation1
    LocationFTP = JvProgramVersionFTPLocation1
    LocationDatabase = JvProgramVersionDatabaseLocation1
    AppStorage = JvAppIniFileStorageVersionCheck
    AppStoragePath = 'Local'
    Left = 355
    Top = 205
  end
  object JvProgramVersionNetworkLocation1: TJvProgramVersionNetworkLocation
    VersionInfoLocationPath = 'Version Check\Remote'
    VersionInfoFilename = 'ProjektVersions.ini'
    Left = 105
    Top = 200
  end
  object JvProgramVersionHTTPLocation1: TJvProgramVersionHTTPLocation
    VersionInfoLocationPath = 'www.oratool.de/test'
    VersionInfoFilename = 'ProjektVersions_http.ini'
    PasswordRequired = False
    OnLoadFileFromRemote = JvProgramVersionHTTPLocation1LoadFileFromRemote
    Left = 105
    Top = 240
  end
  object JvProgramVersionFTPLocation1: TJvProgramVersionFTPLocation
    PasswordRequired = False
    OnLoadFileFromRemote = JvProgramVersionFTPLocation1LoadFileFromRemote
    Left = 105
    Top = 280
  end
  object JvProgramVersionDatabaseLocation1: TJvProgramVersionDatabaseLocation
    Left = 105
    Top = 320
  end
  object JvFtpUrlGrabber: TJvFtpUrlGrabber
    FileName = 'output.txt'
    Agent = 'JEDI-VCL'
    Left = 225
    Top = 190
  end
end
