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
  object JvHttpUrlGrabber1: TJvHttpUrlGrabber
    FileName = 'output.txt'
    Agent = 'JEDI-VCL'
    Port = 0
    Left = 225
    Top = 190
  end
  object IdHTTP1: TIdHTTP
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
    Left = 255
    Top = 310
  end
  object ProgramVersionCheck: TJvProgramVersionCheck
    SupportedLocationTypes = [pvltNetwork, pvltDatabase, pvltFTP, pvltHTTP]
    AllowedReleaseType = prtBeta
    CheckFrequency = 0
    LocationType = pvltNetwork
    LocalDirectory = 'Version Check'
    LocalVersionInfoFileName = 'ProgramVersionCheckLocal.Ini'
    Locations.Network.DownloadThreaded = False
    Locations.Network.VersionInfoLocationPath = 'Version Check\Remote'
    Locations.Network.VersionInfoFilename = 'ProjektVersions.ini'
    Locations.HTTP.DownloadThreaded = False
    Locations.HTTP.VersionInfoLocationPath = 'http://www.oratool.de/test'
    Locations.HTTP.VersionInfoFilename = 'ProjektVersions_http.ini'
    Locations.HTTP.ProxySettings.Port = 0
    Locations.HTTP.PasswordRequired = False
    Locations.FTP.DownloadThreaded = False
    Locations.FTP.ProxySettings.Port = 0
    Locations.FTP.PasswordRequired = False
    Locations.Database.DownloadThreaded = False
    AppStorage = JvAppIniFileStorageVersionCheck
    AppStoragePath = 'Local'
    Left = 355
    Top = 205
  end
  object ImageList1: TImageList
    Left = 385
    Top = 330
  end
end
