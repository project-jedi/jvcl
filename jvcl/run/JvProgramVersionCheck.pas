{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgramVersionCheck.PAS, released on 2004-12-16.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott com]
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvProgramVersionCheck;

{$I jvcl.inc}

interface

uses
  Classes,
  {$IFDEF USE_3RDPARTY_INDY}
  idhttp, idftp,
  {$ENDIF USE_3RDPARTY_INDY}
  {$IFDEF USE_3RDPARTY_ICS}
  HttpProt, FtpCli,
  {$ENDIF USE_3RDPARTY_ICS}
  JvPropertyStore, JvAppStorage, JvAppIniStorage, JvComponent,
  JvParameterList, JvThread, JvUrlListGrabber, JvUrlGrabbers, JvThreadDialog;

type
  { Type of release of a Program Version }
  TJvProgramReleaseType = (prtProduction, prtBeta, prtAlpha);

  TJvRemoteVersionOperation = (rvoIgnore, rvoCopy, rvoCopyInstall);

  { List class to collect and sort version infos }
  TJvProgramVersionsStringList = class(TStringList)
  public
    procedure Sort; override;
  end;

  { Class to collect all informations about a program version
    These informations will be stored in the ini-file on the remote site}
  TJvProgramVersionInfo = class(TJvCustomPropertyStore)
  private
    FDownloadPasswordRequired: Boolean;
    FVersionDescription: TStringList;
    FProgramSize: Integer;
    FProgramVersion: string;
    FProgramLocationPath: string;
    FProgramLocationFileName: string;
    FProgramReleaseType: TJvProgramReleaseType;
    FProgramReleaseDate: TDateTime;
    function GetVersionDescription: TStrings;
    procedure SetVersionDescription(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    { Combination of ProgramVersion and ReleaseType }
    function ProgramVersionReleaseType: string;
    function ProgramSizeString: string;
    function ProgramVersionInfo: string;
  published
    property DownloadPasswordRequired: Boolean read FDownloadPasswordRequired write
      FDownloadPasswordRequired default False;
    { Path where the installer of the version could be found. This could be
    a absolute path or a relative path to the location of the version list file }
    property ProgramLocationPath: string read FProgramLocationPath write FProgramLocationPath;
    { File name of the installer file }
    property ProgramLocationFileName: string
      read FProgramLocationFileName write FProgramLocationFileName;
    { Program version in the format <main>.<sub>.<release>.<build>
    This property is compared with the fileversion properties of the current
    application. }
    property ProgramVersion: string read FProgramVersion write FProgramVersion;
    { This is a description field which could be shown in the update dialog via
      the version info button }
    property VersionDescription: TStrings read GetVersionDescription write SetVersionDescription;
    { Release type of the version.
    In the update dialog there are only the highest version numbers for each type
    visible. The type must be higher then AllowedReleaseType property of the
    TJvProgramVersionCheck component }
    property ProgramReleaseType: TJvProgramReleaseType read FProgramReleaseType write FProgramReleaseType;
    { Size of the installer in bytes }
    property ProgramSize: Integer read FProgramSize write FProgramSize;
    { Date of Release }
    property ProgramReleaseDate: TDateTime read FProgramReleaseDate write FProgramReleaseDate;
  end;

  TJvProgramVersionInfoReleaseArray = array [TJvProgramReleaseType] of TJvProgramVersionInfo;

  { List of all Program version stored in a remote file via TJvAppStorage }
  TJvProgramVersionHistory = class(TJvCustomPropertyListStore)
  private
    FCurrentProductionVersion: string;
    FCurrentBetaVersion: string;
    FCurrentAlphaVersion: string;
    FCurrentProgramVersion: TJvProgramVersionInfoReleaseArray;
  protected
    function CreateObject: TObject; override;
    function CreateItemList: TStringList; override;
    function GetProgramVersion(Index: Integer): TJvProgramVersionInfo;
    function GetCurrentProgramVersion(Index: TJvProgramReleaseType): TJvProgramVersionInfo;
    function SearchCurrentProgramVersion(iProgramReleaseType: TJvProgramReleaseType): TJvProgramVersionInfo;
    function GetCurrentProductionProgramVersion: string;
    function GetCurrentBetaProgramVersion: string;
    function GetCurrentAlphaProgramVersion: string;
    property ProgramVersion[Index: Integer]: TJvProgramVersionInfo read GetProgramVersion;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadData; override;
    procedure RecalculateCurrentProgramVersions;
    function AllowedCurrentProgramVersion(iAllowedReleaseType: TJvProgramReleaseType): TJvProgramVersionInfo;
    function GetVersionsDescription(const iFromVersion, iToVersion: string): string;
    property CurrentProgramVersion[Index: TJvProgramReleaseType]: TJvProgramVersionInfo read GetCurrentProgramVersion;
  published
    property CurrentProductionProgramVersion: string
      read GetCurrentProductionProgramVersion write FCurrentProductionVersion;
    property CurrentBetaProgramVersion: string read GetCurrentBetaProgramVersion write FCurrentBetaVersion;
    property CurrentAlphaProgramVersion: string read GetCurrentAlphaProgramVersion write FCurrentAlphaVersion;
  end;

  { Base class for all location
    A Location is the class which defines where the remote files could be found and
    manages all communications to these files. }
  TJvProgramVersionCustomLocation = class(TJvCustomPropertyStore)
  private
    FDownloadError: string;
    FDownloadStatus: string;
    FDownloadThreaded: Boolean;
  protected
    procedure SetDownloadStatus(Value: string);
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function LoadFileFromRemote(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; virtual;
    function LoadInstallerFileFromRemote(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; virtual;
    function LoadVersionInfoFromRemote(const iLocalDirectory, iLocalVersionInfoFileName: string;
      iBaseThread: TJvBaseThread): string; virtual;
    property DownloadStatus: string read FDownloadStatus write FDownloadStatus;
    property DownloadError: string read FDownloadError write FDownloadError;
  published
    property DownloadThreaded: Boolean read FDownloadThreaded write FDownloadThreaded default False;
  end;

  { Base class for all file based Locations like Network, FTP and HTTP }
  TJvProgramVersionCustomFileBasedLocation = class(TJvProgramVersionCustomLocation)
  private
    FVersionInfoLocationPathList: TStringList;
    FVersionInfoFileName: string;
    FValidLocationPath: string;
    function GetVersionInfoLocationPathList: TStrings;
    procedure SetVersionInfoLocationPathList(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadVersionInfoFromRemote(const iLocalDirectory,
      iLocalVersionInfoFileName: string; iBaseThread: TJvBaseThread): string; override;
    function LoadInstallerFileFromRemote(const iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName: string; iBaseThread: TJvBaseThread): string; override;
    property ValidLocationPath: string read FValidLocationPath;
  published
    { List of locations-path where the remote files could be found
    The application loops throuh all path from the top }
    property VersionInfoLocationPathList: TStrings read GetVersionInfoLocationPathList
      write SetVersionInfoLocationPathList;
    { Name of the VersionInfofile at the remote location }
    property VersionInfoFileName: string read FVersionInfoFileName write FVersionInfoFileName;
  end;

  { Location Class for Local Network Location }
  TJvProgramVersionNetworkLocation = class(TJvProgramVersionCustomFileBasedLocation)
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
  end;

  { Class for Proxy Settings for FTP and HTTP locations }
  TJvProgramVersionProxySettings = class(TPersistent)
  private
    FServer: string;
    FPort: Integer;
    FUserName: string;
    FPassword: string;
  public
    constructor Create;
  published
    property Server: string read FServer write FServer;
    property Port: Integer read FPort write FPort default 80;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
  end;

  { Base class for all Internet locations  }
  TJvProgramVersionInternetLocation = class(TJvProgramVersionCustomFileBasedLocation)
  private
    FProxySettings: TJvProgramVersionProxySettings;
    FPasswordRequired: Boolean;
    FUserName: string;
    FPassword: string;
    FPort: Integer;
  protected
    property ProxySettings: TJvProgramVersionProxySettings read FProxySettings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property PasswordRequired: Boolean read FPasswordRequired write FPasswordRequired default False;
    property Port: Integer read FPort write FPort default 80;
  end;

  TJvProgramVersionHTTPLocation = class;
  TJvLoadFileFromRemoteHTTPEvent = function(iProgramVersionLocation: TJvProgramVersionHTTPLocation;
    const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string): string of object;

  { Simple HTTP location class with no http logic.
  The logic must be implemented manually in the OnLoadFileFromRemote event }
  TJvProgramVersionHTTPLocation = class(TJvProgramVersionInternetLocation)
  private
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteHTTPEvent;
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
  published
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteHTTPEvent
      read FOnLoadFileFromRemote write FOnLoadFileFromRemote;
    property ProxySettings;
  end;

  {$IFDEF USE_3RDPARTY_INDY}
  TJvProgramVersionHTTPLocationIndy = class(TJvProgramVersionHTTPLocation)
  private
    FIdHttp: TIdHttp;
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIndy(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ProxySettings;
  end;
  {$ENDIF USE_3RDPARTY_INDY}

  {$IFDEF USE_3RDPARTY_ICS}
  TJvProgramVersionHTTPLocationICS = class(TJvProgramVersionHTTPLocation)
  private
    FHttpCli: THttpCli;
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIcs(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ProxySettings;
  end;
  {$ENDIF USE_3RDPARTY_ICS}

  TJvProgramVersionFTPLocation = class;
  TJvLoadFileFromRemoteFTPEvent = function(iProgramVersionLocation: TJvProgramVersionFTPLocation;
    const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string): string of object;

  { Simple FTP location class with no http logic.
  The logic must be implemented manually in the OnLoadFileFromRemote event }
  TJvProgramVersionFTPLocation = class(TJvProgramVersionInternetLocation)
  private
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteFTPEvent;
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
  published
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteFTPEvent
      read FOnLoadFileFromRemote write FOnLoadFileFromRemote;
    property ProxySettings;
  end;

  {$IFDEF USE_3RDPARTY_INDY}
  TJvProgramVersionFTPLocationIndy = class(TJvProgramVersionFTPLocation)
  private
    FIdFtp: TIdFtp;
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIndy(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ProxySettings;
  end;
  {$ENDIF USE_3RDPARTY_INDY}

  {$IFDEF USE_3RDPARTY_ICS}
  TJvProgramVersionFTPLocationICS = class(TJvProgramVersionFTPLocation)
  private
    FFtpClient: TFtpClient;
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIcs(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ProxySettings;
  end;
  {$ENDIF USE_3RDPARTY_ICS}

  TJvProgramVersionDatabaseLocation = class;
  TJvLoadFileFromRemoteDatabaseEvent = function(iProgramVersionLocation: TJvProgramVersionDatabaseLocation;
      const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string): string of object;

  { Simple Database location class with no http logic.
  The logic must be implemented manually in the OnLoadFileFromRemote event }

  TJvProgramVersionDatabaseLocation = class(TJvProgramVersionCustomLocation)
  private
    FServerName: string;
    FUserName: string;
    FPassword: string;
    FSelectStatementVersion: string;
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteDatabaseEvent;
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
  public
    function LoadVersionInfoFromRemote(const iLocalDirectory, iLocalVersionInfoFileName: string;
      iBaseThread: TJvBaseThread): string; override;
  published
    property ServerName: string read FServerName write FServerName;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property SelectStatementVersion: string
      read FSelectStatementVersion write FSelectStatementVersion;
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteDatabaseEvent
      read FOnLoadFileFromRemote write FOnLoadFileFromRemote;
  end;

  { Location Type for the TjvProgramVersionCheck class}
  TJvProgramVersionLocationType = (pvltNetwork, pvltDatabase, pvltFTP, pvltHTTP);
  {Set of TJvProgramVersionLocationTypes}
  TJvProgramVersionLocationTypes = set of TJvProgramVersionLocationType;

  { Type for User Customizing options to the JvProgramVersionCheck
  The settings of the Programversioncheck are stored via JvAppStorage. With
  these types could be defined which settings are stored and restored and so
  customisable by the end user}
  TJvProgramVersionUserOption = (uoCheckFrequency, uoLocalDirectory,
    uoAllowedReleaseType, uoLocationType, uoLocationNetwork,
    uoLocationHTTP, uoLocationFTP, uoLocationDatabase);
  { set of TJvProgramVersionUserOption }
  TJvProgramVersionUserOptions = set of TJvProgramVersionUserOption;

  TJvProgramVersionCheck = class(TJvCustomPropertyStore)
  private
    FAllowedReleaseType: TJvProgramReleaseType;
    FCheckFrequency: Integer;
    FDownloadError: string;
    FExecuteDownloadInstallFileName: string;
    FExecuteOperation: TJvRemoteVersionOperation;
    FExecuteVersionInfo: TJvProgramVersionInfo;
    FLastCheck: TDateTime;
    FLocalDirectory: string;
    FLocalInstallerFileName: string;
    FLocalVersionInfoFileName: string;
    FLocationDatabase: TJvProgramVersionDatabaseLocation;
    FLocationFTP: TJvProgramVersionFTPLocation;
    FLocationHTTP: TJvProgramVersionHTTPLocation;
    FLocationNetwork: TJvProgramVersionNetworkLocation;
    FLocationType: TJvProgramVersionLocationType;
    FRemoteAppStorage: TJvAppIniFileStorage;
    FRemoteProgramVersionHistory: TJvProgramVersionHistory;
    FThread: TJvThread;
    FThreadDialog: TJvThreadAnimateDialog;
    FUserOptions: TJvProgramVersionUserOptions;
  protected
    procedure CheckLocalDirectory;
    function CurrentApplicationName: string;
    function CurrentFileVersion: string;
    procedure DownloadThreadOnExecute(Sender: TObject; Params: Pointer);
    procedure DownloadThreadOnFinishAll(Sender: TObject);
    function GetAllowedRemoteProgramVersion: string;
    function GetAllowedRemoteProgramVersionReleaseType: string;
    function GetLocationTypesSupported: TJvProgramVersionLocationTypes;
    function IsRemoteProgramVersionNewer: Boolean;
    function IsRemoteProgramVersionReleaseTypeNewer(iReleaseType: TJvProgramReleaseType): Boolean;
    procedure LoadData; override;
    function LoadRemoteInstallerFile(const iLocalDirectory, iLocalInstallerFileName: string;
      iProgramVersionInfo: TJvProgramVersionInfo; iBaseThread: TJvBaseThread): string;
    function LoadRemoteVersionInfoFile(const iLocalDirectory, iLocalVersionInfoFileName: string): string;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetThreadInfo(const Info: string);
    procedure SetUserOptions(Value: TJvProgramVersionUserOptions);
    procedure StoreData; override;
    procedure StoreRemoteVersionInfoToFile;
    procedure VersionInfoButtonClick(const ParameterList: TJvParameterList;
      const Parameter: TJvBaseParameter);
    property RemoteAppStorage: TJvAppIniFileStorage read FRemoteAppStorage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DownloadInstallerFromRemote;
    procedure Execute;
    function GetRemoteVersionOperation(var ReleaseType: TJvProgramReleaseType): TJvRemoteVersionOperation;
    function SelectedLocation: TJvProgramVersionCustomLocation;
    procedure ShowProgramVersionsDescription(const iFromVersion, iToVersion: string);
    property LastCheck: TDateTime read FLastCheck write FLastCheck;
    property LocationTypesSupported: TJvProgramVersionLocationTypes read GetLocationTypesSupported;
    property RemoteProgramVersionHistory: TJvProgramVersionHistory
      read FRemoteProgramVersionHistory write FRemoteProgramVersionHistory;
  published
    property AboutJVCL;
    { Defines which release types will be shown in the update dialog }
    property AllowedReleaseType: TJvProgramReleaseType
      read FAllowedReleaseType write FAllowedReleaseType default prtProduction;
    property AppStorage;
    property AppStoragePath;
    { Defines how often the check for a new version is executed (in days) }
    property CheckFrequency: Integer read FCheckFrequency write FCheckFrequency;
    { Defines the local directory where the remote files where stored }
    property LocalDirectory: string read FLocalDirectory write FLocalDirectory;
    { Defines the local name of the program installer. If it is empty the name
      of the remote file is used }
    property LocalInstallerFileName: string
      read FLocalInstallerFileName write FLocalInstallerFileName;
    { Defines the name of the local version info file. If it is empty the name
      of the remote file is used }
    property LocalVersionInfoFileName: string
      read FLocalVersionInfoFileName write FLocalVersionInfoFileName;
    { Database Location }
    property LocationDatabase: TJvProgramVersionDatabaseLocation
      read FLocationDatabase write FLocationDatabase;
    { FTP Location }
    property LocationFTP: TJvProgramVersionFTPLocation read FLocationFTP write FLocationFTP;
    { HTTP Location }
    property LocationHTTP: TJvProgramVersionHTTPLocation read FLocationHTTP write FLocationHTTP;
    { Network Location }
    property LocationNetwork: TJvProgramVersionNetworkLocation read FLocationNetwork write FLocationNetwork;
    { Defines location which is used for the version check,
    only assigned locations are supported }
    property LocationType: TJvProgramVersionLocationType read FLocationType write FLocationType;
    {Defines which options of the component are stored/restored wia AppStorage }
    property UserOptions: TJvProgramVersionUserOptions read FUserOptions write SetUserOptions
      default [uoCheckFrequency, uoLocalDirectory, uoAllowedReleaseType,
        uoLocationType, uoLocationNetwork, uoLocationHTTP, uoLocationFTP, uoLocationDatabase];
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Dialogs, Controls, ComCtrls, StdCtrls,
  {$IFDEF MSWINDOWS}
  Forms,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QForms,
  {$ENDIF UNIX}
  JclFileUtils, JclShell,
  JvDSADialogs, JvParameterListParameter;

const
  cProgramVersion = 'Program Version ';
  cLastCheck = 'LastCheck';

resourcestring
  RsPVCReleaseTypeAlpha = 'Alpha';
  RsPVCReleaseTypeBeta = 'Beta';
  RsPVCReleaseTypeProduction = 'Production';

  RsPVCDownloading = 'Downloading ...';
  RsPVCDialogCaption = '%s Upgrade Check';
  RsPVCDialogExecuteButton = '&Execute';
  RsPVCNewVersionAvailable = 'A new version (%s) of %s is available!';
  RsPVCChooseWhichVersion = 'Which &version do you want to install?';
  RsPVCChooseOperation = '&Choose Operation';
  RsPVCOperationIgnore = 'I&gnore';
  RsPVCOperationDownloadOnly = 'Download/Copy &Only';
  RsPVCOperationDownloadInstall = 'Download/Copy and &Install';
  RsPVCWhatNewInS = 'What''s new in %s';
  RsPVCChangesBetween = 'Changes between %s and %s';
  RsPVCFileDownloadNotSuccessful =
    'The file download was not successful!' + #13#10 + 'Please try again manually.';
  RsPVCDownloadSuccessfulInstallManually =
    'The file download was successful.' + #13#10 + 'Install manually from: %s';
  RsPVCErrorStartingSetup = 'Error starting the setup process.';
  RsPVCDownloadSuccessfullInstallNow =
    'The file download was successful.' + #13#10 +
    'Do you want to close and install?';

//=== Common Functions =======================================================

function CompareVersionNumbers(iVersion1, iVersion2: string): Integer;
var
  n1, n2: Integer;

  function GetNextNumber(var Version: string): Integer;
  var
    P: Integer;
    S: string;
  begin
    P := Pos('.', Version);
    if P > 0 then
    begin
      S := Copy(Version, 1, P - 1);
      Version := Copy(Version, P + 1, Length(Version) - P);
    end
    else
    begin
      S := Version;
      Version := '';
    end;
    if S = '' then
      Result := -1
    else
      try
        Result := StrToInt(S);
      except
        Result := -1;
      end;
  end;

begin
  Result := 0;
  repeat
    n1 := GetNextNumber(iVersion1);
    n2 := GetNextNumber(iVersion2);
    if n2 > n1 then
    begin
      Result := 1;
      Exit;
    end
    else
    if n2 < n1 then
    begin
      Result := -1;
      Exit;
    end
  until (iVersion1 = '') or (iVersion2 = '');
end;

//=== { TJvProgramVersionsStringList } =======================================

procedure TJvProgramVersionsStringList.Sort;

  function VersionNumberSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
  var
    s1, s2: string;
  begin
    s1 := TJvProgramVersionInfo(List.Objects[Index1]).ProgramVersion;
    s2 := TJvProgramVersionInfo(List.Objects[Index2]).ProgramVersion;
    Result := CompareVersionNumbers(s1, s2);
  end;

begin
  CustomSort(@VersionNumberSortCompare);
end;

//=== { TJvProgramVersionInfo } ==============================================

constructor TJvProgramVersionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersionDescription := TStringList.Create;
  IgnoreLastLoadTime := True;
  FDownloadPasswordRequired := False;
end;

destructor TJvProgramVersionInfo.Destroy;
begin
  FreeAndNil(FVersionDescription);
  inherited Destroy;
end;

function TJvProgramVersionInfo.GetVersionDescription: TStrings;
begin
  Result := FVersionDescription;
end;

procedure TJvProgramVersionInfo.SetVersionDescription(Value: TStrings);
begin
  FVersionDescription.Assign(Value);
end;

procedure TJvProgramVersionInfo.Clear;
begin
  FVersionDescription.Clear;
  FProgramVersion := '';
  FProgramReleaseType := prtProduction;
end;

function TJvProgramVersionInfo.ProgramVersionReleaseType: string;
begin
  case ProgramReleaseType of
    prtBeta:
      Result := Trim(ProgramVersion + ' ' + RsPVCReleaseTypeBeta);
    prtAlpha:
      Result := Trim(ProgramVersion + ' ' + RsPVCReleaseTypeAlpha);
  else
    Result := Trim(ProgramVersion + ' ' + RsPVCReleaseTypeProduction);
  end;
end;

function TJvProgramVersionInfo.ProgramSizeString: string;
begin
  if ProgramSize <= 0 then
    Result := ''
  else
  if ProgramSize >= 1024 * 1024 * 1024 then
    Result := Format('%6.2f GB', [ProgramSize / 1024 / 1024 / 1024])
  else
  if ProgramSize >= 1024 * 1024 then
    Result := Format('%6.2f MB', [ProgramSize / 1024 / 1024])
  else
  if ProgramSize >= 1024 then
    Result := Format('%6.2f KB', [ProgramSize / 1024])
  else
    Result := IntToStr(ProgramSize) + 'B';
end;

function TJvProgramVersionInfo.ProgramVersionInfo: string;
begin
  Result := ProgramVersionReleaseType;
  if ProgramSize > 0 then
    Result := Result + ' (' + ProgramSizeString + ')';
end;

//=== { TJvProgramVersionHistory } ===========================================

constructor TJvProgramVersionHistory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DeleteBeforeStore := True;
  ItemName := cProgramVersion;
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('Duplicates');
  IgnoreProperties.Add('Sorted');
end;

procedure TJvProgramVersionHistory.RecalculateCurrentProgramVersions;
var
  I: TJvProgramReleaseType;
begin
  for I := Low(TJvProgramReleaseType) to High(TJvProgramReleaseType) do
    FCurrentProgramVersion[I] := SearchCurrentProgramVersion(I);
end;

procedure TJvProgramVersionHistory.LoadData;
begin
  inherited LoadData;
  Items.Sort;
  RecalculateCurrentProgramVersions;
end;

function TJvProgramVersionHistory.AllowedCurrentProgramVersion(
  iAllowedReleaseType: TJvProgramReleaseType): TJvProgramVersionInfo;
var
  I: TJvProgramReleaseType;
begin
  Result := nil;
  I := Low(TJvProgramReleaseType);
  while I <= iAllowedReleaseType do
  begin
    if Result = nil then
      Result := CurrentProgramVersion[I]
    else
    if Assigned(CurrentProgramVersion[I]) and
      (CompareVersionNumbers(Result.ProgramVersion, CurrentProgramVersion[I].ProgramVersion) > 0) then
      Result := CurrentProgramVersion[I];
    Inc(I);
  end;
end;

function TJvProgramVersionHistory.GetProgramVersion(Index: Integer): TJvProgramVersionInfo;
begin
  if Assigned(Objects[Index]) and (Objects[Index] is TJvProgramVersionInfo) then
    Result := TJvProgramVersionInfo(Objects[Index])
  else
    Result := nil;
end;

function TJvProgramVersionHistory.SearchCurrentProgramVersion(
  iProgramReleaseType: TJvProgramReleaseType): TJvProgramVersionInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Assigned(ProgramVersion[I]) then
      if ProgramVersion[I].ProgramReleaseType = iProgramReleaseType then
        if Result = nil then
          Result := ProgramVersion[I]
        else
        if CompareVersionNumbers(Result.ProgramVersion, ProgramVersion[I].ProgramVersion) = 1 then
          Result := ProgramVersion[I];
end;

function TJvProgramVersionHistory.GetCurrentProgramVersion(Index: TJvProgramReleaseType): TJvProgramVersionInfo;
begin
  Result := FCurrentProgramVersion[Index];
end;

function TJvProgramVersionHistory.CreateObject: TObject;
begin
  Result := TJvProgramVersionInfo.Create(Self);
end;

function TJvProgramVersionHistory.CreateItemList: TStringList;
begin
  Result := TJvProgramVersionsStringList.Create;
end;

function TJvProgramVersionHistory.GetCurrentProductionProgramVersion: string;
begin
  if Assigned(CurrentProgramVersion[prtProduction]) then
    Result := CurrentProgramVersion[prtProduction].ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionHistory.GetCurrentBetaProgramVersion: string;
begin
  if Assigned(CurrentProgramVersion[prtBeta]) then
    Result := CurrentProgramVersion[prtBeta].ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionHistory.GetCurrentAlphaProgramVersion: string;
begin
  if Assigned(CurrentProgramVersion[prtAlpha]) then
    Result := CurrentProgramVersion[prtAlpha].ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionHistory.GetVersionsDescription(const iFromVersion, iToVersion: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    if (CompareVersionNumbers(iFromVersion, ProgramVersion[I].ProgramVersion) >= 0) and
      (CompareVersionNumbers(iToVersion, ProgramVersion[I].ProgramVersion) <= 0) then
    begin
      Result := Result + ProgramVersion[I].ProgramVersionReleaseType;
      if ProgramVersion[I].ProgramReleaseDate > 0 then
        Result := Result + ' - ' + DateTimeToStr(ProgramVersion[I].ProgramReleaseDate);
      if ProgramVersion[I].VersionDescription.Count > 0 then
        Result := Result + #13#10 + ProgramVersion[I].VersionDescription.Text;
      Result := Result + #13#10#13#10;
    end;
end;

//=== { TJvProgramVersionCustomLocation } ====================================

constructor TJvProgramVersionCustomLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDownloadThreaded := False;
  FDownloadStatus := '';
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('DownloadThreaded');
end;

function TJvProgramVersionCustomLocation.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
end;

function TJvProgramVersionCustomLocation.LoadFileFromRemote(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
var
  TemporaryLocalFileName: string;
  LocalFileName: string;
begin
  DownloadStatus := RsPVCDownloading;
  DownloadError := '';
  if iLocalFileName = '' then
    LocalFileName := iRemoteFileName
  else
    LocalFileName := iLocalFileName;
  TemporaryLocalFileName := LocalFileName + '.temp';
  if FileExists(PathAppend(iLocalPath, TemporaryLocalFileName)) then
    DeleteFile(PathAppend(iLocalPath, TemporaryLocalFileName));
  Result := LoadFileFromRemoteInt(iRemotePath, iRemoteFileName,
    iLocalPath, TemporaryLocalFileName, iBaseThread);
  if FileExists(Result) then
  begin
    if FileExists(PathAppend(iLocalPath, LocalFileName)) then
      DeleteFile(PathAppend(iLocalPath, LocalFileName));
    if RenameFile(Result, PathAppend(iLocalPath, LocalFileName)) then
      Result := PathAppend(iLocalPath, LocalFileName)
    else
      Result := '';
  end;
end;

function TJvProgramVersionCustomLocation.LoadInstallerFileFromRemote(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := LoadFileFromRemote(iRemotePath, iRemoteFileName,
    iLocalPath, iLocalFileName, iBaseThread);
end;

function TJvProgramVersionCustomLocation.LoadVersionInfoFromRemote(
  const iLocalDirectory, iLocalVersionInfoFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
end;

procedure TJvProgramVersionCustomLocation.SetDownloadStatus(Value: string);
begin
  FDownloadStatus := Value;
  //  if Assigned(Owner.Owner) and
  //     (Owner.Owner is TJvProgramVersionCheck) then
  //       TJvProgramVersionCheck(Owner.Owner).SetThreadInfo(Value);
end;

//=== { TJvProgramVersionCustomFileBasedLocation } ===========================

constructor TJvProgramVersionCustomFileBasedLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersionInfoLocationPathList := TStringList.Create;
end;

destructor TJvProgramVersionCustomFileBasedLocation.Destroy;
begin
  FreeAndNil(FVersionInfoLocationPathList);
  inherited Destroy;
end;

function TJvProgramVersionCustomFileBasedLocation.GetVersionInfoLocationPathList: TStrings;
begin
  Result := FVersionInfoLocationPathList;
end;

procedure TJvProgramVersionCustomFileBasedLocation.SetVersionInfoLocationPathList(Value: TStrings);
begin
  FVersionInfoLocationPathList.Assign(Value);
end;

function TJvProgramVersionCustomFileBasedLocation.LoadVersionInfoFromRemote(
  const iLocalDirectory, iLocalVersionInfoFileName: string;
  iBaseThread: TJvBaseThread): string;
var
  I: Integer;
begin
  for I := 0 to VersionInfoLocationPathList.Count - 1 do
  begin
    Result := LoadFileFromRemote(VersionInfoLocationPathList[I], VersionInfoFileName,
      iLocalDirectory, iLocalVersionInfoFileName, iBaseThread);
    if Result <> '' then
    begin
      FValidLocationPath := VersionInfoLocationPathList[I];
      Exit;
    end;
  end;
  if Result = '' then
  begin
    Result := LoadFileFromRemote('', VersionInfoFileName,
      iLocalDirectory, iLocalVersionInfoFileName, iBaseThread);
    if Result <> '' then
      FValidLocationPath := '';
  end;
end;

function TJvProgramVersionCustomFileBasedLocation.LoadInstallerFileFromRemote(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := LoadFileFromRemote(iRemotePath, iRemoteFileName,
    iLocalPath, iLocalFileName, iBaseThread);
  if Result = '' then
    Result := LoadFileFromRemote(ValidLocationPath + iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName, iBaseThread);
end;

//=== { TJvProgramVersionNetworkLocation } ===================================

function TJvProgramVersionNetworkLocation.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;

  function FileExistsNoDir(iFilename: string): Boolean;
  begin
    Result := FileExists(iFilename) and not DirectoryExists(iFilename);
  end;

begin
  Result := '';
  if (DirectoryExists(iLocalPath) or (iLocalPath = '')) and
    (DirectoryExists(iRemotePath) or (iRemotePath = '')) then
    if FileExistsNoDir(PathAppend(iRemotePath, iRemoteFileName)) then
      if (iRemotePath = iLocalPath) and (iRemoteFileName = iLocalFileName) then
        Result := PathAppend(iRemotePath, iRemoteFileName)
      else
      if FileCopy(PathAppend(iRemotePath, iRemoteFileName), PathAppend(iLocalPath, iLocalFileName), True) then
        if FileExistsNoDir(PathAppend(iLocalPath, iLocalFileName)) then
          Result := PathAppend(iLocalPath, iLocalFileName)
        else
        if FileExistsNoDir(PathAppend(iLocalPath, iRemoteFileName)) then
          Result := PathAppend(iLocalPath, iRemoteFileName)
        else
        if FileExistsNoDir(PathAppend(iLocalPath, ExtractFileName(iRemotePath))) then
          Result := PathAppend(iLocalPath, ExtractFileName(iRemotePath));
end;

//=== { TJvProgramVersionInternetLocation } ==================================

constructor TJvProgramVersionInternetLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProxySettings := TJvProgramVersionProxySettings.Create;
  FPasswordRequired := False;
  FPort := 80;
end;

destructor TJvProgramVersionInternetLocation.Destroy;
begin
  FreeAndNil(FProxySettings);
  inherited Destroy;
end;

//=== { TJvProgramVersionHTTPLocation } ======================================

function TJvProgramVersionHTTPLocation.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName);
end;

//=== { TJvProgramVersionFTPLocation } =======================================

function TJvProgramVersionFTPLocation.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName);
end;

//=== { TJvProgramVersionDatabaseLocation } ==================================

function TJvProgramVersionDatabaseLocation.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName);
end;

function TJvProgramVersionDatabaseLocation.LoadVersionInfoFromRemote(
  const iLocalDirectory, iLocalVersionInfoFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := LoadFileFromRemote(SelectStatementVersion, '', iLocalDirectory,
    iLocalVersionInfoFileName, iBaseThread);
end;

//=== { TJvProgramVersionCheck } =============================================

constructor TJvProgramVersionCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRemoteProgramVersionHistory := TJvProgramVersionHistory.Create(Self);
  FRemoteProgramVersionHistory.IgnoreLastLoadTime := True;
  FRemoteAppStorage := TJvAppIniFileStorage.Create(Self);
  FRemoteAppStorage.Location := flCustom;
  FRemoteAppStorage.ReadOnly := True;
  FRemoteAppStorage.AutoReload := True;
  FRemoteAppStorage.DefaultSection := 'Version';
  with FRemoteAppStorage.StorageOptions do
  begin
    SetAsString := True;
    FloatAsString := True;
    DefaultIfReadConvertError := True;
    DateTimeAsString := True;
  end;
  FRemoteProgramVersionHistory.AppStorage := FRemoteAppStorage;
  FThread := TJvThread.Create(Self);
  FThread.Exclusive := True;
  FThread.RunOnCreate := True;
  FThread.FreeOnTerminate := True;
  FThreadDialog := TJvThreadAnimateDialog.Create(Self);
  FThreadDialog.DialogOptions.ShowDialog := True;
  FThreadDialog.DialogOptions.ShowCancelButton := True;
  FThreadDialog.DialogOptions.ShowElapsedTime := True;
  TJvThreadAnimateDialogOptions(FThreadDialog.DialogOptions).CommonAvi := aviCopyFile;
  FThread.ThreadDialog := FThreadDialog;

  DeleteBeforeStore := True;
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('LocalInstallerFileName');
  IgnoreProperties.Add('LocalVersionInfoFileName');
  IgnoreProperties.Add('RemoteAppStorage');
  IgnoreProperties.Add('UserOptions');

  FUserOptions := [uoCheckFrequency, uoLocalDirectory,
    uoAllowedReleaseType, uoLocationType, uoLocationNetwork,
    uoLocationHTTP, uoLocationFTP, uoLocationDatabase];

  //  FLocations:= TJvProgramVersionLocations.Create(Self);
  FAllowedReleaseType := prtProduction;
  FLocalInstallerFileName := '';
  FLocalVersionInfoFileName := 'versioninfo.ini';
  FLocationType := pvltNetWork;
end;

destructor TJvProgramVersionCheck.Destroy;
begin
  FreeAndNil(FThreadDialog);
  FreeAndNil(FThread);
  FreeAndNil(FRemoteAppStorage);
  inherited Destroy;
end;

procedure TJvProgramVersionCheck.CheckLocalDirectory;
begin
  LocalDirectory := Trim(LocalDirectory);
  if LocalDirectory <> '' then
    if not DirectoryExists(LocalDirectory) then
      if not ForceDirectories(LocalDirectory) then
        LocalDirectory := '';
end;

function TJvProgramVersionCheck.CurrentApplicationName: string;
var
  FileVersionInfo: TJclFileVersionInfo;
begin
  FileVersionInfo := TJclFileVersionInfo.Create(ParamStr(0));
  try
    try
      Result := FileVersionInfo.ProductName;
    except
      Result := '';
    end;
    if Result = '' then
      Result := PathExtractFileNameNoExt(ParamStr(0));
  finally
    FileVersionInfo.Free;
  end;
end;

function TJvProgramVersionCheck.CurrentFileVersion: string;
var
  FileVersionInfo: TJclFileVersionInfo;
begin
  FileVersionInfo := TJclFileVersionInfo.Create(ParamStr(0));
  try
    try
      Result := FileVersionInfo.Fileversion;
    except
      Result := '';
    end;
  finally
    FileVersionInfo.Free;
  end;
end;

procedure TJvProgramVersionCheck.DownloadInstallerFromRemote;
begin
  if Assigned(FExecuteVersionInfo) then
  begin
    FThread.OnExecute := DownloadThreadOnExecute;
    FThread.OnFinishAll := DownloadThreadOnFinishAll;
    FThread.Execute(Self);
  end;
end;

procedure TJvProgramVersionCheck.DownloadThreadOnExecute(Sender: TObject; Params: Pointer);
begin
  if Assigned(FExecuteVersionInfo) then
  begin
    FExecuteDownloadInstallFileName :=
      LoadRemoteInstallerFile(LocalDirectory, LocalInstallerFileName,
      FExecuteVersionInfo, FThread.LastThread);
    if (FExecuteDownloadInstallFileName <> '') and
      not FileExists(FExecuteDownloadInstallFileName) then
      FExecuteDownloadInstallFileName := '';
  end;
end;

procedure TJvProgramVersionCheck.DownloadThreadOnFinishAll(Sender: TObject);
begin
  if FDownloadError <> '' then
    MessageDlg(FDownloadError, mtError, [mbOK], 0)
  else
  if FExecuteDownloadInstallFileName = '' then
    MessageDlg(RsPVCFileDownloadNotSuccessful, mtError, [mbOK], 0)
  else
  if FExecuteOperation = rvoCopy then
    MessageDlg(Format(RsPVCDownloadSuccessfulInstallManually,
      [FExecuteDownloadInstallFileName]), mtInformation, [mbOK], 0)
  else
  if MessageDlg(RsPVCDownloadSuccessfullInstallNow,
    mtWarning, [mbYes, mbNo], 0) = mrYes then
    if ShellExecEx(FExecuteDownloadInstallFileName) then
      Application.Terminate
    else
      MessageDlg(RsPVCErrorStartingSetup, mtError, [mbOK], 0);
end;

procedure TJvProgramVersionCheck.Execute;
var
  ReleaseType: TJvProgramReleaseType;
begin
  FExecuteVersionInfo := nil;
  LoadProperties;
  if (LastCheck < Now - CheckFrequency) and (LocationTypesSupported <> []) then
  begin
    LastCheck := Now;
    if not DirectoryExists(LocalDirectory) then
      if not ForceDirectories(LocalDirectory) then
        LocalDirectory := '';
    RemoteAppStorage.FileName :=
      LoadRemoteVersionInfoFile(LocalDirectory, LocalVersionInfoFileName);
    if RemoteAppStorage.FileName <> '' then
    begin
      RemoteProgramVersionHistory.LoadProperties;
      StoreProperties;
      StoreRemoteVersionInfoToFile;
      if IsRemoteProgramVersionNewer then
      begin
        FExecuteOperation := GetRemoteVersionOperation(ReleaseType);
        FExecuteVersionInfo :=
          RemoteProgramVersionHistory.CurrentProgramVersion[ReleaseType];
        if FExecuteOperation in [rvoCopy, rvoCopyInstall] then
          DownloadInstallerFromRemote;
      end;
    end;
  end;
end;

function TJvProgramVersionCheck.GetAllowedRemoteProgramVersion: string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType).ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionCheck.GetAllowedRemoteProgramVersionReleaseType: string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType).ProgramVersionReleaseType
  else
    Result := '';
end;

function TJvProgramVersionCheck.GetLocationTypesSupported: TJvProgramVersionLocationTypes;
begin
  Result := [];
  if Assigned(FLocationNetwork) then
    Result := Result + [pvltNetWork];
  if Assigned(FLocationDatabase) then
    Result := Result + [pvltDatabase];
  if Assigned(FLocationHTTP) then
    Result := Result + [pvltHTTP];
  if Assigned(FLocationFTP) then
    Result := Result + [pvltFTP];
end;

function TJvProgramVersionCheck.GetRemoteVersionOperation(
  var ReleaseType: TJvProgramReleaseType): TJvRemoteVersionOperation;
var
  ParameterList: TJvParameterList;
  GroupParameter: TJvGroupBoxParameter;
  Parameter: TJvBaseParameter;
  I: TJvProgramReleaseType;
begin
  Result := rvoIgnore;
  ParameterList := TJvParameterList.Create(Self);
  try
    ParameterList.MaxWidth := 460;
    ParameterList.Messages.Caption :=
      Format(RsPVCDialogCaption, [CurrentApplicationName]);
    ParameterList.Messages.OkButton := RsPVCDialogExecuteButton;
    Parameter := TJvBaseParameter(TJvLabelParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'New Version Label';
      Caption := Format(RsPVCNewVersionAvailable,
        [GetAllowedRemoteProgramVersionReleaseType, CurrentApplicationName]);
      Width := 350;
    end;
    ParameterList.AddParameter(Parameter);
    GroupParameter := TJvGroupBoxParameter.Create(ParameterList);
    with GroupParameter do
    begin
      SearchName := 'GroupBox';
      Caption := RsPVCChooseWhichVersion;
      Width := 350;
      Height := 10;
    end;
    ParameterList.AddParameter(GroupParameter);
    for I := High(I) downto Low(I) do
      if (I <= AllowedReleaseType) and
        Assigned(RemoteProgramVersionHistory.CurrentProgramVersion[I]) then
        if CompareVersionNumbers(CurrentFileVersion,
          RemoteProgramVersionHistory.CurrentProgramVersion[I].ProgramVersion) > 0 then
        begin
          Parameter := TJvBaseParameter(TJvRadioButtonParameter.Create(ParameterList));
          with Parameter do
          begin
            ParentParameterName := 'GroupBox';
            SearchName := 'RadioButton' + IntToStr(Ord(I));
            Caption := RemoteProgramVersionHistory.CurrentProgramVersion[I].ProgramVersionInfo;
            Width := 250;
            AsBoolean := GroupParameter.Height <= 10;
          end;
          ParameterList.AddParameter(Parameter);
          Parameter := TJvBaseParameter(TJvButtonParameter.Create(ParameterList));
          with TJvButtonParameter(Parameter) do
          begin
            ParentParameterName := 'GroupBox';
            SearchName := 'VersionButtonInfo' + IntToStr(Ord(I));
            Caption := 'Info';
            Width := 80;
            Tag := Ord(I);
            OnClick := VersionInfoButtonClick;
          end;
          ParameterList.AddParameter(Parameter);
          GroupParameter.Height := GroupParameter.Height + 25;
        end;
    Parameter := TJvBaseParameter(TJvRadioGroupParameter.Create(ParameterList));
    with TJvRadioGroupParameter(Parameter) do
    begin
      SearchName := 'Operation';
      Caption := RsPVCChooseOperation;
      ItemList.Add(RsPVCOperationIgnore);
      ItemList.Add(RsPVCOperationDownloadOnly);
      ItemList.Add(RsPVCOperationDownloadInstall);
      ItemIndex := 2;
      Width := 350;
      Height := 79;
    end;
    ParameterList.AddParameter(Parameter);
    if ParameterList.ShowParameterDialog then
    begin
      case TJvRadioGroupParameter(ParameterList.ParameterByName('Operation')).ItemIndex of
        0:
          Result := rvoIgnore;
        1:
          Result := rvoCopy;
        2:
          Result := rvoCopyInstall;
      end;
      ReleaseType := prtProduction;
      for I := High(I) downto Low(I) do
        if IsRemoteProgramVersionReleaseTypeNewer(I) then
          if Assigned(ParameterList.ParameterByName('RadioButton' + IntToStr(Ord(I)))) then
            if ParameterList.ParameterByName('RadioButton' + IntToStr(Ord(I))).AsBoolean then
            begin
              ReleaseType := I;
              Break;
            end;
    end;
  finally
    ParameterList.Free;
  end;
end;

function TJvProgramVersionCheck.IsRemoteProgramVersionNewer: Boolean;
begin
  Result := CompareVersionNumbers(CurrentFileVersion, GetAllowedRemoteProgramVersion) = 1;
end;

function TJvProgramVersionCheck.IsRemoteProgramVersionReleaseTypeNewer(iReleaseType: TJvProgramReleaseType): Boolean;
begin
  if Assigned(RemoteProgramVersionHistory.CurrentProgramVersion[iReleaseType]) then
    Result := CompareVersionNumbers(CurrentFileVersion,
      RemoteProgramVersionHistory.CurrentProgramVersion[iReleaseType].ProgramVersion) = 1
  else
    Result := False;
end;

procedure TJvProgramVersionCheck.LoadData;
begin
  inherited LoadData;
  LastCheck := AppStorage.ReadDateTime(AppStorage.ConcatPaths([AppStoragePath, cLastCheck]), LastCheck);
end;

function TJvProgramVersionCheck.LoadRemoteInstallerFile(const iLocalDirectory, iLocalInstallerFileName: string;
  iProgramVersionInfo: TJvProgramVersionInfo; iBaseThread: TJvBaseThread): string;
begin
  if Assigned(iProgramVersionInfo) and (SelectedLocation <> nil) then
  begin
    Result := SelectedLocation.LoadInstallerFileFromRemote(iProgramVersionInfo.ProgramLocationPath,
      iProgramVersionInfo.ProgramLocationFileName, iLocalDirectory, iLocalInstallerFileName, iBaseThread);
    FDownloadError := SelectedLocation.DownloadError;
  end
  else
    Result := '';
end;

function TJvProgramVersionCheck.LoadRemoteVersionInfoFile(
  const iLocalDirectory, iLocalVersionInfoFileName: string): string;
begin
  if SelectedLocation <> nil then
    Result := SelectedLocation.LoadVersionInfoFromRemote(iLocalDirectory, iLocalVersionInfoFileName, nil)
  else
    Result := '';
end;

procedure TJvProgramVersionCheck.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
    if AComponent = FLocationNetwork then
      FLocationNetwork := nil
    else
    if AComponent = FLocationDatabase then
      FLocationDatabase := nil
    else
    if AComponent = FLocationHTTP then
      FLocationHTTP := nil
    else
    if AComponent = FLocationFTP then
      FLocationFTP := nil
end;

function TJvProgramVersionCheck.SelectedLocation: TJvProgramVersionCustomLocation;
begin
  case LocationType of
    pvltDatabase:
      Result := LocationDatabase;
    pvltHTTP:
      Result := LocationHTTP;
    pvltFTP:
      Result := LocationFTP;
    pvltNetwork:
      Result := LocationNetwork;
  else
    Result := nil;
  end
end;

procedure TJvProgramVersionCheck.SetThreadInfo(const Info: string);
begin
  if Assigned(FThreadDialog) then
    FThreadDialog.DialogOptions.InfoText := Info;
end;

procedure TJvProgramVersionCheck.SetUserOptions(Value: TJvProgramVersionUserOptions);
begin
  FUserOptions := Value;
  IgnoreProperties.AddDelete('CheckFrequency', (uoCheckFrequency in Value));
  IgnoreProperties.AddDelete('LocalDirectory', (uoLocalDirectory in Value));
  IgnoreProperties.AddDelete('AllowedReleaseType', (uoAllowedReleaseType in Value));
  IgnoreProperties.AddDelete('LocationType', (uoLocationType in Value));
  IgnoreProperties.AddDelete('LocationNetwork', (uoLocationNetwork in Value));
  IgnoreProperties.AddDelete('LocationHTTP', (uoLocationHTTP in Value));
  IgnoreProperties.AddDelete('LocationFTP', (uoLocationFTP in Value));
  IgnoreProperties.AddDelete('LocationDatabase', (uoLocationDatabase in Value));
end;

procedure TJvProgramVersionCheck.ShowProgramVersionsDescription(const iFromVersion, iToVersion: string);
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(Self);
  try
    ParameterList.Messages.Caption := Format(RsPVCWhatNewInS, [CurrentApplicationName]);
    ParameterList.CancelButtonVisible := False;
    Parameter := TJvMemoParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Memo';
      Caption := Format(RsPVCChangesBetween, [iFromVersion, iToVersion]);
      Width := 340;
      Height := 200;
      AsString := RemoteProgramVersionHistory.GetVersionsDescription(iFromVersion, iToVersion);
    end;
    ParameterList.AddParameter(Parameter);
    ParameterList.ShowParameterDialog
  finally
    ParameterList.Free;
  end;
end;

procedure TJvProgramVersionCheck.StoreData;
begin
  inherited StoreData;
  AppStorage.WriteDateTime(AppStorage.ConcatPaths([AppStoragePath, cLastCheck]), LastCheck);
end;

procedure TJvProgramVersionCheck.StoreRemoteVersionInfoToFile;
begin
  FRemoteAppStorage.ReadOnly := False;
  RemoteProgramVersionHistory.StoreProperties;
  FRemoteAppStorage.Flush;
  FRemoteAppStorage.ReadOnly := True;
end;

procedure TJvProgramVersionCheck.VersionInfoButtonClick(const ParameterList: TJvParameterList;
  const Parameter: TJvBaseParameter);
var
  I: TJvProgramReleaseType;
begin
  I := Low(I);
  Inc(I, Parameter.Tag);
  with RemoteProgramVersionHistory do
    if Assigned(CurrentProgramVersion[I]) then
      ShowProgramVersionsDescription(CurrentFileVersion, CurrentProgramVersion[I].ProgramVersion);
end;

{$IFDEF USE_3RDPARTY_INDY}

//=== { TJvProgramVersionHTTPLocationIndy } ==================================

constructor TJvProgramVersionHTTPLocationIndy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIdHTTP := TIdHTTP.Create(Self);
end;

destructor TJvProgramVersionHTTPLocationIndy.Destroy;
begin
  FIdHttp.Free;
  inherited Destroy;
end;

function TJvProgramVersionHTTPLocationIndy.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName)
  else
    Result := LoadFileFromRemoteIndy(iRemotePath, iRemoteFileName, iLocalPath,
      iLocalFileName, iBaseThread);
end;

function TJvProgramVersionHTTPLocationIndy.LoadFileFromRemoteIndy(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
var
  ResultStream: TFileStream;
  ResultName: string;
begin
  Result := '';
  if (DirectoryExists(iLocalPath) or (iLocalPath = '')) then
    if iLocalFileName = '' then
      ResultName := PathAppend(iLocalPath, iRemoteFileName)
    else
      ResultName := PathAppend(iLocalPath, iLocalFileName)
  else
    Exit;

  ResultStream := TFileStream.Create(ResultName, fmCreate);
  try
    FIdHTTP.Port := Port;
    with FIdHTTP do
    begin
      ProxyParams.ProxyPort := ProxySettings.Port;
      ProxyParams.ProxyServer := ProxySettings.Server;
      ProxyParams.ProxyUsername := ProxySettings.UserName;
      ProxyParams.ProxyPassword := ProxySettings.Password;
      if UserName <> '' then
        Request.UserName := UserName;
      if Password <> '' then
        Request.Password := Password;
      try
        if Copy(iRemotePath, Length(iRemotePath), 1) <> '/' then
          Get(iRemotePath + '/' + iRemoteFileName, ResultStream)
        else
          Get(iRemotePath + iRemoteFileName, ResultStream);
      except
        on E: Exception do
          DownloadError := E.Message;
      end;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists(ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

//=== { TJvProgramVersionFTPLocationIndy } ===================================

constructor TJvProgramVersionFTPLocationIndy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIdFTP := TIdFTP.Create(Self);
end;

destructor TJvProgramVersionFTPLocationIndy.Destroy;
begin
  FIdFtp.Free;
  inherited Destroy;
end;

function TJvProgramVersionFTPLocationIndy.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName)
  else
    Result := LoadFileFromRemoteIndy(iRemotePath, iRemoteFileName, iLocalPath,
      iLocalFileName, iBaseThread);
end;

function TJvProgramVersionFTPLocationIndy.LoadFileFromRemoteIndy(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
var
  ResultStream: TFileStream;
  ResultName: string;
begin
  Result := '';
  if (DirectoryExists(iLocalPath) or (iLocalPath = '')) then
    if iLocalFileName = '' then
      ResultName := PathAppend(iLocalPath, iRemoteFileName)
    else
      ResultName := PathAppend(iLocalPath, iLocalFileName)
  else
    Exit;

  ResultStream := TFileStream.Create(ResultName, fmCreate);
  try
    FIdFTP.Port := Port;
    FIdFTP.ProxySettings.Port := ProxySettings.Port;
    FIdFTP.ProxySettings.Host := ProxySettings.Server;
    FIdFTP.ProxySettings.UserName := ProxySettings.UserName;
    FIdFTP.ProxySettings.Password := ProxySettings.Password;
    with FIdFTP do
    begin
      try
        if Copy(iRemotePath, Length(iRemotePath), 1) <> '/' then
          Get(iRemotePath + '/' + iRemoteFileName, ResultStream)
        else
          Get(iRemotePath + iRemoteFileName, ResultStream);
      except
        on E: Exception do
          DownloadError := E.Message;
      end;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists(ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

{$ENDIF USE_3RDPARTY_INDY}

{$IFDEF USE_3RDPARTY_ICS}

//=== { TJvProgramVersionHTTPLocationIcs } ===================================

constructor TJvProgramVersionHTTPLocationIcs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpCli := THttpCli.Create(Self);
end;

destructor TJvProgramVersionHTTPLocationIcs.Destroy;
begin
  FHttpCli.Free;
  inherited Destroy;
end;

function TJvProgramVersionHTTPLocationIcs.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName)
  else
    Result := LoadFileFromRemoteIcs(iRemotePath, iRemoteFileName, iLocalPath,
      iLocalFileName, iBaseThread);
end;

function TJvProgramVersionHTTPLocationIcs.LoadFileFromRemoteIcs(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
var
  ResultStream: TFileStream;
  ResultName: string;
begin
  Result := '';
  if (DirectoryExists(iLocalPath) or (iLocalPath = '')) then
    if iLocalFileName = '' then
      ResultName := PathAppend(iLocalPath, iRemoteFileName)
    else
      ResultName := PathAppend(iLocalPath, iLocalFileName)
  else
    Exit;

  ResultStream := TFileStream.Create(ResultName, fmCreate);
  try
    //FHttpCli.Port := Port;
    with FHttpCli do
    begin
      MultiThreaded := False;
      ProxyPort := inttostr(ProxySettings.Port);
      Proxy := ProxySettings.Server;
      ProxyUsername := ProxySettings.UserName;
      ProxyPassword := ProxySettings.Password;
      RcvdStream := ResultStream;
      if Copy(iRemotePath, Length(iRemotePath), 1) <> '/' then
        Url := iRemotePath + '/' + iRemoteFileName
      else
        Url := iRemotePath + iRemoteFileName;
      try
        Get
      except
        on E: EHttpException do
          DownloadError := 'Failed : ' + IntToStr(StatusCode) + ' ' + ReasonPhrase;
        else
          raise;
      end;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists(ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

//=== { TJvProgramVersionFTPLocationIcs } ====================================

constructor TJvProgramVersionFTPLocationIcs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFtpClient := TFtpClient.Create(Self);
end;

destructor TJvProgramVersionFTPLocationIcs.Destroy;
begin
  FFtpClient.Free;
  inherited Destroy;
end;

function TJvProgramVersionFTPLocationIcs.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName)
  else
    Result := LoadFileFromRemoteIcs(iRemotePath, iRemoteFileName, iLocalPath,
      iLocalFileName, iBaseThread);
end;

function TJvProgramVersionFTPLocationIcs.LoadFileFromRemoteIcs(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
var
  ResultStream: TFileStream;
  ResultName: string;
  P: Integer;
begin
  Result := '';
  if (DirectoryExists(iLocalPath) or (iLocalPath = '')) then
    if iLocalFileName = '' then
      ResultName := PathAppend(iLocalPath, iRemoteFileName)
    else
      ResultName := PathAppend(iLocalPath, iLocalFileName)
  else
    Exit;

  ResultStream := TFileStream.Create(ResultName, fmCreate);
  try
  //  FFtpClient.Port := inttostr(Port);
    FFtpClient.DataPortRangeStart := Port;
    FFtpClient.DataPortRangeEnd := Port;
    FFtpClient.UserName := UserName;
    FFtpClient.Password := Password;
    with FFtpClient do
    begin
//    FtpClient1.HostName           := HostNameEdit.Text;
//    FtpClient1.Port               := PortEdit.Text;
//    FtpClient1.DataPortRangeStart := StrToInt(Trim(DataPortRangeStartEdit.Text));
//    FtpClient1.DataPortRangeEnd   := Port;
//    FtpClient1.UserName           := UserNameEdit.Text;
//    FtpClient1.Password           := PasswordEdit.Text;
//    FtpClient1.Account            := AccountEdit.Text;
//    FtpClient1.HostDirName        := HostDirEdit.Text;
//    FtpClient1.HostFileName       := HostFileEdit.Text;
//    FtpClient1.LocalFileName      := LocalFileEdit.Text;
//    FtpClient1.Passive            := PassiveCheckBox.Checked;
//    FtpClient1.Binary             := BinaryCheckBox.Checked;
      MultiThreaded := False;
      Binary := True;
      ProxyPort := IntToStr(ProxySettings.Port);
      ProxyServer := ProxySettings.Server;
//      ProxyUsername := ProxySettings.UserName;
//      ProxyPassword := ProxySettings.Password;
//      Port := 'ftp';
//      RcvdStream := ResultStream;
      LocalFileName := ResultName;
      P := Pos('://', iRemotePath);
      if P > 0 then
      begin
        HostName := Copy(iRemotePath, P + 3, Length(iRemotePath) - P - 2);
        P := Pos('/', HostName);
        HostDirName := Copy(HostName, P + 1, Length(HostName) - P);
        HostName := Copy(HostName, 1, P - 1);
      end
      else
      begin
        P := Pos('/', iRemotePath);
        HostName := Copy(iRemotePath, 1, P - 1);
        HostDirName := Copy(iRemotePath, P + 1, Length(iRemotePath) - P);
      end;
      if Copy(HostDirName, Length(HostDirName), 1) = '/' then
        HostDirName := Copy(HostDirName, 1, Length(HostDirName) - 1);
      if HostDirName = '' then
        HostDirName := '/';
      if Copy(HostDirName, 1, 1) <> '/' then
        HostDirName := '/' + HostDirName;
      HostFileName := iRemoteFileName;
      try
        try
          if not Open then
          begin
            DownloadError := 'Failed: Unable to connect to ' + HostName;
            Exit;
          end;
          if not Get then
          begin
            DownloadError := 'Failed: Unable to get ' + HostDirName + '/' + HostFileName;
            Exit;
          end;
        except
          on E: Exception do
            DownloadError := 'Failed: ' + E.Message;
        else
          raise;
        end;
      finally
        if Connected then
          Quit;
      end;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists(ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

{$ENDIF USE_3RDPARTY_ICS}

//=== { TJvProgramVersionProxySettings } =====================================

constructor TJvProgramVersionProxySettings.Create;
begin
  inherited Create;
  FPort := 80;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

