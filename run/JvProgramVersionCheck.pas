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

uses Classes,
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
  TJvProgramVersionInfo = class(tJvCustomPropertyStore)
  private
    FDownloadPasswordRequired: Boolean;
    FVersionDescription: TStringList;
    FProgramSize:    integer;
    FProgramVersion: string;
    FProgramLocationPath: string;
    FProgramLocationFileName: string;
    FProgramReleaseType: TJvProgramReleaseType;
    FProgramReleaseDate: tDateTime;
  protected
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
        FDownloadPasswordRequired default false;
    { Path where the installer of the version could be found. This could be
    a absolute path or a relative path to the location of the version list file }
    property ProgramLocationPath: string Read FProgramLocationPath
      Write FProgramLocationPath;
    { File name of the installer file }
    property ProgramLocationFileName: string
      Read FProgramLocationFileName Write FProgramLocationFileName;
    { Program version in the format <main>.<sub>.<release>.<build>
    This property is compared with the fileversion properties of the current
    application. }
    property ProgramVersion: string Read FProgramVersion Write FProgramVersion;
    { This is a description field which could be shown in the update dialog via
      the version info button }
    property VersionDescription: TStringList
      Read FVersionDescription Write FVersionDescription;
    { Release type of the version.
    In the update dialog there are only the highest version numbers for each type
    visible. The type must be higher then AllowedReleaseType property of the
    TJvProgramVersionCheck component }
    property ProgramReleaseType: TJvProgramReleaseType
      Read FProgramReleaseType Write FProgramReleaseType;
    { Size of the installer in bytes }
    property ProgramSize: integer Read FProgramSize Write FProgramSize;
    { Date of Release }
    property ProgramReleaseDate: tDateTime Read FProgramReleaseDate
      Write FProgramReleaseDate;
  end;

  TJvProgramVersionInfoReleaseArray = array[TJvProgramReleaseType] of
    TJvProgramVersionInfo;

  { List of all Program version stored in a remote file via TJvAppStorage }
  TJvProgramVersionHistory = class(TJvCustomPropertyListStore)
  private
    FCurrentProductionVersion: string;
    FCurrentBetaVersion:    string;
    FCurrentAlphaVersion:   string;
    FCurrentProgramVersion: TJvProgramVersionInfoReleaseArray;
  protected
    function CreateObject: TObject; override;
    function CreateItemList: TStringList; override;
    function GetProgramVersion(Index: integer): TJvProgramVersionInfo;
    function GetCurrentProgramVersion(Index: TJvProgramReleaseType)
      : TJvProgramVersionInfo;
    function SearchCurrentProgramVersion(iProgramReleaseType: TJvProgramReleaseType)
      : TJvProgramVersionInfo;
    property ProgramVersion[Index: integer]: TJvProgramVersionInfo
      Read GetProgramVersion;
    function GetCurrentProductionProgramVersion: string;
    function GetCurrentBetaProgramVersion: string;
    function GetCurrentAlphaProgramVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadData; override;
    procedure RecalculateCurrentProgramVersions;
    property CurrentProgramVersion[Index: TJvProgramReleaseType]
      : TJvProgramVersionInfo Read GetCurrentProgramVersion;
    function AllowedCurrentProgramVersion(iAllowedReleaseType: TJvProgramReleaseType)
      : TJvProgramVersionInfo;
    function GetVersionsDescription(const iFromVersion, iToVersion: string): string;
  published
    property CurrentProductionProgramVersion: string
      Read GetCurrentProductionProgramVersion Write FCurrentProductionVersion;
    property CurrentBetaProgramVersion: string
      Read GetCurrentBetaProgramVersion Write FCurrentBetaVersion;
    property CurrentAlphaProgramVersion: string
      Read GetCurrentAlphaProgramVersion Write FCurrentAlphaVersion;
  end;

  { Base class for all location
  A Location is the class which defines where the remote files could be found and
  manages all communications to these files.
  }
  TJvProgramVersionCustomLocation = class(tJvCustomPropertyStore)
  private
    FDownloadError: string;
    FDownloadStatus: string;
    FDownloadThreaded: boolean;
  protected
    procedure SetDownloadStatus(Value: string);
    function LoadFileFromRemoteInt(
      const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function LoadFileFromRemote(const iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName: string; iBaseThread: TJvBaseThread): string; virtual;
    function LoadInstallerFileFromRemote(const iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName: string; iBaseThread: TJvBaseThread): string; virtual;
    function LoadVersionInfoFromRemote(const iLocalDirectory,
      iLocalVersionInfoFileName: string; iBaseThread: TJvBaseThread): string; virtual;
    property DownloadStatus: string Read FDownloadStatus Write FDownloadStatus;
    property DownloadError: string Read FDownloadError Write FDownloadError;
  published
    property DownloadThreaded: boolean Read FDownloadThreaded
      Write FDownloadThreaded default False;
  end;

  { Base class for all file based Locations like Network, FTP and HTTP }
  TJvProgramVersionCustomFileBasedLocation = class(TJvProgramVersionCustomLocation)
  private
    FVersionInfoLocationPathList: TStringList;
    FVersionInfoFilename: string;
    FValidLocationPath: string;
  protected
    procedure SetVersionInfoLocationPathList(Value: TStringList);
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
    property VersionInfoLocationPathList: TStringList
      Read FVersionInfoLocationPathList Write SetVersionInfoLocationPathList;
    { Name of the VersionInfofile at the remote location }
    property VersionInfoFilename: string Read FVersionInfoFilename
      Write FVersionInfoFilename;
  end;

  { Location Class for Local Network Location }
  TJvProgramVersionNetworkLocation = class(TJvProgramVersionCustomFileBasedLocation)
  private
  protected
    function LoadFileFromRemoteInt(
      const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
  public
  published
  end;

  { Class for Proxy Settings for FTP and HTTP locations }
  TJvProgramVersionProxySettings = class(TPersistent)
  private
    FServer:   string;
    FPort: integer;
    FUsername: string;
    FPassword: string;
  public
    constructor Create;
  published
    property Server: string Read FServer Write FServer;
    property Port: integer read FPort write FPort default 80;
    property Username: string Read FUsername Write FUsername;
    property Password: string Read FPassword Write FPassword;
  end;


  { Base class for all Internet locations  }
  TJvProgramVersionInternetLocation = class(TJvProgramVersionCustomFileBasedLocation)
  private
    FProxySettings: TJvProgramVersionProxySettings;
    FPasswordRequired: boolean;
    FUsername: string;
    FPassword: string;
    FPort: integer;
  protected
    property ProxySettings: TJvProgramVersionProxySettings Read FProxySettings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property PasswordRequired: boolean Read FPasswordRequired Write FPasswordRequired default False;
    property Port: integer read FPort write FPort default 80;
  end;

  TJvProgramVersionHTTPLocation  = class;
  TJvLoadFileFromRemoteHTTPEvent = function(iProgramVersionLocation:
    TJvProgramVersionHTTPLocation;
    const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string): string of
    object;

  { Simple HTTP location class with no http logic.
  The logic must be implemented manually in the OnLoadFileFromRemote event }
  TJvProgramVersionHTTPLocation = class(TJvProgramVersionInternetLocation)
  private
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteHTTPEvent;
  protected
    function LoadFileFromRemoteInt(
      const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
  public
  published
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteHTTPEvent
      Read FOnLoadFileFromRemote Write FOnLoadFileFromRemote;
    property ProxySettings;
  end;

  {$IFDEF USE_3RDPARTY_INDY}
  TJvProgramVersionHTTPLocationIndy = class(TJvProgramVersionHTTPLocation)
  private
    FIdHttp: TIdHttp;
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath,
        iLocalFileName: string; iBaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIndy(const iRemotePath, iRemoteFileName, iLocalPath,
        iLocalFileName: string; iBaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner : TComponent); override;
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
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath,
        iLocalFileName: string; iBaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIcs(const iRemotePath, iRemoteFileName, iLocalPath,
        iLocalFileName: string; iBaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property ProxySettings;
  end;
  {$ENDIF USE_3RDPARTY_ICS}


  TJvProgramVersionFTPLocation  = class;
  TJvLoadFileFromRemoteFTPEvent = function(iProgramVersionLocation:
    TJvProgramVersionFTPLocation;
    const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string): string of
    object;

  { Simple FTP location class with no http logic.
  The logic must be implemented manually in the OnLoadFileFromRemote event }
  TJvProgramVersionFTPLocation = class(TJvProgramVersionInternetLocation)
  private
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteFTPEvent;
  protected
    function LoadFileFromRemoteInt(
      const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
  public
  published
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteFTPEvent
      Read FOnLoadFileFromRemote Write FOnLoadFileFromRemote;
    property ProxySettings;
  end;

  {$IFDEF USE_3RDPARTY_INDY}
  TJvProgramVersionFTPLocationIndy = class(TJvProgramVersionFTPLocation)
  private
    FIdFtp: TIdFtp;
  protected
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath,
        iLocalFileName: string; iBaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIndy(const iRemotePath, iRemoteFileName, iLocalPath,
        iLocalFileName: string; iBaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner : TComponent); override;
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
    function LoadFileFromRemoteInt(const iRemotePath, iRemoteFileName, iLocalPath,
        iLocalFileName: string; iBaseThread: TJvBaseThread): string; override;
    function LoadFileFromRemoteIcs(const iRemotePath, iRemoteFileName, iLocalPath,
        iLocalFileName: string; iBaseThread: TJvBaseThread): string;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property ProxySettings;
  end;
  {$ENDIF USE_3RDPARTY_ICS}

  TJvProgramVersionDatabaseLocation  = class;
  TJvLoadFileFromRemoteDatabaseEvent =
    function(iProgramVersionLocation: TJvProgramVersionDatabaseLocation;
    const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string): string of
    object;

  { Simple Database location class with no http logic.
  The logic must be implemented manually in the OnLoadFileFromRemote event }

  TJvProgramVersionDatabaseLocation = class(TJvProgramVersionCustomLocation)
  private
    FServerName: string;
    FUsername:   string;
    FPasswort:   string;
    FSelectStatementVersion: string;
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteDatabaseEvent;
  protected
    function LoadFileFromRemoteInt(
      const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
      iBaseThread: TJvBaseThread): string; override;
  public
    function LoadVersionInfoFromRemote(const iLocalDirectory,
      iLocalVersionInfoFileName: string; iBaseThread: TJvBaseThread): string; override;
  published
    property ServerName: string Read FServerName Write FServerName;
    property Username: string Read FUsername Write FUsername;
    property Passwort: string Read FPasswort Write FPasswort;
    property SelectStatementVersion: string
      Read FSelectStatementVersion Write FSelectStatementVersion;
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteDatabaseEvent
      Read FOnLoadFileFromRemote Write FOnLoadFileFromRemote;
  end;

  { Location Type for the TjvProgramVersionCheck class}
  TJvProgramVersionLocationType  = (pvltNetwork, pvltDatabase,
    pvltFTP, pvltHTTP);
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

  TJvProgramVersionCheck = class(tJvCustomPropertyStore)
  private
    FAllowedReleaseType: TJvProgramReleaseType;
    fCheckFrequency: integer;
    FDownloadError: string;
    fExecuteDownloadInstallFilename: string;
    fExecuteOperation: TJvRemoteVersionOperation;
    fExecuteVersionInfo: TJvProgramVersionInfo;
    fLastCheck:   tDateTime;
    fLocalDirectory: string;
    fLocalInstallerFileName: string;
    fLocalVersionInfoFileName: string;
    FLocationDatabase: TJvProgramVersionDatabaseLocation;
    FLocationFTP: TJvProgramVersionFTPLocation;
    FLocationHTTP: TJvProgramVersionHTTPLocation;
    FLocationNetwork: TJvProgramVersionNetworkLocation;
    FLocationType: TJvProgramVersionLocationType;
    FRemoteAppStorage: TJvAppIniFileStorage;
    FRemoteProgramVersionHistory: TJvProgramVersionHistory;
    FThread:      TJvThread;
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
    function IsRemoteProgramVersionNewer: boolean;
    function IsRemoteProgramVersionReleaseTypeNewer(iReleaseType:
      TJvProgramReleaseType): boolean;
    procedure LoadData; override;
    procedure Loaded; override;
    function LoadRemoteInstallerFile(const iLocalDirectory,
      iLocalInstallerFileName: string; iProgramVersionInfo: TJvProgramVersionInfo;
      iBaseThread: TJvBaseThread): string;
    function LoadRemoteVersionInfoFile(const iLocalDirectory,
      iLocalVersionInfoFileName: string): string;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetThreadInfo(const Info: string);
    procedure SetUserOptions (Value: TJvProgramVersionUserOptions);
    procedure StoreData; override;
    procedure StoreRemoteVersionInfoToFile;
    procedure VersionInfoButtonClick(const ParameterList: TJvParameterList;
      const Parameter: TJvBaseParameter);
    property RemoteAppStorage: TJvAppIniFileStorage Read FRemoteAppStorage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DownloadInstallerFromRemote;
    procedure Execute;
    function GetRemoteVersionOperation(var ReleaseType: TJvProgramReleaseType):
      TJvRemoteVersionOperation;
    function SelectedLocation: TJvProgramVersionCustomLocation;
    procedure ShowProgramVersionsDescription(const iFromVersion, iToVersion: string);
    property LastCheck: tDateTime Read fLastCheck Write fLastCheck;
    property LocationTypesSupported: TJvProgramVersionLocationTypes
      Read GetLocationTypesSupported;
    property RemoteProgramVersionHistory: TJvProgramVersionHistory
      Read FRemoteProgramVersionHistory Write FRemoteProgramVersionHistory;
  published
    property AboutJVCL;
    { Defines which release types will be shown in the update dialog }
    property AllowedReleaseType: TJvProgramReleaseType
      Read FAllowedReleaseType Write FAllowedReleaseType default prtProduction;
    property AppStorage;
    property AppStoragePath;
    { Defines how often the check for a new version is executed (in days) }
    property CheckFrequency: integer Read fCheckFrequency Write fCheckFrequency;
    { Defines the local directory where the remote files where stored }
    property LocalDirectory: string Read fLocalDirectory Write fLocalDirectory;
    { Defines the local name of the program installer. If it is empty the name
      of the remote file is used }
    property LocalInstallerFileName: string
      Read fLocalInstallerFileName Write fLocalInstallerFileName;
    { Defines the name of the local version info file. If it is empty the name
      of the remote file is used }
    property LocalVersionInfoFileName: string
      Read fLocalVersionInfoFileName Write fLocalVersionInfoFileName;
    { Database Location }
    property LocationDatabase: TJvProgramVersionDatabaseLocation
      Read FLocationDatabase Write FLocationDatabase;
    { FTP Location }
    property LocationFTP: TJvProgramVersionFTPLocation
      Read FLocationFTP Write FLocationFTP;
    { HTTP Location }
    property LocationHTTP: TJvProgramVersionHTTPLocation
      Read FLocationHTTP Write FLocationHTTP;
    { Network Location }
    property LocationNetwork: TJvProgramVersionNetworkLocation
      Read FLocationNetwork Write FLocationNetwork;
    { Defines location which is used for the version check,
    only assigned locations are supported }
    property LocationType: TJvProgramVersionLocationType
      Read FLocationType Write FLocationType;
    {Defines which options of the component are stored/restored wia AppStorage }
    property UserOptions: TJvProgramVersionUserOptions read FUserOptions write SetUserOptions default [uoCheckFrequency, uoLocalDirectory,
      uoAllowedReleaseType, uoLocationType, uoLocationNetwork, uoLocationHTTP, uoLocationFTP, uoLocationDatabase];
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
  JvDSADialogs,
  JvParameterListParameter;

const
  cProgramVersion = 'Program Version ';
  cLastCheck      = 'LastCheck';

resourcestring
  RsPVCReleaseTypeAlpha = 'Alpha';
  RsPVCReleaseTypeBeta  = 'Beta';
  RsPVCReleaseTypeProduction = 'Produktion';

  RsPVCDownloading     = 'Downloading ...';
  RsPVCDialogCaption   = '%s Upgrade Check';
  RsPVCDialogExecuteButton = '&Execute';
  RsPVCNewVersionAvailable = 'A new Version (%s) of %s is available!';
  RsPVCChooseWhichVersion = 'Which &Version do you want to install?';
  RsPVCChooseOperation = '&Choose the Operation';
  RsPVCOperationIgnore = 'I&gnore';
  RsPVCOperationDownloadOnly = 'Download/Copy &Only';
  RsPVCOperationDownloadInstall = 'Download/Copy and &Install';
  RsPVCWhatNewInS      = 'What'' new in %s';
  RsPVCChangesBetween  = 'Changes between %s and %s';
  RsPVCFileDownloadNotSuccessfull =
    'The File Download was not Successfull!' + #13 + #10 + 'Please try again manually.';
  RsPVCDownloadSuccessfulInstallManually =
    'The file download was successfull.' + #13 + #10 + 'Install manually from : %s';
  RsPVCErrorStartingSetup = 'Error starting the setup process.';
  RsPVCDownloadSuccessfullInstallNow =
    'The file download was successfull.' + #13 + #10 +
    'Do you want to close and install?';



//=== { Common Functions} =========================================

function CompareVersionNumbers(iVersion1, iVersion2: string): integer;
var
  n1, n2: integer;

  function GetNextNumber(var Version: string): integer;
  var
    p: integer;
    s: string;
  begin
    p := Pos('.', Version);
    if p > 0 then
    begin
      s := Copy(Version, 1, p - 1);
      Version := Copy(Version, p + 1, Length(Version) - p);
    end
    else
    begin
      s := Version;
      Version := '';
    end;
    if s = '' then
      Result := -1
    else
      try
        Result := StrToInt(s);
      except
        on e: Exception do
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
    else if n2 < n1 then
    begin
      Result := -1;
      Exit;
    end
  until (iVersion1 = '') or (iVersion2 = '')
end;


//=== { tJvProgramVersionsStringList } =========================================
function VersionNumberSortCompare(List: TStringList; Index1, Index2: integer): integer;
var
  s1, s2: string;
begin
  s1     := TJvProgramVersionInfo(List.Objects[Index1]).ProgramVersion;
  s2     := TJvProgramVersionInfo(List.Objects[Index2]).ProgramVersion;
  Result := CompareVersionNumbers(s1, s2);
end;

procedure TJvProgramVersionsStringList.Sort;
begin
  CustomSort(VersionNumberSortCompare);
end;

//=== { tJvProgramVersionInfo } =========================================

constructor TJvProgramVersionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersionDescription := TStringList.Create;
  IgnoreLastLoadTime  := True;
  FDownloadPasswordRequired := False;
end;

destructor TJvProgramVersionInfo.Destroy;
begin
  FreeAndNil(FVersionDescription);
  inherited Destroy;
end;

procedure TJvProgramVersionInfo.Clear;
begin
  if Assigned(FVersionDescription) then
    FVersionDescription.Clear;
  FProgramVersion     := '';
  FProgramReleaseType := prtProduction;
end;

function TJvProgramVersionInfo.ProgramVersionReleaseType: string;
begin
  case ProgramReleaseType of
      prtBeta: Result  := trim(ProgramVersion + ' ' + RsPVCReleaseTypeBeta);
      prtAlpha: Result := trim(ProgramVersion + ' ' + RsPVCReleaseTypeAlpha);
    else
      Result := trim(ProgramVersion + ' ' + RsPVCReleaseTypeProduction);
  end;
end;

function TJvProgramVersionInfo.ProgramSizeString: string;
begin
  if ProgramSize <= 0 then
    Result := ''
  else if ProgramSize > 1024 * 1024 * 1024 then
    Result := format('%6.2f GB', [ProgramSize / 1024 / 1024 / 1024])
  else if ProgramSize > 1024 * 1024 then
    Result := format('%6.2f MB', [ProgramSize / 1024 / 1024])
  else if ProgramSize > 1024 then
    Result := format('%6.2f KB', [ProgramSize / 1024])
  else
    Result := IntToStr(ProgramSize) + 'B';

end;

function TJvProgramVersionInfo.ProgramVersionInfo: string;
begin
  Result := ProgramVersionReleaseType;
  if (ProgramSize > 0) then
    Result := Result + ' (' + ProgramSizeString + ')';
end;

//=== { tJvProgramVersionHistory } =========================================

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
  prt: TJvProgramReleaseType;
begin
  for prt := low(TJvProgramReleaseType) to High(TJvProgramReleaseType) do
    FCurrentProgramVersion[prt] := SearchCurrentProgramVersion(prt);
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
  prt: TJvProgramReleaseType;
begin
  Result := nil;
  prt    := Low(TJvProgramReleaseType);
  while prt <= iAllowedReleaseType do
  begin
    if Result = nil then
      Result := CurrentProgramVersion[prt]
    else if Assigned(CurrentProgramVersion[prt]) and
      (CompareVersionNumbers(Result.ProgramVersion,
      CurrentProgramVersion[prt].ProgramVersion) = 1) then
      Result := CurrentProgramVersion[prt];
    Inc(prt);
  end;
end;

function TJvProgramVersionHistory.GetProgramVersion(Index: integer):
TJvProgramVersionInfo;
begin
  if Assigned(Objects[Index]) and
    (Objects[Index] is TJvProgramVersionInfo) then
    Result := TJvProgramVersionInfo(Objects[Index])
  else
    Result := nil;
end;

function TJvProgramVersionHistory.SearchCurrentProgramVersion(
  iProgramReleaseType: TJvProgramReleaseType): TJvProgramVersionInfo;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Assigned(ProgramVersion[i]) then
      if ProgramVersion[i].ProgramReleaseType = iProgramReleaseType then
        if Result = nil then
          Result := ProgramVersion[i]
        else if CompareVersionNumbers(Result.ProgramVersion,
          ProgramVersion[i].ProgramVersion) = 1 then
          Result := ProgramVersion[i];
end;

function TJvProgramVersionHistory.GetCurrentProgramVersion(Index:
  TJvProgramReleaseType): TJvProgramVersionInfo;
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

function TJvProgramVersionHistory.GetVersionsDescription(
  const iFromVersion, iToVersion: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    if (CompareVersionNumbers(iFromVersion, ProgramVersion[i].ProgramVersion) >= 0) and
      (CompareVersionNumbers(iToVersion, ProgramVersion[i].ProgramVersion) <= 0) then
    begin
      Result := Result + ProgramVersion[i].ProgramVersionReleaseType;
      if ProgramVersion[i].ProgramReleaseDate > 0 then
        Result := Result + ' - ' + DateTimeToStr(ProgramVersion[i].ProgramReleaseDate);
      if ProgramVersion[i].VersionDescription.Count > 0 then
        Result := Result + #13#10 + ProgramVersion[i].VersionDescription.Text;
      Result := Result + #13#10#13#10;
    end;
end;

//=== { tJvProgramVersionCustomLocation } =========================================

constructor TJvProgramVersionCustomLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDownloadThreaded  := False;
  FDownloadStatus    := '';
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
  TemporaryLocalFileName := LocalFileName+'.temp';
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

function TJvProgramVersionCustomLocation.LoadInstallerFileFromRemote(const iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName: string; iBaseThread: TJvBaseThread): string;
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

//=== { TJvProgramVersionCustomFileBasedLocation } =========================================

constructor TJvProgramVersionCustomFileBasedLocation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersionInfoLocationPathList:= TStringList.Create;
end;

destructor TJvProgramVersionCustomFileBasedLocation.Destroy;
begin
  FreeAndNil(FVersionInfoLocationPathList);
  Inherited Destroy;
end;

procedure TJvProgramVersionCustomFileBasedLocation.SetVersionInfoLocationPathList(Value: TStringList);
begin
  FVersionInfoLocationPathList.Assign(Value);
end;

function TJvProgramVersionCustomFileBasedLocation.LoadVersionInfoFromRemote(
  const iLocalDirectory, iLocalVersionInfoFileName: string;
  iBaseThread: TJvBaseThread): string;
var
  I : Integer;
begin
  for i := 0 to VersionInfoLocationPathList.Count-1 do
  begin
    Result := LoadFileFromRemote(VersionInfoLocationPathList[I], VersionInfoFilename,
      iLocalDirectory, iLocalVersionInfoFileName, iBaseThread);
    if Result <> '' then
    begin
      FValidLocationPath:= VersionInfoLocationPathList[I];
      exit;
    end;
  end;
  if Result = '' then
  begin
    Result := LoadFileFromRemote('', VersionInfoFilename,
      iLocalDirectory, iLocalVersionInfoFileName, iBaseThread);
    if Result <> '' then
      FValidLocationPath:= '';
  end;
end;

function TJvProgramVersionCustomFileBasedLocation.LoadInstallerFileFromRemote(const iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName: string; iBaseThread: TJvBaseThread): string;
begin
  Result := LoadFileFromRemote(iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName, iBaseThread);
  if Result = '' then
    Result := LoadFileFromRemote(ValidLocationPath+iRemotePath, iRemoteFileName,
      iLocalPath, iLocalFileName, iBaseThread);
end;

//=== { tJvProgramVersionNetworkLocation } =========================================

function TJvProgramVersionNetworkLocation.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;

  function FileExistsNoDir(iFilename: string): boolean;
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

//=== { tJvProgramVersionHTTPLocation } =========================================
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

//=== { tJvProgramVersionHTTPLocation } =========================================

function TJvProgramVersionHTTPLocation.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename,
      iLocalPath, iLocalFileName);
end;

//=== { tJvProgramVersionFTPLocation } =========================================

function TJvProgramVersionFTPLocation.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename,
      iLocalPath, iLocalFileName);
end;

//=== { tJvProgramVersionDatabaseLocation } =========================================

function TJvProgramVersionDatabaseLocation.LoadFileFromRemoteInt(
  const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename,
      iLocalPath, iLocalFileName);
end;

function TJvProgramVersionDatabaseLocation.LoadVersionInfoFromRemote(
  const iLocalDirectory, iLocalVersionInfoFileName: string;
  iBaseThread: TJvBaseThread): string;
begin
  Result := LoadFileFromRemote(SelectStatementVersion, '', iLocalDirectory,
    iLocalVersionInfoFileName, iBaseThread);
end;


//=== { tJvProgramVersionCheck } =========================================

constructor TJvProgramVersionCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRemoteProgramVersionHistory := TJvProgramVersionHistory.Create(self);
  FRemoteProgramVersionHistory.IgnoreLastLoadTime := True;
  FRemoteAppStorage := TJvAppIniFileStorage.Create(self);
  FRemoteAppStorage.Location := flCustom;
  FRemoteAppStorage.ReadOnly := True;
  FRemoteAppStorage.AutoReload := True;
  FRemoteAppStorage.DefaultSection := 'Version';
  with FRemoteAppStorage.StorageOptions do
  begin
    SetAsString      := True;
    FloatAsString    := True;
    DefaultIfReadConvertError := True;
    DateTimeAsString := True;
  end;
  FRemoteProgramVersionHistory.AppStorage := FRemoteAppStorage;
  FThread := TJvThread.Create(self);
  FThread.Exclusive := True;
  FThread.RunOnCreate := True;
  FThread.FreeOnTerminate := True;
  FThreadDialog := TJvThreadAnimateDialog.Create(self);
  FThreadDialog.DialogOptions.ShowDialog := True;
  FThreadDialog.DialogOptions.ShowCancelButton := True;
  FThreadDialog.DialogOptions.ShowElapsedTime := True;
  TJvThreadAnimateDialogOptions(FThreadDialog.DialogOptions).commonAvi := aviCopyFile;
  FThread.ThreadDialog := FThreadDialog;

  DeleteBeforeStore  := True;
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('LocalInstallerFileName');
  IgnoreProperties.Add('LocalVersionInfoFileName');
  IgnoreProperties.Add('RemoteAppStorage');
  IgnoreProperties.Add('UserOptions');

  FUserOptions := [uoCheckFrequency, uoLocalDirectory,
    uoAllowedReleaseType, uoLocationType, uoLocationNetwork,
    uoLocationHTTP, uoLocationFTP, uoLocationDatabase];

  //  FLocations:= TJvProgramVersionLocations.Create(self);
  FAllowedReleaseType     := prtProduction;
  fLocalInstallerFileName := '';
  fLocalVersionInfoFileName := 'versioninfo.ini';
  FLocationType   := pvltNetWork;
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
  LocalDirectory := trim(LocalDirectory);
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
      on e: Exception do
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
      on e: Exception do
        Result := '';
    end;
  finally
    FileVersionInfo.Free;
  end;
end;

procedure TJvProgramVersionCheck.DownloadInstallerFromRemote;
begin
  if Assigned(fExecuteVersionInfo) then
  begin
    FThread.OnExecute   := DownloadThreadOnExecute;
    FThread.OnFinishAll := DownloadThreadOnFinishAll;
    FThread.Execute(self);
  end;
end;

procedure TJvProgramVersionCheck.DownloadThreadOnExecute(Sender: TObject;
  Params: Pointer);
begin
  if Assigned(fExecuteVersionInfo) then
  begin
    fExecuteDownloadInstallFilename :=
      LoadRemoteInstallerFile(LocalDirectory, LocalInstallerFileName,
      fExecuteVersionInfo, FThread.LastThread);
    if (fExecuteDownloadInstallFilename <> '') and not
      FileExists(fExecuteDownloadInstallFilename) then
      fExecuteDownloadInstallFilename := '';
  end;
end;

procedure TJvProgramVersionCheck.DownloadThreadOnFinishAll(Sender: TObject);
begin
  if fDownloadError <> '' then
    MessageDlg(fDownloadError, mtError, [mbOK], 0)
  else
  if fExecuteDownloadInstallFilename = '' then
    MessageDlg(RsPVCFileDownloadNotSuccessfull, mtError, [mbOK], 0)
  else
  if fExecuteOperation = rvoCopy then
    MessageDlg(Format(RsPVCDownloadSuccessfulInstallManually,
      [fExecuteDownloadInstallFilename]), mtInformation, [mbOK], 0)
  else
  if (MessageDlg(RsPVCDownloadSuccessfullInstallNow,
    mtWarning, [mbYes, mbNo], 0) = mrYes) then
  begin
    if ShellExecEx(fExecuteDownloadInstallFilename) then
      Application.Terminate
    else
      MessageDlg(RsPVCErrorStartingSetup, mtError, [mbOK], 0);
  end;
end;

procedure TJvProgramVersionCheck.Execute;
var
  ReleaseType: TJvProgramReleaseType;
begin
  fExecuteVersionInfo := nil;
  LoadProperties;
  if (LastCheck < now - CheckFrequency) and
    (LocationTypesSupported <> []) then
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
        fExecuteOperation   := GetRemoteVersionOperation(ReleaseType);
        fExecuteVersionInfo :=
          RemoteProgramVersionHistory.CurrentProgramVersion[ReleaseType];
        if fExecuteOperation in [rvoCopy, rvoCopyInstall] then
          DownloadInstallerFromRemote;
      end;
    end;
  end;
end;

function TJvProgramVersionCheck.GetAllowedRemoteProgramVersion: string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(
    AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(
      AllowedReleaseType).ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionCheck.GetAllowedRemoteProgramVersionReleaseType: string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(
    AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(
      AllowedReleaseType).ProgramVersionReleaseType
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
  prt: TJvProgramReleaseType;
begin
  Result := rvoIgnore;
  ParameterList := TJvParameterList.Create(self);
  try
    ParameterList.MaxWidth := 460;
    ParameterList.Messages.Caption :=
      Format(RsPVCDialogCaption, [CurrentApplicationName]);
    ParameterList.Messages.OkButton := RsPVCDialogExecuteButton;
    Parameter := TJvBaseParameter(TJvLabelParameter.Create(ParameterList));
    with Parameter do
    begin
      SearchName := 'New Version Label';
      Caption    := Format(RsPVCNewVersionAvailable,
        [GetAllowedRemoteProgramVersionReleaseType, CurrentApplicationName]);
      Width      := 350;
    end;
    ParameterList.AddParameter(Parameter);
    GroupParameter := TJvGroupBoxParameter.Create(ParameterList);
    with GroupParameter do
    begin
      SearchName := 'GroupBox';
      Caption    := RsPVCChooseWhichVersion;
      Width      := 350;
      Height     := 10;
    end;
    ParameterList.AddParameter(GroupParameter);
    for prt := high(prt) downto low(prt) do
      if (prt <= AllowedReleaseType) and
        Assigned(RemoteProgramVersionHistory.CurrentProgramVersion[prt]) then
        if CompareVersionNumbers(CurrentFileVersion,
          RemoteProgramVersionHistory.CurrentProgramVersion[prt].ProgramVersion) > 0 then
        begin
          Parameter := TJvBaseParameter(TJvRadioButtonParameter.Create(ParameterList));
          with Parameter do
          begin
            ParentParameterName := 'GroupBox';
            SearchName := 'RadioButton' + IntToStr(Ord(prt));
            Caption    := RemoteProgramVersionHistory.CurrentProgramVersion[
              prt].ProgramVersionInfo;
            Width      := 250;
            AsBoolean  := GroupParameter.Height <= 10;
          end;
          ParameterList.AddParameter(Parameter);
          Parameter := TJvBaseParameter(TJvButtonParameter.Create(ParameterList));
          with TJvButtonParameter(Parameter) do
          begin
            ParentParameterName := 'GroupBox';
            SearchName := 'VersionButtonInfo' + IntToStr(Ord(prt));
            Caption := 'Info';
            Width := 80;
            Tag := Ord(prt);
            OnClick := VersionInfoButtonClick;
          end;
          ParameterList.AddParameter(Parameter);
          GroupParameter.Height := GroupParameter.Height + 25;
        end;
    Parameter := TJvBaseParameter(TJvRadioGroupParameter.Create(ParameterList));
    with TJvRadioGroupParameter(Parameter) do
    begin
      SearchName := 'Operation';
      Caption    := RsPVCChooseOperation;
      ItemList.Add(RsPVCOperationIgnore);
      ItemList.Add(RsPVCOperationDownloadOnly);
      ItemList.Add(RsPVCOperationDownloadInstall);
      ItemIndex := 2;
      Width     := 350;
      Height    := 79;
    end;
    ParameterList.AddParameter(Parameter);
    if ParameterList.ShowParameterDialog then
    begin
      case TJvRadioGroupParameter(ParameterList.ParameterByName('Operation')).ItemIndex of
        0: Result := rvoIgnore;
        1: Result := rvoCopy;
        2: Result := rvoCopyInstall;
      end;
      ReleaseType := prtProduction;
      for prt := high(prt) downto low(prt) do
        if IsRemoteProgramVersionReleaseTypeNewer(prt) then
          if Assigned(ParameterList.ParameterByName(
            'RadioButton' + IntToStr(Ord(prt)))) then
            if ParameterList.ParameterByName('RadioButton' +
              IntToStr(Ord(prt))).AsBoolean then
            begin
              ReleaseType := prt;
              break;
            end;
    end;
  finally
    ParameterList.Free;
  end;
end;

function TJvProgramVersionCheck.IsRemoteProgramVersionNewer: boolean;
begin
  Result := CompareVersionNumbers(CurrentFileVersion,
    GetAllowedRemoteProgramVersion) = 1;
end;

function TJvProgramVersionCheck.IsRemoteProgramVersionReleaseTypeNewer
  (iReleaseType: TJvProgramReleaseType): boolean;
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
  LastCheck := AppStorage.ReadDateTime(AppStorage.ConcatPaths(
    [AppStoragePath, cLastCheck]), LastCheck);
end;

procedure TJvProgramVersionCheck.Loaded;
begin
  inherited Loaded;
end;

function TJvProgramVersionCheck.LoadRemoteInstallerFile(
  const iLocalDirectory, iLocalInstallerFileName: string;
  iProgramVersionInfo: TJvProgramVersionInfo; iBaseThread: TJvBaseThread): string;
begin
  if Assigned(iProgramVersionInfo) and
    (SelectedLocation <> nil) then
    begin
      Result := SelectedLocation.LoadInstallerFileFromRemote(
        iProgramVersionInfo.ProgramLocationPath,
        iProgramVersionInfo.ProgramLocationFileName,
        iLocalDirectory, iLocalInstallerFileName, iBaseThread);
      FDownloadError := SelectedLocation.DownloadError;
    end
  else
    Result := '';

end;

function TJvProgramVersionCheck.LoadRemoteVersionInfoFile(
  const iLocalDirectory, iLocalVersionInfoFileName: string): string;
begin
  if SelectedLocation <> nil then
    Result := SelectedLocation.LoadVersionInfoFromRemote(iLocalDirectory,
      iLocalVersionInfoFileName, nil)
  else
    Result := '';
end;

procedure TJvProgramVersionCheck.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = FLocationNetwork then
      FLocationNetwork := nil
    else if AComponent = FLocationDatabase then
      FLocationDatabase := nil
    else if AComponent = FLocationHTTP then
      FLocationHTTP := nil
    else if AComponent = FLocationFTP then
      FLocationFTP := nil
  end;
end;

function TJvProgramVersionCheck.SelectedLocation: TJvProgramVersionCustomLocation;
begin
  case LocationType of
    pvltDatabase: Result := LocationDatabase;
    pvltHTTP: Result     := LocationHTTP;
    pvltFTP: Result      := LocationFTP;
    pvltNetwork: Result  := LocationNetwork;
    else
      Result := nil;
  end
end;

procedure TJvProgramVersionCheck.SetThreadInfo(const Info: string);
begin
  if Assigned(FThreadDialog) then
    FThreadDialog.DialogOptions.InfoText := Info;
end;

procedure TJvProgramVersionCheck.SetUserOptions (Value: TJvProgramVersionUserOptions);
begin
  FUserOptions := Value;
  IgnoreProperties.AddDelete('CheckFrequency', (uoCheckFrequency IN Value));
  IgnoreProperties.AddDelete('LocalDirectory', (uoLocalDirectory IN Value));
  IgnoreProperties.AddDelete('AllowedReleaseType', (uoAllowedReleaseType IN Value));
  IgnoreProperties.AddDelete('LocationType', (uoLocationType IN Value));
  IgnoreProperties.AddDelete('LocationNetwork', (uoLocationNetwork IN Value));
  IgnoreProperties.AddDelete('LocationHTTP', (uoLocationHTTP IN Value));
  IgnoreProperties.AddDelete('LocationFTP', (uoLocationFTP IN Value));
  IgnoreProperties.AddDelete('LocationDatabase', (uoLocationDatabase IN Value));
end;

procedure TJvProgramVersionCheck.ShowProgramVersionsDescription(
  const iFromVersion, iToVersion: string);
var
  ParameterList: TJvParameterList;
  Parameter:     TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    ParameterList.Messages.Caption := Format(RsPVCWhatNewInS, [CurrentApplicationName]);
    ParameterList.CancelButtonVisible := False;
    Parameter := TJvMemoParameter.Create(ParameterList);
    with Parameter do
    begin
      SearchName := 'Memo';
      Caption    := Format(RsPVCChangesBetween, [iFromVersion, iToVersion]);
      Width      := 340;
      Height     := 200;
      AsString   := RemoteProgramVersionHistory.GetVersionsDescription
        (iFromVersion, iToVersion);
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
  AppStorage.WriteDateTime(AppStorage.ConcatPaths([AppStoragePath, cLastCheck]),
    LastCheck);
end;

procedure TJvProgramVersionCheck.StoreRemoteVersionInfoToFile;
begin
  FRemoteAppStorage.ReadOnly := False;
  RemoteProgramVersionHistory.StoreProperties;
  FRemoteAppStorage.Flush;
  FRemoteAppStorage.ReadOnly := True;
end;

procedure TJvProgramVersionCheck.VersionInfoButtonClick(
  const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter);
var
  prt: TJvProgramReleaseType;
begin
  prt := low(prt);
  Inc(prt, Parameter.Tag);
  with RemoteProgramVersionHistory do
    if Assigned(CurrentProgramVersion[prt]) then
      ShowProgramVersionsDescription(CurrentFileVersion,
        CurrentProgramVersion[prt].ProgramVersion);
end;

{$IFDEF USE_3RDPARTY_INDY}
//=== { TJvProgramVersionHTTPLocationIndy } =========================================

constructor TJvProgramVersionHTTPLocationIndy.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FIdHTTP := TIdHTTP.Create(self);
end;

destructor TJvProgramVersionHTTPLocationIndy.Destroy;
begin
  FIdHttp.Free;
  inherited Destroy;
end;

function TJvProgramVersionHTTPLocationIndy.LoadFileFromRemoteInt(const
    iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
    iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename,
      iLocalPath, iLocalFileName)
  else
    Result := LoadFileFromRemoteIndy(
        iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName, iBaseThread);
end;

function TJvProgramVersionHTTPLocationIndy.LoadFileFromRemoteIndy(const
    iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
    iBaseThread: TJvBaseThread): string;
var
  ResultStream : TFileStream;
  ResultName   : string;
begin

  Result := '';
  if (DirectoryExists(iLocalPath) or (iLocalPath = ''))  then
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
      if Username <> '' then
        Request.Username := Username;
      if Password <> '' then
        Request.Password := Password;
      try
        if Copy(iRemotePath, Length(iRemotePath),1) <> '/' then
          Get (iRemotePath+'/'+iRemoteFileName, ResultStream)
        else
          Get (iRemotePath+iRemoteFileName, ResultStream);
      except
        on e:exception do
          DownloadError := e.Message;
      end;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists (ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

//=== { TJvProgramVersionFTPLocationIndy } =========================================

constructor TJvProgramVersionFTPLocationIndy.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FIdFTP := TIdFTP.Create(self);
end;

destructor TJvProgramVersionFTPLocationIndy.Destroy;
begin
  FIdFtp.Free;
  inherited Destroy;
end;

function TJvProgramVersionFTPLocationIndy.LoadFileFromRemoteInt(const
    iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
    iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename,
      iLocalPath, iLocalFileName)
  else
    Result := LoadFileFromRemoteIndy(
        iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName, iBaseThread);
end;

function TJvProgramVersionFTPLocationIndy.LoadFileFromRemoteIndy(const
    iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
    iBaseThread: TJvBaseThread): string;
var
  ResultStream : TFileStream;
  ResultName   : string;
begin

  Result := '';
  if (DirectoryExists(iLocalPath) or (iLocalPath = ''))  then
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
    FIdFTP.ProxySettings.Username := ProxySettings.UserName;
    FIdFTP.ProxySettings.Password := ProxySettings.Password;
    with FIdFTP do
    begin
      try
        if Copy(iRemotePath, Length(iRemotePath),1) <> '/' then
          Get (iRemotePath+'/'+iRemoteFileName, ResultStream)
        else
          Get (iRemotePath+iRemoteFileName, ResultStream);
      except
        on e:exception do
          DownloadError := e.Message;
      end;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists (ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

{$ENDIF USE_3RDPARTY_INDY}

{$IFDEF USE_3RDPARTY_ICS}
//=== { TJvProgramVersionHTTPLocationIcs } =========================================

constructor TJvProgramVersionHTTPLocationIcs.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FHttpCli:= THttpCli.Create(self);
end;

destructor TJvProgramVersionHTTPLocationIcs.Destroy;
begin
  FHttpCli.Free;
  inherited Destroy;
end;

function TJvProgramVersionHTTPLocationIcs.LoadFileFromRemoteInt(const
    iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
    iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename,
      iLocalPath, iLocalFileName)
  else
    Result := LoadFileFromRemoteIcs(
        iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName, iBaseThread);
end;

function TJvProgramVersionHTTPLocationIcs.LoadFileFromRemoteIcs(const
    iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
    iBaseThread: TJvBaseThread): string;
var
  ResultStream : TFileStream;
  ResultName   : string;
begin

  Result := '';
  if (DirectoryExists(iLocalPath) or (iLocalPath = ''))  then
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
      if Copy(iRemotePath, Length(iRemotePath),1) <> '/' then
        Url := iRemotePath+'/'+iRemoteFileName
      else
        Url := iRemotePath+iRemoteFileName;
      try
        Get
      except
        on E: EHttpException do
          DownloadError := 'Failed : ' +
                           IntToStr(StatusCode) + ' ' +
                           ReasonPhrase;
        else
          raise;
      end;
    end;
  finally
    ResultStream.Free;
  end;
  if FileExists (ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

//=== { TJvProgramVersionFTPLocationIcs } =========================================

constructor TJvProgramVersionFTPLocationIcs.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FFtpClient:= TFtpClient.Create(self);
end;

destructor TJvProgramVersionFTPLocationIcs.Destroy;
begin
  FFtpClient.Free;
  inherited Destroy;
end;

function TJvProgramVersionFTPLocationIcs.LoadFileFromRemoteInt(const
    iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
    iBaseThread: TJvBaseThread): string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename,
      iLocalPath, iLocalFileName)
  else
    Result := LoadFileFromRemoteIcs(
        iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName, iBaseThread);
end;

function TJvProgramVersionFTPLocationIcs.LoadFileFromRemoteIcs(const
    iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string;
    iBaseThread: TJvBaseThread): string;
var
  ResultStream : TFileStream;
  ResultName: string;
  p: Integer;
begin

  Result := '';
  if (DirectoryExists(iLocalPath) or (iLocalPath = ''))  then
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
    FFtpClient.DataPortRangeEnd   := Port;
    FFtpClient.Username := Username;
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
      Binary:=True;
      ProxyPort := inttostr(ProxySettings.Port);
      ProxyServer := ProxySettings.Server;
//      ProxyUsername := ProxySettings.UserName;
//      ProxyPassword := ProxySettings.Password;
//      Port := 'ftp';
//      RcvdStream := ResultStream;
      LocalFileName := ResultName;
      p := Pos('://', iRemotePath);
      if p > 0 then
      begin
        HostName := Copy(iRemotepath, p+3, Length(iRemotePath)-p-2);
        p := Pos('/', HostName);
        HostDirName := Copy(HostName, p+1, Length(HostName)-p);
        HostName := Copy(HostName, 1, p-1);
      end
      else
      begin
        p := Pos('/', iRemotepath);
        HostName := Copy(iRemotepath, 1, p-1);
        HostDirName := Copy(iRemotepath, p+1, Length(iRemotePath)-p);
      end;
      if Copy(HostDirName,Length(HostDirName),1) = '/' then
        HostDirName := Copy(HostDirName,1, Length(HostDirName)-1);
      if HostDirName = '' then
        HostDirName := '/';
      if Copy(HostDirName,1,1) <> '/' then
        HostDirName := '/' + HostDirName;
      HostFileName := iRemoteFileName;
      try
        try
          if not Open then
          begin
            DownloadError := 'Failed : Unable to connect to '+HostName;
            exit;
          end;
          if not Get then
          begin
            DownloadError := 'Failed : Unable to get '+HostDirName+'/'+HostFileName;
            exit;
          end;
        except
          on E:Exception do
            DownloadError := 'Failed : '+ e.Message;
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
  if FileExists (ResultName) and (DownloadError = '') then
    Result := ResultName;
end;

{$ENDIF USE_3RDPARTY_ICS}

//=== { TJvProgramVersionProxySettings } =========================================

constructor TJvProgramVersionProxySettings.Create;
begin
  inherited;
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

