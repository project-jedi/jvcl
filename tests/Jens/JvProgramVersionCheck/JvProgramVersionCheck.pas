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

Uses Classes, JvPropertyStore, JvAppStorage, JvAppIniStorage, JvComponent,
     JvParameterList, JvThread, JvThreadDialog;


type
  TJvProgramReleaseType = (prtProduction, prtBeta, prtAlpha);

  TJvRemoteVersionOperation = (rvoIgnore, rvoCopy, rvoCopyInstall);

  TJvProgramVersionsStringList = class(tStringList)
  public
    procedure Sort; override;
  end;

  TJvProgramVersionInfo = class(tJvCustomPropertyStore)
  private
    FVersionDescription : tStringList;
    FProgramSize : Integer;
    FProgramVersion : string;
    FProgramLocationPath : string;
    FProgramLocationFileName : string;
    FProgramReleaseType : TJvProgramReleaseType;
    FProgramReleaseDate : tDateTime;
    FDownloadPasswordRequired : Boolean;
  protected
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function ProgramVersionReleaseType : string;
    function ProgramSizeString : string;
    function ProgramVersionInfo : string;
  published
    property ProgramLocationPath : string read FProgramLocationPath write FProgramLocationPath;
    property ProgramLocationFileName : string read FProgramLocationFileName write FProgramLocationFileName;
    property ProgramVersion : string read FProgramVersion write FProgramVersion;
    property VersionDescription : tStringList read FVersionDescription write FVersionDescription;
    property ProgramReleaseType : TJvProgramReleaseType read FProgramReleaseType write FProgramReleaseType;
    property ProgramSize : Integer read FProgramSize write FProgramSize;
    property ProgramReleaseDate : tDateTime read FProgramReleaseDate write FProgramReleaseDate ;
    property DownloadPasswordRequired : Boolean read FDownloadPasswordRequired write FDownloadPasswordRequired default false;
  end;

  TJvProgramVersionInfoReleaseArray = array[TJvProgramReleaseType] of  TJvProgramVersionInfo;

  TJvProgramVersionHistory = class(TJvCustomPropertyListStore)
  private
    FCurrentProductionVersion : string;
    FCurrentBetaVersion : string;
    FCurrentAlphaVersion : string;
    FCurrentProgramVersion : TJvProgramVersionInfoReleaseArray;
  protected
    function CreateObject: TObject; override;
    function CreateItemList : tStringList; override;
    function GetProgramVersion (Index : Integer) : TJvProgramVersionInfo;
    function GetCurrentProgramVersion (Index : TJvProgramReleaseType) : TJvProgramVersionInfo;
    function SearchCurrentProgramVersion (iProgramReleaseType : TJvProgramReleaseType) : TJvProgramVersionInfo;
    property ProgramVersion[Index: Integer]: TJvProgramVersionInfo read GetProgramVersion;
    function GetCurrentProductionProgramVersion : string;
    function GetCurrentBetaProgramVersion : string;
    function GetCurrentAlphaProgramVersion : string;
  public
    constructor Create (AOwner : TComponent); override;
    procedure LoadData; override;
    procedure RecalculateCurrentProgramVersions;
    property CurrentProgramVersion [Index :TJvProgramReleaseType] : TJvProgramVersionInfo read GetCurrentProgramVersion;
    function AllowedCurrentProgramVersion (iAllowedReleaseType : TJvProgramReleaseType) : TJvProgramVersionInfo;
    function GetVersionsDescription (const iFromVersion, iToVersion : string) : string;
  published
    property CurrentProductionProgramVersion : string read GetCurrentProductionProgramVersion write FCurrentProductionVersion;
    property CurrentBetaProgramVersion : string read GetCurrentBetaProgramVersion write FCurrentBetaVersion;
    property CurrentAlphaProgramVersion : string read GetCurrentAlphaProgramVersion  write FCurrentAlphaVersion;
  end;

  TJvProgramVersionCustomLocation = class(tJvCustomPropertyStore)
  private
//    FOwner: TComponent;
    FDownloadStatus : string;
    FDownloadThreaded: Boolean;
  protected
    procedure SetDownloadStatus (Value : string);
//    property Owner: TComponent read FOwner;
    function LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string; virtual;
  public
//    constructor Create (AOwner : TComponent); virtual;
    constructor Create (AOwner : TComponent); override;
    function LoadFileFromRemote (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string;
    function LoadVersionInfoFromRemote (const iLocalDirectory, iLocalVersionInfoFileName: string; iBaseThread : TJvBaseThread) : string; virtual;
    property DownloadStatus : string read FDownloadStatus write FDownloadStatus;
  published
    property DownloadThreaded: Boolean read FDownloadThreaded write FDownloadThreaded default false;
  end;

  TJvProgramVersionCustomFileBasedLocation = class(TJvProgramVersionCustomLocation)
  private
    FVersionInfoLocationPath : string;
    FVersionInfoFilename : string;
  protected
  public
    function LoadVersionInfoFromRemote (const iLocalDirectory, iLocalVersionInfoFileName: string; iBaseThread : TJvBaseThread) : string; override;
  published
    property VersionInfoLocationPath : string read FVersionInfoLocationPath write FVersionInfoLocationPath;
    property VersionInfoFilename : string read FVersionInfoFilename write FVersionInfoFilename;
  end;

  TJvProgramVersionNetworkLocation =  class(TJvProgramVersionCustomFileBasedLocation)
  private
  protected
    function LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string; override;
 public
  published
  end;

  TJvProgramVersionProxySettings = class(tPersistent)
  private
    FServer : string;
    FPort: Integer;
    FUsername: string;
    FPassword: string;
  published
    property Server : string read FServer write FServer;
    property Port: Integer read FPort write FPort;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
  end;


  TJvProgramVersionInternetLocation =  class(TJvProgramVersionCustomFileBasedLocation)
  private
    FProxySettings: TJvProgramVersionProxySettings;
    FPasswordRequired: Boolean;
  protected
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property ProxySettings: TJvProgramVersionProxySettings read FProxySettings ;
    property PasswordRequired: Boolean read FPasswordRequired write FPasswordRequired;
  end;

  TJvProgramVersionHTTPLocation = class;
  TJvLoadFileFromRemoteHTTPEvent = function (iProgramVersionLocation: TJvProgramVersionHTTPLocation; const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string) : string of object;

  TJvProgramVersionHTTPLocation =  class(TJvProgramVersionInternetLocation)
  private
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteHTTPEvent;
  protected
    function LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string; override;
  public
  published
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteHTTPEvent read FOnLoadFileFromRemote write FOnLoadFileFromRemote;
  end;

  TJvProgramVersionFTPLocation = class;
  TJvLoadFileFromRemoteFTPEvent = function (iProgramVersionLocation: TJvProgramVersionFTPLocation; const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string) : string of object;
  TJvProgramVersionFTPLocation =  class(TJvProgramVersionInternetLocation)
  private
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteFTPEvent;
  protected
    function LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string; override;
  public
  published
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteFTPEvent read FOnLoadFileFromRemote write FOnLoadFileFromRemote;
  end;

  TJvProgramVersionDatabaseLocation = class;
  TJvLoadFileFromRemoteDatabaseEvent = function (iProgramVersionLocation: TJvProgramVersionDatabaseLocation; const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string) : string of object;
  TJvProgramVersionDatabaseLocation =  class(TJvProgramVersionCustomLocation)
  private
    FServerName : string;
    FUsername   : string;
    FPasswort   : string;
    FSelectStatementVersion : string;
    FOnLoadFileFromRemote: TJvLoadFileFromRemoteDatabaseEvent;
  protected
    function LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string; override;
  public
    function LoadVersionInfoFromRemote (const iLocalDirectory, iLocalVersionInfoFileName: string; iBaseThread : TJvBaseThread) : string; override;
  published
    property ServerName : string read FServerName write FServerName;
    property Username   : string read FUsername write FUsername;
    property Passwort   : string read FPasswort write FPasswort;
    property SelectStatementVersion : string read FSelectStatementVersion write FSelectStatementVersion;
    property OnLoadFileFromRemote: TJvLoadFileFromRemoteDatabaseEvent read FOnLoadFileFromRemote write FOnLoadFileFromRemote;
  end;

  TJvProgramVersionLocationType = (pvltNetwork, pvltDatabase,
                                   pvltFTP, pvltHTTP);
  TJvProgramVersionLocationTypes = set of TJvProgramVersionLocationType;

  TJvProgramVersionLocations = class(tJvCustomPropertyStore)
  private
    FNetwork : TJvProgramVersionNetworkLocation;
    FHTTP : TJvProgramVersionHTTPLocation;
    FFTP : TJvProgramVersionFTPLocation;
    FDatabase : TJvProgramVersionDatabaseLocation;
  protected
    procedure Loaded; override;
  public
//    constructor Create (AOwner : TComponent); virtual;
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Network : TJvProgramVersionNetworkLocation read FNetwork ;
    property HTTP : TJvProgramVersionHTTPLocation read FHTTP ;
    property FTP : TJvProgramVersionFTPLocation read FFTP ;
    property Database : TJvProgramVersionDatabaseLocation read FDatabase ;
  end;

  TJvProgramVersionCheck = class(tJvCustomPropertyStore)
  private
    // Internal
    FThread : TJvThread;
    FThreadDialog : TJvThreadAnimateDialog;
    // Thread Communication
    fExecuteVersionInfo: TJvProgramVersionInfo;
    fExecuteOperation : TJvRemoteVersionOperation;
    fExecuteDownloadInstallFilename: string;
    FRemoteAppStorage : TJvAppIniFileStorage;
    //Property Variables
    FRemoteProgramVersionHistory : TJvProgramVersionHistory;
//    FLocations: TJvProgramVersionLocations;
    FAllowedReleaseType : TJvProgramReleaseType;
    fLastCheck : tDateTime;
    fCheckFrequency : Integer;
    fLocalDirectory : string;
    fLocalInstallerFileName : string;
    fLocalVersionInfoFileName : string;
    FLocationTypeSelected: TJvProgramVersionLocationType;
    FLocationNetwork : TJvProgramVersionNetworkLocation;
    FLocationHTTP : TJvProgramVersionHTTPLocation;
    FLocationFTP : TJvProgramVersionFTPLocation;
    FLocationDatabase : TJvProgramVersionDatabaseLocation;
  protected
    function GetLocationTypesSupported : TJvProgramVersionLocationTypes ;

    procedure Loaded; override;
    procedure StoreData; override;
    procedure LoadData; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetAllowedRemoteProgramVersion : string;
    function  GetAllowedRemoteProgramVersionReleaseType : string;
    function  IsRemoteProgramVersionReleaseTypeNewer (iReleaseType : TJvProgramReleaseType) : Boolean;
    procedure StoreRemoteVersionInfoToFile;
    function  CurrentFileVersion : string;
    function  CurrentApplicationName : string;
    function  IsRemoteProgramVersionNewer : Boolean;
    procedure DownloadThreadOnExecute(Sender: TObject; Params: Pointer);
    procedure DownloadThreadOnFinishAll(Sender : TObject);
    procedure VersionInfoButtonClick(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter) ;
    procedure SetThreadInfo (const Info : string);
    property  RemoteAppStorage : TJvAppIniFileStorage read FRemoteAppStorage;
    procedure CheckLocalDirectory;
    function LoadRemoteVersionInfoFile (const iLocalDirectory, iLocalVersionInfoFileName: string): String;
    function LoadRemoteInstallerFile (const iLocalDirectory, iLocalInstallerFileName: string;iProgramVersionInfo : TJvProgramVersionInfo;iBaseThread : TJvBaseThread) : string;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Execute ;
    procedure DownloadInstallerFromRemote;
    procedure ShowProgramVersionsDescription (const iFromVersion, iToVersion : string);
    function  GetRemoteVersionOperation (Var ReleaseType : TJvProgramReleaseType) : TJvRemoteVersionOperation;
    function  SelectedLocation : TJvProgramVersionCustomLocation;
    property  RemoteProgramVersionHistory : TJvProgramVersionHistory read FRemoteProgramVersionHistory write FRemoteProgramVersionHistory;
    property  LastCheck : tDateTime read fLastCheck write fLastCheck;
    property  LocationTypesSupported : TJvProgramVersionLocationTypes read GetLocationTypesSupported ;
  published
    property AllowedReleaseType : TJvProgramReleaseType read FAllowedReleaseType write FAllowedReleaseType default prtProduction;
    property CheckFrequency : Integer read fCheckFrequency write fCheckFrequency;
    property LocalDirectory : string read fLocalDirectory write fLocalDirectory;
    property LocalInstallerFileName : string read fLocalInstallerFileName write fLocalInstallerFileName;
    property LocalVersionInfoFileName : string read fLocalVersionInfoFileName write fLocalVersionInfoFileName;
//    property Locations: TJvProgramVersionLocations read FLocations ;
    property LocationTypeSelected : TJvProgramVersionLocationType read FLocationTypeSelected write FLocationTypeSelected;
    property LocationNetwork : TJvProgramVersionNetworkLocation read FLocationNetwork write FLocationNetwork;
    property LocationHTTP : TJvProgramVersionHTTPLocation read FLocationHTTP write FLocationHTTP;
    property LocationFTP : TJvProgramVersionFTPLocation read FLocationFTP write FLocationFTP;
    property LocationDatabase : TJvProgramVersionDatabaseLocation read FLocationDatabase write FLocationDatabase;
    property AboutJVCL;
    property AppStorage;
    property AppStoragePath;
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
  JvParameterListParameter, JvUrlListGrabber, JvUrlGrabbers;

Const cProgramVersion = 'Program Version ';
      cLastCheck = 'LastCheck';

      resourcestring
  RsPVCReleaseTypeAlpha = 'Alpha';
  RsPVCReleaseTypeBeta = 'Beta';
  RsPVCReleaseTypeProduction = 'Produktion';

  RsPVCDownloading = 'Downloading ...';
  RsPVCDialogCaption = '%s Upgrade Check';
  RsPVCDialogExecuteButton = '&Execute';
  RsPVCNewVersionAvailable = 'A new Version (%s) of %s is available!';
  RsPVCChooseWhichVersion = 'Which &Version do you want to install?';
  RsPVCChooseOperation = '&Choose the Operation';
  RsPVCOperationIgnore = 'I&gnore';
  RsPVCOperationDownloadOnly = 'Download/Copy &Only';
  RsPVCOperationDownloadInstall = 'Download/Copy and &Install';
  RsPVCWhatNewInS = 'What'' new in %s';
  RsPVCChangesBetween = 'Changes between %s and %s';
  RsPVCFileDownloadNotSuccessfull = 'The File Download was not Successfull!'+#13+#10+'Please try again manually.';
  RsPVCDownloadSuccessfulInstallManually = 'The file download was successfull.'+#13+#10+'Install manually from : %s';
  RsPVCErrorStartingSetup = 'Error starting the setup process.';
  RsPVCDownloadSuccessfullInstallNow = 'The file download was successfull.'+#13+#10+'Do you want to close and install?';



//=== { Common Functions} =========================================

function CompareVersionNumbers (iVersion1, iVersion2 : string) : Integer;
Var
  n1, n2      : Integer;

    function GetNextNumber (var Version : string) : Integer;
    var
      p : Integer;
      s : string;
    begin
      p := Pos ('.', Version);
      if p > 0 then
      begin
        s := Copy (Version, 1, p-1);
        Version := Copy (Version, p+1, Length(Version)-p);
      end
      else
      begin
        s := Version;
        Version := '';
      end;
      try
        Result := strtoint(s);
      except
        on e:exception do
          Result := -1;
      end;
    end;

begin
  Result:= 0;
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
function VersionNumberSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
Var
  s1, s2 : string;
begin
  s1 := TJvProgramVersionInfo(List.Objects[Index1]).ProgramVersion;
  s2 := TJvProgramVersionInfo(List.Objects[Index2]).ProgramVersion;
  Result := CompareVersionNumbers (s1, s2);
end;

procedure TJvProgramVersionsStringList.Sort;
begin
  CustomSort (VersionNumberSortCompare);
end;

//=== { tJvProgramVersionInfo } =========================================

constructor TJvProgramVersionInfo.create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  FVersionDescription := tStringList.Create;
  IgnoreLastLoadTime := True;
  FDownloadPasswordRequired := False;
end;

destructor TJvProgramVersionInfo.destroy;
begin
  FreeAndNil(FVersionDescription);
  Inherited Destroy;
end;

procedure TJvProgramVersionInfo.Clear;
begin
  if Assigned(FVersionDescription) then
    FVersionDescription.Clear;
  FProgramVersion := '';
  FProgramReleaseType := prtProduction;
end;

function TJvProgramVersionInfo.ProgramVersionReleaseType : string;
begin
  Case ProgramReleaseType of
    prtBeta  : Result := trim(ProgramVersion + ' '+RsPVCReleaseTypeBeta);
    prtAlpha : Result := trim(ProgramVersion + ' '+RsPVCReleaseTypeAlpha);
  else
     Result := trim(ProgramVersion  + ' '+RsPVCReleaseTypeProduction);;
  end;
end;

function TJvProgramVersionInfo.ProgramSizeString : string;
begin
  if ProgramSize <= 0 then
    Result := ''
  else if ProgramSize > 1024*1024*1024 then
    Result := format('%6.2f GB', [ProgramSize/1024/1024/1024])
  else if ProgramSize > 1024*1024 then
    Result := format('%6.2f MB', [ProgramSize/1024/1024])
  else if ProgramSize > 1024 then
    Result := format('%6.2f KB', [ProgramSize/1024])
  else
    Result := inttostr(ProgramSize)+'B';

end;

function TJvProgramVersionInfo.ProgramVersionInfo  : string;
begin
  Result := ProgramVersionReleaseType;
  if (ProgramSize > 0) then
    Result := Result + ' ('+ProgramSizeString+')';
end;

//=== { tJvProgramVersionHistory } =========================================

constructor TJvProgramVersionHistory.create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  DeleteBeforeStore := True;
  ItemName := cProgramVersion;
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('Duplicates');
  IgnoreProperties.Add('Sorted');
end;

procedure TJvProgramVersionHistory.RecalculateCurrentProgramVersions;
var prt : TJvProgramReleaseType;
begin
  for prt := low(TJvProgramReleaseType) to High(TJvProgramReleaseType) do
    FCurrentProgramVersion[prt] := SearchCurrentProgramVersion(prt);
end;

procedure TJvProgramVersionHistory.LoadData;
begin
  Inherited LoadData;
  Items.Sort;
  RecalculateCurrentProgramVersions;
end;

function TJvProgramVersionHistory.AllowedCurrentProgramVersion (iAllowedReleaseType : TJvProgramReleaseType) : TJvProgramVersionInfo;
var prt : TJvProgramReleaseType;
begin
  Result := nil;
  prt := Low(TJvProgramReleaseType);
  while prt <= iAllowedReleaseType do
  begin
    if Result = nil then
      Result := CurrentProgramVersion[prt]
    else
      if Assigned(CurrentProgramVersion[prt]) and
         (CompareVersionNumbers (Result.ProgramVersion, CurrentProgramVersion[prt].ProgramVersion) = 1) then
        Result := CurrentProgramVersion[prt];
    Inc(prt);
  end;
end;

function TJvProgramVersionHistory.GetProgramVersion (Index : Integer) : TJvProgramVersionInfo;
begin
  if Assigned(Objects[Index]) and
     (Objects[Index] IS TJvProgramVersionInfo) then
    Result := TJvProgramVersionInfo(Objects[Index])
  else
    Result := nil;
end;

function TJvProgramVersionHistory.SearchCurrentProgramVersion (iProgramReleaseType : TJvProgramReleaseType) : TJvProgramVersionInfo;
Var i : Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
    if Assigned(ProgramVersion[i]) then
      if ProgramVersion[i].ProgramReleaseType = iProgramReleaseType then
        if Result = nil then
          Result := ProgramVersion[i]
        else
          if CompareVersionNumbers (Result.ProgramVersion, ProgramVersion[i].ProgramVersion) = 1 then
            Result := ProgramVersion[i];
end;

function TJvProgramVersionHistory.GetCurrentProgramVersion (Index : TJvProgramReleaseType) : TJvProgramVersionInfo;
begin
  Result := FCurrentProgramVersion [Index];
end;

function TJvProgramVersionHistory.CreateObject: TObject;
begin
  Result := TJvProgramVersionInfo.Create(Self);
end;

function TJvProgramVersionHistory.CreateItemList : tStringList;
begin
  Result := TJvProgramVersionsStringList.Create;
end;

function TJvProgramVersionHistory.GetCurrentProductionProgramVersion : string;
begin
  if Assigned(CurrentProgramVersion[prtProduction]) then
    Result := CurrentProgramVersion[prtProduction].ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionHistory.GetCurrentBetaProgramVersion : string;
begin
  if Assigned(CurrentProgramVersion[prtBeta]) then
    Result := CurrentProgramVersion[prtBeta].ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionHistory.GetCurrentAlphaProgramVersion : string;
begin
  if Assigned(CurrentProgramVersion[prtAlpha]) then
    Result := CurrentProgramVersion[prtAlpha].ProgramVersion
  else
    Result := '';
end;

function TJvProgramVersionHistory.GetVersionsDescription (const iFromVersion, iToVersion : string) : string;
var i : integer;
begin
  Result := '';
  for i := 0 to Count -1 do
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

constructor TJvProgramVersionCustomLocation.create (AOwner : TComponent);
begin
//  inherited create;
  inherited create(AOwner);
//  SetSubComponent(True);
//  FOwner := AOwner;
  FDownloadThreaded := False;
  FDownloadStatus := '';
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('DownloadThreaded');
end;

function TJvProgramVersionCustomLocation.LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string;
begin
  DownloadStatus := RsPVCDownloading;
end;

function TJvProgramVersionCustomLocation.LoadFileFromRemote (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string;
begin
  Result := LoadFileFromRemoteInt (iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName,iBaseThread);
end;

function TJvProgramVersionCustomLocation.LoadVersionInfoFromRemote (const iLocalDirectory, iLocalVersionInfoFileName: string; iBaseThread : TJvBaseThread) : string; 
begin
end;

procedure TJvProgramVersionCustomLocation.SetDownloadStatus (Value : string);
begin
  FDownloadStatus := Value;
//  if Assigned(Owner.Owner) and
//     (Owner.Owner is TJvProgramVersionCheck) then
//       TJvProgramVersionCheck(Owner.Owner).SetThreadInfo(Value);
end;

//=== { TJvProgramVersionCustomFileBasedLocation } =========================================
function TJvProgramVersionCustomFileBasedLocation.LoadVersionInfoFromRemote (const iLocalDirectory, iLocalVersionInfoFileName: string; iBaseThread : TJvBaseThread) : string;
begin
  Result := LoadFileFromRemote (VersionInfoLocationPath, VersionInfoFilename, iLocalDirectory, iLocalVersionInfoFileName, iBaseThread);
end;

//=== { tJvProgramVersionNetworkLocation } =========================================

function TJvProgramVersionNetworkLocation.LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string;

  Function FileExistsNoDir (iFilename : string) : boolean;
  begin
    Result := FileExists(iFilename) and not DirectoryExists(iFilename);
  end;

begin
  Result := '';
  if (DirectoryExists (iLocalPath) or (iLocalPath = '')) and
     (DirectoryExists (iRemotePath) or (iRemotePath = '')) then
    if FileExistsNoDir (PathAppend(iRemotePath, iRemoteFileName)) then
      if (iRemotePath = iLocalPath) And (iRemoteFileName = iLocalFileName) then
        Result := PathAppend(iRemotePath, iRemoteFileName)
      else
        if FileCopy(PathAppend(iRemotePath, iRemoteFileName), PathAppend(iLocalPath, iLocalFileName), true) then
          if FileExistsNoDir(PathAppend(iLocalPath, iLocalFileName)) then
            Result := PathAppend(iLocalPath, iLocalFileName)
          else if FileExistsNoDir(PathAppend(iLocalPath, iRemoteFileName)) then
            Result := PathAppend(iLocalPath, iRemoteFileName)
          else if FileExistsNoDir(PathAppend(iLocalPath, ExtractFileName(iRemotePath))) then
            Result := PathAppend(iLocalPath, ExtractFileName(iRemotePath))
          else
            Result := PathAppend(iLocalPath, iLocalFileName)
        else
          Result := '';

end;

//=== { tJvProgramVersionHTTPLocation } =========================================
constructor TJvProgramVersionInternetLocation.create (AOwner : TComponent);
begin
  inherited create(AOwner);
  FProxySettings:= TJvProgramVersionProxySettings.Create;
end;

destructor TJvProgramVersionInternetLocation.Destroy;
begin
  FreeAndNil (FProxySettings);
  Inherited Destroy;
end;

//=== { tJvProgramVersionHTTPLocation } =========================================

function TJvProgramVersionHTTPLocation.LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename, iLocalPath, iLocalFileName);
end;

//=== { tJvProgramVersionFTPLocation } =========================================

function TJvProgramVersionFTPLocation.LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename, iLocalPath, iLocalFileName);
end;

//=== { tJvProgramVersionDatabaseLocation } =========================================

function TJvProgramVersionDatabaseLocation.LoadFileFromRemoteInt (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string;
begin
  Result := '';
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(Self, iRemotePath, iRemoteFilename, iLocalPath, iLocalFileName);
end;

function TJvProgramVersionDatabaseLocation.LoadVersionInfoFromRemote (const iLocalDirectory, iLocalVersionInfoFileName: string; iBaseThread : TJvBaseThread) : string;
begin
  Result := LoadFileFromRemote (SelectStatementVersion, '', iLocalDirectory, iLocalVersionInfoFileName, iBaseThread);
end;

//=== { TJvProgramVersionLocations } =========================================

constructor TJvProgramVersionLocations.create (AOwner : TComponent);
begin
  inherited create(AOwner);
  SetSubComponent(True);
  IgnoreLastLoadTime := True;
//  FNetwork := TJvProgramVersionNetworkLocation.Create(AOwner);
//  FHTTP := TJvProgramVersionHTTPLocation.Create(AOwner);
//  FFTP := TJvProgramVersionFTPLocation.Create(AOwner);
//  FDatabase := TJvProgramVersionDatabaseLocation.Create(AOwner);
  FNetwork := TJvProgramVersionNetworkLocation.Create(Self);
  FHTTP := TJvProgramVersionHTTPLocation.Create(Self);
  FFTP := TJvProgramVersionFTPLocation.Create(Self);
  FDatabase := TJvProgramVersionDatabaseLocation.Create(Self);
end;

destructor TJvProgramVersionLocations.destroy;
begin
  FNetwork.Free;
  FHTTP.Free;
  FFTP.Free;
  FDatabase.Free;
  inherited destroy;
end;

procedure TJvProgramVersionLocations.Loaded;
begin
  inherited Loaded;
end;


//=== { tJvProgramVersionCheck } =========================================

constructor TJvProgramVersionCheck.Create (AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FRemoteProgramVersionHistory := TJvProgramVersionHistory.Create(self);
  FRemoteProgramVersionHistory.IgnoreLastLoadTime := True;
  FRemoteAppStorage := TJvAppIniFileStorage.Create(self);
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

  DeleteBeforeStore := True;
  IgnoreLastLoadTime := True;
  IgnoreProperties.Add('LocalInstallerFileName');
  IgnoreProperties.Add('LocalVersionInfoFileName');
  IgnoreProperties.Add('RemoteAppStorage');

//  FLocations:= TJvProgramVersionLocations.Create(self);
  FAllowedReleaseType := prtProduction;
  fLocalInstallerFileName := '';
  fLocalVersionInfoFileName := 'versioninfo.ini';
  FLocationTypeSelected := pvltNetWork;
end;

destructor TJvProgramVersionCheck.destroy;
begin
//  FreeAndNil(FLocations);
  FreeAndNil(FThreadDialog);
  FreeAndNil(FThread);
  FreeAndNil(FRemoteAppStorage);
  Inherited Destroy;
end;

procedure TJvProgramVersionCheck.Loaded;
begin
  inherited Loaded;
end;

function TJvProgramVersionCheck.GetLocationTypesSupported : TJvProgramVersionLocationTypes ;
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

procedure TJvProgramVersionCheck.StoreData;
begin
  Inherited StoreData;
  AppStorage.WriteDateTime(AppStorage.ConcatPaths([AppStoragePath, cLastCheck]), LastCheck);
end;

procedure TJvProgramVersionCheck.LoadData;
begin
  Inherited LoadData;
  LastCheck := AppStorage.ReadDateTime(AppStorage.ConcatPaths([AppStoragePath, cLastCheck]), LastCheck);
end;

procedure TJvProgramVersionCheck.Notification(AComponent: TComponent; Operation: TOperation);
begin
  Inherited Notification(AComponent, Operation);

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

function TJvProgramVersionCheck.CurrentFileVersion : string;
VAR FileVersionInfo   : TJclFileVersionInfo;
BEGIN
  FileVersionInfo := TJclFileVersionInfo.Create (ParamStr(0));
  try
    try
      Result := FileVersionInfo.Fileversion;
    except
      on e:exception do
        Result := '';
    end;
  Finally
    FileVersionInfo.Free;
  end;
end;

function TJvProgramVersionCheck.CurrentApplicationName : string;
VAR FileVersionInfo   : TJclFileVersionInfo;
BEGIN
  FileVersionInfo := TJclFileVersionInfo.Create (ParamStr(0));
  try
    try
      Result := FileVersionInfo.ProductName;
    except
      on e:exception do
        Result := '';
    end;
    if Result = '' then
      Result := PathExtractFileNameNoExt(ParamStr(0));
  Finally
    FileVersionInfo.Free;
  end;
end;

function TJvProgramVersionCheck.IsRemoteProgramVersionNewer : Boolean;
begin
  Result:= CompareVersionNumbers (CurrentFileVersion, GetAllowedRemoteProgramVersion) = 1;
end;

function TJvProgramVersionCheck.IsRemoteProgramVersionReleaseTypeNewer (iReleaseType : TJvProgramReleaseType) : Boolean;
begin
  if Assigned(RemoteProgramVersionHistory.CurrentProgramVersion[iReleaseType]) then
    Result:= CompareVersionNumbers (CurrentFileVersion, RemoteProgramVersionHistory.CurrentProgramVersion[iReleaseType].ProgramVersion) = 1
  else
    Result := False;
end;

procedure TJvProgramVersionCheck.ShowProgramVersionsDescription (const iFromVersion, iToVersion : string);
Var ParameterList : TJvParameterList;
    Parameter : TJvBaseParameter;
begin
  ParameterList := TJvParameterList.Create(self);
  try
    ParameterList.Messages.Caption := Format(RsPVCWhatNewInS, [CurrentApplicationName]);
    ParameterList.CancelButtonVisible := False;
    Parameter := TJvMemoParameter.Create(ParameterList);
    with Parameter Do
    begin
      SearchName := 'Memo';
      Caption := Format (RsPVCChangesBetween, [iFromVersion, iToVersion]);
      Width := 340;
      Height := 200;
      AsString := RemoteProgramVersionHistory.GetVersionsDescription (iFromVersion, iToVersion);
    end;
    ParameterList.AddParameter(Parameter);
    ParameterList.ShowParameterDialog
  finally
    ParameterList.Free;
  end;
end;

procedure TJvProgramVersionCheck.VersionInfoButtonClick(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter) ;
var prt : TJvProgramReleaseType;
begin
  prt := low(prt);
  Inc (prt,Parameter.Tag);
  with RemoteProgramVersionHistory do
    if Assigned(CurrentProgramVersion[prt]) then
      ShowProgramVersionsDescription (CurrentFileVersion, CurrentProgramVersion[prt].ProgramVersion);
end;

function  TJvProgramVersionCheck.GetRemoteVersionOperation (Var ReleaseType : TJvProgramReleaseType) : TJvRemoteVersionOperation;
Var ParameterList : TJvParameterList;
    GroupParameter : TJvGroupBoxParameter;
    Parameter : TJvBaseParameter;
    prt : TJvProgramReleaseType;
begin
  Result := rvoIgnore;
  ParameterList := TJvParameterList.Create(self);
  try
    ParameterList.MaxWidth := 460;
    ParameterList.Messages.Caption := Format(RsPVCDialogCaption, [CurrentApplicationName]);
    ParameterList.Messages.OkButton := RsPVCDialogExecuteButton;
    Parameter := TJvBaseParameter(TJvLabelParameter.Create(ParameterList));
    with Parameter Do
    begin
      SearchName := 'New Version Label';
      Caption := Format (RsPVCNewVersionAvailable, [GetAllowedRemoteProgramVersionReleaseType ,CurrentApplicationName]);
      Width := 350;
    end;
    ParameterList.AddParameter(Parameter);
    GroupParameter := TJvGroupBoxParameter.Create(ParameterList);
    with GroupParameter Do
    begin
      SearchName := 'GroupBox';
      Caption := RsPVCChooseWhichVersion;
      Width := 350;
      Height := 10;
    end;
    ParameterList.AddParameter(GroupParameter);
    for prt := high(prt) downto low(prt) do
      if prt <= AllowedReleaseType then
        if CompareVersionNumbers(CurrentFileVersion, RemoteProgramVersionHistory.CurrentProgramVersion[prt].ProgramVersion) > 0 then
          begin
            Parameter := TJvBaseParameter(TJvRadioButtonParameter.Create(ParameterList));
            with Parameter Do
            begin
              ParentParameterName := 'GroupBox';
              SearchName := 'RadioButton'+inttostr(ord(prt));
              Caption := RemoteProgramVersionHistory.CurrentProgramVersion[prt].ProgramVersionInfo;
              Width := 250;
              AsBoolean:= GroupParameter.Height <= 10;
            end;
            ParameterList.AddParameter(Parameter);
            Parameter := TJvBaseParameter(TJvButtonParameter.Create(ParameterList));
            with TJvButtonParameter(Parameter) Do
            begin
              ParentParameterName := 'GroupBox';
              SearchName := 'VersionButtonInfo'+inttostr(ord(prt));
              Caption := 'Info';
              Width := 80;
              Tag := Ord(prt);
              OnButtonClick := VersionInfoButtonClick;
            end;
            ParameterList.AddParameter(Parameter);
            GroupParameter.Height := GroupParameter.Height + 25;
          end;
    Parameter := TJvBaseParameter(TJvRadioGroupParameter.Create(ParameterList));
    with TJvRadioGroupParameter(Parameter) Do
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
      Case TJvRadioGroupParameter(ParameterList.ParameterByName('Operation')).ItemIndex of
        0 : Result := rvoIgnore;
        1 : Result := rvoCopy;
        2 : Result := rvoCopyInstall;
      End;
      ReleaseType := prtProduction;
      for prt := high(prt) downto low(prt) do
        if IsRemoteProgramVersionReleaseTypeNewer (prt) then
          if Assigned(ParameterList.ParameterByName('RadioButton'+inttostr(ord(prt)))) then
            if ParameterList.ParameterByName('RadioButton'+inttostr(ord(prt))).AsBoolean then
            begin
              ReleaseType := prt;
              break;
            end;
    end;
  finally
    ParameterList.Free;
  end;
end;

procedure TJvProgramVersionCheck.DownloadThreadOnExecute(Sender: TObject; Params: Pointer);
begin
  if Assigned(fExecuteVersionInfo) then
  begin
    fExecuteDownloadInstallFilename := LoadRemoteInstallerFile (LocalDirectory, LocalInstallerFileName, fExecuteVersionInfo, FThread.LastThread);
    if (fExecuteDownloadInstallFilename <> '') and
        not FileExists(fExecuteDownloadInstallFilename) then
      fExecuteDownloadInstallFilename := '';
  end;
end;

procedure TJvProgramVersionCheck.DownloadThreadOnFinishAll(Sender : TObject);
begin
  if fExecuteDownloadInstallFilename = '' then
    MessageDlg(RsPVCFileDownloadNotSuccessfull, mtError, [mbOK], 0)
  else
    if fExecuteOperation = rvoCopy then
      MessageDlg(Format(RsPVCDownloadSuccessfulInstallManually, [fExecuteDownloadInstallFilename]), mtInformation, [mbOK], 0)
    else
      if (MessageDlg(RsPVCDownloadSuccessfullInstallNow, mtWarning, [mbYes, mbNo], 0) = mrYes) then
      begin
        if ShellExecEx(fExecuteDownloadInstallFilename) then
          Application.Terminate
        else
        MessageDlg(RsPVCErrorStartingSetup, mtError, [mbOK], 0);
      end;
end;

procedure TJvProgramVersionCheck.DownloadInstallerFromRemote;
begin
  if Assigned(fExecuteVersionInfo) then
  begin
    FThread.OnExecute := DownloadThreadOnExecute;
    FThread.OnFinishAll := DownloadThreadOnFinishAll;
    FThread.Execute(self);
  end;
end;

procedure TJvProgramVersionCheck.SetThreadInfo (const Info : string);
begin
  if Assigned(FThreadDialog) then
    FThreadDialog.DialogOptions.InfoText := Info;
end;

function  TJvProgramVersionCheck.GetAllowedRemoteProgramVersion : string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType).ProgramVersion
  else
    Result := '';
end;

function  TJvProgramVersionCheck.GetAllowedRemoteProgramVersionReleaseType : string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(AllowedReleaseType).ProgramVersionReleaseType
  else
    Result := '';
end;

procedure TJvProgramVersionCheck.StoreRemoteVersionInfoToFile;
begin
  FRemoteAppStorage.ReadOnly := False;
  RemoteProgramVersionHistory.StoreProperties;
  FRemoteAppStorage.Flush;
  FRemoteAppStorage.ReadOnly := True;
end;

procedure TJvProgramVersionCheck.Execute ;
var ReleaseType : TJvProgramReleaseType;
begin
  fExecuteVersionInfo:= nil;
  LoadProperties;
  if (LastCheck < now-CheckFrequency) and
     (LocationTypesSupported <> []) then
  begin
    LastCheck:= Now;
    RemoteAppStorage.FileName := LoadRemoteVersionInfoFile(LocalDirectory, LocalVersionInfoFileName);
    if RemoteAppStorage.FileName <> '' then
    begin
      RemoteProgramVersionHistory.LoadProperties;
      StoreProperties;
      StoreRemoteVersionInfoToFile;
      if IsRemoteProgramVersionNewer then
      begin
        fExecuteOperation := GetRemoteVersionOperation (ReleaseType);
        fExecuteVersionInfo:= RemoteProgramVersionHistory.CurrentProgramVersion[ReleaseType];
        if fExecuteOperation in [rvoCopy, rvoCopyInstall] then
          DownloadInstallerFromRemote;
      end;
    end;
  end;
end;

procedure TJvProgramVersionCheck.CheckLocalDirectory;
begin
  LocalDirectory := trim(LocalDirectory);
  if LocalDirectory <> '' then
    if not DirectoryExists(LocalDirectory) then
      if not ForceDirectories(LocalDirectory) then
        LocalDirectory := '';
end;

function TJvProgramVersionCheck.LoadRemoteVersionInfoFile(const iLocalDirectory, iLocalVersionInfoFileName: string) : String;
begin
  if SelectedLocation <> nil then
    Result := SelectedLocation.LoadVersionInfoFromRemote(iLocalDirectory, iLocalVersionInfoFileName, nil)
  else
    Result := '';
end;

function TJvProgramVersionCheck.LoadRemoteInstallerFile (const iLocalDirectory, iLocalInstallerFileName: string;iProgramVersionInfo : TJvProgramVersionInfo;iBaseThread : TJvBaseThread) : String;
begin
//  sleep(5000);
  if Assigned(iProgramVersionInfo) and
     (SelectedLocation <> nil) then
    Result := SelectedLocation.LoadFileFromRemote(iProgramVersionInfo.ProgramLocationPath, iProgramVersionInfo.ProgramLocationFileName, iLocalDirectory, iLocalInstallerFileName, iBaseThread)
  else
    Result := '';
end;

function  TJvProgramVersionCheck.SelectedLocation : TJvProgramVersionCustomLocation;
begin
  Case LocationTypeSelected of
    pvltDatabase : Result := LocationDatabase;
    pvltHTTP : Result := LocationHTTP;
    pvltFTP : Result := LocationFTP;
    pvltNetwork : Result := LocationNetwork;
  else
    Result := nil;
  end
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
