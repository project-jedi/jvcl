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

Uses Classes, JvPropertyStore, JvAppStorage, JvAppIniStorage,
     JvParameterList, JvThread, JvThreadDialog;


type
  tJvProgramVersionCustomLocation = class;

  tJvProgramReleaseType = (prtProduction, prtBeta, prtAlpha);

  tJvRemoteVersionOperation = (rvoIgnore, rvoCopy, rvoCopyInstall);
  TJvLoadFileFromRemoteEvent = function (iProgramVersionLocation: tJvProgramVersionCustomLocation; const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string of object;

  tJvProgramVersionsStringList = class(tStringList)
  public
    procedure Sort; override;
  end;

  tJvProgramVersionInfo = class(tJvCustomPropertyStore)
  private
    fVersionDescription : tStringList;
    fProgramSize : Integer;
    fProgramVersion : string;
    fProgramLocationPath : string;
    fProgramLocationFileName : string;
    fProgramReleaseType : tJvProgramReleaseType;
    fProgramReleaseDate : tDateTime;
  protected
  public
    constructor create (AOwner: TComponent); override;
    destructor destroy; override;
    procedure Clear; override;
    function ProgramVersionReleaseType : string;
    function ProgramSizeString : string;
    function ProgramVersionInfo : string;
  published
    property ProgramLocationPath : string read fProgramLocationPath write fProgramLocationPath;
    property ProgramLocationFileName : string read fProgramLocationFileName write fProgramLocationFileName;
    property ProgramVersion : string read fProgramVersion write fProgramVersion;
    property VersionDescription : tStringList read fVersionDescription write fVersionDescription;
    property ProgramReleaseType : tJvProgramReleaseType read fProgramReleaseType write fProgramReleaseType;
    property ProgramSize : Integer read fProgramSize write fProgramSize;
    property ProgramReleaseDate : tDateTime read fProgramReleaseDate write fProgramReleaseDate ;
  end;

  tJvProgramVersionInfoReleaseArray = array[tJvProgramReleaseType] of  tJvProgramVersionInfo;

  tJvProgramVersionHistory = class(tJvCustomPropertyListStore)
  private
    fCurrentProductionVersion : string;
    fCurrentBetaVersion : string;
    fCurrentAlphaVersion : string;
    fCurrentProgramVersion : tJvProgramVersionInfoReleaseArray;
  protected
    function CreateObject: TObject; override;
    function CreateItemList : tStringList; override;
    function GetProgramVersion (Index : Integer) : tJvProgramVersionInfo;
    function GetCurrentProgramVersion (Index : tJvProgramReleaseType) : tJvProgramVersionInfo;
    function SearchCurrentProgramVersion (iProgramReleaseType : TJvProgramReleaseType) : tJvProgramVersionInfo;
    property ProgramVersion[Index: Integer]: tJvProgramVersionInfo read GetProgramVersion;
    function GetCurrentProductionProgramVersion : string;
    function GetCurrentBetaProgramVersion : string;
    function GetCurrentAlphaProgramVersion : string;
  public
    constructor create (AOwner : TComponent); override;
    procedure LoadData; override;
    procedure RecalculateCurrentProgramVersions;
    property CurrentProgramVersion [Index :tJvProgramReleaseType] : tJvProgramVersionInfo read GetCurrentProgramVersion;
    function AllowedCurrentProgramVersion (iAllowedReleaseType : tJvProgramReleaseType) : tJvProgramVersionInfo;
    function GetVersionsDescription (const iFromVersion, iToVersion : string) : string;
  published
    property CurrentProductionProgramVersion : string read GetCurrentProductionProgramVersion write fCurrentProductionVersion;
    property CurrentBetaProgramVersion : string read GetCurrentBetaProgramVersion write fCurrentBetaVersion;
    property CurrentAlphaProgramVersion : string read GetCurrentAlphaProgramVersion  write fCurrentAlphaVersion;
  end;

  tJvProgramVersionCustomLocation = class(tJvCustomPropertyStore)
  private
    fOnLoadFileFromRemote : TJvLoadFileFromRemoteEvent;
    fDownloadStatus : string;
  protected
    procedure SetDownloadStatus (Value : string);
  public
    constructor create (AOwner : TComponent); override;
    function LoadFileFromRemote (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string; virtual;
    property DownloadStatus : string read fDownloadStatus write fDownloadStatus;
  published
    property OnLoadFileFromRemote : TJvLoadFileFromRemoteEvent read fOnLoadFileFromRemote write fOnLoadFileFromRemote;
  end;

  tJvProgramVersionCustomFileBasedLocation = class(tJvProgramVersionCustomLocation)
  private
    fLocationPathVersion : string;
  protected
  public
  published
    property LocationPathVersion : string read fLocationPathVersion write fLocationPathVersion;
  end;

  tJvProgramVersionNetworkLocation =  class(tJvProgramVersionCustomFileBasedLocation)
  private
  protected
    property OnLoadFileFromRemote;
 public
    function LoadFileFromRemote (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string; override;
  published
  end;

  tJvProgramVersionInternetLocation =  class(tJvProgramVersionCustomFileBasedLocation)
  private
    fProxyServer : string;
    fProxyPort: Integer;
  protected
  public
  published
    property ProxyServer : string read fProxyServer write fProxyServer;
    property ProxyPort: Integer read fProxyPort write fProxyPort;
  end;

  tJvProgramVersionDatabaseLocation =  class(tJvProgramVersionCustomLocation)
  private
    fServerName : string;
    fUsername   : string;
    fPasswort   : string;
    fSelectStatementVersion : string;
  protected
  public
  published
    property ServerName : string read fServerName write fServerName;
    property Username   : string read fUsername write fUsername;
    property Passwort   : string read fPasswort write fPasswort;
    property SelectStatementVersion : string read fSelectStatementVersion write fSelectStatementVersion;
  end;


  tJvProgramVersionLocationType = (pvltNetwork, pvltDatabase,
                                   pvltFTP, pvltHTTP);
  tJvProgramVersionLocationTypes = set of tJvProgramVersionLocationType;

  tJvProgramVersionCheckOptions = class(tJvCustomPropertyStore)
  private
    fAllowedReleaseType : tJvProgramReleaseType;
    fLastCheck : tDateTime;
    fCheckFrequency : Integer;
    fLocationType : tJvProgramVersionLocationType;
    fNetworkLocation : tJvProgramVersionNetworkLocation;
    fHTTPLocation : tJvProgramVersionInternetLocation;
    fFTPLocation : tJvProgramVersionInternetLocation;
    fDatabaseLocation : tJvProgramVersionDatabaseLocation;
    fSupportedLocationTypes : tJvProgramVersionLocationTypes;
    fShowDownloadDialog : Boolean;
    fLocalDirectory : string;
    fLocalInstallerFileName : string;
    fLocalVersionInfoFileName : string;
  protected
    procedure CheckLocalDirectory;
    procedure SetSupportedLocationTypes (Value : tJvProgramVersionLocationTypes );
  public
    constructor create (AOwner : TComponent); override;
    destructor destroy; override;
    procedure LoadProperties; override;
    function LoadRemoteVersionInfoFile : Boolean;
    function LoadRemoteInstallerFile (iProgramVersionInfo : TJvProgramVersionInfo;iBaseThread : TJvBaseThread) : string;
  published
    property AllowedReleaseType : tJvProgramReleaseType read fAllowedReleaseType write fAllowedReleaseType default prtProduction;
    property LastCheck : tDateTime read fLastCheck write fLastCheck;
    property CheckFrequency : Integer read fCheckFrequency write fCheckFrequency;
    property LocationType : tJvProgramVersionLocationType read fLocationType write fLocationType;
    property NetworkLocation : tJvProgramVersionNetworkLocation read fNetworkLocation write fNetworkLocation;
    property HTTPLocation : tJvProgramVersionInternetLocation read fHTTPLocation write fHTTPLocation;
    property FTPLocation : tJvProgramVersionInternetLocation read fFTPLocation write fFTPLocation;
    property DatabaseLocation : tJvProgramVersionDatabaseLocation read fDatabaseLocation write fDatabaseLocation;
    property SupportedLocationTypes : tJvProgramVersionLocationTypes read fSupportedLocationTypes write SetSupportedLocationTypes default [pvltNetwork];
    property ShowDownloadDialog : Boolean read fShowDownloadDialog write fShowDownloadDialog default true;
    property LocalDirectory : string read fLocalDirectory write fLocalDirectory;
    property LocalInstallerFileName : string read fLocalInstallerFileName write fLocalInstallerFileName;
    property LocalVersionInfoFileName : string read fLocalVersionInfoFileName write fLocalVersionInfoFileName;
  end;

  tJvProgramVersionCheck = class(tComponent)
  private
    fRemoteAppStorage : TJvAppIniFileStorage;
    fAppStorage : TJvCustomAppStorage;
    fAppStoragePath : string;
    fCloseAfterInstallStarted : Boolean;
    fProgramVersionCheckOptions : tJvProgramVersionCheckOptions;
    fMinRequiredVersion : string;
    fRemoteProgramVersionHistory : tJvProgramVersionHistory;
    fThread : TJvThread;
    fThreadDialog : TJvThreadAnimateDialog;
    fExecuteVersionInfo: tJvProgramVersionInfo;
    fExecuteOperation : tJvRemoteVersionOperation;
    fExecuteDownloadInstallFilename: string;
  protected
    procedure SetAppStorage (Value : TJvCustomAppStorage);
    procedure SetAppStoragePath (Value : string);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetAllowedRemoteProgramVersion : string;
    function  GetAllowedRemoteProgramVersionReleaseType : string;
    function  IsRemoteProgramVersionReleaseTypeNewer (iReleaseType : tJvProgramReleaseType) : Boolean;
    procedure StoreRemoteVersionInfoToFile;
    function  CurrentFileVersion : string;
    function  CurrentApplicationName : string;
    function  IsRemoteProgramVersionNewer : Boolean;
    procedure ShowProgramVersionsDescription (const iFromVersion, iToVersion : string);
    function  GetRemoteVersionOperation (Var ReleaseType : tJvProgramReleaseType) : tJvRemoteVersionOperation;
    procedure DownloadThreadOnExecute(Sender: TObject; Params: Pointer);
    procedure DownloadThreadOnFinishAll(Sender : TObject);
    procedure VersionInfoButtonClick(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter) ;
    function  DownloadInstallerFromRemote : boolean;
    procedure SetThreadInfo (const Info : string);
    property  RemoteAppStorage : TJvAppIniFileStorage read fRemoteAppStorage;
  public
    constructor Create (AOwner : TComponent); override;
    destructor destroy; override;
    procedure Execute ;
    property  RemoteProgramVersionHistory : tJvProgramVersionHistory read fRemoteProgramVersionHistory write fRemoteProgramVersionHistory;
  published
    property AppStorage : TJvCustomAppStorage read fAppStorage write SetAppStorage;
    property AppStoragePath : string read fAppStoragePath write SetAppStoragePath;
    property CloseAfterInstallStarted : Boolean read fCloseAfterInstallStarted write fCloseAfterInstallStarted default true;
    property MinRequiredVersion : string read fMinRequiredVersion write fMinRequiredVersion;
    property ProgramVersionCheckOptions : tJvProgramVersionCheckOptions read fProgramVersionCheckOptions ;
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

Const cProgramVersion = 'Program Version ';

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
  s1 := tJvProgramVersionInfo(List.Objects[Index1]).ProgramVersion;
  s2 := tJvProgramVersionInfo(List.Objects[Index2]).ProgramVersion;
  Result := CompareVersionNumbers (s1, s2);
end;

procedure tJvProgramVersionsStringList.Sort;
begin
  CustomSort (VersionNumberSortCompare);
end;

//=== { tJvProgramVersionInfo } =========================================

constructor tJvProgramVersionInfo.create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  fVersionDescription := tStringList.Create;
end;

destructor tJvProgramVersionInfo.destroy;
begin
  FreeAndNil(fVersionDescription);
  Inherited Destroy;
end;

procedure tJvProgramVersionInfo.Clear;
begin
  if Assigned(fVersionDescription) then
    fVersionDescription.Clear;
  fProgramVersion := '';
  fProgramReleaseType := prtProduction;
end;

function tJvProgramVersionInfo.ProgramVersionReleaseType : string;
begin
  Case ProgramReleaseType of
    prtBeta  : Result := trim(ProgramVersion + ' '+RsPVCReleaseTypeBeta);
    prtAlpha : Result := trim(ProgramVersion + ' '+RsPVCReleaseTypeAlpha);
  else
     Result := trim(ProgramVersion  + ' '+RsPVCReleaseTypeProduction);;
  end;
end;

function tJvProgramVersionInfo.ProgramSizeString : string;
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

function tJvProgramVersionInfo.ProgramVersionInfo  : string;
begin
  Result := ProgramVersionReleaseType;
  if (ProgramSize > 0) then
    Result := Result + ' ('+ProgramSizeString+')';
end;

//=== { tJvProgramVersionHistory } =========================================

constructor tJvProgramVersionHistory.create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  DeleteBeforeStore := True;
  ItemName := cProgramVersion;
  IgnoreProperties.Add('Duplicates');
  IgnoreProperties.Add('Sorted');
end;

procedure tJvProgramVersionHistory.RecalculateCurrentProgramVersions;
var prt : tJvProgramReleaseType;
begin
  for prt := low(tJvProgramReleaseType) to High(tJvProgramReleaseType) do
    fCurrentProgramVersion[prt] := SearchCurrentProgramVersion(prt);
end;

procedure tJvProgramVersionHistory.LoadData;
begin
  Inherited LoadData;
  Items.Sort;
  RecalculateCurrentProgramVersions;
end;

function tJvProgramVersionHistory.AllowedCurrentProgramVersion (iAllowedReleaseType : tJvProgramReleaseType) : tJvProgramVersionInfo;
var prt : tJvProgramReleaseType;
begin
  result := nil;
  prt := Low(tJvProgramReleaseType);
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

function tJvProgramVersionHistory.GetProgramVersion (Index : Integer) : tJvProgramVersionInfo;
begin
  if Assigned(Objects[Index]) and
     (Objects[Index] IS tJvProgramVersionInfo) then
    Result := tJvProgramVersionInfo(Objects[Index])
  else
    Result := nil;
end;

function tJvProgramVersionHistory.SearchCurrentProgramVersion (iProgramReleaseType : TJvProgramReleaseType) : tJvProgramVersionInfo;
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

function tJvProgramVersionHistory.GetCurrentProgramVersion (Index : tJvProgramReleaseType) : tJvProgramVersionInfo;
begin
  Result := fCurrentProgramVersion [Index];
end;

function tJvProgramVersionHistory.CreateObject: TObject;
begin
  Result := tJvProgramVersionInfo.Create(Self);
end;

function tJvProgramVersionHistory.CreateItemList : tStringList;
begin
  Result := tJvProgramVersionsStringList.Create;
end;

function tJvProgramVersionHistory.GetCurrentProductionProgramVersion : string;
begin
  if Assigned(CurrentProgramVersion[prtProduction]) then
    Result := CurrentProgramVersion[prtProduction].ProgramVersion
  else
    Result := '';
end;

function tJvProgramVersionHistory.GetCurrentBetaProgramVersion : string;
begin
  if Assigned(CurrentProgramVersion[prtBeta]) then
    Result := CurrentProgramVersion[prtBeta].ProgramVersion
  else
    Result := '';
end;

function tJvProgramVersionHistory.GetCurrentAlphaProgramVersion : string;
begin
  if Assigned(CurrentProgramVersion[prtAlpha]) then
    Result := CurrentProgramVersion[prtAlpha].ProgramVersion
  else
    Result := '';
end;

function tJvProgramVersionHistory.GetVersionsDescription (const iFromVersion, iToVersion : string) : string;
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

constructor tJvProgramVersionCustomLocation.create (AOwner : TComponent);
begin
  inherited create(AOwner);
  fDownloadStatus := '';
end;

function tJvProgramVersionCustomLocation.LoadFileFromRemote (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string;
begin
  DownloadStatus := RsPVCDownloading;
  if Assigned(OnLoadFileFromRemote) then
    Result := OnLoadFileFromRemote(self, iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName, iBaseThread)
  else
    Result := '';
end;

procedure tJvProgramVersionCustomLocation.SetDownloadStatus (Value : string);
begin
  fDownloadStatus := Value;
  if Assigned(Owner) and
     Assigned(Owner.Owner) and
     (Owner.Owner is tJvProgramVersionCheck) then
       tJvProgramVersionCheck(Owner.Owner).SetThreadInfo(Value);
end;
//=== { tJvProgramVersionNetworkLocation } =========================================

function tJvProgramVersionNetworkLocation.LoadFileFromRemote (const iRemotePath, iRemoteFileName, iLocalPath, iLocalFileName: string; iBaseThread : TJvBaseThread) : string;

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
          result := '';

end;

//=== { tJvProgramVersionCheckOptions } =========================================

constructor tJvProgramVersionCheckOptions.create (AOwner : TComponent);
begin
  inherited create(AOwner);
  DeleteBeforeStore := True;
  IgnoreLastLoadTime := True;
  fSupportedLocationTypes := [pvltNetwork];
  fLocationType := pvltNetWork;
  fNetworkLocation := tJvProgramVersionNetworkLocation.Create(Self);
  fHTTPLocation := tJvProgramVersionInternetLocation.Create(Self);
  fFTPLocation := tJvProgramVersionInternetLocation.Create(Self);
  fDatabaseLocation := tJvProgramVersionDatabaseLocation.Create(Self);
  fAllowedReleaseType := prtProduction;
  fShowDownloadDialog := true;
  fLocalInstallerFileName := '';
  fLocalVersionInfoFileName := 'versioninfo.ini';
  IgnoreProperties.Add('LocalInstallerFileName');
  IgnoreProperties.Add('LocalVersionInfoFileName');
  IgnoreProperties.Add('SupportedLocationTypes');
end;

destructor tJvProgramVersionCheckOptions.destroy;
begin
  fNetworkLocation.Free;
  fHTTPLocation.Free;
  fFTPLocation.Free;
  fDatabaseLocation.Free;
  inherited destroy;
end;

procedure tJvProgramVersionCheckOptions.LoadProperties;
begin
  inherited LoadProperties;
  if not (LocationType in SupportedLocationTypes) then
    LocationType := pvltNetWork;
  CheckLocalDirectory;
end;

procedure tJvProgramVersionCheckOptions.CheckLocalDirectory;
begin
  LocalDirectory := trim(LocalDirectory);
  if LocalDirectory <> '' then
    if not DirectoryExists(LocalDirectory) then
      if not ForceDirectories(LocalDirectory) then
        LocalDirectory := '';
end;

function tJvProgramVersionCheckOptions.LoadRemoteVersionInfoFile : Boolean;
begin
  Case LocationType of
    pvltDatabase : Result := DatabaseLocation.LoadFileFromRemote(DatabaseLocation.SelectStatementVersion, LocalVersionInfoFileName, LocalDirectory, LocalVersionInfoFileName, nil) <> '';
    pvltHTTP : Result := HTTPLocation.LoadFileFromRemote(HTTPLocation.LocationPathVersion, LocalVersionInfoFileName, LocalDirectory, LocalVersionInfoFileName, nil) <> '';
    pvltFTP : Result := FTPLocation.LoadFileFromRemote(FTPLocation.LocationPathVersion, LocalVersionInfoFileName, LocalDirectory, LocalVersionInfoFileName, nil) <> '';
  else
    Result := NetworkLocation.LoadFileFromRemote(NetworkLocation.LocationPathVersion, LocalVersionInfoFileName, LocalDirectory, LocalVersionInfoFileName, nil) <> '';
  end;
end;

function tJvProgramVersionCheckOptions.LoadRemoteInstallerFile (iProgramVersionInfo : TJvProgramVersionInfo;iBaseThread : TJvBaseThread) : String;
begin
//  sleep(5000);
  if Assigned(iProgramVersionInfo) then
    Case LocationType of
      pvltDatabase : Result := DatabaseLocation.LoadFileFromRemote(iProgramVersionInfo.ProgramLocationPath, iProgramVersionInfo.ProgramLocationFileName, LocalDirectory, LocalInstallerFileName, iBaseThread);
      pvltHTTP : Result := HTTPLocation.LoadFileFromRemote(iProgramVersionInfo.ProgramLocationPath, iProgramVersionInfo.ProgramLocationFileName, LocalDirectory, LocalInstallerFileName, iBaseThread);
      pvltFTP : Result := FTPLocation.LoadFileFromRemote(iProgramVersionInfo.ProgramLocationPath, iProgramVersionInfo.ProgramLocationFileName, LocalDirectory, LocalInstallerFileName, iBaseThread);
    else
      Result := NetworkLocation.LoadFileFromRemote(iProgramVersionInfo.ProgramLocationPath, iProgramVersionInfo.ProgramLocationFileName, LocalDirectory, LocalInstallerFileName, iBaseThread);
    end
  else
    Result := '';
end;

procedure tJvProgramVersionCheckOptions.SetSupportedLocationTypes (Value : tJvProgramVersionLocationTypes );
begin
  if not (pvltNetwork IN Value) then
    Value := Value + [pvltNetwork];
  fSupportedLocationTypes := Value;
  if IgnoreProperties.IndexOf('DatabaseLocation') >= 0 then
    IgnoreProperties.Delete(IgnoreProperties.IndexOf('DatabaseLocation'));
  if IgnoreProperties.IndexOf('FTPLocation') >= 0 then
    IgnoreProperties.Delete(IgnoreProperties.IndexOf('FTPLocation'));
  if IgnoreProperties.IndexOf('HTTPLocation') >= 0 then
    IgnoreProperties.Delete(IgnoreProperties.IndexOf('HTTPLocation'));
  if not (pvltDatabase IN Value) then
    IgnoreProperties.Add('DatabaseLocation');
  if not (pvltFTP IN Value) then
    IgnoreProperties.Add('FTPLocation');
  if not (pvltHTTP IN Value) then
    IgnoreProperties.Add('HTTPLocation');
end;

//=== { tJvProgramVersionCheck } =========================================

constructor tJvProgramVersionCheck.Create (AOwner : TComponent);
begin
  Inherited Create(AOwner);
  fRemoteProgramVersionHistory := tJvProgramVersionHistory.Create(self);
  fRemoteProgramVersionHistory.IgnoreLastLoadTime := True;
  fRemoteAppStorage := TJvAppIniFileStorage.Create(self);
  fRemoteAppStorage.Location := flCustom;
  fRemoteAppStorage.ReadOnly := True;
  fRemoteAppStorage.AutoReload := True;
  fRemoteAppStorage.DefaultSection := 'Version';
  with fRemoteAppStorage.StorageOptions do
  begin
    SetAsString := True;
    FloatAsString := True;
    DefaultIfReadConvertError := True;
    DateTimeAsString := True;
  end;
  fRemoteProgramVersionHistory.AppStorage := fRemoteAppStorage;
  fProgramVersionCheckOptions := tJvProgramVersionCheckOptions.Create(self);
  fCloseAfterInstallStarted := true;
  fThread := TJvThread.Create(self);
  fThread.Exclusive := True;
  fThread.RunOnCreate := True;
  fThread.FreeOnTerminate := True;
  fThreadDialog := TJvThreadAnimateDialog.Create(self);
  fThreadDialog.DialogOptions.ShowDialog := True;
  fThreadDialog.DialogOptions.ShowCancelButton := True;
  fThreadDialog.DialogOptions.ShowElapsedTime := True;
  TJvThreadAnimateDialogOptions(fThreadDialog.DialogOptions).commonAvi := aviCopyFile;
  fThread.ThreadDialog := fThreadDialog;
end;

destructor tJvProgramVersionCheck.destroy;
begin
  FreeAndNil(fThreadDialog);
  FreeAndNil(fThread);
  FreeAndNil(fProgramVersionCheckOptions);
  FreeAndNil(fRemoteAppStorage);
  Inherited Destroy;
end;

procedure tJvProgramVersionCheck.SetAppStorage (Value : TJvCustomAppStorage);
begin
  fAppStorage := Value;
  fProgramVersionCheckOptions.AppStorage := Value;
  fProgramVersionCheckOptions.LoadProperties;
end;

procedure tJvProgramVersionCheck.SetAppStoragePath (Value : string);
begin
  fAppStoragePath := Value;
  fProgramVersionCheckOptions.AppStoragePath := Value;
  fProgramVersionCheckOptions.LoadProperties;
end;

PROCEDURE tJvProgramVersionCheck.Notification(AComponent: TComponent; Operation: TOperation);
begin
  IF Operation = opRemove THEN
  begin
    IF (AComponent = fAppStorage) THEN
      fAppStorage := NIL;
  end;
end;

function tJvProgramVersionCheck.CurrentFileVersion : string;
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

function tJvProgramVersionCheck.CurrentApplicationName : string;
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

function tJvProgramVersionCheck.IsRemoteProgramVersionNewer : Boolean;
begin
  Result:= CompareVersionNumbers (CurrentFileVersion,GetAllowedRemoteProgramVersion) = 1;
end;

function tJvProgramVersionCheck.IsRemoteProgramVersionReleaseTypeNewer (iReleaseType : tJvProgramReleaseType) : Boolean;
begin
  if Assigned(RemoteProgramVersionHistory.CurrentProgramVersion[iReleaseType]) then
    Result:= CompareVersionNumbers (CurrentFileVersion, RemoteProgramVersionHistory.CurrentProgramVersion[iReleaseType].ProgramVersion) = 1
  else
    Result := False;
end;

procedure tJvProgramVersionCheck.ShowProgramVersionsDescription (const iFromVersion, iToVersion : string);
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

procedure tJvProgramVersionCheck.VersionInfoButtonClick(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter) ;
var prt : tJvProgramReleaseType;
begin
  prt := low(prt);
  Inc (prt,Parameter.Tag);
  with RemoteProgramVersionHistory do
    if Assigned(CurrentProgramVersion[prt]) then
      ShowProgramVersionsDescription (CurrentFileVersion, CurrentProgramVersion[prt].ProgramVersion);
end;

function  tJvProgramVersionCheck.GetRemoteVersionOperation (Var ReleaseType : tJvProgramReleaseType) : tJvRemoteVersionOperation;
Var ParameterList : TJvParameterList;
    GroupParameter : TJvGroupBoxParameter;
    Parameter : TJvBaseParameter;
    prt : tJvProgramReleaseType;
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

procedure tJvProgramVersionCheck.DownloadThreadOnExecute(Sender: TObject; Params: Pointer);
begin
  if Assigned(fExecuteVersionInfo) then
  begin
    fExecuteDownloadInstallFilename := ProgramVersionCheckOptions.LoadRemoteInstallerFile (fExecuteVersionInfo, fThread.LastThread);
    if (fExecuteDownloadInstallFilename <> '') and
        not FileExists(fExecuteDownloadInstallFilename) then
      fExecuteDownloadInstallFilename := '';
  end;
end;

procedure tJvProgramVersionCheck.DownloadThreadOnFinishAll(Sender : TObject);
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

function  tJvProgramVersionCheck.DownloadInstallerFromRemote : boolean;
begin
  if Assigned(fExecuteVersionInfo) then
  begin
    fThread.OnExecute := DownloadThreadOnExecute;
    fThread.OnFinishAll := DownloadThreadOnFinishAll;
    fThread.Execute(self);
  end;
end;

procedure tJvProgramVersionCheck.SetThreadInfo (const Info : string);
begin
  if Assigned(fThreadDialog) then
    fThreadDialog.DialogOptions.InfoText := Info;
end;

function  tJvProgramVersionCheck.GetAllowedRemoteProgramVersion : string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(ProgramVersionCheckOptions.AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(ProgramVersionCheckOptions.AllowedReleaseType).ProgramVersion
  else
    Result := '';
end;

function  tJvProgramVersionCheck.GetAllowedRemoteProgramVersionReleaseType : string;
begin
  if Assigned(RemoteProgramVersionHistory.AllowedCurrentProgramVersion(ProgramVersionCheckOptions.AllowedReleaseType)) then
    Result := RemoteProgramVersionHistory.AllowedCurrentProgramVersion(ProgramVersionCheckOptions.AllowedReleaseType).ProgramVersionReleaseType
  else
    Result := '';
end;

procedure tJvProgramVersionCheck.StoreRemoteVersionInfoToFile;
begin
  fRemoteAppStorage.ReadOnly := False;
  RemoteProgramVersionHistory.StoreProperties;
  fRemoteAppStorage.Flush;
  fRemoteAppStorage.ReadOnly := True;
end;

procedure tJvProgramVersionCheck.Execute ;
var ReleaseType : tJvProgramReleaseType;
begin
  fExecuteVersionInfo:= nil;
  with ProgramVersionCheckOptions do
  begin
    ProgramVersionCheckOptions.LoadProperties;
    if (ProgramVersionCheckOptions.LastCheck < now-ProgramVersionCheckOptions.CheckFrequency) then
    begin
      ProgramVersionCheckOptions.AllowedReleaseType := prtAlpha;
      ProgramVersionCheckOptions.LastCheck:= Now;
      ProgramVersionCheckOptions.LoadRemoteVersionInfoFile;
      RemoteAppStorage.FileName := PathAppend(ProgramVersionCheckOptions.LocalDirectory, ProgramVersionCheckOptions.LocalVersionInfoFileName);
      RemoteProgramVersionHistory.LoadProperties;
      ProgramVersionCheckOptions.StoreProperties;
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
