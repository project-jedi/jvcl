{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUrlGrabbers.Pas, released on 2003-08-04.

The Initial Developer of the Original Code is Olivier Sannier [obones@meloo.com]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-11-02

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvUrlGrabbers;

{$HPPEMIT '#pragma link "wininet.lib"'}

interface

uses
  Windows, Contnrs, Classes, SysUtils,
  JvTypes, JvFinalize;

type
  // forward declarations
  TJvUrlGrabberList = class;
  TJvUrlGrabberThread = class;
  TJvUrlGrabberDefaultProperties = class;
  TJvUrlGrabberThreadClass = class of TJvUrlGrabberThread;

  // a trick for the Delphi editor that allows to have a sub object
  // for each member of the a TJvUrlGrabberDefaultPropertiesList
  // Because an indexed property cannot be published, the editor
  // for TJvUrlGrabberDefaultPropertiesList enumerates all the
  // items in the list, and passes the EditorTrick property of
  // each of its TJvUrlGrabberDefaultProperties members. The
  // trick contains only one published property that gets displayed
  // and this property points to the TJvUrlGrabberDefaultPropertiesList
  // object to which the trick belongs, thus allowing to publish
  // the indexed property. The only drawback is that the name
  // in the property editor for each object is the same, DefaultProperties
  // Hence, the need for an editor for TJvUrlGrabberDefPropEdTrick that
  // displays a meaningful name instead
  TJvUrlGrabberDefPropEdTrick = class(TPersistent)
  private
    FDefaultProperties: TJvUrlGrabberDefaultProperties;
  public
    constructor Create(GrabberDefaults: TJvUrlGrabberDefaultProperties);
  published
    property DefaultProperties: TJvUrlGrabberDefaultProperties read FDefaultProperties;
  end;

  // A container for Default properties, and a list of such
  // containers
  TJvUrlGrabberDefaultProperties = class(TPersistent)
  private
    FEditorTrick: TJvUrlGrabberDefPropEdTrick;
  protected
    // agent to impersonate
    FAgent: string;
    // user information
    FUserName: string;
    FPassword: string;
    // filename to use
    FFileName: TFileName;
    // output mode (stream or file)
    FOutputMode: TJvOutputMode;
  public
    constructor Create;
    destructor Destroy; override;
    property EditorTrick: TJvUrlGrabberDefPropEdTrick read FEditorTrick;
  published
    // the user name and password to use for authentication
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    // the name of the file to write to if OutputMode is omFile
    property FileName: TFileName read FFileName write FFileName;
    // The output mode
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
    // The agent to impersonate
    property Agent: string read FAgent write FAgent;
  end;

  TJvUrlGrabberDefaultPropertiesClass = class of TJvUrlGrabberDefaultProperties;

  TJvUrlGrabberDefaultPropertiesList = class(TPersistent)
  protected
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TJvUrlGrabberDefaultProperties;
    procedure SetItems(Index: Integer; const Value: TJvUrlGrabberDefaultProperties);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Item: TJvUrlGrabberDefaultProperties);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJvUrlGrabberDefaultProperties read GetItems write SetItems;
  end;

  // the status of a grabber
  TJvGrabberStatus = (gsStopped, gsConnecting, gsGrabbing);

  // The main class. It is the ancestor of all the Url Grabbers
  // and declares the required methods that a grabber must provide
  // Do not instanciate a TJvUrlGrabber directly, simply use one
  // of its descendants. This family of classes is used by
  // TJvUrlListGrabber to allow downloading a list of URLs

  TJvUrlGrabber = class(TObject)
  protected
    // the thread that will grab for us
    FUrlGrabberThread: TJvUrlGrabberThread;
    // events
    FOnDoneFile: TJvDoneFileEvent; // file is done
    FOnDoneStream: TJvDoneStreamEvent; // stream is done
    FOnError: TJvErrorEvent; // error occured
    FOnProgress: TJvFTPProgressEvent; // download progressed a bit
    FOnClosed: TNotifyEvent; // connection is closed
    FOnReceiving: TNotifyEvent; // beginning to receive
    FOnReceived: TNotifyEvent; // end of reception
    FOnConnecting: TNotifyEvent; // beginning of connection
    FOnResolving: TNotifyEvent; // beginning of resolving URL
    FOnRedirect: TNotifyEvent; // redirection happened
    FOnConnected: TNotifyEvent; // now connected to host
    FOnStateChange: TNotifyEvent; // state of connection changed
    FOnResolved: TNotifyEvent; // name has been resolved
    FOnClosing: TNotifyEvent; // beginning of close of connection
    FOnRequest: TNotifyEvent; // sending a request
    FOnSent: TNotifyEvent; // data sent
    FOnSending: TNotifyEvent; // beginning to send data
    // current status of the grabber
    FStatus: TJvGrabberStatus;
    // URL to grab
    FUrl: string;
    // the stream to grab into.
    FStream: TMemoryStream;
    FTotalBytes: Int64;
    FBytesRead: Int64;
    // agent to impersonate
    FAgent: string;
    // user information
    FUserName: string;
    FPassword: string;
    // filename to use
    FFileName: TFileName;
    // output mode (stream or file)
    FOutputMode: TJvOutputMode;
    procedure DoError(ErrorMsg: string);
    procedure DoProgress(Status: DWORD);
    procedure DoEnded;
    procedure DoClosed;
    function GetGrabberThreadClass: TJvUrlGrabberThreadClass; virtual; abstract;
  public
    constructor Create(AUrl: string; DefaultProperties: TJvUrlGrabberDefaultProperties); virtual;
    destructor Destroy; override;
    // this function must return True if the given URL can be grabbed
    // by the class being asked. It returns False otherwise
    // It MUST be overriden in the derived classes but cannot be abstract
    // because of BCB compatibility issues
    class function CanGrab(const Url: string): Boolean; virtual;
    // This function returns the class of a property holder to
    // be displayed in the object inspector. This property holder
    // will be used by TJvUrlListGrabber to let the user specify default
    // properties and will be passed to this class when created to
    // handle a specific URL.
    // It MUST be overriden in the derived classes but cannot be abstract
    // because of BCB compatibility issues
    class function GetDefaultPropertiesClass: TJvUrlGrabberDefaultPropertiesClass; virtual;
    // Asks to Start to grab the URL
    procedure Start; virtual;
    // Asks to Stop to grab the URL
    procedure Stop; virtual;
    // The status of the grab
    property Status: TJvGrabberStatus read FStatus;
    // the Url being grabbed
    property Url: string read FUrl;
    // the user name and password to use for authentication
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    // the name of the file to write to if OutputMode is omFile
    property FileName: TFileName read FFileName write FFileName;
    // The output mode
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
    // The agent to impersonate
    property Agent: string read FAgent write FAgent;
    // Events
    property OnDoneFile: TJvDoneFileEvent read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TJvDoneStreamEvent read FOnDoneStream write FOnDoneStream;
    property OnError: TJvErrorEvent read FOnError write FOnError;
    property OnProgress: TJvFTPProgressEvent read FOnProgress write FOnProgress;
    property OnResolvingName: TNotifyEvent read FOnResolving write FOnResolving;
    property OnNameResolved: TNotifyEvent read FOnResolved write FOnResolved;
    property OnConnectingToServer: TNotifyEvent read FOnConnecting write FOnConnecting;
    property OnConnectedToServer: TNotifyEvent read FOnConnected write FOnConnected;
    property OnSendingRequest: TNotifyEvent read FOnSending write FOnSending;
    property OnRequestSent: TNotifyEvent read FOnSent write FOnSent;
    property OnRequestComplete: TNotifyEvent read FOnRequest write FOnRequest;
    property OnReceivingResponse: TNotifyEvent read FOnReceiving write FOnReceiving;
    property OnResponseReceived: TNotifyEvent read FOnReceived write FOnReceived;
    property OnClosingConnection: TNotifyEvent read FOnClosing write FOnClosing;
    property OnConnectionClosed: TNotifyEvent read FOnClosed write FOnClosed;
    property OnRedirect: TNotifyEvent read FOnRedirect write FOnRedirect;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

  // A grabber for FTP URLs
  TJvFtpDownloadMode = (hmBinary, hmAscii);

  TJvFtpUrlGrabberDefaultProperties = class(TJvUrlGrabberDefaultProperties)
  protected
    FPassive: Boolean;
    FMode: TJvFtpDownloadMode;
  published
    property Passive: Boolean read FPassive write FPassive;
    property Mode: TJvFtpDownloadMode read FMode write FMode;
  end;

  TJvFtpUrlGrabber = class(TJvUrlGrabber)
  protected
    FPassiveFTP: Boolean;
    FMode: TJvFtpDownloadMode;
    FSize: Int64;
    function GetGrabberThreadClass: TJvUrlGrabberThreadClass; override;
  public
    constructor Create(AUrl: string; DefaultProperties: TJvUrlGrabberDefaultProperties); override;
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvUrlGrabberDefaultPropertiesClass; override;
    property Size: Int64 read FSize;
  published
    property Passive: Boolean read FPassiveFTP write FPassiveFTP;
    property Mode: TJvFtpDownloadMode read FMode write FMode;
  end;

  // A grabber for HTTP URLs
  TJvHttpUrlGrabberDefaultProperties = class(TJvUrlGrabberDefaultProperties);

  TJvHttpUrlGrabber = class(TJvUrlGrabber)
  protected
    function GetGrabberThreadClass: TJvUrlGrabberThreadClass; override;
  public
    constructor Create(AUrl: string; DefaultProperties: TJvUrlGrabberDefaultProperties); override;
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvUrlGrabberDefaultPropertiesClass; override;
  end;

  // A thread that will grab the given URL in the background
  // this is the ancestor of all the grabber threads, and there
  // should be as many descendants as there are TJvUrlGrabber descendants.
  TJvUrlGrabberThread = class(TThread)
  protected
    FErrorText: string; // the error string received from the server
    FGrabber: TJvUrlGrabber;
    FStatus: DWORD;
    procedure Error;
    procedure Progress;
    procedure Ended;
    procedure ParseUrl(Value: string; Protocol: string; var Host: string; var FileName: string);
  public
    constructor Create(Grabber: TJvUrlGrabber); virtual;
  end;

  // a grabbing thread for HTTP URLs
  TJvHttpUrlGrabberThread = class(TJvUrlGrabberThread)
  protected
    FReferer: string;
    FContinue: Boolean;
    function GetGrabber: TJvHttpUrlGrabber;
    procedure Execute; override;
  public
    constructor Create(Grabber: TJvUrlGrabber); override;
    property Grabber: TJvHttpUrlGrabber read GetGrabber;
  end;

  // a grabbing thread for FTP URLs
  TJvFtpUrlGrabberThread = class(TJvUrlGrabberThread)
  protected
    function GetGrabber: TJvFtpUrlGrabber;
    procedure Closed;
    procedure Execute; override;
  public
    property Grabber: TJvFtpUrlGrabber read GetGrabber;
  end;

  // A list of instances of TJvUrlGrabber descendants
  // This is used internally by TJvUrlListGrabber to keep track of
  // the objects in charge of every URLs it has to grab

  TJvUrlGrabberList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TJvUrlGrabber;
    procedure SetItem(Index: Integer; const AGrabber: TJvUrlGrabber);
  public
    function Add(AGrabber: TJvUrlGrabber): integer;
    procedure Insert(Index: integer; AGrabber: TJvUrlGrabber);
    property Items[Index: Integer]: TJvUrlGrabber read GetItem write SetItem; default;
  end;

  TJvUrlGrabberClass = class of TJvUrlGrabber;

  // A list of classes inheriting from TJvUrlGrabber
  // This is the type of list used by the JvUrlGrabberClassList
  // function that returns all the registered classes.
  // This list is then used by TJvUrlListGrabber to determine which
  // class is best suited for handling a given URL

  TJvUrlGrabberClassList = class(TClassList)
  protected
    function GetItem(Index: Integer): TJvUrlGrabberClass;
    procedure SetItem(Index: Integer; const AGrabberClass: TJvUrlGrabberClass);
  public
    procedure Populate(DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList);
    function Add(AGrabberClass: TJvUrlGrabberClass): integer;
    procedure Insert(Index: integer; AGrabberClass: TJvUrlGrabberClass);
    function CreateFor(Url: string; DefaultPropertiesCollection: TJvUrlGrabberDefaultPropertiesList): TJvUrlGrabber;
    property Items[Index: Integer]: TJvUrlGrabberClass read GetItem write SetItem; default;
  end;

function JvUrlGrabberClassList: TJvUrlGrabberClassList;

implementation

uses
  WinInet;

const
  sUnitName = 'JvUrlGrabbers';

{$IFNDEF COMPILER6_UP}
function FtpGetFileSize(hFile: HINTERNET; lpdwFileSizeHigh: LPDWORD): DWORD; stdcall;
  external 'wininet.dll' name 'FtpGetFileSize';
{$ENDIF COMPILER6_UP}

var
  // the global object to contain the list of registered
  // url grabber classes
  GJvUrlGrabberClassList: TJvUrlGrabberClassList = nil;

function JvUrlGrabberClassList: TJvUrlGrabberClassList;
begin
  if not Assigned(GJvUrlGrabberClassList) then
  begin
    // create the object
    GJvUrlGrabberClassList := TJvUrlGrabberClassList.Create;
    AddFinalizeObjectNil(sUnitName, TObject(GJvUrlGrabberClassList));

    // register the classes
    GJvUrlGrabberClassList.Add(TJvFtpUrlGrabber);
    GJvUrlGrabberClassList.Add(TJvHttpUrlGrabber);
  end;
  Result := GJvUrlGrabberClassList;
end;

// global download callback

procedure DownloadCallBack(Handle: HInternet; Context: DWord;
  Status: DWord; Info: Pointer; StatLen: DWord); stdcall;
begin
  with TJvUrlGrabberThread(Context) do
  begin
    FStatus := Status;
    Synchronize(Progress);
  end;
end;

//=== TJvUrlGrabber ==========================================================

constructor TJvUrlGrabber.Create(AUrl: string; DefaultProperties: TJvUrlGrabberDefaultProperties);
begin
  inherited Create;
  FUrl := AUrl;
  FUrlGrabberThread := nil;

  // get values from the default properties
  Agent := DefaultProperties.Agent;
  UserName := DefaultProperties.UserName;
  Password := DefaultProperties.Password;
  FileName := DefaultProperties.FileName;
  OutputMode := DefaultProperties.OutputMode;
end;

destructor TJvUrlGrabber.Destroy;
begin
  FUrlGrabberThread.Free;
  inherited Destroy;
end;

class function TJvUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  // useless implementation required for BCB compatibility as
  // C++ doesn't support abstract virtual class methods
  Result := False;
end;

procedure TJvUrlGrabber.DoClosed;
begin
  if Assigned(FOnClosed) then
    FOnClosed(Self);
end;

procedure TJvUrlGrabber.DoEnded;
begin
  FStream.Position := 0;
  if FOutputMode = omStream then
  begin
    if Assigned(FOnDoneStream) then
      FOnDoneStream(Self, FStream, FStream.Size, FUrl)
  end
  else
  begin
    FStream.SaveToFile(FFileName);
    if Assigned(FOnDoneFile) then
      FOnDoneFile(Self, FFileName, FStream.Size, FUrl);
  end;
end;

procedure TJvUrlGrabber.DoError(ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(self, ErrorMsg);
end;

procedure TJvUrlGrabber.DoProgress(Status: DWORD);
begin
  if Assigned(FOnProgress) then
    FOnProgress(self, Status, FUrl);
end;

class function TJvUrlGrabber.GetDefaultPropertiesClass: TJvUrlGrabberDefaultPropertiesClass;
begin
  // useless implementation for BCB Compatibility
  Result := nil;
end;

procedure TJvUrlGrabber.Start;
begin
  FUrlGrabberThread := GetGrabberThreadClass.Create(Self);
  FUrlGrabberThread.Resume;
end;

procedure TJvUrlGrabber.Stop;
begin
  FUrlGrabberThread.Terminate;
end;

//=== TJvHttpUrlGrabber ======================================================

constructor TJvHttpUrlGrabber.Create(AUrl: string;
  DefaultProperties: TJvUrlGrabberDefaultProperties);
begin
  inherited Create(AUrl, DefaultProperties);
end;

class function TJvHttpUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  Result := LowerCase(Copy(Url, 1, 7)) = 'http://';
end;

class function TJvHttpUrlGrabber.GetDefaultPropertiesClass: TJvUrlGrabberDefaultPropertiesClass;
begin
  Result := TJvHttpUrlGrabberDefaultProperties;
end;

function TJvHttpUrlGrabber.GetGrabberThreadClass: TJvUrlGrabberThreadClass;
begin
  Result := TJvHttpUrlGrabberThread;
end;

//=== TJvFtpUrlGrabber =======================================================

constructor TJvFtpUrlGrabber.Create(AUrl: string;
  DefaultProperties: TJvUrlGrabberDefaultProperties);
begin
  inherited Create(AUrl, DefaultProperties);
  Passive := TJvFtpUrlGrabberDefaultProperties(DefaultProperties).Passive;
  Mode := TJvFtpUrlGrabberDefaultProperties(DefaultProperties).Mode;
end;

class function TJvFtpUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  Result := LowerCase(Copy(Url, 1, 6)) = 'ftp://';
end;

class function TJvFtpUrlGrabber.GetDefaultPropertiesClass: TJvUrlGrabberDefaultPropertiesClass;
begin
  Result := TJvFtpUrlGrabberDefaultProperties;
end;

function TJvFtpUrlGrabber.GetGrabberThreadClass: TJvUrlGrabberThreadClass;
begin
  Result := TJvFtpUrlGrabberThread;
end;

//=== TJvUrlGrabberList ======================================================

function TJvUrlGrabberList.Add(AGrabber: TJvUrlGrabber): integer;
begin
  Result := inherited Add(AGrabber);
end;

function TJvUrlGrabberList.GetItem(Index: Integer): TJvUrlGrabber;
begin
  Result := TJvUrlGrabber(inherited Items[Index]);
end;

procedure TJvUrlGrabberList.Insert(Index: Integer; AGrabber: TJvUrlGrabber);
begin
  inherited Insert(Index, AGrabber);
end;

procedure TJvUrlGrabberList.SetItem(Index: Integer; const AGrabber: TJvUrlGrabber);
begin
  inherited Items[Index] := AGrabber;
end;

//=== TJvUrlGrabberClassList =================================================

function TJvUrlGrabberClassList.Add(AGrabberClass: TJvUrlGrabberClass): integer;
begin
  Result := inherited Add(AGrabberClass);
end;

function TJvUrlGrabberClassList.CreateFor(Url: string;
  DefaultPropertiesCollection: TJvUrlGrabberDefaultPropertiesList): TJvUrlGrabber;
var
  I: Integer;
begin
  I := 0;
  Result := nil;
  while (I < Count) and not Assigned(Result) do
  begin
    if Items[I].CanGrab(Url) then
      Result := Items[I].Create(Url, DefaultPropertiesCollection.Items[I]);
    Inc(I);
  end;
end;

function TJvUrlGrabberClassList.GetItem(Index: Integer): TJvUrlGrabberClass;
begin
  Result := TJvUrlGrabberClass(inherited Items[Index]);
end;

procedure TJvUrlGrabberClassList.Insert(Index: Integer;
  AGrabberClass: TJvUrlGrabberClass);
begin
  inherited Insert(Index, AGrabberClass);
end;

procedure TJvUrlGrabberClassList.Populate(
  DefaultPropertiesList: TJvUrlGrabberDefaultPropertiesList);
var
  I: Integer;
begin
  DefaultPropertiesList.Clear;
  for I := 0 to Count - 1 do
    DefaultPropertiesList.Add(Items[I].GetDefaultPropertiesClass.Create);
end;

procedure TJvUrlGrabberClassList.SetItem(Index: Integer;
  const AGrabberClass: TJvUrlGrabberClass);
begin
  inherited Items[Index] := AGrabberClass;
end;

//=== TJvUrlGrabberThread ====================================================

constructor TJvUrlGrabberThread.Create(Grabber: TJvUrlGrabber);
begin
  inherited Create(True);
  FGrabber := Grabber;
end;

procedure TJvUrlGrabberThread.Ended;
begin
  FGrabber.DoEnded;
end;

procedure TJvUrlGrabberThread.Error;
begin
  FGrabber.DoError(FErrorText);
end;

procedure TJvUrlGrabberThread.Progress;
begin
  FGrabber.DoProgress(FStatus);
end;

procedure TJvUrlGrabberThread.ParseUrl(Value: string; Protocol: string;
  var Host: string; var FileName: string);
begin
  Host := '';
  FileName := '';
  if Pos(UpperCase(Protocol), UpperCase(Value)) <> 0 then
    Value := Copy(Value, 8, Length(Value));
  if Pos('/', Value) <> 0 then
  begin
    Host := Copy(Value, 1, Pos('/', Value) - 1);
    FileName := Copy(Value, Pos('/', Value) + 1, Length(Value));
  end
  else
    Host := Value;
end;

//=== TJvFtpUrlGrabberThread =================================================

procedure TJvFtpUrlGrabberThread.Closed;
begin
  Grabber.DoClosed;
end;

procedure TJvFtpUrlGrabberThread.Execute;
const
  cPassive: array [Boolean] of DWORD = (0, INTERNET_FLAG_PASSIVE);
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName: string;
  UserName, Password: PChar;
  BytesRead, TotalBytes: DWORD;
  Buf: array [0..1023] of Byte;
  dwFileSizeHigh: DWORD;
  Buffer: Pointer;
  dwBufLen, dwIndex: DWORD;
begin
  // (rom) secure thread against exceptions
  Grabber.FStream := nil;
  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  try
    try
      FErrorText := '';
      ParseUrl(Grabber.FUrl, 'ftp://', HostName, FileName);

      // Connect to the web
      hSession := InternetOpen(PChar(Grabber.FAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
      if hSession = nil then
      begin
        FErrorText := SysErrorMessage(GetLastError);
        Synchronize(Error);
        Exit;
      end;

      // Connect to the hostname
      if Grabber.FUserName = '' then
        UserName := nil
      else
        UserName := PChar(Grabber.FUserName);
      if Grabber.FPassword = '' then
        Password := nil
      else
        Password := PChar(Grabber.FPassword);
      hHostConnection := InternetConnect(hSession, PChar(HostName), INTERNET_DEFAULT_FTP_PORT,
        UserName, Password, INTERNET_SERVICE_FTP, cPassive[Grabber.FPassiveFTP], 0);
      if hHostConnection = nil then
      begin
        dwIndex := 0;
        dwBufLen := 1024;
        GetMem(Buffer, dwBufLen);
        InternetGetLastResponseInfo(dwIndex, Buffer, dwBufLen);
        FErrorText := StrPas(Buffer);
        FreeMem(Buffer);

        Synchronize(Error);
        Exit;
      end;

      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@DownloadCallBack));

      // Request the file
      if Grabber.FMode = hmBinary then
        hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ, FTP_TRANSFER_TYPE_BINARY or
          INTERNET_FLAG_DONT_CACHE, 0)
      else
        hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ, FTP_TRANSFER_TYPE_ASCII or
          INTERNET_FLAG_DONT_CACHE, 0);

      if hDownload = nil then
      begin
        FErrorText := SysErrorMessage(GetLastError);
        Synchronize(Error);
        Exit;
      end;
      Grabber.FSize := FtpGetFileSize(hDownload, @dwFileSizeHigh); // acp

      Grabber.FStream := TMemoryStream.Create;

      TotalBytes := 0;
      BytesRead := 1;
      while (BytesRead <> 0) and not Terminated do // acp
      begin
        if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), BytesRead) then
          BytesRead := 0
        else
        begin
          Inc(TotalBytes, BytesRead);
          Grabber.FBytesRead := TotalBytes;
          Grabber.FStream.Write(Buf, BytesRead);
          Synchronize(Progress);
        end;
      end;
      if not Terminated then // acp
        Synchronize(Ended);
    except
    end;
  finally
    //Free all stuff's
    Grabber.FStream.Free;

    //Release all handles
    // (rom) now all connections get closed and Closed is always signalled
    if (hDownload <> nil) and not InternetCloseHandle(hDownload) then
    begin
      FErrorText := SysErrorMessage(GetLastError);
      Synchronize(Error);
    end;
    if (hHostConnection <> nil) and not InternetCloseHandle(hHostConnection) then
    begin
      FErrorText := SysErrorMessage(GetLastError);
      Synchronize(Error);
    end;
    if (hSession <> nil) and not InternetCloseHandle(hSession) then
    begin
      FErrorText := SysErrorMessage(GetLastError);
      Synchronize(Error);
    end;
    Synchronize(Closed);
  end;
end;

function TJvFtpUrlGrabberThread.GetGrabber: TJvFtpUrlGrabber;
begin
  Result := TJvFtpUrlGrabber(FGrabber);
end;

//=== TJvHttpUrlGrabberThread ================================================

constructor TJvHttpUrlGrabberThread.Create(Grabber: TJvUrlGrabber);
begin
  inherited Create(Grabber);
  FContinue := True;
end;

procedure TJvHttpUrlGrabberThread.Execute;
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName: string;
  Username, Password: PChar;
  Buffer: PChar;
  dwBufLen, dwIndex, dwBytesRead, dwTotalBytes: DWORD;
  HasSize: Boolean;
  Buf: array [0..1024] of Byte;

begin
  // (rom) secure thread against exceptions
  Buffer := nil;

  Grabber.FStream := nil;
  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  try
    try
      ParseUrl(Grabber.FUrl, 'http://', HostName, FileName);
      FErrorText := '';

      //Connect to the web
      hSession := InternetOpen(PChar(Grabber.FAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
      if hSession = nil then
      begin
        FErrorText := SysErrorMessage(GetLastError);
        Synchronize(Error);
        Exit;
      end;

      // Connect to the hostname
      if Grabber.FUsername = '' then
        Username := nil
      else
        Username := PChar(Grabber.FUsername);
      if Grabber.FPassword = '' then
        Password := nil
      else
        Password := PChar(Grabber.FPassword);
      hHostConnection := InternetConnect(hSession, PChar(HostName), INTERNET_DEFAULT_HTTP_PORT,
        Username, Password, INTERNET_SERVICE_HTTP, 0, DWORD(Self));
      if hHostConnection = nil then
      begin
        dwIndex := 0;
        dwBufLen := 1024;
        GetMem(Buffer, dwBufLen);
        InternetGetLastResponseInfo(dwIndex, Buffer, dwBufLen);
        FErrorText := Buffer;
        FreeMem(Buffer);
        Error;
        Exit;
      end;

//      FCriticalSection.Enter;
      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@DownloadCallBack));
      //Request the file
      // (rom) any difference here?
      hDownload := HttpOpenRequest(hHostConnection, 'GET', PChar(FileName), 'HTTP/1.0', PChar(FReferer),
        nil, INTERNET_FLAG_RELOAD, 0);
//      FCriticalSection.Leave;

      if hDownload = nil then
      begin
        FErrorText := SysErrorMessage(GetLastError);
        Synchronize(Error);
        Exit;
      end;

//      FCriticalSection.Enter;
      //Send the request
      HttpSendRequest(hDownload, nil, 0, nil, 0);

      Grabber.FStream := TMemoryStream.Create;

      dwIndex := 0;
      dwBufLen := 1024;
      GetMem(Buffer, dwBufLen);
      HasSize := HttpQueryInfo(hDownload, HTTP_QUERY_CONTENT_LENGTH, Buffer, dwBufLen, dwIndex);
      if HasSize then
        Grabber.FTotalBytes := StrToInt(StrPas(Buffer))
      else
        Grabber.FTotalBytes := 0;

      dwTotalBytes := 0;
      if HasSize then
      begin
        dwBytesRead := 1;
        while (dwBytesRead > 0) and not Terminated do
        begin
          if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) then
            dwBytesRead := 0
          else
          begin
            Inc(dwTotalBytes, dwBytesRead);
            Grabber.FBytesRead := dwTotalBytes;
            Grabber.FStream.Write(Buf, dwBytesRead);
            Synchronize(Progress);
          end;
        end;
        if FContinue and not Terminated then
          Synchronize(Ended);
//        FCriticalSection.Leave;
      end
      else
      begin
//        FCriticalSection.Enter;
        while InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) and not Terminated do
        begin
          if dwBytesRead = 0 then
            Break;
//          Inc(dwTotalBytes,dwBytesRead);
          Grabber.FStream.Write(Buf, dwBytesRead);
          Synchronize(Progress);
        end;
        if FContinue and not Terminated then
          Synchronize(Ended);
//        FCriticalSection.Leave;
      end;
    except
    end;
  finally
    // Free all stuff's
    if Buffer <> nil then
      FreeMem(Buffer);
    Grabber.FStream.Free;

    // Release all handles
    if hDownload <> nil then
      InternetCloseHandle(hDownload);
    if hHostConnection <> nil then
      InternetCloseHandle(hHostConnection);
    if hSession <> nil then
      InternetCloseHandle(hSession);
  end;
end;

function TJvHttpUrlGrabberThread.GetGrabber: TJvHttpUrlGrabber;
begin
  Result := TJvHttpUrlGrabber(FGrabber);
end;

//=== TJvUrlGrabberDefaultPropertiesCollection ===============================

constructor TJvUrlGrabberDefaultPropertiesList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create(True);
  JvUrlGrabberClassList.Populate(Self);
end;

destructor TJvUrlGrabberDefaultPropertiesList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJvUrlGrabberDefaultPropertiesList.Add(Item: TJvUrlGrabberDefaultProperties);
begin
  FItems.Add(Item);
end;

procedure TJvUrlGrabberDefaultPropertiesList.Clear;
begin
  FItems.Clear;
end;

function TJvUrlGrabberDefaultPropertiesList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvUrlGrabberDefaultPropertiesList.GetItems(
  Index: Integer): TJvUrlGrabberDefaultProperties;
begin
  Result := TJvUrlGrabberDefaultProperties(FItems[Index]);
end;

procedure TJvUrlGrabberDefaultPropertiesList.SetItems(Index: Integer;
  const Value: TJvUrlGrabberDefaultProperties);
begin
  FItems[Index] := Value;
end;

//=== TJvUrlGrabberDefPropEdTrick ============================================

constructor TJvUrlGrabberDefPropEdTrick.Create(
  GrabberDefaults: TJvUrlGrabberDefaultProperties);
begin
  FDefaultProperties := GrabberDefaults;
end;

//=== TJvUrlGrabberDefaultProperties =========================================

constructor TJvUrlGrabberDefaultProperties.Create;
begin
  inherited Create;
  FEditorTrick := TJvUrlGrabberDefPropEdTrick.Create(Self);
  FFileName := 'output.txt';
end;

destructor TJvUrlGrabberDefaultProperties.Destroy;
begin
  FEditorTrick.Free;
  inherited Destroy;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

