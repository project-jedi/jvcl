{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMultiHTTPGrabber.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

{$HPPEMIT '#pragma link "wininet.lib"'}

unit JvMultiHTTPGrabber;

interface

uses
  Windows, SysUtils, Classes, WinInet,
  JvTypes, JvComponent;

type
  TUrlEvent = procedure(Sender: TObject; UserData: Integer; Url: string) of object;
  TUrlEventError = procedure(Sender: TObject; UserData: Integer;
    Url: string; Error: string) of object;
  TUrlResolved = procedure(Sender: TObject; UserData: Integer; Url: string; Name: string) of object;
  TUrlRedirect = procedure(Sender: TObject; UserData: Integer;
    Url: string; NewUrl: string) of object;
  TUrlSent = procedure(Sender: TObject; UserData: Integer;
    Url: string; DataSize: Integer) of object;
  TJvDoneFileEvent = procedure(Sender: TObject; UserData: Integer; FileName: string;
    FileSize: Integer; Url: string) of object;
  TJvDoneStreamEvent = procedure(Sender: TObject; UserData: Integer; Stream: TStream;
    StreamSize: Integer; Url: string) of object;
  TDateEvent = procedure(Sender: TObject; UserData: Integer; FileDate: TDateTime;
    Url: string) of object;

  TJvMultiHTTPGrabber = class(TJvComponent)
  private
    FAgent: string;
    FUrl: string;
    FReferer: string;
    FPassword: string;
    FUserName: string;
    FOutputMode: TJvOutputMode;
    FFileName: TFileName;
    FOnDoneFile: TJvDoneFileEvent;
    FOnDoneStream: TJvDoneStreamEvent;
    FOnProgress: TJvHTTPProgressEvent;
    FOnReceived: TUrlSent;
    FOnReceivingResponse: TUrlEvent;
    FOnClosed: TUrlEvent;
    FOnConnecting: TUrlEvent;
    FOnResolving: TUrlEvent;
    FOnRedirect: TUrlRedirect;
    FOnConnected: TUrlEvent;
    FOnResolved: TUrlResolved;
    FOnClosing: TUrlEvent;
    FOnRequestComplete: TUrlEvent;
    FOnRequestSent: TUrlSent;
    FOnSendingRequest: TUrlEvent;
    FOnError: TUrlEventError;
    FCount: Integer;
    FOnDateRetrieved: TDateEvent;
    function GetWorking: Boolean;
    procedure RaiseWebError(Infos: Pointer);
  protected
    procedure RaiseError(Value: Pointer);
    function StartConnection(UserData: Integer; IgnoreMessages: Boolean = False): Pointer;
    procedure StopConnection(Infos: Pointer);
    procedure ThreadTerminated(Sender: TObject);
    procedure ThreadDateTerminated(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Download(UserData: Integer = 0);
    procedure GetFileAge(UserData: Integer = 0);
    property DownloadCount: Integer read FCount;
  published
    property Agent: string read FAgent write FAgent;
    property FileName: TFileName read FFileName write FFileName;
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
    property Password: string read FPassword write FPassword;
    property Referer: string read FReferer write FReferer;
    property Url: string read FUrl write FUrl;
    property UserName: string read FUserName write FUserName;
    property Working: Boolean read GetWorking;
    property OnClosingConnection: TUrlEvent read FOnClosing write FOnClosing;
    property OnClosedConnection: TUrlEvent read FOnClosed write FOnClosed;
    property OnConnectingToServer: TUrlEvent read FOnConnecting write FOnConnecting;
    property OnConnectedToServer: TUrlEvent read FOnConnected write FOnConnected;
    property OnDoneFile: TJvDoneFileEvent read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TJvDoneStreamEvent read FOnDoneStream write FOnDoneStream;
    property OnError: TUrlEventError read FOnError write FOnError;
    property OnProgress: TJvHTTPProgressEvent read FOnProgress write FOnProgress;
    property OnReceivingResponse: TUrlEvent read FOnReceivingResponse write FOnReceivingResponse;
    property OnReceivedResponse: TUrlSent read FOnReceived write FOnReceived;
    property OnRedirect: TUrlRedirect read FOnRedirect write FOnRedirect;
    property OnRequestComplete: TUrlEvent read FOnRequestComplete write FOnRequestComplete;
    property OnRequestSent: TUrlSent read FOnRequestSent write FOnRequestSent;
    property OnResolvingName: TUrlEvent read FOnResolving write FOnResolving;
    property OnResolvedName: TUrlResolved read FOnResolved write FOnResolved;
    property OnSendingRequest: TUrlEvent read FOnSendingRequest write FOnSendingRequest;
    property OnDateRetrieved: TDateEvent read FOnDateRetrieved write FOnDateRetrieved;
  end;

implementation

uses
  JvResources;

type
  PRequestInfos = ^TRequestInfos;
  TRequestInfos = record
    Url: string;
    Filename:string;
    OutputMode: TJvOutputMode;
    hSession: HINTERNET;
    hHostConnect: HINTERNET;
    hRequest: HINTERNET;
    FileSize: Integer;
    IgnoreMsg: Boolean;
    Grabber: TJvMultiHTTPGrabber;
    UserData: Integer;
  end;

  TJvMultiHttpThread = class(TThread)
  private
    FInfos: Pointer;
    FPosition: Integer;
    FContinue: Boolean;
    FStream: TMemoryStream;
  protected
    procedure Execute; override;
    procedure Progress;
    procedure Error;
  public
    constructor Create(Value: Pointer);
    destructor Destroy; override;
  end;

  TJvMultiDateHttpThread = class(TThread)
  private
    FInfos: Pointer;
    FValue: TDateTime;
  protected
    procedure Execute; override;
    procedure Error;
  public
    constructor Create(Value: Pointer);
  end;

//=== TJvMultiHTTPGrabber ====================================================

constructor TJvMultiHTTPGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOutputMode := omStream;
  FAgent := RsMultiAgent;
  FCount := 0;
end;

procedure StatusCallback(Handle: HInternet; Context: DWord;
  Status: DWord; Info: Pointer; StatLen: DWord); stdcall;
begin
  with PRequestInfos(Context)^ do
    if not IgnoreMsg then
      case Status of
        INTERNET_STATUS_CLOSING_CONNECTION:
          if Assigned(Grabber.FOnClosing) then
            Grabber.FOnClosing(Grabber, UserData, Url);
        INTERNET_STATUS_CONNECTED_TO_SERVER:
          if Assigned(Grabber.FOnConnected) then
            Grabber.FOnConnected(Grabber, UserData, Url);
        INTERNET_STATUS_CONNECTING_TO_SERVER:
          if Assigned(Grabber.FOnConnecting) then
            Grabber.FOnConnecting(Grabber, UserData, Url);
        INTERNET_STATUS_NAME_RESOLVED:
          if Assigned(Grabber.FOnResolved) then
            Grabber.FOnResolved(Grabber, UserData, Url, StrPas(PChar(Info)));
        INTERNET_STATUS_RECEIVING_RESPONSE:
          if Assigned(Grabber.FOnReceivingResponse) then
            Grabber.FOnReceivingResponse(Grabber, UserData, Url);
        INTERNET_STATUS_REDIRECT:
          if Assigned(Grabber.FOnRedirect) then
            Grabber.FOnRedirect(Grabber, UserData, Url, StrPas(PChar(Info)));
        INTERNET_STATUS_REQUEST_COMPLETE:
          if Assigned(Grabber.FOnRequestComplete) then
            Grabber.FOnRequestComplete(Grabber, UserData, Url);
        INTERNET_STATUS_REQUEST_SENT:
          if Assigned(Grabber.FOnRequestSent) then
            Grabber.FOnRequestSent(Grabber, UserData, Url, DWORD(Info^));
        INTERNET_STATUS_RESOLVING_NAME:
          if Assigned(Grabber.FOnResolving) then
            Grabber.FOnResolving(Grabber, UserData, Url);
        INTERNET_STATUS_RESPONSE_RECEIVED:
          if Assigned(Grabber.FOnReceived) then
            Grabber.FOnReceived(Grabber, UserData, Url, DWORD(Info^));
        INTERNET_STATUS_SENDING_REQUEST:
          if Assigned(Grabber.FOnSendingRequest) then
            Grabber.FOnSendingRequest(Grabber, UserData, Url);
      end;
end;

procedure TJvMultiHTTPGrabber.Download(UserData: Integer);
var
  Infos: PRequestInfos;
begin
  Infos := StartConnection(UserData, False);
  if Infos <> nil then
    with TJvMultiHttpThread.Create(Infos) do
    begin
      OnTerminate := ThreadTerminated;
      FreeOnTerminate := true;
      Resume;
      Inc(FCount);
    end;
end;

procedure TJvMultiHTTPGrabber.GetFileAge(UserData: Integer = 0);
var
  Infos: PrequestInfos;
begin
  Infos := StartConnection(UserData, True);
  if Infos <> nil then
    with TJvMultiDateHttpThread.Create(Infos) do
    begin
      OnTerminate := ThreadDateTerminated;
      FreeOnTerminate := true;
      Resume;
    end;
end;

function TJvMultiHTTPGrabber.GetWorking: Boolean;
begin
  Result := FCount > 0;
end;

procedure TJvMultiHTTPGrabber.RaiseError(Value: Pointer);
var
  Msg: array [0..256] of Char;
begin
  if Assigned(FOnError) then
    with PRequestInfos(Value)^ do
    begin
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, 0, Msg,
        SizeOf(Msg), nil);
      FOnError(Self, UserData, Url, Msg);
    end;
end;

procedure TJvMultiHTTPGrabber.RaiseWebError(Infos: Pointer);
var
  dwIndex, dwBufLen: DWORD;
  Buf: array [0..1024] of Char;
begin
  if Assigned(FOnError) then
  begin
    dwIndex := 0;
    dwBufLen := SizeOf(Buf);
    InternetGetLastResponseInfo(dwIndex, Buf, dwBufLen);
    with PRequestInfos(Infos)^ do
      FOnError(Self, UserData, Url, StrPas(buf));
  end;
end;

function TJvMultiHTTPGrabber.StartConnection(UserData: Integer; IgnoreMessages: Boolean): Pointer;
var
  Infos: PRequestInfos;
  HostName, FilePath: string;
  HostPort: Word;

  procedure ParseUrl(Value: string);
  begin
    HostName := '';
    FilePath := '';
    if Pos('HTTP://', UpperCase(Value)) <> 0 then
      Value := Copy(Value, 8, Length(Value));
    if Pos('/', Value) <> 0 then
    begin
      HostName := Copy(Value, 1, Pos('/', Value) - 1);
      FilePath := Copy(Value, Pos('/', Value) + 1, Length(Value));
    end
    else
      HostName := Value;

    if Pos(':', HostName) <> 0 then
    begin // If URL contains a non-standard Port number, attempt to use it
      HostPort := StrToIntDef(Copy(HostName, Pos(':', HostName) + 1, Length(HostName)), INTERNET_DEFAULT_HTTP_PORT);
      HostName := Copy(HostName, 1, Pos(':', HostName) - 1);
    end
    else // If not, use the standard one
      HostPort := INTERNET_DEFAULT_HTTP_PORT;
  end;

begin
  Result := nil;

  Infos := New(PRequestInfos);
  Infos^.Url := Url;
  Infos^.Filename := FileName;
  Infos^.OutputMode := OutputMode;
  Infos^.UserData := UserData;
  Infos^.Grabber := Self;
  Infos^.IgnoreMsg := IgnoreMessages;

  //Opening the web session with the server
  Infos^.hSession := InternetOpen(PChar(FAgent), INTERNET_OPEN_TYPE_PRECONFIG,
    nil, nil, 0);
  if Infos^.hSession = nil then
  begin
    RaiseError(Infos);
    Dispose(Infos);
    Exit;
  end;

  //Setting callback function
  InternetSetStatusCallback(Infos^.hSession, PFNInternetStatusCallback(@StatusCallback));

  //Open the internet connection
  ParseUrl(Url);
  Infos^.hHostConnect := InternetConnect(Infos^.hSession, PChar(HostName),
    HostPort, PChar(FUserName), PChar(FPassword), INTERNET_SERVICE_HTTP,
    0, Cardinal(Infos));
  if Infos^.hHostConnect = nil then
  begin
    RaiseWebError(Infos);
    InternetCloseHandle(Infos^.hSession);
    Dispose(Infos);
    Exit;
  end;

  //prepare the GET order
  Infos^.hRequest := HttpOpenRequest(Infos^.hHostConnect, 'GET', PChar(FilePath),
    'HTTP/1.0', PChar(FReferer), nil, INTERNET_FLAG_RELOAD, 0);

  Result := Infos;
end;

procedure TJvMultiHTTPGrabber.StopConnection(Infos: Pointer);
begin
  InternetCloseHandle(PRequestInfos(Infos)^.hRequest);
  InternetCloseHandle(PRequestInfos(Infos)^.hHostConnect);
  InternetCloseHandle(PRequestInfos(Infos)^.hSession);
end;

procedure TJvMultiHTTPGrabber.ThreadDateTerminated(Sender: TObject);
begin
  with Sender as TJvMultiDateHttpThread do
  begin
    with PRequestInfos(FInfos)^ do
      if Assigned(FOnDateRetrieved) then
        FOnDateRetrieved(Self, UserData, FValue, Url);

    StopConnection(FInfos);
    Dispose(FInfos);
//    Free; // (p3) FreeOnTerminate is set when creating, so don't free here
  end;
end;

procedure TJvMultiHTTPGrabber.ThreadTerminated(Sender: TObject);
var
  TT: TJvMultiHttpThread;
begin
  TT := Sender as TJvMultiHttpThread; // need this for debugging purposes
  try
    if (TT.FStream <> nil) and (TT.FStream.Size > 0) then
    begin
      if OutputMode = omStream then
      begin
        if Assigned(FOnDoneStream) then
          FOnDoneStream(Self, PRequestInfos(TT.FInfos)^.UserData, TT.FStream, TT.FStream.Size, PRequestInfos(TT.FInfos)^.Url);
      end
      else
      begin
        TT.FStream.SaveToFile(PRequestInfos(TT.FInfos)^.FileName);
        if Assigned(FOnDoneFile) then
          FOnDoneFile(Self, PRequestInfos(TT.FInfos)^.UserData, PRequestInfos(TT.FInfos)^.FileName, TT.FStream.Size, PRequestInfos(TT.FInfos)^.Url);
      end;
    end;

    StopConnection(PRequestInfos(TT.FInfos));
    Dispose(PRequestInfos(TT.FInfos));
    Dec(FCount);
  finally
//    TT.Free; // (p3) FreeOnTerminate is set when creating, so don't free here
  end;
end;

//=== TJvMultiHttpThread =====================================================

constructor TJvMultiHttpThread.Create(Value: Pointer);
begin
  inherited Create(True);
  FInfos := Value;
  FPosition := 0;
  FContinue := True;
  FStream := nil;
end;

destructor TJvMultiHttpThread.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TJvMultiHttpThread.Error;
var
  Infos: TRequestInfos;
begin
  Infos := PRequestInfos(FInfos)^;
  if Assigned(Infos.Grabber.FOnError) then
    Infos.Grabber.FOnError(Self, Infos.UserData, Infos.Url, RsErrorConnection);
end;

procedure TJvMultiHttpThread.Execute;
var
  Infos: PRequestInfos;
  Buffer: array [0..512] of Byte;
  BytesRead: DWORD;
  dLength, dReserved, dSize: DWORD;
begin
  // (p3) avoid memory leaks
  FreeAndNil(FStream);
  try
    Infos := PRequestInfos(FInfos);

    //Send the request
    if not HttpSendRequest(Infos^.hRequest, nil, 0, nil, 0) then
    begin
      Synchronize(Error);
      Exit;
    end;

    // Get the Size
    dLength := SizeOf(dSize);
    dReserved := 0;
    if HttpQueryInfo(Infos^.hRequest, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER,
      @dSize, dLength, dReserved) then
      Infos^.FileSize := dSize
    else
      Infos^.FileSize := -1;

    // Download the stuff
    Synchronize(Progress);
    if not FContinue then
      Exit;

    FStream := TMemoryStream.Create;
    repeat
      if not InternetReadFile(Infos^.hRequest, @Buffer[0], SizeOf(Buffer), BytesRead) then
        BytesRead := 0
      else
      begin
        Inc(FPosition, BytesRead);
        FStream.Write(buffer, BytesRead);
        Synchronize(Progress);
        if not FContinue then
          Exit;
      end;
    until BytesRead = 0;
    FStream.Position := 0;
  except
  end;
  Terminate;
end;

procedure TJvMultiHttpThread.Progress;
begin
  with PRequestInfos(FInfos)^ do
    if Assigned(Grabber.OnProgress) then
      Grabber.OnProgress(Grabber, UserData, FPosition, FileSize, Url, FContinue);
end;

//=== TJvMultiDateHttpThread =================================================

constructor TJvMultiDateHttpThread.Create(Value: Pointer);
begin
  inherited Create(True);
  FInfos := Value;
end;

procedure TJvMultiDateHttpThread.Error;
var
  Infos: TRequestInfos;
begin
  Infos := PRequestInfos(FInfos)^;
  if Assigned(Infos.Grabber.FOnError) then
    Infos.Grabber.FOnError(Self, Infos.UserData, Infos.Url, RsErrorConnection);
end;

procedure TJvMultiDateHttpThread.Execute;
var
  Infos: PRequestInfos;
  STime: TSystemTime;
  dLength, dReserved: DWORD;
begin
  // (rom) secure thread against exceptions
  try
    Infos := PRequestInfos(FInfos);

    dLength := SizeOf(TSystemTime);
    dReserved := 0;

    HttpSendRequest(Infos^.hRequest, nil, 0, nil, 0);

    if HttpQueryInfo(Infos^.hRequest, HTTP_QUERY_LAST_MODIFIED or HTTP_QUERY_FLAG_SYSTEMTIME,
      @STime, dLength, dReserved) then
      FValue := SystemTimeToDateTime(STime)
    else
      FValue := -1;
  except
  end;
end;

end.

