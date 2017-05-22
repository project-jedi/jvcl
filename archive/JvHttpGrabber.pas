{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHTTPGrabber.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
  Michael Beck [mbeck att bigfoot dott com],
  Michail Michaylov [m.mihajlov@is-bg.net].

Last Modified: 2003-06-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

{$HPPEMIT '#pragma link "wininet.lib"'}

unit JvHTTPGrabber;

interface

uses
  Windows, SysUtils, Classes, WinInet, SyncObjs,
  JvTypes, JvComponent;

type
  TJvHttpThread = class(TThread)
  private
    FStream: TMemoryStream;
    FUrl: string;
    FReferer: string;
    FUsername: string;
    FFileName: string;
    FPassword: string;
    FOutputMode: TJvOutputMode;
    FOnError: TJvErrorEvent;
    FOnDoneFile: TJvDoneFileEvent;
    FOnDoneStream: TJvDoneStreamEvent;
    FOnProgress: TJvHTTPProgressEvent;
    FAgent: string;
    FBytesRead: Integer;
    FTotalBytes: Integer;
    FErrorText: string;
    FOnStatus: TJvFTPProgressEvent;
    FContinue: Boolean;
    FCriticalSection: TCriticalSection;
    function GetLastErrorMsg: string;
  protected
    procedure Error;
    procedure Progress;
    procedure Ended;
    procedure Execute; override;
  public
    constructor Create(Url, Referer, Username, FileName, Password: string;
      OutPutMode: TJvOutputMode; AOnError: TJvErrorEvent;
      AOnDoneFile: TJvDoneFileEvent; AOnDoneStream: TJvDoneStreamEvent;
      AOnProgress: TJvHTTPProgressEvent; Agent: string;
      AOnStatus: TJvFTPProgressEvent);
    destructor Destroy; override;
  end;

  TJvHTTPGrabber = class(TJvComponent)
  private
    FThread: TJvHttpThread;
    FUrl: string;
    FReferer: string;
    FUsername: string;
    FFileName: TFileName;
    FPassword: string;
    FOutputMode: TJvOutputMode;
    FOnError: TJvErrorEvent;
    FOnDoneFile: TJvDoneFileEvent;
    FOnDoneStream: TJvDoneStreamEvent;
    FOnProgress: TJvHTTPProgressEvent;
    FAgent: string;
    FOnReceived: TNotifyEvent;
    FOnResolving: TNotifyEvent;
    FOnConnecting: TNotifyEvent;
    FOnConnected: TNotifyEvent;
    FOnResolved: TNotifyEvent;
    FOnRedirect: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnSent: TNotifyEvent;
    FOnSending: TNotifyEvent;
    FOnReceiving: TNotifyEvent;
    FOnClosed: TNotifyEvent;
    FOnClosing: TNotifyEvent;
    FOnRequest: TNotifyEvent;
    procedure ThreadFinished(Sender: TObject);
    procedure Error(Sender: TObject; ErrorMsg: string);
    procedure DoneFile(Sender: TObject; FileName: string; FileSize: Integer; Url: string);
    procedure DoneStream(Sender: TObject; FStream: TStream; StreamSize: Integer; Url: string);
    procedure Progress(Sender: TObject; UserData, Position, TotalSize: Integer; Url: string; var Continue: Boolean);
    procedure Status(Sender: TObject; Position: Integer; Url: string);
    function GetWorking: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Abort;
    property Working: Boolean read GetWorking;
  published
    property Url: string read FUrl write FUrl;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Referer: string read FReferer write FReferer;
    property FileName: TFileName read FFileName write FFileName;
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
    property Agent: string read FAgent write FAgent;
    property OnDoneFile: TJvDoneFileEvent read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TJvDoneStreamEvent read FOnDoneStream write FOnDoneStream;
    property OnError: TJvErrorEvent read FOnError write FOnError;
    property OnProgress: TJvHTTPProgressEvent read FOnProgress write FOnProgress;
    property OnResolvingName: TNotifyEvent read FOnResolving write FOnResolving;
    property OnResolvedName: TNotifyEvent read FOnResolved write FOnResolved;
    property OnConnectingToServer: TNotifyEvent read FOnConnecting write FOnConnecting;
    property OnConnectedToServer: TNotifyEvent read FOnConnected write FOnConnected;
    property OnSendingRequest: TNotifyEvent read FOnSending write FOnSending;
    property OnRequestSent: TNotifyEvent read FOnSent write FOnSent;
    property OnReceivingResponse: TNotifyEvent read FOnReceiving write FOnReceiving;
    property OnReceivedResponse: TNotifyEvent read FOnReceived write FOnReceived;
    property OnClosingConnection: TNotifyEvent read FOnClosing write FOnClosing;
    property OnClosedConnection: TNotifyEvent read FOnClosed write FOnClosed;
    property OnRequestComplete: TNotifyEvent read FOnRequest write FOnRequest;
    property OnRedirect: TNotifyEvent read FOnRedirect write FOnRedirect;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

resourcestring
  SURLIsEmpty = 'URL is empty';
  
implementation

uses
  JvResources;

//=== TJvHTTPGrabber =========================================================

constructor TJvHTTPGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUrl := '';
  FUsername := '';
  FPassword := '';
  FReferer := '';
  FFileName := '';
  FOutputMode := omStream;
  FAgent := RsAgent;
  FThread := nil;
end;

destructor TJvHTTPGrabber.Destroy;
begin
  if FThread <> nil then
  begin
    FThread.FreeOnTerminate := True;
    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
  end;
  inherited Destroy;
end;

procedure TJvHTTPGrabber.Abort;
begin
  if FThread <> nil then
  begin
    FThread.Suspend;
    FThread.FOnError := nil;
    FThread.FOnDoneFile := nil;
    FThread.FOnDoneStream := nil;
    FThread.FOnProgress := nil;
    FThread.FContinue := False;
    FThread := nil;
  end;
end;

procedure TJvHTTPGrabber.DoneFile(Sender: TObject; FileName: string;
  FileSize: Integer; Url: string);
begin
  if Assigned(FOnDoneFile) then
    FOnDoneFile(Self, FileName, FileSize, Url);
  FThread := nil;
end;

procedure TJvHTTPGrabber.DoneStream(Sender: TObject; FStream: TStream;
  StreamSize: Integer; Url: string);
begin
  if Assigned(FOnDoneStream) then
    FOnDoneStream(Self, FStream, StreamSize, Url);
  FThread := nil;
end;

procedure TJvHTTPGrabber.Error(Sender: TObject; ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg);
end;

procedure TJvHTTPGrabber.Execute;
begin
  //Download it
  if (FThread = nil) then
  begin
    FThread := TJvHttpThread.Create(Url, Referer, Username, FileName, Password,
      OutPutMode, Error, DoneFile, DoneStream, Progress, Agent, Status);
    FThread.OnTerminate := ThreadFinished;
    FThread.Resume;
    FThread.WaitFor;
  end;
end;

function TJvHTTPGrabber.GetWorking: Boolean;
begin
  Result := FThread <> nil;
end;

procedure TJvHTTPGrabber.Progress(Sender: TObject; UserData, Position,
  TotalSize: Integer; Url: string; var Continue: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, UserData, Position, TotalSize, Url, Continue);
end;

procedure TJvHTTPGrabber.Status(Sender: TObject; Position: Integer;
  Url: string);
begin
  case Position of
    INTERNET_STATUS_RESOLVING_NAME:
      if Assigned(FOnResolving) then
        FOnResolving(Self);
    INTERNET_STATUS_NAME_RESOLVED:
      if Assigned(FOnResolved) then
        FOnResolved(Self);
    INTERNET_STATUS_CONNECTING_TO_SERVER:
      if Assigned(FOnConnecting) then
        FOnConnecting(Self);
    INTERNET_STATUS_CONNECTED_TO_SERVER:
      if Assigned(FOnConnected) then
        FOnConnected(Self);
    INTERNET_STATUS_SENDING_REQUEST:
      if Assigned(FOnSending) then
        FOnSending(Self);
    INTERNET_STATUS_REQUEST_SENT:
      if Assigned(FOnSent) then
        FOnSent(Self);
    INTERNET_STATUS_RECEIVING_RESPONSE:
      if Assigned(FOnReceiving) then
        FOnReceiving(Self);
    INTERNET_STATUS_RESPONSE_RECEIVED:
      if Assigned(FOnReceived) then
        FOnReceived(Self);
    INTERNET_STATUS_CLOSING_CONNECTION:
      if Assigned(FOnClosing) then
        FOnClosing(Self);
    INTERNET_STATUS_CONNECTION_CLOSED:
      if Assigned(FOnClosed) then
        FOnClosed(Self);
    INTERNET_STATUS_REQUEST_COMPLETE:
      if Assigned(FOnRequest) then
        FOnRequest(Self);
    INTERNET_STATUS_REDIRECT:
      if Assigned(FOnRedirect) then
        FOnRedirect(Self);
    INTERNET_STATUS_STATE_CHANGE:
      if Assigned(FOnStateChange) then
        FOnStateChange(Self);
  end;
end;

procedure TJvHTTPGrabber.ThreadFinished(Sender: TObject);
begin
  FThread := nil;
end;

//=== TJvHttpThread ==========================================================

constructor TJvHttpThread.Create(Url, Referer, Username, FileName,
  Password: string; OutPutMode: TJvOutputMode; AOnError: TJvErrorEvent;
  AOnDoneFile: TJvDoneFileEvent; AOnDoneStream: TJvDoneStreamEvent;
  AOnProgress: TJvHTTPProgressEvent; Agent: string; AOnStatus: TJvFTPProgressEvent);
begin
  inherited Create(True);
  FUrl := Url;
  FReferer := Referer;
  FUsername := Username;
  FFileName := FileName;
  FPassword := Password;
  FOutputMode := OutPutMode;
  FOnError := AOnError;
  FOnDoneFile := AOnDoneFile;
  FOnDoneStream := AOnDoneStream;
  FOnProgress := AOnProgress;
  FAgent := Agent;
  FOnStatus := AOnStatus;
  FContinue := True;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TJvHttpThread.Destroy;
begin
  FCriticalSection.Destroy;
  // (rom) added inherited Destroy 
  inherited Destroy;
end;

procedure TJvHttpThread.Ended;
begin
  FCriticalSection.Enter;
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
  FCriticalSection.Leave;
end;

procedure TJvHttpThread.Error;
begin
  FCriticalSection.Enter;
  if Assigned(FOnError) then
    FOnError(Self, FErrorText);
  FCriticalSection.Leave;
end;

function TJvHttpThread.GetLastErrorMsg: string;
begin
  Result := SysErrorMessage(GetLastError);
end;

procedure DownloadCallBack(Handle: HInternet; Context: DWord;
  Status: DWord; Info: Pointer; StatLen: DWord); stdcall;
begin
  with TJvHttpThread(Context) do
    if Assigned(FOnStatus) then
      FOnStatus(TJvHttpThread(Context), Status, FUrl);
end;

procedure TJvHttpThread.Execute;
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName: string;
  Username, Password: PChar;
  Buffer: PChar;
  dwBufLen, dwIndex, dwBytesRead, dwTotalBytes: DWORD;
  HasSize: Boolean;
  Buf: array [0..1024] of Byte;

  procedure ParseUrl(Value: string);
  begin
    HostName := '';
    FileName := '';
    if Pos('HTTP://', UpperCase(Value)) <> 0 then
      Value := Copy(Value, 8, Length(Value));
    if Pos('/', Value) <> 0 then
    begin
      HostName := Copy(Value, 1, Pos('/', Value) - 1);
      FileName := Copy(Value, Pos('/', Value) + 1, Length(Value));
    end
    else
      HostName := Value;
  end;

begin
  if FUrl = '' then
  begin
    FErrorText := SURLIsEmpty;
    Error;
    Exit;
  end;

  // (rom) secure thread against exceptions
  Buffer := nil;

  FStream := nil;
  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  try
    try
      ParseUrl(FUrl);
      FErrorText := '';

      //Connect to the web
      hSession := InternetOpen(PChar(FAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
      if hSession = nil then
      begin
        FErrorText := GetLastErrorMsg;
        Error;
        Exit;
      end;

      //Connect to the hostname
      if FUsername = '' then
        Username := nil
      else
        Username := PChar(FUsername);
      if FPassword = '' then
        Password := nil
      else
        Password := PChar(FPassword);
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

      FCriticalSection.Enter;
      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@DownloadCallBack));
      //Request the file
      // (rom) any difference here?
      hDownload := HttpOpenRequest(hHostConnection, 'GET', PChar(FileName), 'HTTP/1.0', PChar(FReferer),
        nil, INTERNET_FLAG_RELOAD, 0);
      FCriticalSection.Leave;

      if hDownload = nil then
      begin
        FErrorText := GetLastErrorMsg;
        Error;
        Exit;
      end;

      FCriticalSection.Enter;
      //Send the request
      HttpSendRequest(hDownload, nil, 0, nil, 0);

      FStream := TMemoryStream.Create;

      dwIndex := 0;
      dwBufLen := 1024;
      GetMem(Buffer, dwBufLen);
      HasSize := HttpQueryInfo(hDownload, HTTP_QUERY_CONTENT_LENGTH, Buffer, dwBufLen, dwIndex);
      if HasSize then
        FTotalBytes := StrToInt(StrPas(Buffer))
      else
        FTotalBytes := 0;

      dwTotalBytes := 0;
      if HasSize then
      begin
        dwBytesRead := 1;
        while dwBytesRead > 0 do
        begin
          if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) then
            dwBytesRead := 0
          else
          begin
            Inc(dwTotalBytes, dwBytesRead);
            FBytesRead := dwTotalBytes;
            FStream.Write(Buf, dwBytesRead);
            Progress;
          end;
        end;
        if FContinue then
          Ended;
        FCriticalSection.Leave;
      end
      else
      begin
        FCriticalSection.Enter;
        while InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) do
        begin
          if dwBytesRead = 0 then Break;
//          Inc(dwTotalBytes,dwBytesRead);
          FStream.Write(Buf, dwBytesRead);
          Progress;
        end;
        if FContinue then
          Ended;
        FCriticalSection.Leave;
      end;
    except
    end;
  finally
    //Free all stuff's
    if Buffer <> nil then
    FreeMem(Buffer);
    FStream.Free;

    //Release all handles
    if hDownload <> nil then
      InternetCloseHandle(hDownload);
    if hHostConnection <> nil then
      InternetCloseHandle(hHostConnection);
    if hSession <> nil then
      InternetCloseHandle(hSession);
  end;
end;

procedure TJvHttpThread.Progress;
begin
  FCriticalSection.Enter;
  if Assigned(FOnProgress) then
    FOnProgress(Self, 0, FBytesRead, FTotalBytes, FUrl, FContinue);
  FCriticalSection.Leave;
end;

end.


