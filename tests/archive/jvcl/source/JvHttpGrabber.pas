{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHttpGrabber.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvHttpGrabber;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, WinInet, JvTypes, JvComponent;

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
    FOnError: TOnError;
    FOnDoneFile: TOnDoneFile;
    FOnDoneStream: TOnDoneStream;
    FOnProgress: TOnProgress;
    FAgent: string;
    FBytesReaded: Integer;
    FTotalBytes: Integer;
    FErrorText: string;
    FOnStatus: TOnFtpProgress;
    FContinue: Boolean;
    function GetLastErrorMsg: string;
  protected
    procedure Error;
    procedure Progress;
    procedure Ended;
    procedure Execute; override;
  public
    constructor Create(Url, Referer, Username, FileName, Password: string;
      OutPutMode: TJvOutputMode; OnError: TOnError;
      OnDoneFile: TOnDoneFile; OnDoneStream: TOnDoneStream;
      OnProgress: TOnProgress; Agent: string;
      OnStatus: TOnFtpProgress);
  end;

  TJvHttpGrabber = class(TJvComponent)
  private
    FThread: TJvHttpThread;
    FUrl: string;
    FReferer: string;
    FUsername: string;
    FFileName: TFileName;
    FPassword: string;
    FOutputMode: TJvOutputMode;
    FOnError: TOnError;
    FOnDoneFile: TOnDoneFile;
    FOnDoneStream: TOnDoneStream;
    FOnProgress: TOnProgress;
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
    procedure Progress(Sender: TObject; Position: Integer; TotalSize: Integer; Url: string; var Continue: Boolean);
    procedure Status(Sender: TObject; Position: Integer; Url: string);
    function GetWorking: Boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Abort;
  published
    property Url: string read FUrl write FUrl;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Referer: string read FReferer write FReferer;
    property FileName: TFileName read FFileName write FFileName;
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
    property Agent: string read FAgent write FAgent;
    property OnDoneFile: TOnDoneFile read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TOnDoneStream read FOnDoneStream write FOnDoneStream;
    property OnError: TOnError read FOnError write FOnError;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
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
    property Working: Boolean read GetWorking;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvHttpGrabber
///////////////////////////////////////////////////////////

procedure TJvHttpGrabber.Abort;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread.FOnError := nil;
    FThread.FOnDoneFile := nil;
    FThread.FOnDoneStream := nil;
    FThread.FOnProgress := nil;
    FThread.FContinue := False;
    FThread := nil;
  end;
end;

{*************************************************}

constructor TJvHttpGrabber.Create(AOwner: TComponent);
begin
  inherited;
  FUrl := '';
  FUsername := '';
  FPassword := '';
  FReferer := '';
  FFileName := '';
  FOutputMode := omStream;
  FAgent := 'TJvHttpGrabber Component';
  FThread := nil;
end;

{*************************************************}

destructor TJvHttpGrabber.Destroy;
begin
  if FThread <> nil then
  begin
    FThread.FreeOnTerminate := True;
    FThread.Terminate;
  end;
  inherited;
end;

{*************************************************}

procedure TJvHttpGrabber.DoneFile(Sender: TObject; FileName: string;
  FileSize: Integer; Url: string);
begin
  if Assigned(FOnDoneFile) then
    FOnDoneFile(Self, FileName, FileSize, Url);
  FThread := nil;
end;

{*************************************************}

procedure TJvHttpGrabber.DoneStream(Sender: TObject; FStream: TStream;
  StreamSize: Integer; Url: string);
begin
  if Assigned(FOnDoneStream) then
    FOnDoneStream(Self, FStream, StreamSize, Url);
  FThread := nil;
end;

{*************************************************}

procedure TJvHttpGrabber.Error(Sender: TObject; ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg);
end;

{*************************************************}

procedure TJvHttpGrabber.Execute;
begin
  //Download it
  if FThread = nil then
  begin
    FThread := TJvHttpThread.Create(Url, Referer, Username, FileName, Password,
      OutPutMode, Error, DoneFile, DoneStream, Progress, Agent, Status);
    FThread.OnTerminate := ThreadFinished;
    FThread.Resume;
  end;
end;

{*************************************************}

function TJvHttpGrabber.GetWorking: Boolean;
begin
  Result := FThread <> nil;
end;

{*************************************************}

procedure TJvHttpGrabber.Progress(Sender: TObject; Position,
  TotalSize: Integer; Url: string; var Continue: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, TotalSize, Url, Continue);
end;

{*************************************************}

procedure TJvHttpGrabber.Status(Sender: TObject; Position: Integer;
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

{*************************************************}

procedure TJvHttpGrabber.ThreadFinished(Sender: TObject);
begin
  FThread := nil;
end;

///////////////////////////////////////////////////////////
// TJvHttpThread
///////////////////////////////////////////////////////////

constructor TJvHttpThread.Create(Url, Referer, Username, FileName,
  Password: string; OutPutMode: TJvOutputMode; OnError: TOnError;
  OnDoneFile: TOnDoneFile; OnDoneStream: TOnDoneStream;
  OnProgress: TOnProgress; Agent: string; OnStatus: TOnFtpProgress);
begin
  inherited Create(True);
  FUrl := Url;
  FReferer := Referer;
  FUsername := Username;
  FFileName := FileName;
  FPassword := Password;
  FOutputMode := OutPutMode;
  FOnError := OnError;
  FOnDoneFile := OnDoneFile;
  FOnDoneStream := OnDoneStream;
  FOnProgress := OnProgress;
  FAgent := Agent;
  FOnStatus := OnStatus;
  FContinue := True;
end;

{*************************************************}

procedure TJvHttpThread.Ended;
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

{*************************************************}

procedure TJvHttpThread.Error;
begin
  if Assigned(FOnError) then
    FOnError(Self, FErrorText);
end;

{**************************************************}

function TJvHttpThread.GetLastErrorMsg: string;
var
  msg: array[0..1000] of Char;
begin
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, 0, msg, 1000, nil);
  Result := msg;
end;

{*************************************************}

procedure DownloadCallBack(Handle: HInternet; Context: DWord;
  Status: DWord; Info: Pointer; StatLen: DWord); stdcall;
begin
  with TJvHttpThread(Context) do
    if Assigned(FOnStatus) then
      FOnStatus(TJvHttpThread(Context), Status, FUrl);
end;
{*************************************************}

procedure TJvHttpThread.Execute;
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName: string;
  username, password: PChar;
  buffer: Pointer;
  dwBufLen, dwIndex, ReadedBytes, TotalBytes: DWORD;
  HasSize: Boolean;
  Buf: array[0..1024] of Byte;

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
  ParseUrl(FUrl);
  FErrorText := '';

  //Connect to the web
  hSession := InternetOpen(PChar(FAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, DWORD(Self));
  if hSession = nil then
  begin
    FErrorText := GetLastErrorMsg;
    Synchronize(Error);
    Exit;
  end;

  //Connect to the hostname
  if FUsername = '' then
    username := nil
  else
    username := PChar(FUsername);
  if FPassword = '' then
    password := nil
  else
    password := PChar(FPassword);
  hHostConnection := InternetConnect(hSession, PChar(HostName), INTERNET_DEFAULT_HTTP_PORT,
    username, password, INTERNET_SERVICE_HTTP, 0, DWORD(Self));
  if hHostConnection = nil then
  begin
    dwIndex := 0;
    dwBufLen := 1024;
    GetMem(Buffer, dwBufLen);
    InternetGetLastResponseInfo(dwIndex, buffer, dwBufLen);
    FErrorText := strpas(Buffer);
    FreeMem(Buffer);

    Synchronize(Error);
    Exit;
  end;

  InternetSetStatusCallback(hHostConnection, @DownloadCallBack);

  //Request the file
{$IFDEF D5}
  hDownload := HttpOpenRequest(hHostConnection, 'GET', PChar(FileName), 'HTTP/1.0', PChar(FReferer),
    nil, INTERNET_FLAG_RELOAD, 0);
{$ELSE}
  hDownload := HttpOpenRequest(hHostConnection, 'GET', PChar(FileName), 'HTTP/1.0', PChar(FReferer),
    nil, INTERNET_FLAG_RELOAD, 0);
{$ENDIF}

  if hDownload = nil then
  begin
    FErrorText := GetLastErrorMsg;
    Synchronize(Error);
    Exit;
  end;

  //Send the request
  HttpSendRequest(hDownload, nil, 0, nil, 0);

  FStream := TMemoryStream.Create;

  dwIndex := 0;
  dwBufLen := 1024;
  GetMem(Buffer, dwBufLen);
  HasSize := HttpQueryInfo(hDownload, HTTP_QUERY_CONTENT_LENGTH, buffer, dwBufLen, dwIndex);
  if HasSize then
    FTotalBytes := StrToInt(StrPas(buffer))
  else
    FTotalBytes := 0;

  TotalBytes := 0;
  if HasSize then
  begin
    ReadedBytes := 1;
    while ReadedBytes > 0 do
    begin
      if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), ReadedBytes) then
        ReadedBytes := 0
      else
      begin
        Inc(TotalBytes, ReadedBytes);
        FBytesReaded := TotalBytes;
        FStream.Write(Buf, ReadedBytes);
        Synchronize(Progress);
      end;
    end;
    if FContinue then
      Synchronize(Ended);
  end
  else
    Synchronize(Error);

  //Free all stuff's
  FreeMem(Buffer);
  FStream.Free;

  //Release all handles
  InternetCloseHandle(hDownload);
  InternetCloseHandle(hHostConnection);
  InternetCloseHandle(hSession);
end;

{*************************************************}

procedure TJvHttpThread.Progress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, FBytesReaded, FTotalBytes, FUrl, FContinue);
end;

end.
