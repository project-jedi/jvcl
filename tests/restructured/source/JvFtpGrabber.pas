{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFtpGrabber.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFtpGrabber;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, WinInet, JvTypes, JvComponent;

type
  TJvDownloadMode = (hmBinary, hmAscii);
{$EXTERNALSYM TJvDownloadMode}

  TJvFtpThread = class(TThread)
  private
    Stream: TMemoryStream;
    FUrl: string;
    FUserName: string;
    FFileName: string;
    FPassword: string;
    FOutputMode: TJvOutputMode;
    FOnError: TOnError;
    FOnDoneFile: TOnDoneFile;
    FOnDoneStream: TOnDoneStream;
    FOnProgress: TOnFtpProgress;
    FMode: TJvDownloadMode;
    FAgent: string;
    FBytesReaded: Integer;
    FErrorText: string;
    FOnStatus: TOnFtpProgress;
    function GetLastErrorMsg: string;
  protected
    procedure Error;
    procedure Progress;
    procedure Ended;
    procedure Execute; override;
  public
    constructor Create(Url, Username, FileName, Password: string;
      OutPutMode: TJvOutputMode; OnError: TOnError;
      OnDoneFile: TOnDoneFile; OnDoneStream: TOnDoneStream;
      OnProgress: TOnFtpProgress; Mode: TJvDownloadMode; Agent: string;
      OnStatus: TOnFtpProgress);
  end;

  TJvFtpGrabber = class(TJvComponent)
  private
    FThread: TJvFtpThread;
    FUrl: string;
    FUserName: string;
    FFileName: TFileName;
    FPassword: string;
    FOutputMode: TJvOutputMode;
    FOnError: TOnError;
    FOnDoneFile: TOnDoneFile;
    FOnDoneStream: TOnDoneStream;
    FOnProgress: TOnFtpProgress;
    FMode: TJvDownloadMode;
    FAgent: string;
    FOnReceived: TNotifyEvent;
    FOnResolving: TNotifyEvent;
    FOnConnecting: TNotifyEvent;
    FOnConnected: TNotifyEvent;
    FOnResolved: TNotifyEvent;
    FOnRedirect: TNotifyEvent;
    FOnSent: TNotifyEvent;
    FOnSending: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnReceiving: TNotifyEvent;
    FOnClosed: TNotifyEvent;
    FOnClosing: TNotifyEvent;
    FOnRequest: TNotifyEvent;
    procedure ThreadFinished(Sender: TObject);
    procedure Error(Sender: TObject; ErrorMsg: string);
    procedure DoneFile(Sender: TObject; FileName: string; FileSize: Integer; Url: string);
    procedure DoneStream(Sender: TObject; Stream: TStream; StreamSize: Integer; Url: string);
    procedure Progress(Sender: TObject; Position: Integer; Url: string);
    procedure Status(Sender: TObject; Position: Integer; Url: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Url: string read FUrl write FUrl;
    property Username: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property FileName: TFileName read FFileName write FFileName;
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
    property Mode: TJvDownloadMode read FMode write FMode default hmBinary;
    property Agent: string read FAgent write FAgent;
    property OnDoneFile: TOnDoneFile read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TOnDoneStream read FOnDoneStream write FOnDoneStream;
    property OnError: TOnError read FOnError write FOnError;
    property OnProgress: TOnFtpProgress read FOnProgress write FOnProgress;
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
    procedure Execute;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvFtpGrabber
///////////////////////////////////////////////////////////

constructor TJvFtpGrabber.Create(AOwner: TComponent);
begin
  inherited;
  FUrl := '';
  FUserName := '';
  FPassword := '';
  FFileName := '';
  FOutputMode := omStream;
  FMode := hmBinary;
  FAgent := 'TJvHttpGrabber Component';
  FThread := nil;
end;

{*************************************************}

destructor TJvFtpGrabber.Destroy;
begin
  if FThread <> nil then
  begin
    FThread.FreeOnTerminate := True;
    FThread.Terminate;
  end;
  inherited;
end;

{*************************************************}

procedure TJvFtpGrabber.DoneFile(Sender: TObject; FileName: string;
  FileSize: Integer; Url: string);
begin
  if Assigned(FOnDoneFile) then
    FOnDoneFile(Self, FileName, FileSize, Url);
end;

{*************************************************}

procedure TJvFtpGrabber.DoneStream(Sender: TObject; Stream: TStream;
  StreamSize: Integer; Url: string);
begin
  if Assigned(FOnDoneStream) then
    FOnDoneStream(Self, Stream, StreamSize, Url);
end;

{*************************************************}

procedure TJvFtpGrabber.Error(Sender: TObject; ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg);
end;

{*************************************************}

procedure TJvFtpGrabber.Execute;
begin
  //Download it
  if FThread = nil then
  begin
    FThread := TJvFtpThread.Create(Url, Username, FileName, Password, OutPutMode, Error, DoneFile, DoneStream,
      Progress, Mode, Agent, Status);
    FThread.OnTerminate := ThreadFinished;
    FThread.Resume;
  end;
end;

{*************************************************}

procedure TJvFtpGrabber.Progress(Sender: TObject; Position: Integer; Url: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, Url);
end;

{*************************************************}

procedure TJvFtpGrabber.Status(Sender: TObject; Position: Integer;
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

procedure TJvFtpGrabber.ThreadFinished(Sender: TObject);
begin
  FThread := nil;
end;

///////////////////////////////////////////////////////////
// TJvFtpThread
///////////////////////////////////////////////////////////

constructor TJvFtpThread.Create(Url, Username, FileName,
  Password: string; OutPutMode: TJvOutputMode; OnError: TOnError;
  OnDoneFile: TOnDoneFile; OnDoneStream: TOnDoneStream;
  OnProgress: TOnFtpProgress; Mode: TJvDownloadMode;
  Agent: string; OnStatus: TOnFtpProgress);
begin
  inherited Create(True);
  FUrl := Url;
  FUserName := Username;
  FFileName := FileName;
  FPassword := Password;
  FOutputMode := OutPutMode;
  FOnError := OnError;
  FOnDoneFile := OnDoneFile;
  FOnDoneStream := OnDoneStream;
  FOnProgress := OnProgress;
  FOnStatus := OnStatus;
  FMode := Mode;
  FAgent := Agent;
end;

{*************************************************}

procedure TJvFtpThread.Ended;
begin
  Stream.Position := 0;
  if FOutputMode = omStream then
    FOnDoneStream(Self, Stream, Stream.Size, FUrl)
  else
  begin
    Stream.SaveToFile(FFileName);
    FOnDoneFile(Self, FFileName, Stream.Size, FUrl);
  end;
end;

{*************************************************}

procedure TJvFtpThread.Error;
begin
  FOnError(Self, FErrorText);
end;

{**************************************************}

function TJvFtpThread.GetLastErrorMsg: string;
var
  msg: array[0..1000] of Char;
begin
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, 0, msg, 1000, nil);
  Result := msg;
end;

{*************************************************}

procedure FtpDownloadCallBack(Handle: HInternet; Context: DWord;
  Status: DWord; Info: Pointer; StatLen: DWord); stdcall;
begin
  with TJvFtpThread(Context) do
    FOnStatus(TJvFtpThread(Context), Status, FUrl);
end;

{*************************************************}

procedure TJvFtpThread.Execute;
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName: string;
  username, password: PChar;
  ReadedBytes, TotalBytes: DWORD;
  Buf: array[0..1024] of Byte;

  buffer: Pointer;
  dwBufLen, dwIndex: DWORD;

  procedure ParseUrl(Value: string);
  begin
    HostName := '';
    FileName := '';
    if Pos('FTP://', UpperCase(Value)) <> 0 then
      Value := Copy(Value, 7, Length(Value));
    if Pos('/', Value) <> 0 then
    begin
      HostName := Copy(Value, 1, Pos('/', Value) - 1);
      FileName := Copy(Value, Pos('/', Value) + 1, Length(Value));
    end
    else
      HostName := Value;
  end;

begin
  FErrorText := '';
  ParseUrl(FUrl);

  //Connect to the web
  hSession := InternetOpen(PChar(FAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if hSession = nil then
  begin
    FErrorText := GetLastErrorMsg;
    Synchronize(Error);
    Exit;
  end;

  //Connect to the hostname
  if FUserName = '' then
    username := nil
  else
    username := PChar(FUserName);
  if FPassword = '' then
    password := nil
  else
    password := PChar(FPassword);
  hHostConnection := InternetConnect(hSession, PChar(HostName), INTERNET_DEFAULT_FTP_PORT,
    username, password, INTERNET_SERVICE_FTP, 0, 0);
  if hHostConnection = nil then
  begin
    dwIndex := 0;
    dwBufLen := 1024;
    GetMem(Buffer, dwBufLen);
    InternetGetLastResponseInfo(dwIndex, buffer, dwBufLen);
    FErrorText := StrPas(buffer);
    FreeMem(Buffer);

    Synchronize(Error);
    Exit;
  end;

  InternetSetStatusCallback(hHostConnection, @FtpDownloadCallBack);

  //Request the file
  if FMode = hmBinary then
    hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ, FTP_TRANSFER_TYPE_BINARY or
      INTERNET_FLAG_DONT_CACHE, 0)
  else
    hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ, FTP_TRANSFER_TYPE_ASCII or
      INTERNET_FLAG_DONT_CACHE, 0);

  if hDownload = nil then
  begin
    FErrorText := GetLastErrorMsg;
    Synchronize(Error);
    Exit;
  end;

  Stream := TMemoryStream.Create;

  TotalBytes := 0;
  ReadedBytes := 1;
  while (ReadedBytes <> 0) do
  begin
    if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), ReadedBytes) then
      ReadedBytes := 0
    else
    begin
      Inc(TotalBytes, ReadedBytes);
      FBytesReaded := TotalBytes;
      Stream.Write(Buf, ReadedBytes);
      Synchronize(Progress);
    end;
  end;
  Synchronize(Ended);

  //Free all stuff's
  Stream.Free;

  //Release all handles
  if not (InternetCloseHandle(hDownload)) then
  begin
    FErrorText := GetLastErrorMsg;
    Synchronize(Error);
    Exit;
  end;
  if not (InternetCloseHandle(hHostConnection)) then
  begin
    FErrorText := GetLastErrorMsg;
    Synchronize(Error);
    Exit;
  end;
  if not (InternetCloseHandle(hSession)) then
  begin
    FErrorText := GetLastErrorMsg;
    Synchronize(Error);
    Exit;
  end;
end;

{*************************************************}

procedure TJvFtpThread.Progress;
begin
  FOnProgress(Self, FBytesReaded, FUrl);
end;

end.
