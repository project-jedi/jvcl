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
Alejandro Castro [alejandro@alfra.info].

Last Modified: 2000-02-28 / 2002-10-18

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFtpGrabber;

interface

uses
  Windows, SysUtils, Classes, WinInet,
  JvTypes, JvComponent;

type
  TJvDownloadMode = (hmBinary, hmAscii);

  TJvFtpThread = class(TThread)
  private
    FSender: TObject; //acp
    FStream: TMemoryStream;
    FUrl: string;
    FUserName: string;
    FFileName: string;
    FPassword: string;
    FOutputMode: TJvOutputMode;
    FOnError: TJvErrorEvent;
    FOnDoneFile: TJvDoneFileEvent;
    FOnDoneStream: TJvDoneStreamEvent;
    FOnProgress: TJvFTPProgressEvent;
    FMode: TJvDownloadMode;
    FAgent: string;
    FBytesRead: Integer;
    FErrorText: string;
    FOnStatus: TJvFTPProgressEvent;
    FOnClosed: TNotifyEvent;
    function GetLastErrorMsg: string;
  protected
    procedure Error;
    procedure Progress;
    procedure Ended;
    procedure Execute; override;
    procedure Closed;
  public
    constructor Create(Url, UserName, FileName, Password: string;
      OutputMode: TJvOutputMode; OnError: TJvErrorEvent;
      OnDoneFile: TJvDoneFileEvent; OnDoneStream: TJvDoneStreamEvent;
      OnProgress: TJvFTPProgressEvent; Mode: TJvDownloadMode; Agent: string;
      OnStatus: TJvFTPProgressEvent; Sender: TObject; OnClosedConnection: TNotifyEvent); // acp
  end;

  TJvFtpGrabber = class(TJvComponent)
  private
    FSize: Integer; // acp
    FThread: TJvFtpThread;
    FUrl: string;
    FUserName: string;
    FFileName: TFileName;
    FPassword: string;
    FOutputMode: TJvOutputMode;
    FOnError: TJvErrorEvent;
    FOnDoneFile: TJvDoneFileEvent;
    FOnDoneStream: TJvDoneStreamEvent;
    FOnProgress: TJvFTPProgressEvent;
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
    procedure Closed(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Size: Integer read FSize write FSize; // acp
    property Url: string read FUrl write FUrl;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property FileName: TFileName read FFileName write FFileName;
    property OutputMode: TJvOutputMode read FOutputMode write FOutputMode default omStream;
    property Mode: TJvDownloadMode read FMode write FMode default hmBinary;
    property Agent: string read FAgent write FAgent;
    property OnDoneFile: TJvDoneFileEvent read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TJvDoneStreamEvent read FOnDoneStream write FOnDoneStream;
    property OnError: TJvErrorEvent read FOnError write FOnError;
    property OnProgress: TJvFTPProgressEvent read FOnProgress write FOnProgress;
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
    procedure Terminate;
  end;

implementation

{$IFNDEF COMPILER6_UP}
const
  WinetDll = 'wininet.dll';

function FtpGetFileSize(hFile: HINTERNET; lpdwFileSizeHigh: LPDWORD): DWORD; stdcall;
  external WinetDll name 'FtpGetFileSize';
{$EXTERNALSYM FtpGetFileSize}
{$ENDIF}

//=== TJvFtpGrabber ==========================================================

constructor TJvFtpGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUrl := '';
  FUserName := '';
  FPassword := '';
  FFileName := '';
  FOutputMode := omStream;
  FMode := hmBinary;
  FAgent := 'TJvHttpGrabber Component';
  FThread := nil;
  FSize := 0;
end;

destructor TJvFtpGrabber.Destroy;
begin
  if FThread <> nil then
  begin
    FThread.FreeOnTerminate := True;
    FThread.Terminate;
  end;
  inherited Destroy;
end;

procedure TJvFtpGrabber.Terminate; // acp
begin
  if Assigned(FThread) then
    FThread.Terminate;
end;

procedure TJvFtpGrabber.DoneFile(Sender: TObject; FileName: string;
  FileSize: Integer; Url: string);
begin
  if Assigned(FOnDoneFile) then
    FOnDoneFile(Self, FileName, FileSize, Url);
end;

procedure TJvFtpGrabber.DoneStream(Sender: TObject; Stream: TStream;
  StreamSize: Integer; Url: string);
begin
  if Assigned(FOnDoneStream) then
    FOnDoneStream(Self, Stream, StreamSize, Url);
end;

procedure TJvFtpGrabber.Error(Sender: TObject; ErrorMsg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg);
end;

procedure TJvFtpGrabber.Closed(Sender: TObject);
begin
  if Assigned(FOnClosed) then
    FOnClosed(Self);
end;

procedure TJvFtpGrabber.Execute;
begin
   //Download it
  if FThread = nil then
  begin
    FThread := TJvFtpThread.Create(Url, UserName, FileName, Password, OutputMode, Error, DoneFile, DoneStream,
      Progress, Mode, Agent, Status, Self, Closed); // acp
    FThread.OnTerminate := ThreadFinished;
    FThread.Resume;
  end;
end;

procedure TJvFtpGrabber.Progress(Sender: TObject; Position: Integer; Url: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, Url);
end;

procedure TJvFtpGrabber.Status(Sender: TObject; Position: Integer; Url: string);
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

procedure TJvFtpGrabber.ThreadFinished(Sender: TObject);
begin
  FThread := nil;
end;

//=== TJvFtpThread ===========================================================

constructor TJvFtpThread.Create(Url, UserName, FileName,
  Password: string; OutputMode: TJvOutputMode; OnError: TJvErrorEvent;
  OnDoneFile: TJvDoneFileEvent; OnDoneStream: TJvDoneStreamEvent;
  OnProgress: TJvFTPProgressEvent; Mode: TJvDownloadMode;
  Agent: string; OnStatus: TJvFTPProgressEvent; Sender: TObject; OnClosedConnection: TNotifyEvent); // acp
begin
  inherited Create(True);
  FUrl := Url;
  FUserName := UserName;
  FFileName := FileName;
  FPassword := Password;
  FOutputMode := OutputMode;
  FOnError := OnError;
  FOnDoneFile := OnDoneFile;
  FOnDoneStream := OnDoneStream;
  FOnProgress := OnProgress;
  FOnStatus := OnStatus;
  FMode := Mode;
  FAgent := Agent;
  FSender := Sender; // acp
  FOnClosed := OnClosedConnection;
end;

procedure TJvFtpThread.Closed;
begin
  FOnClosed(Self);
end;

procedure TJvFtpThread.Ended;
begin
  FStream.Position := 0;
  if FOutputMode = omStream then
    FOnDoneStream(Self, FStream, FStream.Size, FUrl)
  else
  begin
    FStream.SaveToFile(FFileName);
    FOnDoneFile(Self, FFileName, FStream.Size, FUrl);
  end;
end;

procedure TJvFtpThread.Error;
begin
  FOnError(Self, FErrorText);
end;

function TJvFtpThread.GetLastErrorMsg: string;
var
  Msg: array [0..1023] of Char;
begin
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, 0, Msg, SizeOf(Msg), nil);
  Result := Msg;
end;

procedure FtpDownloadCallBack(Handle: HInternet; Context: DWORD;
  Status: DWORD; Info: Pointer; StatLen: DWORD); stdcall;
begin
  with TJvFtpThread(Context) do
    FOnStatus(TJvFtpThread(Context), Status, FUrl);
end;

procedure TJvFtpThread.Execute;
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName: string;
  UserName, Password: PChar;
  BytesRead, TotalBytes: DWORD;
  Buf: array [0..1023] of Byte;
  dwFileSizeHigh: DWORD;
  Buffer: Pointer;
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
  // (rom) secure thread against exceptions
  FStream := nil;
  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  try
    try
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
        UserName := nil
      else
        UserName := PChar(FUserName);
      if FPassword = '' then
        Password := nil
      else
        Password := PChar(FPassword);
      hHostConnection := InternetConnect(hSession, PChar(HostName), INTERNET_DEFAULT_FTP_PORT,
        UserName, Password, INTERNET_SERVICE_FTP, 0, 0);
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

      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@FtpDownloadCallBack));

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
      (FSender as TJvFtpGrabber).FSize := FtpGetFileSize(hDownload, @dwFileSizeHigh); // acp

      FStream := TMemoryStream.Create;

      TotalBytes := 0;
      BytesRead := 1;
      while (BytesRead <> 0) and (not Terminated) do // acp
      begin
        if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), BytesRead) then
          BytesRead := 0
        else
        begin
          Inc(TotalBytes, BytesRead);
          FBytesRead := TotalBytes;
          FStream.Write(Buf, BytesRead);
          Synchronize(Progress);
        end;
      end;
      if not Terminated then // acp
        Synchronize(Ended);
    except
    end;
  finally
    //Free all stuff's
    FStream.Free;

    //Release all handles
    // (rom) now all connections get closed and Closed is always signalled
    if (hDownload <> nil) and not InternetCloseHandle(hDownload) then
    begin
      FErrorText := GetLastErrorMsg;
      Synchronize(Error);
    end;
    if (hHostConnection <> nil) and not InternetCloseHandle(hHostConnection) then
    begin
      FErrorText := GetLastErrorMsg;
      Synchronize(Error);
    end;
    if (hSession <> nil) and not InternetCloseHandle(hSession) then
    begin
      FErrorText := GetLastErrorMsg;
      Synchronize(Error);
    end;
    Synchronize(Closed);
  end;
end;

procedure TJvFtpThread.Progress;
begin
  FOnProgress(Self, FBytesRead, FUrl);
end;

end.

