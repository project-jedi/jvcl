{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMultiHttpGrabber.PAS, released on 2001-02-28.

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

unit JvMultiHttpGrabber;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, WinInet, JvTypes, JvComponent;

type
  TUrlEvent = procedure(Sender: TObject; UserData: Integer;
    Url: string) of object;
  TUrlEventError = procedure(Sender: TObject; UserData: Integer;
    Url: string; Error: string) of object;
  TUrlResolved = procedure(Sender: TObject; UserData: Integer;
    Url: string; Name: string) of object;
  TUrlRedirect = procedure(Sender: TObject; UserData: Integer;
    Url: string; NewUrl: string) of object;
  TUrlSent = procedure(Sender: TObject; UserData: Integer;
    Url: string; DataSize: Integer) of object;
  TOnDoneFile = procedure(Sender: TObject; UserData: Integer; FileName: string;
    FileSize: Integer; Url: string) of object;
  TOnDoneStream = procedure(Sender: TObject; UserData: Integer; Stream: TMemoryStream;
    StreamSize: Integer; Url: string) of object;
  TOnProgress = procedure(Sender: TObject; UserData: Integer; Position: Integer;
    TotalSize: Integer; Url: string; var Continue: Boolean) of object;
  TDateEvent = procedure(Sender: TObject; UserData: Integer; FileDate: TDateTime;
    Url: string) of object;

  TJvMultiHttpGrabber = class(TJvComponent)
  private
    FAgent: string;
    FUrl: string;
    FReferer: string;
    FPassword: string;
    FUsername: string;
    FOutputMode: TJvOutputMode;
    FFileName: TFileName;
    FOnDoneFile: TOnDoneFile;
    FOnDoneStream: TOnDoneStream;
    FOnProgress: TOnProgress;
    FOnReceived: TUrlSent;
    FOnReceiving: TUrlEvent;
    FOnClosed: TUrlEvent;
    FOnConnecting: TUrlEvent;
    FOnResolving: TUrlEvent;
    FOnRedirect: TUrlRedirect;
    FOnConnected: TUrlEvent;
    FOnResolved: TUrlResolved;
    FOnClosing: TUrlEvent;
    FOnRequest: TUrlEvent;
    FOnSent: TUrlSent;
    FOnSending: TUrlEvent;
    FOnError: TUrlEventError;
    FCount: Integer;
    FOnDate: TDateEvent;
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
    property UserName: string read FUsername write FUsername;
    property Working: Boolean read GetWorking;

    property OnClosingConnection: TUrlEvent read FOnClosing write FOnClosing;
    property OnClosedConnection: TUrlEvent read FOnClosed write FOnClosed;
    property OnConnectingToServer: TUrlEvent read FOnConnecting write FOnConnecting;
    property OnConnectedToServer: TUrlEvent read FOnConnected write FOnConnected;
    property OnDoneFile: TOnDoneFile read FOnDoneFile write FOnDoneFile;
    property OnDoneStream: TOnDoneStream read FOnDoneStream write FOnDoneStream;
    property OnError: TUrlEventError read FOnError write FOnError;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property OnReceivingResponse: TUrlEvent read FOnReceiving write FOnReceiving;
    property OnReceivedResponse: TUrlSent read FOnReceived write FOnReceived;
    property OnRedirect: TUrlRedirect read FOnRedirect write FOnRedirect;
    property OnRequestComplete: TUrlEvent read FOnRequest write FOnRequest;
    property OnRequestSent: TUrlSent read FOnSent write FOnSent;
    property OnResolvingName: TUrlEvent read FOnResolving write FOnResolving;
    property OnResolvedName: TUrlResolved read FOnResolved write FOnResolved;
    property OnSendingRequest: TUrlEvent read FOnSending write FOnSending;

    property OnDateRetrieved: TDateEvent read FOnDate write FOnDate;
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

implementation

resourcestring
  RES_ErrorConnection = 'Unable to connect';
  RES_Agent = 'TJvMultiHttpGrabber Component';

type
  PRequestInfos = ^TRequestInfos;
  TRequestInfos = record
    Url: string;
    OutputMode: TJvOutputMode;

    hSession: HINTERNET;
    hHostConnect: HINTERNET;
    hRequest: HINTERNET;

    FileSize: Integer;

    IgnoreMsg: Boolean;
    Grabber: TJvMultiHttpGrabber;
    UserData: Integer;
  end;

  ///////////////////////////////////////////////////////////
  // TJvMultiHttpGrabber
  ///////////////////////////////////////////////////////////

constructor TJvMultiHttpGrabber.Create(AOwner: TComponent);
begin
  inherited;
  FOutputMode := omStream;
  FAgent := RES_Agent;
  FCount := 0;
end;

{*************************************************}

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
          if Assigned(Grabber.FOnReceiving) then
            Grabber.FOnReceiving(Grabber, UserData, Url);

        INTERNET_STATUS_REDIRECT:
          if Assigned(Grabber.FOnRedirect) then
            Grabber.FOnRedirect(Grabber, UserData, Url, StrPas(PChar(Info)));

        INTERNET_STATUS_REQUEST_COMPLETE:
          if Assigned(Grabber.FOnRequest) then
            Grabber.FOnRequest(Grabber, UserData, Url);

        INTERNET_STATUS_REQUEST_SENT:
          if Assigned(Grabber.FOnSent) then
            Grabber.FOnSent(Grabber, UserData, Url, DWORD(Info^));

        INTERNET_STATUS_RESOLVING_NAME:
          if Assigned(Grabber.FOnResolving) then
            Grabber.FOnResolving(Grabber, UserData, Url);

        INTERNET_STATUS_RESPONSE_RECEIVED:
          if Assigned(Grabber.FOnReceived) then
            Grabber.FOnReceived(Grabber, UserData, Url, DWORD(Info^));

        INTERNET_STATUS_SENDING_REQUEST:
          if Assigned(Grabber.FOnSending) then
            Grabber.FOnSending(Grabber, UserData, Url);
      end;
end;

{*************************************************}

procedure TJvMultiHttpGrabber.Download(UserData: Integer);
var
  Infos: PRequestInfos;
begin
  Infos := StartConnection(UserData, False);
  if Infos <> nil then
    with TJvMultiHttpThread.Create(Infos) do
    begin
      OnTerminate := ThreadTerminated;
      Resume;
      Inc(FCount);
    end;
end;

{*************************************************}

procedure TJvMultiHttpGrabber.GetFileAge(UserData: Integer = 0);
var
  Infos: PrequestInfos;
begin
  Infos := StartConnection(UserData, True);

  if Infos <> nil then
    with TJvMultiDateHttpThread.Create(Infos) do
    begin
      OnTerminate := ThreadDateTerminated;
      Resume;
    end;
end;

{*************************************************}

function TJvMultiHttpGrabber.GetWorking: Boolean;
begin
  Result := FCount > 0;
end;
{*************************************************}

procedure TJvMultiHttpGrabber.RaiseError(Value: Pointer);
var
  Msg: array[0..256] of Char;
begin
  if Assigned(FOnError) then
    with (PRequestInfos(Value))^ do
    begin
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, 0, Msg,
        SizeOf(Msg), nil);
      FOnError(Self, UserData, Url, Msg);
    end;
end;

{*************************************************}

procedure TJvMultiHttpGrabber.RaiseWebError(Infos: Pointer);
var
  dwIndex, dwBufLen: DWORD;
  buf: array[0..1024] of Char;
begin
  if Assigned(FOnError) then
  begin
    dwIndex := 0;
    dwBufLen := SizeOf(buf);
    InternetGetLastResponseInfo(dwIndex, buf, dwBufLen);
    with PRequestInfos(Infos)^ do
      FOnError(Self, UserData, Url, StrPas(buf));
  end;
end;

{*************************************************}

function TJvMultiHttpGrabber.StartConnection(UserData: Integer; IgnoreMessages: Boolean): Pointer;
var
  Infos: PRequestInfos;
  HostName, FilePath: string;

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
  end;

begin
  Result := nil;

  Infos := New(PRequestInfos);
  Infos^.Url := Url;
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
  InternetSetStatusCallback(Infos^.hSession, @StatusCallback);

  //Open the internet connection
  ParseUrl(Url);
  Infos^.hHostConnect := InternetConnect(Infos^.hSession, PChar(HostName),
    INTERNET_DEFAULT_HTTP_PORT, PChar(FUsername), PChar(FPassword), INTERNET_SERVICE_HTTP,
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

{*************************************************}

procedure TJvMultiHttpGrabber.StopConnection(Infos: Pointer);
begin
  InternetCloseHandle(PRequestInfos(Infos)^.hRequest);
  InternetCloseHandle(PRequestInfos(Infos)^.hHostConnect);
  InternetCloseHandle(PRequestInfos(Infos)^.hSession);
end;

{*************************************************}

procedure TJvMultiHttpGrabber.ThreadDateTerminated(Sender: TObject);
begin
  with Sender as TJvMultiDateHttpThread do
  begin
    with PRequestInfos(FInfos)^ do
      if Assigned(FOnDate) then
        FOnDate(Self, UserData, FValue, Url);

    StopConnection(FInfos);
    Dispose(FInfos);
    Free;
  end;
end;

{*************************************************}

procedure TJvMultiHttpGrabber.ThreadTerminated(Sender: TObject);
begin
  with Sender as TJvMultiHttpThread do
  begin
    with PRequestInfos(FInfos)^ do
      if (FStream <> nil) and (FStream.Size > 0) then
        if OutputMode = omStream then
        begin
          if Assigned(FOnDoneStream) then
            FOnDoneStream(Self, UserData, FStream, FStream.Size, Url)
        end
        else
        begin
          FStream.SaveToFile(FileName);
          if Assigned(FOnDoneFile) then
            FOnDoneFile(Self, UserData, FFileName, FStream.Size, Url);
        end;

    StopConnection(FInfos);
    Dispose(FInfos);
    Free;
  end;
  Dec(FCount);
end;

///////////////////////////////////////////////////////////
// TJvMultiHttpThread
///////////////////////////////////////////////////////////

constructor TJvMultiHttpThread.Create(Value: Pointer);
begin
  inherited Create(True);
  FInfos := Value;
  FPosition := 0;
  FContinue := True;
  FStream := nil;
end;

{*************************************************}

procedure TJvMultiHttpThread.Error;
var
  Infos: TRequestInfos;
begin
  Infos := PRequestInfos(FInfos)^;
  if Assigned(Infos.Grabber.FOnError) then
    Infos.Grabber.FOnError(Self, Infos.UserData, Infos.Url, RES_ErrorConnection);
end;

{*************************************************}

procedure TJvMultiHttpThread.Execute;
var
  Infos: PRequestInfos;
  buffer: array[0..512] of Byte;
  ReadedBytes: DWORD;
  dLength, dReserved, dSize: DWORD;
begin
  Infos := PRequestInfos(FInfos);

  //Send the request
  if not HttpSendRequest(Infos^.hRequest, nil, 0, nil, 0) then
  begin
    Synchronize(Error);
    Exit;
  end;

  //Get the Size
  dLength := SizeOf(dSize);
  dReserved := 0;
  if HttpQueryInfo(Infos^.hRequest, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER,
    @dSize, dLength, dReserved) then
  begin
    Infos^.FileSize := dSize;
  end
  else
    Infos^.FileSize := -1;

  //Download the stuff
  Synchronize(Progress);
  if not FContinue then
    Exit;

  FStream := TMemoryStream.Create;
  try
    repeat
      if not InternetReadFile(Infos^.hRequest, @buffer, SizeOf(buffer), ReadedBytes) then
        ReadedBytes := 0
      else
      begin
        Inc(FPosition, ReadedBytes);
        FStream.Write(buffer, ReadedBytes);
        Synchronize(Progress);
        if not FContinue then
        begin
          FStream.Free;
          FStream := nil;
          Exit;
        end;
      end;
    until ReadedBytes = 0;
    FStream.Position := 0;
  except
    FStream.Free;
    FStream := nil;
  end;
end;

{*************************************************}

procedure TJvMultiHttpThread.Progress;
begin
  with PRequestInfos(FInfos)^ do
    if Assigned(Grabber.OnProgress) then
      Grabber.OnProgress(Grabber, UserData, FPosition, FileSize, Url, FContinue);
end;

///////////////////////////////////////////////////////////
// TJvMultiDateHttpThread
///////////////////////////////////////////////////////////

constructor TJvMultiDateHttpThread.Create(Value: Pointer);
begin
  inherited Create(True);
  FInfos := Value;
end;

{*************************************************}

procedure TJvMultiDateHttpThread.Error;
var
  Infos: TRequestInfos;
begin
  Infos := PRequestInfos(FInfos)^;
  if Assigned(Infos.Grabber.FOnError) then
    Infos.Grabber.FOnError(Self, Infos.UserData, Infos.Url, RES_ErrorConnection);
end;

{*************************************************}

procedure TJvMultiDateHttpThread.Execute;
var
  Infos: PRequestInfos;
  STime: TSystemTime;
  dLength, dReserved: DWORD;
begin
  Infos := PRequestInfos(FInfos);

  dLength := SizeOf(TSystemTime);
  dReserved := 0;

  HttpSendRequest(Infos^.hRequest, nil, 0, nil, 0);

  if HttpQueryInfo(Infos^.hRequest, HTTP_QUERY_LAST_MODIFIED or HTTP_QUERY_FLAG_SYSTEMTIME,
    @STime, dLength, dReserved) then
    FValue := SystemTimeToDateTime(STime)
  else
    FValue := -1;
end;

end.
