{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUrlGrabbers.Pas, released on 2003-08-04.

The Initial Developer of the Original Code is Olivier Sannier [obones att altern dott org]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvUrlGrabbers;

{$I jvcl.inc}
{$I windowsonly.inc}

{$HPPEMIT '#pragma link "wininet.lib"'}

interface

uses
  Windows, Contnrs, Classes, SysUtils,
  JvUrlListGrabber, JvTypes, JvFinalize;

type
  // A grabber for FTP URLs
  TJvFtpDownloadMode = (hmBinary, hmAscii);

  TJvFtpUrlGrabberDefaultProperties = class(TJvCustomUrlGrabberDefaultProperties)
  protected
    FPassive: Boolean;
    FMode: TJvFtpDownloadMode;
    function GetSupportedURLName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TJvUrlGrabberDefaultPropertiesList); override;
  published
    property Agent;
    property UserName;
    property Password;
    property Port default 21;
    property Passive: Boolean read FPassive write FPassive default True;
    property Mode: TJvFtpDownloadMode read FMode write FMode default hmBinary;
  end;

  TJvFtpUrlGrabber = class(TJvCustomUrlGrabber)
  protected
    FPassiveFTP: Boolean;
    FMode: TJvFtpDownloadMode;
    function GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass; override;

    procedure DoStatus; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties); overload;
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; override;
    class function GetSupportedProtocolMarker: string; override;
    class function GetSupportedURLName: string; override;
  published
    property Passive: Boolean read FPassiveFTP write FPassiveFTP default True;
    property Mode: TJvFtpDownloadMode read FMode write FMode default hmBinary;

    property UserName;
    property Password;
    property FileName;
    property OutputMode;
    property Agent;
    property Url;
    property Port default 21;
    property OnDoneFile;
    property OnDoneStream;
    property OnError;
    property OnProgress;
    property OnResolvingName;
    property OnNameResolved;
    property OnConnectingToServer;
    property OnConnectedToServer;
    property OnSendingRequest;
    property OnRequestSent;
    property OnRequestComplete;
    property OnReceivingResponse;
    property OnResponseReceived;
    property OnClosingConnection;
    property OnConnectionClosed;
    property OnRedirect;
    property OnStatusChange;
  end;

  TJvFtpUrlGrabberThread = class(TJvCustomUrlGrabberThread)
  protected
    function GetGrabber: TJvFtpUrlGrabber;
    procedure Closed;
    procedure Execute; override;
  public
    property Grabber: TJvFtpUrlGrabber read GetGrabber;
  end;

  // A grabber for HTTP URLs
  TJvHttpUrlGrabber = class(TJvCustomUrlGrabber)
  private
    FReferer: string;
  protected
    function GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass; override;

    procedure DoStatus; override;
  public
    constructor Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties); 
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; override;
    class function GetSupportedProtocolMarker: string; override;
    class function GetSupportedURLName: string; override;
  published
    property Referer: string read FReferer write FReferer;

    property UserName;
    property Password;
    property FileName;
    property OutputMode;
    property Agent;
    property Url;
    property Port default 80;
    property OnDoneFile;
    property OnDoneStream;
    property OnError;
    property OnProgress;
    property OnResolvingName;
    property OnNameResolved;
    property OnConnectingToServer;
    property OnConnectedToServer;
    property OnSendingRequest;
    property OnRequestSent;
    property OnRequestComplete;
    property OnReceivingResponse;
    property OnResponseReceived;
    property OnClosingConnection;
    property OnConnectionClosed;
    property OnRedirect;
    property OnStatusChange;
  end;

  TJvHttpUrlGrabberDefaultProperties = class(TJvCustomUrlGrabberDefaultProperties)
  private
    FReferer: string;
  protected
    function GetSupportedURLName: string; override;
  public
    constructor Create(AOwner: TJvUrlGrabberDefaultPropertiesList); override;
  published
    property Referer: string read FReferer write FReferer;

    property Agent;
    property UserName;
    property Password;
    property Port default 80;
  end;

  TJvHttpUrlGrabberThread = class(TJvCustomUrlGrabberThread)
  protected
    FContinue: Boolean;
    function GetGrabber: TJvHttpUrlGrabber;
    procedure Execute; override;
  public
    constructor Create(Grabber: TJvCustomUrlGrabber); override;
    property Grabber: TJvHttpUrlGrabber read GetGrabber;
  end;

  // A grabber for local and UNC files
  TJvLocalFileUrlGrabber = class(TJvCustomUrlGrabber)
  private
    FPreserveAttributes: Boolean;
  protected
    function GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties); overload; 
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; override;
    class function GetSupportedProtocolMarker: string; override;
    class function GetSupportedURLName: string; override;
    procedure ParseUrl(URL: string; Protocol: string; var Host: string; var FileName: string;
                       var UserName: string; var Password: string; var Port: Cardinal); overload; override;
    procedure ParseUrl(const Url: string; var FileName: string); reintroduce; overload;
  published
    property PreserveAttributes: Boolean read FPreserveAttributes write FPreserveAttributes default True;
    property UserName;
    property Password;
    property FileName;
    property OutputMode;
    property Agent;
    property Url;
    property OnDoneFile;
    property OnDoneStream;
    property OnError;
    property OnProgress;
    property OnResolvingName;
    property OnNameResolved;
    property OnConnectingToServer;
    property OnConnectedToServer;
    property OnSendingRequest;
    property OnRequestSent;
    property OnRequestComplete;
    property OnReceivingResponse;
    property OnResponseReceived;
    property OnClosingConnection;
    property OnConnectionClosed;
    property OnRedirect;
    property OnStatusChange;
  end;

  TJvLocalFileUrlGrabberThread = class(TJvCustomUrlGrabberThread)
  protected
    function GetGrabber: TJvLocalFileUrlGrabber;
    procedure Execute; override;
  public
    property Grabber: TJvLocalFileUrlGrabber read GetGrabber;
  end;

  TJvLocalFileUrlGrabberProperties = class(TJvCustomUrlGrabberDefaultProperties)
  private
    FPreserveAttributes: Boolean;
  protected
    function GetSupportedURLName: string; override;
  public
    constructor Create(AOwner: TJvUrlGrabberDefaultPropertiesList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property PreserveAttributes: Boolean read FPreserveAttributes write FPreserveAttributes default True;
  end;

implementation

uses
  WinInet,
  JvResources;

const
  sUnitName = 'JvUrlGrabbers';

{$IFNDEF COMPILER6_UP}
function FtpGetFileSize(hFile: HINTERNET; lpdwFileSizeHigh: LPDWORD): DWORD; stdcall;
  external 'wininet.dll' name 'FtpGetFileSize';
{$ENDIF COMPILER6_UP}

// global download callback

procedure RegisterUrlGrabberClasses;
begin
  // register the classes
  JvUrlGrabberClassList.Add(TJvFtpUrlGrabber);
  JvUrlGrabberClassList.Add(TJvHttpUrlGrabber);
  JvUrlGrabberClassList.Add(TJvLocalFileUrlGrabber);
end;

procedure DownloadCallBack(Handle: HINTERNET; Context: DWORD;
  AStatus: DWORD; Info: Pointer; StatLen: DWORD); stdcall;
begin
  with TJvCustomUrlGrabberThread(Context) do
  begin
    Status := AStatus;
    DoProgress;
    DoStatus;
  end;
end;

// helper function to get the last error message from the internet functions
function GetLastInternetError: string;
var
  dwIndex: DWORD;
  dwBufLen: DWORD;
  Buffer: PChar;
begin
  dwIndex := 0;
  dwBufLen := 1024;
  GetMem(Buffer, dwBufLen);
  InternetGetLastResponseInfo(dwIndex, Buffer, dwBufLen);
  Result := StrPas(Buffer);
  FreeMem(Buffer);
end;

// helper procedure to trigger various events depending on the
// value of the given status.
procedure TriggerEventsFromStatus(Grabber: TJvCustomUrlGrabber;
  InternetStatusValue: DWORD);
begin
 with Grabber do
   case InternetStatusValue of
     INTERNET_STATUS_RESOLVING_NAME:
       if Assigned(OnResolvingName) then
         OnResolvingName(Grabber);
     INTERNET_STATUS_NAME_RESOLVED:
       if Assigned(OnNameResolved) then
         OnNameResolved(Grabber);
     INTERNET_STATUS_CONNECTING_TO_SERVER:
       if Assigned(OnConnectingToServer) then
         OnConnectingToServer(Grabber);
     INTERNET_STATUS_CONNECTED_TO_SERVER:
       if Assigned(OnConnectedToServer) then
         OnConnectedToServer(Grabber);
     INTERNET_STATUS_SENDING_REQUEST:
       if Assigned(OnSendingRequest) then
         OnSendingRequest(Grabber);
     INTERNET_STATUS_REQUEST_SENT:
       if Assigned(OnRequestSent) then
         OnRequestSent(Grabber);
     INTERNET_STATUS_RECEIVING_RESPONSE:
       if Assigned(OnReceivingResponse) then
         OnReceivingResponse(Grabber);
     INTERNET_STATUS_RESPONSE_RECEIVED:
       if Assigned(OnResponseReceived) then
         OnResponseReceived(Grabber);
     INTERNET_STATUS_CLOSING_CONNECTION:
       if Assigned(OnClosingConnection) then
         OnClosingConnection(Grabber);
     INTERNET_STATUS_CONNECTION_CLOSED:
       if Assigned(OnConnectionClosed) then
         OnConnectionClosed(Grabber);
     INTERNET_STATUS_REQUEST_COMPLETE:
       if Assigned(OnRequestComplete) then
         OnRequestComplete(Grabber);
     INTERNET_STATUS_REDIRECT:
       if Assigned(OnRedirect) then
         OnRedirect(Grabber);
     INTERNET_STATUS_STATE_CHANGE:
       if Assigned(OnStatusChange) then
         OnStatusChange(Grabber);
   end;
end;

//=== { TJvHttpUrlGrabber } ==================================================

constructor TJvHttpUrlGrabber.Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  inherited Create(AOwner, AUrl, DefaultProperties);
  Port := 80;
end;

class function TJvHttpUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  Result := LowerCase(Copy(Url, 1, 7)) = 'http://';
end;

class function TJvHttpUrlGrabber.GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass;
begin
  Result := TJvHttpUrlGrabberDefaultProperties;
end;

function TJvHttpUrlGrabber.GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass;
begin
  Result := TJvHttpUrlGrabberThread;
end;

class function TJvHttpUrlGrabber.GetSupportedProtocolMarker: string;
begin
  Result := 'http://';
end;

class function TJvHttpUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'HTTP';
end;

procedure TJvHttpUrlGrabber.DoStatus;
begin
  inherited;
  TriggerEventsFromStatus(Self, FUrlGrabberThread.Status);
end;

//=== { TJvFtpUrlGrabber } ===================================================

constructor TJvFtpUrlGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 21;
  Passive := True;
  Mode := hmBinary;
end;

constructor TJvFtpUrlGrabber.Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  inherited Create(AOwner, AUrl, DefaultProperties);
  Passive := TJvFtpUrlGrabberDefaultProperties(DefaultProperties).Passive;
  Mode := TJvFtpUrlGrabberDefaultProperties(DefaultProperties).Mode;
end;

class function TJvFtpUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  Result := LowerCase(Copy(Url, 1, 6)) = 'ftp://';
end;

class function TJvFtpUrlGrabber.GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass;
begin
  Result := TJvFtpUrlGrabberDefaultProperties;
end;

function TJvFtpUrlGrabber.GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass;
begin
  Result := TJvFtpUrlGrabberThread;
end;

class function TJvFtpUrlGrabber.GetSupportedProtocolMarker: string;
begin
  Result := 'ftp://';
end;

class function TJvFtpUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'FTP';
end;

procedure TJvFtpUrlGrabber.DoStatus;
begin
  inherited;
  TriggerEventsFromStatus(Self, FUrlGrabberThread.Status);
end;

//=== { TJvFtpUrlGrabberDefaultProperties } ==================================

constructor TJvFtpUrlGrabberDefaultProperties.Create(AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  inherited Create(AOwner);
  FPassive := True;
  FMode := hmBinary;
  Port := 21;
end;

procedure TJvFtpUrlGrabberDefaultProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvFtpUrlGrabberDefaultProperties then
    with Source as TJvFtpUrlGrabberDefaultProperties do
    begin
      Self.Mode := Mode;
      Self.Passive := Passive;
    end;
end;

function TJvFtpUrlGrabberDefaultProperties.GetSupportedURLName: string;
begin
  Result := TJvFtpUrlGrabber.GetSupportedURLName;
end;

//=== { TJvFtpUrlGrabberThread } =============================================

procedure TJvFtpUrlGrabberThread.Closed;
begin
  Grabber.DoClosed;
end;


procedure TJvFtpUrlGrabberThread.Execute;
const
  cPassive: array [Boolean] of DWORD = (0, INTERNET_FLAG_PASSIVE);
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName, strUserName, strPassword: string;
  UserName, Password: PChar;
  Port: Cardinal;
  LocalBytesRead, TotalBytes: DWORD;
  Buf: array [0..1023] of Byte;
  dwFileSizeHigh: DWORD;
begin
  Grabber.FStream := nil;
  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  try
    try
      FErrorText := '';
      Grabber.ParseUrl(Grabber.FUrl, Grabber.GetSupportedProtocolMarker, HostName, FileName, strUserName, strPassword, Port);
      if strUserName = '' then
        strUserName := Grabber.UserName;
      if strPassword = '' then
        strPassword := Grabber.Password;
      if Port = 0 then
        Port := Grabber.Port;
        
      // Setup the PChars for the call to InternetConnect
      if strUserName = '' then
        UserName := nil
      else
        UserName := PChar(strUserName);
      if strPassword = '' then
        Password := nil
      else
        Password := PChar(strPassword);

      // Connect to the web
      hSession := InternetOpen(PChar(Grabber.FAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
      if hSession = nil then
      begin
        FErrorText := GetLastInternetError;
        Synchronize(Error);
        Exit;
      end;
//      InternetSetStatusCallback(hSession, PFNInternetStatusCallback(@DownloadCallBack));

      // Connect to the hostname
      hHostConnection := InternetConnect(hSession, PChar(HostName), Port,
        UserName, Password, INTERNET_SERVICE_FTP, cPassive[Grabber.FPassiveFTP], 0);
      if hHostConnection = nil then
      begin
        FErrorText := GetLastInternetError;
        Synchronize(Error);
        Exit;
      end;

      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@DownloadCallBack));

      // Request the file
      if Grabber.FMode = hmBinary then
        hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ, FTP_TRANSFER_TYPE_BINARY or
          INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_RELOAD, 0)
      else
        hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ, FTP_TRANSFER_TYPE_ASCII or
          INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_RELOAD, 0);

      if Terminated then
        Exit;

      if hDownload = nil then
      begin
        FErrorText := GetLastInternetError;
        Synchronize(Error);
        Exit;
      end;
      Grabber.FSize := FtpGetFileSize(hDownload, @dwFileSizeHigh); // acp

      if Terminated then
        Exit;

      Grabber.FStream := TMemoryStream.Create;

      TotalBytes := 0;
      LocalBytesRead := 1;
      while (LocalBytesRead <> 0) and not Terminated and FContinue do // acp
      begin
        if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), LocalBytesRead) then
          LocalBytesRead := 0
        else
        begin
          Inc(TotalBytes, LocalBytesRead);
          Grabber.FBytesRead := TotalBytes;
          Grabber.FStream.Write(Buf, LocalBytesRead);
          DoProgress;
        end;
      end;
      if not Terminated and FContinue then // acp
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
      FErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
    if (hHostConnection <> nil) and not InternetCloseHandle(hHostConnection) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
    if (hSession <> nil) and not InternetCloseHandle(hSession) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
    Synchronize(Closed);
  end;
end;

function TJvFtpUrlGrabberThread.GetGrabber: TJvFtpUrlGrabber;
begin
  Result := TJvFtpUrlGrabber(FGrabber);
end;

//=== { TJvHttpUrlGrabberThread } ============================================

constructor TJvHttpUrlGrabberThread.Create(Grabber: TJvCustomUrlGrabber);
begin
  inherited Create(Grabber);
end;

procedure TJvHttpUrlGrabberThread.Execute;
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName, strUserName, strPassword: string;
  UserName, Password: PChar;
  Port : Cardinal;
  Buffer: PChar;
  dwBufLen, dwIndex, dwBytesRead, dwTotalBytes: DWORD;
  HasSize: Boolean;
  Buf: array [0..1024] of Byte;
begin
  Buffer := nil;

  FContinue := True;
  Grabber.FStream := nil;
  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  try
    try
      Grabber.ParseUrl(Grabber.FUrl, Grabber.GetSupportedProtocolMarker, HostName, FileName, strUserName, strPassword, Port);
      if strUserName = '' then
        strUserName := Grabber.UserName;
      if strPassword = '' then
        strPassword := Grabber.Password;
      if Port = 0 then
        Port := Grabber.Port;
        
      // Setup the PChars for the call to InternetConnect
      if strUserName = '' then
        UserName := nil
      else
        UserName := PChar(strUserName);
      if strPassword = '' then
        Password := nil
      else
        Password := PChar(strPassword);
      
      FErrorText := '';

      //Connect to the web
      hSession := InternetOpen(PChar(Grabber.FAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
      if hSession = nil then
      begin
        FErrorText := SysErrorMessage(GetLastError);
        Synchronize(Error);
        Exit;
      end;
      InternetSetStatusCallback(hSession, PFNInternetStatusCallback(@DownloadCallBack));


      // Connect to the host
      hHostConnection := InternetConnect(hSession, PChar(HostName), Port,
        UserName, Password, INTERNET_SERVICE_HTTP, 0, DWORD(Self));

      if Terminated then
        Exit;

      if hHostConnection = nil then
      begin
        FErrorText := GetLastInternetError;
        Buffer := nil;
        Synchronize(Error);
        Exit;
      end;

//      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@DownloadCallBack));
      //Request the file
      hDownload := HttpOpenRequest(hHostConnection, 'GET', PChar(FileName), 'HTTP/1.0', PChar(Grabber.Referer),
        nil, INTERNET_FLAG_RELOAD or INTERNET_FLAG_PRAGMA_NOCACHE, 0);

      if hDownload = nil then
      begin
        FErrorText := GetLastInternetError;
        Synchronize(Error);
        Exit;
      end;
//      InternetSetStatusCallback(hDownload, PFNInternetStatusCallback(@DownloadCallBack));

      //Send the request
      HttpSendRequest(hDownload, nil, 0, nil, 0);

      if Terminated then
        Exit;

      Grabber.FStream := TMemoryStream.Create;

      dwIndex := 0;
      dwBufLen := 1024;
      GetMem(Buffer, dwBufLen);
      HasSize := HttpQueryInfo(hDownload, HTTP_QUERY_CONTENT_LENGTH, Buffer, dwBufLen, dwIndex);
      if Terminated then
        Exit;

      if HasSize then
        Grabber.FSize := StrToInt(StrPas(Buffer))
      else
        Grabber.FSize := 0;

      dwTotalBytes := 0;
      if HasSize then
      begin
        dwBytesRead := 1;
        while (dwBytesRead > 0) and not Terminated and FContinue do
        begin
          if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) then
            dwBytesRead := 0
          else
          begin
            Inc(dwTotalBytes, dwBytesRead);
            Grabber.FBytesRead := dwTotalBytes;
            Grabber.FStream.Write(Buf, dwBytesRead);
            DoProgress;
          end;
        end;
        if FContinue and not Terminated then
          Synchronize(Ended);
      end
      else
      begin
        while InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) and not Terminated do
        begin
          if dwBytesRead = 0 then
            Break;
          Grabber.FStream.Write(Buf, dwBytesRead);
          Synchronize(UpdateGrabberProgress);
        end;
        if FContinue and not Terminated then
          Synchronize(Ended);
      end;
    except
    end;
  finally
    // Free all stuff's
    if Buffer <> nil then
      FreeMem(Buffer);
    Grabber.FStream.Free;

    // Release all handles
    if (hDownload <> nil) and not InternetCloseHandle(hDownload) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
    if (hHostConnection <> nil) and not InternetCloseHandle(hHostConnection) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
    if (hSession <> nil) and not InternetCloseHandle(hSession) then
    begin
      FErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
  end;
end;

function TJvHttpUrlGrabberThread.GetGrabber: TJvHttpUrlGrabber;
begin
  Result := TJvHttpUrlGrabber(FGrabber);
end;

//=== { TJvHttpUrlGrabberDefaultProperties } =================================

constructor TJvHttpUrlGrabberDefaultProperties.Create(
  AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  inherited Create(AOwner);
  Port := 80;
end;

function TJvHttpUrlGrabberDefaultProperties.GetSupportedURLName: string;
begin
  Result := TJvHttpUrlGrabber.GetSupportedURLName;
end;

//=== { TJvLocalFileUrlGrabber } =============================================

constructor TJvLocalFileUrlGrabber.Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  inherited Create(AOwner, AUrl, DefaultProperties);
  FPreserveAttributes := TJvLocalFileUrlGrabberProperties(DefaultProperties).PreserveAttributes;
end;

constructor TJvLocalFileUrlGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPreserveAttributes := True;
end;

class function TJvLocalFileUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  // accepts "file://", UNC and local path and existing files
  Result := (LowerCase(Copy(Url, 1, 7)) = 'file://') or (Copy(Url,1,2) = '//') or
    (Copy(Url, 2,2) = ':\') or FileExists(Url);
end;

class function TJvLocalFileUrlGrabber.GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass;
begin
  Result := TJvLocalFileUrlGrabberProperties;
end;

function TJvLocalFileUrlGrabber.GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass;
begin
  Result := TJvLocalFileUrlGrabberThread;
end;

class function TJvLocalFileUrlGrabber.GetSupportedProtocolMarker: string;
begin
  Result := 'file://';
end;

class function TJvLocalFileUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'LocalFile';
end;

procedure TJvLocalFileUrlGrabber.ParseUrl(URL, Protocol: string; var Host,
  FileName, UserName, Password: string; var Port: Cardinal);
begin
  ParseUrl(URL, FileName);
end;

procedure TJvLocalFileUrlGrabber.ParseUrl(const Url: string;
  var FileName: string);
begin
  FileName := StringReplace(Url, '/', '\', [rfReplaceAll]);
  if AnsiSameText(Copy(Url, 1, 7), 'file://') then
    FileName := Copy(FileName, 8, MaxInt)
  else
    FileName := ExpandUNCFilename(FileName);
end;

//=== { TJvLocalFileUrlGrabberThread } =======================================

procedure TJvLocalFileUrlGrabberThread.Execute;
var
  FileName: string;
  BytesRead, TotalBytes: DWORD;
  Buf: array [0..1023] of Byte;
  AFileStream: TFileStream;
  Attrs: Integer;
begin
  Grabber.FStream := nil;
  Grabber.ParseUrl(Grabber.FUrl, FileName);
  if not FileExists(FileName) then
  begin
    FErrorText := Format(RsFileNotFoundFmt, [FileName]);
    Synchronize(Error);
    Exit;
  end;

  if Grabber.PreserveAttributes then
    Attrs := GetFileAttributes(PChar(FileName))
  else
    Attrs := 0;
  try
    FErrorText := '';
    Grabber.FStream := TMemoryStream.Create;
    AFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Grabber.FSize := AFileStream.Size;
      Grabber.FBytesRead := 0;
      FStatus    := 0;
      DoProgress;
      TotalBytes := 0;
      BytesRead := 1;
      while (BytesRead <> 0) and not Terminated and FContinue do
      begin
        BytesRead := AFileStream.Read(Buf, SizeOf(Buf));
        Inc(TotalBytes, BytesRead);
        Grabber.FBytesRead := TotalBytes;
        FStatus := Grabber.FBytesRead;
        if BytesRead > 0 then
          Grabber.FStream.Write(Buf, BytesRead);
        DoProgress;
      end;
      if not Terminated and FContinue then // acp
        Synchronize(Ended);
      if Grabber.PreserveAttributes and FileExists(Grabber.FileName) then
        SetFileAttributes(PChar(Grabber.FileName), Attrs);
    finally
      AFileStream.Free;
      Grabber.FStream.Free;
      Grabber.FStream := nil;
    end;
  except
//    Application.HandleException(Self);
  end;
end;

function TJvLocalFileUrlGrabberThread.GetGrabber: TJvLocalFileUrlGrabber;
begin
  Result := TJvLocalFileUrlGrabber(FGrabber);
end;

//=== { TJvLocalFileUrlGrabberProperties } ===================================

constructor TJvLocalFileUrlGrabberProperties.Create(AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  inherited Create(AOwner);
  FPreserveAttributes := True;
end;

procedure TJvLocalFileUrlGrabberProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvLocalFileUrlGrabberProperties then
    with Source as TJvLocalFileUrlGrabberProperties do
      Self.PreserveAttributes := PreserveAttributes;
end;

function TJvLocalFileUrlGrabberProperties.GetSupportedURLName: string;
begin
  Result := TJvLocalFileUrlGrabber.GetSupportedURLName;
end;

initialization
  RegisterUrlGrabberClasses;

finalization
  FinalizeUnit(sUnitName);

end.

