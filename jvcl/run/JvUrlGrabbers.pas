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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvUrlGrabbers;

{$I jvcl.inc}
{$I windowsonly.inc}

{$IFDEF WIN64}
{$HPPEMIT '#pragma link "wininet.a"'}
{$ELSE}
{$HPPEMIT '#pragma link "wininet.lib"'}
{$ENDIF WIN64}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, SysUtils,
  JvUrlListGrabber, JvTypes;

type
  // A grabber than can use a proxy. Note that this class is not registered,
  // it is just a base class for other grabbers
  // see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/wininet/wininet/enabling_internet_functionality.asp
  // for details on the values to give to the proxy properties (address and ignorelist)
  TJvProxyMode = (pmNoProxy, pmSysConfig, pmManual);

  TJvProxyingUrlGrabber = class (TJvCustomUrlGrabber)
  protected
    FProxyAddresses: string;
    FProxyMode: TJvProxyMode;
    FProxyIgnoreList: string;
    FProxyUserName: string;
    FProxyPassword: string;
    property ProxyMode: TJvProxyMode read FProxyMode write FProxyMode default pmSysConfig;
    property ProxyAddresses: string read FProxyAddresses write FProxyAddresses;
    property ProxyIgnoreList: string read FProxyIgnoreList write FProxyIgnoreList;
    property ProxyUserName: string read FProxyUserName write FProxyUserName;
    property ProxyPassword: string read FProxyPassword write FProxyPassword;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties); overload;
  end;

  TJvProxyingUrlGrabberDefaultProperties = class(TJvCustomUrlGrabberDefaultProperties)
  protected
    FProxyAddresses: string;
    FProxyMode: TJvProxyMode;
    FProxyIgnoreList: string;
    FProxyUserName: string;
    FProxyPassword: string;
    property ProxyMode: TJvProxyMode read FProxyMode write FProxyMode default pmSysConfig;
    property ProxyAddresses: string read FProxyAddresses write FProxyAddresses;
    property ProxyIgnoreList: string read FProxyIgnoreList write FProxyIgnoreList;
    property ProxyUserName: string read FProxyUserName write FProxyUserName;
    property ProxyPassword: string read FProxyPassword write FProxyPassword;
  public
    constructor Create(AOwner: TJvUrlGrabberDefaultPropertiesList); override;
  end;

  // A grabber for FTP URLs
  TJvFtpDownloadMode = (hmBinary, hmAscii);

  TJvFtpUrlGrabberDefaultProperties = class(TJvProxyingUrlGrabberDefaultProperties)
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
    property ProxyMode;
    property ProxyAddresses;
    property ProxyIgnoreList;
    property ProxyUserName;
    property ProxyPassword;
    property Port default 21;
    property Passive: Boolean read FPassive write FPassive default True;
    property Mode: TJvFtpDownloadMode read FMode write FMode default hmBinary;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFtpUrlGrabber = class(TJvProxyingUrlGrabber)
  protected
    FPassive: Boolean;
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
    property Passive: Boolean read FPassive write FPassive default True;
    property Mode: TJvFtpDownloadMode read FMode write FMode default hmBinary;
    property UserName;
    property Password;
    property FileName;
    property OutputMode;
    property Agent;
    property Url;
    property ProxyMode;
    property ProxyAddresses;
    property ProxyIgnoreList;
    property ProxyUserName;
    property ProxyPassword;
    property Port default 21;
    property TimeOut;
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
    procedure Grab; override;
  public
    property Grabber: TJvFtpUrlGrabber read GetGrabber;
  end;

  // A grabber for HTTP URLs
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvHttpUrlGrabber = class(TJvProxyingUrlGrabber)
  private
    FReferer: string;
    FHTTPStatusCode: DWORD;
    FHTTPStatusText: string;
    function GetHTTPStatus: string;
  protected
    function GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass; override;
    procedure DoStatus; override;
  public
    constructor Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; override;
    class function GetSupportedProtocolMarker: string; override;
    class function GetSupportedURLName: string; override;

    // The status line (Code + ' ' + Text)
    property HTTPStatus: string read GetHTTPStatus;
    // The status (200, 404, 301) as returned by the HTTP server.
    property HTTPStatusCode: DWORD read FHTTPStatusCode;
    property HTTPStatusText: string read FHTTPStatusText;
  published
    property Referer: string read FReferer write FReferer;
    property UserName;
    property Password;
    property FileName;
    property OutputMode;
    property Agent;
    property Url;
    property Port default 80;
    property ProxyMode;
    property ProxyAddresses;
    property ProxyIgnoreList;
    property ProxyUserName;
    property ProxyPassword;
    property TimeOut;
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

  TJvHttpUrlGrabberDefaultProperties = class(TJvProxyingUrlGrabberDefaultProperties)
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
    property ProxyMode;
    property ProxyAddresses;
    property ProxyIgnoreList;
    property ProxyUserName;
    property ProxyPassword;
  end;

  TJvHttpUrlGrabberThread = class(TJvCustomUrlGrabberThread)
  protected
    function GetGrabber: TJvHttpUrlGrabber;
    procedure Grab; override;
  public
    property Grabber: TJvHttpUrlGrabber read GetGrabber;
  end;

  // A grabber for Secure HTTP URLs
  TJvHttpsUrlGrabber = class(TJvHttpUrlGrabber)
  public
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; override;
    class function GetSupportedProtocolMarker: string; override;
    class function GetSupportedURLName: string; override;
  end;

  TJvHttpsUrlGrabberDefaultProperties = class(TJvHttpUrlGrabberDefaultProperties)
  protected
    function GetSupportedURLName: string; override;
  end;

  // A grabber for local and UNC files
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
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
    class procedure ParseUrl(Url: string; Protocol: string; var Host: string; var FileName: string;
      var UserName: string; var Password: string; var Port: Cardinal); overload; override;
    class procedure ParseUrl(const Url: string; var FileName: string); reintroduce; overload;
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
    procedure Grab; override;
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

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  WinInet,
  JclBase, // DWORD_PTR
  JvResources;

const
  cFilePrefix = 'file://';
  cHTTPPrefix = 'http://';
  cHTTPsPrefix = 'https://';
  cFTPPrefix = 'ftp://';

procedure RegisterUrlGrabberClasses;
begin
  // register the classes
  JvUrlGrabberClassList.Add(TJvFtpUrlGrabber);
  JvUrlGrabberClassList.Add(TJvHttpUrlGrabber);
  JvUrlGrabberClassList.Add(TJvHttpsUrlGrabber);
  JvUrlGrabberClassList.Add(TJvLocalFileUrlGrabber);
end;

// global download callback
procedure DownloadCallBack(Handle: HINTERNET; Context: DWORD_PTR;
  AStatus: DWORD; Info: Pointer; StatLen: DWORD); stdcall;
begin
  with TJvCustomUrlGrabberThread(Context) do
  begin
    APIStatus := AStatus;
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
  GetMem(Buffer, dwBufLen * SizeOf(Char));
  try
    InternetGetLastResponseInfo(dwIndex, Buffer, dwBufLen);
    Result := StrPas(Buffer);
  finally
    FreeMem(Buffer);
  end;
end;

// helper procedure to trigger various events depending on the
// value of the given status.

procedure TriggerEventsFromStatus(Grabber: TJvCustomUrlGrabber; InternetStatusValue: DWORD);
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

//=== { TJvProxyingUrlGrabber } ==============================================

constructor TJvProxyingUrlGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FProxyMode := pmSysConfig;
  FProxyAddresses := 'proxyserver';
  FProxyIgnoreList := '<local>';
end;

constructor TJvProxyingUrlGrabber.Create(AOwner: TComponent; AUrl: string;
  DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  inherited Create(AOwner, AUrl, DefaultProperties);

  // TODO add else (probably exception).
  if DefaultProperties is TJvFtpUrlGrabberDefaultProperties then
  begin
    ProxyMode := TJvProxyingUrlGrabberDefaultProperties(DefaultProperties).ProxyMode;
    ProxyAddresses := TJvProxyingUrlGrabberDefaultProperties(DefaultProperties).ProxyAddresses;
    ProxyIgnoreList := TJvProxyingUrlGrabberDefaultProperties(DefaultProperties).ProxyIgnoreList;
  end;
end;

//=== { TJvProxyingUrlGrabberDefaultProperties } =============================

constructor TJvProxyingUrlGrabberDefaultProperties.Create(
  AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  inherited Create(AOwner);

  FProxyMode := pmSysConfig;
  FProxyAddresses := 'proxyserver';
  FProxyIgnoreList := '<local>';
end;

//=== { TJvHttpUrlGrabber } ==================================================

constructor TJvHttpUrlGrabber.Create(AOwner: TComponent; AUrl: string;
  DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  inherited Create(AOwner, AUrl, DefaultProperties);
  Port := 80;
end;

class function TJvHttpUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  Result := LowerCase(Copy(Url, 1, 7)) = cHTTPPrefix;
end;

class function TJvHttpUrlGrabber.GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass;
begin
  Result := TJvHttpUrlGrabberDefaultProperties;
end;

function TJvHttpUrlGrabber.GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass;
begin
  Result := TJvHttpUrlGrabberThread;
end;

function TJvHttpUrlGrabber.GetHTTPStatus: string;
begin
  Result := IntToStr(HTTPStatusCode) + ' ' + HTTPStatusText;
end;

class function TJvHttpUrlGrabber.GetSupportedProtocolMarker: string;
begin
  Result := cHTTPPrefix;
end;

class function TJvHttpUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'HTTP';
end;

procedure TJvHttpUrlGrabber.DoStatus;
begin
  inherited DoStatus;
  TriggerEventsFromStatus(Self, UrlGrabberThread.APIStatus);
end;

//=== { TJvFtpUrlGrabber } ===================================================

constructor TJvFtpUrlGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Port := 21;
  Passive := True;
  Mode := hmBinary;
end;

constructor TJvFtpUrlGrabber.Create(AOwner: TComponent; AUrl: string;
  DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  inherited Create(AOwner, AUrl, DefaultProperties);
  // (rom) added check. TODO add else (probably exception).
  if DefaultProperties is TJvFtpUrlGrabberDefaultProperties then
  begin
    Passive := TJvFtpUrlGrabberDefaultProperties(DefaultProperties).Passive;
    Mode := TJvFtpUrlGrabberDefaultProperties(DefaultProperties).Mode;
  end;
end;

class function TJvFtpUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  Result := LowerCase(Copy(Url, 1, 6)) = cFTPPrefix;
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
  Result := cFTPPrefix;
end;

class function TJvFtpUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'FTP';
end;

procedure TJvFtpUrlGrabber.DoStatus;
begin
  inherited DoStatus;
  TriggerEventsFromStatus(Self, UrlGrabberThread.APIStatus);
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

procedure TJvFtpUrlGrabberThread.Grab;
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
  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  try
    try
      ErrorText := '';
      Grabber.ParseUrl(Grabber.Url, Grabber.GetSupportedProtocolMarker,
        HostName, FileName, strUserName, strPassword, Port);
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
      SetGrabberStatus(gsConnecting);
      case Grabber.ProxyMode of
        pmNoProxy:
          hSession := InternetOpen(PChar(Grabber.Agent), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
        pmSysConfig:
          hSession := InternetOpen(PChar(Grabber.Agent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
        pmManual:
          hSession := InternetOpen(PChar(Grabber.Agent), INTERNET_OPEN_TYPE_PROXY, PChar(Grabber.ProxyAddresses), PChar(Grabber.ProxyIgnoreList), 0);
      end;
      if hSession = nil then
      begin
        ErrorText := GetLastInternetError;
        Synchronize(Error);
        Exit;
      end;
      if Grabber.ProxyMode = pmManual then
      begin
        // if manual mode and valid session, we set proxy user name and password
        InternetSetOption(hSession, INTERNET_OPTION_PROXY_USERNAME, PChar(Grabber.ProxyUserName), Length(Grabber.ProxyUserName)+1);
        InternetSetOption(hSession, INTERNET_OPTION_PROXY_PASSWORD, PChar(Grabber.ProxyPassword), Length(Grabber.ProxyPassword)+1);
      end;
      Grabber.TimeOut.SetupSession(hSession);

//      InternetSetStatusCallback(hSession, PFNInternetStatusCallback(@DownloadCallBack));

      // Connect to the hostname
      hHostConnection := InternetConnect(hSession, PChar(HostName), Port,
        UserName, Password, INTERNET_SERVICE_FTP, cPassive[Grabber.Passive], 0);
      if hHostConnection = nil then
      begin
        ErrorText := GetLastInternetError;
        Synchronize(Error);
        Exit;
      end;

      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@DownloadCallBack));

      // Request the file
      if Grabber.FMode = hmBinary then
        hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ,
          FTP_TRANSFER_TYPE_BINARY or INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_RELOAD, 0)
      else
        hDownload := FtpOpenFile(hHostConnection, PChar(FileName), GENERIC_READ,
          FTP_TRANSFER_TYPE_ASCII or INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_RELOAD, 0);

      if Terminated then
        Exit;

      if hDownload = nil then
      begin
        ErrorText := GetLastInternetError;
        Synchronize(Error);
        Exit;
      end;
      Grabber.SetSize(FtpGetFileSize(hDownload, @dwFileSizeHigh)); // acp

      if Terminated then
        Exit;

      Grabber.Stream := TMemoryStream.Create;

      TotalBytes := 0;
      LocalBytesRead := 1;
      SetGrabberStatus(gsGrabbing);
      while (LocalBytesRead <> 0) and not Terminated and Continue do // acp
      begin
        if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), LocalBytesRead) then
          LocalBytesRead := 0
        else
        begin
          Inc(TotalBytes, LocalBytesRead);
          Grabber.SetBytesRead(TotalBytes);
          Grabber.Stream.Write(Buf, LocalBytesRead);
          DoProgress;
        end;

        // Be CPU friendly.
        SleepEx(0, True);
      end;

      SetGrabberStatus(gsStopping);
      if not Terminated and Continue then // acp
        Synchronize(Ended);
    except
    end;
  finally
    //Release all handles
    // (rom) now all connections get closed and Closed is always signalled
    if (hDownload <> nil) and not InternetCloseHandle(hDownload) then
    begin
      ErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
    if (hHostConnection <> nil) and not InternetCloseHandle(hHostConnection) then
    begin
      ErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
    if (hSession <> nil) and not InternetCloseHandle(hSession) then
    begin
      ErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
  end;
end;

function TJvFtpUrlGrabberThread.GetGrabber: TJvFtpUrlGrabber;
begin
  Result := TJvFtpUrlGrabber(FGrabber);
end;

//=== { TJvHttpUrlGrabberThread } ============================================

procedure TJvHttpUrlGrabberThread.Grab;
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName, strUserName, strPassword: string;
  UserName, Password: PChar;
  Port: Cardinal;
  Buffer: PChar;
  dwBufLen, dwIndex, dwBytesRead, dwTotalBytes: DWORD;
  HasSize: Boolean;
  Buf: array [0..1024] of Byte;
begin
  Buffer := nil;

  Continue := True;
  hSession := nil;
  hHostConnection := nil;
  hDownload := nil;
  try
    try
      Grabber.ParseUrl(Grabber.Url, Grabber.GetSupportedProtocolMarker,
        HostName, FileName, strUserName, strPassword, Port);
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

      ErrorText := '';

      //Connect to the web
      SetGrabberStatus(gsConnecting);
      case Grabber.ProxyMode of
        pmNoProxy:
          hSession := InternetOpen(PChar(Grabber.Agent), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
        pmSysConfig:
          hSession := InternetOpen(PChar(Grabber.Agent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
        pmManual:
          hSession := InternetOpen(PChar(Grabber.Agent), INTERNET_OPEN_TYPE_PROXY, PChar(Grabber.ProxyAddresses), PChar(Grabber.ProxyIgnoreList), 0);
      end;
      if hSession = nil then
      begin
        ErrorText := SysErrorMessage(GetLastError);
        Synchronize(Error);
        Exit;
      end;
      Grabber.TimeOut.SetupSession(hSession);
      InternetSetStatusCallback(hSession, PFNInternetStatusCallback(@DownloadCallBack));

      // Connect to the host
      hHostConnection := InternetConnect(hSession, PChar(HostName), Port,
        UserName, Password, INTERNET_SERVICE_HTTP, 0, DWORD_PTR(Self));

      if Terminated then
        Exit;

      if hHostConnection = nil then
      begin
        ErrorText := GetLastInternetError;
        Buffer := nil;
        Synchronize(Error);
        Exit;
      end;

//      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@DownloadCallBack));
      //Request the file
      hDownload := HttpOpenRequest(hHostConnection, 'GET', PChar(FileName), 'HTTP/1.0',
        PChar(Grabber.Referer), nil, INTERNET_FLAG_RELOAD or INTERNET_FLAG_PRAGMA_NOCACHE, 0);

      if hDownload = nil then
      begin
        ErrorText := GetLastInternetError;
        Synchronize(Error);
        Exit;
      end;
//      InternetSetStatusCallback(hDownload, PFNInternetStatusCallback(@DownloadCallBack));

      if Grabber.ProxyMode in [pmManual, pmSysConfig] then
      begin
        // if manual mode and valid session, we set proxy user name and password
        InternetSetOption(hDownload,
                          INTERNET_OPTION_PROXY_USERNAME,
                          PChar(Grabber.ProxyUserName),
                          Length(Grabber.ProxyUserName)+1);
        InternetSetOption(hDownload,
                          INTERNET_OPTION_PROXY_PASSWORD,
                          PChar(Grabber.ProxyPassword),
                          Length(Grabber.ProxyPassword)+1);
      end;

      //Send the request
      HttpSendRequest(hDownload, nil, 0, nil, 0);

      if Terminated then
        Exit;

      Grabber.Stream := TMemoryStream.Create;

      dwBufLen := SizeOf(Grabber.FHTTPStatusCode);
      HttpQueryInfo(hDownload, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @Grabber.FHTTPStatusCode, dwBufLen, dwIndex);

      dwIndex := 0;
      dwBufLen := 1024;
      GetMem(Buffer, dwBufLen * SizeOf(Char));
      HttpQueryInfo(hDownload, HTTP_QUERY_STATUS_TEXT, Buffer, dwBufLen, dwIndex);
      Grabber.FHTTPStatusText := Buffer;

      dwIndex := 0;
      dwBufLen := 1024;
      HasSize := HttpQueryInfo(hDownload, HTTP_QUERY_CONTENT_LENGTH, Buffer, dwBufLen, dwIndex);
      if Terminated then
        Exit;

      if HasSize then
        Grabber.SetSize(StrToInt(StrPas(Buffer)))
      else
        Grabber.SetSize(0);

      dwTotalBytes := 0;
      SetGrabberStatus(gsGrabbing);
      if HasSize then
      begin
        dwBytesRead := 1;
        while (dwBytesRead > 0) and not Terminated and Continue do
        begin
          if not InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) then
            dwBytesRead := 0
          else
          begin
            Inc(dwTotalBytes, dwBytesRead);
            Grabber.SetBytesRead(dwTotalBytes);
            Grabber.Stream.Write(Buf, dwBytesRead);
            DoProgress;
          end;

          // Be CPU friendly.
          SleepEx(0, True);
        end;

        SetGrabberStatus(gsStopping);
        if Continue and not Terminated then
          Synchronize(Ended);
      end
      else
      begin
        while InternetReadFile(hDownload, @Buf, SizeOf(Buf), dwBytesRead) and not Terminated do
        begin
          if dwBytesRead = 0 then
            Break;
          Grabber.Stream.Write(Buf, dwBytesRead);
          Synchronize(UpdateGrabberProgress);

          // Be CPU friendly.
          SleepEx(0, True);
        end;

        SetGrabberStatus(gsStopping);
        if Continue and not Terminated then
          Synchronize(Ended);
      end;
    except
    end;
  finally
    // Free all stuff's
    if Buffer <> nil then
      FreeMem(Buffer);

    // Release all handles
    if (hDownload <> nil) and not InternetCloseHandle(hDownload) then
    begin
      ErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
    if (hHostConnection <> nil) and not InternetCloseHandle(hHostConnection) then
    begin
      ErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
    if (hSession <> nil) and not InternetCloseHandle(hSession) then
    begin
      ErrorText := GetLastInternetError;
      Synchronize(Error);
    end;
  end;
end;

function TJvHttpUrlGrabberThread.GetGrabber: TJvHttpUrlGrabber;
begin
  Result := TJvHttpUrlGrabber(FGrabber);
end;

//=== { TJvHttpUrlGrabberDefaultProperties } =================================

constructor TJvHttpUrlGrabberDefaultProperties.Create(AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  inherited Create(AOwner);
  Port := 80;
end;

function TJvHttpUrlGrabberDefaultProperties.GetSupportedURLName: string;
begin
  Result := TJvHttpUrlGrabber.GetSupportedURLName;
end;

//=== { TJvHttpsUrlGrabber } =============================================

class function TJvHttpsUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  Result := LowerCase(Copy(Url, 1, 7)) = cHTTPsPrefix;
end;

class function TJvHttpsUrlGrabber.GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass;
begin
  Result := TJvHttpsUrlGrabberDefaultProperties;
end;

class function TJvHttpsUrlGrabber.GetSupportedProtocolMarker: string;
begin
  Result := cHTTPsPrefix;
end;

class function TJvHttpsUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'Secure HTTP';
end;

//=== { TJvHttpsUrlGrabberDefaultProperties } =============================================

function TJvHttpsUrlGrabberDefaultProperties.GetSupportedURLName: string;
begin
  Result := TJvHttpsUrlGrabber.GetSupportedURLName;
end;

//=== { TJvLocalFileUrlGrabber } =============================================

constructor TJvLocalFileUrlGrabber.Create(AOwner: TComponent; AUrl: string;
  DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
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
  Result := (LowerCase(Copy(Url, 1, 7)) = cFilePrefix) or (Copy(Url,1,2) = '//') or
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
  Result := cFilePrefix;
end;

class function TJvLocalFileUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'LocalFile';
end;

class procedure TJvLocalFileUrlGrabber.ParseUrl(Url, Protocol: string;
  var Host, FileName, UserName, Password: string; var Port: Cardinal);
begin
  ParseUrl(Url, FileName);
end;

class procedure TJvLocalFileUrlGrabber.ParseUrl(const Url: string;
  var FileName: string);
begin
  FileName := StringReplace(Url, '/', '\', [rfReplaceAll]);
  if AnsiSameText(Copy(Url, 1, 7), cFilePrefix) then
    FileName := Copy(FileName, 8, MaxInt)
  else
    FileName := ExpandUNCFilename(FileName);
end;

//=== { TJvLocalFileUrlGrabberThread } =======================================

procedure TJvLocalFileUrlGrabberThread.Grab;
var
  FileName: string;
  BytesRead, TotalBytes: DWORD;
  Buf: array [0..1023] of Byte;
  AFileStream: TFileStream;
  Attrs: Integer;
begin
  Grabber.ParseUrl(Grabber.Url, FileName);
  if not FileExists(FileName) then
  begin
    ErrorText := Format(RsFileNotFoundFmt, [FileName]);
    Synchronize(Error);
    Exit;
  end;

  if Grabber.PreserveAttributes then
    Attrs := GetFileAttributes(PChar(FileName))
  else
    Attrs := 0;
  try
    ErrorText := '';
    SetGrabberStatus(gsConnecting);
    Grabber.Stream := TMemoryStream.Create;
    AFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Grabber.SetSize(AFileStream.Size);
      Grabber.SetBytesRead(0);
      APIStatus := 0;
      DoProgress;
      TotalBytes := 0;
      BytesRead := 1;
      SetGrabberStatus(gsGrabbing);
      while (BytesRead <> 0) and not Terminated and Continue do
      begin
        BytesRead := AFileStream.Read(Buf, SizeOf(Buf));
        Inc(TotalBytes, BytesRead);
        Grabber.SetBytesRead(TotalBytes);
        APIStatus := Grabber.BytesRead;
        if BytesRead > 0 then
          Grabber.Stream.Write(Buf, BytesRead);
        DoProgress;

        // Be CPU friendly.
        SleepEx(0, True);
      end;
      SetGrabberStatus(gsStopping);
      if not Terminated and Continue then // acp
        Synchronize(Ended);
      if Grabber.PreserveAttributes and FileExists(Grabber.FileName) then
        SetFileAttributes(PChar(Grabber.FileName), Attrs);
    finally
      AFileStream.Free;
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
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  RegisterUrlGrabberClasses;

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
