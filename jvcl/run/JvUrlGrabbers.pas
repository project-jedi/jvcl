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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvUrlGrabbers;

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
    property Passive: Boolean read FPassive write FPassive default True;
    property Mode: TJvFtpDownloadMode read FMode write FMode default hmBinary;
  end;

  TJvFtpUrlGrabber = class(TJvCustomUrlGrabber)
  protected
    FPassiveFTP: Boolean;
    FMode: TJvFtpDownloadMode;
    function GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties); overload; override;
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; override;
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
    property OnStateChange;
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
  public
    constructor Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties); override;
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; override;
    class function GetSupportedURLName: string; override;
  published
    property Referer: string read FReferer write FReferer;

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
    property OnStateChange;
  end;

  TJvHttpUrlGrabberDefaultProperties = class(TJvCustomUrlGrabberDefaultProperties)
  private
    FReferer: string;
  protected
    function GetSupportedURLName: string; override;
  published
    property Referer: string read FReferer write FReferer;

    property Agent;
    property UserName;
    property Password;
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
    FPreserveAttributes: boolean;
  protected
    function GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties); overload; override;
    class function CanGrab(const Url: string): Boolean; override;
    class function GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass; override;
    class function GetSupportedURLName: string; override;
  published
    property PreserveAttributes:boolean read FPreserveAttributes write FPreserveAttributes default True;

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
    property OnStateChange;
  end;

  TJvLocalFileUrlGrabberThread = class(TJvCustomUrlGrabberThread)
  protected
    function GetGrabber: TJvLocalFileUrlGrabber;
    procedure ParseUrl(const Url: string; var FileName: string);
    procedure Execute; override;
  public
    property Grabber: TJvLocalFileUrlGrabber read GetGrabber;
  end;

  TJvLocalFileUrlGrabberProperties = class(TJvCustomUrlGrabberDefaultProperties)
  private
    FPreserveAttributes: boolean;
  protected
    function GetSupportedURLName: string; override;
  public
    constructor Create(AOwner: TJvUrlGrabberDefaultPropertiesList); override;
    procedure Assign(Source: TPersistent); override;
  published
    property PreserveAttributes:boolean read FPreserveAttributes write FPreserveAttributes default True;
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

procedure DownloadCallBack(Handle: HInternet; Context: DWord;
  AStatus: DWord; Info: Pointer; StatLen: DWord); stdcall;
begin
  with TJvCustomUrlGrabberThread(Context) do
  begin
    Status := AStatus;
    DoProgress;
  end;
end;

//=== TJvHttpUrlGrabber ======================================================

constructor TJvHttpUrlGrabber.Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  inherited Create(AOwner, AUrl, DefaultProperties);
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

class function TJvHttpUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'HTTP';
end;

//=== TJvFtpUrlGrabber =======================================================

constructor TJvFtpUrlGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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

class function TJvFtpUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'FTP';
end;

{ TJvFtpUrlGrabberDefaultProperties }

procedure TJvFtpUrlGrabberDefaultProperties.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TJvFtpUrlGrabberDefaultProperties then
    with Source as TJvFtpUrlGrabberDefaultProperties do
    begin
      Self.Mode := Mode;
      Self.Passive := Passive;
    end;
end;

constructor TJvFtpUrlGrabberDefaultProperties.Create(AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  inherited;
  FPassive := True;
  FMode := hmBinary;
end;

function TJvFtpUrlGrabberDefaultProperties.GetSupportedURLName: string;
begin
  Result := TJvFtpUrlGrabber.GetSupportedURLName;
end;

//=== TJvFtpUrlGrabberThread =================================================

procedure TJvFtpUrlGrabberThread.Closed;
begin
  Grabber.DoClosed;
end;

procedure TJvFtpUrlGrabberThread.Execute;
const
  cPassive: array[Boolean] of DWORD = (0, INTERNET_FLAG_PASSIVE);
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName: string;
  UserName, Password: PChar;
  LocalBytesRead, TotalBytes: DWORD;
  Buf: array[0..1023] of Byte;
  dwFileSizeHigh: DWORD;
  Buffer: Pointer;
  dwBufLen, dwIndex: DWORD;
begin
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

constructor TJvHttpUrlGrabberThread.Create(Grabber: TJvCustomUrlGrabber);
begin
  inherited Create(Grabber);
end;

procedure TJvHttpUrlGrabberThread.Execute;
var
  hSession, hHostConnection, hDownload: HINTERNET;
  HostName, FileName: string;
  Username, Password: PChar;
  Buffer: PChar;
  dwBufLen, dwIndex, dwBytesRead, dwTotalBytes: DWORD;
  HasSize: Boolean;
  Buf: array[0..1024] of Byte;

begin
  Buffer := nil;

  FContinue := True;
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
        Buffer := nil;
        Synchronize(Error);
        Exit;
      end;

      //      FCriticalSection.Enter;
      InternetSetStatusCallback(hHostConnection, PFNInternetStatusCallback(@DownloadCallBack));
      //Request the file
      // (rom) any difference here?
      hDownload := HttpOpenRequest(hHostConnection, 'GET', PChar(FileName), 'HTTP/1.0', PChar(Grabber.Referer),
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

{ TJvHttpUrlGrabberDefaultProperties }

function TJvHttpUrlGrabberDefaultProperties.GetSupportedURLName: string;
begin
  Result := TJvHttpUrlGrabber.GetSupportedURLName;
end;

{ TJvLocalFileUrlGrabber }

class function TJvLocalFileUrlGrabber.CanGrab(const Url: string): Boolean;
begin
  // accepts "file://", UNC and local path and existing files
  Result := (LowerCase(Copy(Url, 1, 7)) = 'file://') or (Copy(Url,1,2) = '//') or
    (Copy(Url, 2,2) = ':\') or FileExists(Url);
end;

constructor TJvLocalFileUrlGrabber.Create(AOwner: TComponent; AUrl: string; DefaultProperties: TJvCustomUrlGrabberDefaultProperties);
begin
  inherited Create(AOwner, AURL, DefaultProperties);
  PreserveAttributes := TJvLocalFileUrlGrabberProperties(DefaultProperties).PreserveAttributes;
end;

constructor TJvLocalFileUrlGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PreserveAttributes := True;
end;

class function TJvLocalFileUrlGrabber.GetDefaultPropertiesClass: TJvCustomUrlGrabberDefaultPropertiesClass;
begin
  Result := TJvLocalFileUrlGrabberProperties;
end;

function TJvLocalFileUrlGrabber.GetGrabberThreadClass: TJvCustomUrlGrabberThreadClass;
begin
  Result := TJvLocalFileUrlGrabberThread;
end;

class function TJvLocalFileUrlGrabber.GetSupportedURLName: string;
begin
  Result := 'LocalFile';
end;

{ TJvLocalFileUrlGrabberThread }

procedure TJvLocalFileUrlGrabberThread.Execute;
var
  FileName: string;
  BytesRead, TotalBytes: DWORD;
  Buf: array[0..1023] of Byte;
  AFileStream: TFileStream;
  Attrs:integer;
begin

  Grabber.FStream := nil;
  ParseUrl(Grabber.FUrl, FileName);
  if not FileExists(FileName) then
  begin
    FErrorText := Format(RsFileNotFoundFmt, [FileName]);
    Synchronize(Error);
    Exit;
  end;

  if Grabber.FPreserveAttributes then
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
        BytesRead := AFileStream.Read(Buf, sizeof(Buf));
        Inc(TotalBytes, BytesRead);
        Grabber.FBytesRead := TotalBytes;
        FStatus    := Grabber.FBytesRead;
        if BytesRead > 0 then
          Grabber.FStream.Write(Buf, BytesRead);
        DoProgress;
      end;
      if not Terminated and FContinue then // acp
        Synchronize(Ended);
      if Grabber.FPreserveAttributes and FileExists(Grabber.FileName) then
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

procedure TJvLocalFileUrlGrabberThread.ParseUrl(const Url: string;
  var FileName: string);
begin
  FileName := StringReplace(Url, '/', '\', [rfReplaceAll]);
  if AnsiSameText(Copy(Url, 1, 7), 'file://') then
    FileName := Copy(FileName, 8, MaxInt)
  else
    FileName := ExpandUNCFilename(FileName);
end;

{ TJvLocalFileUrlGrabberProperties }

procedure TJvLocalFileUrlGrabberProperties.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TJvLocalFileUrlGrabberProperties then
    with Source as TJvLocalFileUrlGrabberProperties do
    begin
      Self.PreserveAttributes := PreserveAttributes;
    end;
end;

constructor TJvLocalFileUrlGrabberProperties.Create(AOwner: TJvUrlGrabberDefaultPropertiesList);
begin
  inherited Create(AOwner);
  FPreserveAttributes := True;
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

