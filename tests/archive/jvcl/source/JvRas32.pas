{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRas32.PAS, released on 2001-02-28.

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

unit JvRas32;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Registry, JvHRas32,
  JvComponent, JvTypes;

type
  TJvRas32 = class(TJvComponent)
  private
    FPath: TFileName;
    FPassword: string;
    FDeviceName: string;
    FUsername: string;
    FEntry: string;
    FDevice: string;
    FPhone: string;
    FCallBack: string;
    FDomain: string;
    FConnection: DWord;
    FHandle: THandle;
    FPHandle: THandle;
    RASEvent: Word;
    FIndex: Integer;
    FConnected: Boolean;
    FOnAuthProject: TNotifyEvent;
    FOnAuthChangePassword: TNotifyEvent;
    FOnAuthLinkSpeed: TNotifyEvent;
    FOnDisConnected: TNotifyEvent;
    FOnAuthNotify: TNotifyEvent;
    FOnDeviceConnected: TNotifyEvent;
    FOnReAuthenticate: TNotifyEvent;
    FOnAuthAck: TNotifyEvent;
    FOnConnectDevice: TNotifyEvent;
    FOnAuthRetry: TNotifyEvent;
    FOnAuthenticate: TNotifyEvent;
    FOnWaiTFormodemReset: TNotifyEvent;
    FOnOpenPort: TNotifyEvent;
    FOnAuthCallback: TNotifyEvent;
    FOnRetryAuthentication: TNotifyEvent;
    FOnPortOpened: TNotifyEvent;
    FOnWaitForCallBack: TNotifyEvent;
    FOnPrepareForCallback: TNotifyEvent;
    FOnPasswordExpired: TNotifyEvent;
    FOnInteractive: TNotifyEvent;
    FOnConnected: TNotifyEvent;
    FOnAuthenticated: TNotifyEvent;
    FOnAllDevicesConnected: TNotifyEvent;
    FDll: THandle;
    FRasDial: TRasDial;
    FRasEnumConnections: TRasEnumConnections;
    FRasEnumEntries: TRasEnumEntries;
    FRasGetConnectStatus: TRasGetConnectStatus;
    FRasGetErrorstring: TRasGetErrorstring;
    FRasHangUp: TRasHangUp;
    FRasGetEntryDialParams: TRasGetEntryDialParams;
    FRasValidateEntryName: TRasValidateEntryName;
    FRasCreatePhonebookEntry: TRasCreatePhonebookEntry;
    FRasEditPhonebookEntry: TRasEditPhonebookEntry;
    function GetPhoneBook: TStringList;
    procedure WndProc(var Msg: TMessage);
    procedure SetIndex(const Value: Integer);
    function GetConnected: Boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DeviceType: string read FDevice;
    property DeviceName: string read FDeviceName;
    property PhoneNumber: string read FPhone write FPhone;
    property Domain: string read FDomain write FDomain;
    property CallBackNumber: string read FCallBack write FCallBack;
  published
    property PhoneBook: TStringList read GetPhoneBook;
    property EntryIndex: Integer read FIndex write SetIndex default -1;
    property PhoneBookPath: TFileName read FPath write FPath;
    property Entry: string read FEntry write FEntry;
    property Username: string read FUsername write FUSername;
    property Password: string read FPassword write FPassword;
    property Connected: Boolean read GetConnected write FConnected;
    property OnOpenPort: TNotifyEvent read FOnOpenPort write FOnOpenport;
    property OnPortOpened: TNotifyEvent read FOnPortOpened write FOnPortOpened;
    property OnConnectDevice: TNotifyEvent read FOnConnectDevice write FOnConnectDevice;
    property OnDeviceConnected: TNotifyEvent read FOnDeviceConnected write FOnDeviceConnected;
    property OnAllDevicesConnected: TNotifyEvent read FOnAllDevicesConnected write FOnAllDevicesConnected;
    property OnAuthenticate: TNotifyEvent read FOnAuthenticate write FOnAuthenticate;
    property OnAuthNotify: TNotifyEvent read FOnAuthNotify write FOnAuthNotify;
    property OnAuthRetry: TNotifyEvent read FOnAuthRetry write FOnAuthRetry;
    property OnAuthCallback: TNotifyEvent read FOnAuthCallback write FOnAuthCallback;
    property OnAuthChangePassword: TNotifyEvent read FOnAuthChangePassword write FOnAuthChangePassword;
    property OnAuthProject: TNotifyEvent read FOnAuthProject write FOnAuthProject;
    property OnAuthLinkSpeed: TNotifyEvent read FOnAuthLinkSpeed write FOnAuthLinkSpeed;
    property OnAuthAck: TNotifyEvent read FOnAuthAck write FOnAuthAck;
    property OnReAuthenticate: TNotifyEvent read FOnReAuthenticate write FOnReAuthenticate;
    property OnAuthenticated: TNotifyEvent read FOnAuthenticated write FOnAuthenticated;
    property OnPrepareForCallback: TNotifyEvent read FOnPrepareForCallback write FOnPrepareForCallback;
    property OnWaiTFormodemReset: TNotifyEvent read FOnWaiTFormodemReset write FOnWaiTFormodemReset;
    property OnInteractive: TNotifyEvent read FOnInteractive write FOnInteractive;
    property OnRetryAuthentication: TNotifyEvent read FOnRetryAuthentication write FOnRetryAuthentication;
    property OnPasswordExpired: TNotifyEvent read FOnPasswordExpired write FOnPasswordExpired;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisConnected: TNotifyEvent read FOnDisConnected write FOnDisConnected;
    property OnWaitForCallBack: TNotifyEvent read FOnWaitForCallBack write FOnWaitForCallBack;
    function Dial(index: Integer): Boolean;
    function HangUp: Boolean;
    function CreateNewConnection: Boolean;
    function EditConnection(Index: Integer): Boolean;
  end;

  ERasError = class(EJVCLException);

implementation

resourcestring
  RC_RasError = 'RAS: Unable to find RasApi32.dll';
  RC_RasDllName = 'RASAPI32.DLL';

  {**************************************************}

constructor TJvRas32.Create(AOwner: TComponent);
begin
  inherited;
  FPath := '';
  FPassword := '';
  FDeviceName := '';
  FUsername := '';
  FEntry := '';
  FDevice := '';
  FPhone := '';
  FCallBack := '';
  FDomain := '';
  FConnection := 0;
  FHandle := {$IFDEF COMPILER6_UP}Classes.{$ENDIF}AllocateHWnd(WndProc);
  RASEvent := RegisterWindowMessage(RASDialEvent);
  if RASEvent = 0 then
    RASEvent := WM_RASDialEvent;
  if AOwner is TWinControl then
    FPHandle := (AOwner as TWinControl).Handle
  else
    FPHandle := Application.Handle;
  Findex := -1;

  FDll := LoadLibrary(PChar(RC_RasDllName));
  if FDll <> 0 then
  begin
    FRasDial := GetProcAddress(FDll, 'RasDialA');
    FRasEnumConnections := GetProcAddress(FDll, 'RasEnumConnectionsA');
    FRasEnumEntries := GetProcAddress(FDll, 'RasEnumEntriesA');
    FRasGetConnectStatus := GetProcAddress(FDll, 'RasGetConnectStatusA');
    FRasGetErrorstring := GetProcAddress(FDll, 'RasGetErrorstringA');
    FRasHangUp := GetProcAddress(FDll, 'RasHangUpA');
    FRasGetEntryDialParams := GetProcAddress(FDll, 'RasGetEntryDialParamsA');
    FRasValidateEntryName := GetProcAddress(FDll, 'RasValidateEntryNameA');
    FRasCreatePhonebookEntry := GetProcAddress(FDll, 'RasCreatePhonebookEntryA');
    FRasEditPhonebookEntry := GetProcAddress(FDll, 'RasEditPhonebookEntryA');
  end
  else
    raise ERasError.Create(RC_RasError);
end;

{**************************************************}

function TJvRas32.CreateNewConnection: Boolean;
begin
  if @FRasCreatePhonebookEntry <> nil then
    Result := FRasCreatePhonebookEntry(FPHandle, nil) = 0
  else
    Result := False;
end;

{**************************************************}

destructor TJvRas32.Destroy;
begin
  if FDll <> 0 then
  begin
    try
      HangUp;
    except
    end;
    FreeLibrary(FDll);
  end;
  {$IFDEF COMPILER6_UP}Classes.{$ENDIF}DeallocateHWnd(FHandle);
  inherited
end;

{**************************************************}

function TJvRas32.Dial(index: Integer): Boolean;
var
  RASDialParams: TRASDialParams;
  R: DWORD;
  x: Integer;
begin
  if FConnection <> 0 then
    Result := False
  else
  begin
    FillChar(RASDialParams, SizeOf(RASDialParams), #0);
    FConnection := 0;
    with RASDialParams do
    begin
      dwSize := SizeOf(TRasDialParams);
      StrLCopy(szEntryName, PChar(GetPhoneBook[Index]), RAS_MAXENTRYNAME);
      x := Self.EntryIndex;
      Self.EntryIndex := index;
      StrLCopy(szUserName, PChar(FUsername), RAS_MAXENTRYNAME);
      StrLCopy(szPassword, PChar(FPassword), RAS_MAXENTRYNAME);
      Self.EntryIndex := x;
      szDomain := '*';
      szCallbackNumber := '*';
      szPhoneNumber := '';
    end;
    if @FRasDial <> nil then
    begin
      if FPath <> '' then
        r := FRasDial(nil, PChar(FPath), @RASDialParams, $FFFFFFFF, FHandle, FConnection)
      else
        r := FRasDial(nil, nil, @RASDialParams, $FFFFFFFF, FHandle, FConnection);
      Result := r = 0;
    end
    else
      Result := False;
  end;
end;

{**************************************************}

function TJvRas32.EditConnection(Index: Integer): Boolean;
var
  st: TStringList;
begin
  st := GetPhonebook;

  Result := False;
  if @FRasEditPhonebookEntry <> nil then
    if Index < st.Count then
      Result := FRasEditPhonebookEntry(FPHandle, nil, PChar(st[Index])) = 0;
  st.Free;
end;

{**************************************************}

function TJvRas32.GetConnected: Boolean;
var
  status: TRASConnStatus;
begin
  if (FConnection <> 0) and (@FRasGetConnectStatus <> nil) then
  begin
    Status.dwSize := SizeOf(TRASConnStatus);
    FRasGetConnectStatus(FConnection, @Status);
    Result := Status.rasConnstate = RASCS_Connected;
  end
  else
    Result := False;
end;

{**************************************************}

function TJvRas32.GetPhoneBook: TStringList;
var
  RASEntryName: array[1..50] of TRasEntryName;
  I, BufSize, Entries: DWORD;
begin
  Result := TStringList.Create;
  RasEntryName[1].dwSize := SizeOf(RasEntryName[1]);
  BufSize := SizeOf(RasEntryName);

  if @FRasEnumEntries <> nil then
  begin
    if (FPath <> '') then
      i := FRasEnumEntries(nil, PChar(FPath), @RASEntryName, BufSize, Entries)
    else
      i := FRasEnumEntries(nil, nil, @RASEntryName, BufSize, Entries);
    if (i = 0) or (i = ERROR_BUFFER_TOO_SMALL) then
      for i := 1 to Entries do
        if (i < 51) and (RasEntryName[i].szEntryName[0] <> #0) then
          Result.Add(StrPas(RasEntryName[i].szEntryName));
  end;
end;

{**************************************************}

function TJvRas32.HangUp: Boolean;
begin
  Result := False;
  if (FConnection <> 0) and (@FRasHangUp <> nil) then
  begin
    Result := FRasHangUp(FConnection) = 0;
    FConnection := 0;
  end;
end;

{**************************************************}

procedure TJvRas32.SetIndex(const Value: Integer);
var
  RasDial: TRASDialParams;
  res: LongBool;
begin
  FIndex := Value;

  FEntry := '';
  FUsername := '';
  FPhone := '';
  FDomain := '';
  FCallBack := '';
  FPassword := '';

  if FIndex >= GetPhoneBook.Count then
  begin
    if GetPhoneBook.Count > 0 then
      FIndex := 0
    else
      FIndex := -1;
  end;

  if FIndex <> -1 then
  begin
    FEntry := GetPhonebook[FIndex];

    FillChar(RasDial, SizeOf(TRasDialParams), #0);
    StrLCopy(RasDial.szEntryName, PChar(GetPhoneBook[Findex]), RAS_MAXENTRYNAME);
    RasDial.dwSize := SizeOf(TRasDialParams);

    if @FRasGetEntryDialParams <> nil then
    begin
      if FRasGetEntryDialParams(nil, RasDial, Res) = 0 then
      begin
        with RasDial do
        begin
          FUsername := StrPas(szUsername);
          FPassword := StrPas(szPassword);
          FDomain := StrPas(szDomain);
          FCallBack := StrPas(szCallbackNumber);
          FPhone := StrPas(szPhoneNumber);
        end;
      end;
    end;
  end;
end;

{**************************************************}

procedure TJvRas32.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = RASEvent) and (FConnection <> 0) then
  begin
    case Msg.wParam of
      RASCS_OpenPort:
        if Assigned(FOnOpenPort) then
          FOnOpenPort(Self);
      RASCS_PortOpened:
        if Assigned(FOnPortOpened) then
          FOnPortOpened(Self);
      RASCS_ConnectDevice:
        if Assigned(FOnConnectDevice) then
          FOnConnectDevice(Self);
      RASCS_DeviceConnected:
        if Assigned(FOnDeviceConnected) then
          FOnDeviceConnected(Self);
      RASCS_AllDevicesConnected:
        if Assigned(FOnAllDevicesConnected) then
          FOnAllDevicesConnected(Self);
      RASCS_Authenticate:
        if Assigned(FOnAuthenticate) then
          FOnAuthenticate(Self);
      RASCS_AuthNotify:
        if Assigned(FOnAuthNotify) then
          FOnAuthNotify(Self);
      RASCS_AuthRetry:
        if Assigned(FOnAuthRetry) then
          FOnAuthRetry(Self);
      RASCS_AuthCallback:
        if Assigned(FOnAuthCallback) then
          FOnAuthCallback(Self);
      RASCS_AuthChangePassword:
        if Assigned(FOnAuthChangePassword) then
          FOnAuthChangePassword(Self);
      RASCS_AuthProject:
        if Assigned(FOnAuthProject) then
          FOnAuthProject(Self);
      RASCS_AuthLinkSpeed:
        if Assigned(FOnAuthLinkSpeed) then
          FOnAuthLinkSpeed(Self);
      RASCS_AuthAck:
        if Assigned(FOnAuthAck) then
          FOnAuthAck(Self);
      RASCS_ReAuthenticate:
        if Assigned(FOnReAuthenticate) then
          FOnReAuthenticate(Self);
      RASCS_Authenticated:
        if Assigned(FOnAuthenticated) then
          FOnAuthenticated(Self);
      RASCS_PrepareForCallback:
        if Assigned(FOnPrepareForCallback) then
          FOnPrepareForCallback(Self);
      RASCS_WaiTFormodemReset:
        if Assigned(FOnWaiTFormodemReset) then
          FOnWaiTFormodemReset(Self);
      RASCS_Interactive:
        if Assigned(FOnInteractive) then
          FOnInteractive(Self);
      RASCS_RetryAuthentication:
        if Assigned(FOnRetryAuthentication) then
          FOnRetryAuthentication(Self);
      RASCS_PasswordExpired:
        if Assigned(FOnPasswordExpired) then
          FOnPasswordExpired(Self);
      RASCS_Connected:
        if Assigned(FOnConnected) then
          FOnConnected(Self);
      RASCS_DisConnected:
        if Assigned(FOnDisConnected) then
          FOnDisConnected(Self);
      RASCS_WaitForCallBack:
        if Assigned(FOnWaitForCallBack) then
          FOnWaitForCallBack(Self);
    end;
  end
  else
    DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

end.
