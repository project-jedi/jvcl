{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRas32.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvRas32;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Controls, Forms,
  Ras32,
  Windows, Messages,
   // Messages must be after QControls
  JvComponentBase, JvTypes;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvRas32 = class(TJvComponent)
  private
    FPhoneBookPath: TFileName;
    FPassword: string;
    FDeviceName: string;
    FUsername: string;
    FEntry: string;
    FDeviceType: string;
    FPhoneNumber: string;
    FCallBackNumber: string;
    FDomain: string;
    FConnection: DWORD;
    FHandle: THandle;
    FPHandle: THandle;
    RASEvent: Word;
    FEntryIndex: Integer;
    FDummyConnected: Boolean;
    FPhoneBook: TStringList;
    FOnAuthProject: TNotifyEvent;
    FOnAuthChangePassword: TNotifyEvent;
    FOnAuthLinkSpeed: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnAuthNotify: TNotifyEvent;
    FOnDeviceConnected: TNotifyEvent;
    FOnReAuthenticate: TNotifyEvent;
    FOnAuthAck: TNotifyEvent;
    FOnConnectDevice: TNotifyEvent;
    FOnAuthRetry: TNotifyEvent;
    FOnAuthenticate: TNotifyEvent;
    FOnWaitForModemReset: TNotifyEvent;
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
    FKeepConnected: Boolean;
    FAvailable: Boolean;
    //    function GetPhoneBook: TStringList;
    procedure WndProc(var Msg: TMessage);
    procedure SetEntryIndex(const Value: Integer);
    function GetConnected: Boolean;
    function GetPhoneBook: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshPhoneBook;
    function Dial(Index: Integer): Boolean;
    function HangUp: Boolean;
    function CreateNewConnection: Boolean;
    function EditConnection(Index: Integer): Boolean;
    function GetActiveConnection: string;
    property CallBackNumber: string read FCallBackNumber write FCallBackNumber;
    property DeviceType: string read FDeviceType;
    property DeviceName: string read FDeviceName;
    property PhoneNumber: string read FPhoneNumber write FPhoneNumber;
    property Domain: string read FDomain write FDomain;
    property PhoneBook: TStrings read GetPhoneBook;
    property RasAvailable: Boolean read FAvailable;
  published
    property KeepConnected: Boolean read FKeepConnected write FKeepConnected default False;
    //    property PhoneBook: TStringList read GetPhoneBook;
    property EntryIndex: Integer read FEntryIndex write SetEntryIndex default -1;
    property PhoneBookPath: TFileName read FPhoneBookPath write FPhoneBookPath;
    property Entry: string read FEntry write FEntry;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Connected: Boolean read GetConnected write FDummyConnected stored False;
    property OnOpenPort: TNotifyEvent read FOnOpenPort write FOnOpenPort;
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
    property OnWaitForModemReset: TNotifyEvent read FOnWaitForModemReset write FOnWaitForModemReset;
    property OnInteractive: TNotifyEvent read FOnInteractive write FOnInteractive;
    property OnRetryAuthentication: TNotifyEvent read FOnRetryAuthentication write FOnRetryAuthentication;
    property OnPasswordExpired: TNotifyEvent read FOnPasswordExpired write FOnPasswordExpired;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnWaitForCallBack: TNotifyEvent read FOnWaitForCallBack write FOnWaitForCallBack;
  end;

  // (rom) renamed
  EJvRasError = class(EJVCLException);

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
  JvJVCLUtils, JvResources;

constructor TJvRas32.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeepConnected := False;
  FPhoneBookPath := '';
  FPassword := '';
  FDeviceName := '';
  FUsername := '';
  FEntry := '';
  FDeviceType := '';
  FPhoneNumber := '';
  FCallBackNumber := '';
  FDomain := '';
  FConnection := 0;
  if AOwner is TWinControl then
    FPHandle := (AOwner as TWinControl).Handle
  else
    // (rom) is this safe?
    FPHandle := GetForegroundWindow;
  FEntryIndex := -1;

  FDll := SafeLoadLibrary(RsRasDllName);
  if FDll <> 0 then
  begin
    FRasDial := GetProcAddress(FDll, {$IFDEF UNICODE}'RasDialW'{$ELSE}'RasDialA'{$ENDIF UNICODE});
    FRasEnumConnections := GetProcAddress(FDll, {$IFDEF UNICODE}'RasEnumConnectionsW'{$ELSE}'RasEnumConnectionsA'{$ENDIF UNICODE});
    FRasEnumEntries := GetProcAddress(FDll, {$IFDEF UNICODE}'RasEnumEntriesW'{$ELSE}'RasEnumEntriesA'{$ENDIF UNICODE});
    FRasGetConnectStatus := GetProcAddress(FDll, {$IFDEF UNICODE}'RasGetConnectStatusW'{$ELSE}'RasGetConnectStatusA'{$ENDIF UNICODE});
    FRasGetErrorstring := GetProcAddress(FDll, {$IFDEF UNICODE}'RasGetErrorstringW'{$ELSE}'RasGetErrorstringA'{$ENDIF UNICODE});
    FRasHangUp := GetProcAddress(FDll, {$IFDEF UNICODE}'RasHangUpW'{$ELSE}'RasHangUpA'{$ENDIF UNICODE});
    FRasGetEntryDialParams := GetProcAddress(FDll, {$IFDEF UNICODE}'RasGetEntryDialParamsW'{$ELSE}'RasGetEntryDialParamsA'{$ENDIF UNICODE});
    FRasValidateEntryName := GetProcAddress(FDll, {$IFDEF UNICODE}'RasValidateEntryNameW'{$ELSE}'RasValidateEntryNameA'{$ENDIF UNICODE});
    FRasCreatePhonebookEntry := GetProcAddress(FDll, {$IFDEF UNICODE}'RasCreatePhonebookEntryW'{$ELSE}'RasCreatePhonebookEntryA'{$ENDIF UNICODE});
    FRasEditPhonebookEntry := GetProcAddress(FDll, {$IFDEF UNICODE}'RasEditPhonebookEntryW'{$ELSE}'RasEditPhonebookEntryA'{$ENDIF UNICODE});
    FHandle := AllocateHWndEx(WndProc);
    RASEvent := RegisterWindowMessage(RASDialEvent);
    if RASEvent = 0 then
      RASEvent := WM_RASDialEvent;
  end;
  FAvailable := (FDll <> 0) and Assigned(FRasDial);
end;

destructor TJvRas32.Destroy;
begin
  FPhoneBook.Free;
  if RasAvailable then
  begin
    try
      if not KeepConnected then
        HangUp;
    except
    end;
    FreeLibrary(FDll);
    DeallocateHWndEx(FHandle);
  end;
  FDll := 0;
  inherited Destroy;
end;

function TJvRas32.GetActiveConnection: string;
var
  Ret: Longint;
  nCB: DWORD;
  RasConn: array of TRASCONN;
  nRasConnCount: DWORD;
  I: Integer;
begin
  Result := '';

  if RasAvailable then
  begin
    // We enumerate the RAS connections in a loop which allows us to use
    // a dynamic array rather than a static one that may not be big
    // enough to contain all the connections (Mantis 5079).
    // We start with 64 which should be fine on most systems
    repeat
      SetLength(RasConn, Length(RasConn) + 64);
      RasConn[0].dwSize := SizeOf(RasConn[0]);
      nCB := Length(RasConn) * SizeOf(RasConn[0]);
      
      Ret := FRasEnumConnections(@RasConn[0], nCB, nRasConnCount);
    until Ret <> ERROR_BUFFER_TOO_SMALL;

    if Ret <> ERROR_SUCCESS then
      raise Exception.CreateFmt('Unable to enumerate RAS connections, Error code is %d', [Ret]);
      
    if nRasConnCount = 0 then
      Exit;

    if not Assigned(FPhoneBook) then
      RefreshPhoneBook;
    for I := 0 to FPhoneBook.Count - 1 do
     if FPhoneBook[I] = RasConn[0].szEntryName then
     begin
       FConnection := RasConn[0].rasConn;
       Result := FPhoneBook[I];
       Break;
     end;
  end;
end;

function TJvRas32.CreateNewConnection: Boolean;
begin
  if RasAvailable then
    Result := FRasCreatePhonebookEntry(FPHandle, nil) = 0
  else
    Result := False;
end;

function TJvRas32.Dial(Index: Integer): Boolean;
var
  RASDialParams: TRASDialParams;
  R: DWORD;
  X: Integer;
begin
  if not RasAvailable or (FConnection <> 0) then
    Result := False
  else
  begin
    FillChar(RASDialParams, SizeOf(RASDialParams), #0);
    FConnection := 0;
    with RASDialParams do
    begin
      dwSize := SizeOf(TRASDialParams);
      StrLCopy(szEntryName, PChar(PhoneBook[Index]), RAS_MAXENTRYNAME);
      X := Self.EntryIndex;
      Self.EntryIndex := Index;
      StrLCopy(szUserName, PChar(FUsername), RAS_MAXENTRYNAME);
      StrLCopy(szPassword, PChar(FPassword), RAS_MAXENTRYNAME);
      Self.EntryIndex := X;
      szDomain := AnsiString('*');
      szCallbackNumber := AnsiString('*');
      szPhoneNumber := '';
    end;
    if Assigned(FRasDial) then
    begin
      if FPhoneBookPath <> '' then
        R := FRasDial(nil, PChar(FPhoneBookPath), @RASDialParams, $FFFFFFFF, FHandle, FConnection)
      else
        R := FRasDial(nil, nil, @RASDialParams, $FFFFFFFF, FHandle, FConnection);
      Result := R = 0;
    end
    else
      Result := False;
  end;
end;

function TJvRas32.EditConnection(Index: Integer): Boolean;
begin
  Result := False;
  if RasAvailable then
  begin
    RefreshPhoneBook;
    if Index < PhoneBook.Count then
      Result := FRasEditPhonebookEntry(FPHandle, nil, PChar(PhoneBook[Index])) = 0;
  end;
end;

function TJvRas32.GetConnected: Boolean;
var
  Status: TRASConnStatus;
begin
  if RasAvailable and (FConnection <> 0) then
  begin
    Status.dwSize := SizeOf(TRASConnStatus);
    FRasGetConnectStatus(FConnection, @Status);
    Result := Status.rasConnstate = RASCS_Connected;
  end
  else
    Result := False;
end;

procedure TJvRas32.RefreshPhoneBook;
var
  RASEntryName: array of TRasEntryName;
  Ret, I, BufSize, Entries: DWORD;
begin
  { Build internal copy. }
  if FPhoneBook = nil then
    FPhoneBook := TStringList.Create;
  if RasAvailable then
  begin
    FPhoneBook.BeginUpdate;
    try
      FPhoneBook.Clear;

      if Assigned(FRasEnumEntries) then
      begin
        // We enumerate the RAS entries in a loop which allows us to use
        // a dynamic array rather than a static one that may not be big
        // enough to contain all the entries (Mantis 5079).
        // We start with 50 which should be fine on most systems
        repeat
          SetLength(RASEntryName, Length(RASEntryName) + 50);
          BufSize := Length(RASEntryName) * SizeOf(RASEntryName[0]);
          RASEntryName[0].dwSize := SizeOf(RASEntryName[0]);
          if FPhoneBookPath <> '' then
            Ret := FRasEnumEntries(nil, PChar(FPhoneBookPath), @RASEntryName[0], BufSize, Entries)
          else
            Ret := FRasEnumEntries(nil, nil, @RASEntryName[0], BufSize, Entries);
        until Ret <> ERROR_BUFFER_TOO_SMALL;

        if Ret <> ERROR_SUCCESS then
          raise Exception.CreateFmt('Unable to enumerate RAS entries, Error code is %d', [Ret]);

        I := 0;
        while I < Entries do
        begin
          if (RASEntryName[I].szEntryName[0] <> #0) then
            FPhoneBook.Add(StrPas(RASEntryName[I].szEntryName));
          Inc(I);
        end;
      end;
    finally
      FPhoneBook.EndUpdate;
    end;
  end;
end;

function TJvRas32.HangUp: Boolean;
var
  Rc: Longint;
  I: Integer;
  RasConnStatus: TRASConnStatus;
begin
  Result := False;
  if RasAvailable and (FConnection <> 0) then
  begin
    Rc := FRasHangUp(FConnection);
    if Rc <> 0 then
    begin
      RasConnStatus.dwSize := SizeOf(TRASConnStatus);
      I := 0;
      while True do
      begin
        Rc := FRasGetConnectStatus(FConnection, @RasConnStatus);
        if Rc = ERROR_INVALID_HANDLE then
        begin
          Rc := 0;
          Break;
        end;
        Sleep(10);
        Inc(I);
        if I > 9 then
          Break; // don't want an infinite loop...
      end;
    end;
    Result := Rc = 0;
    FConnection := 0;
  end;
end;

procedure TJvRas32.SetEntryIndex(const Value: Integer);
var
  RasDial: TRASDialParams;
  Res: LongBool;
begin
  if RasAvailable then
  begin
    FEntryIndex := Value;

    FEntry := '';
    FUsername := '';
    FPhoneNumber := '';
    FDomain := '';
    FCallBackNumber := '';
    FPassword := '';

    if FEntryIndex >= PhoneBook.Count then
    begin
      if PhoneBook.Count > 0 then
        FEntryIndex := 0
      else
        FEntryIndex := -1;
    end;

    if FEntryIndex <> -1 then
    begin
      FEntry := PhoneBook[FEntryIndex];

      FillChar(RasDial, SizeOf(TRASDialParams), #0);
      StrLCopy(RasDial.szEntryName, PChar(PhoneBook[FEntryIndex]), RAS_MAXENTRYNAME);
      RasDial.dwSize := SizeOf(TRASDialParams);

      if Assigned(FRasGetEntryDialParams) then
        if FRasGetEntryDialParams(nil, RasDial, Res) = 0 then
          with RasDial do
          begin
            FUsername := StrPas(szUserName);
            FPassword := StrPas(szPassword);
            FDomain := StrPas(szDomain);
            FCallBackNumber := StrPas(szCallbackNumber);
            FPhoneNumber := StrPas(szPhoneNumber);
          end;
    end;
  end;
end;

procedure TJvRas32.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = RASEvent) and (FConnection <> 0) then
  begin
    case Msg.WParam of
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
      RASCS_WaitForModemReset:
        if Assigned(FOnWaitForModemReset) then
          FOnWaitForModemReset(Self);
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
        if Assigned(FOnDisconnected) then
          FOnDisconnected(Self);
      RASCS_WaitForCallBack:
        if Assigned(FOnWaitForCallBack) then
          FOnWaitForCallBack(Self);
    end;
  end
  else
    Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

function TJvRas32.GetPhoneBook: TStrings;
begin
  if FPhoneBook = nil then
    FPhoneBook := TStringList.Create;
  if FPhoneBook.Count = 0 then
    RefreshPhoneBook;
  Result := FPhoneBook;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
