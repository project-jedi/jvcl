{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgMailSlots.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
Delivery network messages longer then 424 bytes requires installation of
NetBEUI protocol. There is no direct support of this old protocol in XP
but driver is available for manual installation (search for 'NetBEUI' on
www.microsoft.com). Delivery network messages longer then 1365 bytes can be 
problem too (if it's possible at all).
-----------------------------------------------------------------------------}
// $Id$

unit JvgMailSlots;

{$I jvcl.inc}

interface

uses
  {$IFDEF USEJVCL}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$ENDIF USEJVCL}
  Windows, Messages, SysUtils, Classes, Graphics,
  {$IFDEF USEJVCL}
  Controls, Forms, Dialogs, ExtCtrls,
  JvComponentBase;
  {$ELSE}
  Controls, Forms, Dialogs, ExtCtrls;
  {$ENDIF USEJVCL}

type
  TOnNewMessage = procedure(Sender: TObject; MessageText: string) of object;

  {$IFDEF USEJVCL}
  TJvgMailSlotServer = class(TJvComponent)
  {$ELSE}
  TJvgMailSlotServer = class(TComponent)
  {$ENDIF USEJVCL}
  private
    FMailSlotName: string;
    FLastMessage: string;
    FOnNewMessage: TOnNewMessage;
    FOnError: TNotifyEvent;
    FTimer: TTimer;
    FDeliveryCheckInterval: integer;
    FHandle: THandle;
    // FEnabled: Boolean; // use Open/Close instead
    FData: TMemoryStream;
    procedure SetMailSlotName(const SlotName: string);
    procedure SetDeliveryCheckInterval(T: integer);
    procedure OnTimer(Sender: TObject);
    function GetMessageDataPointer: Pointer;
    function GetMessageLength: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    // Message as binary data:
    property MessageData: Pointer read GetMessageDataPointer;
    property MessageLength: integer read GetMessageLength;
  // protected
    // procedure Loaded; override;
  published
    property MailSlotName: string read FMailSlotName write SetMailSlotName;
    property DeliveryCheckInterval: integer read FDeliveryCheckInterval write
      SetDeliveryCheckInterval default 1000;
    property OnNewMessage: TOnNewMessage read FOnNewMessage write
      FOnNewMessage;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

  {$IFDEF USEJVCL}
  TJvgMailSlotClient = class(TJvComponent)
  {$ELSE}
  TJvgMailSlotClient = class(TComponent)
  {$ENDIF USEJVCL}
  private
    FMailSlotName: string;
    FServerName: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Send(const Message: string): Boolean; overload;
    // For sending binary data
    function Send(const MessageData; MessageLength: integer): Boolean; overload;
  // protected
    // procedure Loaded; override;
    // procedure ErrorCatch(Sender: TObject; Exc: Exception);
  published
    property ServerName: string read FServerName write FServerName;
    property MailSlotName: string read FMailSlotName write FMailSlotName;
  end;

{$IFDEF USEJVCL}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}
{$ENDIF USEJVCL}

implementation

uses
  {$IFDEF USEJVCL}
  JvResources, JvConsts,
  {$ENDIF USEJVCL}
  JvgUtils, JvgTypes;

{$IFNDEF USEJVCL}
resourcestring
  RsETJvgMailSlotServerErrorCreatingChan = 'TJvgMailSlotServer: Error creating channel!';
  RsETJvgMailSlotServerErrorGatheringInf = 'TJvgMailSlotServer: Error gathering information!';
  RsETJvgMailSlotServerErrorReadingMessa = 'TJvgMailSlotServer: Error reading message!';
{$ENDIF !USEJVCL}

constructor TJvgMailSlotServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMailSlotName := 'MailSlot';
  FHandle := INVALID_HANDLE_VALUE;
  FData := TMemoryStream.Create;
  FDeliveryCheckInterval := 1000;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := OnTimer;
  FTimer.Interval := FDeliveryCheckInterval;
end;

destructor TJvgMailSlotServer.Destroy;
begin
  Close;
  FTimer.Free;
  FData.Free;
  inherited Destroy;
end;

{
// Opening of connection is the prerogative of the user.
procedure TJvgMailSlotServer.Loaded;
begin
  inherited Loaded;
  Open;
end;
}

procedure TJvgMailSlotServer.Open;
begin
  Close;
  // FHandle := CreateMailSlot(PChar('\\.\mailslot\' + MailSlotName), 0, MAILSLOT_WAIT_FOREVER, nil);
  // IMO Immediate return is better (no chance of hang up)
  FHandle := CreateMailSlot(PChar('\\.\mailslot\' + MailSlotName), 65535, 0 , nil);
  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.CreateRes(@RsETJvgMailSlotServerErrorCreatingChan);
  FTimer.Enabled := True;
end;

procedure TJvgMailSlotServer.Close;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
  end;  
  FTimer.Enabled := False;
end;

procedure TJvgMailSlotServer.SetMailSlotName(const SlotName: string);
begin
  if FMailSlotName<>SlotName then
  begin
    Close;
    FMailSlotName := SlotName;
  end;
end;

procedure TJvgMailSlotServer.SetDeliveryCheckInterval(T: integer);
begin
  if T<1 then T := 1;
  FTimer.Interval := T;
  FDeliveryCheckInterval := T;
end;

procedure TJvgMailSlotServer.OnTimer(Sender: TObject);
var
  MsgSize: DWORD;
  MsgNumber: DWORD;
  Read: DWORD;
  Buffer: Pointer;
begin
  // Determining if there's message
  if not GetMailSlotInfo(FHandle, nil, MsgSize, @MsgNumber, nil) then
    if Assigned(FOnError) then
      FOnError(Self) // user-defined handling
    else
      // default error notification; not recommended:
      // if error is permanent it will produce endless exceptions in timer
      raise Exception.CreateRes(@RsETJvgMailSlotServerErrorGatheringInf)
  else
    if MsgSize <> MAILSLOT_NO_MESSAGE then
    begin
      // Allocate memory for the message
      FData.Size := MsgSize;
      Buffer := FData.Memory;
      // Reading message
      if ReadFile(FHandle, Buffer^, MsgSize, Read, nil) then
      begin
        FLastMessage := PChar(Buffer);
        if Assigned(FOnNewMessage) then
          FOnNewMessage(Self, FLastMessage);
      end
      else
        if Assigned(FOnError) then
          FOnError(Self) // user-defined handling
        else
          // default error notification; not recommended:
          // if error is permanent it will produce endless exceptions in timer
          raise Exception.CreateRes(@RsETJvgMailSlotServerErrorReadingMessa);
    end;        
end;

function TJvgMailSlotServer.GetMessageDataPointer: Pointer;
begin
 Result := FData.Memory;
end;

function TJvgMailSlotServer.GetMessageLength: integer;
begin
 Result := FData.Size;
end;
//------------------------------------------------------------------------------

constructor TJvgMailSlotClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMailSlotName := 'MailSlot';
  FServerName := '';
end;

{
// Use of slot is the prerogative of the user.
procedure TJvgMailSlotClient.Loaded;
begin
  inherited Loaded;
  // (rom) this is not a good idea
  Application.OnException := ErrorCatch;
end;

procedure TJvgMailSlotClient.ErrorCatch(Sender: TObject; Exc: Exception);
var
  UserName: PChar;
  Size: DWORD;
begin
  // Querying user name
  // First query with a buffer too small, to get the required size
  Size := 0;
  UserName := nil;
  GetUserName(UserName, Size);

  // then allocate some memory for the user name
  GetMem(UserName, Size);
  try
    GetUserName(UserName, Size);
    Send('/' + UserName + '/' + FormatDateTime('hh:mm', Time) + '/' +
      Exc.Message);
  finally
    FreeMem(UserName);
  end;

  // Showing message about error to user
  Application.ShowException(Exc);
end;
}

function TJvgMailSlotClient.Send(const Message: string): Boolean;
var
  Buffer: PChar;
begin
  Buffer := PChar(Message);
  Result := Send(Pointer(Buffer)^, Length(Message)+1);
end;

function TJvgMailSlotClient.Send(const MessageData; MessageLength: integer): Boolean;
var
  FHandle: THandle;
  Written: DWORD;
begin
  if FServerName = '' then
    FServerName := '.\'; // the same computer
  FHandle := CreateFile(PChar('\\' + FServerName + '\mailslot\' + FMailSlotName),
    GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := FHandle <> INVALID_HANDLE_VALUE;
  if Result then
  begin
    Result := WriteFile(FHandle, MessageData, MessageLength, Written, nil);
    CloseHandle(FHandle);
  end;
end;

{$IFDEF USEJVCL}
{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
{$ENDIF USEJVCL}

end.
