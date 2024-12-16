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
located at http://jvcl.delphi-jedi.org

Known Issues:
Delivery network messages longer then 424 bytes requires installation of
NetBEUI protocol. There is no direct support of this old protocol in XP
but driver is available for manual installation (search for 'NetBEUI' on
www.microsoft.com). Delivery network messages longer then 1365 bytes can be
problem too (if it's possible at all).
-----------------------------------------------------------------------------}
// $Id$

unit JvMailSlots;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls,
  JvComponentBase;

type
  TOnNewMessage = procedure(Sender: TObject; MessageText: string) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvMailSlotServer = class(TJvComponent)
  private
    FMailSlotName: string;
    FLastMessage: string;
    FOnNewMessage: TOnNewMessage;
    FOnError: TNotifyEvent;
    FTimer: TTimer;
    FDeliveryCheckInterval: Integer;
    FHandle: THandle;
    FData: TMemoryStream;
    procedure SetMailSlotName(const SlotName: string);
    procedure SetDeliveryCheckInterval(Value: Integer);
    procedure OnTimer(Sender: TObject);
    function GetMessageDataPointer: Pointer;
    function GetMessageLength: LongWord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    // Message as binary data:
    property MessageData: Pointer read GetMessageDataPointer;
    property MessageLength: LongWord read GetMessageLength;
  published
    property MailSlotName: string read FMailSlotName write SetMailSlotName;
    property DeliveryCheckInterval: Integer read FDeliveryCheckInterval write SetDeliveryCheckInterval default 1000;
    property OnNewMessage: TOnNewMessage read FOnNewMessage write FOnNewMessage;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvMailSlotClient = class(TJvComponent)
  private
    FMailSlotName: string;
    FServerName: string;
  public
    constructor Create(AOwner: TComponent); override;
    function Send(const Msg: string): Boolean; overload;
    // For sending binary data
    function Send(const MessageData; MessageLength: LongWord): Boolean; overload;
  published
    property ServerName: string read FServerName write FServerName;
    property MailSlotName: string read FMailSlotName write FMailSlotName;
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
  JvResources;

constructor TJvMailSlotServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := OnTimer;

  FMailSlotName := 'MailSlot';
  FHandle := INVALID_HANDLE_VALUE;
  FData := TMemoryStream.Create;

  FDeliveryCheckInterval := 1000;
  FTimer.Interval := FDeliveryCheckInterval;
end;

destructor TJvMailSlotServer.Destroy;
begin
  Close;
  FTimer.Free;
  FData.Free;
  inherited Destroy;
end;

procedure TJvMailSlotServer.Open;
begin
  Close;
  // FHandle := CreateMailSlot(PChar('\\.\mailslot\' + MailSlotName), 0, MAILSLOT_WAIT_FOREVER, nil);
  // IMO Immediate return is better (no chance of hang up)
  FHandle := CreateMailSlot(PChar('\\.\mailslot\' + MailSlotName), High(Word), 0 , nil);
  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.CreateRes(@RsJvMailSlotServerErrorCreatingChan);
  FTimer.Enabled := True;
end;

procedure TJvMailSlotServer.Close;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
  end;
  FTimer.Enabled := False;
end;

procedure TJvMailSlotServer.SetMailSlotName(const SlotName: string);
begin
  if FMailSlotName <> SlotName then
  begin
    Close;
    FMailSlotName := SlotName;
  end;
end;

procedure TJvMailSlotServer.SetDeliveryCheckInterval(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  FTimer.Interval := Value;
  FDeliveryCheckInterval := Value;
end;

procedure TJvMailSlotServer.OnTimer(Sender: TObject);
var
  MsgSize: DWORD;
  MsgNumber: DWORD;
  Read: DWORD;
  Buffer: Pointer;
begin
  // Determining if there's message
  if not GetMailSlotInfo(FHandle, nil, MsgSize, @MsgNumber, nil) then
  begin
    if Assigned(FOnError) then
      FOnError(Self) // user-defined handling
    else
      // default error notification; not recommended:
      // if error is permanent it will produce endless exceptions in timer
      raise Exception.CreateRes(@RsJvMailSlotServerErrorGatheringInf);
  end
  else
  begin
    if MsgSize <> MAILSLOT_NO_MESSAGE then
    begin
      // Allocate memory for the message
      FData.Size := MsgSize;
      Buffer := FData.Memory;
      // Reading message
      if ReadFile(FHandle, Buffer^, MsgSize, Read, nil) then
      begin
        SetString(FLastMessage, PChar(Buffer), (MsgSize - 1) div SizeOf(Char)); // exclude trailing #0
        if Assigned(FOnNewMessage) then
          FOnNewMessage(Self, FLastMessage);
      end
      else
      begin
        if Assigned(FOnError) then
          FOnError(Self) // user-defined handling
        else
          // default error notification; not recommended:
          // if error is permanent it will produce endless exceptions in timer
          raise Exception.CreateRes(@RsJvMailSlotServerErrorReadingMessa);
      end;
    end;
  end;
end;

function TJvMailSlotServer.GetMessageDataPointer: Pointer;
begin
  Result := FData.Memory;
end;

function TJvMailSlotServer.GetMessageLength: LongWord;
begin
  Result := FData.Size;
end;

//------------------------------------------------------------------------------

constructor TJvMailSlotClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMailSlotName := 'MailSlot';
  FServerName := '';
end;

function TJvMailSlotClient.Send(const Msg: string): Boolean;
var
  Buffer: PChar;
begin
  Buffer := PChar(Msg);
  Result := Send(Pointer(Buffer)^, (Length(Msg) + 1) * SizeOf(Char)); // include trailing #0
end;

function TJvMailSlotClient.Send(const MessageData; MessageLength: LongWord): Boolean;
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.