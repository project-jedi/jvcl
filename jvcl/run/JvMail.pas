{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMail.PAS, released Jun 10, 2000.

The Initial Developer of the Original Code is Petr Vones (petr dott v att mujmail dott cz)
Portions created by Petr Vones are Copyright (C) 2000 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

Current Version: 0.50

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
  Doesn't work with Paegasus Mail because it has no MAPI support at all.
-----------------------------------------------------------------------------}
// $Id$

unit JvMail;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, Controls, Forms,
  Mapi,
  JclMapi,
  JvComponentBase;

type
  TJvMail = class;

  // (rom) renamed
  TJvMailRecipient = class(TCollectionItem)
  private
    FAddress: string;
    FName: string;
    function GetAddressAndName: string;
    function GetValid: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    // (rom) renamed
    property AddressAndName: string read GetAddressAndName;
  published
    property Address: string read FAddress write FAddress;
    property Name: string read FName write FName;
    property Valid: Boolean read GetValid;
  end;

  // (rom) renamed
  TJvMailRecipients = class(TCollection)
  private
    FOwner: TJvMail;
    FRecipientClass: DWORD;
    function GetItem(Index: Integer): TJvMailRecipient;
    procedure SetItem(Index: Integer; const Value: TJvMailRecipient);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TJvMail; ARecipientClass: DWORD);
    function Add: TJvMailRecipient;
    function AddRecipient(const Address: string; const Name: string = ''): Integer;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvMailRecipient read GetItem write SetItem; default;
    property RecipientClass: DWORD read FRecipientClass;
  end;

  TJvMailLogonOption = (loLogonUI, loNewSession, loDownloadMail);
  TJvMailLogonOptions = set of TJvMailLogonOption;
  TJvMailReadOption = (roUnreadOnly, roFifo, roPeek, roHeaderOnly, roAttachments);
  TJvMailReadOptions = set of TJvMailReadOption;

  TJvMailReadedData = record
    RecipientAddress: string;
    RecipientName: string;
    ConversationID: string;
    DateReceived: TDateTime;
  end;

  TJvMailErrorEvent = procedure(Sender: TJvMail; ErrorCode: ULONG) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvMail = class(TJvComponent)
  private
    FAttachment: TStrings;
    FAttachArray: array of TMapiFileDesc;
    FBlindCopy: TJvMailRecipients;
    FBody: TStrings;
    FCarbonCopy: TJvMailRecipients;
    FRecipient: TJvMailRecipients;
    FSimpleMapi: TJclSimpleMapi;
    FSubject: string;
    FSessionHandle: THandle;
    FMapiMessage: TMapiMessage;
    FRecipArray: array of TMapiRecipDesc;
    FLongMsgId: Boolean;
    FLogonOptions: TJvMailLogonOptions;
    FPassword: string;
    FProfileName: string;
    FSaveTaskWindows: array of Boolean;
    FSaveTaskActiveForm: TForm;
    FSeedMessageID: string;
    FReadOptions: TJvMailReadOptions;
    FReadedMail: TJvMailReadedData;
    FOnError: TJvMailErrorEvent;
    procedure BeforeClientLibUnload(Sender: TObject);
    procedure SetBlindCopy(const Value: TJvMailRecipients);
    procedure SetCarbonCopy(const Value: TJvMailRecipients);
    procedure SetRecipient(const Value: TJvMailRecipients);
    procedure SetBody(const Value: TStrings);
    function GetUserLogged: Boolean;
    procedure SetAttachment(const Value: TStrings);
    function GetSimpleMapi: TJclSimpleMapi;
  protected
    procedure CheckLoadLib;
    procedure CheckUserLogged;
    procedure CreateMapiMessage;
    procedure CreateRecips;
    procedure DecodeAttachments(Attachments: PMapiFileDesc; AttachCount: Integer);
    procedure DecodeRecipients(Recips: PMapiRecipDesc; RecipCount: Integer);
    procedure FreeMapiMessage;
    procedure FreeRecipArray;
    function LogonFlags: DWORD;
    procedure RestoreTaskWindowsState;
    procedure SaveTaskWindowsState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Address(const Caption: string = ''; EditFields: Integer = 3): Boolean;
    procedure Clear;
    function ErrorCheck(Res: DWORD): DWORD;
    function FindFirstMail: Boolean;
    function FindNextMail: Boolean;
    procedure FreeSimpleMapi;
    procedure LogOff;
    procedure LogOn;
    procedure ReadMail;
    function ResolveName(const Name: string): string;
    function SaveMail(const MessageID: string): string;
    procedure SendMail(ShowDialog: Boolean = True);
    property ReadedMail: TJvMailReadedData read FReadedMail;
    property SeedMessageID: string read FSeedMessageID write FSeedMessageID;
    property SessionHandle: THandle read FSessionHandle;
    property SimpleMAPI: TJclSimpleMapi read GetSimpleMapi;
    property UserLogged: Boolean read GetUserLogged;
  published
    property Attachment: TStrings read FAttachment write SetAttachment;
    property BlindCopy: TJvMailRecipients read FBlindCopy write SetBlindCopy;
    property Body: TStrings read FBody write SetBody;
    property CarbonCopy: TJvMailRecipients read FCarbonCopy write SetCarbonCopy;
    property LogonOptions: TJvMailLogonOptions read FLogonOptions write FLogonOptions
      default [loLogonUI, loNewSession];
    property LongMsgId: Boolean read FLongMsgId write FLongMsgId default True;
    property Password: string read FPassword write FPassword;
    property ProfileName: string read FProfileName write FProfileName;
    property ReadOptions: TJvMailReadOptions read FReadOptions write FReadOptions
      default [roFifo, roPeek];
    property Recipient: TJvMailRecipients read FRecipient write SetRecipient;
    property Subject: string read FSubject write FSubject;
    property OnError: TJvMailErrorEvent read FOnError write FOnError;
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
  JclSysUtils, JclAnsiStrings,
  JvResources;

//=== { TJvMailRecipient } ===================================================

function TJvMailRecipient.GetAddressAndName: string;
var
  N: string;
begin
  if Name = '' then
    N := Address
  else
    N := Name;
  Result := Format('"%s" <%s>', [N, Address]);
end;

function TJvMailRecipient.GetDisplayName: string;
begin
  if Valid then
    Result := AddressAndName
  else
    Result := inherited GetDisplayName;
end;

function TJvMailRecipient.GetValid: Boolean;
begin
  Result := FAddress <> '';
end;

//=== { TJvMailRecipients } ==================================================

function TJvMailRecipients.Add: TJvMailRecipient;
begin
  Result := TJvMailRecipient(inherited Add);
end;

function TJvMailRecipients.AddRecipient(const Address, Name: string): Integer;
var
  Item: TJvMailRecipient;
begin
  Item := Add;
  Result := Item.Index;
  try
    Item.Address := Address;
    Item.Name := Name;
  except
    Item.Free;
    raise;
  end;
end;

procedure TJvMailRecipients.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      for I := 0 to TStrings(Source).Count - 1 do
        AddRecipient(TStrings(Source)[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvMailRecipients.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).BeginUpdate;
    try
      for I := 0 to Count - 1 do
        TStrings(Dest).Add(Items[I].Address);
    finally
      TStrings(Dest).EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TJvMailRecipients.Create(AOwner: TJvMail; ARecipientClass: DWORD);
begin
  inherited Create(TJvMailRecipient);
  FOwner := AOwner;
  FRecipientClass := ARecipientClass;
end;

function TJvMailRecipients.GetItem(Index: Integer): TJvMailRecipient;
begin
  Result := TJvMailRecipient(inherited GetItem(Index));
end;

function TJvMailRecipients.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJvMailRecipients.SetItem(Index: Integer; const Value: TJvMailRecipient);
begin
  inherited SetItem(Index, Value);
end;

//=== { TJvMail } ============================================================

constructor TJvMail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAttachment := TStringList.Create;
  FBody := TStringList.Create;
  FBlindCopy := TJvMailRecipients.Create(Self, MAPI_BCC);
  FCarbonCopy := TJvMailRecipients.Create(Self, MAPI_CC);
  FRecipient := TJvMailRecipients.Create(Self, MAPI_TO);
  FLongMsgId := True;
  FLogonOptions := [loLogonUI, loNewSession];
  FReadOptions := [roFifo, roPeek];
end;

destructor TJvMail.Destroy;
begin
  FreeSimpleMapi;
  FreeAndNil(FAttachment);
  FreeAndNil(FBody);
  FreeAndNil(FBlindCopy);
  FreeAndNil(FCarbonCopy);
  FreeAndNil(FRecipient);
  inherited Destroy;
end;

function TJvMail.Address(const Caption: string; EditFields: Integer): Boolean;
var
  NewRecipCount: ULONG;
  NewRecips: PMapiRecipDesc;
begin
  CheckLoadLib;
  CreateRecips;
  try
    SaveTaskWindowsState;
    try
      Result := (ErrorCheck(FSimpleMapi.MapiAddress(FSessionHandle, Application.Handle,
        PAnsiChar(AnsiString(Caption)), EditFields, nil, Length(FRecipArray), FRecipArray[0],
        LogonFlags, 0, @NewRecipCount, NewRecips)) = SUCCESS_SUCCESS);
    finally
      RestoreTaskWindowsState;
    end;
    if Result then
      DecodeRecipients(NewRecips, NewRecipCount);
    FSimpleMapi.MapiFreeBuffer(NewRecips);
  finally
    FreeRecipArray;
  end;
end;

procedure TJvMail.BeforeClientLibUnload(Sender: TObject);
begin
  if UserLogged then
    LogOff;
end;

procedure TJvMail.CheckLoadLib;
begin
  GetSimpleMapi;
  FSimpleMapi.LoadClientLib;
  if not FSimpleMapi.ClientLibLoaded then
    raise EJclMapiError.CreateRes(@RsNoClientInstalled);
end;

procedure TJvMail.CheckUserLogged;
begin
  if not UserLogged then
    raise EJclMapiError.CreateRes(@RsNoUserLogged);
end;

procedure TJvMail.Clear;
begin
  Body.Clear;
  BlindCopy.Clear;
  CarbonCopy.Clear;
  Recipient.Clear;
  Attachment.Clear;
  Subject := '';
  with FReadedMail do
  begin
    RecipientAddress := '';
    RecipientName := '';
    ConversationID := '';
    DateReceived := 0;
  end;
  FreeMapiMessage;
end;

procedure TJvMail.CreateMapiMessage;

  procedure MakeAttachments;
  var
    I: Integer;
    NullPos: Integer;
    FileName, PathName: string;
  begin
    if Attachment.Count > 0 then
    begin
      SetLength(FAttachArray, Attachment.Count);
      for I := 0 to Attachment.Count - 1 do
      begin
        // Look for #0 splitting parameter in physical filename (PathName) and virtual name (FileName)
        NullPos := Pos(#0, Attachment[I]);
        if NullPos > 0 then
        begin
          PathName := Copy(Attachment[i], 0, NullPos - 1);
          FileName := Copy(Attachment[i], NullPos + 1, Length(Attachment[i]));
        end
        else
        begin
          // Use real filename in attachment if no #0 is found
          PathName := Attachment[i];
          FileName := ExtractFileName(PathName);
        end;

        if not FileExists(PathName) then
          raise EJclMapiError.CreateResFmt(@RsAttachmentNotFound, [PathName]);

        FillChar(FAttachArray[I], SizeOf(TMapiFileDesc), #0);
        FAttachArray[I].nPosition := $FFFFFFFF;
        FAttachArray[I].lpszFileName := StrNewA(PAnsiChar(AnsiString(FileName)));
        FAttachArray[I].lpszPathName := StrNewA(PAnsiChar(AnsiString(PathName)));
      end;
    end
    else
      FAttachArray := nil;
  end;

begin
  try
    FillChar(FMapiMessage, SizeOf(FMapiMessage), #0);
    CreateRecips;
    MakeAttachments;

    FMapiMessage.lpszSubject := StrNewA(PAnsiChar(AnsiString(FSubject)));
    FMapiMessage.lpszNoteText := StrNewA(PAnsiChar(AnsiString(FBody.Text)));
    FMapiMessage.lpRecips := PMapiRecipDesc(FRecipArray);
    FMapiMessage.nRecipCount := Length(FRecipArray);
    FMapiMessage.lpFiles := PMapiFileDesc(FAttachArray);
    FMapiMessage.nFileCount := Length(FAttachArray);
  except
    FreeMapiMessage;
    raise;
  end;
end;

procedure TJvMail.CreateRecips;
var
  RecipIndex: Integer;

  procedure MakeRecips(RecipList: TJvMailRecipients);
  var
    I: Integer;
  begin
    for I := 0 to RecipList.Count - 1 do
    begin
      if not RecipList[I].Valid then
        raise EJclMapiError.CreateResFmt(@RsRecipNotValid, [RecipList[I].GetNamePath]);

      FillChar(FRecipArray[RecipIndex], SizeOf(TMapiRecipDesc), #0);
      FRecipArray[RecipIndex].ulRecipClass := RecipList.RecipientClass;
      FRecipArray[RecipIndex].lpszAddress := StrNewA(PAnsiChar(AnsiString(RecipList[I].Address)));
      if RecipList[I].Name = '' then // some clients requires Name item always filled
        FRecipArray[RecipIndex].lpszName := FRecipArray[RecipIndex].lpszAddress
      else
        FRecipArray[RecipIndex].lpszName := StrNewA(PAnsiChar(AnsiString(RecipList[I].Name)));

      Inc(RecipIndex);
    end;
  end;

begin
  SetLength(FRecipArray, FBlindCopy.Count + FCarbonCopy.Count + FRecipient.Count);
  RecipIndex := 0;
  MakeRecips(FBlindCopy);
  MakeRecips(FCarbonCopy);
  MakeRecips(FRecipient);
end;

procedure TJvMail.DecodeAttachments(Attachments: PMapiFileDesc; AttachCount: Integer);
var
  I: Integer;
begin
  Attachment.Clear;
  if Attachments = nil then
    Exit;
  for I := 0 to AttachCount - 1 do
  begin
    Attachment.Add(string(Attachments^.lpszPathName));
    Inc(Attachments);
  end;
end;

procedure TJvMail.DecodeRecipients(Recips: PMapiRecipDesc; RecipCount: Integer);
var
  I: Integer;
begin
  FBlindCopy.Clear;
  FCarbonCopy.Clear;
  FRecipient.Clear;
  if Recips = nil then
    Exit;
  for I := 0 to RecipCount - 1 do
  begin
    with Recips^ do
      case ulRecipClass of
        MAPI_BCC:
          BlindCopy.AddRecipient(string(lpszAddress), string(lpszName));
        MAPI_CC:
          CarbonCopy.AddRecipient(string(lpszAddress), string(lpszName));
        MAPI_TO:
          Recipient.AddRecipient(string(lpszAddress), string(lpszName));
      end;
    Inc(Recips);
  end;
end;

function TJvMail.ErrorCheck(Res: DWORD): DWORD;
begin
  if Assigned(FOnError) then
  begin
    Result := Res;
    if Res <> SUCCESS_SUCCESS then
      FOnError(Self, Res);
  end
  else
    Result := MapiCheck(Res);
end;

function TJvMail.FindFirstMail: Boolean;
begin
  FSeedMessageID := '';
  Result := FindNextMail;
end;

function TJvMail.FindNextMail: Boolean;
var
  MsgID: array [0..512] of AnsiChar;
  Flags, Res: ULONG;
begin
  CheckUserLogged;
  Flags := 0;
  if FLongMsgId then
    Inc(Flags, MAPI_LONG_MSGID);
  if roFifo in FReadOptions then
    Inc(Flags, MAPI_GUARANTEE_FIFO);
  if roUnreadOnly in FReadOptions then
    Inc(Flags, MAPI_UNREAD_ONLY);
  Res := FSimpleMapi.MapiFindNext(SessionHandle, Application.Handle, nil,
    PAnsiChar(AnsiString(FSeedMessageID)), Flags, 0, @MsgID[0]);
  Result := (Res = SUCCESS_SUCCESS);
  if Result then
    FSeedMessageID := string(MsgID)
  else
  begin
    FSeedMessageID := '';
    if Res <> MAPI_E_NO_MESSAGES then
      ErrorCheck(Res);
  end;
end;

procedure TJvMail.FreeMapiMessage;
var
  I: Integer;
begin
  for I := 0 to High(FAttachArray) do
  begin
    if FAttachArray[I].lpszPathName <> FAttachArray[I].lpszFileName then
      StrDisposeA(FAttachArray[I].lpszPathName);
    StrDisposeA(FAttachArray[I].lpszFileName);
  end;
  FAttachArray := nil;
  FreeRecipArray;
  StrDisposeA(FMapiMessage.lpszSubject);
  StrDisposeA(FMapiMessage.lpszNoteText);
  FillChar(FMapiMessage, SizeOf(FMapiMessage), #0);
end;

procedure TJvMail.FreeRecipArray;
var
  I: Integer;
begin
  for I := 0 to High(FRecipArray) do
  begin
    if FRecipArray[I].lpszName <> FRecipArray[I].lpszAddress then
      StrDisposeA(FRecipArray[I].lpszName);
    StrDisposeA(FRecipArray[I].lpszAddress);
  end;
  FRecipArray := nil;
end;

procedure TJvMail.FreeSimpleMapi;
begin
  try
    if FSimpleMapi <> nil then
    begin
      FSimpleMapi.BeforeUnloadClient := nil; // prevent memory leak
      LogOff;
    end;
  finally
    FreeAndNil(FSimpleMapi);
  end;
end;

function TJvMail.GetSimpleMapi: TJclSimpleMapi;
begin
  if not Assigned(FSimpleMapi) then
  begin
    FSimpleMapi := TJclSimpleMapi.Create;
    FSimpleMapi.BeforeUnloadClient := BeforeClientLibUnload;
  end;
  Result := FSimpleMapi;
end;

function TJvMail.GetUserLogged: Boolean;
begin
  Result := FSessionHandle <> 0;
end;

procedure TJvMail.LogOff;
begin
  CheckLoadLib;
  if UserLogged then
  begin
    ErrorCheck(FSimpleMapi.MapiLogOff(FSessionHandle, Application.Handle, 0, 0));
    FSessionHandle := 0;
  end;
end;

procedure TJvMail.LogOn;
begin
  CheckLoadLib;
  if UserLogged then
    Exit;
  SaveTaskWindowsState;
  try
    ErrorCheck(FSimpleMapi.MapiLogOn(Application.Handle, PAnsiChar(AnsiString(FProfileName)),
      PAnsiChar(AnsiString(FPassword)), LogonFlags, 0, @FSessionHandle));
  finally
    RestoreTaskWindowsState;
  end;
end;

function TJvMail.LogonFlags: DWORD;
begin
  Result := 0;
  if not UserLogged then
  begin
    if loLogonUI in FLogonOptions then
      Inc(Result, MAPI_LOGON_UI);
    if loNewSession in FLogonOptions then
      Inc(Result, MAPI_NEW_SESSION);
    if loDownloadMail in FLogonOptions then
      Inc(Result, MAPI_FORCE_DOWNLOAD);
  end;
end;

procedure TJvMail.ReadMail;
var
  Flags: ULONG;
  Msg: PMapiMessage;
  SOldDateFormat: string;
  OldDateSeparator: Char;
begin
  CheckUserLogged;
  Clear;
  Flags := 0;
  if roHeaderOnly in FReadOptions then
    Inc(Flags, MAPI_ENVELOPE_ONLY);
  if roPeek in FReadOptions then
    Inc(Flags, MAPI_PEEK);
  if not (roAttachments in FReadOptions) then
    Inc(Flags, MAPI_SUPPRESS_ATTACH);
  ErrorCheck(FSimpleMapi.MapiReadMail(SessionHandle, Application.Handle,
    PAnsiChar(AnsiString(FSeedMessageID)), Flags, 0, Msg));
  with Msg^ do
  begin
    if lpOriginator <> nil then
    begin
      FReadedMail.RecipientAddress := string(lpOriginator^.lpszAddress);
      FReadedMail.RecipientName := string(lpOriginator^.lpszName);
    end;
    DecodeRecipients(lpRecips, nRecipCount);
    FSubject := string(lpszSubject);
    Body.Text := string(lpszNoteText);
    //    FDateReceived := StrToDateTime(lpszDateReceived);
    SOldDateFormat := JclFormatSettings.ShortDateFormat;
    OldDateSeparator := JclFormatSettings.DateSeparator;
    try
      JclFormatSettings.ShortDateFormat := 'yyyy/M/d';
      JclFormatSettings.DateSeparator := '/';
      FReadedMail.DateReceived := StrToDateTime(string(lpszDateReceived));
    finally
      JclFormatSettings.ShortDateFormat := SOldDateFormat;
      JclFormatSettings.DateSeparator := OldDateSeparator;
    end;
    FReadedMail.ConversationID := string(lpszConversationID);
    DecodeAttachments(lpFiles, nFileCount);
  end;
  FSimpleMapi.MapiFreeBuffer(Msg);
end;

function TJvMail.ResolveName(const Name: string): string;
var
  RecipDesc: PMapiRecipDesc;
  Res: DWORD;
begin
  Result := '';
  CheckLoadLib;
  SaveTaskWindowsState;
  Res := FSimpleMapi.MapiResolveName(SessionHandle, Application.Handle,
    PAnsiChar(AnsiString(Name)), LogonFlags or MAPI_AB_NOMODIFY or MAPI_DIALOG, 0, RecipDesc);
  RestoreTaskWindowsState;
  if (Res <> MAPI_E_AMBIGUOUS_RECIPIENT) and (Res <> MAPI_E_UNKNOWN_RECIPIENT) then
  begin
    Result := string(RecipDesc^.lpszName);
    FSimpleMapi.MapiFreeBuffer(RecipDesc);
    ErrorCheck(Res);
  end;
end;

procedure TJvMail.RestoreTaskWindowsState;
var
  I: Integer;
begin
  if (FSaveTaskWindows <> nil) and (Length(FSaveTaskWindows) >= Screen.FormCount) then
    for I := 0 to Screen.FormCount - 1 do
      EnableWindow(Screen.Forms[I].Handle, FSaveTaskWindows[I]);
  FSaveTaskWindows := nil;
  if FSaveTaskActiveForm <> nil then
    SetFocus(FSaveTaskActiveForm.Handle);
end;

function TJvMail.SaveMail(const MessageID: string): string;
var
  MsgID: array [0..512] of AnsiChar;
  Flags: ULONG;
begin
  Result := '';
  CheckLoadLib;
  CreateMapiMessage;
  try
    StrPCopyA(MsgID, AnsiString(MessageID));
    SaveTaskWindowsState;
    Flags := LogonFlags;
    if FLongMsgId then
      Flags := Flags or MAPI_LONG_MSGID;
    try
      ErrorCheck(FSimpleMapi.MapiSaveMail(FSessionHandle, Application.Handle,
        FMapiMessage, Flags, 0, MsgID));
    finally
      RestoreTaskWindowsState;
    end;
    Result := string(MsgID);
  finally
    FreeMapiMessage;
  end;
end;

procedure TJvMail.SaveTaskWindowsState;
var
  I: Integer;
  W: HWND;
begin
  SetLength(FSaveTaskWindows, Screen.FormCount);
  FSaveTaskActiveForm := Screen.ActiveForm;
  for I := 0 to Screen.FormCount - 1 do
  begin
    W := Screen.Forms[I].Handle;
    FSaveTaskWindows[I] := IsWindowEnabled(W);
    EnableWindow(W, False);
  end;
end;

procedure TJvMail.SendMail(ShowDialog: Boolean);
var
  Flags: ULONG;
begin
  CheckLoadLib;
  CreateMapiMessage;
  try
    Flags := LogonFlags;
    if ShowDialog then
      Flags := Flags or MAPI_DIALOG;
    SaveTaskWindowsState;
    try
      ErrorCheck(FSimpleMapi.MapiSendMail(FSessionHandle, Application.Handle,
        FMapiMessage, Flags, 0));
    finally
      RestoreTaskWindowsState;
    end;
  finally
    FreeMapiMessage;
  end;
end;

procedure TJvMail.SetAttachment(const Value: TStrings);
begin
  FAttachment.Assign(Value);
end;

procedure TJvMail.SetBlindCopy(const Value: TJvMailRecipients);
begin
  FBlindCopy.Assign(Value);
end;

procedure TJvMail.SetBody(const Value: TStrings);
begin
  FBody.Assign(Value);
end;

procedure TJvMail.SetCarbonCopy(const Value: TJvMailRecipients);
begin
  FCarbonCopy.Assign(Value);
end;

procedure TJvMail.SetRecipient(const Value: TJvMailRecipients);
begin
  FRecipient.Assign(Value);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.