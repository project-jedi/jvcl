{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not Use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEventLog.PAS, released on 2002-09-02.

The Initial Developer of the Original Code is Fernando Silva [fernando dott silva att myrealbox dott com]
Portions created by Fernando Silva are Copyright (C) 2002 Fernando Silva.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvQNTEventLog;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  JvQComponent;

type
  TNotifyChangeEventLog = class;
  TJvNTEventLogRecord = class;

  TJvNTEventLog = class(TJvComponent)
  private
    FLogHandle: THandle;
    FLog: string;
    FServer: string;
    FSource: string;
    FActive: Boolean;
    FLastError: Cardinal;
    FOnChange: TNotifyEvent;
    FNotifyThread: TNotifyChangeEventLog;
    FEventRecord: TJvNTEventLogRecord;
    procedure SetActive(Value: Boolean);
    procedure SetServer(const Value: string);
    procedure SetSource(const Value: string);
    procedure SetLog(const Value: string);
    function GetEventCount: Cardinal;
    procedure SeekRecord(N: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure First;
    procedure Last;
    function Eof: Boolean;
    procedure Next;
    procedure Seek(N: Cardinal);
    procedure ReadEventLogs(AStrings: TStrings);
    property EventCount: Cardinal read GetEventCount;
    property EventRecord: TJvNTEventLogRecord read FEventRecord;
  published
    property Server: string read FServer write SetServer;
    property Source: string read FSource write SetSource;
    property Log: string read FLog write SetLog;
    property Active: Boolean read FActive write SetActive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TNotifyChangeEventLog = class(TThread)
  private
    FEventLog: TJvNTEventLog;
    FEventHandle: THandle;
    procedure DoChange;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TComponent);
  end;

  TJvNTEventLogRecord = class(TObject)
  private
    FEventLog: TJvNTEventLog;
    FCurrentRecord: Pointer;
    FOwner: TComponent;
    function GetRecordNumber: Cardinal;
    function GetDateTime: TDateTime;
    function GetID: DWORD;
    function GetType: string;
    function GetStringCount: DWORD;
    function GetCategory: Cardinal;
    function GetSource: string;
    function GetComputer: string;
    function GetSID: PSID;
    function GetString(Index: Cardinal): string;
    function GetMessageText: string;
    function GetUsername: string;
  public
    constructor Create(AOwner: TComponent);
    property RecordNumber: Cardinal read GetRecordNumber;
    property DateTime: TDateTime read GetDateTime;
    property EventType: string read GetType;
    property Category: Cardinal read GetCategory;
    property Source: string read GetSource;
    property Computer: string read GetComputer;
    property ID: DWORD read GetID;
    property StringCount: DWORD read GetStringCount;
    property SID: PSID read GetSID;
    property EventString[Index: Cardinal]: string read GetString;
    property MessageText: string read GetMessageText;
    property UserName: string read GetUsername;
    property Owner: TComponent read FOwner;
  end;

implementation

uses
  Registry, 
  JvQResources;

const
  EVENTLOG_SEQUENTIAL_READ = $0001;
  EVENTLOG_SEEK_READ = $0002;
  EVENTLOG_FORWARDS_READ = $0004;
  EVENTLOG_BACKWARDS_READ = $0008;

  cEventLogBaseKey = 'SYSTEM\CurrentControlSet\Services\EventLog';

type
  PEventLogRecord = ^TEventLogRecord;
  TEventLogRecord = packed record
    Length: DWORD; // Length of full record
    Reserved: DWORD; // Used by the service
    RecordNumber: DWORD; // Absolute record number
    TimeGenerated: DWORD; // Seconds since 1-1-1970
    TimeWritten: DWORD; // Seconds since 1-1-1970
    EventID: DWORD;
    EventType: WORD;
    NumStrings: WORD;
    EventCategory: WORD;
    ReservedFlags: WORD; // For Use with paired events (auditing)
    ClosingRecordNumber: DWORD; // For Use with paired events (auditing)
    StringOffset: DWORD; // Offset from beginning of record
    UserSidLength: DWORD;
    UserSidOffset: DWORD;
    DataLength: DWORD;
    DataOffset: DWORD; // Offset from beginning of record
  end;

//=== TJvNTEventLog ==========================================================

constructor TJvNTEventLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLog := '';
  FSource := '';
  FOnChange := nil;
  FNotifyThread := nil;
  FEventRecord := TJvNTEventLogRecord.Create(Self);
end;

destructor TJvNTEventLog.Destroy;
begin
  Close;
  FEventRecord.Free;
  inherited Destroy;
end;

procedure TJvNTEventLog.SetActive(Value: Boolean);
begin
  if Value <> FActive then
    if csDesigning in ComponentState then
      FActive := Value
    else
    if Value then
      Open
    else
      Close;
end;

procedure TJvNTEventLog.SetServer(const Value: string);
var
  OldActive: Boolean;
begin
  if FServer <> Value then
  begin
    OldActive := Active;
    Active := False;
    FServer := Value;
    Active := OldActive;
  end
end;

procedure TJvNTEventLog.SetSource(const Value: string);
var
  OldActive: Boolean;
begin
  if FSource <> Value then
  begin
    OldActive := Active;
    Active := False;
    FSource := Value;
    Active := OldActive;
  end
end;

procedure TJvNTEventLog.SetLog(const Value: string);
var
  OldActive: Boolean;
begin
  if FLog <> Value then
  begin
    OldActive := Active;
    Active := False;
    FLog := Value;
    Active := OldActive;
  end
end;

function TJvNTEventLog.GetEventCount: Cardinal;
begin
  if Active then
    GetNumberOfEventLogRecords(FLogHandle, Result)
  else
    Result := 0;
end;

procedure TJvNTEventLog.Open;
begin
  if Source <> '' then
  begin
    FLogHandle := OpenEventLog(PChar(Server), PChar(Source));
    if FLogHandle = 0 then
      RaiseLastOSError;
    FNotifyThread := TNotifyChangeEventLog.Create(Self);
    FActive := True;
  end;
end;

procedure TJvNTEventLog.Close;
begin
  if FLogHandle <> 0 then
  begin
    FNotifyThread.Terminate;
    CloseEventLog(FLogHandle);
    FLogHandle := 0
  end;
  ReallocMem(FEventRecord.FCurrentRecord, 0);
  FActive := False;
end;

procedure TJvNTEventLog.First;
begin
  SeekRecord(0);
end;

procedure TJvNTEventLog.Last;
begin
  SeekRecord(GetEventCount - 1);
end;

function TJvNTEventLog.Eof: Boolean;
begin
  Result := (EventRecord.FCurrentRecord = nil) or (EventRecord.RecordNumber = GetEventCount) or
    (FLastError = ERROR_HANDLE_EOF);
end;

procedure TJvNTEventLog.Next;
var
  BytesRead, BytesNeeded, Flags: DWORD;
  Dummy: Char;
begin
  Flags := EVENTLOG_SEQUENTIAL_READ;
  Flags := Flags or EVENTLOG_FORWARDS_READ;

  ReadEventLog(FLogHandle, Flags, 0, @Dummy, 0, BytesRead, BytesNeeded);
  FLastError := GetLastError;
  if FLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    ReallocMem(FEventRecord.FCurrentRecord, BytesNeeded);
    if not ReadEventLog(FLogHandle, Flags, 0, FEventRecord.FCurrentRecord, BytesNeeded, BytesRead, BytesNeeded) then
      RaiseLastOSError;
  end
  else
  if FLastError <> ERROR_HANDLE_EOF then
    RaiseLastOSError;
end;

procedure TJvNTEventLog.SeekRecord(N: Cardinal);
var
  Offset, Flags: DWORD;
  BytesRead, BytesNeeded: Cardinal;
  Dummy: Char;
  RecNo: Integer;
begin
  GetOldestEventLogRecord(FLogHandle, Offset);
  RecNo := N + Offset;

  Flags := EVENTLOG_SEEK_READ;
  Flags := Flags or EVENTLOG_FORWARDS_READ;

  ReadEventLog(FLogHandle, Flags, RecNo, @Dummy, 0, BytesRead, BytesNeeded);
  FLastError := GetLastError;
  if FLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    ReallocMem(FEventRecord.FCurrentRecord, BytesNeeded);
    if not ReadEventLog(FLogHandle, Flags, RecNo, FEventRecord.FCurrentRecord, BytesNeeded, BytesRead, BytesNeeded) then
      RaiseLastOSError;
  end
  else
  if FLastError <> ERROR_HANDLE_EOF then
    RaiseLastOSError;
end;

procedure TJvNTEventLog.Seek(N: Cardinal);
begin
  if N <> FEventRecord.RecordNumber then
    SeekRecord(N);
end;

procedure TJvNTEventLog.ReadEventLogs(AStrings: TStrings);
begin
  with TRegistry.Create do
  begin
    AStrings.BeginUpdate;
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(cEventLogBaseKey, False);
      GetKeyNames(AStrings);
    finally
      Free;
      AStrings.EndUpdate;
    end;
  end;
end;

//=== TNotifyChangeEventLog ==================================================

constructor TNotifyChangeEventLog.Create(AOwner: TComponent);
begin
  inherited Create(True); // Create thread suspended
  FreeOnTerminate := True; // Thread Free Itself when terminated

  // initialize system events
  FEventLog := TJvNTEventLog(AOwner);
  FEventHandle := CreateEvent(nil, True, False, nil);
  NotifyChangeEventLog(FEventLog.FLogHandle, FEventHandle);

  Suspended := False; // Continue the thread
end;

procedure TNotifyChangeEventLog.DoChange;
begin
  if Assigned(FEventLog.FOnChange) then
    FEventLog.FOnChange(FEventLog);
end;

procedure TNotifyChangeEventLog.Execute;
var
  LResult: DWORD;
begin
  // (rom) secure thread against exceptions
  try
    while not Terminated do
    begin
      // reset event signal, so we can get it again
      ResetEvent(FEventHandle);
      // wait for event to happen
      LResult := WaitForSingleObject(FEventHandle, INFINITE);
      // check event Result
      case LResult of
        WAIT_OBJECT_0:
          Synchronize(DoChange);
      else
        Synchronize(DoChange);
      end;
    end;
  except
  end;
end;

//=== TJvNTEventLogRecord ====================================================

constructor TJvNTEventLogRecord.Create(AOwner: TComponent);
begin
  // (rom) added inherited Create
  inherited Create;
  FEventLog := TJvNTEventLog(AOwner);
  FCurrentRecord := nil;
  FOwner := AOwner;
end;

function TJvNTEventLogRecord.GetRecordNumber: Cardinal;
begin
  Result := PEventLogRecord(FCurrentRecord)^.RecordNumber;
end;

function TJvNTEventLogRecord.GetMessageText: string;
var
  MessagePath: string;
  Count, I: Integer;
  P: PChar;
  Args, PArgs: ^PChar;
  St: string;

  function FormatMessageFrom(const DllName: string): Boolean;
  var
    DllModule: THandle;
    Buffer: array [0..2047] of Char;
    FullDLLName: array [0..MAX_PATH] of Char;
  begin
    Result := False;
    ExpandEnvironmentStrings(PChar(DllName), FullDLLName, MAX_PATH);
    DllModule := LoadLibraryEx(FullDLLName, 0, LOAD_LIBRARY_AS_DATAFILE);
    if DllModule <> 0 then
    try
      // (rom) memory leak fixed
      if FormatMessage(
        FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ARGUMENT_ARRAY,
        Pointer(DllModule), ID, 0, Buffer, SizeOf(Buffer), Args) > 0 then
      begin
        Buffer[StrLen(Buffer) - 2] := #0;
        St := Buffer;
        Result := True;
      end
    finally
      FreeLibrary(DllModule);
    end
  end;

begin
  St := '';
  Count := StringCount;
  GetMem(Args, Count * SizeOf(PChar));
  try
    PArgs := Args;
    P := PEventLogRecord(FCurrentRecord)^.StringOffset + PChar(FCurrentRecord);
    for I := 0 to Count - 1 do
    begin
      PArgs^ := P;
      Inc(P, lstrlen(P) + 1);
      Inc(PArgs);
    end;
    with TRegistry.Create do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(Format('%s\%s\%s', [cEventLogBaseKey, FEventLog.Log, Source]), False); {rw}
//      OpenKey(Format('SYSTEM\CurrentControlSet\Services\EventLog\%s\%s', [FEventLog.Log, FEventLog.Source]), False);
      MessagePath := ReadString('EventMessageFile');
      repeat
        I := Pos(';', MessagePath);
        if I <> 0 then
        begin
          if FormatMessageFrom(Copy(MessagePath, 1, I - 1 )) then {rw}
//          if FormatMessageFrom(Copy(MessagePath, 1, I)) then
            Break;
          MessagePath := Copy(MessagePath, I + 1, MaxInt); {rw}
//          MessagePath := Copy(MessagePath, I, MaxInt);
        end
        else
          FormatMessageFrom(MessagePath);
      until I = 0;
    end
  finally
    FreeMem(Args)
  end;
  Result := St;
end;

function TJvNTEventLogRecord.GetUsername: string;
var
  UserName: array [0..256] of Char;
  UserNameLen: Cardinal;
  DomainName: array [0..256] of Char;
  DomainNameLen: Cardinal;
  Use: SID_NAME_USE;

begin
  Result := '';
  UserNameLen := SizeOf(UserName);
  DomainNameLen := SizeOf(DomainName);
  if LookupAccountSID(nil, SID, UserName, UserNameLen, DomainName, DomainNameLen, Use) then
    Result := string(DomainName) + '\' + string(UserName);
end;

function TJvNTEventLogRecord.GetType: string;
begin
  case PEventLogRecord(FCurrentRecord)^.EventType of
    EVENTLOG_ERROR_TYPE:
      Result := RsLogError;
    EVENTLOG_WARNING_TYPE:
      Result := RsLogWarning;
    EVENTLOG_INFORMATION_TYPE:
      Result := RsLogInformation;
    EVENTLOG_AUDIT_SUCCESS:
      Result := RsLogSuccessAudit;
    EVENTLOG_AUDIT_FAILURE:
      Result := RsLogFailureAudit;
  else
    Result := '';
  end;
end;

function TJvNTEventLogRecord.GetSource: string;
begin
  Result := PChar(FCurrentRecord) + SizeOf(TEventLogRecord);
end;

function TJvNTEventLogRecord.GetComputer: string;
var
  P: PChar;
begin
  P := PChar(FCurrentRecord) + SizeOf(TEventLogRecord);
  Result := P + StrLen(P) + 1;
end;

function TJvNTEventLogRecord.GetID: DWORD;
begin
  Result := PEventLogRecord(FCurrentRecord)^.EventID;
end;

function TJvNTEventLogRecord.GetStringCount: DWORD;
begin
  Result := PEventLogRecord(FCurrentRecord)^.NumStrings;
end;

function TJvNTEventLogRecord.GetCategory: Cardinal;
begin
  Result := PEventLogRecord(FCurrentRecord)^.EventCategory;
end;

function TJvNTEventLogRecord.GetSID: PSID;
begin
  Result := PSID(PChar(FCurrentRecord) + PEventLogRecord(FCurrentRecord)^.userSIDOffset);
end;

function TJvNTEventLogRecord.GetString(Index: Cardinal): string;
var
  P: PChar;
begin
  if Index < StringCount then
  begin
    P := PChar(FCurrentRecord) + PEventLogRecord(FCurrentRecord)^.StringOffset;
    while Index > 0 do
    begin
      Inc(P, StrLen(P) + 1);
      Dec(Index);
    end;
  end;
end;

function TJvNTEventLogRecord.GetDateTime: TDateTime;
const
  StartPoint: TDateTime = 25569.0; // January 1, 1970 00:00:00
begin
  // Result := IncSecond(StartPoint, PEventLogRecord(FCurrentRecord)^.TimeGenerated);
//  Result := IncSecond(StartPoint, PEventLogRecord(FCurrentRecord)^.TimeWritten);
  Result := ((StartPoint * 86400.0) + PEventLogRecord(FCurrentRecord)^.TimeWritten) / 86400.0;
end;

end.

