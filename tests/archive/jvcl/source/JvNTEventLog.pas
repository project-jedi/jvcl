{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEventLog.PAS, released on 2002-09-02.

The Initial Developer of the Original Code is Fernando Silva [fernando.silva@myrealbox.com]
Portions created by Fernando Silva are Copyright (C) 2002 Fernando Silva.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-09-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvNTEventLog;

interface

uses
  Windows, Classes, SysUtils, JvComponent;

type
  TNotifyChangeEventLog = class;
  TJvNTEventLogRecord = class;

  TJvNTEventLog = class(TJvComponent)
  private
    FLogHandle: THandle;
    FLog: string;
    FServer: string;
    FSource: string;
    FActive: boolean;

    FOnChange: TNotifyEvent;
    FNotifyThread: TNotifyChangeEventLog;
    FEventRecord: TJvNTEventLogRecord;

    procedure SetActive(value: boolean);
    procedure SetServer(const value: string);
    procedure SetSource(const value: string);
    procedure SetLog(const value: string);

    function GetEventCount: Cardinal;

    procedure SeekRecord(n: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure First;
    procedure Last;
    function Eof: Boolean;
    procedure Next;
    procedure Seek(n: Cardinal);
    procedure ReadEventLogs(aStrings: TStrings);

    property EventCount: Cardinal read GetEventCount;
    property EventRecord: TJvNTEventLogRecord read FEventRecord;
  published
    property Server: string read FServer write SetServer;
    property Source: string read FSource write SetSource;
    property Log: string read FLog write SetLog;
    property Active: boolean read FActive write SetActive;
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

  TJvNTEventLogRecord = class
  private
    FEventLog: TJvNTEventLog;
    FCurrentRecord: Pointer;

    function GetRecordNumber: Cardinal;
    function GetDateTime: TDateTime;
    function GetID: DWORD;
    function GetType: string;
    function GetStringCount: DWORD;
    function GetCategory: Cardinal;
    function GetSource: string;
    function GetComputer: string;
    function GetSID: PSID;
    function GetString(index: Cardinal): string;
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
    property EventString[index: Cardinal]: string read GetString;
    property MessageText: string read GetMessageText;
    property Username: string read GetUsername;
  end;



implementation

uses
  Registry{$IFNDEF COMPILER6_UP} ,JvFunctions{$ENDIF};

const
  EVENTLOG_SEQUENTIAL_READ = $0001;
  EVENTLOG_SEEK_READ = $0002;
  EVENTLOG_FORWARDS_READ = $0004;
  EVENTLOG_BACKWARDS_READ = $0008;

type
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
    ReservedFlags: WORD; // For use with paired events (auditing)
    ClosingRecordNumber: DWORD; // For use with paired events (auditing)
    StringOffset: DWORD; // Offset from beginning of record
    UserSidLength: DWORD;
    UserSidOffset: DWORD;
    DataLength: DWORD;
    DataOffset: DWORD; // Offset from beginning of record
  end;
  PEventLogRecord = ^TEventLogRecord;

  { TJvNTEventLog }

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
  inherited;
end;

procedure TJvNTEventLog.SetActive(value: boolean);
begin
  if value <> FActive then
    if csDesigning in ComponentState then
      FActive := value
    else if value then
      Open
    else
      Close
end;

procedure TJvNTEventLog.SetServer(const value: string);
var
  lOldActive: boolean;

begin
  if FServer <> value then
  begin
    lOldActive := Active;
    Active := False;
    FServer := value;
    Active := lOldActive;
  end
end;

procedure TJvNTEventLog.SetSource(const value: string);
var
  lOldActive: boolean;

begin
  if FSource <> value then
  begin
    lOldActive := Active;
    Active := False;
    FSource := value;
    Active := lOldActive;
  end
end;

procedure TJvNTEventLog.SetLog(const value: string);
var
  lOldActive: boolean;

begin
  if FLog <> value then
  begin
    lOldActive := Active;
    Active := False;
    FLog := value;
    Active := lOldActive;
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
  Result := (EventRecord.RecordNumber = GetEventCount);
end;

procedure TJvNTEventLog.Next;
var
  bytesRead, bytesNeeded, flags: DWORD;
  dummy: char;

begin
  flags := EVENTLOG_SEQUENTIAL_READ;
  flags := flags or EVENTLOG_FORWARDS_READ;

  ReadEventLog(FLogHandle, flags, 0, @dummy, 0, bytesRead, bytesNeeded);
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    ReallocMem(FEventRecord.FCurrentRecord, bytesNeeded);

    if not ReadEventLog(FLogHandle, flags, 0, FEventRecord.FCurrentRecord, bytesNeeded, bytesRead, bytesNeeded) then
      RaiseLastOSError;
  end
  else
    RaiseLastOSError;
end;

procedure TJvNTEventLog.SeekRecord(n: Cardinal);
var
  offset, flags: DWORD;
  bytesRead, bytesNeeded: Cardinal;
  dummy: char;
  recNo: Integer;

begin
  GetOldestEventLogRecord(FLogHandle, offset);
  recNo := n + offset;

  flags := EVENTLOG_SEEK_READ;
  flags := flags or EVENTLOG_FORWARDS_READ;

  ReadEventLog(FLogHandle, flags, recNo, @dummy, 0, bytesRead, bytesNeeded);
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    ReallocMem(FEventRecord.FCurrentRecord, bytesNeeded);
    if not ReadEventLog(FLogHandle, flags, recNo, FEventRecord.FCurrentRecord, bytesNeeded, bytesRead, bytesNeeded) then
      RaiseLastOSError;
  end
  else
    RaiseLastOSError;
end;

procedure TJvNTEventLog.Seek(n: Cardinal);
begin
  if n <> FEventRecord.RecordNumber then
    SeekRecord(n);
end;

procedure TJvNTEventLog.ReadEventLogs(aStrings: TStrings);
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey('SYSTEM\CurrentControlSet\Services\EventLog', False);

    GetKeyNames(aStrings);
  finally
    Free;
  end;
end;

{ TNotifyChangeEventLog }

constructor TNotifyChangeEventLog.Create(AOwner: TComponent);
begin
  inherited Create(true); // Create thread suspended
  FreeOnTerminate := true; // Thread Free Itself when terminated

  // initialize system events
  FEventLog := TJvNTEventLog(AOwner);
  FEventHandle := CreateEvent(nil, True, False, nil);
  NotifyChangeEventLog(FEventLog.FLogHandle, FEventHandle);

  Suspended := false; // Continue the thread
end;

procedure TNotifyChangeEventLog.DoChange;
begin
  if Assigned(FEventLog.FOnChange) then
    FEventLog.FOnChange(FEventLog);
end;

procedure TNotifyChangeEventLog.Execute;
var
  lResult: DWORD;

begin
  while (not Terminated) do
  begin
    // reset event signal, so we can get it again
    ResetEvent(FEventHandle);
    // wait for event to happen
    lResult := WaitForSingleObject(FEventHandle, INFINITE);
    // check event result
    case lResult of
      WAIT_OBJECT_0: Synchronize(DoChange);
    else
      Synchronize(DoChange);
    end;
  end;
end;

{ TJvNTEventLogRecord }

constructor TJvNTEventLogRecord.Create(AOwner: TComponent);
begin
  FEventLog := TJvNTEventLog(AOwner);
  FCurrentRecord := nil;
end;

function TJvNTEventLogRecord.GetRecordNumber: Cardinal;
begin
  Result := PEventLogRecord(fCurrentRecord)^.RecordNumber;
end;

function TJvNTEventLogRecord.GetMessageText: string;
var
  messagePath: string;
  count, i: Integer;
  p: Pchar;
  args, pArgs: ^PCHAR;
  st: string;

  function FormatMessageFrom(const dllName: string): boolean;
  var
    dllModule: THandle;
    buffer: PChar;
    fullDLLName: array[0..MAX_PATH] of char;
  begin
    result := False;
    ExpandEnvironmentStrings(PChar(dllName), fullDllName, MAX_PATH);
    dllModule := LoadLibraryEx(fullDLLName, 0, LOAD_LIBRARY_AS_DATAFILE);
    if dllModule <> 0 then
    try
      if FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ARGUMENT_ARRAY,
        pointer(dllModule),
        ID,
        0,
        @buffer,
        0,
        args) > 0 then
      begin
        buffer[lstrlen(buffer) - 2] := #0;
        st := buffer;

        result := True
      end
    finally
      FreeLibrary(dllModule)
    end
  end;

begin
  st := '';

  count := StringCount;
  GetMem(args, count * sizeof(PChar));
  try
    pArgs := args;
    p := PEventLogRecord(fCurrentRecord)^.StringOffset + PChar(fCurrentRecord);
    for i := 0 to count - 1 do
    begin
      pArgs^ := p;
      Inc(p, lstrlen(p) + 1);
      Inc(pArgs)
    end;

    with TRegistry.Create do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(Format('SYSTEM\CurrentControlSet\Services\EventLog\%s\%s', [FEventLog.Log, FEventLog.Source]), False);
      messagePath := ReadString('EventMessageFile');

      repeat
        i := Pos(';', MessagePath);
        if i <> 0 then
        begin
          if FormatMessageFrom(Copy(MessagePath, 1, i)) then
            break;
          MessagePath := Copy(MessagePath, i, MaxInt);
        end
        else
          FormatMessageFrom(MessagePath)
      until i = 0
    end
  finally
    FreeMem(args)
  end;
  result := st
end;

function TJvNTEventLogRecord.GetUsername: string;
var
  userName: array[0..256] of char;
  userNameLen: Cardinal;
  domainName: array[0..256] of char;
  domainNameLen: Cardinal;
  use: SID_NAME_USE;

begin
  Result := '';
  userNameLen := sizeof(userName);
  domainNameLen := sizeof(domainName);
  if LookupAccountSID(nil, SID, userName, userNameLen, domainName, domainNameLen, use) then
    Result := string(domainName) + '\' + string(userName);
end;

function TJvNTEventLogRecord.GetType: string;
begin
  case PEventLogRecord(fCurrentRecord)^.EventType of
    EVENTLOG_ERROR_TYPE: Result := 'Error';
    EVENTLOG_WARNING_TYPE: Result := 'Warning';
    EVENTLOG_INFORMATION_TYPE: Result := 'Information';
    EVENTLOG_AUDIT_SUCCESS: Result := 'Success Audit';
    EVENTLOG_AUDIT_FAILURE: Result := 'Failure Audit';
  else
    Result := '';
  end;
end;

function TJvNTEventLogRecord.GetSource: string;
begin
  result := PChar(fCurrentRecord) + sizeof(TEventLogRecord);
end;

function TJvNTEventLogRecord.GetComputer: string;
var
  p: PChar;

begin
  p := PChar(fCurrentRecord) + sizeof(TEventLogRecord);
  result := p + lstrlen(p) + 1;
end;

function TJvNTEventLogRecord.GetID: DWORD;
begin
  result := PEventLogRecord(fCurrentRecord)^.EventID;
end;

function TJvNTEventLogRecord.GetStringCount: DWORD;
begin
  result := PEventLogRecord(fCurrentRecord)^.NumStrings;
end;

function TJvNTEventLogRecord.GetCategory: Cardinal;
begin
  Result := PEventLogRecord(fCurrentRecord)^.EventCategory;
end;

function TJvNTEventLogRecord.GetSID: PSID;
begin
  result := PSID(PChar(fCurrentRecord) + PEventLogRecord(fCurrentRecord)^.userSIDOffset);
end;

function TJvNTEventLogRecord.GetString(index: Cardinal): string;
var
  p: PChar;

begin
  if index < StringCount then
  begin
    p := PChar(fCurrentRecord) + PEventLogRecord(fCurrentRecord)^.StringOffset;
    while index > 0 do
    begin
      Inc(p, lstrlen(p) + 1);
      Dec(index);
    end
  end
end;

function TJvNTEventLogRecord.GetDateTime: TDateTime;
const
  lStartPoint: TDateTime = 25569; // January 1, 1970 00:00:00

begin
  // Result := IncSecond(lStartPoint, PEventLogRecord(fCurrentRecord)^.TimeGenerated);
//  Result := IncSecond(lStartPoint, PEventLogRecord(fCurrentRecord)^.TimeWritten);
  Result := ((lStartPoint * 86400) + PEventLogRecord(fCurrentRecord)^.TimeWritten) / 86400;
end;


end.

