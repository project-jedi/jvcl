{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNotify.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvNotify;

interface

{$IFDEF WIN32}

uses Windows, SysUtils, Classes, Messages, ExtCtrls{, JvComponent};

type
  TFileChangeFilter = (fnFileName, fnDirName, fnAttributes, fnSize,
    fnLastWrite, fnLastAccess, fnCreation, fnSecurity);
  TFileChangeFilters = set of TFileChangeFilter;
  TJvNotifyThread = class;

{ TJvFolderMonitor }

  TJvFolderMonitor = class(TComponent)
  private
    FNotifyThread: TJvNotifyThread;
    FFilter: TFileChangeFilters;
    FDelayTimer: TTimer;
    FDelayTime: Cardinal;
    FMonitorSubtree: Boolean;
    FFolderName: string;
    FStreamedActive: Boolean;
    FOnChange: TNotifyEvent;
    function GetActive: Boolean;
    function GetDelayTime: Cardinal;
    procedure SetActive(Value: Boolean);
    procedure SetFilter(Value: TFileChangeFilters);
    procedure SetMonitorSubtree(Value: Boolean);
    procedure SetFolderName(const Value: string);
    procedure SetDelayTime(Value: Cardinal);
    procedure Timer(Sender: TObject);
    procedure ThreadNotification(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure Changed; dynamic;
    procedure FreeNotifyThread;
    procedure ResetNotifyThread(Activate: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property DelayTime: Cardinal read GetDelayTime write SetDelayTime default 0;
    property Filter: TFileChangeFilters read FFilter write SetFilter
      default [fnFileName, fnDirName, fnSize, fnLastWrite];
    property FolderName: string read FFolderName write SetFolderName;
    property MonitorSubtree: Boolean read FMonitorSubtree write SetMonitorSubtree
      default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TJvNotifyThread }

  TJvNotifyThread = class(TThread)
  private
    FNotifyHandle: THandle;
    FEvent: THandle;
    FOnChange: TNotifyEvent;
    FFinished: Boolean;
    FLastError: DWORD;
    procedure CallOnChange;
    procedure StopWaiting;
  protected
    procedure DoChange; virtual;
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(const FolderName: string; WatchSubtree: Boolean;
      Filter: TFileChangeFilters);
    destructor Destroy; override;
    procedure Terminate;
    property Terminated;
    property Finished: Boolean read FFinished;
    property LastError: DWORD read FLastError;
    property NotifyHandle: THandle read FNotifyHandle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function CreateNotifyThread(const FolderName: string; WatchSubtree: Boolean;
  Filter: TFileChangeFilters): TJvNotifyThread;

{$ENDIF WIN32}

implementation

{$IFDEF WIN32}

uses Forms, JvVCLUtils, JvFileUtil;

{$IFNDEF Delphi3_Up}
const
  FILE_NOTIFY_CHANGE_LAST_ACCESS  = $00000020;
  FILE_NOTIFY_CHANGE_CREATION     = $00000040;
{$ENDIF}

{ TJvNotifyThread }

constructor TJvNotifyThread.Create(const FolderName: string;
  WatchSubtree: Boolean; Filter: TFileChangeFilters);
const
  NotifyFilters: array[TFileChangeFilter] of DWORD = (
    FILE_NOTIFY_CHANGE_FILE_NAME, FILE_NOTIFY_CHANGE_DIR_NAME,
    FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,
    FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_LAST_ACCESS,
    FILE_NOTIFY_CHANGE_CREATION, FILE_NOTIFY_CHANGE_SECURITY);
var
  Filters: DWORD;
  I: TFileChangeFilter;
  Subtree: Integer;
begin
  FLastError := ERROR_SUCCESS;
  Filters := 0;
  for I := Low(TFileChangeFilter) to High(TFileChangeFilter) do
    if I in Filter then Filters := Filters or NotifyFilters[I];
  if WatchSubtree then Subtree := 1 else Subtree := 0;
  FNotifyHandle := FindFirstChangeNotification(PChar(FolderName),
    Bool(Subtree), Filters);
  if FNotifyHandle <> INVALID_HANDLE_VALUE then
    FEvent := CreateEvent(nil, BOOL(1), BOOL(0), nil)
  else FLastError := GetLastError;
  inherited Create(False);
end;

destructor TJvNotifyThread.Destroy;
begin
  FOnChange := nil;
  StopWaiting;
  inherited Destroy;
end;

procedure TJvNotifyThread.Terminate;
begin
  inherited Terminate;
  StopWaiting;
end;

procedure TJvNotifyThread.CallOnChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJvNotifyThread.DoChange;
begin
  if Assigned(FOnChange) then Synchronize(CallOnChange);
end;

procedure TJvNotifyThread.DoTerminate;
begin
  if FNotifyHandle <> INVALID_HANDLE_VALUE then
    FindCloseChangeNotification(FNotifyHandle);
  FNotifyHandle := INVALID_HANDLE_VALUE;
  if FEvent <> 0 then CloseHandle(FEvent);
  FEvent := 0;
  inherited DoTerminate;
end;

procedure TJvNotifyThread.Execute;
var
  Handles: array[0..1] of THandle;
begin
  while not Terminated and (FNotifyHandle <> INVALID_HANDLE_VALUE) do
  begin
    Handles[0] := FNotifyHandle;
    Handles[1] := FEvent;
    case WaitForMultipleObjects(2, PWOHandleArray(@Handles), False, INFINITE) of
      WAIT_OBJECT_0: { notification }
        if not Terminated then begin
          DoChange;
          if not FindNextChangeNotification(FNotifyHandle) then begin
            FLastError := GetLastError;
            Break;
          end;
        end;
      WAIT_OBJECT_0 + 1: { event is signaled }
        Break;
      WAIT_FAILED:
        begin
          FLastError := GetLastError;
          Break;
        end;
    end;
  end;
  FFinished := True;
end;

procedure TJvNotifyThread.StopWaiting;
begin
  if FEvent <> 0 then SetEvent(FEvent);
end;

function CreateNotifyThread(const FolderName: string; WatchSubtree: Boolean;
  Filter: TFileChangeFilters): TJvNotifyThread;
begin
  Result := TJvNotifyThread.Create(FolderName, WatchSubtree, Filter);
  try
    if Result.LastError <> ERROR_SUCCESS then
      RaiseWin32Error(Result.LastError);
  except
    Result.Free;
    raise;
  end;
end;

{ TJvFolderMonitor }

constructor TJvFolderMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter := [fnFileName, fnDirName, fnSize, fnLastWrite];
  FMonitorSubtree := True;
end;

destructor TJvFolderMonitor.Destroy;
begin
  if FDelayTimer <> nil then
    FDelayTimer.OnTimer := nil;
  FreeNotifyThread;
  FDelayTimer.Free;
  inherited Destroy;
end;

procedure TJvFolderMonitor.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then Active := True;
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else raise;
  end;
end;

function TJvFolderMonitor.GetActive: Boolean;
begin
  Result := FNotifyThread <> nil;
end;

procedure TJvFolderMonitor.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then begin
    if Value then FStreamedActive := True;
  end
  else if Active <> Value then begin
    ResetNotifyThread(Value);
  end;
end;

procedure TJvFolderMonitor.SetFilter(Value: TFileChangeFilters);
var
  SaveFilter: TFileChangeFilters;
  IsActive: Boolean;
begin
  if FFilter <> Value then begin
    SaveFilter := FFilter;
    IsActive := Active;
    FFilter := Value;
    try
      ResetNotifyThread(IsActive);
    except
      FFilter := SaveFilter;
      if IsActive then
      try
        ResetNotifyThread(True);
      except
      end;
      raise;
    end;
  end;
end;

procedure TJvFolderMonitor.SetMonitorSubtree(Value: Boolean);
begin
  if FMonitorSubtree <> Value then begin
    FMonitorSubtree := Value;
    ResetNotifyThread(Active);
  end;
end;

procedure TJvFolderMonitor.SetFolderName(const Value: string);
begin
  if FFolderName <> Value then begin
    FFolderName := Value;
    ResetNotifyThread(Active);
  end;
end;

procedure TJvFolderMonitor.FreeNotifyThread;
begin
  if FNotifyThread <> nil then
    with FNotifyThread do begin
      OnChange := nil;
      if FFinished then Free
      else begin
        FreeOnTerminate := True;
        Terminate;
      end;
    end;
  FNotifyThread := nil;
end;

procedure TJvFolderMonitor.ResetNotifyThread(Activate: Boolean);
begin
  FreeNotifyThread;
  if Activate and DirExists(FFolderName) then begin
    FNotifyThread := CreateNotifyThread(FolderName, MonitorSubtree, Filter);
    FNotifyThread.OnChange := ThreadNotification;
  end;
end;

function TJvFolderMonitor.GetDelayTime: Cardinal;
begin
  if FDelayTimer <> nil then
    Result := FDelayTimer.Interval
  else Result := FDelayTime;
end;

procedure TJvFolderMonitor.SetDelayTime(Value: Cardinal);
begin
  if (FDelayTimer <> nil) then begin
    if Value > 0 then
      FDelayTimer.Interval := Value
    else begin
      FDelayTimer.OnTimer := nil;
      FDelayTimer.Free;
      FDelayTimer := nil;
    end;
  end;
  FDelayTime := Value;
end;

procedure TJvFolderMonitor.ThreadNotification(Sender: TObject);
begin
  if FDelayTime <= 0 then
    Changed
  else if FDelayTimer = nil then begin
    FDelayTimer := TTimer.Create(Self);
    with FDelayTimer do begin
      Interval := FDelayTime;
      OnTimer := Timer;
      Enabled := True;
    end;
  end;
end;

procedure TJvFolderMonitor.Timer(Sender: TObject);
begin
  FDelayTimer.Free;
  FDelayTimer := nil;
  Changed;
end;

procedure TJvFolderMonitor.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{$ENDIF WIN32}

end.
