{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThread.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvThread;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows,
  {$ENDIF UNIX}
  JvTypes, JvComponent, JvThreadDialog;

type
  TJvThread = class(TJvComponent)
  private
    FThreadCount: Integer;
    FThreads: TThreadList;
    FExclusive: Boolean;
    FRunOnCreate: Boolean;
    FOnBegin: TNotifyEvent;
    FOnExecute: TJvNotifyParamsEvent;
    FOnFinish: TNotifyEvent;
    FOnFinishAll: TNotifyEvent;
    FFreeOnTerminate: Boolean;
    FThreadDialog: TJvCustomThreadDialog;
    fThreadDialogForm: TJvCustomThreadDialogForm;
    procedure DoCreate;
    procedure DoTerminate(Sender: TObject);
    function GetCount: Integer;
    function GetThreads(Index: Integer): TThread;
    function GetTerminated: Boolean;
    procedure CreateThreadDialogForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property Count: Integer read GetCount;
    property Threads[Index: Integer]: TThread read GetThreads;
  published
    function Execute(P: Pointer): THandle;
    function OneThreadIsRunning: Boolean;
    function GetPriority(Thread: THandle): TThreadPriority;
    procedure SetPriority(Thread: THandle; Priority: TThreadPriority);
    {$IFDEF UNIX}
    function GetPolicy(Thread: THandle): Integer;
    procedure SetPolicy(Thread: THandle; Policy: Integer);
    {$ENDIF UNIX}
    procedure QuitThread(Thread: THandle);
    procedure Suspend(Thread: THandle); // should not be used
    procedure Resume(Thread: THandle);
    procedure Terminate; // terminates all running threads
    property Terminated: Boolean read GetTerminated;
    property Exclusive: Boolean read FExclusive write FExclusive;
    property RunOnCreate: Boolean read FRunOnCreate write FRunOnCreate;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property OnBegin: TNotifyEvent read FOnBegin write FOnBegin;
    property OnExecute: TJvNotifyParamsEvent read FOnExecute write FOnExecute;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnFinishAll: TNotifyEvent read FOnFinishAll write FOnFinishAll;
    property ThreadDialog: TJvCustomThreadDialog read FThreadDialog write FThreadDialog;
  end;

// Cannot be synchronized to the MainThread (VCL)
procedure Synchronize(Method: TNotifyEvent);
procedure SynchronizeParams(Method: TJvNotifyParamsEvent; P: Pointer);

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

var
  SyncMtx: THandle = 0;

procedure Synchronize(Method: TNotifyEvent);
begin
  WaitForSingleObject(SyncMtx, INFINITE);
  try
    Method(nil);
  finally
    ReleaseMutex(SyncMtx);
  end;
end;

procedure SynchronizeParams(Method: TJvNotifyParamsEvent; P: Pointer);
begin
  WaitForSingleObject(SyncMtx, INFINITE);
  try
    Method(nil, P);
  finally
    ReleaseMutex(SyncMtx);
  end;
end;

type
  TJvHideThread = class(TThread)
  private
    FExecuteEvent: TJvNotifyParamsEvent;
    FParams: Pointer;
    FSender: TObject;
    FException: Exception;
    FExceptionAddr: Pointer;
    procedure ExceptionHandler;
  public
    constructor Create(Sender: TObject; Event: TJvNotifyParamsEvent;
      Params: Pointer); virtual;
    procedure Execute; override;
  end;

//=== { TJvThread } ==========================================================

constructor TJvThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadCount := 0;
  FRunOnCreate := True;
  FExclusive := True;
  FFreeOnTerminate := True;
  FThreads := TThreadList.Create;
end;

destructor TJvThread.Destroy;
begin
  Terminate;
  while OneThreadIsRunning do
  begin
    Sleep(1);
    {$IFDEF COMPILER6_UP}
    // Delphi 5 uses SendMessage -> no need for this code
    // Delphi 6+ uses an event and CheckSynchronize
    CheckSynchronize; // TThread.OnTerminate is synchronized
    {$ENDIF COMPILER6_UP}
  end;
  FThreads.Free;
  inherited Destroy;
end;

procedure TJvThread.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification (AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FThreadDialog then
      FThreadDialog := nil
    else if AComponent = FThreadDialogForm then
      FThreadDialogForm:= nil
end;

function TJvThread.Execute(P: Pointer): THandle;
var
  HideThread: TJvHideThread;
begin
  Result := 0;
  if Exclusive and OneThreadIsRunning then
    Exit;

  if Assigned(FOnExecute) then
  begin
    Inc(FThreadCount);
    HideThread := TJvHideThread.Create(Self, FOnExecute, P);
    try
      HideThread.FreeOnTerminate := FFreeOnTerminate;
      HideThread.OnTerminate := DoTerminate;
      FThreads.Add(HideThread);
      DoCreate;
    except
      HideThread.Free;
      raise;
    end;
    if FRunOnCreate then
    begin
      HideThread.Resume;
      CreateThreadDialogForm;
    end;
    Result := HideThread.ThreadID;
  end;
end;

function TJvThread.GetPriority(Thread: THandle): TThreadPriority;
begin
  {$IFDEF MSWINDOWS}
  Result := tpIdle;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := 0;
  {$ENDIF UNIX}
  if Thread <> 0 then
    Result := TThreadPriority(GetThreadPriority(Thread));
end;

procedure TJvThread.SetPriority(Thread: THandle; Priority: TThreadPriority);
begin
  SetThreadPriority(Thread, Integer(Priority));
end;

{$IFDEF UNIX}

function TJvThread.GetPolicy(Thread: THandle): Integer;
begin
  Result := 0;
  if Thread <> 0 then
    Result := GetThreadPolicy(Thread);
end;

procedure TJvThread.SetPolicy(Thread: THandle; Policy: Integer);
begin
  if Thread <> 0 then
    SetThreadPriority(Thread, Policy);
end;

{$ENDIF UNIX}

procedure TJvThread.QuitThread(Thread: THandle);
begin
  TerminateThread(Thread, 0);
end;

procedure TJvThread.Suspend(Thread: THandle);
begin
  SuspendThread(Thread);
end;

procedure TJvThread.Resume(Thread: THandle);
begin
  ResumeThread(Thread);
  CreateThreadDialogForm;
end;

procedure TJvThread.DoCreate;
begin
  if Assigned(FOnBegin) then
    FOnBegin(nil);
end;

procedure TJvThread.DoTerminate(Sender: TObject);
begin
  Dec(FThreadCount);
  FThreads.Remove(Sender);
  try
    if Assigned(FOnFinish) then
      FOnFinish(nil);
  finally
    if FThreadCount = 0 then
      if Assigned(FOnFinishAll) then
        FOnFinishAll(nil);
  end;
end;

function TJvThread.OneThreadIsRunning: Boolean;
begin
  Result := FThreadCount > 0;
end;

procedure TJvThread.Terminate;
var
  List: TList;
  I: Integer;
begin
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      TJvHideThread(List[I]).Terminate;
      if TJvHideThread(List[I]).Suspended then
        TJvHideThread(List[I]).Resume;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

function TJvThread.GetCount: Integer;
var
  List: TList;
begin
  List := FThreads.LockList;
  try
    Result := List.Count;
  finally
    FThreads.UnlockList;
  end;
end;

function TJvThread.GetThreads(Index: Integer): TThread;
var
  List: TList;
begin
  List := FThreads.LockList;
  try
    Result := TJvHideThread(List[Index]);
  finally
    FThreads.UnlockList;
  end;
end;

function TJvThread.GetTerminated: Boolean;
var
  I: Integer;
  List: TList;
begin
  Result := True;
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Result := Result and TJvHideThread(List[I]).Terminated;
      if not Result then
        Break;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TJvThread.CreateThreadDialogForm;
begin
  if Assigned(ThreadDialog) and
     Not Assigned(fThreadDialogForm) then
    fThreadDialogForm := ThreadDialog.CreateThreadDialogForm (self);
end;
//=== { TJvHideThread } ======================================================

constructor TJvHideThread.Create(Sender: TObject; Event: TJvNotifyParamsEvent;
  Params: Pointer);
begin
  inherited Create(True);
  FSender := Sender;
  FExecuteEvent := Event;
  FParams := Params;
end;

procedure TJvHideThread.ExceptionHandler;
begin
  ShowException(FException, FExceptionAddr);
end;

procedure TJvHideThread.Execute;
begin
  try
    FExecuteEvent(FSender, FParams);
  except
    on E: Exception do
    begin
      FException := E;
      FExceptionAddr := ExceptAddr;
      Self.Synchronize(ExceptionHandler);
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

  SyncMtx := CreateMutex(nil, False, 'VCLJvThreadMutex');

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  CloseHandle(SyncMtx);

end.

