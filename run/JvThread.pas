{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThread.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2004-01-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvThread;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Types, QWindows,
  {$ENDIF LINUX}
  SysUtils, Classes,
  JvTypes, JvComponent;

type
  TJvThread = class(TJvComponent)
  private
    FThreadCount: Integer;
    FExclusive: Boolean;
    FRunOnCreate: Boolean;
    FOnBegin: TNotifyEvent;
    FOnExecute: TJvNotifyParamsEvent;
    FOnFinish: TNotifyEvent;
    FOnFinishAll: TNotifyEvent;
    FFreeOnTerminate: Boolean;
    procedure DoCreate;
    procedure DoTerminate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    function Execute(P: Pointer): THandle;
    function OneThreadIsRunning: Boolean;
    function GetPriority(Thread: THandle): TThreadPriority;
    procedure SetPriority(Thread: THandle; Priority: TThreadPriority);
    procedure QuitThread(Thread: THandle);
    procedure Suspend(Thread: THandle);
    procedure Resume(Thread: THandle);
    property Exclusive: Boolean read FExclusive write FExclusive;
    property RunOnCreate: Boolean read FRunOnCreate write FRunOnCreate;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property OnBegin: TNotifyEvent read FOnBegin write FOnBegin;
    property OnExecute: TJvNotifyParamsEvent read FOnExecute write FOnExecute;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnFinishAll: TNotifyEvent read FOnFinishAll write FOnFinishAll;
  end;

// Cannot be synchronized to the MainThread (VCL)
procedure Synchronize(Method: TNotifyEvent);
procedure SynchronizeParams(Method: TJvNotifyParamsEvent; P: Pointer);

implementation

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

//=== TJvThread ==============================================================

constructor TJvThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadCount := 0;
  FRunOnCreate := True;
  FExclusive := True;
  FFreeOnTerminate := True;
end;

destructor TJvThread.Destroy;
begin
  while OneThreadIsRunning do
    Sleep(0);
  inherited Destroy;
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
      DoCreate;
    except
      HideThread.Free;
      raise;
    end;
    if FRunOnCreate then
      HideThread.Resume;
    Result := HideThread.ThreadID;
  end;
end;

function TJvThread.GetPriority(Thread: THandle): TThreadPriority;
begin
  Result := tpIdle;
  if Thread <> 0 then
    Result := TThreadPriority(GetThreadPriority(Thread));
end;

procedure TJvThread.SetPriority(Thread: THandle; Priority: TThreadPriority);
begin
  SetThreadPriority(Thread, Integer(Priority));
end;

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
end;

procedure TJvThread.DoCreate;
begin
  if Assigned(FOnBegin) then
    FOnBegin(nil);
end;

procedure TJvThread.DoTerminate;
begin
  Dec(FThreadCount);
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

//=== TJvHideThread ==========================================================

constructor TJvHideThread.Create(Sender: TObject; Event: TJvNotifyParamsEvent; Params: Pointer);
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

initialization
  SyncMtx := CreateMutex(nil, False, 'VCLJvThreadMutex');

finalization
  CloseHandle(SyncMtx);
  SyncMtx := 0;

end.

