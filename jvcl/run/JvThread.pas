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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvThread;

interface

uses
  Windows, SysUtils, Classes,
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
  protected
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

  TJvHideThread = class(TThread)
  private
    FExecuteEvent: TJvNotifyParamsEvent;
    FParams: Pointer;
  public
    constructor Create(Event: TJvNotifyParamsEvent; Params: Pointer); virtual;
    procedure Execute; override;
  end;

procedure Synchronize(Method: TNotifyEvent);
procedure SynchronizeParams(Method: TJvNotifyParamsEvent; P: Pointer);

implementation

var
  Mtx: THandle;

procedure Synchronize(Method: TNotifyEvent);
begin
  WaitForSingleObject(Mtx, INFINITE);
  Method(nil);
  ReleaseMutex(Mtx);
end;

procedure SynchronizeParams(Method: TJvNotifyParamsEvent; P: Pointer);
begin
  WaitForSingleObject(Mtx, INFINITE);
  Method(nil, P);
  ReleaseMutex(Mtx);
end;

//=== TJvThread ==============================================================

constructor TJvThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadCount := 0;
  FRunOnCreate := True;
  FExclusive := True;
  FreeOnTerminate := True;
end;

destructor TJvThread.Destroy;
begin
  inherited Destroy;
end;

function TJvThread.Execute(P: Pointer): THandle;
var
  HideThread: TJvHideThread;
begin
  Result := 0;
  if Assigned(FOnExecute) then
  begin
    if Exclusive then
      if OneThreadIsRunning then
        Exit;
    Inc(FThreadCount);
    HideThread := TJvHideThread.Create(FOnExecute, P);
    HideThread.FreeOnTerminate := FFreeOnTerminate;
    HideThread.OnTerminate := DoTerminate;
    DoCreate;
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
  if Assigned(FOnFinish) then
    FOnFinish(nil);
  if FThreadCount = 0 then
    if Assigned(FOnFinishAll) then
      FOnFinishAll(nil);
end;

function TJvThread.OneThreadIsRunning: Boolean;
begin
  Result := FThreadCount > 0;
end;

//=== TJvHideThread ==========================================================

constructor TJvHideThread.Create(Event: TJvNotifyParamsEvent; Params: Pointer);
begin
  inherited Create(True);
  FExecuteEvent := Event;
  FParams := Params;
end;

procedure TJvHideThread.Execute;
begin
  FExecuteEvent(nil, FParams);
end;

initialization
  Mtx := CreateMutex(nil, False, 'VCLJvThreadMutex');

finalization
  CloseHandle(Mtx);

end.

