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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvThread;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JvComponent;

type
  TJvThread = class(TJvComponent)
  private
    FThreadCount: Integer;
    FExclusive: Boolean;
    FRunOnCreate: Boolean;
    FOnBegin: TNotifyEvent;
    FOnExecute: TNotifyEventParams;
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
    function Execute(p: Pointer): Thandle;
    function OneThreadIsRunning: Boolean;
    function GetPriority(Thread: Thandle): TThreadPriority;
    procedure SetPriority(Thread: THandle; Priority: TThreadPriority);
    procedure QuitThread(Thread: Thandle);
    procedure Suspend(Thread: Thandle);
    procedure Resume(Thread: Thandle);
    property Exclusive: Boolean read FExclusive write FExclusive;
    property RunOnCreate: Boolean read FRunOnCreate write FRunOnCreate;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Onbegin: TNotifyEvent read FOnBegin write FOnBegin;
    property OnExecute: TNotifyEventParams read FOnExecute write FOnExecute;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnFinishAll: TNotifyEvent read FOnFinishAll write FOnFinishAll;
  end;

  TJvHideThread = class(TThread)
  private
    FExecuteEvent: TNotifyEventParams;
    FParams: Pointer;
  public
    constructor Create(event: TNotifyEventParams; params: Pointer); virtual;
    procedure Execute; override;
  end;

procedure Synchronize(Method: TNotifyEvent);
procedure SynchronizeParams(Method: TNotifyEventParams; p: Pointer);

implementation

var
  mtx: THandle;

  {*****************************************************}

procedure Synchronize(Method: TNotifyEvent);
begin
  WaitForSingleObject(mtx, INFINITE);
  Method(nil);
  ReleaseMutex(mtx);
end;

{*****************************************************}

procedure SynchronizeParams(Method: TNotifyEventParams; p: Pointer);
begin
  WaitForSingleObject(mtx, INFINITE);
  Method(nil, p);
  ReleaseMutex(mtx);
end;

///////////////////////////////////////////////////////////
// TJvThread
///////////////////////////////////////////////////////////

constructor TJvThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadCount := 0;
  FRunOnCreate := True;
  FExclusive := True;
  FreeOnTerminate := True;
end;

{*****************************************************}

destructor TJvThread.Destroy;
begin
  inherited Destroy;
end;

{*****************************************************}

function TJvThread.Execute(p: Pointer): Thandle;
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
    HideThread := TJvHideThread.Create(FOnExecute, p);
    HideThread.FreeOnTerminate := FFreeOnTerminate;
    HideThread.OnTerminate := DoTerminate;
    DoCreate;
    if FRunOnCreate then
      HideThread.Resume;
    Result := HideThread.ThreadID;
  end;
end;

{*****************************************************}

function TJvThread.GetPriority(Thread: Thandle): TThreadPriority;
begin
  Result := tpIdle;
  if Thread <> 0 then
    Result := TThreadPriority(GetThreadPriority(Thread));
end;

{*****************************************************}

procedure TJvThread.SetPriority(Thread: THandle; Priority: TThreadPriority);
begin
  SetThreadPriority(Thread, Integer(Priority));
end;

{*****************************************************}

procedure TJvThread.QuitThread(Thread: Thandle);
begin
  TerminateThread(Thread, 0);
end;

{*****************************************************}

procedure TJvThread.Suspend(Thread: Thandle);
begin
  SuspendThread(Thread);
end;

{*****************************************************}

procedure TJvThread.Resume(Thread: Thandle);
begin
  ResumeThread(thread);
end;

{*****************************************************}

procedure TJvThread.DoCreate;
begin
  if Assigned(FOnBegin) then
    FOnBegin(nil);
end;

{*****************************************************}

procedure TJvThread.DoTerminate;
begin
  Dec(FThreadCount);
  if Assigned(FOnFinish) then
    FOnFinish(nil);
  if FThreadCount = 0 then
    if Assigned(FOnFinishAll) then
      FOnFinishAll(nil);
end;

{*****************************************************}

function TJvThread.OneThreadIsRunning: Boolean;
begin
  Result := FThreadCount > 0;
end;

///////////////////////////////////////////////////////////
// TJvHideThread
///////////////////////////////////////////////////////////

constructor TJvHideThread.Create(event: TNotifyEventParams; params: Pointer);
begin
  inherited Create(True);
  FExecuteEvent := event;
  FParams := params;
end;

{*****************************************************}

procedure TJvHideThread.Execute;
begin
  FExecuteEvent(nil, FParams);
end;

{*****************************************************}

initialization
  mtx := CreateMutex(nil, False, 'VCLJvThreadMutex');

finalization
  CloseHandle(mtx);

end.
