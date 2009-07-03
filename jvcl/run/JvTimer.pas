{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimer.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTimer;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, ExtCtrls, Classes;

type
  TJvTimerEventTime = (tetPre, tetPost);

  TJvTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FSyncEvent: Boolean;
    FThreaded: Boolean;
    FTimerThread: TThread;
    FTimer: TTimer;
    FEventTime: TJvTimerEventTime;
    FThreadPriority: TThreadPriority;
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure SetThreaded(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure UpdateTimer;
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Synchronize(Method: TThreadMethod);
  published
    property EventTime: TJvTimerEventTime read FEventTime write FEventTime default tetPre;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property SyncEvent: Boolean read FSyncEvent write FSyncEvent default True;
    property Threaded: Boolean read FThreaded write SetThreaded default True;
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority default tpNormal;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
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
  Forms, Consts, SyncObjs,
  JvJVCLUtils;

//=== { TJvTimerThread } =====================================================

type
  TJvTimerThread = class(TThread)
  private
    FOwner: TJvTimer;
    FInterval: Cardinal;
    FException: Exception;
    FPaused: Boolean;
    FPauseSection: TCriticalSection;
    FCurrentDuration: Cardinal;

    procedure HandleException;
    procedure SetPaused(const Value: Boolean);
    function GetPaused: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Timer: TJvTimer; Enabled: Boolean);
    destructor Destroy; override;
    {$IFDEF CLR}
    procedure Synchronize(Method: TThreadMethod); // makes method public
    {$ENDIF CLR}
    property Terminated;

    property Paused: Boolean read GetPaused write SetPaused;
  end;

constructor TJvTimerThread.Create(Timer: TJvTimer; Enabled: Boolean);
begin
  FOwner := Timer;
  FPauseSection := TCriticalSection.Create;
  inherited Create(not Enabled);
  FInterval := 1000;
  FreeOnTerminate := False;
end;

procedure TJvTimerThread.HandleException;
begin
  if not (FException is EAbort) then
    Application.HandleException(Self);
end;

procedure TJvTimerThread.SetPaused(const Value: Boolean);
begin
  if FPaused <> Value then
  begin
    FPauseSection.Acquire;
    FPaused := Value;
    FPauseSection.Release;

    if not FPaused and Suspended then
      Resume;
  end;
end;

{$IFDEF CLR}
procedure TJvTimerThread.Synchronize(Method: TThreadMethod);
begin
  inherited Synchronize(Method);
end;
{$ENDIF CLR}

destructor TJvTimerThread.Destroy;
begin
  inherited Destroy;

  // Used by Execute, and hence in the inherited Destroy (Mantis 3819).
  FPauseSection.Free;
end;

procedure TJvTimerThread.Execute;
const
  Step = 10;  // Time of a wait slot, in milliseconds
var
  EventTime: TJvTimerEventTime;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwner = nil);
  end;

begin
  repeat
    EventTime := FOwner.EventTime;

    if EventTime = tetPost then
    begin
      { Wait first and then trigger the event }
      FCurrentDuration := 0;
      while not ThreadClosed and (FCurrentDuration < FInterval) do
      begin
        SleepEx(Step, False);
        Inc(FCurrentDuration, Step);
      end;
    end;

    if not ThreadClosed and not ThreadClosed and FOwner.FEnabled then
    begin
      if FOwner.SyncEvent then
      begin
        Synchronize(FOwner.Timer)
      end
      else
      begin
        try
          FOwner.Timer;
        except
          on E: Exception do
          begin
            FException := E;
            HandleException;
          end;
        end;
      end;
    end;

    if EventTime = tetPre then
    begin
      { Wait after the event was triggered }
      FCurrentDuration := 0;
      while not ThreadClosed and (FCurrentDuration < FInterval) do
      begin
        SleepEx(Step, False);
        Inc(FCurrentDuration, Step);
      end;
    end;

    // while we are paused, we do not do anything. However, we do call SleepEx
    // in the alertable state to avoid 100% CPU usage. Note that the delay
    // should not be 0 as it may lead to 100% CPU in that case. 10 is a safe
    // value that is small enough not to have a big impact on restart.
    while Paused and not Terminated do
      SleepEx(10, True);
  until Terminated;
end;

function TJvTimerThread.GetPaused: Boolean;
begin
  FPauseSection.Acquire;
  Result := FPaused;
  FPauseSection.Release;
end;

//=== { TJvTimer } ===========================================================

constructor TJvTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventTime := tetPre;
  FEnabled := True;
  FInterval := 1000;
  FSyncEvent := True;
  FThreaded := True;
  FThreadPriority := tpNormal;
  FTimerThread := TJvTimerThread.Create(Self, False);
  FTimer := nil;
end;

destructor TJvTimer.Destroy;
begin
  Destroying;
  FEnabled := False;
  FOnTimer := nil;
  {TTimerThread(FTimerThread).FOwner := nil;}
  FTimerThread.Terminate;
  while FTimerThread.Suspended do
    FTimerThread.Resume;
  (FTimerThread as TJvTimerThread).Paused := False;
  FTimerThread.Free;
  FTimer.Free;
  inherited Destroy;
end;

procedure TJvTimer.UpdateTimer;
begin
  if FThreaded then
  begin
    FreeAndNil(FTimer);
    (FTimerThread as TJvTimerThread).Paused := True;
    (FTimerThread as TJvTimerThread).FCurrentDuration := 0;
{    if not FTimerThread.Suspended then
      FTimerThread.Suspend;}
    TJvTimerThread(FTimerThread).FInterval := FInterval;
    if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    begin
      FTimerThread.Priority := FThreadPriority;

      (FTimerThread as TJvTimerThread).Paused := False;
(*      while FTimerThread.Suspended do
        FTimerThread.Resume;*)
    end;
  end
  else
  begin
    if not FTimerThread.Suspended then
      FTimerThread.Suspend;
    if not Assigned(FTimer) then
      FTimer := TTimer.Create(Self);
    FTimer.Interval := FInterval;
    FTimer.OnTimer := FOnTimer;
    FTimer.Enabled := (FInterval <> 0) and FEnabled and Assigned(FOnTimer);
  end;
end;

procedure TJvTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TJvTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TJvTimer.SetThreaded(Value: Boolean);
begin
  if Value <> FThreaded then
  begin
    FThreaded := Value;
    UpdateTimer;
  end;
end;

procedure TJvTimer.SetThreadPriority(Value: TThreadPriority);
begin
  if Value <> FThreadPriority then
  begin
    FThreadPriority := Value;
    if FThreaded then
      UpdateTimer;
  end;
end;

procedure TJvTimer.Synchronize(Method: TThreadMethod);
begin
  if FTimerThread <> nil then
  begin
    with TJvTimerThread(FTimerThread) do
    begin
      if Suspended or Terminated then
        Method
      else
        TJvTimerThread(FTimerThread).Synchronize(Method);
    end;
  end
  else
    Method;
end;

procedure TJvTimer.SetOnTimer(Value: TNotifyEvent);
begin
  if Assigned(FOnTimer) <> Assigned(Value) then
  begin
    FOnTimer := Value;
    UpdateTimer;
  end
  else
    FOnTimer := Value;
end;

procedure TJvTimer.Timer;
begin
  if FEnabled and not (csDestroying in ComponentState) and Assigned(FOnTimer) then
    FOnTimer(Self);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

