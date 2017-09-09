{-----------------------------------------------------------------------------

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThreadTimer.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S?stien Buysse [sbuysse att buypin dott com]
Portions created by S?stien Buysse are Copyright (C) 2001 S?stien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Thrnqvist
Ivo Bauer

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

History:
  2003-07-24 (p3)
    * Changed Active->Enabled and Delay->Interval to make property names match TTimer
    * Changed implementation so that setting Enabled := false, frees the thread instead
      of suspending it. This makes it possible to restart the timer interval.
  2003-07-25 (ivobauer)
    * Rewritten almost everything.

-----------------------------------------------------------------------------}
// $Id$

unit JvThreadTimer;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes,
  JvTypes, JvComponentBase;

type
  // TThreadPriority has been marked platform and we don't want the warning
  {$IFDEF RTL230_UP}{$IFDEF MSWINDOWS}{$WARNINGS OFF}TThreadPriority = Classes.TThreadPriority;{$WARNINGS ON}{$ENDIF RTL230_UP}{$ENDIF MSWINDOWS}

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvThreadTimer = class(TJvComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FKeepAlive: Boolean;
    FOnTimer: TNotifyEvent;
    {$IFDEF MSWINDOWS}
    FPriority: TThreadPriority;
    {$ENDIF MSWINDOWS}
    FStreamedEnabled: Boolean;
    FThread: TThread;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnTimer(const Value: TNotifyEvent);
    {$IFDEF MSWINDOWS}
    procedure SetPriority(const Value: TThreadPriority);
    {$ENDIF MSWINDOWS}
    procedure SetKeepAlive(const Value: Boolean);
  protected
    procedure DoOnTimer;
    procedure Loaded; override;
    procedure StopTimer;
    procedure UpdateTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Thread: TThread read FThread;
  published
    // (p3) renamed Active->Enabled, Delay->Interval to make it compatible with TTimer
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive default False;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
    {$IFDEF MSWINDOWS}
    property Priority: TThreadPriority read FPriority write SetPriority default tpNormal;
    {$ENDIF MSWINDOWS}
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
  Messages;

type
  TJvTimerThread = class(TJvCustomThread)
  private
    FEvent: THandle;
    FHasBeenSuspended: Boolean;
    FInterval: Cardinal;
    FTimer: TJvThreadTimer;
    {$IFDEF MSWINDOWS}
    FPriority: TThreadPriority;
    {$ENDIF MSWINDOWS}
    FSynchronizing: Boolean;
  protected
    procedure DoSuspend;
    procedure Execute; override;
  public
    constructor Create(ATimer: TJvThreadTimer);
    destructor Destroy; override;
    procedure Stop;
    property Interval: Cardinal read FInterval;
    property Timer: TJvThreadTimer read FTimer;
    property Synchronizing: Boolean read FSynchronizing;
  end;

function SubtractMin0(const Big, Small: Cardinal): Cardinal;
begin
  if Big <= Small then
    Result := 0
  else
    Result := Big - Small;
end;

//=== { TJvTimerThread } =====================================================

constructor TJvTimerThread.Create(ATimer: TJvThreadTimer);
begin
  inherited Create(False);

  { Manually reset = false; Initial State = false }
  FEvent := CreateEvent(nil, False, False, nil);
  if FEvent = 0 then
    RaiseLastOSError;
  FInterval := ATimer.FInterval;
  FTimer := ATimer;
  {$IFDEF MSWINDOWS}
  FPriority := ATimer.Priority; // setting the priority is deferred to Execute()
  {$ENDIF MSWINDOWS}
  ThreadName := Format('%s: %s',[ClassName, ATimer.Name]);
end;

destructor TJvTimerThread.Destroy;
begin
  Stop;
  inherited Destroy;
  if FEvent <> 0 then
    CloseHandle(FEvent);
end;

procedure TJvTimerThread.DoSuspend;
begin
  FHasBeenSuspended := True;
  Suspended := True;
end;

procedure TJvTimerThread.Execute;
var
  Offset, TickCount: Cardinal;
begin
  NameThread(ThreadName);
  {$IFDEF MSWINDOWS}
  Priority := FPriority;
  {$ENDIF MSWINDOWS}
  if WaitForSingleObject(FEvent, Interval) <> WAIT_TIMEOUT then
    Exit;

  while not Terminated do
  begin
    FHasBeenSuspended := False;

    TickCount := GetTickCount;
    if not Terminated then
    begin
      FSynchronizing := True;
      try
        Synchronize(FTimer.DoOnTimer);
      finally
        FSynchronizing := False;
      end;
    end;

    // Determine how much time it took to execute OnTimer event handler. Take a care
    // of wrapping the value returned by GetTickCount API around zero if Windows is
    // run continuously for more than 49.7 days.
    if FHasBeenSuspended then
      Offset := 0
    else
    begin
      Offset := GetTickCount;
      if Offset >= TickCount then
        Dec(Offset, TickCount)
      else
        Inc(Offset, High(Cardinal) - TickCount);
    end;

    // Make sure Offset is less than or equal to FInterval.
    // (rb) Ensure it's atomic, because of KeepAlive
    if Terminated or (WaitForSingleObject(FEvent, SubtractMin0(Interval, Offset)) <> WAIT_TIMEOUT) then
      Exit;
  end;
end;

procedure TJvTimerThread.Stop;
begin
  Terminate;
  SetEvent(FEvent);
  if Suspended then
    Suspended := False;
end;

//=== { TJvThreadTimer } =====================================================

constructor TJvThreadTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := 1000;
  {$IFDEF MSWINDOWS}
  FPriority := tpNormal;
  {$ENDIF MSWINDOWS}
end;

destructor TJvThreadTimer.Destroy;
begin
  StopTimer;
  inherited Destroy;
end;

procedure TJvThreadTimer.DoOnTimer;
begin
  if csDestroying in ComponentState then
    Exit;

  try
    if Assigned(FOnTimer) then
      FOnTimer(Self);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;

procedure TJvThreadTimer.Loaded;
begin
  inherited Loaded;
  SetEnabled(FStreamedEnabled);
end;

procedure TJvThreadTimer.SetEnabled(const Value: Boolean);
begin
  if csLoading in ComponentState then
    FStreamedEnabled := Value
  else
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TJvThreadTimer.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TJvThreadTimer.SetKeepAlive(const Value: Boolean);
begin
  if FKeepAlive <> Value then
  begin
    StopTimer;
    FKeepAlive := Value;
    UpdateTimer;
  end;
end;

procedure TJvThreadTimer.SetOnTimer(const Value: TNotifyEvent);
begin
  if @FOnTimer <> @Value then
  begin
    FOnTimer := Value;
    UpdateTimer;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TJvThreadTimer.SetPriority(const Value: TThreadPriority);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    if FThread <> nil then
      FThread.Priority := FPriority;
  end;
end;
{$ENDIF MSWINDOWS}

procedure TJvThreadTimer.StopTimer;
begin
  if FThread <> nil then
  begin
    TJvTimerThread(FThread).Stop;
    if not TJvTimerThread(FThread).Synchronizing then
      FreeAndNil(FThread)
    else
    begin
      // We can't destroy the thread because it called us through Synchronize()
      // and is waiting for our return. But we need to destroy it after it returned.
      TJvTimerThread(FThread).FreeOnTerminate := True;
      FThread := nil
    end;
  end;
end;

procedure TJvThreadTimer.UpdateTimer;
var
  DoEnable: Boolean;
begin
  if ComponentState * [csDesigning, csLoading] <> [] then
    Exit;

  DoEnable := FEnabled and Assigned(FOnTimer) and (FInterval > 0);

  if not KeepAlive then
    StopTimer;

  if DoEnable then
  begin
    if FThread <> nil then
    begin
      TJvTimerThread(FThread).FInterval := FInterval;
      if FThread.Suspended then
        FThread.Suspended := False;
    end
    else
      FThread := TJvTimerThread.Create(Self);
  end
  else
  if FThread <> nil then
  begin
    if not FThread.Suspended then
      TJvTimerThread(FThread).DoSuspend;

    TJvTimerThread(FThread).FInterval := FInterval;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
