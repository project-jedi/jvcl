{-----------------------------------------------------------------------------

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThreadTimer.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Thörnqvist
Ivo Bauer

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

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

{$I jvcl.inc}

unit JvThreadTimer;

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
  TJvThreadTimer = class(TJvComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FKeepAlive: Boolean;
    FOnTimer: TNotifyEvent;
    FPriority: TThreadPriority;
    FStreamedEnabled: Boolean;
    FThread: TThread;
    function GetThread: TThread;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetPriority(const Value: TThreadPriority);
    procedure SetKeepAlive(const Value: Boolean);
  protected
    procedure DoOnTimer;
    procedure Loaded; override;
    procedure StopTimer;
    procedure UpdateTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Thread: TThread read GetThread;
  published
    // (p3) renamed Active->Enabled, Delay->Interval to make it compatible with TTimer
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive default False;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
    property Priority: TThreadPriority read FPriority write SetPriority{$IFDEF MSWINDOWS} default tpNormal{$ENDIF};
  end;

implementation

{$IFDEF VCL}
uses
  {$IFNDEF COMPILER6_UP}
  Forms,
  {$ENDIF COMPILER6_UP}
  Messages;
{$ENDIF VCL}

type
  TJvTimerThread = class(TThread)
  private
    FEvent: THandle;
    FHasBeenSuspended: Boolean;
    FInterval: Cardinal;
    FTimer: TJvThreadTimer;
  protected
    procedure DoSuspend;
    procedure Execute; override;
  public
    constructor Create(ATimer: TJvThreadTimer);
    destructor Destroy; override;
    procedure Stop;
    property Interval: Cardinal read FInterval;
    property Timer: TJvThreadTimer read FTimer;
  end;

function SubtractMin0(const Big, Small: Cardinal): Cardinal;
begin
  if Big <= Small then
    Result := 0
  else
    Result := Big - Small;
end;

//=== TJvTimerThread =========================================================

constructor TJvTimerThread.Create(ATimer: TJvThreadTimer);
begin
  { Create suspended because of priority setting }
  inherited Create(True);

  FreeOnTerminate := True;
  { Manually reset = false; Initial State = false }
  FEvent := CreateEvent(nil, False, False, nil);
  if FEvent = 0 then
    {$IFDEF COMPILER6_UP}
    RaiseLastOSError;
    {$ELSE}
    RaiseLastWin32Error;
    {$ENDIF COMPILER6_UP}
  FInterval := ATimer.FInterval;
  FTimer := ATimer;
  Priority := ATimer.Priority;
  Resume;
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
  Suspend;
end;

procedure TJvTimerThread.Execute;
var
  Offset, TickCount: Cardinal;
begin
  if WaitForSingleObject(FEvent, Interval) <> WAIT_TIMEOUT then
    Exit;

  while not Terminated do
  begin
    FHasBeenSuspended := False;

    TickCount := GetTickCount;
    if not Terminated then
      Synchronize(FTimer.DoOnTimer);

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
    if WaitForSingleObject(FEvent, SubtractMin0(Interval, Offset)) <> WAIT_TIMEOUT then
      Exit;
  end;
end;

procedure TJvTimerThread.Stop;
begin
  Terminate;
  SetEvent(FEvent);
  if Suspended then
    Resume;
  Sleep(0);
end;

//=== TJvThreadTimer =========================================================

constructor TJvThreadTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := 1000;
  {$IFDEF MSWINDOWS}
  FPriority := tpNormal;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  FPriority := 0;
  {$ENDIF}
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
    {$IFDEF COMPILER6_UP}
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
    {$ELSE}
    Application.HandleException(Self);
    {$ENDIF COMPILER6_UP}
  end;
end;

function TJvThreadTimer.GetThread: TThread;
begin
  Result := FThread;
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
  if Value <> KeepAlive then
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

procedure TJvThreadTimer.SetPriority(const Value: TThreadPriority);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    if FThread <> nil then
      FThread.Priority := FPriority;
  end;
end;

procedure TJvThreadTimer.StopTimer;
begin
  if FThread is TJvTimerThread then
    TJvTimerThread(FThread).Stop;
  FThread := nil;
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
    if FThread is TJvTimerThread then
      TJvTimerThread(FThread).FInterval := FInterval
    else
      FThread := TJvTimerThread.Create(Self);

    if FThread.Suspended then
      FThread.Resume;
  end
  else
  if FThread is TJvTimerThread then
  begin
    if not FThread.Suspended then
      TJvTimerThread(FThread).DoSuspend;

    TJvTimerThread(FThread).FInterval := FInterval;
  end;
end;

end.

