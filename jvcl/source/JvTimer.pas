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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTimer;

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ELSE}
  WinTypes, WinProcs,
  {$ENDIF}
  Messages, SysUtils, Classes {, JvComponent};

type
  TJvTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FWindowHandle: HWND;
    {$IFDEF WIN32}
    FSyncEvent: Boolean;
    FThreaded: Boolean;
    FTimerThread: TThread;
    FThreadPriority: TThreadPriority;
    procedure SetThreaded(Value: Boolean);
    procedure SetThreadPriority(Value: TThreadPriority);
    {$ENDIF}
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure UpdateTimer;
    procedure WndProc(var Msg: TMessage);
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF WIN32}
    procedure Synchronize(Method: TThreadMethod);
    {$ENDIF}
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    {$IFDEF WIN32}
    property SyncEvent: Boolean read FSyncEvent write FSyncEvent default True;
    property Threaded: Boolean read FThreaded write SetThreaded default True;
    property ThreadPriority: TThreadPriority read FThreadPriority write
      SetThreadPriority default tpNormal;
    {$ENDIF}
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

implementation

uses
  Forms, Consts,
  JvVCLUtils;

{$IFDEF WIN32}

//=== TJvTimerThread =========================================================

type
  TJvTimerThread = class(TThread)
  private
    FOwner: TJvTimer;
    FInterval: Cardinal;
    FException: Exception;
    procedure HandleException;
  protected
    procedure Execute; override;
  public
    constructor Create(Timer: TJvTimer; Enabled: Boolean);
  end;

constructor TJvTimerThread.Create(Timer: TJvTimer; Enabled: Boolean);
begin
  FOwner := Timer;
  inherited Create(not Enabled);
  FInterval := 1000;
  FreeOnTerminate := True;
end;

procedure TJvTimerThread.HandleException;
begin
  if not (FException is EAbort) then
  begin
    if Assigned(Application.OnException) then
      Application.OnException(Self, FException)
    else
      Application.ShowException(FException);
  end;
end;

procedure TJvTimerThread.Execute;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwner = nil);
  end;

begin
  repeat
    if not ThreadClosed then
      if SleepEx(FInterval, False) = 0 then
        if not ThreadClosed and FOwner.FEnabled then
          with FOwner do
            if SyncEvent then
              Synchronize(Timer)
            else
              try
                Timer;
              except
                on E: Exception do
                begin
                  FException := E;
                  HandleException;
                end;
              end;
  until Terminated;
end;

{$ENDIF}

//=== TJvTimer ===============================================================

constructor TJvTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;
  {$IFDEF WIN32}
  FSyncEvent := True;
  FThreaded := True;
  FThreadPriority := tpNormal;
  FTimerThread := TJvTimerThread.Create(Self, False);
  {$ELSE}
  {$IFDEF COMPILER6_UP}
  FWindowHandle := Classes.AllocateHWnd(WndProc)
  {$ELSE}
  FWindowHandle := AllocateHWnd(WndProc);
  {$ENDIF}
  {$ENDIF}
end;

destructor TJvTimer.Destroy;
begin
  Destroying;
  FEnabled := False;
  FOnTimer := nil;
  {$IFDEF WIN32}
  {TTimerThread(FTimerThread).FOwner := nil;}
  while FTimerThread.Suspended do
    FTimerThread.Resume;
  FTimerThread.Terminate;
  {if not SyncEvent then FTimerThread.WaitFor;}
  if FWindowHandle <> 0 then
  begin
    {$ENDIF}
    KillTimer(FWindowHandle, 1);
    {$IFDEF COMPILER6_UP}
    Classes.DeallocateHWnd(FWindowHandle);
    {$ELSE}
    DeallocateHWnd(FWindowHandle);
    {$ENDIF}
    {$IFDEF WIN32}
  end;
  {$ENDIF}
  inherited Destroy;
end;

procedure TJvTimer.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_TIMER then
    try
      Timer;
    except
      Application.HandleException(Self);
    end
    else
      Result := DefWindowProc(FWindowHandle, Msg, WParam, LParam);
end;

procedure TJvTimer.UpdateTimer;
begin
  {$IFDEF WIN32}
  if FThreaded then
  begin
    if FWindowHandle <> 0 then
    begin
      KillTimer(FWindowHandle, 1);
      {$IFDEF COMPILER6_UP}
      Classes.DeallocateHWnd(FWindowHandle);
      {$ELSE}
      DeallocateHWnd(FWindowHandle);
      {$ENDIF}
      FWindowHandle := 0;
    end;
    if not FTimerThread.Suspended then
      FTimerThread.Suspend;
    TJvTimerThread(FTimerThread).FInterval := FInterval;
    if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    begin
      FTimerThread.Priority := FThreadPriority;
      while FTimerThread.Suspended do
        FTimerThread.Resume;
    end;
  end
  else
  begin
    if not FTimerThread.Suspended then
      FTimerThread.Suspend;
    if FWindowHandle = 0 then
      {$IFDEF COMPILER6_UP}
      FWindowHandle := Classes.AllocateHWnd(WndProc)
      {$ELSE}
      FWindowHandle := AllocateHWnd(WndProc)
      {$ENDIF}
    else
      KillTimer(FWindowHandle, 1);
    if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
      if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
        raise EOutOfResources.Create(ResStr(SNoTimers));
  end;
  {$ELSE}
  KillTimer(FWindowHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create(ResStr(SNoTimers));
  {$ENDIF}
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

{$IFDEF WIN32}

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

{$ENDIF}

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
  if FEnabled and not (csDestroying in ComponentState) and
    Assigned(FOnTimer) then
    FOnTimer(Self);
end;

end.

