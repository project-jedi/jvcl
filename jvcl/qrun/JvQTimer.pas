{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit JvQTimer;

{$I jvcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes;

type
  TJvTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FSyncEvent: Boolean;
    FThreaded: Boolean;
    FTimerThread: TThread;
    {$IFDEF MSWINDOWS}
    FThreadPriority: TThreadPriority;
    procedure SetThreadPriority(Value: TThreadPriority);
    {$ENDIF MSWINDOWS}
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
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property SyncEvent: Boolean read FSyncEvent write FSyncEvent default True;
    property Threaded: Boolean read FThreaded write SetThreaded default True;
    {$IFDEF MSWINDOWS}
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority default tpNormal;
    {$ENDIF MSWINDOWS}
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}  
  QForms, QConsts, 
  JvQJVCLUtils;

//=== { TJvTimerThread } =====================================================

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
    Application.HandleException(Self);
end;

procedure TJvTimerThread.Execute;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwner = nil);
  end;

  {$IFDEF UNIX}
  function SleepEx(Ms: Cardinal; Alertable: Boolean): Cardinal;
  begin
    Sleep(Ms);
    Result := 0;
  end;
  {$ENDIF UNIX}

begin
  repeat
    if (not ThreadClosed) and (SleepEx(FInterval, False) = 0) and
      (not ThreadClosed) and FOwner.FEnabled then
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

//=== { TJvTimer } ===========================================================

constructor TJvTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;
  FSyncEvent := True;
  FThreaded := True;
  {$IFDEF MSWINDOWS}
  FThreadPriority := tpNormal;
  {$ENDIF MSWINDOWS}
  FTimerThread := TJvTimerThread.Create(Self, False);
end;

destructor TJvTimer.Destroy;
begin
  Destroying;
  FEnabled := False;
  FOnTimer := nil;
  {TTimerThread(FTimerThread).FOwner := nil;}
  while FTimerThread.Suspended do
    FTimerThread.Resume;
  FTimerThread.Terminate;
  inherited Destroy;
end;

procedure TJvTimer.UpdateTimer;
begin
  if FThreaded then
  begin
    if not FTimerThread.Suspended then
      FTimerThread.Suspend;
    TJvTimerThread(FTimerThread).FInterval := FInterval;
    if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    begin
      {$IFDEF MSWINDOWS}
      FTimerThread.Priority := FThreadPriority;
      {$ENDIF MSWINDOWS}
      while FTimerThread.Suspended do
        FTimerThread.Resume;
    end;
  end
  else
  begin
    if not FTimerThread.Suspended then
      FTimerThread.Suspend;
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

{$IFDEF MSWINDOWS}
procedure TJvTimer.SetThreadPriority(Value: TThreadPriority);
begin
  if Value <> FThreadPriority then
  begin
    FThreadPriority := Value;
    if FThreaded then
      UpdateTimer;
  end;
end;
{$ENDIF MSWINDOWS}

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
  if FEnabled and not (csDestroying in ComponentState) and
    Assigned(FOnTimer) then
    FOnTimer(Self);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

