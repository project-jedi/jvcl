{-----------------------------------------------------------------------------

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThreadTimer.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Thörnqvist
Ivo Bauer

Last Modified: 2003-07-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

History:
  2003-07-24 (p3)
    * Changed Active->Enabled and Delay-> Interval to make property names match TTimer
    * Changed implementation so that setting Enabled := false, fress the thread instead
      of suspending it. This makes it possible to restart the timer interval.
  2003-07-25 (ivobauer)
    * Rewritten almost everything.

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvThreadTimer;

interface

uses
  Windows, SysUtils, Classes,
  JvTypes, JvComponent;

type
  TJvThreadTimer = class(TJvComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FPriority: TThreadPriority;
    FStreamedEnabled: Boolean;
    FThread: TThread;
    function GetThread: TThread;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetPriority(const Value: TThreadPriority);
  protected
    procedure Loaded; override;
    procedure UpdateTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Thread: TThread read GetThread;
  published
    // (p3) renamed Active->Enabled, Delay->Interval to make it compatible with TTimer
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
    property Priority: TThreadPriority read FPriority write SetPriority default tpNormal;
  end;

implementation

uses
  Messages {$IFNDEF COMPILER6_UP} , Forms {$ENDIF COMPILER6_UP} ;

type
  TJvTimerThread = class(TThread)
  private
    FEvent:THandle;
    FException: Exception;
    FExceptionAddress: Pointer;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FTimer: TJvThreadTimer;
    procedure DoHandleException;
    procedure HandleException;
  protected
    procedure DoTimer;
    procedure Execute; override;
  public
    constructor Create(ATimer: TJvThreadTimer);
    destructor Destroy; override;
    property Interval:Cardinal read FInterval;
    property OnTimer: TNotifyEvent read FOnTimer;
    property Timer: TJvThreadTimer read FTimer;
  end;

//=== TJvTimerThread ====================================================

constructor TJvTimerThread.Create(ATimer: TJvThreadTimer);
begin
  inherited Create(true);
  FEvent := CreateEvent(nil,false,false,nil);
  if FEvent = 0 then
    {$IFDEF COMPILER6_UP} RaiseLastOSError {$ELSE} RaiseLastWin32Error {$ENDIF COMPILER6_UP} ;
  FInterval := ATimer.FInterval;
  FOnTimer := ATimer.FOnTimer;
  FTimer := ATimer;
  Priority := ATimer.Priority;
  Resume;
end;

destructor TJvTimerThread.Destroy;
begin
  Terminate;
  SetEvent(FEvent);
  sleep(0);
  inherited;
  if FEvent <> 0 then
    CloseHandle(FEvent);
end;

procedure TJvTimerThread.DoHandleException;
begin
  if GetCapture <> 0 then
    SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  if FException is Exception then
  begin
    {$IFDEF COMPILER6_UP}
    if Assigned(ApplicationShowException) then
      ApplicationShowException(FException);
    {$ELSE}
    Application.ShowException(FException);
    {$ENDIF COMPILER6_UP}
  end
  else
    SysUtils.ShowException(FException, FExceptionAddress);
end;

procedure TJvTimerThread.DoTimer;
begin
  if not (csDestroying in FTimer.ComponentState) then
    FOnTimer(FTimer);
end;

procedure TJvTimerThread.Execute;
var
  Offset, TickCount: Cardinal;
begin
  Offset := 0;
  while not Terminated do
  begin
    // A simple runtime interval correction technique takes the place here.
    if (WaitForSingleObject(FEvent, FInterval - Offset) = WAIT_TIMEOUT) and not Terminated then
    begin
      TickCount := GetTickCount;

      try
        Synchronize(DoTimer);
      except
        HandleException;
      end;

      // Determine how much time it took to execute OnTimer event handler. Take a care
      // of wrapping the value returned by GetTickCount API around zero if Windows is
      // run continuously for more than 49.7 days.
      Offset := GetTickCount;
      if Offset >= TickCount then
        Dec(Offset, TickCount)
      else
        Inc(Offset, High(Cardinal) - TickCount);

      // Make sure Offset is less than or equal to FInterval.
      if Offset > FInterval then
        Offset := FInterval;
    end;
  end;
end;

procedure TJvTimerThread.HandleException;
begin
  FException := Exception(ExceptObject);
  FExceptionAddress := ExceptAddr;
  try
    // Ignore all silent exceptions.
    if not (FException is EAbort) then Synchronize(DoHandleException);
  finally
    FException := nil;
    FExceptionAddress := nil;
  end;
end;

//=== TJvThreadTimer =========================================================

constructor TJvThreadTimer.Create(AOwner: TComponent);
begin
  inherited;
  FInterval := 1000;
  FPriority := tpNormal;
end;

destructor TJvThreadTimer.Destroy;
begin
  SetEnabled(False);
  inherited;
end;

function TJvThreadTimer.GetThread: TThread;
begin
  Result := FThread;
end;

procedure TJvThreadTimer.Loaded;
begin
  inherited;
  SetEnabled(FStreamedEnabled);
end;

procedure TJvThreadTimer.SetEnabled(const Value: Boolean);
begin
  if csReading in ComponentState then
    FStreamedEnabled := Value
  else
    if FEnabled <> Value then
    begin
      FEnabled := Value;
      if not (csDesigning in ComponentState) then
        UpdateTimer;
    end;
end;

procedure TJvThreadTimer.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    if ComponentState * [csDesigning, csReading] = [] then
      UpdateTimer;
  end;
end;

procedure TJvThreadTimer.SetOnTimer(const Value: TNotifyEvent);
begin
  if @FOnTimer <> @Value then
  begin
    FOnTimer := Value;
    if ComponentState * [csDesigning, csReading] = [] then
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

procedure TJvThreadTimer.UpdateTimer;
begin
  if FEnabled and Assigned(FOnTimer) and (FInterval > 0) then
    FThread := TJvTimerThread.Create(Self)
  else
    FreeAndNil(FThread);
end;

end.
