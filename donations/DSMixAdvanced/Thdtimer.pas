////////////////////////////////////////////////////
//                                                //
//   ThreadedTimer 1.2a                           //
//                                                //
//   Copyright (C) 1996, 2000 Carlos Barbosa      //
//   email: delphi@carlosb.com                    //
//   Home Page: http://www.carlosb.com            //
//                                                //
//   Portions (C) 2000, Andrew N. Driazgov        //
//   email: andrey@asp.tstu.ru                    //
//                                                //
//   Last updated: November 24, 2000              //
//                                                //
////////////////////////////////////////////////////

unit ThdTimer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
  DEFAULT_INTERVAL = 1000;

type
  TThreadedTimer = class;

  TTimerThread = class(TThread)
  private
    FOwner: TThreadedTimer;
    FInterval: Cardinal;
    FStop: THandle;
  protected
    procedure Execute; override;
  end;

  TThreadedTimer = class(TComponent)
  private
    FOnTimer: TNotifyEvent;
    FTimerThread: TTimerThread;
    FEnabled,
    FAllowZero: Boolean;

    procedure DoTimer;

    procedure SetEnabled(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    function GetThreadPriority: TThreadPriority;
    procedure SetThreadPriority(Value: TThreadPriority);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property AllowZero: Boolean read FAllowZero write FAllowZero default False;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read GetInterval write SetInterval default DEFAULT_INTERVAL;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property ThreadPriority: TThreadPriority read GetThreadPriority  write SetThreadPriority default tpNormal;
  end;

procedure Register;

implementation

{ TTimerThread }

procedure TTimerThread.Execute;
begin
  repeat
    if WaitForSingleObject(FStop, FInterval) = WAIT_TIMEOUT then
      Synchronize(FOwner.DoTimer);
  until Terminated;
end;

{ TThreadedTimer }

constructor TThreadedTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimerThread := TTimerThread.Create(True);
  with FTimerThread do
  begin
    FOwner := Self;
    FInterval := DEFAULT_INTERVAL;
    Priority := tpNormal;

    // Event is completely manipulated by TThreadedTimer object
    FStop := CreateEvent(nil, False, False, nil);
  end;
end;

destructor TThreadedTimer.Destroy;
begin
  with FTimerThread do
  begin
    Terminate;

    // When this method is called we must be confident that the event handle was not closed
    SetEvent(FStop);
    if Suspended then
      Resume;
    WaitFor;
    CloseHandle(FStop);  // Close event handle in the primary thread
    Free;
  end;
  inherited Destroy;
end;

procedure TThreadedTimer.DoTimer;
begin

  // We have to check FEnabled in the primary thread
  // Otherwise we get AV when the program is closed
  if FEnabled and Assigned(FOnTimer) then
    try
      FOnTimer(Self);
    except
    end;
end;

procedure TThreadedTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      if (FTimerThread.FInterval > 0) or
        ((FTimerThread.FInterval = 0) and FAllowZero) then
      begin
        SetEvent(FTimerThread.FStop);
        FTimerThread.Resume;
      end;
    end
    else
      FTimerThread.Suspend;
  end;
end;

function TThreadedTimer.GetInterval: Cardinal;
begin
  Result := FTimerThread.FInterval;
end;

procedure TThreadedTimer.SetInterval(Value: Cardinal);
var
  PrevEnabled: Boolean;
begin
  if Value <> FTimerThread.FInterval then
  begin
    PrevEnabled := FEnabled;
    Enabled := False;
    FTimerThread.FInterval := Value;
    Enabled := PrevEnabled;
  end;
end;

function TThreadedTimer.GetThreadPriority: TThreadPriority;
begin
  Result := FTimerThread.Priority;
end;

procedure TThreadedTimer.SetThreadPriority(Value: TThreadPriority);
begin
  FTimerThread.Priority := Value;
end;

procedure Register;
begin
   RegisterComponents('System', [TThreadedTimer]);
end;

end.

