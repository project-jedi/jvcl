{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MTThreading.pas, released on 2000-09-22.

The Initial Developer of the Original Code is Erwin Molendijk.
Portions created by Erwin Molendijk are Copyright (C) 2002 Erwin Molendijk.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-09-29

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
unit MTThreading;

interface

uses
  SyncObjs, SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Contnrs, MTConst, MTSync;

type
  TMTManager = class;
  TMTThread = class;

  TMTEvent = procedure (Thread: TMTThread) of object;
  TMTInternalThread = class (TThread)
  private
    FName: string;
    FOnExecute: TNotifyEvent;
  protected
    procedure Execute; override;
    procedure RaiseName;
  public
    property Name: string read FName write FName;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;
  
  TMTThread = class (TObject)
  private
    FFinished: Boolean;
    FIntThread: TMTInternalThread;
    FManager: TMTManager;
    FName: string;
    FOnExecute: TMTEvent;
    FOnFinished: TMTEvent;
    FOnTerminating: TMTEvent;
    FReferenceCount: Integer;
    FStatusChange: TCriticalSection;
    FTerminateSignal: THandle;
    FTicket: TMTTicket;
    procedure CreateAndRun;
    function GetStatus: TMTThreadStatus;
    procedure Log(Msg: string);
    procedure OnIntThreadExecute(Sender: TObject);
    procedure OnIntThreadTerminate(Sender: TObject);
    procedure SetName(const Value: string);
  protected
    procedure DecRef;
    procedure IncRef;
  public
    constructor Create(Manager: TMTManager; Ticket: Integer);
    destructor Destroy; override;
    procedure CheckTerminate;
    procedure Release;
    procedure Run;
    procedure Synchronize(Method: TThreadMethod);
    procedure Terminate;
    procedure Wait;
    property Name: string read FName write SetName;
    property OnExecute: TMTEvent read FOnExecute write FOnExecute;
    property OnFinished: TMTEvent read FOnFinished write FOnFinished;
    property OnTerminating: TMTEvent read FOnTerminating write FOnTerminating;
    property ReferenceCount: Integer read FReferenceCount;
    property Status: TMTThreadStatus read GetStatus;
    property TerminateSignal: THandle read FTerminateSignal;
    property ThreadManager: TMTManager read FManager;
    property Ticket: TMTTicket read FTicket;
  end;
  
  TMTManager = class (TObject)
  private
    FGenTicket: TCriticalSection;
    FNextTicket: TMTTicket;
    FThreads: TObjectList;
    FThreadsChange: TCriticalSection;
    function FindThread(Ticket: TMTTicket; var Thread: TMTThread): Boolean;
    function GenerateTicket: TMTTicket;
    procedure Log(Msg: string);
    procedure TryRemoveThread(Thread: TMTThread);
  protected
    procedure OnThreadFinished(Thread: TMTThread);
  public
    constructor Create;
    destructor Destroy; override;
    function AcquireNewThread: TMTThread;
    function AcquireThread(Ticket: TMTTicket; var Thread: TMTThread): Boolean;
    function ActiveThreads: Boolean;
    procedure ReleaseThread(Ticket: TMTTicket);
    procedure TerminateThreads;
    procedure WaitThreads;
  end;
  

function  CurrentMTThread: TMTThread;

implementation

threadvar
  _CurrentMTThread: TMTThread;

function CurrentMTThread: TMTThread;
begin
  Result := _CurrentMTThread;
end;


{$IFDEF DELPHI5}
{$MESSAGE Error 'Delphi 5 is not fully supported (yet).'}
procedure CheckSynchronize;
begin
  { BUG: Delphi 5 Synchronize() recursion problem...

    Need to make the Delphi 5 synchronize scheme work the same as
    Delphi 6+. Because calling Application.ProcessMessages() is a bad idea.
  }
  if (CurrentMTThread = nil) then
    Application.ProcessMessages;
end;
{$ENDIF}

{ TMTInternalThread }

procedure TMTInternalThread.Execute;
begin
  RaiseName;
  if Assigned(FOnExecute) then FOnExecute(Self);
end;

procedure TMTInternalThread.RaiseName;

  {$IFDEF DELPHI7_UP}
  var
    ThreadNameInfo: TThreadNameInfo;
  {$ENDIF}

begin
  {$IFDEF DELPHI7_UP}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PChar(FName);
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;
  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord),
      @ThreadNameInfo );
  except
  end;
  {$ENDIF}
end;

{ TMTThread }

constructor TMTThread.Create(Manager: TMTManager; Ticket: Integer);
begin
  inherited Create;
  FStatusChange := TCriticalSection.Create;
  FManager := Manager;
  FTicket := Ticket;
  FName := 'MT'+IntToStr(Ticket);
  FTerminateSignal := CreateSemaphore(nil, 0, 1, '');
end;

destructor TMTThread.Destroy;
begin
  CloseHandle(FTerminateSignal);
  FStatusChange.Free;
  inherited Destroy;
end;

procedure TMTThread.CheckTerminate;
begin
  if CurrentMTThread <> Self then
    raise EMTThread.Create('CheckTerminate can only be called by the same thread');
  
  if GetStatus = tsTerminating then
    raise EMTTerminate.Create('');
end;

procedure TMTThread.CreateAndRun;
begin
  FStatusChange.Acquire;
  try
    FIntThread := TMTInternalThread.Create(True);
    FIntThread.OnExecute := OnIntThreadExecute;
    FIntThread.OnTerminate := OnIntThreadTerminate;
    FIntThread.FreeOnTerminate := True;
    FIntThread.Name := FName;
    FIntThread.Resume;
  finally
    FStatusChange.Release;
  end;
end;

procedure TMTThread.DecRef;
begin
  InterlockedDecrement(FReferenceCount);
end;

function TMTThread.GetStatus: TMTThreadStatus;
begin
  FStatusChange.Acquire;
  try
    if FFinished then Result := tsFinished
    else if FIntThread = nil then Result := tsInitializing
    else if FIntThread.Suspended then Result := tsWaiting
    else if FIntThread.Terminated then Result := tsTerminating
    else Result := tsRunning;
  finally
    FStatusChange.Release;
  end;
end;

procedure TMTThread.IncRef;
begin
  InterlockedIncrement(FReferenceCount);
end;

procedure TMTThread.Log(Msg: string);
begin
  OutputDebugString(PChar('['+ClassName+'] '+Msg));
end;

procedure TMTThread.OnIntThreadExecute(Sender: TObject);
begin
  // set the CurrentMTThread variable.
  //  this variable is global, but only to this thread.
  _CurrentMTThread := Self;
  
  // run OnExecute event
  try
    if Assigned(FOnExecute) then FOnExecute(Self);
  except
    on E: EMTTerminate do {nothing};
    on E: Exception do Log('OnExecute Exception: "'+E.Message+'"');
  end;
  
  // make sure terminate flag is set
  FIntThread.Terminate;
  
  // run OnTerminating event
  try
    if Assigned(FOnTerminating) then FOnTerminating(Self);
  except
    on E: Exception do Log('OnTerminate Exception: "'+E.Message+'"');
  end;
end;

procedure TMTThread.OnIntThreadTerminate(Sender: TObject);
begin
  FStatusChange.Acquire;
  try
    FFinished := True;
  finally
    FStatusChange.Release;
  end;
  
  if Assigned(FOnFinished) then FOnFinished(Self);
  
  FStatusChange.Acquire;
  try
    FIntThread := nil;
  finally
    FStatusChange.Release;
  end;
  
  // After a call to OnThreadFinished, this object might be destroyed.
  // So don't access any fields after this call.
  FManager.OnThreadFinished(Self);
end;

procedure TMTThread.Release;
begin
  FManager.ReleaseThread(FTicket);
end;

procedure TMTThread.Run;
begin
  FStatusChange.Acquire;
  try
    if GetStatus = tsInitializing then
      CreateAndRun
    else if GetStatus = tsWaiting then
      FIntThread.Resume
    else
      raise EMTThread.Create('Can''t run: thread is not Initializing or Waiting.');
  finally
    FStatusChange.Release;
  end;
end;

procedure TMTThread.SetName(const Value: string);
begin
  FStatusChange.Acquire;
  try
  
    if GetStatus in [tsInitializing, tsFinished] then
      FName := Value
    else
    begin
      if CurrentMTThread <> Self then
        raise EMTThread.Create('Can not change name of other active thread.');
  
      FName := Value;
      if FIntThread <> nil then
      begin
        FIntThread.Name := FName;
        FIntThread.RaiseName;
      end;
    end;
  finally
    FStatusChange.Release;
  end;
end;

procedure TMTThread.Synchronize(Method: TThreadMethod);
begin
  if CurrentMTThread = Self then
    FIntThread.Synchronize(Method)
  else if CurrentMTThread = nil then
    Method
  else
    CurrentMTThread.Synchronize(Method);
end;

procedure TMTThread.Terminate;
begin
  if (GetStatus in [tsTerminating,tsFinished]) then exit;
  
  FStatusChange.Acquire;
  try
    if FIntThread <> nil then
      FIntThread.Terminate  {thread was Running}
    else
      FFinished := True;    {thread was initializing}
  
    // make sure thread escapes from any Wait() calls
    ReleaseSemaphore(FTerminateSignal, 1, nil);
  finally
    FStatusChange.Release;
  end;
end;

procedure TMTThread.Wait;
var
  SelfRef: TMTThread;
begin
  if FManager.AcquireThread(Ticket, SelfRef) then
  try
    if GetCurrentThreadId = MainThreadID then
    begin
      while GetStatus <> tsFinished do
      begin
        CheckSynchronize;
        Sleep(0);
      end;
    end
    else begin
      while GetStatus <> tsFinished do
        Sleep(0);
    end;
  finally
    Release;
  end;
end;

{ TMTManager }

constructor TMTManager.Create;
begin
  inherited Create;
  FGenTicket := TCriticalSection.Create;
  FThreadsChange := TCriticalSection.Create;
  FThreads := TObjectList.Create(True);
end;

destructor TMTManager.Destroy;
var
  I: Integer;
begin
  // set the terminate flag at each thread
  TerminateThreads;
  // wait for them to finish
  WaitThreads;
  
  FThreadsChange.Acquire;
  try
    for I := 0 to FThreads.Count-1 do
      Log('Unreleased thread: "'+TMTThread(FThreads[I]).Name+'"');
  finally
    FThreadsChange.Release;
  end;
  
  FThreads.Free;
  FThreadsChange.Free;
  FGenTicket.Free;
  inherited Destroy;
end;

function TMTManager.AcquireNewThread: TMTThread;
begin
  Result := TMTThread.Create(self, GenerateTicket);
  try
    Result.IncRef;
    FThreadsChange.Acquire;
    try
      FThreads.Add(Result);
    finally
      FThreadsChange.Release;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TMTManager.AcquireThread(Ticket: TMTTicket; var Thread: TMTThread): 
  Boolean;
begin
  FThreadsChange.Acquire;
  try
    Result := FindThread(Ticket, Thread);
    if Result then
      Thread.IncRef;
  finally
    FThreadsChange.Release;
  end;
end;

function TMTManager.ActiveThreads: Boolean;
var
  I: Integer;
begin
  Result := False;
  FThreadsChange.Acquire;
  try
    for I := 0 to FThreads.Count-1 do
     if TMTThread(FThreads[I]).Status <> tsFinished then
       Result := True;
  finally
    FThreadsChange.Release;
  end;
end;

function TMTManager.FindThread(Ticket: TMTTicket; var Thread: TMTThread): 
  Boolean;
var
  I: Integer;
begin
  FThreadsChange.Acquire;
  try
    I := FThreads.Count-1;
    while (I <> -1) and (TMTThread(FThreads[I]).Ticket <> Ticket) do
      Dec(I);
  
    Result := I <> -1;
    if Result then
      Thread := TMTThread(FThreads[I])
    else
      Thread := nil;
  
  finally
    FThreadsChange.Release;
  end;
end;

function TMTManager.GenerateTicket: TMTTicket;
begin
  FGenTicket.Acquire;
  try
    Result := FNextTicket;
    Inc(FNextTicket);
  finally
    FGenTicket.Release;
  end;
end;

procedure TMTManager.Log(Msg: string);
begin
  OutputDebugString(PChar('['+ClassName+'] '+Msg));
end;

procedure TMTManager.OnThreadFinished(Thread: TMTThread);
begin
  TryRemoveThread(Thread);
end;

procedure TMTManager.ReleaseThread(Ticket: TMTTicket);
var
  Thread: TMTThread;
begin
  FThreadsChange.Acquire;
  try
    if FindThread(Ticket, Thread) then
      Thread.DecRef
    else
      raise EMTThread.Create('Release of unused ticket');
  
    // if this was the last reference then the thread must be removed
    TryRemoveThread(Thread);
  finally
    FThreadsChange.Release;
  end;
end;

procedure TMTManager.TerminateThreads;
var
  I: Integer;
begin
  FThreadsChange.Acquire;
  try
    for I := 0 to FThreads.Count-1 do
      TMTThread(FThreads[I]).Terminate;
  finally
    FThreadsChange.Release;
  end;
end;

procedure TMTManager.TryRemoveThread(Thread: TMTThread);
begin
  FThreadsChange.Acquire;
  try
    if (Thread.Status = tsFinished) and (Thread.ReferenceCount = 0) then
      FThreads.Remove(Thread);
  finally
    FThreadsChange.Release;
  end;
end;

procedure TMTManager.WaitThreads;
  
  // wait until the threads are all finished
  
begin
  // running from inside the main VCL thread?
  if GetCurrentThreadID = MainThreadID then
  begin
    //  use CheckSynchronise to process the OnFinished events
    while ActiveThreads do
    begin
      CheckSynchronize;
      Sleep(0);
    end;
  end
  else begin
    {TODO: If current thread is part of this manager then deadlock
           will occur. Detect and raise exception.}
  
    //running in a MTThread, just wait for all threads to finish
    while ActiveThreads do
        Sleep(0);
  end;
end;

end.
