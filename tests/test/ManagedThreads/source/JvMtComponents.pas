{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MTComponents.pas, released on 2000-09-22.

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
unit JvMtComponents;

interface

uses
  JvComponent, SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, SyncObjs, Consts,
  MTThreading, MTConst, MTData, MTSync, MTSyncMon;

type
  TJvMtComponent = class(TJvComponent);
  TJvMtSingleThread = class(TMTThread);
  TJvMtThread = class;

  TJvMtThreadEvent = procedure (Sender: TJvMtThread;
    MTThread: TJvMtSingleThread) of object;

  TJvMtManager = class (TJvMtComponent)
  private
    FManager: TMTManager;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); 
      override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function AcquireNewThread: TJvMtSingleThread;
    function AcquireThread(Ticket: TMTTicket;
      var Thread: TJvMtSingleThread): Boolean;
    function ActiveThreads: Boolean;
    procedure ReleaseThread(Ticket: TMTTicket);
    procedure TerminateThreads;
    procedure WaitThreads;
  end;

  TJvMtManagedComponent = class (TJvMtComponent)
  private
    FManager: TJvMtManager;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); 
      override;
    procedure SetManager(Value: TJvMtManager); virtual;
  published
    property Manager: TJvMtManager read FManager write SetManager;
  end;

  TJvMtThread = class (TJvMtManagedComponent)
  private
    FOnExecute: TJvMtThreadEvent;
    FOnFinished: TJvMtThreadEvent;
    FOnTerminating: TJvMtThreadEvent;
    FThread: TJvMtSingleThread;
    FRunOnCreate: Boolean;
    function GetStatus: TMTThreadStatus;
    function GetTicket: TMTTicket;
    procedure HookThread;
    procedure OnIntExecute(Thread: TMTThread);
    procedure OnIntFinished(Thread: TMTThread);
    procedure OnIntTerminating(Thread: TMTThread);
    procedure ReleaseThread;
    procedure SetOnExecute(Value: TJvMtThreadEvent);
    procedure SetOnFinished(Value: TJvMtThreadEvent);
    procedure SetOnTerminating(Value: TJvMtThreadEvent);
    procedure UnHookThread;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetManager(Value: TJvMtManager); override;
    procedure DoExecute(MTThread: TJvMtSingleThread); dynamic;
    procedure DoFinished(MTThread: TJvMtSingleThread); dynamic;
    procedure DoTerminating(MTThread: TJvMtSingleThread); dynamic;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckTerminate;
    procedure Run;
    procedure RunCopy;
    procedure Synchronize(Method: TThreadMethod);
    procedure Terminate;
    procedure Wait;
    property Status: TMTThreadStatus read GetStatus;
    property Ticket: TMTTicket read GetTicket;
  published
    property RunOnCreate: Boolean read FRunOnCreate write FRunOnCreate;
    property OnExecute: TJvMtThreadEvent read FOnExecute write SetOnExecute;
    property OnFinished: TJvMtThreadEvent read FOnFinished write SetOnFinished;
    property OnTerminating: TJvMtThreadEvent read FOnTerminating write
      SetOnTerminating;
  end;

  TJvMtSectionBase = class (TJvMtComponent)
  private
    FSync: TSynchroObject;
    function GetActive: Boolean;
    procedure HookSync;
  protected
    procedure CheckInactiveProperty;
    procedure CreateSync; virtual; abstract;
    procedure Loaded; override;
  public
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  published
    property Active: Boolean read GetActive;
  end;

  TJvMtSection = class (TJvMtSectionBase)
  private
    FAllowRecursion: Boolean;
    FInitEntered: Boolean;
    procedure SetAllowRecursion(Value: Boolean);
    procedure SetInitEntered(Value: Boolean);
  protected
    procedure CreateSync; override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property AllowRecursion: Boolean read FAllowRecursion write 
      SetAllowRecursion default True;
    property InitEntered: Boolean read FInitEntered write SetInitEntered 
      default False;
  end;
  
  TJvMtCountingSection = class (TJvMtSectionBase)
  private
    FInitCount: Integer;
    FMaxCount: Integer;
    procedure SetInitAndMax(Init,Max: Integer);
    procedure SetInitCount(Value: Integer);
    procedure SetMaxCount(Value: Integer);
  protected
    procedure CreateSync; override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property InitCount: Integer read FInitCount write SetInitCount default 0;
    property MaxCount: Integer read FMaxCount write SetMaxCount default 1;
  end;
  
  TJvMtAsyncBufferBase = class (TJvMtComponent)
  private
    FBuffer: TMTAsyncBuffer;
    FHooking: TCriticalSection;
    FMaxBufferSize: Integer;
    procedure SetMaxBufferSize(Value: Integer);
  protected
    procedure CreateBuffer; virtual; abstract;
    procedure HookBuffer;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Read: TObject;
    procedure Write(AObject: TObject);
  published
    property MaxBufferSize: Integer read FMaxBufferSize write SetMaxBufferSize
      default MTDefaultBufferSize;
  end;

  TJvMtThreadToVCL = class (TJvMtAsyncBufferBase)
  private
    FOnCanRead: TNotifyEvent;
  protected
    procedure DoCanRead(Sender: TObject); dynamic;
    procedure CreateBuffer; override;
  published
    property OnCanRead: TNotifyEvent read FOnCanRead write FOnCanRead;
  end;

  TJvMtVCLToThread = class (TJvMtAsyncBufferBase)
  private
    FOnCanWrite: TNotifyEvent;
  protected
    procedure DoCanWrite(Sender: TObject); dynamic;
    procedure CreateBuffer; override;
    procedure Loaded; override;
  published
    property OnCanWrite: TNotifyEvent read FOnCanWrite write FOnCanWrite;
  end;

  TJvMtThreadToThread = class (TJvMtComponent)
  private
    FHooking: TCriticalSection;
    FMaxBufferSize: Integer;
    FQueue: TMTBoundedQueue;
    procedure HookQueue;
    procedure SetMaxBufferSize(Value: Integer);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Read: TObject;
    procedure Write(AObject: TObject);
  published
    property MaxBufferSize: Integer read FMaxBufferSize write SetMaxBufferSize
      default MTDefaultBufferSize;
  end;

  TJvMtMonitorSection = class (TJvMtComponent)
  private
    FMonitor: TMTMonitor;
    function GetCondition(ID: Integer): TMTCondition;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    property Condition[ID: Integer]: TMTCondition read GetCondition; default;
  end;

implementation


{ TJvMtManager }

constructor TJvMtManager.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);

  // We want to know about the form going down
  if AOwner <> nil then
    AOwner.FreeNotification(Self);
  
  // hook to a manager object if not designing in the IDE
  if not (csDesigning in ComponentState) then
    FManager := TMTManager.Create;
end;

destructor TJvMtManager.Destroy;
begin
  // call inherited destroy, this will send Notification's to all the mtcThread
  //  components. These will release all their threads.
  inherited Destroy;
  
  // Now all threads have been released.
  // Free the manager and the threads belonging to this manager.
  FManager.Free;
end;

function TJvMtManager.AcquireNewThread: TJvMtSingleThread;
begin
  Result := TJvMtSingleThread(FManager.AcquireNewThread);
end;

function TJvMtManager.AcquireThread(Ticket: TMTTicket;
  var Thread: TJvMtSingleThread): Boolean;
begin
  Result := FManager.AcquireThread(Ticket, TMTThread(Thread));
end;

function TJvMtManager.ActiveThreads: Boolean;
begin
  Result := FManager.ActiveThreads;
end;

procedure TJvMtManager.Notification(AComponent: TComponent; Operation: 
  TOperation);
begin
  // check if the form is being destroyed
  if (not (csDesigning in ComponentState)) and (Operation = opRemove) and
    (AComponent = Owner) then
  begin
    // form is going down: terminate all threads
    TerminateThreads;
    // and wait until all is well
    WaitThreads;
  end;
  
  inherited;
end;

procedure TJvMtManager.ReleaseThread(Ticket: TMTTicket);
begin
  FManager.ReleaseThread(Ticket);
end;

procedure TJvMtManager.TerminateThreads;
begin
  FManager.TerminateThreads;
end;

procedure TJvMtManager.WaitThreads;
begin
  FManager.WaitThreads;
end;


{ TJvMtManagedComponent }

procedure TJvMtManagedComponent.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = FManager) then
    FManager := nil;    // important during designtime

  inherited Notification(AComponent, Operation);
end;

procedure TJvMtManagedComponent.SetManager(Value: TJvMtManager);
begin
  if Assigned(FManager) then
    FManager.RemoveFreeNotification(Self);

  FManager := Value;

  if Assigned(FManager) then
    FManager.FreeNotification(Self);
end;

{ TJvMtThread }

constructor TJvMtThread.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor TJvMtThread.Destroy;
begin
  ReleaseThread;
  inherited Destroy;
end;

procedure TJvMtThread.CheckTerminate;
begin
  HookThread;
  FThread.CheckTerminate;
end;

function TJvMtThread.GetStatus: TMTThreadStatus;
begin
  HookThread;
  Result := FThread.Status;
end;

function TJvMtThread.GetTicket: TMTTicket;
begin
  HookThread;
  Result := FThread.Ticket;
end;

procedure TJvMtThread.HookThread;
begin
  if FThread = nil then
  begin
    if FManager = nil then
      raise EThread.Create('No ThreadManager specified');
  
    // get the new thread
    FThread := FManager.AcquireNewThread;
  
    // hook up the nessesary events
    if Assigned(FOnExecute) then FThread.OnExecute := OnIntExecute;
    if Assigned(FOnTerminating) then FThread.OnTerminating := OnIntTerminating;
    if Assigned(FOnFinished) then FThread.OnFinished := OnIntFinished;
  
    // give it a name
    FThread.Name := Name;
  end;
end;

procedure TJvMtThread.Notification(AComponent: TComponent; Operation: 
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = FManager) then
    ReleaseThread;      // important during runtime

  // now can inherited (this wil invalidate FManager)
  inherited Notification(AComponent, Operation);
end;

procedure TJvMtThread.OnIntExecute(Thread: TMTThread);
begin
  DoExecute(TJvMtSingleThread(Thread));
end;

procedure TJvMtThread.OnIntFinished(Thread: TMTThread);
begin
  DoFinished(TJvMtSingleThread(Thread));
end;

procedure TJvMtThread.OnIntTerminating(Thread: TMTThread);
begin
  DoTerminating(TJvMtSingleThread(Thread));
end;

procedure TJvMtThread.ReleaseThread;
begin
  // check if there is an acquired thread
  if FThread <> nil then
  begin
    // release the thread and invalidate the pointer
    FThread.Release;
    FThread := nil;
  end;
end;

procedure TJvMtThread.Run;
begin
  HookThread;
  FThread.Run;
end;

procedure TJvMtThread.RunCopy;
begin
  ReleaseThread;
  Run;
end;

procedure TJvMtThread.SetManager(Value: TJvMtManager);
begin
  UnhookThread;
  inherited;
end;

procedure TJvMtThread.SetOnExecute(Value: TJvMtThreadEvent);
begin
  UnhookThread;
  FOnExecute := Value;
end;

procedure TJvMtThread.SetOnFinished(Value: TJvMtThreadEvent);
begin
  UnhookThread;
  FOnFinished := Value;
end;

procedure TJvMtThread.SetOnTerminating(Value: TJvMtThreadEvent);
begin
  UnhookThread;
  FOnTerminating := Value;
end;

procedure TJvMtThread.Synchronize(Method: TThreadMethod);
begin
  HookThread;
  FThread.Synchronize(Method);
end;

procedure TJvMtThread.Terminate;
begin
  HookThread;
  FThread.Terminate;
end;

procedure TJvMtThread.UnHookThread;
begin
  if FThread <> nil then
  begin
    if FThread.Status in [tsInitializing, tsFinished] then
    begin
      FThread.Terminate; {incase initializing}
      FThread.Release;
      FThread := nil;
    end
    else
      raise EThread.Create('Operation not available while thread is active');
  end;
end;

procedure TJvMtThread.Wait;
begin
  HookThread;
  FThread.Wait;
end;
    
procedure TJvMtThread.DoExecute(MTThread: TJvMtSingleThread);
begin
  if Assigned(FOnExecute) then FOnExecute(Self, MTThread);
end;

procedure TJvMtThread.DoFinished(MTThread: TJvMtSingleThread);
begin
  if Assigned(FOnFinished) then FOnFinished(Self, MTThread);
end;

procedure TJvMtThread.DoTerminating(MTThread: TJvMtSingleThread);
begin
  if Assigned(FOnTerminating) then FOnTerminating(Self, MTThread);
end;

procedure TJvMtThread.Loaded;
begin
  inherited;
  // Comonent is ready. Shall we start a thread?
  if (not (csDesigning in ComponentState)) and FRunOnCreate then
    Run;
end;

{ TJvMtSectionBase }

destructor TJvMtSectionBase.Destroy;
begin
  // signal interested components that we are going down
  inherited Destroy;
  // cleanup
  FSync.Free;
end;

procedure TJvMtSectionBase.CheckInactiveProperty;
begin
  if Active then
    raise EThread.Create('Can not change property of active section');
end;

procedure TJvMtSectionBase.Enter;
begin
  HookSync;
  FSync.Acquire;
end;

function TJvMtSectionBase.GetActive: Boolean;
begin
  Result := FSync <> nil;
end;

procedure TJvMtSectionBase.HookSync;
begin
  if not Active then
    CreateSync;
end;

procedure TJvMtSectionBase.Leave;
begin
  HookSync;
  FSync.Release;
end;

procedure TJvMtSectionBase.Loaded;
begin
  inherited Loaded;
end;

{ TJvMtSection }

constructor TJvMtSection.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FAllowRecursion := True;
end;

procedure TJvMtSection.CreateSync;
begin
  if FAllowRecursion then
    FSync := TMTCriticalSection.Create(Name)
  else
    FSync := TMTMutex.Create(Name);

  if FInitEntered then
    Enter;
end;

procedure TJvMtSection.SetAllowRecursion(Value: Boolean);
begin
  CheckInactiveProperty;
  FAllowRecursion := Value;
end;

procedure TJvMtSection.SetInitEntered(Value: Boolean);
begin
  CheckInactiveProperty;
  FInitEntered := Value;
end;

{ TJvMtCountingSection }

constructor TJvMtCountingSection.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMaxCount := 1;
end;

procedure TJvMtCountingSection.CreateSync;
begin
  FSync := TMTSemaphore.Create(FMaxCount-FInitCount, FMaxCount, Name);
end;

procedure TJvMtCountingSection.SetInitAndMax(Init,Max: Integer);
begin
  CheckInactiveProperty;
  if (Max < 1) or (Init < 0) or (Init > Max) then
    raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
  
  FInitCount := Init;
  FMaxCount := Max;
end;

procedure TJvMtCountingSection.SetInitCount(Value: Integer);
begin
  SetInitAndMax(Value, FMaxCount);
end;

procedure TJvMtCountingSection.SetMaxCount(Value: Integer);
begin
  SetInitAndMax(FInitCount, Value);
end;

{ TJvMtAsyncBufferBase }

constructor TJvMtAsyncBufferBase.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMaxBufferSize := MTDefaultBufferSize;
  FHooking := TCriticalSection.Create;
end;

destructor TJvMtAsyncBufferBase.Destroy;
begin
  // notify interested components
  inherited Destroy;
  // cleanup
  FBuffer.Free;
  FHooking.Free;
end;

procedure TJvMtAsyncBufferBase.HookBuffer;
begin
  // buffer still uncreated?
  if FBuffer = nil then
  begin
    // enter critical section
    FHooking.Enter;
    try
      // perform check again. and create if we are the first in this section
      if FBuffer = nil then CreateBuffer;
    finally
      FHooking.Leave;
    end;
  end;
end;

function TJvMtAsyncBufferBase.Read: TObject;
begin
  HookBuffer;
  Result := FBuffer.Read;
end;

procedure TJvMtAsyncBufferBase.SetMaxBufferSize(Value: Integer);
begin
  if FBuffer <> nil then
    raise EThread.Create('Can''t change property of active buffer');
  FMaxBufferSize := Value;
end;

procedure TJvMtAsyncBufferBase.Write(AObject: TObject);
begin
  HookBuffer;
  FBuffer.Write(AObject);
end;

{ TJvMtThreadToVCL }

procedure TJvMtThreadToVCL.CreateBuffer;
begin
  FBuffer := TMTBufferToVCL.Create(FMaxBufferSize, Name);
  TMTBufferToVCL(FBuffer).OnCanRead := DoCanRead;
end;

procedure TJvMtThreadToVCL.DoCanRead(Sender: TObject);
begin
  // call the OnCanRead event with this object as the sender
  if Assigned(FOnCanRead) then FOnCanRead(Self);
end;

{ TJvMtVCLToThread }

procedure TJvMtVCLToThread.CreateBuffer;
begin
  FBuffer := TMTVCLToBuffer.Create(FMaxBufferSize, Name);
  TMTVCLToBuffer(FBuffer).OnCanWrite := DoCanWrite;
end;

procedure TJvMtVCLToThread.DoCanWrite(Sender: TObject);
begin
  // call the OnCanWrite event with this object as the sender
  if Assigned(FOnCanWrite) then FOnCanWrite(Self);
end;

procedure TJvMtVCLToThread.Loaded;
begin
  inherited Loaded;
  // force first Event
  HookBuffer;
  if Assigned(FOnCanWrite) then
    FOnCanWrite(Self);
end;

{ TJvMtThreadToThread }

constructor TJvMtThreadToThread.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMaxBufferSize := MTDefaultBufferSize;
  FHooking := TCriticalSection.Create;
end;

destructor TJvMtThreadToThread.Destroy;
begin
  inherited Destroy;
  FQueue.Free;
  FHooking.Free;
end;

procedure TJvMtThreadToThread.HookQueue;
begin
  // buffer still uncreated?
  if FQueue = nil then
  begin
    // enter critical section
    FHooking.Enter;
    try
      // perform check again. and create if we are the first in this section
      if FQueue = nil then
        FQueue := TMTBoundedQueue.Create(FMaxBufferSize,Name);
    finally
      FHooking.Leave;
    end;
  end;
end;

function TJvMtThreadToThread.Read: TObject;
begin
  HookQueue;
  Result := FQueue.Pop;
end;

procedure TJvMtThreadToThread.SetMaxBufferSize(Value: Integer);
begin
  if FQueue <> nil then
    raise EThread.Create('Can''t change property of active buffer');
  if Value < 1 then
    raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
  FMaxBufferSize := Value;
end;

procedure TJvMtThreadToThread.Write(AObject: TObject);
begin
  HookQueue;
  FQueue.Push(AObject);
end;


{ TJvMtMonitorSection }

constructor TJvMtMonitorSection.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMonitor := TMTMonitor.Create;
end;

destructor TJvMtMonitorSection.Destroy;
begin
  FMonitor.Free;
  inherited Destroy;
end;

procedure TJvMtMonitorSection.Enter;
begin
  FMonitor.Enter;
end;

function TJvMtMonitorSection.GetCondition(ID: Integer): TMTCondition;
begin
  Result := FMonitor.Condition[ID];
end;

procedure TJvMtMonitorSection.Leave;
begin
  FMonitor.Leave;
end;

end.
