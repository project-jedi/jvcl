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

The Original Code is: MTComponents.pas, released on 2000-09-22.

The Initial Developer of the Original Code is Erwin Molendijk.
Portions created by Erwin Molendijk are Copyright (C) 2002 Erwin Molendijk.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQMTComponents;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, SyncObjs,
  QConsts, 
  JvQComponent, 
  JvQMTThreading, JvQMTConsts, JvQMTData, JvQMTSync, JvQMTSyncMon;

type 
  TJvMTComponent = class(TJvComponent); 
  TJvMTSingleThread = class(TMTThread);
  TJvMTThread = class;

  TJvMTThreadEvent = procedure (Sender: TJvMTThread;
    MTThread: TJvMTSingleThread) of object;

  TJvMTManager = class(TJvMTComponent)
  private
    FManager: TMTManager;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AcquireNewThread: TJvMTSingleThread;
    function AcquireThread(Ticket: TMTTicket;
      var Thread: TJvMTSingleThread): Boolean;
    function ActiveThreads: Boolean;
    procedure ReleaseThread(Ticket: TMTTicket);
    procedure TerminateThreads;
    procedure WaitThreads;
  end;

  TJvMTManagedComponent = class(TJvMTComponent)
  private
    FManager: TJvMTManager;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); 
      override;
    procedure SetManager(Value: TJvMTManager); virtual;
  published
    property Manager: TJvMTManager read FManager write SetManager;
  end;

  TJvMTThread = class(TJvMTManagedComponent)
  private
    FOnExecute: TJvMTThreadEvent;
    FOnFinished: TJvMTThreadEvent;
    FOnTerminating: TJvMTThreadEvent;
    FThread: TJvMTSingleThread;
    FRunOnCreate: Boolean;
    function GetStatus: TMTThreadStatus;
    function GetTicket: TMTTicket;
    procedure HookThread;
    procedure OnIntExecute(Thread: TMTThread);
    procedure OnIntFinished(Thread: TMTThread);
    procedure OnIntTerminating(Thread: TMTThread);
    procedure ReleaseThread;
    procedure SetOnExecute(Value: TJvMTThreadEvent);
    procedure SetOnFinished(Value: TJvMTThreadEvent);
    procedure SetOnTerminating(Value: TJvMTThreadEvent);
    procedure UnHookThread;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetManager(Value: TJvMTManager); override;
    procedure DoExecute(MTThread: TJvMTSingleThread); dynamic;
    procedure DoFinished(MTThread: TJvMTSingleThread); dynamic;
    procedure DoTerminating(MTThread: TJvMTSingleThread); dynamic;
  public
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
    property OnExecute: TJvMTThreadEvent read FOnExecute write SetOnExecute;
    property OnFinished: TJvMTThreadEvent read FOnFinished write SetOnFinished;
    property OnTerminating: TJvMTThreadEvent read FOnTerminating write
      SetOnTerminating;
  end;

  TJvMTSectionBase = class(TJvMTComponent)
  private
    FSync: TSynchroObject;
    function GetActive: Boolean;
    procedure HookSync;
  protected
    procedure CheckInactiveProperty;
    procedure CreateSync; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  published
    property Active: Boolean read GetActive;
  end;

  TJvMTSection = class(TJvMTSectionBase)
  private
    FAllowRecursion: Boolean;
    FInitEntered: Boolean;
    procedure SetAllowRecursion(Value: Boolean);
    procedure SetInitEntered(Value: Boolean);
  protected
    procedure CreateSync; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowRecursion: Boolean read FAllowRecursion write 
      SetAllowRecursion default True;
    property InitEntered: Boolean read FInitEntered write SetInitEntered 
      default False;
  end;
  
  TJvMTCountingSection = class(TJvMTSectionBase)
  private
    FInitCount: Integer;
    FMaxCount: Integer;
    procedure SetInitAndMax(Init,Max: Integer);
    procedure SetInitCount(Value: Integer);
    procedure SetMaxCount(Value: Integer);
  protected
    procedure CreateSync; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property InitCount: Integer read FInitCount write SetInitCount default 0;
    property MaxCount: Integer read FMaxCount write SetMaxCount default 1;
  end;
  
  TJvMTAsyncBufferBase = class(TJvMTComponent)
  private
    FBuffer: TMTAsyncBuffer;
    FHooking: TCriticalSection;
    FMaxBufferSize: Integer;
    procedure SetMaxBufferSize(Value: Integer);
  protected
    procedure CreateBuffer; virtual; abstract;
    procedure HookBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Read: TObject;
    procedure Write(AObject: TObject);
  published
    property MaxBufferSize: Integer read FMaxBufferSize write SetMaxBufferSize
      default MTDefaultBufferSize;
  end;

  TJvMTThreadToVCL = class(TJvMTAsyncBufferBase)
  private
    FOnCanRead: TNotifyEvent;
  protected
    procedure DoCanRead(Sender: TObject); dynamic;
    procedure CreateBuffer; override;
  published
    property OnCanRead: TNotifyEvent read FOnCanRead write FOnCanRead;
  end;

  TJvMTVCLToThread = class(TJvMTAsyncBufferBase)
  private
    FOnCanWrite: TNotifyEvent;
  protected
    procedure DoCanWrite(Sender: TObject); dynamic;
    procedure CreateBuffer; override;
    procedure Loaded; override;
  published
    property OnCanWrite: TNotifyEvent read FOnCanWrite write FOnCanWrite;
  end;

  TJvMTThreadToThread = class(TJvMTComponent)
  private
    FHooking: TCriticalSection;
    FMaxBufferSize: Integer;
    FQueue: TMTBoundedQueue;
    procedure HookQueue;
    procedure SetMaxBufferSize(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Read: TObject;
    procedure Write(AObject: TObject);
  published
    property MaxBufferSize: Integer read FMaxBufferSize write SetMaxBufferSize
      default MTDefaultBufferSize;
  end;

  TJvMTMonitorSection = class(TJvMTComponent)
  private
    FMonitor: TMTMonitor;
    function GetCondition(ID: Integer): TMTCondition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    property Condition[ID: Integer]: TMTCondition read GetCondition; default;
  end;

implementation


uses
  JvQResources;




constructor TJvMTManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // We want to know about the form going down
  if AOwner <> nil then
    AOwner.FreeNotification(Self);
  
  // hook to a manager object if not designing in the IDE
  if not (csDesigning in ComponentState) then
    FManager := TMTManager.Create;
end;

destructor TJvMTManager.Destroy;
begin
  // call inherited destroy, this will send Notification's to all the mtcThread
  //  components. These will release all their threads.
  inherited Destroy;
  
  // Now all threads have been released.
  // Free the manager and the threads belonging to this manager.
  FManager.Free;
end;

function TJvMTManager.AcquireNewThread: TJvMTSingleThread;
begin
  Result := TJvMTSingleThread(FManager.AcquireNewThread);
end;

function TJvMTManager.AcquireThread(Ticket: TMTTicket;
  var Thread: TJvMTSingleThread): Boolean;
begin
  Result := FManager.AcquireThread(Ticket, TMTThread(Thread));
end;

function TJvMTManager.ActiveThreads: Boolean;
begin
  Result := FManager.ActiveThreads;
end;

procedure TJvMTManager.Notification(AComponent: TComponent; Operation: 
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
  
  inherited Notification(AComponent, Operation);
end;

procedure TJvMTManager.ReleaseThread(Ticket: TMTTicket);
begin
  FManager.ReleaseThread(Ticket);
end;

procedure TJvMTManager.TerminateThreads;
begin
  FManager.TerminateThreads;
end;

procedure TJvMTManager.WaitThreads;
begin
  FManager.WaitThreads;
end;

//=== { TJvMTManagedComponent } ==============================================

procedure TJvMTManagedComponent.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = FManager) then
    FManager := nil;    // important during designtime

  inherited Notification(AComponent, Operation);
end;

procedure TJvMTManagedComponent.SetManager(Value: TJvMTManager);
begin
  if Assigned(FManager) then
    FManager.RemoveFreeNotification(Self);

  FManager := Value;

  if Assigned(FManager) then
    FManager.FreeNotification(Self);
end;

//=== { TJvMTThread } ========================================================

destructor TJvMTThread.Destroy;
begin
  ReleaseThread;
  inherited Destroy;
end;

procedure TJvMTThread.CheckTerminate;
begin
  HookThread;
  FThread.CheckTerminate;
end;

function TJvMTThread.GetStatus: TMTThreadStatus;
begin
  HookThread;
  Result := FThread.Status;
end;

function TJvMTThread.GetTicket: TMTTicket;
begin
  HookThread;
  Result := FThread.Ticket;
end;

procedure TJvMTThread.HookThread;
begin
  if FThread = nil then
  begin
    if FManager = nil then
      raise EThread.CreateRes(@RsENoThreadManager);
  
    // get the new thread
    FThread := FManager.AcquireNewThread;
  
    // hook up the nessesary events
    if Assigned(FOnExecute) then
      FThread.OnExecute := OnIntExecute;
    if Assigned(FOnTerminating) then
      FThread.OnTerminating := OnIntTerminating;
    if Assigned(FOnFinished) then
      FThread.OnFinished := OnIntFinished;
  
    // give it a name
    FThread.Name := Name;
  end;
end;

procedure TJvMTThread.Notification(AComponent: TComponent; Operation: 
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = FManager) then
    ReleaseThread;      // important during runtime

  // now can inherited (this wil invalidate FManager)
  inherited Notification(AComponent, Operation);
end;

procedure TJvMTThread.OnIntExecute(Thread: TMTThread);
begin
  DoExecute(TJvMTSingleThread(Thread));
end;

procedure TJvMTThread.OnIntFinished(Thread: TMTThread);
begin
  DoFinished(TJvMTSingleThread(Thread));
end;

procedure TJvMTThread.OnIntTerminating(Thread: TMTThread);
begin
  DoTerminating(TJvMTSingleThread(Thread));
end;

procedure TJvMTThread.ReleaseThread;
begin
  // check if there is an acquired thread
  if FThread <> nil then
  begin
    // release the thread and invalidate the pointer
    FThread.Release;
    FThread := nil;
  end;
end;

procedure TJvMTThread.Run;
begin
  HookThread;
  FThread.Run;
end;

procedure TJvMTThread.RunCopy;
begin
  ReleaseThread;
  Run;
end;

procedure TJvMTThread.SetManager(Value: TJvMTManager);
begin
  UnHookThread;
  inherited SetManager(Value);
end;

procedure TJvMTThread.SetOnExecute(Value: TJvMTThreadEvent);
begin
  UnHookThread;
  FOnExecute := Value;
end;

procedure TJvMTThread.SetOnFinished(Value: TJvMTThreadEvent);
begin
  UnHookThread;
  FOnFinished := Value;
end;

procedure TJvMTThread.SetOnTerminating(Value: TJvMTThreadEvent);
begin
  UnHookThread;
  FOnTerminating := Value;
end;

procedure TJvMTThread.Synchronize(Method: TThreadMethod);
begin
  HookThread;
  FThread.Synchronize(Method);
end;

procedure TJvMTThread.Terminate;
begin
  HookThread;
  FThread.Terminate;
end;

procedure TJvMTThread.UnHookThread;
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
      raise EThread.CreateRes(@RsEOperatorNotAvailable);
  end;
end;

procedure TJvMTThread.Wait;
begin
  HookThread;
  FThread.Wait;
end;
    
procedure TJvMTThread.DoExecute(MTThread: TJvMTSingleThread);
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self, MTThread);
end;

procedure TJvMTThread.DoFinished(MTThread: TJvMTSingleThread);
begin
  if Assigned(FOnFinished) then
    FOnFinished(Self, MTThread);
end;

procedure TJvMTThread.DoTerminating(MTThread: TJvMTSingleThread);
begin
  if Assigned(FOnTerminating) then
    FOnTerminating(Self, MTThread);
end;

procedure TJvMTThread.Loaded;
begin
  inherited Loaded;
  // Component is ready. Shall we start a thread?
  if (not (csDesigning in ComponentState)) and FRunOnCreate then
    Run;
end;

//=== { TJvMTSectionBase } ===================================================

destructor TJvMTSectionBase.Destroy;
begin
  // signal interested components that we are going down
  inherited Destroy;
  // cleanup
  FSync.Free;
end;

procedure TJvMTSectionBase.CheckInactiveProperty;
begin
  if Active then
    raise EThread.CreateRes(@RsECannotChangePropertySection);
end;

procedure TJvMTSectionBase.Enter;
begin
  HookSync;
  FSync.Acquire;
end;

function TJvMTSectionBase.GetActive: Boolean;
begin
  Result := FSync <> nil;
end;

procedure TJvMTSectionBase.HookSync;
begin
  if not Active then
    CreateSync;
end;

procedure TJvMTSectionBase.Leave;
begin
  HookSync;
  FSync.Release;
end;

//=== { TJvMTSection } =======================================================

constructor TJvMTSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowRecursion := True;
end;

procedure TJvMTSection.CreateSync;
begin
  if FAllowRecursion then
    FSync := TMTCriticalSection.Create(Name)
  else
    FSync := TMTMutex.Create(Name);

  if FInitEntered then
    Enter;
end;

procedure TJvMTSection.SetAllowRecursion(Value: Boolean);
begin
  CheckInactiveProperty;
  FAllowRecursion := Value;
end;

procedure TJvMTSection.SetInitEntered(Value: Boolean);
begin
  CheckInactiveProperty;
  FInitEntered := Value;
end;

//=== { TJvMTCountingSection } ===============================================

constructor TJvMTCountingSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxCount := 1;
end;

procedure TJvMTCountingSection.CreateSync;
begin
  FSync := TMTSemaphore.Create(FMaxCount-FInitCount, FMaxCount, Name);
end;

procedure TJvMTCountingSection.SetInitAndMax(Init,Max: Integer);
begin
  CheckInactiveProperty;
  if (Max < 1) or (Init < 0) or (Init > Max) then
    raise EInvalidOperation.CreateResFmt(@SPropertyOutOfRange, [ClassName]);
  
  FInitCount := Init;
  FMaxCount := Max;
end;

procedure TJvMTCountingSection.SetInitCount(Value: Integer);
begin
  SetInitAndMax(Value, FMaxCount);
end;

procedure TJvMTCountingSection.SetMaxCount(Value: Integer);
begin
  SetInitAndMax(FInitCount, Value);
end;

//=== { TJvMTAsyncBufferBase } ===============================================

constructor TJvMTAsyncBufferBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxBufferSize := MTDefaultBufferSize;
  FHooking := TCriticalSection.Create;
end;

destructor TJvMTAsyncBufferBase.Destroy;
begin
  // notify interested components
  inherited Destroy;
  // cleanup
  FBuffer.Free;
  FHooking.Free;
end;

procedure TJvMTAsyncBufferBase.HookBuffer;
begin
  // buffer still uncreated?
  if FBuffer = nil then
  begin
    // enter critical section
    FHooking.Enter;
    try
      // perform check again. and create if we are the first in this section
      if FBuffer = nil then
       CreateBuffer;
    finally
      FHooking.Leave;
    end;
  end;
end;

function TJvMTAsyncBufferBase.Read: TObject;
begin
  HookBuffer;
  Result := FBuffer.Read;
end;

procedure TJvMTAsyncBufferBase.SetMaxBufferSize(Value: Integer);
begin
  if FBuffer <> nil then
    raise EThread.CreateRes(@RsECannotChangePropertyBuffer);
  FMaxBufferSize := Value;
end;

procedure TJvMTAsyncBufferBase.Write(AObject: TObject);
begin
  HookBuffer;
  FBuffer.Write(AObject);
end;

//=== { TJvMTThreadToVCL } ===================================================

procedure TJvMTThreadToVCL.CreateBuffer;
begin
  FBuffer := TMTBufferToVCL.Create(FMaxBufferSize, Name);
  TMTBufferToVCL(FBuffer).OnCanRead := DoCanRead;
end;

procedure TJvMTThreadToVCL.DoCanRead(Sender: TObject);
begin
  // call the OnCanRead event with this object as the sender
  if Assigned(FOnCanRead) then
    FOnCanRead(Self);
end;

//=== { TJvMTVCLToThread } ===================================================

procedure TJvMTVCLToThread.CreateBuffer;
begin
  FBuffer := TMTVCLToBuffer.Create(FMaxBufferSize, Name);
  TMTVCLToBuffer(FBuffer).OnCanWrite := DoCanWrite;
end;

procedure TJvMTVCLToThread.DoCanWrite(Sender: TObject);
begin
  // call the OnCanWrite event with this object as the sender
  if Assigned(FOnCanWrite) then
    FOnCanWrite(Self);
end;

procedure TJvMTVCLToThread.Loaded;
begin
  inherited Loaded;
  // force first Event
  HookBuffer;
  if Assigned(FOnCanWrite) then
    FOnCanWrite(Self);
end;

//=== { TJvMTThreadToThread } ================================================

constructor TJvMTThreadToThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxBufferSize := MTDefaultBufferSize;
  FHooking := TCriticalSection.Create;
end;

destructor TJvMTThreadToThread.Destroy;
begin
  inherited Destroy;
  FQueue.Free;
  FHooking.Free;
end;

procedure TJvMTThreadToThread.HookQueue;
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

function TJvMTThreadToThread.Read: TObject;
begin
  HookQueue;
  Result := FQueue.Pop;
end;

procedure TJvMTThreadToThread.SetMaxBufferSize(Value: Integer);
begin
  if FQueue <> nil then
    raise EThread.CreateRes(@RsECannotChangePropertyBuffer);
  if Value < 1 then
    raise EInvalidOperation.CreateResFmt(@SPropertyOutOfRange, [ClassName]);
  FMaxBufferSize := Value;
end;

procedure TJvMTThreadToThread.Write(AObject: TObject);
begin
  HookQueue;
  FQueue.Push(AObject);
end;

//=== { TJvMTMonitorSection } ================================================

constructor TJvMTMonitorSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMonitor := TMTMonitor.Create;
end;

destructor TJvMTMonitorSection.Destroy;
begin
  FMonitor.Free;
  inherited Destroy;
end;

procedure TJvMTMonitorSection.Enter;
begin
  FMonitor.Enter;
end;

function TJvMTMonitorSection.GetCondition(ID: Integer): TMTCondition;
begin
  Result := FMonitor.Condition[ID];
end;

procedure TJvMTMonitorSection.Leave;
begin
  FMonitor.Leave;
end;

end.
