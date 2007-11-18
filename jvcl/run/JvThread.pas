{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThread.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvThread;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Syncobjs,
  {$IFDEF MSWINDOWS}
  Windows, Controls, ExtCtrls,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows,
  {$ENDIF UNIX}
  Forms, Dialogs,
  JvTypes, JvComponentBase, JvComponent;

type
  TJvCustomThreadDialog = class;
  TJvCustomThreadDialogForm = class;
  TJvThread = class;

  TJvCustomThreadDialogFormEvent = procedure(DialogForm: TJvCustomThreadDialogForm) of object;

  TJvCustomThreadDialogOptions = class(TPersistent)
  private
    FFormStyle: TFormStyle;
    FOwner: TJvCustomThreadDialog;
    FShowDelay: Integer;
    FShowDialog: Boolean;
    FShowModal: Boolean;
    procedure SetShowDelay(const Value: Integer);
    procedure SetShowDialog(Value: Boolean);
    procedure SetShowModal(Value: Boolean);
  protected
  public
    constructor Create(AOwner: TJvCustomThreadDialog); virtual;
  published
    property FormStyle: TFormStyle read FFormStyle write FFormStyle;
    //1 Delay in milliseconds for starting the thread dialog
    property ShowDelay: Integer read FShowDelay write SetShowDelay default 0;
    //1 Flag if there should be a dialog which shows the thread status
    property ShowDialog: Boolean read FShowDialog write SetShowDialog default False;
    //1 Flag if the status dialog is modal
    property ShowModal: Boolean read FShowModal write SetShowModal default True;
  end;

  TJvCustomThreadDialogForm = class(TJvForm)
  private
    FConnectedDataComponent: TComponent;
    FConnectedDataObject: TObject;
    FConnectedThread: TJvThread;
    FDialogOptions: TJvCustomThreadDialogOptions;
    FFormIsShown: Boolean;
    FInternalShowDelay: Integer;
    FInternalTimer: TTimer;
    FInternalTimerInterval: Integer;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnPressCancel: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FParentHandle: HWND;
    procedure SetConnectedDataComponent(Value: TComponent);
    procedure SetConnectedDataObject(Value: TObject);
    procedure SetInternalTimerInterval(Value: Integer);
    procedure SetOnClose(Value: TCloseEvent);
    procedure InternalTimer(Sender: TObject); virtual;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure InitializeFormContents; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TransferDialogOptions; virtual;
    procedure UpdateFormContents; virtual;
    property FormIsShown: Boolean read FFormIsShown default False;
    property OnPressCancel: TNotifyEvent read FOnPressCancel write FOnPressCancel;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    constructor CreateNewFormStyle(AOwner: TJvThread; FormStyle: TFormStyle;
      Parent: TWinControl = nil); virtual;
    destructor Destroy; override;
    procedure DefaultCancelBtnClick(Sender: TObject);
    procedure ReplaceFormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReplaceFormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ReplaceFormShow(Sender: TObject);
    property ConnectedDataComponent: TComponent read FConnectedDataComponent write SetConnectedDataComponent;
    property ConnectedDataObject: TObject read FConnectedDataObject write SetConnectedDataObject;
    property ConnectedThread: TJvThread read FConnectedThread;
    property DialogOptions: TJvCustomThreadDialogOptions read FDialogOptions write
        FDialogOptions;
    property InternalTimerInterval: Integer read FInternalTimerInterval write SetInternalTimerInterval;
  published
    property OnClose: TCloseEvent read FOnClose write SetOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TJvCustomThreadDialog = class(TJvComponent)
  private
    FDialogOptions: TJvCustomThreadDialogOptions;
    FOnPressCancel: TNotifyEvent;
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; virtual; abstract;
  published
    property DialogOptions: TJvCustomThreadDialogOptions read FDialogOptions write FDialogOptions;
    property OnPressCancel: TNotifyEvent read FOnPressCancel write FOnPressCancel;
  end;

  TJvThreadShowMessageDlgEvent = procedure(const Msg: string; AType: TMsgDlgType;
      AButtons: TMsgDlgButtons; HelpCtx: Longint; var DlgResult : Word) of object;

  TJvBaseThread = class(TThread)
  private
    FException: Exception;
    FExceptionAddr: Pointer;
    FInternalTerminate: Boolean;
    FExecuteEvent: TJvNotifyParamsEvent;
    FOnResumeDone: Boolean;
    FExecuteIsActive: Boolean;
    FFinished: Boolean;
    FOnShowMessageDlgEvent: TJvThreadShowMessageDlgEvent;
    FParams: Pointer;
    FSender: TObject;
    FSynchAButtons: TMsgDlgButtons;
    FSynchAType: TMsgDlgType;
    FSynchHelpCtx: Longint;
    FSynchMessageDlgResult: Word;
    FSynchMsg: string;
    procedure ExceptionHandler;
  protected
    procedure InternalMessageDlg;
  public
    constructor Create(Sender: TObject; Event: TJvNotifyParamsEvent; Params: Pointer); virtual;
    destructor Destroy; override;
    procedure Resume;
    procedure Execute; override;
    procedure Synchronize(Method: TThreadMethod);
    function SynchMessageDlg(const Msg: string; AType: TMsgDlgType;
      AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
    property Container: TObject read FSender;
    property ExecuteIsActive: Boolean read FExecuteIsActive;
    property Finished: Boolean read FFinished;
    property Terminated;
    property Params: Pointer read FParams;
    property ReturnValue;
    property OnShowMessageDlgEvent: TJvThreadShowMessageDlgEvent
      read FOnShowMessageDlgEvent write FOnShowMessageDlgEvent;
  end;

  TJvThread = class(TJvComponent)
  private
    FAfterCreateDialogForm: TJvCustomThreadDialogFormEvent;
    FBeforeResume: TNotifyEvent;
    FThreads: TThreadList;
    FListLocker: TCriticalSection;
    FLockedList: TList;
    FExclusive: Boolean;
    FMaxCount: Integer;
    FRunOnCreate: Boolean;
    FOnBegin: TNotifyEvent;
    FOnExecute: TJvNotifyParamsEvent;
    FOnFinish: TNotifyEvent;
    FOnFinishAll: TNotifyEvent;
    FFreeOnTerminate: Boolean;
    FOnShowMessageDlgEvent: TJvThreadShowMessageDlgEvent;
    FPriority: TThreadPriority;
    FThreadDialog: TJvCustomThreadDialog;
    FThreadDialogAllowed: Boolean;
    FThreadDialogForm: TJvCustomThreadDialogForm;
    procedure DoBegin;
    procedure DoTerminate(Sender: TObject);
    function GetCount: Integer;
    function GetThreads(Index: Integer): TJvBaseThread;
    function GetTerminated: Boolean; // in context of thread in list - for itself; in others - for all threads in list
    procedure SetReturnValue(RetVal: Integer); // in context of thread in list - set return value (slower)
    function GetReturnValue: Integer;  // in context of thread in list - get return value (slower)
    procedure CreateThreadDialogForm;
    function GetCurrentThread: TJvBaseThread;
  protected
    procedure InternalAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm); virtual;
    function GetOneThreadIsRunning: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelExecute; virtual;
    function Execute(P: Pointer): TJvBaseThread;
    procedure ExecuteAndWait(P: Pointer); // wait for all threads in list
    procedure ExecuteThreadAndWait(P: Pointer); // wait only this thread
    procedure ExecuteWithDialog(P: Pointer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Synchronize(Method: TThreadMethod); // (slower)
    function SynchMessageDlg(const Msg: string; AType: TMsgDlgType;
      AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;

    procedure Lock;   // for safe use of property Threads[]
    procedure Unlock;

    property Count: Integer read GetCount;
    property Threads[Index: Integer]: TJvBaseThread read GetThreads;
    property LastThread: TJvBaseThread read GetCurrentThread; //GetLastThread;
    property Terminated: Boolean read GetTerminated; // in context of thread in list - for itself; in others - for all threads in list
    property ReturnValue: Integer read GetReturnValue write SetReturnValue; // in context of thread in list - set return value (slower)
    property OneThreadIsRunning: Boolean read GetOneThreadIsRunning;
    //1 Property to allow/disallow the thread dialog form
    property ThreadDialogAllowed: Boolean read FThreadDialogAllowed write
        FThreadDialogAllowed default True;
    property ThreadDialogForm: TJvCustomThreadDialogForm read FThreadDialogForm;
(*
    function GetPriority(Thread: THandle): TThreadPriority;
    procedure SetPriority(Thread: THandle; Priority: TThreadPriority);
    {$IFDEF UNIX}
    function GetPolicy(Thread: THandle): Integer;
    procedure SetPolicy(Thread: THandle; Policy: Integer);
    procedure SetPolicyAll(Policy: Integer);
    {$ENDIF UNIX}
    procedure QuitThread(Thread: THandle);
    procedure Suspend(Thread: THandle); // should not be used
    procedure Resume(Thread: THandle); overload;
*)
    {$IFDEF UNIX}
    procedure SetPolicy(Policy: Integer); // [not tested] in context of thread in list - for itself; in other contexts - for all threads in list
    {$ENDIF UNIX}
    procedure SetPriority(NewPriority: TThreadPriority); // in context of thread in list - for itself; in other contexts - for all threads in list
    procedure Resume(BaseThread: TJvBaseThread); overload;
    procedure Resume; overload; // resumes all threads including deferred (RunOnCreate=false)
    procedure Suspend;          // in context of thread in list - for itself; in other contexts - for all threads in list
    procedure Terminate;        // terminates all threads
    procedure WaitFor;          // wait for all threads
    procedure RemoveZombie(BaseThread: TJvBaseThread); overload; // remove finished thread (where FreeOnTerminate was false)
    procedure RemoveZombie; overload; // remove all finished threads (where FreeOnTerminate was false)
    //1 //Combination of Terminate and WaitFor, optional RemoveZombie
    procedure TerminateWaitFor(iRemoveZombies: Boolean = true);
  published
    property Exclusive: Boolean read FExclusive write FExclusive;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property RunOnCreate: Boolean read FRunOnCreate write FRunOnCreate;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Priority: TThreadPriority read FPriority write FPriority default tpNormal;
    property ThreadDialog: TJvCustomThreadDialog read FThreadDialog write FThreadDialog;
    property AfterCreateDialogForm: TJvCustomThreadDialogFormEvent
      read FAfterCreateDialogForm write FAfterCreateDialogForm;
    property BeforeResume: TNotifyEvent read FBeforeResume write FBeforeResume;
    property OnBegin: TNotifyEvent read FOnBegin write FOnBegin;
    property OnExecute: TJvNotifyParamsEvent read FOnExecute write FOnExecute;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnFinishAll: TNotifyEvent read FOnFinishAll write FOnFinishAll;
    property OnShowMessageDlgEvent: TJvThreadShowMessageDlgEvent read
      FOnShowMessageDlgEvent write FOnShowMessageDlgEvent;
  end;

// Cannot be synchronized to the MainThread (VCL)
// (rom) why are these in the interface section?
procedure Synchronize(Method: TNotifyEvent);
procedure SynchronizeParams(Method: TJvNotifyParamsEvent; P: Pointer);

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
  JvResources, JvDSADialogs;

var
  SyncMtx: THandle = 0;

procedure Synchronize(Method: TNotifyEvent);
begin
  WaitForSingleObject(SyncMtx, INFINITE);
  try
    Method(nil);
  finally
    ReleaseMutex(SyncMtx);
  end;
end;

procedure SynchronizeParams(Method: TJvNotifyParamsEvent; P: Pointer);
begin
  WaitForSingleObject(SyncMtx, INFINITE);
  try
    Method(nil, P);
  finally
    ReleaseMutex(SyncMtx);
  end;
end;

//=== { TJvCustomThreadDialogOptions } =======================================

constructor TJvCustomThreadDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create;
  FOwner := AOwner;
  FShowDialog := False;
  FShowModal := True;
  FShowDelay := 0;
end;

procedure TJvCustomThreadDialogOptions.SetShowDelay(const Value: Integer);
begin
  FShowDelay := Value;
  if FShowDelay < 0 then
    FShowDelay := 0;
end;

procedure TJvCustomThreadDialogOptions.SetShowDialog(Value: Boolean);
begin
  FShowDialog := Value;
end;

procedure TJvCustomThreadDialogOptions.SetShowModal(Value: Boolean);
begin
  FShowModal := Value;
end;

//=== { TJvCustomThreadDialogForm } ==========================================

constructor TJvCustomThreadDialogForm.CreateNew(AOwner: TComponent; Dummy:
    Integer = 0);
begin
  inherited CreateNew(AOwner, Dummy);
  FInternalTimerInterval := 250;
  if AOwner is TJvThread then
    FConnectedThread := TJvThread(AOwner)
  else
    raise EJVCLException.CreateRes(@RsENotATJvThread);
  inherited OnShow := ReplaceFormShow;
  inherited OnClose := ReplaceFormClose;
  inherited OnCloseQuery := ReplaceFormCloseQuery;
  FInternalTimer := TTimer.Create(Self);
  FInternalTimer.OnTimer := InternalTimer;
  FInternalTimer.Interval := FInternalTimerInterval;
  FInternalShowDelay := 0;
  FFormIsShown := False;
end;

constructor TJvCustomThreadDialogForm.CreateNewFormStyle(AOwner: TJvThread; FormStyle: TFormStyle;
  Parent: TWinControl = nil);
begin
  if FormStyle <> fsStayOnTop then
    if Assigned(Parent) then
      FParentHandle := Parent.Handle
    else
      FParentHandle := 0;
  CreateNew(AOwner);
end;

destructor TJvCustomThreadDialogForm.Destroy;
begin
  FreeAndNil(FInternalTimer);
  inherited Destroy;
end;

procedure TJvCustomThreadDialogForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FParentHandle <> 0 then
    Params.WndParent := FParentHandle;
end;

procedure TJvCustomThreadDialogForm.DefaultCancelBtnClick(Sender: TObject);
begin
  if Assigned(FOnPressCancel) then
    FOnPressCancel(Sender);
  if Assigned(FConnectedThread) then
    FConnectedThread.CancelExecute;
  ModalResult := mrNone;
end;

procedure TJvCustomThreadDialogForm.InitializeFormContents;
begin
end;

procedure TJvCustomThreadDialogForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FConnectedDataComponent then
      FConnectedDataComponent := nil;
end;

procedure TJvCustomThreadDialogForm.InternalTimer(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    if not Assigned(ConnectedThread) then
    begin
      Hide; // no connected component
      Close;
    end
    else  // connected component present
      if ConnectedThread.Terminated then
      begin
        if FormIsShown then
        begin
          Hide;
          Close;
        end;
      end
      else // not terminated
      begin
        if FInternalShowDelay > 0 then // Dialog is not shown until yet
          FInternalShowDelay := FInternalShowDelay  - FInternalTimerInterval
        else
          if not FormIsShown then
          begin
            if ConnectedThread.ThreadDialogAllowed then
            begin
              if DialogOptions.ShowModal then
                ShowModal
              else
                Show;
            end;
          end
          else
            if ConnectedThread.ThreadDialogAllowed then
              UpdateFormContents;
      end;   // not terminated
  end;   // if not (csDestroying in ComponentState) then
end;

procedure TJvCustomThreadDialogForm.ReplaceFormClose(Sender: TObject; var Action: TCloseAction);
begin
  FFormIsShown := False;
  FInternalTimer.OnTimer := nil;
  FInternalTimer.Enabled := False;
  Action := caFree;
  if Assigned(FOnClose) then
    FOnClose(Sender, Action);
end;

procedure TJvCustomThreadDialogForm.ReplaceFormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FConnectedThread) then
    CanClose := not FConnectedThread.OneThreadIsRunning
  else
    CanClose := True;
  if CanClose then
    if Assigned(FOnCloseQuery) then
      FOnCloseQuery(Sender, CanClose);
end;

procedure TJvCustomThreadDialogForm.ReplaceFormShow(Sender: TObject);
begin
  FFormIsShown := True;
  InitializeFormContents;
  UpdateFormContents;
  FInternalTimer.Enabled := True;
end;

procedure TJvCustomThreadDialogForm.TransferDialogOptions;
begin
  if Assigned(DialogOptions) then
    fInternalShowDelay := DialogOptions.ShowDelay;
end;

procedure TJvCustomThreadDialogForm.UpdateFormContents;
begin
end;

procedure TJvCustomThreadDialogForm.SetConnectedDataComponent(Value: TComponent);
begin
  FConnectedDataComponent := Value;
  // (rom) Huh? Does this make sense?
  if Assigned(Value) then
    FreeNotification(Value);
end;

procedure TJvCustomThreadDialogForm.SetConnectedDataObject(Value: TObject);
begin
  FConnectedDataObject := Value;
end;

procedure TJvCustomThreadDialogForm.SetInternalTimerInterval(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  FInternalTimerInterval := Value;
  FInternalTimer.Interval := Value;
end;

procedure TJvCustomThreadDialogForm.SetOnClose(Value: TCloseEvent);
begin
  FOnClose := Value;
end;

//=== { TJvCustomThreadDialog } ==============================================

constructor TJvCustomThreadDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDialogOptions := CreateDialogOptions;
end;

destructor TJvCustomThreadDialog.Destroy;
begin
  FDialogOptions.Free;
  inherited Destroy;
end;

//=== { TJvThread } ==========================================================

constructor TJvThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRunOnCreate := True;
  FExclusive := True;
  FFreeOnTerminate := True;
  FThreads := TThreadList.Create;
  FListLocker := TCriticalSection.Create;
  FPriority := tpNormal;
  FThreadDialogAllowed := True;
end;

destructor TJvThread.Destroy;
begin
  Terminate;
  while OneThreadIsRunning do
  begin
    Sleep(1);
    {$IFDEF COMPILER6_UP}
    // Delphi 5 uses SendMessage -> no need for this code
    // Delphi 6+ uses an event and CheckSynchronize
    CheckSynchronize; // TThread.OnTerminate is synchronized
    {$ENDIF COMPILER6_UP}
  end;
  FThreads.Free;
  FListLocker.Free;
  inherited Destroy;
end;

procedure TJvThread.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FThreadDialog then
      FThreadDialog := nil
    else
    if AComponent = FThreadDialogForm then
      FThreadDialogForm := nil
end;

function TJvThread.Execute(P: Pointer): TJvBaseThread;
var
  BaseThread: TJvBaseThread;
begin
  BaseThread := nil;
  if not ((Exclusive and OneThreadIsRunning) or ((FMaxCount > 0) and (Count >= FMaxCount))) and
     Assigned(FOnExecute) then
  begin
    try
      BaseThread := TJvBaseThread.Create(Self, FOnExecute, P);
      BaseThread.FreeOnTerminate := FFreeOnTerminate;
      BaseThread.OnShowMessageDlgEvent := OnShowMessageDlgEvent;
      BaseThread.Priority := Priority;
      BaseThread.OnTerminate := DoTerminate;
      FThreads.Add(BaseThread);
      DoBegin;
    except
      // We can't terminate right now due to discrepancy between old and recent versions of TThread
      if Assigned(BaseThread) then
        BaseThread.FInternalTerminate := True;
    end;

    if FRunOnCreate and Assigned(BaseThread) then
      Resume(BaseThread);
  end;
  Result := BaseThread;
end;

procedure TJvThread.DoBegin;
begin
  if Assigned(FOnBegin) then
    FOnBegin(Self);
end;

procedure TJvThread.ExecuteAndWait(P: Pointer);
var
  B: Boolean;
  Thread: TJvBaseThread;
begin
  B := FRunOnCreate;
  FRunOnCreate := True;
  try
    Thread := Execute(P);
  finally
    FRunOnCreate := B;
  end;

  if Assigned(Thread) then
    WaitFor;  // all threads in list
end;

procedure TJvThread.ExecuteThreadAndWait(P: Pointer);
var
  B: Boolean;
  Thread: TJvBaseThread;
begin
  B := FRunOnCreate;
  FRunOnCreate := True;
  try
    Thread := Execute(P);
    if Assigned(Thread) then
      while(not Thread.Finished) do  // wait for this thread
        Application.HandleMessage;
  finally
    FRunOnCreate := B;
  end;
end;

procedure TJvThread.Resume(BaseThread: TJvBaseThread);
var
 B: Boolean;
begin
  if Assigned(BaseThread) then
  begin
    B := BaseThread.FOnResumeDone;
    BaseThread.Resume;
    if (not B) and (not BaseThread.FInternalTerminate) and
       (not BaseThread.Finished) then
     CreateThreadDialogForm;
  end
  else
    Resume; // no target, resume all
end;

procedure TJvThread.Resume; // All
var
  List: TList;
  I: Integer;
  Thread: TJvBaseThread;
begin
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Thread := TJvBaseThread(List[I]);
      while Thread.Suspended do
        Resume(Thread);
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TJvThread.Suspend;
var
  List: TList;
  I: Integer;
  Thread: TJvBaseThread;
begin
  Thread := GetCurrentThread;
  if Assigned(Thread) then
    Thread.Suspend // suspend itself
  else
  begin
    List := FThreads.LockList;  // suspend all
    try
      for I := 0 to List.Count - 1 do
        try  // against "Access denied" for already finished threads
          Thread := TJvBaseThread(List[I]);
          if not Thread.Finished then // it's faster (prevents raising exceptions in most cases)
            Thread.Suspend;
        except
        end;
    finally
      FThreads.UnlockList;
    end;
  end;
end;

procedure TJvThread.Terminate;
var
  List: TList;
  I: Integer;
begin
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
      TJvBaseThread(List[I]).Terminate;
    Resume; // All
  finally
    FThreads.UnlockList;
  end;
end;

procedure TJvThread.CancelExecute;
begin
  Terminate;
end;

procedure TJvThread.DoTerminate(Sender: TObject);
begin
  TJvBaseThread(Sender).FExecuteIsActive := False;
  if Assigned(FOnFinish) then
    try
      FOnFinish(Sender);
    except
      // DoTerminate is part of destructor; destructor should not raise exceptions
    end;

  if TJvBaseThread(Sender).FreeOnTerminate then
    FThreads.Remove(Sender);
  TJvBaseThread(Sender).FFinished := True;

  if Count = 0 then
  begin
    if Assigned(ThreadDialogForm) then
      ThreadDialogForm.Close;
    if Assigned(FOnFinishAll) then
      try
        FOnFinishAll(Self);
      except
      // DoTerminate is part of destructor; destructor should not raise exceptions
      end;
  end;
end;

procedure TJvThread.RemoveZombie(BaseThread: TJvBaseThread); // remove finished thread (where FreeOnTerminate was false)
begin
  if Assigned(BaseThread) then
  begin
    if BaseThread.FFinished and (not BaseThread.FreeOnTerminate) then
    begin
      FThreads.Remove(BaseThread);
      BaseThread.Free;
    end;
  end
  else
    RemoveZombie; // no target, do for all
end;

procedure TJvThread.RemoveZombie; // remove all finished threads (where FreeOnTerminate was false)
var
  List: TList;
  I: Integer;
  Thread: TJvBaseThread;
begin
  List := FThreads.LockList;
  try
    for I := List.Count - 1 downto 0 do
    begin
      Thread := TJvBaseThread(List[I]);
      if Thread.FFinished and (not Thread.FreeOnTerminate) then
      begin
        FThreads.Remove(Thread);
        Thread.Free;
      end;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

function TJvThread.GetTerminated: Boolean;
var
  H: DWORD;
  List: TList;
  I: Integer;
  Thread: TJvBaseThread;
begin
  H := GetCurrentThreadID;
  Result := True;
  List:=FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Thread := TJvBaseThread(List[I]);
      if Thread.ThreadID = H then
      begin
        Result := Thread.Terminated; // context of thread in list
        Break;
      end
      else
        Result := Result and Thread.Terminated; // context of all other threads
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TJvThread.WaitFor;
begin
  while OneThreadIsRunning do
    Application.HandleMessage;
end;

procedure TJvThread.SetReturnValue(RetVal: Integer);
var
  Thread: TJvBaseThread;
begin
  Thread := GetCurrentThread;
  if Assigned(Thread) then
    Thread.ReturnValue := RetVal;
end;

function TJvThread.GetReturnValue: Integer;
var
  Thread: TJvBaseThread;
begin
  Thread := GetCurrentThread;
  if Assigned(Thread) then
    Result := Thread.ReturnValue
  else
    Result := 0;
end;

function TJvThread.GetCount: Integer;
var
  List: TList;
begin
  List := FThreads.LockList;
  try
    Result := List.Count;
  finally
    FThreads.UnlockList;
  end;
end;

function TJvThread.GetCurrentThread: TJvBaseThread;
var
  H: DWORD;
  List: TList;
  I: Integer;
  Thread: TJvBaseThread;
begin
  Result := nil;
  H := GetCurrentThreadID;
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Thread := TJvBaseThread(List[I]);
      if Thread.ThreadID = H then
      begin
        Result := Thread;
        Break;
      end;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

function TJvThread.GetOneThreadIsRunning: Boolean;
var
  I: Integer;
  List: TList;
begin
  Result := False;
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Result := not TJvBaseThread(List[I]).Finished;
      if Result then
        Break;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TJvThread.Lock;   // for safe use of property Threads[]
begin
 FListLocker.Acquire;
 try
   if not Assigned(FLockedList) then
     FLockedList := FThreads.LockList;
 except
   FListLocker.Release;
   raise;
 end;
end;

function TJvThread.GetThreads(Index: Integer): TJvBaseThread;
begin
  FListLocker.Acquire;
  try
    if Assigned(FLockedList) then
      Result := TJvBaseThread(FLockedList[Index])
    else
      Result := nil;
  finally
   FListLocker.Release;
 end;
end;

procedure TJvThread.Unlock;
begin
 try
   if Assigned(FLockedList) then
   begin
     FThreads.UnlockList;
     FLockedList := nil;
   end;
 finally
   FListLocker.Release;
 end;
end;

procedure TJvThread.Synchronize(Method: TThreadMethod);
var
  Thread: TJvBaseThread;
begin
 Thread := GetCurrentThread;
 if Assigned(Thread) then
   Thread.Synchronize(Method);
end;

function TJvThread.SynchMessageDlg(const Msg: string; AType: TMsgDlgType;
  AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
var
 Thread: TJvBaseThread;
begin
  Thread := GetCurrentThread;
  if Assigned(Thread) then
    Result := Thread.SynchMessageDlg(Msg, AType, AButtons, HelpCtx)
  else
    Result := 0;
end;

// new
{$IFDEF UNIX}
// not tested
procedure TJvThread.SetPolicy(Policy: Integer);
var
  List: TList;
  Thread: TJvBaseThread;
  I: Integer;
begin
  List := FThreads.LockList;
  try
    Thread := GetCurrentThread;
    if Assigned(Thread) then
      SetThreadPolicy(Thread.Handle, Policy)  // context of thread in list
    else
      for I := 0 to List.Count - 1 do    // context of all other threads
        SetThreadPolicy(TJvBaseThread(List[I]).Handle, Policy);
    end;
  finally
    FThreads.UnlockList;
  end;
end;
{$ENDIF UNIX}

procedure TJvThread.SetPriority(NewPriority: TThreadPriority);
var
  List: TList;
  Thread: TJvBaseThread;
  I: Integer;
begin
  List := FThreads.LockList;
  try
    Thread := GetCurrentThread;
    if Assigned(Thread) then
      Thread.Priority := NewPriority   // context of thread in list
    else
    begin
      for I := 0 to List.Count - 1 do    // context of all other threads
        TJvBaseThread(List[I]).Priority := NewPriority;
      Priority := NewPriority;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

procedure TJvThread.CreateThreadDialogForm;
begin
  if Assigned(ThreadDialog) and not Assigned(FThreadDialogForm) then
  begin
    FThreadDialogForm := ThreadDialog.CreateThreadDialogForm(Self);
    if Assigned(FThreadDialogForm) then
    begin
      FreeNotification(FThreadDialogForm);
      FThreadDialogForm.TransferDialogOptions;
      InternalAfterCreateDialogForm(FThreadDialogForm);
      if ThreadDialog.DialogOptions.ShowDelay <= 0 then
      begin
        if ThreadDialog.DialogOptions.ShowModal then
          FThreadDialogForm.ShowModal
        else
          FThreadDialogForm.Show;
      end;
    end;
  end;
end;

procedure TJvThread.ExecuteWithDialog(P: Pointer);
begin
  if Assigned(ThreadDialog) and ThreadDialog.DialogOptions.ShowDialog and
    ThreadDialog.DialogOptions.ShowModal then
    ExecuteAndWait(P)
  else
    Execute(P);
end;

procedure TJvThread.InternalAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm);
begin
  if Assigned(FAfterCreateDialogForm) then
    FAfterCreateDialogForm(DialogForm);
end;

procedure TJvThread.TerminateWaitFor(iRemoveZombies: Boolean = true);
begin
  Terminate;
  WaitFor;
  if iRemoveZombies then
    RemoveZombie;
end;

//=== { TJvBaseThread } ======================================================

constructor TJvBaseThread.Create(Sender: TObject; Event: TJvNotifyParamsEvent; Params: Pointer);
begin
  inherited Create(True);
  FSender := Sender;
  FExecuteEvent := Event;
  FParams := Params;
end;

destructor TJvBaseThread.Destroy;
begin
  inherited Destroy;
end;

procedure TJvBaseThread.ExceptionHandler;
begin
  ShowException(FException, FExceptionAddr);
end;

procedure TJvBaseThread.Resume;
begin
  if not FOnResumeDone then
  begin
    // the first resume (perhaps deferred)
    FOnResumeDone := True;
    if (FSender is TJvThread) and Assigned(TJvThread(FSender).BeforeResume) then
      try
        TJvThread(FSender).BeforeResume(Self);
      except
        // Self.Terminate;
        // We can't terminate right now due to discrepancy between old and recent versions TThread
        FInternalTerminate := True;
      end;
    FExecuteIsActive := True;
  end;
  inherited Resume;     // after suspend too
end;

procedure TJvBaseThread.Execute;
begin
  try
    FExecuteIsActive := True;
    if FInternalTerminate then
      Self.Terminate;
    FExecuteEvent(Self, FParams);
  except
    on E: Exception do
    begin
      FException := E;
      FExceptionAddr := ExceptAddr;
      Self.Synchronize(ExceptionHandler);
    end;
  end;
end;

procedure TJvBaseThread.Synchronize(Method: TThreadMethod);
begin
 inherited Synchronize(Method);
end;

procedure TJvBaseThread.InternalMessageDlg;
begin
  if Assigned(OnShowMessageDlgEvent) then
    OnShowMessageDlgEvent(FSynchMsg, FSynchAType,
     FSynchAButtons, FSynchHelpCtx, FSynchMessageDlgResult)
  else
    FSynchMessageDlgResult := JvDSADialogs.MessageDlg(FSynchMsg, FSynchAType, FSynchAButtons, FSynchHelpCtx);
end;

function TJvBaseThread.SynchMessageDlg(const Msg: string; AType: TMsgDlgType;
  AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
begin
  FSynchMsg := Msg;
  FSynchAType := AType;
  FSynchAButtons := AButtons;
  FSynchHelpCtx := HelpCtx;
  Self.Synchronize(InternalMessageDlg);
  Result := FSynchMessageDlgResult;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  SyncMtx := CreateMutex(nil, False, 'VCLJvThreadMutex');

finalization
  CloseHandle(SyncMtx);
  SyncMtx := 0;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
