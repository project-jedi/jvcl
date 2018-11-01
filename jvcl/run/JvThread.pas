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
located at http://jvcl.delphi-jedi.org

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
  Windows, SysUtils, Classes, SyncObjs, Controls, ExtCtrls, Forms, Dialogs,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.Types,
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}  
  JvTypes, JvComponentBase, JvComponent;

type
  // TThreadPriority has been marked platform and we don't want the warning
  {$IFDEF RTL230_UP}{$IFDEF MSWINDOWS}{$WARNINGS OFF}TThreadPriority = Classes.TThreadPriority;{$WARNINGS ON}{$ENDIF RTL230_UP}{$ENDIF MSWINDOWS}

  TJvCustomThreadDialog = class;
  TJvCustomThreadDialogForm = class;
  TJvThread = class;

  TJvCustomThreadDialogFormEvent = procedure(DialogForm: TJvCustomThreadDialogForm) of object;
  TJvThreadCancelEvent = procedure(CurrentThread: TJvThread) of object;
  TJvThreadExceptionEvent = procedure (Sender: TObject; E: Exception; EAddr: Pointer) of object;

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
  public
    constructor Create(AOwner: TJvCustomThreadDialog); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property FormStyle: TFormStyle read FFormStyle write FFormStyle;
    // Delay in milliseconds for starting the thread dialog
    property ShowDelay: Integer read FShowDelay write SetShowDelay default 0;
    // Flag if there should be a dialog which shows the thread status
    property ShowDialog: Boolean read FShowDialog write SetShowDialog default False;
    // Flag if the status dialog is modal
    property ShowModal: Boolean read FShowModal write SetShowModal default True;
  end;

  TJvCustomThreadDialogForm = class(TJvForm)
  private
    FConnectedDataObject: TObject;
    FConnectedThread: TJvThread;
    FDialogOptions: TJvCustomThreadDialogOptions;
    FFormControlsCreated: Boolean;
    FFormIsShown: Boolean;
    FInternalShowDelay: Integer;
    FInternalTimer: TTimer;
    FInternalTimerInterval: Integer;
    FOnClose: TCloseEvent;
    FSaveOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnPressCancel: TJvThreadCancelEvent;
    FOnShow: TNotifyEvent;
    FSaveOnShow: TNotifyEvent;
    FParentHandle: HWND;
    procedure CloseThreadForm;
    function GetConnectedDataComponent: TComponent;
    procedure SetConnectedDataComponent(Value: TComponent);
    procedure SetConnectedDataObject(Value: TObject);
    procedure SetInternalTimerInterval(Value: Integer);
    procedure SetOnClose(Value: TCloseEvent);
    procedure OnInternalTimer(Sender: TObject); virtual;
  protected
    procedure CreateFormControls; virtual;
    procedure FreeFormControls; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure InitializeFormContents; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReplaceFormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReplaceFormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ReplaceFormShow(Sender: TObject);
    procedure TransferDialogOptions; virtual;
    procedure UpdateFormContents; virtual;
    property FormControlsCreated: Boolean read FFormControlsCreated;
    property FormIsShown: Boolean read FFormIsShown default False;
    property OnPressCancel: TJvThreadCancelEvent read FOnPressCancel write FOnPressCancel;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    constructor CreateNewFormStyle(AOwner: TJvThread; FormStyle: TFormStyle;
      Parent: TWinControl = nil); virtual;
    destructor Destroy; override;
    procedure DefaultCancelBtnClick(Sender: TObject);
    property ConnectedDataComponent: TComponent read GetConnectedDataComponent write SetConnectedDataComponent;
    property ConnectedDataObject: TObject read FConnectedDataObject write SetConnectedDataObject;
    property ConnectedThread: TJvThread read FConnectedThread;
    property DialogOptions: TJvCustomThreadDialogOptions read FDialogOptions write FDialogOptions;
    property InternalTimerInterval: Integer read FInternalTimerInterval write SetInternalTimerInterval;
  published
    property OnClose: TCloseEvent read FOnClose write SetOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TJvCustomThreadDialog = class(TJvComponent)
  private
    FDialogOptions: TJvCustomThreadDialogOptions;
    FOnPressCancel: TJvThreadCancelEvent;
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; virtual; abstract;
  published
    property DialogOptions: TJvCustomThreadDialogOptions read FDialogOptions write FDialogOptions;
    property OnPressCancel: TJvThreadCancelEvent read FOnPressCancel write FOnPressCancel;
  end;

  TJvThreadShowMessageDlgEvent = procedure(const Msg: string; AType: TMsgDlgType;
    AButtons: TMsgDlgButtons; HelpCtx: Longint; var DlgResult: Word) of object;

  // This thread is a descendent of TThread but proposes a different
  // behaviour with regard to being suspended or resumed.
  // Indeed, the MSDN recommends not to use them and it was even noticed
  // that using Suspend and Resume under Windows NT, 2K and XP led to weird
  // errors such as being refused access to the thread, despite being its
  // creator.
  // So another mechanism has been implemented: the thread must be
  // paused instead of suspended.
  // Pausing the thread actually acquires a critical section which the Execute
  // function must try to get before it calls InternalExecute.
  // Hence, if the critical section was acquired before this try, the Execute
  // function is stopped and the thread paused until another thread (the main
  // thread in most cases) releases the critical section when setting
  // Paused to false.
  // Obviously, the Execute method in derived classes has to be cooperative
  // and actually acquire and release the FPauseSection critical section via
  // the appropriate protected methods
  TJvPausableThread = class(TJvCustomThread)
  private
    FPauseSection: TCriticalSection;
    FPaused: Boolean;

    procedure SetPaused(const Value: Boolean);
  protected
    procedure EnterUnpauseableSection;
    procedure LeaveUnpauseableSection;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    property Paused: Boolean read FPaused write SetPaused;
  end;

  TJvBaseThread = class(TJvPausableThread)
  private
    FException: Exception;
    FExceptionAddr: Pointer;
    FInternalTerminate: Boolean;
    FExecuteEvent: TJvNotifyParamsEvent;
    FOnResumeDone: Boolean;
    FExecuteIsActive: Boolean;
    FFinished: Boolean;
    FOnShowMessageDlgEvent: TJvThreadShowMessageDlgEvent;
    FOnException: TJvThreadExceptionEvent;
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
    {$IFNDEF COMPILER14_UP}
    procedure Resume; // There is no way to silence the compiler ("Resume" is deprecated)
    {$ENDIF ~COMPILER14_UP}
    procedure ResumeThread;
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
    property OnShowMessageDlgEvent: TJvThreadShowMessageDlgEvent read FOnShowMessageDlgEvent write FOnShowMessageDlgEvent;
    property OnException: TJvThreadExceptionEvent read FOnException write FOnException;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvThread = class(TJvComponent)
  private
    FAfterCreateDialogForm: TJvCustomThreadDialogFormEvent;
    FBeforeResume: TNotifyEvent;
    FConnectedDataObject: TObject;
    FDisalbeDialogShowDelayCounter: Integer;
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
    FOnCancelExecute: TJvThreadCancelEvent;
    FOnShowMessageDlgEvent: TJvThreadShowMessageDlgEvent;
    FOnException: TJvThreadExceptionEvent;
    {$IFDEF MSWINDOWS}
    FPriority: TThreadPriority;
    {$ENDIF MSWINDOWS}
    FThreadDialog: TJvCustomThreadDialog;
    FThreadDialogAllowed: Boolean;
    FThreadDialogForm: TJvCustomThreadDialogForm;
    FThreadName: String;
    procedure DoBegin;
    procedure DoTerminate(Sender: TObject);
    function GetCount: Integer;
    function GetThreads(Index: Integer): TJvBaseThread;
    function GetTerminated: Boolean; // in context of thread in list - for itself; in others - for all threads in list
    procedure SetReturnValue(RetVal: Integer); // in context of thread in list - set return value (slower)
    function GetReturnValue: Integer;  // in context of thread in list - get return value (slower)
    procedure CreateThreadDialogForm;
    procedure CloseThreadDialogForm;
    function GetConnectedDataComponent: TComponent;
    function GetCurrentThread: TJvBaseThread;
    procedure SetConnectedDataComponent(Value: TComponent);
    procedure SetConnectedDataObject(Value: TObject);
    procedure SetThreadDialog(const Value: TJvCustomThreadDialog);
    procedure SetThreadName(const Value: String);
    procedure ShowThreadDialogForm;
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
    function SynchMessageDlg(const Msg: string; AType: TMsgDlgType; AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;

    procedure Lock;   // for safe use of property Threads[]
    procedure Unlock;

    property ConnectedDataComponent: TComponent read GetConnectedDataComponent write SetConnectedDataComponent;
    property ConnectedDataObject: TObject read FConnectedDataObject write SetConnectedDataObject;
    property Count: Integer read GetCount;
    property Threads[Index: Integer]: TJvBaseThread read GetThreads;
    property LastThread: TJvBaseThread read GetCurrentThread; //GetLastThread;
    property Terminated: Boolean read GetTerminated; // in context of thread in list - for itself; in others - for all threads in list
    property ReturnValue: Integer read GetReturnValue write SetReturnValue; // in context of thread in list - set return value (slower)
    property OneThreadIsRunning: Boolean read GetOneThreadIsRunning;
    // Property to allow/disallow the thread dialog form
    property ThreadDialogAllowed: Boolean read FThreadDialogAllowed write FThreadDialogAllowed default True;
    property ThreadDialogForm: TJvCustomThreadDialogForm read FThreadDialogForm;
    function CalcThreadName(ThreadPos: Integer): String;
    // Disables the delayed showing of the thread dialog
    procedure DisableDialogShowDelay;
    // Enables the delayed showing of the thread dialog
    procedure EnableDialogShowDelay;
    // Is the delayed showing of the thread dialog disabled
    function IsDialogShowDelayDisabled: Boolean;
    {$IFDEF MSWINDOWS}
    procedure SetPriority(NewPriority: TThreadPriority); // in context of thread in list - for itself; in other contexts - for all threads in list
    {$ENDIF MSWINDOWS}
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
    {$IFDEF MSWINDOWS}
    property Priority: TThreadPriority read FPriority write FPriority default tpNormal;
    {$ENDIF MSWINDOWS}
    property ThreadDialog: TJvCustomThreadDialog read FThreadDialog write SetThreadDialog;
    property ThreadName: String read FThreadName write SetThreadName;
    property AfterCreateDialogForm: TJvCustomThreadDialogFormEvent read FAfterCreateDialogForm write FAfterCreateDialogForm;
    property BeforeResume: TNotifyEvent read FBeforeResume write FBeforeResume;
    property OnBegin: TNotifyEvent read FOnBegin write FOnBegin;
    property OnCancelExecute: TJvThreadCancelEvent read FOnCancelExecute write FOnCancelExecute;
    property OnExecute: TJvNotifyParamsEvent read FOnExecute write FOnExecute;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnFinishAll: TNotifyEvent read FOnFinishAll write FOnFinishAll;
    property OnShowMessageDlgEvent: TJvThreadShowMessageDlgEvent read FOnShowMessageDlgEvent write FOnShowMessageDlgEvent;
    property OnException: TJvThreadExceptionEvent read FOnException write FOnException;
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
  JvResources, JvJVCLUtils;

//=== { TJvCustomThreadDialogOptions } =======================================

constructor TJvCustomThreadDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create;
  FOwner := AOwner;
  FShowDialog := False;
  FShowModal := True;
  FShowDelay := 0;
end;

procedure TJvCustomThreadDialogOptions.Assign(Source: TPersistent);
begin
  if Source is TJvCustomThreadDialogOptions then
    begin
      FormStyle := TJvCustomThreadDialogOptions(Source).FormStyle;
      ShowDialog := TJvCustomThreadDialogOptions(Source).ShowDialog;
      ShowDelay := TJvCustomThreadDialogOptions(Source).ShowDelay;
      ShowModal := TJvCustomThreadDialogOptions(Source).ShowModal;
    end
  else
    Inherited Assign(Source);
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

constructor TJvCustomThreadDialogForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited CreateNew(AOwner, Dummy);
  FInternalTimerInterval := 500;
  if AOwner is TJvThread then
    FConnectedThread := TJvThread(AOwner)
  else
    raise EJVCLException.CreateRes(@RsENotATJvThread);
  FSaveOnShow := inherited OnShow;
  FSaveOnClose := inherited OnClose;
  inherited OnShow := ReplaceFormShow;
  inherited OnClose := ReplaceFormClose;
  inherited OnCloseQuery := ReplaceFormCloseQuery;
  FInternalTimer := TTimer.Create(Self);
  FInternalTimer.OnTimer := OnInternalTimer;
  FInternalTimer.Interval := FInternalTimerInterval;
  FInternalShowDelay := 0;
  FFormIsShown := False;
  FFormControlsCreated := False;
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
  FreeFormControls;
  FreeAndNil(FInternalTimer);
  inherited Destroy;
end;

procedure TJvCustomThreadDialogForm.CloseThreadForm;
begin
  Hide;
  if fsModal in FormState then
    ModalResult := mrCancel
  else
    Close;
end;

procedure TJvCustomThreadDialogForm.CreateFormControls;
begin
  FFormControlsCreated := True;
end;

procedure TJvCustomThreadDialogForm.FreeFormControls;
begin
  FFormControlsCreated := False;
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
    FOnPressCancel(FConnectedThread)
  else
    if Assigned(FConnectedThread) then
      FConnectedThread.CancelExecute;
  ModalResult := mrNone;
end;

function TJvCustomThreadDialogForm.GetConnectedDataComponent: TComponent;
begin
  if Assigned(FConnectedDataObject) and (FConnectedDataObject is TComponent) then
    Result := TComponent(ConnectedDataObject)
  else
    Result := nil;
end;

procedure TJvCustomThreadDialogForm.InitializeFormContents;
begin
end;

procedure TJvCustomThreadDialogForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FConnectedDataObject then
      ConnectedDataObject := nil;
end;

procedure TJvCustomThreadDialogForm.OnInternalTimer(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    if not Assigned(ConnectedThread) then
      CloseThreadForm
    else  // connected component present
      if ConnectedThread.Terminated or not ConnectedThread.OneThreadIsRunning then
      begin
        if FormIsShown then
          CloseThreadForm;
      end
      else // not terminated
      begin
        if FInternalShowDelay > 0 then // Dialog is not shown until yet
          FInternalShowDelay := FInternalShowDelay  - FInternalTimerInterval
        else
          if not FormIsShown then
          begin
            if ConnectedThread.ThreadDialogAllowed and not ConnectedThread.IsDialogShowDelayDisabled then
            begin
              if DialogOptions.ShowModal then
                ShowModal
              else
                Show;
            end;
          end
          else
            if ConnectedThread.ThreadDialogAllowed and FormIsShown then
              UpdateFormContents;
      end;   // not terminated
  end;   // if not (csDestroying in ComponentState) then
end;

procedure TJvCustomThreadDialogForm.ReplaceFormClose(Sender: TObject; var Action: TCloseAction);
begin
  FFormIsShown := False;
  if Assigned(FInternalTimer) then
    FInternalTimer.Enabled := False;
  Action := caFree;
  if Assigned(FOnClose) then
    FOnClose(Sender, Action);
  if Assigned(FSaveOnClose) then
    FSaveOnClose(Sender, Action);
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
  if not FormControlsCreated then
    CreateFormControls;
  InitializeFormContents;
  UpdateFormContents;
  FInternalTimer.Enabled := True;
  if Assigned(FOnShow) then
    FOnShow(Sender);
  if Assigned(FSaveOnShow) then
    FSaveOnShow(Sender);
end;

procedure TJvCustomThreadDialogForm.TransferDialogOptions;
begin
  if Assigned(DialogOptions) then
    fInternalShowDelay := DialogOptions.ShowDelay;
end;

procedure TJvCustomThreadDialogForm.UpdateFormContents;
begin
end;

procedure TJvCustomThreadDialogForm.SetConnectedDataComponent(Value:
    TComponent);
begin
  if Assigned(FConnectedDataObject) and (FConnectedDataObject is TComponent) then
    TComponent(FConnectedDataObject).RemoveFreeNotification(self);
  ConnectedDataObject := Value;
  if Assigned(FConnectedDataObject) and (FConnectedDataObject is TComponent) then
    TComponent(FConnectedDataObject).FreeNotification(self);
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
  {$IFDEF MSWINDOWS}
  FPriority := tpNormal;
  {$ENDIF MSWINDOWS}
  FThreadDialogAllowed := True;
  FDisalbeDialogShowDelayCounter := 0;
end;

destructor TJvThread.Destroy;
begin
  Terminate;
  while OneThreadIsRunning do
  begin
    Sleep(1);
    // Delphi 6+ uses an IPC event and CheckSynchronize
    CheckSynchronize; // TThread.OnTerminate is synchronized
  end;
  FThreads.Free;
  FListLocker.Free;
  inherited Destroy;
end;

function TJvThread.CalcThreadName(ThreadPos: Integer): String;
begin
  if ThreadName = '' then
    Result := Name
  else
    Result := ThreadName;
  if Result = '' then
    Result := ClassName;
  if ThreadPos > 0 then
    Result := Result + ' ['+Inttostr(ThreadPos)+']';
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
    else
    if AComponent = FConnectedDataObject then
      ConnectedDataObject := nil;
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
      BaseThread.OnException := OnException;
      {$IFDEF MSWINDOWS}
      BaseThread.Priority := Priority;
      {$ENDIF MSWINDOWS}
      BaseThread.OnTerminate := DoTerminate;
      BaseThread.ThreadName := CalcThreadName(Count);
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
  finally
    FRunOnCreate := B;
  end;
  if Assigned(Thread) then
  begin
    if GetCurrentThreadId = MainThreadID then
    begin
      while not Thread.Finished do  // wait for this thread
        Application.HandleMessage;
    end
    else
      while not Thread.Finished do  // wait for this thread
        Sleep(5);
  end;
end;

procedure TJvThread.Resume(BaseThread: TJvBaseThread);
var
  B: Boolean;
begin
  if Assigned(BaseThread) then
  begin
    CreateThreadDialogForm;
    B := BaseThread.FOnResumeDone;
    BaseThread.ResumeThread;
    if (not B) and
       (not BaseThread.FInternalTerminate) and
       (not BaseThread.Finished) then
    begin
      ShowThreadDialogForm;
    end;
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
    Thread.Suspended := True // suspend itself
  else
  begin
    List := FThreads.LockList;  // suspend all
    try
      for I := 0 to List.Count - 1 do
        try  // against "Access denied" for already finished threads
          Thread := TJvBaseThread(List[I]);
          if not Thread.Finished then // it's faster (prevents raising exceptions in most cases)
            Thread.Suspended := True
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
  if Assigned(fOnCancelExecute) then
    fOnCancelExecute (Self)
  else
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
    CloseThreadDialogForm;
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
  if GetCurrentThreadId = MainThreadID then
  begin
    while OneThreadIsRunning do
      Application.HandleMessage;
  end
  else
    while OneThreadIsRunning do
      Sleep(5);
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
  DisableDialogShowDelay;
  try
     Thread := GetCurrentThread;
     if Assigned(Thread) then
       Thread.Synchronize(Method)
     else
       Method;
  finally
    EnableDialogShowDelay;
  end;
end;

function TJvThread.SynchMessageDlg(const Msg: string; AType: TMsgDlgType; AButtons: TMsgDlgButtons; HelpCtx: Longint):
    Word;
var
 Thread: TJvBaseThread;
begin
  DisableDialogShowDelay;
  try
    Thread := GetCurrentThread;
    if Assigned(Thread) then
      Result := Thread.SynchMessageDlg(Msg, AType, AButtons, HelpCtx)
    else
      if Assigned(OnShowMessageDlgEvent) then
        OnShowMessageDlgEvent(Msg, AType,
         AButtons, HelpCtx, Result)
      else
        Result := MessageDlg(Msg, AType, AButtons, HelpCtx);
  finally
    EnableDialogShowDelay;
  end;
end;

{$IFDEF MSWINDOWS}
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
{$ENDIF MSWINDOWS}

procedure TJvThread.CreateThreadDialogForm;
begin
  if Assigned(ThreadDialog) and not Assigned(FThreadDialogForm) then
  begin
    FThreadDialogForm := ThreadDialog.CreateThreadDialogForm(Self);
    if Assigned(FThreadDialogForm) then
    begin
      FreeNotification(FThreadDialogForm);
      FThreadDialogForm.CreateFormControls;
      FThreadDialogForm.ConnectedDataObject := ConnectedDataObject;
      FThreadDialogForm.TransferDialogOptions;
      InternalAfterCreateDialogForm(FThreadDialogForm);
    end;
  end;
end;

procedure TJvThread.DisableDialogShowDelay;
begin
  Inc(FDisalbeDialogShowDelayCounter);
end;

procedure TJvThread.EnableDialogShowDelay;
begin
  Dec(FDisalbeDialogShowDelayCounter);
end;

procedure TJvThread.ExecuteWithDialog(P: Pointer);
begin
  if Assigned(ThreadDialog) and ThreadDialog.DialogOptions.ShowDialog and
     ThreadDialog.DialogOptions.ShowModal then
    ExecuteAndWait(P)
  else
    Execute(P);
end;

procedure TJvThread.CloseThreadDialogForm;
begin
  if Assigned(ThreadDialogForm) then
  begin
    while Assigned(ThreadDialogForm) AND ThreadDialogForm.Visible do
    begin
      ThreadDialogForm.CloseThreadForm;
      Application.HandleMessage;
    end;
  end;
end;

function TJvThread.GetConnectedDataComponent: TComponent;
begin
  if Assigned(FConnectedDataObject) and (FConnectedDataObject is TComponent) then
    Result := TComponent(ConnectedDataObject)
  else
    Result := nil;
end;

procedure TJvThread.InternalAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm);
begin
  if Assigned(FAfterCreateDialogForm) then
    FAfterCreateDialogForm(DialogForm);
end;

function TJvThread.IsDialogShowDelayDisabled: Boolean;
begin
  Result := FDisalbeDialogShowDelayCounter > 0;
end;

procedure TJvThread.SetConnectedDataComponent(Value: TComponent);
begin
  if Assigned(FConnectedDataObject) and (FConnectedDataObject is TComponent) then
    TComponent(FConnectedDataObject).RemoveFreeNotification(self);
  ConnectedDataObject := Value;
  if Assigned(FConnectedDataObject) and (FConnectedDataObject is TComponent) then
    TComponent(FConnectedDataObject).FreeNotification(self);
end;

procedure TJvThread.SetConnectedDataObject(Value: TObject);
begin
  FConnectedDataObject := Value;
  if Assigned(FThreadDialogForm) then
    FThreadDialogForm.ConnectedDataObject := Value;
end;

procedure TJvThread.SetThreadDialog(const Value: TJvCustomThreadDialog);
begin
  ReplaceComponentReference(Self, Value, TComponent(FThreadDialog));
end;

procedure TJvThread.SetThreadName(const Value: String);
var
  i: Integer;
begin
  FThreadName := Value;
  Lock;
  try
    for i := 0 to Count -1 do
      if Assigned(Threads[i]) then
        Threads[i].ThreadName := CalcThreadName(i);
  finally
    UnLock;
  end;
end;

procedure TJvThread.ShowThreadDialogForm;
begin
  if Assigned (ThreadDialog) and Assigned(FThreadDialogForm) then
    if ThreadDialog.DialogOptions.ShowDelay <= 0 then
    begin
      if ThreadDialog.DialogOptions.ShowModal then
        FThreadDialogForm.ShowModal
      else
        FThreadDialogForm.Show;
    end;
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

procedure TJvBaseThread.ExceptionHandler;
begin
  ShowException(FException, FExceptionAddr);
end;

procedure TJvBaseThread.ResumeThread;
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
  {$WARNINGS OFF}
  inherited Resume;     // after suspend too
  {$WARNINGS ON}
end;

{$IFNDEF COMPILER14_UP}
procedure TJvBaseThread.Resume;
begin
  ResumeThread;
end;
{$ENDIF ~COMPILER14_UP}

procedure TJvBaseThread.Execute;
begin
  try
    FExecuteIsActive := True;
    NameThread(ThreadName);
    if FInternalTerminate then
      Terminate;
    FExecuteEvent(Self, FParams);
  except
    on E: Exception do
    if Assigned(OnException) then
      OnException(self, E, ExceptAddr)
    else
    begin
      FException := E;
      FExceptionAddr := ExceptAddr;
      Synchronize(ExceptionHandler);
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
    FSynchMessageDlgResult := MessageDlg(FSynchMsg, FSynchAType, FSynchAButtons, FSynchHelpCtx);
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

{ TJvPausableThread }

constructor TJvPausableThread.Create(CreateSuspended: Boolean);
begin
  FPauseSection := TCriticalSection.Create;
  inherited Create(CreateSuspended);
end;

destructor TJvPausableThread.Destroy;
begin
  if Paused then
  begin
    Terminate;
    Paused := False;
  end;

  inherited Destroy;
  FPauseSection.Free;
end;

procedure TJvPausableThread.EnterUnpauseableSection;
begin
  FPauseSection.Acquire;
end;

procedure TJvPausableThread.LeaveUnpauseableSection;
begin
  FPauseSection.Release;
end;

procedure TJvPausableThread.SetPaused(const Value: Boolean);
begin
  if FPaused <> Value then
  begin
    // store the Value
    FPaused := Value;

    if FPaused then
      FPauseSection.Acquire
    else
      FPauseSection.Release;
  end;

  // If the thread was created "Suspended", then we must start it
  if Suspended and not Paused then
    Suspended := False;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
