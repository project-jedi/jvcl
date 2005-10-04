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
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows, Controls, ExtCtrls,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows, QControls, QExtCtrls,
  {$ENDIF UNIX}
  Forms, Dialogs,
  JvTypes, JvComponentBase;

type
  TJvCustomThreadDialog = class;
  TJvCustomThreadDialogForm = class;
  TJvThread = class;

  TJvCustomThreadDialogFormEvent = procedure(DialogForm: TJvCustomThreadDialogForm) of
    object;

  TJvCustomThreadDialogOptions = class(TPersistent)
  private
    FFormStyle: tFormStyle;
    FOwner: TJvCustomThreadDialog;
    FShowDialog: boolean;
    FShowModal: boolean;
  protected
    procedure SetShowDialog(Value: boolean);
    procedure SetShowModal(Value: boolean);
  public
    constructor Create(AOwner: TJvCustomThreadDialog); virtual;
  published
    property FormStyle: tFormStyle Read FFormStyle Write FFormStyle;
    property ShowDialog: boolean Read FShowDialog Write SetShowDialog default False;
    property ShowModal: boolean Read FShowModal Write SetShowModal default True;
  end;

  TJvCustomThreadDialogForm = class(TForm)
  private
    FConnectedDataComponent: TComponent;
    FConnectedDataObject: TObject;
    FConnectedThread: TJvThread;
    FDialogOptions: TJvCustomThreadDialogOptions;
    FInternalTimer: TTimer;
    FInternalTimerInterval: integer;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnShow: TNotifyEvent;
    FParentHandle: HWND;
    procedure SetConnectedDataComponent(Value: TComponent);
    procedure SetConnectedDataObject(Value: TObject);
    procedure SetInternalTimerInterval(Value: integer);
    procedure SetOnClose(Value: TCloseEvent);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure InitializeFormContents; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnInternalTimer(Sender: TObject); virtual;
    procedure TransferDialogOptions; virtual;
    procedure UpdateFormContents; virtual;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    constructor CreateNewFormStyle(AOwner: TJvThread; FormStyle: TFormStyle;
      Parent: TWinControl = nil); virtual;
    destructor Destroy; override;
    procedure DefaultCancelBtnClick(Sender: TObject);
    procedure ReplaceFormClose(Sender: TObject; var Action: TCloseAction);
    procedure ReplaceFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ReplaceFormShow(Sender: TObject);
    property ConnectedDataComponent: TComponent
      Read FConnectedDataComponent Write SetConnectedDataComponent;
    property ConnectedDataObject: TObject Read FConnectedDataObject
      Write SetConnectedDataObject;
    property ConnectedThread: TJvThread Read FConnectedThread;
    property DialogOptions: TJvCustomThreadDialogOptions
      Read FDialogOptions Write FDialogOptions;
    property InternalTimerInterval: integer
      Read FInternalTimerInterval Write SetInternalTimerInterval;
  published
    property OnClose: TCloseEvent Read FOnClose Write SetOnClose;
    property OnCloseQuery: TCloseQueryEvent Read FOnCloseQuery Write FOnCloseQuery;
    property OnShow: TNotifyEvent Read FOnShow Write FOnShow;
  end;

  TJvCustomThreadDialog = class(TJvComponent)
  private
    FDialogOptions: TJvCustomThreadDialogOptions;
    FOnPressCancel: TNotifyEvent;
    FThreadStatusDialog: TJvCustomThreadDialogForm;
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; virtual; abstract;
    //    property ThreadStatusDialog: TJvCustomThreadDialogForm
    //      Read FThreadStatusDialog Write FThreadStatusDialog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseThreadDialogForm;
    function CreateThreadDialogForm(ConnectedThread: TJvThread):
      TJvCustomThreadDialogForm; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property DialogOptions: TJvCustomThreadDialogOptions
      Read FDialogOptions Write FDialogOptions;
    property OnPressCancel: TNotifyEvent Read FOnPressCancel Write FOnPressCancel;
  end;

  TJvBaseThread = class(TThread)
  private
    FException: Exception;
    FExceptionAddr: Pointer;
    FExecuteEvent: TJvNotifyParamsEvent;
    FParams: Pointer;
    FSender: TObject;
    FSynchAButtons: TMsgDlgButtons;
    FSynchAType: TMsgDlgType;
    FSynchHelpCtx: longint;
    FSynchMessageDlgResult: word;
    FSynchMsg: string;
    procedure ExceptionHandler;
  protected
    procedure InternalMessageDlg;
  public
    constructor Create(Sender: TObject; Event: TJvNotifyParamsEvent;
      Params: Pointer); virtual;
    procedure Execute; override;
    function SynchMessageDlg(const Msg: string; AType: TMsgDlgType;
      AButtons: TMsgDlgButtons; HelpCtx: longint): word;
  end;

  TJvThread = class(TJvComponent)
  private
    FAfterCreateDialogForm: TJvCustomThreadDialogFormEvent;
    FThreadCount: integer;
    FThreads: TThreadList;
    FExclusive: boolean;
    FRunOnCreate: boolean;
    FOnBegin: TNotifyEvent;
    FOnExecute: TJvNotifyParamsEvent;
    FOnFinish: TNotifyEvent;
    FOnFinishAll: TNotifyEvent;
    FFreeOnTerminate: boolean;
    FThreadDialog: TJvCustomThreadDialog;
    FThreadDialogForm: TJvCustomThreadDialogForm;
    procedure DoCreate;
    procedure DoTerminate(Sender: TObject);
    function GetCount: integer;
    function GetThreads(Index: integer): TJvBaseThread;
    function GetTerminated: boolean;
    procedure CreateThreadDialogForm;
    function GetLastThread: TJvBaseThread;
  protected
    procedure intAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm);
      virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelExecute; virtual;
    function Execute(P: Pointer): THandle;
    procedure ExecuteAndWait(P: Pointer);
    procedure ExecuteWithDialog(P: Pointer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure Synchronize(Method: TThreadMethod);
    function SynchMessageDlg(const Msg: string; AType: TMsgDlgType;
      AButtons: TMsgDlgButtons; HelpCtx: longint): word;

    property Count: integer Read GetCount;
    property Threads[Index: integer]: TJvBaseThread Read GetThreads;
    property LastThread: TJvBaseThread Read GetLastThread;
    property ThreadDialogForm: TJvCustomThreadDialogForm Read FThreadDialogForm;
  published
    function OneThreadIsRunning: boolean;
    function GetPriority(Thread: THandle): TThreadPriority;
    procedure SetPriority(Thread: THandle; Priority: TThreadPriority);
    {$IFDEF UNIX}
    function GetPolicy(Thread: THandle): integer;
    procedure SetPolicy(Thread: THandle; Policy: integer);
    {$ENDIF UNIX}
    procedure QuitThread(Thread: THandle);
    procedure Suspend(Thread: THandle); // should not be used
    procedure Resume(Thread: THandle);
    procedure Terminate; // terminates all running threads
    property Terminated: boolean Read GetTerminated;
    property Exclusive: boolean Read FExclusive Write FExclusive;
    property RunOnCreate: boolean Read FRunOnCreate Write FRunOnCreate;
    property FreeOnTerminate: boolean Read FFreeOnTerminate Write FFreeOnTerminate;
    property ThreadDialog: TJvCustomThreadDialog Read FThreadDialog Write FThreadDialog;
    property AfterCreateDialogForm: TJvCustomThreadDialogFormEvent
      Read FAfterCreateDialogForm Write FAfterCreateDialogForm;
    property OnBegin: TNotifyEvent Read FOnBegin Write FOnBegin;
    property OnExecute: TJvNotifyParamsEvent Read FOnExecute Write FOnExecute;
    property OnFinish: TNotifyEvent Read FOnFinish Write FOnFinish;
    property OnFinishAll: TNotifyEvent Read FOnFinishAll Write FOnFinishAll;
  end;



 // Cannot be synchronized to the MainThread (VCL)
 // (rom) why are these in the interface section?
procedure Synchronize(Method: TNotifyEvent);

procedure SynchronizeParams(Method: TJvNotifyParamsEvent; P: Pointer);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses JvResources;

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

//=== { TJvCustomThreadDialogOptions } =========================================

constructor TJvCustomThreadDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create;
  FOwner      := AOwner;
  FShowDialog := False;
  FShowModal  := True;
end;

procedure TJvCustomThreadDialogOptions.SetShowDialog(Value: boolean);
begin
  FShowDialog := Value;
end;

procedure TJvCustomThreadDialogOptions.SetShowModal(Value: boolean);
begin
  FShowModal := Value;
end;

//=== { TJvCustomThreadDialogForm } =========================================

constructor TJvCustomThreadDialogForm.CreateNew(AOwner: TComponent;
  Dummy: integer = 0);
begin
  inherited CreateNew(AOwner, Dummy);
  FInternalTimerInterval := 250;
  if Assigned(AOwner) and
    (AOwner is TJvThread) then
    FConnectedThread := TJvThread(AOwner)
  else
    raise EJVCLException.CreateRes(@RsENotATJvThread);
  inherited OnShow := ReplaceFormShow;
  inherited OnClose := ReplaceFormClose;
  inherited OnCloseQuery := ReplaceFormCloseQuery;
  FInternalTimer := TTimer.Create(Self);
  FInternalTimer.OnTimer := OnInternalTimer;
  FInternalTimer.Interval := FInternalTimerInterval;
end;

constructor TJvCustomThreadDialogForm.CreateNewFormStyle(AOwner: TJvThread;
  FormStyle: TFormStyle; Parent: TWinControl = nil);
begin
  if FormStyle <> fsStayOnTop then
    if Assigned(Parent) then
      fParentHandle := Parent.Handle
    else
      fParentHandle := 0;
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
  if fParentHandle <> 0 then
    Params.WndParent := fParentHandle;
end;

procedure TJvCustomThreadDialogForm.DefaultCancelBtnClick(Sender: TObject);
begin
  if Assigned(ConnectedThread) then
    ConnectedThread.CancelExecute;
end;

procedure TJvCustomThreadDialogForm.InitializeFormContents;
begin

end;

procedure TJvCustomThreadDialogForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FConnectedDataComponent then
      FConnectedDataComponent := nil;
end;

procedure TJvCustomThreadDialogForm.OnInternalTimer(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    if Assigned(ConnectedThread) and ConnectedThread.Terminated then
      Close
    else if not Assigned(ConnectedThread) then
      Close
    else
      UpdateFormContents;
end;

procedure TJvCustomThreadDialogForm.ReplaceFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FInternalTimer.OnTimer := nil;
  FInternalTimer.Enabled := False;
  Action := caFree;
  if Assigned(FOnClose) then
    FOnClose(Sender, Action);
end;

procedure TJvCustomThreadDialogForm.ReplaceFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if Assigned(ConnectedThread) then
    CanClose := ConnectedThread.Terminated
  else
    CanClose := True;
  if CanClose then
    if Assigned(FOnCloseQuery) then
      FOnCloseQuery(Sender, CanClose);
end;

procedure TJvCustomThreadDialogForm.ReplaceFormShow(Sender: TObject);
begin
  InitializeFormContents;
  OnInternalTimer(nil);
  FInternalTimer.Enabled := True;
end;

procedure TJvCustomThreadDialogForm.TransferDialogOptions;
begin
end;

procedure TJvCustomThreadDialogForm.UpdateFormContents;
begin
end;

procedure TJvCustomThreadDialogForm.SetConnectedDataComponent(Value: TComponent);
begin
  FConnectedDataComponent := Value;
  FreeNotification(Value);
end;

procedure TJvCustomThreadDialogForm.SetConnectedDataObject(Value: TObject);
begin
  FConnectedDataObject := Value;
end;

procedure TJvCustomThreadDialogForm.SetInternalTimerInterval(Value: integer);
begin
  FInternalTimerInterval  := Value;
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

procedure TJvCustomThreadDialog.CloseThreadDialogForm;
begin
  if Assigned(FThreadStatusDialog) and not (csDestroying in ComponentState) then
  begin
    FThreadStatusDialog.Close;
    Application.HandleMessage;
  end;
end;

procedure TJvCustomThreadDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FThreadStatusDialog then
      FThreadStatusDialog := nil;
end;

//=== { TJvThread } ==========================================================

constructor TJvThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadCount := 0;
  FRunOnCreate := True;
  FExclusive   := True;
  FFreeOnTerminate := True;
  FThreads     := TThreadList.Create;
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
  inherited Destroy;
end;

procedure TJvThread.CancelExecute;
begin
  Terminate;
end;

procedure TJvThread.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FThreadDialog then
      FThreadDialog := nil
    else if AComponent = FThreadDialogForm then
      FThreadDialogForm := nil
end;

procedure TJvThread.Synchronize(Method: TThreadMethod);
begin
  if Assigned(LastThread) then
    LastThread.Synchronize(Method);
end;

function TJvThread.SynchMessageDlg(const Msg: string; AType: TMsgDlgType;
  AButtons: TMsgDlgButtons; HelpCtx: longint): word;
begin
  if Assigned(LastThread) then
    Result := LastThread.SynchMessageDlg(Msg, AType, AButtons, HelpCtx)
  else
    Result := 0;
end;

function TJvThread.Execute(P: Pointer): THandle;
var
  BaseThread: TJvBaseThread;
begin
  Result := 0;
  if Exclusive and OneThreadIsRunning then
    Exit;

  if Assigned(FOnExecute) then
  begin
    Inc(FThreadCount);
    BaseThread := TJvBaseThread.Create(Self, FOnExecute, P);
    try
      BaseThread.FreeOnTerminate := FFreeOnTerminate;
      BaseThread.OnTerminate     := DoTerminate;
      FThreads.Add(BaseThread);
      DoCreate;
    except
      BaseThread.Free;
      raise;
    end;
    if FRunOnCreate then
    begin
      BaseThread.Resume;
      CreateThreadDialogForm;
    end;
    Result := BaseThread.ThreadID;
  end;
end;

procedure TJvThread.ExecuteAndWait(P: Pointer);
begin
  Execute(P);
  while OneThreadIsRunning do
    Application.HandleMessage;
end;

function TJvThread.GetPriority(Thread: THandle): TThreadPriority;
begin
  {$IFDEF MSWINDOWS}
  Result := tpIdle;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := 0;
  {$ENDIF UNIX}
  if Thread <> 0 then
    Result := TThreadPriority(GetThreadPriority(Thread));
end;

procedure TJvThread.SetPriority(Thread: THandle; Priority: TThreadPriority);
begin
  SetThreadPriority(Thread, integer(Priority));
end;

{$IFDEF UNIX}

function TJvThread.GetPolicy(Thread: THandle): integer;
begin
  Result := 0;
  if Thread <> 0 then
    Result := GetThreadPolicy(Thread);
end;

procedure TJvThread.SetPolicy(Thread: THandle; Policy: integer);
begin
  if Thread <> 0 then
    SetThreadPriority(Thread, Policy);
end;

{$ENDIF UNIX}

procedure TJvThread.QuitThread(Thread: THandle);
begin
  TerminateThread(Thread, 0);
end;

procedure TJvThread.Suspend(Thread: THandle);
begin
  SuspendThread(Thread);
end;

procedure TJvThread.Resume(Thread: THandle);
begin
  ResumeThread(Thread);
  CreateThreadDialogForm;
end;

procedure TJvThread.DoCreate;
begin
  if Assigned(FOnBegin) then
    FOnBegin(Self);
end;

procedure TJvThread.DoTerminate(Sender: TObject);
begin
  Dec(FThreadCount);
  FThreads.Remove(Sender);
  try
    if Assigned(FOnFinish) then
      FOnFinish(Self);
  finally
    if FThreadCount = 0 then
    begin
      if Assigned(ThreadDialogForm) then
        ThreadDialogForm.Close;
      if Assigned(FOnFinishAll) then
        FOnFinishAll(Self);
    end;
  end;
end;

function TJvThread.OneThreadIsRunning: boolean;
begin
  Result := FThreadCount > 0;
end;

procedure TJvThread.Terminate;
var
  List: TList;
  I:    integer;
begin
  List := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      TJvBaseThread(List[I]).Terminate;
      if TJvBaseThread(List[I]).Suspended then
        TJvBaseThread(List[I]).Resume;
    end;
  finally
    FThreads.UnlockList;
  end;
end;

function TJvThread.GetCount: integer;
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

function TJvThread.GetThreads(Index: integer): TJvBaseThread;
var
  List: TList;
begin
  List := FThreads.LockList;
  try
    Result := TJvBaseThread(List[Index]);
  finally
    FThreads.UnlockList;
  end;
end;

function TJvThread.GetTerminated: boolean;
var
  I:    integer;
  List: TList;
begin
  Result := True;
  List   := FThreads.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Result := Result and TJvBaseThread(List[I]).Terminated;
      if not Result then
        Break;
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
      intAfterCreateDialogForm(FThreadDialogForm);
      if ThreadDialog.DialogOptions.ShowModal then
        FThreadDialogForm.ShowModal
      else
        FThreadDialogForm.Show;
    end;
  end;
end;

procedure TJvThread.ExecuteWithDialog(P: Pointer);
begin
  if Assigned(ThreadDialog) and
    ThreadDialog.DialogOptions.ShowDialog and
    ThreadDialog.DialogOptions.ShowModal then
    ExecuteAndWait(P)
  else
    Execute(P);
end;

function TJvThread.GetLastThread: TJvBaseThread;
begin
  if Count > 0 then
    Result := Threads[Count - 1]
  else
    Result := nil;
end;

procedure TJvThread.intAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm);
begin
  if Assigned(FAfterCreateDialogForm) then
    FAfterCreateDialogForm(DialogForm);
end;

//=== { TJvBaseThread } ======================================================

constructor TJvBaseThread.Create(Sender: TObject; Event: TJvNotifyParamsEvent;
  Params: Pointer);
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

procedure TJvBaseThread.Execute;
begin
  try
    FExecuteEvent(FSender, FParams);
  except
    on E: Exception do
    begin
      FException     := E;
      FExceptionAddr := ExceptAddr;
      Self.Synchronize(ExceptionHandler);
    end;
  end;
end;

procedure TJvBaseThread.InternalMessageDlg;
begin
  FSynchMessageDlgResult := MessageDlg(FSynchMsg, FSynchAType,
    FSynchAButtons, FSynchHelpCtx);
end;

function TJvBaseThread.SynchMessageDlg(const Msg: string; AType: TMsgDlgType;
  AButtons: TMsgDlgButtons; HelpCtx: longint): word;
begin
  FSynchMsg      := Msg;
  FSynchAType    := AType;
  FSynchAButtons := AButtons;
  FSynchHelpCtx  := HelpCtx;
  Synchronize(InternalMessageDlg);
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

