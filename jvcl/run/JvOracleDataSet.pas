{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOracleDataset.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Oracle Dataset with Threaded Functions

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvOracleDataSet;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, StdCtrls, ExtCtrls, Forms, Controls,
  DB, OracleData,
  JvThread, JvThreadDialog, JvDynControlEngine;

type
  TJvOracleDatasetOperation = (todoOpen, todoFetch, todoLast, todoRefresh, todoNothing);
  TJvOracleDatasetAction = (todaOpen, todaFetch, todaNothing);

  TJvOracleDatasetFetchMode = (todfmFetch, todfmBreak, todfmStop);

  TJvOracleDatasetContinueAllowButton = (todcaPause, todcaStop, todcaAll);
  TJvOracleDatasetContinueAllowButtons = set of TJvOracleDatasetContinueAllowButton;

  TJvOracleDatasetAllowedAfterFetchRecordAction = (todafPause, todafCancel, todafAll);
  TJvOracleDatasetAllowedAfterFetchRecordActions = set of TJvOracleDatasetAllowedAfterFetchRecordAction;

  TJvOracleDataSet = class;

  TJvOracleDatasetThreadEvent = procedure(DataSet: TJvOracleDataSet;
    Operation: TJvOracleDatasetOperation) of object;

  TJvOracleDatasetDialogOptions = class(TJvCustomThreadDialogOptions)
  private
    FCaption: string;
    FDynControlEngine: TJvDynControlEngine;
    FEnableCancelButton: Boolean;
    FFormStyle: TFormStyle;
    FShowCancelButton: Boolean;
    FShowRowsLabel: Boolean;
    FShowTimeLabel: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetDynControlEngine(const Value: TJvDynControlEngine);
    procedure SetEnableCancelButton(const Value: Boolean);
    procedure SetFormStyle(const Value: TFormStyle);
    procedure SetShowCancelButton(const Value: Boolean);
    procedure SetShowRowsLabel(const Value: Boolean);
    procedure SetShowTimeLabel(const Value: Boolean);
  public
    constructor Create(AOwner: TJvCustomThreadDialog); override;
    destructor Destroy; override;
  published
    property Caption: string read FCaption write SetCaption;
    property DynControlEngine: TJvDynControlEngine read FDynControlEngine write SetDynControlEngine;
    property EnableCancelButton: Boolean read FEnableCancelButton write SetEnableCancelButton default True;
    property FormStyle: TFormStyle read FFormStyle write SetFormStyle;
    property ShowCancelButton: Boolean read FShowCancelButton write SetShowCancelButton default True;
    property ShowRowsLabel: Boolean read FShowRowsLabel write SetShowRowsLabel default True;
    property ShowTimeLabel: Boolean read FShowTimeLabel write SetShowTimeLabel default True;
  end;

  TJvOracleDatasetThreadOptions = class(TPersistent)
  private
    FLastInThread: Boolean;
    FOpenInThread: Boolean;
    FPriority: TThreadPriority;
    FRefreshInThread: Boolean;
    FShowExceptionMessage: Boolean;
  public
    constructor Create;
  published
    property LastInThread: Boolean read FLastInThread write FLastInThread default False;
    property OpenInThread: Boolean read FOpenInThread write FOpenInThread default False;
    property Priority: TThreadPriority read FPriority write FPriority default tpIdle;
    property RefreshInThread: Boolean read FRefreshInThread write FRefreshInThread default False;
    property ShowExceptionMessage: Boolean read FShowExceptionMessage
      write FShowExceptionMessage default True;
  end;

  TJvOracleDatasetCapitalizeLabelOptions = class(TPersistent)
  private
    FAutoExecuteAfterOpen: Boolean;
    FTrimToFirstBlank: Boolean;
  public
    constructor Create;
  published
    property AutoExecuteAfterOpen: Boolean read FAutoExecuteAfterOpen
      write FAutoExecuteAfterOpen default False;
    property TrimToFirstBlank: Boolean read FTrimToFirstBlank write FTrimToFirstBlank default False;
  end;

  TJvOracleDatasetEnhancedOptions = class(TPersistent)
  private
    FAllowedAfterFetchRecordActions: TJvOracleDatasetAllowedAfterFetchRecordActions;
    FCapitalizeLabelOptions: TJvOracleDatasetCapitalizeLabelOptions;
    FFetchRowsCheck: Integer;
    FFetchRowsFirst: Integer;
    FRefreshAsOpenClose: Boolean;
    FRefreshLastPosition: Boolean;
    procedure SetCapitalizeLabelOptions(const Value: TJvOracleDatasetCapitalizeLabelOptions);
    procedure SetFetchRowsCheck(const Value: Integer);
    procedure SetFetchRowsFirst(const Value: Integer);
    procedure SetRefreshAsOpenClose(Value: Boolean);
    procedure SetRefreshLastPosition(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AllowedAfterFetchRecordActions: TJvOracleDatasetAllowedAfterFetchRecordActions
      read FAllowedAfterFetchRecordActions write FAllowedAfterFetchRecordActions
      default [todafPause, todafCancel, todafAll];
    property CapitalizeLabelOptions: TJvOracleDatasetCapitalizeLabelOptions read
      FCapitalizeLabelOptions write SetCapitalizeLabelOptions;
    property FetchRowsCheck: Integer read FFetchRowsCheck write SetFetchRowsCheck;
    property FetchRowsFirst: Integer read FFetchRowsFirst write SetFetchRowsFirst;
    property RefreshAsOpenClose: Boolean read FRefreshAsOpenClose
      write SetRefreshAsOpenClose default False;
    property RefreshLastPosition: Boolean read FRefreshLastPosition
      write SetRefreshLastPosition default False;
  end;

  TJvOracleDatasetThreadDialogForm = class(TJvCustomThreadDialogForm)
  private
    FRowsLabel: TControl;
    FTimeLabel: TControl;
    FRowsStaticText: TWinControl;
    FTimeStaticText: TWinControl;
    FCancelBtn: TButton;
    FCancelButtonPanel: TWinControl;
    FRowsPanel: TWinControl;
    FTimePanel: TWinControl;
    FDialogOptions: TJvOracleDatasetDialogOptions;
    FDynControlEngine: TJvDynControlEngine;
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl;
      var Panel: TWinControl; var LabelCtrl: TControl; var StaticText: TWinControl;
      const BaseName: string);
    function GetConnectedDataset: TJvOracleDataset;
    function GetDialogOptions: TJvOracleDatasetDialogOptions;
    procedure SetDialogOptions(const Value: TJvOracleDatasetDialogOptions);
    procedure SetDynControlEngine(const Value: TJvDynControlEngine);
  protected
    procedure FillDialogData;
    procedure UpdateFormContents; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateFormControls;
    procedure TransferDialogOptions; override;
    property ConnectedDataset: TJvOracleDataset read GetConnectedDataset;
    property DynControlEngine: TJvDynControlEngine read FDynControlEngine write SetDynControlEngine;
  published
    property DialogOptions: TJvOracleDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
  end;

  TJvOracleDatasetThreadDialog = class(TJvCustomThreadDialog)
  private
    function GetDialogOptions: TJvOracleDatasetDialogOptions;
    procedure SetDialogOptions(const Value: TJvOracleDatasetDialogOptions);
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvOracleDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
  end;

  TJvOracleDatasetThread = class(TJvThread)
  private
    FConnectedDataset: TJvOracleDataset;
  protected
    procedure intAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm); override;
  public
    procedure CancelExecute; override;
    property ConnectedDataset: TJvOracleDataset read FConnectedDataset write FConnectedDataset;
  end;

  TJvOracleDataSet = class(TOracleDataSet)
  private
    FAfterFetchRecord: TAfterFetchRecordEvent;
    FAfterOpen: TDataSetNotifyEvent;
    FAfterRefresh: TDataSetNotifyEvent;
    FAfterThreadExecution: TJvOracleDatasetThreadEvent;
    FBeforeOpen: TDataSetNotifyEvent;
    FBeforeRefresh: TDataSetNotifyEvent;
    FBeforeThreadExecution: TJvOracleDatasetThreadEvent;
    FCurrentAction: TJvOracleDatasetAction;
    FCurrentFetchDuration: TDateTime;
    FCurrentOpenDuration: TDateTime;
    FCurrentOperation: TJvOracleDatasetOperation;
    FCurrentOperationStart: TDateTime;
    FCurrentRow: Integer;
    FEnhancedOptions: TJvOracleDatasetEnhancedOptions;
    FErrorMessage: string;
    FFetchMode: TJvOracleDatasetFetchMode;
    FFetchStartTime: TDateTime;
    FIgnoreRowsCheck: Integer;
    FIntDatasetWasFiltered: Boolean;
    FintQueryAllRecords: Boolean;
    FIntRowCheckEnabled: Boolean;
    FOpenStartTime: TDateTime;
    FSynchAfterFetchAction: TAfterFetchRecordAction;
    FSynchAfterFetchFilterAccept: Boolean;
    FSynchAfterFetchSender: TOracleDataSet;
    FSynchMessageDlgBtn: Word;
    FSynchMessageDlgMsg: string;
    FExecuteThread: TJvOracleDatasetThread;
    FLastRowChecked: Integer;
    FMoveToRecordAfterOpen: Longint;
    FOperationWasHandledInThread: Boolean;
    FThreadDialog: TJvOracleDatasetThreadDialog;
    FThreadOptions: TJvOracleDatasetThreadOptions;
    function GetCurrentAction: TJvOracleDatasetAction;
    function GetCurrentFetchDuration: TDateTime;
    function GetCurrentOpenDuration: TDateTime;
    function GetCurrentOperation: TJvOracleDatasetOperation;
    function GetCurrentOperationAction: string;
    function GetDialogOptions: TJvOracleDatasetDialogOptions;
    function GetFetchMode: TJvOracleDatasetFetchMode;
    procedure HandleAfterOpenRefresh;
    procedure SetCurrentAction(const Value: TJvOracleDatasetAction);
    procedure SetCurrentFetchDuration(const Value: TDateTime);
    procedure SetCurrentOpenDuration(const Value: TDateTime);
    procedure SetDialogOptions(Value: TJvOracleDatasetDialogOptions);
    procedure SetEnhancedOptions(Value: TJvOracleDatasetEnhancedOptions);
    procedure SetFetchMode(const Value: TJvOracleDatasetFetchMode);
    procedure SetFetchStartTime(const Value: TDateTime);
    procedure SetIgnoreRowsCheck(const Value: Integer);
    procedure SetOpenStartTime(const Value: TDateTime);
    procedure SetThreadOptions(const Value: TJvOracleDatasetThreadOptions);
    procedure SynchAfterFetchRecord;
    procedure SynchAfterThreadExecution;
    procedure SynchBeforeThreadExecution;
    procedure SynchContinueFetchMessageDlg;
    procedure SynchErrorMessageDlg;
    property OperationWasHandledInThread: Boolean read FOperationWasHandledInThread
        write FOperationWasHandledInThread;
  protected
    procedure ExecuteThreadSynchronize(Method: TThreadMethod);
    procedure DoThreadLast;
    procedure DoThreadOpen;
    procedure DoThreadRefresh;
    function ExecuteThreadIsActive: Boolean;
    procedure HandleAfterOpenRefreshThread;
    procedure HandleBeforeOpenRefresh;
    procedure InternalLast; override;
    procedure InternalRefresh; override;
    procedure IntSynchAfterOpen;
    procedure IntSynchAfterRefresh;
    procedure IntAfterThreadExecution(DataSet: TJvOracleDataSet; Operation: TJvOracleDatasetOperation);
    procedure IntSynchBeforeOpen;
    procedure IntSynchBeforeRefresh;
    procedure IntBeforeThreadExecution(DataSet: TJvOracleDataSet; Operation: TJvOracleDatasetOperation);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReplaceAfterFetchRecord(Sender: TOracleDataSet;
      FilterAccept: Boolean; var Action: TAfterFetchRecordAction);
    procedure ReplaceAfterOpen(Dataset: TDataSet);
    procedure ReplaceAfterRefresh(Dataset: TDataSet);
    procedure ReplaceBeforeOpen(Dataset: TDataSet);
    procedure ReplaceBeforeRefresh(Dataset: TDataSet);
    procedure SetActive(Value: Boolean); override;
    procedure SetErrorMessage(const Value: string);
    procedure ThreadExecute(Sender: TObject; Params: Pointer);
    property CurrentOperation: TJvOracleDatasetOperation read GetCurrentOperation;
    property FetchMode: TJvOracleDatasetFetchMode read GetFetchMode write SetFetchMode;
    property FetchStartTime: TDateTime read FFetchStartTime write SetFetchStartTime;
    property IgnoreRowsCheck: Integer read FIgnoreRowsCheck write SetIgnoreRowsCheck;
    property IntRowCheckEnabled: Boolean read FIntRowCheckEnabled write FIntRowCheckEnabled;
    property OpenStartTime: TDateTime read FOpenStartTime write SetOpenStartTime;
    property ExecuteThread: TJvOracleDatasetThread read FExecuteThread;
    property ThreadDialog: TJvOracleDatasetThreadDialog read FThreadDialog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BreakExecution;
    procedure CapitalizeDatasetLabels;
    class function CreateJvOracleDataSet(AOwner: TComponent): TJvOracleDataSet;
    procedure MoveTo(Position: Integer);
    function ThreadIsActive: Boolean;
    property CurrentAction: TJvOracleDatasetAction read GetCurrentAction write SetCurrentAction;
    property CurrentFetchDuration: TDateTime read GetCurrentFetchDuration write SetCurrentFetchDuration;
    property CurrentOpenDuration: TDateTime read GetCurrentOpenDuration write SetCurrentOpenDuration;
    property CurrentOperationAction: string read GetCurrentOperationAction;
    property CurrentRow: Integer read FCurrentRow;
    property ErrorMessage: string read FErrorMessage write SetErrorMessage;
  published
    property AfterFetchRecord: TAfterFetchRecordEvent read FAfterFetchRecord write FAfterFetchRecord;
    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeOpen: TDataSetNotifyEvent read FBeforeOpen write FBeforeOpen;
    property DialogOptions: TJvOracleDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
    property EnhancedOptions: TJvOracleDatasetEnhancedOptions
      read FEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvOracleDatasetThreadOptions read FThreadOptions write SetThreadOptions;
    property AfterRefresh: TDataSetNotifyEvent read FAfterRefresh write FAfterRefresh;
    property AfterThreadExecution: TJvOracleDatasetThreadEvent
      read FAfterThreadExecution write FAfterThreadExecution;
    property BeforeRefresh: TDataSetNotifyEvent read FBeforeRefresh write FBeforeRefresh;
    property BeforeThreadExecution: TJvOracleDatasetThreadEvent
      read FBeforeThreadExecution write FBeforeThreadExecution;
  end;

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

uses
  Dialogs, DateUtils,
  JvDynControlEngineIntf, JvDSADialogs, JvResources;

//=== { TJvOracleDataSet } ===================================================

constructor TJvOracleDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadOptions := TJvOracleDatasetThreadOptions.Create;
  FExecuteThread := TJvOracleDatasetThread.Create(Self);
  FThreadDialog := TJvOracleDatasetThreadDialog.Create(Self);
  FExecuteThread.Exclusive := True;
  FExecuteThread.OnExecute := ThreadExecute;
  FExecuteThread.ConnectedDataset := Self;
  FExecuteThread.ThreadDialog := ThreadDialog;
  FEnhancedOptions := TJvOracleDatasetEnhancedOptions.Create;
  FFetchMode := todfmFetch;
  IntRowCheckEnabled := True;
  FCurrentAction := todaNothing;
  inherited AfterFetchRecord := ReplaceAfterFetchRecord;
  inherited BeforeOpen := ReplaceBeforeOpen;
  inherited AfterOpen := ReplaceAfterOpen;
  inherited BeforeRefresh := ReplaceBeforeRefresh;
  inherited AfterRefresh := ReplaceAfterRefresh;
end;

destructor TJvOracleDataSet.Destroy;
begin
  if Assigned(FExecuteThread) then
  begin
    if not FExecuteThread.Terminated then
      FExecuteThread.Terminate;
    FreeAndNil(FExecuteThread);
  end;
  if Assigned(FEnhancedOptions) then
    FreeAndNil(FEnhancedOptions);
  if Assigned(FThreadOptions) then
    FreeAndNil(FThreadOptions);
  if Assigned(FThreadDialog) then
    FreeAndNil(FThreadDialog);
  inherited;
end;

procedure TJvOracleDataSet.BreakExecution;
begin
  if CurrentAction = todaOpen then
    if Assigned(Session) and Session.Connected then
      Session.BreakExecution;
  if FetchMode = todfmFetch then
    FetchMode := todfmBreak;
  IntRowCheckEnabled := False;
end;

procedure TJvOracleDataSet.CapitalizeDatasetLabels;
var
  I, J: Integer;
  S: string;
  Upper: Boolean;
begin
  if Active then
    for I := 0 to FieldCount - 1 do
    begin
      S := LowerCase(Fields[I].DisplayLabel);
      Upper := True;
      for J := 1 to Length(S) do
        if S[J] in ['_', '$', ' '] then
        begin
          Upper := True;
          S[J] := ' ';
        end
        else if Upper then
        begin
          S[J] := UpCase(S[J]);
          Upper := False;
        end;
      if EnhancedOptions.CapitalizeLabelOptions.TrimToFirstBlank then
      begin
        J := Pos(' ', S);
        if J > 0 then
          S := Copy(S, J + 1, Length(S) - J);
      end;
      Fields[I].DisplayLabel := S;
    end;
end;

class function TJvOracleDataSet.CreateJvOracleDataSet(AOwner: TComponent): TJvOracleDataSet;
begin
  Result := TJvOracleDataSet.Create(AOwner);
end;

procedure TJvOracleDataSet.ExecuteThreadSynchronize(Method: TThreadMethod);
begin
  if ExecuteThreadIsActive then
    ExecuteThread.Synchronize(Method)
  else
    Method;
end;

function TJvOracleDataSet.GetCurrentAction: TJvOracleDatasetAction;
begin
  Result := FCurrentAction;
end;

function TJvOracleDataSet.GetCurrentFetchDuration: TDateTime;
begin
  case CurrentAction of
    todaOpen:
      Result := 0;
    todaNothing:
      Result := FCurrentFetchDuration;
    todaFetch:
      Result := FCurrentFetchDuration + (Now - FCurrentOperationStart);
  else
    Result := 0;
  end;
end;

function TJvOracleDataSet.GetCurrentOpenDuration: TDateTime;
begin
  if CurrentAction = todaOpen then
    Result := Now - FCurrentOperationStart
  else
    Result := FCurrentOpenDuration;
end;

function TJvOracleDataSet.GetCurrentOperation: TJvOracleDatasetOperation;
begin
  Result := FCurrentOperation;
end;

function TJvOracleDataSet.GetCurrentOperationAction: string;
begin
  case CurrentOperation of
    todoOpen:
      case CurrentAction of
        todaOpen:
          Result := SODSOpenQuery;
        todaFetch:
          Result := SODSOpenQueryFetchRecords;
      end;
    todoRefresh:
      case CurrentAction of
        todaOpen:
          Result := SODSRefreshQuery;
        todaFetch:
          Result := SODSRefreshQueryFetchRecords;
      end;
    todoFetch:
      Result := SODSFetchRecords;
    todoLast:
      Result := SODSGotoLastFetchRecords;
  end;
end;

function TJvOracleDataSet.GetFetchMode: TJvOracleDatasetFetchMode;
begin
  Result := FFetchMode;
end;

procedure TJvOracleDataSet.DoThreadLast;
begin
  inherited InternalLast;
end;

procedure TJvOracleDataSet.DoThreadOpen;
begin
  try
    inherited SetActive(True);
  finally
    HandleAfterOpenRefreshThread;
  end;
end;

procedure TJvOracleDataSet.DoThreadRefresh;
begin
  try
    if not EnhancedOptions.RefreshAsOpenClose then
    begin
      inherited InternalRefresh;
    end
    else
    begin
      Close;
      InternalOpen;
    end;
  finally
    HandleAfterOpenRefreshThread;
  end;
end;

function TJvOracleDataSet.ExecuteThreadIsActive: Boolean;
begin
  Result := Not ExecuteThread.Terminated;
end;

function TJvOracleDataSet.GetDialogOptions: TJvOracleDatasetDialogOptions;
begin
  Result := ThreadDialog.DialogOptions;
end;

procedure TJvOracleDataSet.HandleAfterOpenRefresh;
begin
  CurrentOpenDuration := Now - FCurrentOperationStart;
  FCurrentOperationStart := Now;
  QueryAllRecords := FIntQueryAllRecords;
  CurrentFetchDuration := 0;
  CurrentAction := todaFetch;
  if Active then
  begin
    First;
    if QueryAllRecords then
      inherited InternalLast
    else
    if (EnhancedOptions.FetchRowsFirst > RecordCount) or (FMoveToRecordAfterOpen > RecordCount) then
      if FMoveToRecordAfterOpen > EnhancedOptions.FetchRowsFirst then
        MoveBy(FMoveToRecordAfterOpen - 1)
      else
        MoveBy(EnhancedOptions.FetchRowsFirst - 1);
  end;
  try
    Filtered := FIntDatasetWasFiltered;
    if Active then
      if FMoveToRecordAfterOpen > 0 then
        MoveTo(FMoveToRecordAfterOpen)
      else
        First;
    CurrentAction := todaNothing;
  finally
    ExecuteThreadSynchronize(EnableControls);
  end;
end;

procedure TJvOracleDataSet.HandleAfterOpenRefreshThread;
begin
  HandleAfterOpenRefresh;
  if Active and Assigned(FAfterOpen) and (CurrentOperation <> todoRefresh) then
      ExecuteThreadSynchronize(IntSynchAfterOpen);
end;

procedure TJvOracleDataSet.HandleBeforeOpenRefresh;
begin
  OperationWasHandledInThread := False;
  ExecuteThreadSynchronize(DisableControls);
  CurrentOpenDuration := 0;
  CurrentFetchDuration := 0;
  IntRowCheckEnabled := True;
  FCurrentRow := 0;
  FCurrentOperationStart := Now;
  CurrentAction := todaOpen;
  FIntQueryAllRecords := QueryAllRecords;
  FIntDatasetWasFiltered := Filtered;
  FLastRowChecked := 0;
  Filtered := False;
  QueryAllRecords := False;
end;

procedure TJvOracleDataSet.InternalLast;
var
  ShowModal    :Boolean;
begin
  FCurrentOperation := todoLast;
  if not ThreadOptions.LastInThread or ThreadIsActive or (csDesigning in ComponentState) then
  begin
    inherited InternalLast;
    FCurrentOperation := todoNothing;
  end
  else
  begin
    if Assigned(ExecuteThread.ThreadDialog) then
    begin
      showModal := ExecuteThread.ThreadDialog.DialogOptions.ShowModal;
      ExecuteThread.ThreadDialog.DialogOptions.ShowModal := True;
    end
    else
      ShowModal := False;
    ExecuteThread.ExecuteWithDialog(nil);
    if Assigned(ExecuteThread.ThreadDialog) then
      ExecuteThread.ThreadDialog.DialogOptions.ShowModal := showModal;
  end;
end;

procedure TJvOracleDataSet.InternalRefresh;
var
  ThreadAllowed: Boolean;
begin
  if Assigned(Master) and (Master is TJvOracleDataSet) then
    ThreadAllowed := not TJvOracleDataSet(Master).ThreadIsActive
  else
    ThreadAllowed := True;
  FCurrentOperation := todoRefresh;
  if not ThreadOptions.RefreshInThread or not ThreadAllowed or
    ThreadIsActive or (csDesigning in ComponentState) then
  begin
    inherited InternalRefresh;
    FCurrentOperation := todoNothing;
  end
  else
    ExecuteThread.ExecuteWithDialog(nil);
end;

procedure TJvOracleDataSet.IntSynchAfterOpen;
begin
  if EnhancedOptions.CapitalizeLabelOptions.AutoExecuteAfterOpen then
    CapitalizeDatasetLabels;
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

procedure TJvOracleDataSet.IntSynchAfterRefresh;
begin
  if Assigned(FAfterRefresh) then
    FAfterRefresh(Self);
end;

procedure TJvOracleDataSet.IntAfterThreadExecution(DataSet: TJvOracleDataSet;
  Operation: TJvOracleDatasetOperation);
begin
  if Assigned(FAfterThreadExecution) then
    FAfterThreadExecution(DataSet, Operation);
end;

procedure TJvOracleDataSet.IntSynchBeforeOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
end;

procedure TJvOracleDataSet.IntSynchBeforeRefresh;
begin
  if Assigned(FBeforeRefresh) then
    FBeforeRefresh(Self);
end;

procedure TJvOracleDataSet.IntBeforeThreadExecution(DataSet: TJvOracleDataSet;
  Operation: TJvOracleDatasetOperation);
begin
  if Assigned(FBeforeThreadExecution) then
    FBeforeThreadExecution(DataSet, Operation);
end;

procedure TJvOracleDataSet.MoveTo(Position: Integer);
begin
  MoveBy(Position - RecNo);
end;

procedure TJvOracleDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = ThreadDialog) and (Operation = opRemove) then
    FThreadDialog := nil;
end;

procedure TJvOracleDataSet.ReplaceAfterFetchRecord(Sender: TOracleDataSet;
  FilterAccept: Boolean; var Action: TAfterFetchRecordAction);
begin
  FCurrentRow := RecordCount;
  if Assigned(FAfterFetchRecord) then
  begin
    FSynchAfterFetchSender := Sender;
    FSynchAfterFetchFilterAccept := FilterAccept;
    FSynchAfterFetchAction := Action;
    ExecuteThreadSynchronize(SynchAfterFetchRecord);
    Action := FSynchAfterFetchAction;
    Exit;
  end;
  case FetchMode of
    todfmBreak:
      begin
        Action := afPause;
        FetchMode := todfmFetch;
        Exit;
      end;
    todfmStop:
      begin
        Action := afStop;
        Exit;
      end;
  end;
  if (EnhancedOptions.FetchRowsCheck > 0) and IntRowCheckEnabled then
    if CurrentRow >= FLastRowChecked + EnhancedOptions.FetchRowsCheck then
    begin
      fCurrentFetchDuration := fCurrentFetchDuration + Now - FCurrentOperationStart;
      CurrentAction := todaNothing;
      FLastRowChecked := CurrentRow;
      FSynchMessageDlgMsg := Format(SODSRowsFetchedContinue, [CurrentRow]);
      ExecuteThreadSynchronize(SynchContinueFetchMessageDlg);
      case FSynchMessageDlgBtn of
        mrYes:
          Action := afContinue;
        mrAll:
          begin
            Action := afContinue;
            IntRowCheckEnabled := False;
          end;
        mrAbort:
          Action := afCancel;
        mrCancel:
          Action := afPause;
        mrNo:
          Action := afStop;
      else
        Action := afStop;
      end;
      CurrentAction := todaFetch;
      FCurrentOperationStart := Now;
    end;
end;

procedure TJvOracleDataSet.ReplaceAfterOpen(Dataset: TDataSet);
begin
  if not ExecuteThreadIsActive and not OperationWasHandledInThread and (CurrentOperation <> todoRefresh) then
    HandleAfterOpenRefresh;
  if not ExecuteThreadIsActive then
    if Assigned(FAfterOpen) then
      ExecuteThreadSynchronize(IntSynchAfterOpen);
end;

procedure TJvOracleDataSet.ReplaceAfterRefresh(Dataset: TDataSet);
begin
  if not ExecuteThreadIsActive and not OperationWasHandledInThread then
    HandleAfterOpenRefresh;
  if Assigned(FAfterRefresh) then
    ExecuteThreadSynchronize(IntSynchAfterRefresh);
end;

procedure TJvOracleDataSet.ReplaceBeforeOpen(Dataset: TDataSet);
begin
  if (CurrentOperation <> todoRefresh) then
  begin
    FMoveToRecordAfterOpen := -1;
    HandleBeforeOpenRefresh;
  end;
  if Assigned(FBeforeOpen) then
    ExecuteThreadSynchronize(IntSynchBeforeOpen);
end;

procedure TJvOracleDataSet.ReplaceBeforeRefresh(Dataset: TDataSet);
begin
  if EnhancedOptions.RefreshLastPosition then
    FMoveToRecordAfterOpen := RecNo
  else
    FMoveToRecordAfterOpen := -1;
  HandleBeforeOpenRefresh;
  if Assigned(FBeforeRefresh) then
    ExecuteThreadSynchronize(IntSynchBeforeRefresh);
end;

procedure TJvOracleDataSet.SetActive(Value: Boolean);
begin
  if not Value then
  begin
    CurrentOpenDuration := 0;
    CurrentFetchDuration := 0;
    inherited SetActive(Value);
  end
  else
  begin
    if CurrentOperation <> todoRefresh then
      FCurrentOperation := todoOpen;
    if not ThreadOptions.OpenInThread or ThreadIsActive or (csDesigning in ComponentState) then
    begin
      inherited SetActive(Value);
      if CurrentOperation <> todoRefresh then
        FCurrentOperation := todoNothing;
    end
    else
      ExecuteThread.ExecuteWithDialog(nil);
  end;
end;

procedure TJvOracleDataSet.SetCurrentAction(const Value: TJvOracleDatasetAction);
begin
  FCurrentAction := Value;
end;

procedure TJvOracleDataSet.SetCurrentFetchDuration(const Value: TDateTime);
begin
  FCurrentFetchDuration := Value;
end;

procedure TJvOracleDataSet.SetCurrentOpenDuration(const Value: TDateTime);
begin
  FCurrentOpenDuration := Value;
end;

procedure TJvOracleDataSet.SetDialogOptions(Value: TJvOracleDatasetDialogOptions);
begin
  ThreadDialog.DialogOptions.Assign(Value);
end;

procedure TJvOracleDataSet.SetEnhancedOptions(Value: TJvOracleDatasetEnhancedOptions);
begin
  FEnhancedOptions.Assign(Value);
end;

procedure TJvOracleDataSet.SetErrorMessage(const Value: string);
begin
  FErrorMessage := Value;
end;

procedure TJvOracleDataSet.SetFetchMode(const Value: TJvOracleDatasetFetchMode);
begin
  FFetchMode := Value;
end;

procedure TJvOracleDataSet.SetFetchStartTime(const Value: TDateTime);
begin
  FFetchStartTime := Value;
end;

procedure TJvOracleDataSet.SetIgnoreRowsCheck(const Value: Integer);
begin
  FIgnoreRowsCheck := Value;
end;

procedure TJvOracleDataSet.SetOpenStartTime(const Value: TDateTime);
begin
  FOpenStartTime := Value;
end;

procedure TJvOracleDataSet.SetThreadOptions(const Value: TJvOracleDatasetThreadOptions);
begin
  FThreadOptions.Assign(Value);
end;

procedure TJvOracleDataSet.SynchAfterFetchRecord;
begin
  if Assigned(FAfterFetchRecord) then
    FAfterFetchRecord(FSynchAfterFetchSender, FSynchAfterFetchFilterAccept, FSynchAfterFetchAction);
end;

procedure TJvOracleDataSet.SynchAfterThreadExecution;
begin
  IntAfterThreadExecution(Self, CurrentOperation);
end;

procedure TJvOracleDataSet.SynchBeforeThreadExecution;
begin
  IntBeforeThreadExecution(Self, CurrentOperation);
end;

procedure TJvOracleDataSet.SynchContinueFetchMessageDlg;
var
  Buttons: array of string;
  Results: array of Integer;
  L: Integer;

  procedure AddButton(Caption: string; ResultValue: Integer);
  begin
    Inc(L);
    SetLength (Buttons, L);
    SetLength (Results, L);
    Buttons[L-1] := Caption;
    Results[L-1] := ResultValue;
  end;

begin
  L := 0;
  AddButton(SODSContinueYes, Integer(mrYes));
  if todafPause in EnhancedOptions.AllowedAfterFetchRecordActions then
    AddButton(SODSContinuePause, Integer(mrCancel));
  AddButton(SODSContinueNo, Integer(mrNo));
  if todafAll in EnhancedOptions.AllowedAfterFetchRecordActions then
    AddButton(SODSContinueAll, Integer(mrAll));
  if todafCancel in EnhancedOptions.AllowedAfterFetchRecordActions then
    AddButton(SODSContinueClose, Integer(mrAbort));
  FSynchMessageDlgBtn := JvDSADialogs.MessageDlgEx(FSynchMessageDlgMsg,
      mtConfirmation, Buttons, Results, 0, dckActiveForm, 0,
      0, 1, -1, DialogOptions.DynControlEngine);
end;

procedure TJvOracleDataSet.SynchErrorMessageDlg;
begin
  FSynchMessageDlgBtn := JvDSADialogs.MessageDlg(FSynchMessageDlgMsg,
    mtError, [mbOK], 0, dckScreen, 0,
    mbDefault, mbDefault, mbHelp, DialogOptions.DynControlEngine);
end;

procedure TJvOracleDataSet.ThreadExecute(Sender: TObject; Params: Pointer);
begin
  OperationWasHandledInThread := True;
  try
    SetErrorMessage('');
    ExecuteThreadSynchronize(SynchBeforeThreadExecution);
    try
      case FCurrentOperation of
        todoOpen:
          DoThreadOpen;
        todoRefresh:
          DoThreadRefresh;
        todoLast:
          DoThreadLast;
      end;
    except
      on E: Exception do
      begin
        SetErrorMessage(E.Message);
        if ThreadOptions.ShowExceptionMessage then
        begin
          FSynchMessageDlgMsg := E.Message;
          ExecuteThreadSynchronize(SynchErrorMessageDlg);
        end;
      end;
    end;
    ExecuteThreadSynchronize(SynchAfterThreadExecution);
  finally
    FCurrentOperation := todoNothing;
  end;
end;

function TJvOracleDataSet.ThreadIsActive: Boolean;
begin
  Result := not ExecuteThread.Terminated;
end;

function TJvOracleDatasetThreadDialog.CreateDialogOptions: TJvCustomThreadDialogOptions;
begin
  Result := TJvOracleDatasetDialogOptions.Create(Self);
end;

function TJvOracleDatasetThreadDialog.CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm;
var
  ThreadDialogForm: TJvOracleDatasetThreadDialogForm;
begin
  if DialogOptions.ShowDialog then
  begin
    if Assigned(ConnectedThread.Owner) and (ConnectedThread.Owner is TWinControl) then
      ThreadDialogForm := TJvOracleDatasetThreadDialogForm.CreateNewFormStyle(ConnectedThread,
        DialogOptions.FormStyle, TWinControl(ConnectedThread.Owner))
    else
    if Assigned(ConnectedThread.Owner) and Assigned(ConnectedThread.Owner.Owner) and
      (ConnectedThread.Owner.Owner is TWinControl) then
      ThreadDialogForm := TJvOracleDatasetThreadDialogForm.CreateNewFormStyle(ConnectedThread,
        DialogOptions.FormStyle, TWinControl(ConnectedThread.Owner.Owner))
    else
     ThreadDialogForm := TJvOracleDatasetThreadDialogForm.CreateNewFormStyle(ConnectedThread,
       DialogOptions.FormStyle);
    ThreadDialogForm.DialogOptions := DialogOptions;
    ThreadDialogForm.CreateFormControls;
    Result := ThreadDialogForm;
  end
  else
    Result := nil;
end;

function TJvOracleDatasetThreadDialog.GetDialogOptions: TJvOracleDatasetDialogOptions;
begin
  Result := TJvOracleDatasetDialogOptions(inherited DialogOptions);
end;

procedure TJvOracleDatasetThreadDialog.SetDialogOptions(const Value: TJvOracleDatasetDialogOptions);
begin
  inherited DialogOptions.Assign(Value);
end;

//=== { TJvOracleDatasetThreadDialogForm } ===================================

constructor TJvOracleDatasetThreadDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DynControlEngine := nil;
end;

procedure TJvOracleDatasetThreadDialogForm.CreateFormControls;
var
  MainPanel: TWinControl;
  ITmpPanel: IJvDynControlPanel;
  ITmpControl: IJvDynControl;
begin
  MainPanel := DynControlEngine.CreatePanelControl(Self, Self, 'MainPanel', '', alClient);
  if not Supports(MainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);

  CreateTextPanel(Self, MainPanel, FTimePanel, FTimeLabel, FTimeStaticText, 'Time');
  if Supports(FTimeLabel, IJvDynControl, ITmpControl) then
    ITmpControl.ControlSetCaption(SODSOpenFetch);
  CreateTextPanel(Self, MainPanel, FRowsPanel, FRowsLabel, FRowsStaticText, 'Rows');
  if Supports(FRowsLabel, IJvDynControl, ITmpControl) then
    ITmpControl.ControlSetCaption(SODSCurrentRecord);
  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', RsButtonCancelCaption, '', DefaultCancelBtnClick, True, True);
  with FCancelBtn do
  begin
    Anchors := [akTop];
    Top := 2;
    FCancelButtonPanel.Height := FCancelBtn.Height + 3;
  end;

  BorderIcons := [];
  BorderStyle := bsDialog;
  if DialogOptions.Caption <> '' then
    Caption := DialogOptions.Caption
  else
    Caption := ' ';
  FormStyle := DialogOptions.FormStyle;
  OldCreateOrder := False;
  {$IFDEF COMPILER7_UP}
  Position := poOwnerFormCenter;
  {$ELSE}
  Position := poScreenCenter;
  {$ENDIF COMPILER7_UP}
  PixelsPerInch := 96;
end;

procedure TJvOracleDatasetThreadDialogForm.CreateTextPanel(AOwner: TComponent;
  AParent: TWinControl; var Panel: TWinControl; var LabelCtrl: TControl;
  var StaticText: TWinControl; const BaseName: string);
var
  ITmpPanel: IJvDynControlPanel;
  ITmpAutoSize: IJvDynControlAutoSize;
  ITmpAlignment: IJvDynControlAlignment;
begin
  Panel := DynControlEngine.CreatePanelControl(AOwner, AParent, BaseName + 'Panel', '', alTop);
  if not Supports(Panel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 3);
  LabelCtrl := DynControlEngine.CreateLabelControl(AOwner, Panel, BaseName + 'Label', '', nil);
  with LabelCtrl do
  begin
    Top := 1;
    Left := 1;
  end;
  StaticText := DynControlEngine.CreateStaticTextControl(AOwner, Panel, BaseName + 'StaticText', '');
  if Supports(StaticText, IJvDynControlAutoSize, ITmpAutoSize) then
    ITmpAutoSize.ControlSetAutoSize(True);
  if Supports(StaticText, IJvDynControlAlignment, ITmpAlignment) then
    ITmpAlignment.ControlSetAlignment(taCenter);
  with StaticText do
  begin
    Top := 1;
    Left := 100;
    Height := 13;
    Panel.Height := Height + 6;
  end;
end;

procedure TJvOracleDatasetThreadDialogForm.FillDialogData;
var
  ITmpControl: IJvDynControl;
begin
  if Assigned(ConnectedDataset) then
  begin
    if DialogOptions.Caption <> '' then
      Caption := DialogOptions.Caption +' - '+ConnectedDataset.CurrentOperationAction
    else
      Caption := ConnectedDataset.CurrentOperationAction;
    if Supports(FRowsStaticText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(IntToStr(ConnectedDataset.CurrentRow));
    if Supports(FTimeStaticText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(
        FormatDateTime('hh:nn:ss', ConnectedDataset.CurrentOpenDuration) + ' / ' +
        FormatDateTime('hh:nn:ss', ConnectedDataset.CurrentFetchDuration));
  end
  else
  begin
    if DialogOptions.Caption <> '' then
      Caption := DialogOptions.Caption +' - '
    else
      Caption := '';
    if Supports(FRowsStaticText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(IntToStr(0));
    if Supports(FTimeStaticText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(
        FormatDateTime('hh:nn:ss', 0) + ' / ' +
        FormatDateTime('hh:nn:ss', 0));
  end;
end;

function TJvOracleDatasetThreadDialogForm.GetConnectedDataset: TJvOracleDataset;
begin
  if Assigned(ConnectedDataComponent) and (ConnectedDataComponent is TJvOracleDataSet) then
    Result := TJvOracleDataSet(ConnectedDataComponent)
  else
    Result := nil;
end;

function TJvOracleDatasetThreadDialogForm.GetDialogOptions: TJvOracleDatasetDialogOptions;
begin
  Result := FDialogOptions;
end;

procedure TJvOracleDatasetThreadDialogForm.SetDialogOptions(const Value: TJvOracleDatasetDialogOptions);
begin
  FDialogOptions := Value;
  DynControlEngine := DialogOptions.DynControlEngine;
end;

procedure TJvOracleDatasetThreadDialogForm.SetDynControlEngine(const Value: TJvDynControlEngine);
begin
  if not Assigned(Value) then
    FDynControlEngine := DefaultDynControlEngine
  else
    FDynControlEngine := Value;
end;

procedure TJvOracleDatasetThreadDialogForm.TransferDialogOptions;
var
  H: Integer;
begin
  ClientWidth := 220;
  FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
  FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
  FCancelBtn.Left := Round((FCancelButtonPanel.Width - FCancelBtn.Width) / 2);
  FRowsPanel.Visible := DialogOptions.ShowRowsLabel;
  FTimePanel.Visible := DialogOptions.ShowTimeLabel;
  H := 10;
  if FRowsPanel.Visible then
    H := H + FRowsPanel.Height;
  if FTimePanel.Visible then
    H := H + FTimePanel.Height;
  if FCancelButtonPanel.Visible then
    H := H + FCancelButtonPanel.Height;
  ClientHeight := H;
end;

procedure TJvOracleDatasetThreadDialogForm.UpdateFormContents;
begin
  inherited UpdateFormContents;
  FillDialogData;
end;

//=== { TJvOracleDatasetDialogOptions } ======================================

constructor TJvOracleDatasetDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create(AOwner);
  FEnableCancelButton := True;
  FShowCancelButton := True;
  FShowRowsLabel := True;
  FShowTimeLabel := True;
end;

destructor TJvOracleDatasetDialogOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TJvOracleDatasetDialogOptions.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TJvOracleDatasetDialogOptions.SetDynControlEngine(const Value: TJvDynControlEngine);
begin
  FDynControlEngine := Value;
end;

procedure TJvOracleDatasetDialogOptions.SetEnableCancelButton(const Value: Boolean);
begin
  FEnableCancelButton := Value;
end;

procedure TJvOracleDatasetDialogOptions.SetFormStyle(const Value: TFormStyle);
begin
  FFormStyle := Value;
end;

procedure TJvOracleDatasetDialogOptions.SetShowCancelButton(const Value: Boolean);
begin
  FShowCancelButton := Value;
end;

procedure TJvOracleDatasetDialogOptions.SetShowRowsLabel(const Value: Boolean);
begin
  FShowRowsLabel := Value;
end;

procedure TJvOracleDatasetDialogOptions.SetShowTimeLabel(const Value: Boolean);
begin
  FShowTimeLabel := Value;
end;

procedure TJvOracleDatasetThread.intAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm);
begin
  DialogForm.ConnectedDataComponent := ConnectedDataset;
end;

procedure TJvOracleDatasetThread.CancelExecute;
begin
  if Assigned(ConnectedDataSet) then
    ConnectedDataSet.BreakExecution
  else
    inherited CancelExecute;
end;

// { TJvOracleDatasetThreadOptions } =========================================

constructor TJvOracleDatasetThreadOptions.Create;
begin
  inherited Create;
  FLastInThread := False;
  FOpenInThread := False;
  FPriority := tpIdle;
  FRefreshInThread := False;
  FShowExceptionMessage := True;
end;

//=== { TJvOracleDatasetEnhancedOptions } ====================================

constructor TJvOracleDatasetEnhancedOptions.Create;
begin
  inherited Create;
  FRefreshAsOpenClose := False;
  FRefreshLastPosition := False;
  FCapitalizeLabelOptions := TJvOracleDatasetCapitalizeLabelOptions.Create;
  FAllowedAfterFetchRecordActions := [todafPause, todafCancel];
end;

destructor TJvOracleDatasetEnhancedOptions.Destroy;
begin
  FreeAndNil(FCapitalizeLabelOptions);
  inherited Destroy;
end;

procedure TJvOracleDatasetEnhancedOptions.SetCapitalizeLabelOptions(const Value: TJvOracleDatasetCapitalizeLabelOptions);
begin
  FCapitalizeLabelOptions.Assign(Value);
end;

procedure TJvOracleDatasetEnhancedOptions.SetFetchRowsCheck(const Value: Integer);
begin
  FFetchRowsCheck := Value;
end;

procedure TJvOracleDatasetEnhancedOptions.SetFetchRowsFirst(const Value: Integer);
begin
  FFetchRowsFirst := Value;
end;

procedure TJvOracleDatasetEnhancedOptions.SetRefreshAsOpenClose(Value: Boolean);
begin
  FRefreshAsOpenClose := Value;
end;

procedure TJvOracleDatasetEnhancedOptions.SetRefreshLastPosition(const Value: Boolean);
begin
  FRefreshLastPosition := Value;
end;

//=== { TJvOracleDatasetCapitalizeLabelOptions } =============================

constructor TJvOracleDatasetCapitalizeLabelOptions.Create;
begin
  inherited Create;
  FAutoExecuteAfterOpen := False;
  FTrimToFirstBlank := False;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
end.

