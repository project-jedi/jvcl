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
    FDynControlEngine: TJvDynControlEngine;
    FEnableCancelButton: boolean;
    FFormStyle: tFormStyle;
    FShowCancelButton: boolean;
    procedure SetDynControlEngine(const Value: TJvDynControlEngine);
    procedure SetEnableCancelButton(const Value: boolean);
    procedure SetFormStyle(const Value: tFormStyle);
    procedure SetShowCancelButton(const Value: boolean);
  protected
  public
    constructor Create(AOwner: TJvCustomThreadDialog); override;
    destructor Destroy; override;
  published
    property DynControlEngine: TJvDynControlEngine
      read FDynControlEngine write SetDynControlEngine;
    property EnableCancelButton: boolean read FEnableCancelButton
      write SetEnableCancelButton default True;
    property FormStyle: tFormStyle read FFormStyle write SetFormStyle;
    property ShowCancelButton: boolean read FShowCancelButton
      write SetShowCancelButton default True;
  end;

  TJvOracleDatasetThreadOptions = class(TPersistent)
  private
    FLastInThread: boolean;
    FOpenInThread: boolean;
    FPriority: TThreadPriority;
    FRefreshInThread: boolean;
    FShowExceptionMessage: boolean;
  public
    constructor Create;
  published
    property LastInThread: boolean read FLastInThread write FLastInThread default False;
    property OpenInThread: boolean read FOpenInThread write FOpenInThread default False;
    property Priority: TThreadPriority read FPriority write FPriority default tpIdle;
    property RefreshInThread: boolean read FRefreshInThread
      write FRefreshInThread default False;
    property ShowExceptionMessage: boolean read FShowExceptionMessage
      write FShowExceptionMessage default True;
  end;

  TJvOracleDatasetCapitalizeLabelOptions = class(TPersistent)
  private
    FAutoExecuteAfterOpen: Boolean;
    FTrimToFirstBlank: Boolean;
  public
    constructor Create;
  published
    property AutoExecuteAfterOpen: Boolean read FAutoExecuteAfterOpen write
      FAutoExecuteAfterOpen default False;
    property TrimToFirstBlank: Boolean read FTrimToFirstBlank write
      FTrimToFirstBlank default False;
  end;

  TJvOracleDatasetEnhancedOptions = class(TPersistent)
  private
    FAllowedAfterFetchRecordActions: TJvOracleDatasetAllowedAfterFetchRecordActions;
    FCapitalizeLabelOptions: TJvOracleDatasetCapitalizeLabelOptions;
    FFetchRowsCheck: integer;
    FFetchRowsFirst: integer;
    FRefreshAsOpenClose: boolean;
    FRefreshLastPosition: boolean;
    procedure SetCapitalizeLabelOptions(const Value:
      TJvOracleDatasetCapitalizeLabelOptions);
    procedure SetFetchRowsCheck(const Value: integer);
    procedure SetFetchRowsFirst(const Value: integer);
    procedure SetRefreshAsOpenClose(Value: boolean);
    procedure SetRefreshLastPosition(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AllowedAfterFetchRecordActions:
        TJvOracleDatasetAllowedAfterFetchRecordActions read
        FAllowedAfterFetchRecordActions write FAllowedAfterFetchRecordActions
        default [todafPause, todafCancel, todafAll];
    property CapitalizeLabelOptions: TJvOracleDatasetCapitalizeLabelOptions read
      FCapitalizeLabelOptions write SetCapitalizeLabelOptions;
    property FetchRowsCheck: integer read FFetchRowsCheck write SetFetchRowsCheck;
    property FetchRowsFirst: integer read FFetchRowsFirst write SetFetchRowsFirst;
    property RefreshAsOpenClose: boolean read FRefreshAsOpenClose
      write SetRefreshAsOpenClose default False;
    property RefreshLastPosition: boolean read FRefreshLastPosition
      write SetRefreshLastPosition default False;
  end;

  TJvOracleDatasetThreadDialogForm = class(TJvCustomThreadDialogForm)
  private
    FRowsLabel, FTimeLabel: TControl;
    FRowsStaticText, FTimeStaticText: TWincontrol;
    FCancelBtn: TButton;
    FCancelButtonPanel, FRowsPanel, FTimePanel: TWinControl;
    FDialogOptions: TJvOracleDatasetDialogOptions;
    FDynControlEngine: TJvDynControlEngine;
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl; var Panel:
      TWincontrol; var LabelCtrl: TControl; var StaticText: TWincontrol; const
      BaseName: string);
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
    property DynControlEngine: TJvDynControlEngine
      read FDynControlEngine write SetDynControlEngine;
  published
    property DialogOptions: TJvOracleDatasetDialogOptions
      read GetDialogOptions write SetDialogOptions;
  end;

  TJvOracleDatasetThreadDialog = class(TJvCustomThreadDialog)
  private
    function GetDialogOptions: TJvOracleDatasetDialogOptions;
    procedure SetDialogOptions(const Value: TJvOracleDatasetDialogOptions);
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread):
      TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvOracleDatasetDialogOptions
      read GetDialogOptions write SetDialogOptions;
  end;

  TJvOracleDatasetThread = class(TJvThread)
  private
    FConnectedDataset: TJvOracleDataset;
  protected
    procedure intAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm);
      override;
  public
    procedure CancelExecute; override;
    property ConnectedDataset: TJvOracleDataset
      read FConnectedDataset write FConnectedDataset;
  end;

  TJvOracleDataSet = class(TOracleDataSet)
  private
    FAfterFetchRecord: TAfterFetchRecordEvent;
    FAfterOpen: TDataSetNotifyEvent;
    FAfterRefresh: TDataSetNotifyEvent;
    FAfterThreadExecution: TJvOracleDatasetThreadEvent;
    FBeforeOpen: TDataSetNotifyEvent;
    FBeforeRefresh: TNotifyEvent;
    FBeforeThreadExecution: TJvOracleDatasetThreadEvent;
    FCurrentAction: TJvOracledatasetAction;
    FCurrentFetchDuration: TDateTime;
    FCurrentOpenDuration: TDateTime;
    FCurrentOperation: TJvOracledatasetOperation;
    FCurrentOperationStart: tDateTime;
    FCurrentRow: integer;
    FEnhancedOptions: TJvOracleDatasetEnhancedOptions;
    FErrorMessage: string;
    FFetchMode: TJvOracleDatasetFetchMode;
    FFetchStartTime: TDateTime;
    FIgnoreRowsCheck: integer;
    FIntDatasetWasFiltered: boolean;
    FintQueryAllRecords: boolean;
    FIntRowCheckEnabled: boolean;
    FOpenStartTime: TDateTime;
    FSynchAfterFetchAction: TAfterFetchRecordAction;
    FSynchAfterFetchFilterAccept: boolean;
    FSynchAfterFetchSender: TOracleDataSet;
    FSynchMessageDlgBtn: word;
    FSynchMessageDlgMsg: string;
    FExecuteThread: TJvOracleDatasetThread;
    FLastRowChecked: integer;
    FMoveToRecordAfterOpen: longint;
    FThreadDialog: TJvOracleDatasetThreadDialog;
    FThreadOptions: TJvOracleDatasetThreadOptions;
    procedure EnableDatasetControls;
    function GetCurrentAction: TJvOracledatasetAction;
    function GetCurrentFetchDuration: TDateTime;
    function GetCurrentOpenDuration: TDateTime;
    function GetCurrentOperation: TJvOracledatasetOperation;
    function GetCurrentOperationAction: string;
    function GetDialogOptions: TJvOracleDatasetDialogOptions;
    function GetFetchMode: TJvOracleDatasetFetchMode;
    procedure SetCurrentAction(const Value: TJvOracledatasetAction);
    procedure SetCurrentFetchDuration(const Value: TDateTime);
    procedure SetCurrentOpenDuration(const Value: TDateTime);
    procedure SetDialogOptions(Value: TJvOracleDatasetDialogOptions);
    procedure SetEnhancedOptions(Value: TJvOracleDatasetEnhancedOptions);
    procedure SetFetchMode(const Value: TJvOracleDatasetFetchMode);
    procedure SetFetchStartTime(const Value: TDateTime);
    procedure SetIgnoreRowsCheck(const Value: integer);
    procedure SetOpenStartTime(const Value: TDateTime);
    procedure SetThreadOptions(const Value: TJvOracleDatasetThreadOptions);
    procedure SynchAfterFetchRecord;
    procedure SynchAfterThreadExecution;
    procedure SynchBeforeThreadExecution;
    procedure SynchContinueFetchMessageDlg;
    procedure SynchErrorMessageDlg;
  protected
    procedure ExecuteThreadSynchronize(Method: TThreadMethod);
    procedure DoThreadLast;
    procedure DoThreadOpen;
    procedure DoThreadRefresh;
    procedure HandleAfterOpenRefresh;
    procedure HandleAfterOpenRefreshThread;
    procedure HandleBeforeOpenRefresh;
    procedure InternalLast; override;
    procedure InternalRefresh; override;
    procedure intAfterOpen;
    procedure intAfterRefresh;
    procedure intAfterThreadExecution(DataSet: TJvOracleDataSet;
      Operation: TJvOracleDatasetOperation);
    procedure intBeforeOpen;
    procedure intBeforeRefresh;
    procedure intBeforeThreadExecution(DataSet: TJvOracleDataSet;
      Operation: TJvOracleDatasetOperation);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReplaceAfterFetchRecord(Sender: TOracleDataset;
      FilterAccept: boolean; var Action: TAfterFetchRecordAction);
    procedure ReplaceAfterOpen(Dataset: TDataset);
    procedure ReplaceAfterRefresh(Dataset: TDataset);
    procedure ReplaceBeforeOpen(Dataset: TDataset);
    procedure ReplaceBeforeRefresh(Dataset: TDataset);
    procedure SetActive(Value: boolean); override;
    procedure SetErrorMessage(const Value: string);
    procedure ThreadExecute(Sender: TObject; Params: Pointer);
    property CurrentOperation: TJvOracledatasetOperation read GetCurrentOperation;
    property FetchMode: TJvOracleDatasetFetchMode read GetFetchMode write SetFetchMode;
    property FetchStartTime: TDateTime read FFetchStartTime write SetFetchStartTime;
    property IgnoreRowsCheck: integer read FIgnoreRowsCheck write SetIgnoreRowsCheck;
    property IntRowCheckEnabled: boolean read FIntRowCheckEnabled
      write FIntRowCheckEnabled;
    property OpenStartTime: TDateTime read FOpenStartTime write SetOpenStartTime;
    property ExecuteThread: TJvOracleDatasetThread read FExecuteThread;
    property ThreadDialog: TJvOracleDatasetThreadDialog read FThreadDialog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BreakExecution;
    procedure CapitalizeDatasetLabels;
    class function CreateJvOracleDataSet(AOwner: TComponent): TJvOracleDataSet;
    procedure MoveTo(Position: integer);
    function ThreadIsActive: boolean;
    property CurrentAction: TJvOracledatasetAction
      read GetCurrentAction write SetCurrentAction;
    property CurrentFetchDuration: TDateTime
      read GetCurrentFetchDuration write SetCurrentFetchDuration;
    property CurrentOpenDuration: TDateTime
      read GetCurrentOpenDuration write SetCurrentOpenDuration;
    property CurrentOperationAction: string read GetCurrentOperationAction;
    property CurrentRow: integer read FCurrentRow;
    property ErrorMessage: string read FErrorMessage write SetErrorMessage;
  published
    property AfterFetchRecord: TAfterFetchRecordEvent
      read FAfterFetchRecord write FAfterFetchRecord;
    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeOpen: TDataSetNotifyEvent read FBeforeOpen write FBeforeOpen;
    property DialogOptions: TJvOracleDatasetDialogOptions
      read GetDialogOptions write SetDialogOptions;
    property EnhancedOptions: TJvOracleDatasetEnhancedOptions
      read FEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvOracleDatasetThreadOptions
      read FThreadOptions write SetThreadOptions;
    property AfterRefresh: TDataSetNotifyEvent read FAfterRefresh write FAfterRefresh;
    property AfterThreadExecution: TJvOracleDatasetThreadEvent
      read FAfterThreadExecution write FAfterThreadExecution;
    property BeforeRefresh: TNotifyEvent read FBeforeRefresh write FBeforeRefresh;
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

uses JvDynControlEngineIntf, JvResources, JvDSADialogs, Dialogs, DateUtils;

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
  i, j: integer;
  s: string;
  Upper: boolean;
begin
  if Active then
    for i := 0 to FieldCount - 1 do
    begin
      s := LowerCase(Fields[i].DisplayLabel);
      Upper := True;
      for j := 1 to Length(s) do
        if s[j] in ['_', '$', ' '] then
        begin
          Upper := True;
          s[j] := ' ';
        end
        else if Upper then
        begin
          s[j] := UpCase(s[j]);
          Upper := False;
        end;
      if EnhancedOptions.CapitalizeLabelOptions.TrimToFirstBlank then
      begin
        j := Pos(' ', s);
        if j > 0 then
          s := Copy(s, j + 1, Length(s) - j);
      end;
      Fields[i].DisplayLabel := s;
    end;
end;

class function TJvOracleDataSet.CreateJvOracleDataSet(AOwner:
  TComponent): TJvOracleDataSet;
begin
  Result := TJvOracleDataSet.Create(AOwner);
end;

procedure TJvOracleDataSet.EnableDatasetControls;
var
  p: integer;
begin
  try
    UpdateCursorPos;
  except
    on e: Exception do
  end;
  EnableControls;
  try
    if not ControlsDisabled and Active then
    begin
      p := RecNo;
      First;
      MoveBy(p - 1);
    end;
  except
    on e: Exception do
  end;
end;

procedure TJvOracleDataSet.ExecuteThreadSynchronize(Method: TThreadMethod);
begin
  if not ExecuteThread.Terminated then
    ExecuteThread.Synchronize(Method)
  else
    Method;
end;

function TJvOracleDataSet.GetCurrentAction: TJvOracledatasetAction;
begin
  Result := FCurrentAction;
end;

function TJvOracleDataSet.GetCurrentFetchDuration: TDateTime;
begin
  case CurrentAction of
    todaOpen: Result := 0;
    todaNothing: Result := FCurrentFetchDuration;
    todaFetch: Result := FCurrentFetchDuration + (Now - FCurrentOperationStart);
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

function TJvOracleDataSet.GetCurrentOperation: TJvOracledatasetOperation;
begin
  Result := FCurrentOperation;
end;

function TJvOracleDataSet.GetCurrentOperationAction: string;
begin
  case CurrentOperation of
    todoOpen:
      case CurrentAction of
        todaOpen: Result := SODSOpenQuery;
        todaFetch: Result := SODSOpenQueryFetchRecords;
      end;
    todoRefresh:
      case CurrentAction of
        todaOpen: Result := SODSRefreshQuery;
        todaFetch: Result := SODSRefreshQueryFetchRecords;
      end;
    todoFetch: Result := SODSFetchRecords;
    todoLast: Result := SODSGotoLastFetchRecords;
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
  inherited SetActive(True);
  HandleAfterOpenRefreshThread;
end;

procedure TJvOracleDataSet.DoThreadRefresh;
begin
  if not EnhancedOptions.RefreshAsOpenClose then
    inherited InternalRefresh
  else
  begin
    Close;
    InternalOpen;
  end;
  HandleAfterOpenRefreshThread;
end;

function TJvOracleDataSet.GetDialogOptions: TJvOracleDatasetDialogOptions;
begin
  Result := ThreadDialog.DialogOptions;
end;

procedure TJvOracleDataSet.HandleAfterOpenRefresh;
begin
  Filtered := fIntDatasetWasFiltered;
  if FMoveToRecordAfterOpen > 0 then
    MoveTo(FMoveToRecordAfterOpen)
  else
    First;
  CurrentAction := todaNothing;
end;

procedure TJvOracleDataSet.HandleAfterOpenRefreshThread;
begin
  CurrentOpenDuration := Now - FCurrentOperationStart;
  FCurrentOperationStart := Now;
  CurrentFetchDuration := 0;
  CurrentAction := todaFetch;
  First;
  if fIntQueryAllRecords then
  begin
    QueryAllRecords := True;
    inherited InternalLast;
  end {*** IF fIntQueryAllRecords THEN ***}
  else if (EnhancedOptions.FetchRowsFirst > RecordCount) or
    (FMoveToRecordAfterOpen > RecordCount) then
    if (FMoveToRecordAfterOpen > EnhancedOptions.FetchRowsFirst) then
      MoveBy(FMoveToRecordAfterOpen - 1)
    else
      MoveBy(EnhancedOptions.FetchRowsFirst - 1);
end;

procedure TJvOracleDataSet.HandleBeforeOpenRefresh;
begin
  CurrentOpenDuration := 0;
  CurrentFetchDuration := 0;
  IntRowCheckEnabled := True;
  FCurrentRow := 0;
  FCurrentOperationStart := Now;
  CurrentAction := todaOpen;
  fintQueryAllRecords := QueryAllRecords;
  fIntDatasetWasFiltered := Filtered;
  FLastRowChecked := 0;
  Filtered := False;
  QueryAllRecords := False;
end;

procedure TJvOracleDataSet.InternalLast;
begin
  FCurrentOperation := todoLast;
  if not ThreadOptions.LastInThread or ThreadIsActive or
    (csDesigning in ComponentState) then
  begin
    inherited InternalLast;
    FCurrentOperation := todoNothing;
  end
  else
    ExecuteThread.ExecuteWithDialog(nil);
end;

procedure TJvOracleDataSet.InternalRefresh;
var
  ThreadAllowed: boolean;
begin
  ThreadAllowed := True;
  if Assigned(Master) and
    (Master is TJvOracleDataSet) then
    ThreadAllowed := not TJvOracleDataSet(Master).ThreadIsActive;
  FCurrentOperation := todoRefresh;
  if not ThreadOptions.RefreshInThread or not ThreadAllowed or
    ThreadIsActive or
    (csDesigning in ComponentState) then
  begin
    inherited InternalRefresh;
    FCurrentOperation := todoNothing;
  end
  else
    ExecuteThread.ExecuteWithDialog(nil);
end;

procedure TJvOracleDataSet.intAfterOpen;
begin
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
  if EnhancedOptions.CapitalizeLabelOptions.AutoExecuteAfterOpen then
    CapitalizeDatasetLabels;
end;

procedure TJvOracleDataSet.intAfterRefresh;
begin
  if Assigned(FAfterRefresh) then
    FAfterRefresh(Self);
end;

procedure TJvOracleDataSet.intAfterThreadExecution(DataSet: TJvOracleDataSet;
  Operation: TJvOracleDatasetOperation);
begin
  if Assigned(FAfterThreadExecution) then
    FAfterThreadExecution(DataSet, Operation);
end;

procedure TJvOracleDataSet.intBeforeOpen;
begin
  if Assigned(FBeforeOpen) then
    FBeforeOpen(Self);
end;

procedure TJvOracleDataSet.intBeforeRefresh;
begin
  if Assigned(FBeforeRefresh) then
    FBeforeRefresh(Self);
end;

procedure TJvOracleDataSet.intBeforeThreadExecution(DataSet: TJvOracleDataSet;
  Operation: TJvOracleDatasetOperation);
begin
  if Assigned(FBeforeThreadExecution) then
    FBeforeThreadExecution(DataSet, Operation);
end;

procedure TJvOracleDataSet.MoveTo(Position: integer);
begin
  MoveBy(Position - Recno);
end;

procedure TJvOracleDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = ThreadDialog) and
    (Operation = opRemove) then
    FThreadDialog := nil;
end;

procedure TJvOracleDataSet.ReplaceAfterFetchRecord(Sender: TOracleDataset;
  FilterAccept: boolean; var Action: TAfterFetchRecordAction);
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
    if (CurrentRow >= FLastRowChecked + EnhancedOptions.FetchRowsCheck) then
    begin
      fCurrentFetchDuration := fCurrentFetchDuration + Now - FCurrentOperationStart;
      CurrentAction := todaNothing;
      FLastRowChecked := CurrentRow;
      FSynchMessageDlgMsg := Format(SODSRowsFetchedContinue, [CurrentRow]);
      ExecuteThreadSynchronize(SynchContinueFetchMessageDlg);
      case FSynchMessageDlgBtn of
        mrYes: Action := afContinue;
        mrAll:
          begin
            Action := afContinue;
            IntRowCheckEnabled := False;
          end;
        mrAbort: Action := afCancel;
        mrCancel: Action := afPause;
        mrNo: Action := afStop;
      else
        Action := afStop;
      end; {*** case fGlobalBtn of ***}
      CurrentAction := todaFetch;
      FCurrentOperationStart := Now;
    end; {*** if  (Sender.CurrentRow MOD FetchRowsCheck) = 0 then ***}
end;

procedure TJvOracleDataSet.ReplaceAfterOpen(Dataset: TDataset);
begin
  if (CurrentOperation <> todoRefresh) then
    HandleAfterOpenRefresh;
  if Assigned(FAfterOpen) then
    ExecuteThreadSynchronize(intAfterOpen);
end;

procedure TJvOracleDataSet.ReplaceAfterRefresh(Dataset: TDataset);
begin
  HandleAfterOpenRefresh;
  if Assigned(FAfterRefresh) then
    ExecuteThreadSynchronize(intAfterRefresh);
end;

procedure TJvOracleDataSet.ReplaceBeforeOpen(Dataset: TDataset);
begin
  if (CurrentOperation <> todoRefresh) then
  begin
    FMoveToRecordAfterOpen := -1;
    HandleBeforeOpenRefresh;
  end;
  if Assigned(FBeforeOpen) then
    ExecuteThreadSynchronize(intBeforeOpen);
end;

procedure TJvOracleDataSet.ReplaceBeforeRefresh(Dataset: TDataset);
begin
  if EnhancedOptions.RefreshLastPosition then
    FMoveToRecordAfterOpen := RecNo
  else
    FMoveToRecordAfterOpen := -1;
  HandleBeforeOpenRefresh;
  if Assigned(FBeforeRefresh) then
    ExecuteThreadSynchronize(intBeforeRefresh);
end;

procedure TJvOracleDataSet.SetActive(Value: boolean);
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
    if not ThreadOptions.OpenInThread or ThreadIsActive or
      (csDesigning in ComponentState) then
    begin
      inherited SetActive(Value);
      if CurrentOperation <> todoRefresh then
        FCurrentOperation := todoNothing;
    end
    else
      ExecuteThread.ExecuteWithDialog(nil);
  end;
end;

procedure TJvOracleDataSet.SetCurrentAction(const Value: TJvOracledatasetAction);
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

procedure TJvOracleDataSet.SetIgnoreRowsCheck(const Value: integer);
begin
  FIgnoreRowsCheck := Value;
end;

procedure TJvOracleDataSet.SetOpenStartTime(const Value: TDateTime);
begin
  FOpenStartTime := Value;
end;

procedure TJvOracleDataSet.SetThreadOptions(
  const Value: TJvOracleDatasetThreadOptions);
begin
  FThreadOptions.Assign(Value);
end;

procedure TJvOracleDataSet.SynchAfterFetchRecord;
begin
  if Assigned(FAfterFetchRecord) then
    FAfterFetchRecord(FSynchAfterFetchSender, FSynchAfterFetchFilterAccept,
      FSynchAfterFetchAction);
end;

procedure TJvOracleDataSet.SynchAfterThreadExecution;
begin
  intAfterThreadExecution(Self, CurrentOperation);
end;

procedure TJvOracleDataSet.SynchBeforeThreadExecution;
begin
  intBeforeThreadExecution(Self, CurrentOperation);
end;

procedure TJvOracleDataSet.SynchContinueFetchMessageDlg;
var
  Buttons: array of string;
  Results: array of Integer;
  l: Integer;

  procedure AddButton(Caption: string; ResultValue: Integer);
  begin
    inc(l);
    SetLength (Buttons, l);
    SetLength (Results, l);
    Buttons[l-1] := Caption;
    Results[l-1] := ResultValue;
  end;
begin
  l := 0;
  AddButton (SODSContinueYes, Integer(mrYes));
  if todafPause in EnhancedOptions.AllowedAfterFetchRecordActions then
    AddButton (SODSContinuePause, Integer(mrCancel));
  AddButton (SODSContinueNo, Integer(mrNo));
  if todafAll in EnhancedOptions.AllowedAfterFetchRecordActions then
    AddButton (SODSContinueClose, Integer(mrAbort));
  AddButton (SODSContinueAll, Integer(mrAll));
  if todafCancel in EnhancedOptions.AllowedAfterFetchRecordActions then
    AddButton (SODSContinueClose, Integer(mrAbort));
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
var
  CurrControlsDisabled: boolean;
begin
  try
    SetErrorMessage('');
    CurrControlsDisabled := ControlsDisabled;
    ExecuteThreadSynchronize(SynchBeforeThreadExecution);
    try
      if not CurrControlsDisabled then
        ExecuteThreadSynchronize(DisableControls);
      try
        case FCurrentOperation of
          todoOpen: DoThreadOpen;
          todoRefresh: DoThreadRefresh;
          todoLast: DoThreadLast;
        end;
      except
        on e: Exception do
        begin
          SetErrorMessage(e.Message);
          if ThreadOptions.ShowExceptionMessage then
          begin
            FSynchMessageDlgMsg := e.Message;
            ExecuteThreadSynchronize(SynchErrorMessageDlg);
          end;
        end; {*** on e:exception DO ***}
      end;
    finally
      try
        if not CurrControlsDisabled then
        begin
          ExecuteThreadSynchronize(EnableDatasetControls);
          while ControlsDisabled do
            ExecuteThreadSynchronize(EnableDatasetControls);
        end;
      except
        on e: Exception do
      end;
    end;
    ExecuteThreadSynchronize(SynchAfterThreadExecution);
  finally
    FCurrentOperation := todoNothing;
  end;
end;

function TJvOracleDataSet.ThreadIsActive: boolean;
begin
  Result := not ExecuteThread.Terminated;
end;

function TJvOracleDatasetThreadDialog.CreateDialogOptions: TJvCustomThreadDialogOptions;
begin
  Result := TJvOracleDatasetDialogOptions.Create(Self);
end;

function TJvOracleDatasetThreadDialog.CreateThreadDialogForm(
  ConnectedThread: TJvThread): TJvCustomThreadDialogForm;
var
  ThreadDialogForm: TJvOracleDatasetThreadDialogForm;
begin
  if DialogOptions.ShowDialog then
  begin
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

procedure TJvOracleDatasetThreadDialog.SetDialogOptions(
  const Value: TJvOracleDatasetDialogOptions);
begin
  inherited DialogOptions.Assign(Value);
end;

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
  MainPanel := DynControlEngine.CreatePanelControl(Self, Self,
    'MainPanel', '', alClient);
  if not Supports(MainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);

  CreateTextPanel(Self, MainPanel, FTimePanel, FTimeLabel, FTimeStaticText,
    'Time');
  if Supports(FTimeLabel, IJvDynControl, ITmpControl) then
    ITmpControl.ControlSetCaption(SODSOpenFetch);
  CreateTextPanel(Self, MainPanel, FRowsPanel, FRowsLabel, FRowsStaticText,
    'Rows');
  if Supports(FRowsLabel, IJvDynControl, ITmpControl) then
    ITmpControl.ControlSetCaption(SODSCurrentRecord);
  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self,
    MainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', RsButtonCancelCaption, '', DefaultCancelBtnClick,
    True, True);
  with FCancelBtn do
  begin
    Anchors := [akTop];
    Top := 2;
    FCancelButtonPanel.Height := FCancelBtn.Height + 3;
  end;

  BorderIcons := [];
  BorderStyle := bsDialog;
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
  AParent: TWinControl; var Panel: TWincontrol; var LabelCtrl: TControl; var
  StaticText: TWincontrol; const BaseName: string);
var
  ITmpPanel: IJvDynControlPanel;
  ITmpAutoSize: IJvDynControlAutoSize;
  ITmpAlignment: IJvDynControlAlignment;
begin
  Panel := DynControlEngine.CreatePanelControl(AOwner, AParent,
    BaseName + 'Panel', '', alTop);
  if not Supports(Panel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 3);
  LabelCtrl := DynControlEngine.CreateLabelControl(AOwner, Panel,
    BaseName + 'Label', '', nil);
  with LabelCtrl do
  begin
    Top := 1;
    Left := 1;
  end;
  StaticText := DynControlEngine.CreateStaticTextControl(AOwner,
    Panel, BaseName + 'StaticText', '');
  if Supports(StaticText, IJvDynControlAutoSize, ITmpAutoSize) then
    ITmpAutoSize.ControlSetAutoSize(True);
  if Supports(StaticText, IJvDynControlAlignment, ITmpAlignment) then
    ITmpAlignment.ControlSetAlignment(taCenter);
  with StaticText do
  begin
    Top := 1;
    Left := 80;
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
    Caption := ConnectedDataset.CurrentOperationAction;
    if Supports(FRowsStaticText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(IntToStr(ConnectedDataset.CurrentRow));
    if Supports(FTimeStaticText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(
        FormatDateTime('hh:nn:ss', ConnectedDataset.CurrentOpenDuration) + ' / ' +
        FormatDateTime('hh:nn:ss', ConnectedDataset.CurrentFetchDuration));
  end;
end;

function TJvOracleDatasetThreadDialogForm.GetConnectedDataset: TJvOracleDataset;
begin
  if Assigned(ConnectedDataComponent) and
    (ConnectedDataComponent is TJvOracleDataSet) then
    Result := TJvOracleDataSet(ConnectedDataComponent)
  else
    Result := nil;
end;

function TJvOracleDatasetThreadDialogForm.GetDialogOptions:
  TJvOracleDatasetDialogOptions;
begin
  Result := FDialogOptions;
end;

procedure TJvOracleDatasetThreadDialogForm.SetDialogOptions(
  const Value: TJvOracleDatasetDialogOptions);
begin
  FDialogOptions := Value;
  DynControlEngine := DialogOptions.DynControlEngine;
end;

procedure TJvOracleDatasetThreadDialogForm.SetDynControlEngine(
  const Value: TJvDynControlEngine);
begin
  if not Assigned(Value) then
    FDynControlEngine := DefaultDynControlEngine
  else
    FDynControlEngine := Value;
end;

procedure TJvOracleDatasetThreadDialogForm.TransferDialogOptions;
var
  h: integer;
begin
  ClientWidth := 220;
  FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
  FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
  FCancelBtn.Left := Round((FCancelButtonPanel.Width - FCancelBtn.Width) / 2);
  h := 10;
  if FRowsPanel.Visible then
    h := h + FRowsPanel.Height;
  if FTimePanel.Visible then
    h := h + FTimePanel.Height;
  if FCancelButtonPanel.Visible then
    h := h + FCancelButtonPanel.Height;
  ClientHeight := h;
end;

procedure TJvOracleDatasetThreadDialogForm.UpdateFormContents;
begin
  inherited UpdateFormContents;
  FillDialogData;
end;

constructor TJvOracleDatasetDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create(AOwner);
  FEnableCancelButton := True;
  FShowCancelButton := True;
end;

destructor TJvOracleDatasetDialogOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TJvOracleDatasetDialogOptions.SetDynControlEngine(
  const Value: TJvDynControlEngine);
begin
  FDynControlEngine := Value;
end;

procedure TJvOracleDatasetDialogOptions.SetEnableCancelButton(const Value: boolean);
begin
  FEnableCancelButton := Value;
end;

procedure TJvOracleDatasetDialogOptions.SetFormStyle(const Value: tFormStyle);
begin
  FFormStyle := Value;
end;

procedure TJvOracleDatasetDialogOptions.SetShowCancelButton(const Value: boolean);
begin
  FShowCancelButton := Value;
end;

procedure TJvOracleDatasetThread.intAfterCreateDialogForm(DialogForm:
  TJvCustomThreadDialogForm);
begin
  DialogForm.ConnectedDataComponent := ConnectedDataset;
end;

procedure TJvOracleDatasetThread.CancelExecute;
begin
  if Assigned(ConnectedDataSet) then
    ConnectedDataSet.BreakExecution
  else
    inherited;
end;

constructor TJvOracleDatasetThreadOptions.Create;
begin
  inherited Create;
  FLastInThread := False;
  FOpenInThread := False;
  FPriority := tpIdle;
  FRefreshInThread := False;
  FShowExceptionMessage := True;
end;

constructor TJvOracleDatasetEnhancedOptions.Create;
begin
  inherited Create;
  FRefreshAsOpenClose := False;
  FRefreshLastPosition := False;
  FCapitalizeLabelOptions := TJvOracleDatasetCapitalizeLabelOptions.Create();
  FAllowedAfterFetchRecordActions := [todafPause, todafCancel];
end;

destructor TJvOracleDatasetEnhancedOptions.Destroy;
begin
  FreeAndNil(FCapitalizeLabelOptions);
  inherited Destroy;
end;

procedure TJvOracleDatasetEnhancedOptions.SetCapitalizeLabelOptions(const
  Value: TJvOracleDatasetCapitalizeLabelOptions);
begin
  FCapitalizeLabelOptions.Assign(Value);
end;

procedure TJvOracleDatasetEnhancedOptions.SetFetchRowsCheck(const Value: integer);
begin
  FFetchRowsCheck := Value;
end;

procedure TJvOracleDatasetEnhancedOptions.SetFetchRowsFirst(const Value: integer);
begin
  FFetchRowsFirst := Value;
end;

procedure TJvOracleDatasetEnhancedOptions.SetRefreshAsOpenClose(Value: boolean);
begin
  FRefreshAsOpenClose := Value;
end;

procedure TJvOracleDatasetEnhancedOptions.SetRefreshLastPosition(
  const Value: boolean);
begin
  FRefreshLastPosition := Value;
end;

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

