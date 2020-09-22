{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBaseDBThreadedDataset.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  Oracle Dataset with Threaded Functions

Known Issues:
-----------------------------------------------------------------------------}
// $Id: jvcl/run/JvBaseDBThreadedDataset.pas jfudickar date $

unit JvBaseDBThreadedDataset;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, StdCtrls, Forms, Controls,
  DB,
  JvThread, JvThreadDialog, JvDynControlEngineIntf, JvDynControlEngine;

type
  TJvThreadedDatasetOperation = (tdoOpen, tdoFetch, tdoLast, tdoRefresh, tdoNothing);
  TJvThreadedDatasetAction = (tdaOpen, tdaFetch, tdaNothing, tdaCancel);

  TJvThreadedDatasetFetchMode = (tdfmFetch, tdfmBreak, tdfmStop, tdfmNothing);

  TJvThreadedDatasetContinueCheckResult = (tdccrContinue, tdccrPause, tdccrStop, tdccrAll, tdccrCancel);

  TJvThreadedDatasetContinueAllowButton = (tdcaPause, tdcaStop, tdcaAll);
  TJvThreadedDatasetContinueAllowButtons = set of TJvThreadedDatasetContinueAllowButton;

  TJvBaseDatasetThreadHandler = class;

  TJvThreadedDatasetThreadEvent = procedure(DataSet: TDataSet;
    Operation: TJvThreadedDatasetOperation) of object;
  TJvThreadedDatasetThreadExceptionEvent = procedure(DataSet: TDataSet;
      Operation: TJvThreadedDatasetOperation; E: Exception) of object;


  TJvThreadedDatasetDialogOptions = class;
  TJvThreadedDatasetThreadOptions = class;

  IJvThreadedDatasetInterface = interface
    ['{220CC94D-AA41-4195-B90C-ECA24BAD3CDB}']
    procedure BreakExecution;
    procedure BringThreadDialogToFront;
    function CurrentFetchDuration: TDateTime;
    function CurrentOpenDuration: TDateTime;
    procedure DoInheritedInternalLast;
    procedure DoInheritedInternalRefresh;
    procedure DoInheritedSetActive(Active: Boolean);
    procedure DoInternalOpen;
    function IsThreadAllowed: Boolean;
    function ThreadIsActive: Boolean;
    procedure DoInheritedBeforeOpen;
    procedure DoInheritedAfterOpen;
    procedure DoInheritedBeforeRefresh;
    procedure DoInheritedAfterRefresh;
    procedure DoInheritedAfterScroll;
    function DoGetInheritedNextRecord : Boolean;
    function EofReached: Boolean;
    function ErrorException: Exception;
    function ErrorMessage: string;
    function GetAfterOpenFetch: TDataSetNotifyEvent;
    function GetAfterThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
    function GetDatasetFetchAllRecords: Boolean;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetOnThreadException: TJvThreadedDatasetThreadExceptionEvent;
    function GetThreadOptions: TJvThreadedDatasetThreadOptions;
    procedure SetAfterOpenFetch(const Value: TDataSetNotifyEvent);
    procedure SetAfterThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetBeforeThreadExecution(const Value: TJvThreadedDatasetThreadEvent);
    procedure SetDatasetFetchAllRecords(const Value: Boolean);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetOnThreadException(const Value: TJvThreadedDatasetThreadExceptionEvent);
    procedure SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read GetAfterThreadExecution write SetAfterThreadExecution;
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read GetBeforeThreadExecution write
        SetBeforeThreadExecution;
    property OnThreadException: TJvThreadedDatasetThreadExceptionEvent read GetOnThreadException write SetOnThreadException;
    property AfterOpenFetch: TDataSetNotifyEvent read GetAfterOpenFetch write SetAfterOpenFetch;
    property DatasetFetchAllRecords: Boolean read GetDatasetFetchAllRecords write SetDatasetFetchAllRecords;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read GetThreadOptions write SetThreadOptions;
  end;

  TJvThreadedDatasetDialogOptions = class(TJvCustomThreadDialogOptions)
  private
    FCaption: string;
    FDynControlEngine: TJvDynControlEngine;
    FEnableCancelButton: Boolean;
    FFormStyle: TFormStyle;
    FShowCancelButton: Boolean;
    FShowProgressBar: Boolean;
    FShowRowsLabel: Boolean;
    FShowTimeLabel: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetDynControlEngine(const Value: TJvDynControlEngine);
    procedure SetEnableCancelButton(const Value: Boolean);
    procedure SetFormStyle(const Value: TFormStyle);
    procedure SetShowCancelButton(const Value: Boolean);
    procedure SetShowProgressBar(const Value: Boolean);
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
    property ShowProgressBar: Boolean read FShowProgressBar write SetShowProgressBar default true;
    property ShowRowsLabel: Boolean read FShowRowsLabel write SetShowRowsLabel default True;
    property ShowTimeLabel: Boolean read FShowTimeLabel write SetShowTimeLabel default True;
  end;

  TJvThreadedDatasetThreadOptions = class(TPersistent)
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
    property ShowExceptionMessage: Boolean read FShowExceptionMessage write FShowExceptionMessage default True;
  end;

  TJvThreadedDatasetCapitalizeLabelOptions = class(TPersistent)
  private
    FAutoExecuteAfterOpen: Boolean;
    FTrimToFirstBlank: Boolean;
  public
    constructor Create; virtual;
  published
    property AutoExecuteAfterOpen: Boolean read FAutoExecuteAfterOpen write FAutoExecuteAfterOpen default False;
    property TrimToFirstBlank: Boolean read FTrimToFirstBlank write FTrimToFirstBlank default False;
  end;

  TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions = class(TPersistent)
  private
    FAll: Boolean;
    FPause: Boolean;
    FCancel: Boolean;
  protected
    property Pause: Boolean read FPause write FPause default False;
    property Cancel: Boolean read FCancel write FCancel default False;
    property All: Boolean read FAll write FAll default False;
  public
    constructor Create; virtual;
  end;

  TJvBaseThreadedDatasetEnhancedOptions = class(TPersistent)
  private
    FAllowedContinueRecordFetchOptions: TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions;
    FCapitalizeLabelOptions: TJvThreadedDatasetCapitalizeLabelOptions;
    FFetchRowsCheck: Integer;
    FFetchRowsFirst: Integer;
    FRefreshAsOpenClose: Boolean;
    FRefreshLastPosition: Boolean;
    procedure SetCapitalizeLabelOptions(const Value: TJvThreadedDatasetCapitalizeLabelOptions);
    procedure SetFetchRowsCheck(const Value: Integer);
    procedure SetFetchRowsFirst(const Value: Integer);
    procedure SetRefreshAsOpenClose(Value: Boolean);
    procedure SetRefreshLastPosition(const Value: Boolean);
  protected
    function CreateAllowedContinueRecordFetchOptions: TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property AllowedContinueRecordFetchOptions: TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions read
        FAllowedContinueRecordFetchOptions write FAllowedContinueRecordFetchOptions;
    property CapitalizeLabelOptions: TJvThreadedDatasetCapitalizeLabelOptions read FCapitalizeLabelOptions write
        SetCapitalizeLabelOptions;
    property FetchRowsCheck: Integer read FFetchRowsCheck write SetFetchRowsCheck default 2000;
    property FetchRowsFirst: Integer read FFetchRowsFirst write SetFetchRowsFirst default 1000;
    property RefreshAsOpenClose: Boolean read FRefreshAsOpenClose write SetRefreshAsOpenClose default False;
    property RefreshLastPosition: Boolean read FRefreshLastPosition write SetRefreshLastPosition default False;
  end;

  TJvDatasetThreadDialogForm = class(TJvDynControlEngineThreadDialogForm)
  private
    FRowsLabel: TControl;
    FTimeLabel: TControl;
    FRowsStaticText: TWinControl;
    FTimeStaticText: TWinControl;
    FCancelBtn: TButton;
    FCancelButtonPanel: TWinControl;
    FRowsPanel: TWinControl;
    FTimePanel: TWinControl;
    FProgressbar: TWinControl;
    FProgressbarPanel: TWinControl;
    IProgressBarControl : IJvDynControlProgressbar;
    FDialogOptions: TJvThreadedDatasetDialogOptions;
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl; var Panel: TWinControl; var LabelCtrl: TControl;
        var StaticText: TWinControl; const BaseName: string);
    function GetConnectedDataset: TDataSet;
    function GetConnectedDatasetHandler: TJvBaseDatasetThreadHandler;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    procedure SetDialogOptions(const Value: TJvThreadedDatasetDialogOptions);
  protected
    procedure FillDialogData;
    procedure FreeFormControls; override;
    procedure InitializeFormContents; override;
    procedure UpdateFormContents; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateFormControls; override;
    procedure TransferDialogOptions; override;
    property ConnectedDataset: TDataSet read GetConnectedDataset;
    property ConnectedDatasetHandler: TJvBaseDatasetThreadHandler read GetConnectedDatasetHandler;
  published
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
  end;

  TJvDatasetThreadDialog = class(TJvCustomThreadDialog)
  private
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    procedure SetDialogOptions(const Value: TJvThreadedDatasetDialogOptions);
  protected
    function CreateDialogOptions: TJvCustomThreadDialogOptions; override;
  public
    function CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm; override;
  published
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
  end;

  TJvBaseDatasetThread = class(TJvThread)
  private
    FConnectedDataset: TDataSet;
    FConnectedDatasetInterface: IJvThreadedDatasetInterface;
    FConnectedDatasetThreadHandler: TJvBaseDatasetThreadHandler;
    procedure SetConnectedDataset(const Value: TDataSet);
    procedure SetConnectedDatasetThreadHandler(const Value: TJvBaseDatasetThreadHandler);
  protected
    procedure InternalAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm); override;
    property ConnectedDatasetInterface: IJvThreadedDatasetInterface read FConnectedDatasetInterface;
    property ConnectedDataset: TDataSet read FConnectedDataset;
  public
    procedure CancelExecute; override;
    property ConnectedDatasetThreadHandler: TJvBaseDatasetThreadHandler read FConnectedDatasetThreadHandler write
        SetConnectedDatasetThreadHandler;
  end;

  TJvBaseDatasetThreadHandler = class(TComponent)
  private
    FAfterOpenFetch: TDataSetNotifyEvent;
    FAfterThreadExecution: TJvThreadedDatasetThreadEvent;
    FBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
    FIntCurrentAction: TJvThreadedDatasetAction;
    FIntCurrentFetchDuration: TDateTime;
    FIntCurrentOpenDuration: TDateTime;
    FIntCurrentOperation: TJvThreadedDatasetOperation;
    FIntCurrentOperationStart: TDateTime;
    FCurrentRow: Integer;
    FDataset: TDataSet;
    FEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions;
    FErrorException: Exception;
    FErrorMessage: string;
    FExecuteThread: TJvBaseDatasetThread;
    FFetchMode: TJvThreadedDatasetFetchMode;
    FIntDatasetWasFiltered: Boolean;
    FIntDatasetFetchAllRecords: Boolean;
    FIntRowCheckEnabled: Boolean;
    FIThreadedDatasetInterface: IJvThreadedDatasetInterface;
    FLastRowChecked: Integer;
    FMaxRowChecked: Integer;
    FAfterOpenRecordPosition: Longint;
    FEofReached: Boolean;
    FOnThreadException: TJvThreadedDatasetThreadExceptionEvent;
    FOperationWasHandledInThread: Boolean;
    FSynchMessageDlgBtn: Word;
    FSynchMessageDlgMsg: string;
    FThreadDialog: TJvDatasetThreadDialog;
    FThreadOptions: TJvThreadedDatasetThreadOptions;
    IntThreadException: Exception;
    MessageDlgIsActive: Boolean;
    function GetCurrentAction: TJvThreadedDatasetAction;
    function GetCurrentFetchDuration: tDateTime;
    function GetCurrentOpenDuration: tDateTime;
    function GetIntCurrentAction: TJvThreadedDatasetAction;
    function GetIntCurrentFetchDuration: TDateTime;
    function GetIntCurrentOpenDuration: TDateTime;
    function GetCurrentOperation: TJvThreadedDatasetOperation;
    function GetIntCurrentOperation: TJvThreadedDatasetOperation;
    function GetCurrentOperationAction: string;
    function GetDatasetFetchAllRecords: Boolean;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetFetchMode: TJvThreadedDatasetFetchMode;
    procedure HandleAfterOpenRefresh;
    procedure SetIntCurrentAction(const Value: TJvThreadedDatasetAction);
    procedure SetIntCurrentFetchDuration(const Value: TDateTime);
    procedure SetIntCurrentOpenDuration(const Value: TDateTime);
    procedure SetIntCurrentOperationStart(const Value: TDateTime);
    procedure SetDatasetFetchAllRecords(const Value: Boolean);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetEnhancedOptions(Value: TJvBaseThreadedDatasetEnhancedOptions);
    procedure SetEofReached(const Value: Boolean);
    procedure SetFetchMode(const Value: TJvThreadedDatasetFetchMode);
    procedure SetIntCurrentOperation(const Value: TJvThreadedDatasetOperation);
    procedure SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
    procedure SynchAfterThreadExecution;
    procedure SynchBeforeThreadExecution;
    procedure SynchAfterOpenFetch;
    procedure SynchContinueFetchMessageDlg;
    procedure SynchErrorMessageDlg;
    procedure SynchOnThreadException;
    property IntCurrentOperationStart: TDateTime read FIntCurrentOperationStart write SetIntCurrentOperationStart;
    property DatasetFetchAllRecords: Boolean read GetDatasetFetchAllRecords write SetDatasetFetchAllRecords;
    property IntCurrentAction: TJvThreadedDatasetAction read GetIntCurrentAction write SetIntCurrentAction;
    property IntCurrentFetchDuration: TDateTime read GetIntCurrentFetchDuration write SetIntCurrentFetchDuration;
    property IntCurrentOpenDuration: TDateTime read GetIntCurrentOpenDuration write SetIntCurrentOpenDuration;
    property IntCurrentOperation: TJvThreadedDatasetOperation read GetIntCurrentOperation write SetIntCurrentOperation;
    property OperationWasHandledInThread: Boolean read FOperationWasHandledInThread
      write FOperationWasHandledInThread;
  protected
    function MaxRowCheckExceeded: Boolean;
    procedure BreakExecution;
    function CreateEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions; virtual;
    procedure DoThreadLast;
    procedure DoThreadOpen;
    procedure DoThreadRefresh;
    function ExecuteThreadIsActive: Boolean;
    procedure ExecuteThreadSynchronize(Method: TThreadMethod);
    procedure HandleAfterOpenRefreshThread;
    procedure HandleBeforeOpenRefresh;
    procedure InitOperation;
    procedure IntAfterThreadExecution(DataSet: TDataSet; Operation: TJvThreadedDatasetOperation);
    procedure IntAfterOpenFetch(DataSet: TDataSet);
    procedure IntBeforeThreadExecution(DataSet: TDataSet; Operation: TJvThreadedDatasetOperation);
    procedure IntOnThreadException(DataSet: TDataSet; Operation: TJvThreadedDatasetOperation; E: Exception);
    procedure IntSynchAfterOpen;
    procedure IntSynchAfterRefresh;
    procedure IntSynchBeforeOpen;
    procedure IntSynchBeforeRefresh;
    procedure MoveToRecordPositionAfterOpen;
    procedure SetError(const Msg: string = ''; Excep: Exception = nil);
    function SupportsBreakExecution: Boolean; virtual;
    procedure ThreadExecute(Sender: TObject; Params: Pointer);
    property ExecuteThread: TJvBaseDatasetThread read FExecuteThread;
    property FetchMode: TJvThreadedDatasetFetchMode read GetFetchMode write SetFetchMode;
    property IntRowCheckEnabled: Boolean read FIntRowCheckEnabled write FIntRowCheckEnabled;
    property IThreadedDatasetInterface: IJvThreadedDatasetInterface read FIThreadedDatasetInterface;
    property ThreadDialog: TJvDatasetThreadDialog read FThreadDialog;
  public
    constructor Create(AOwner: TComponent; ADataset: TDataSet); reintroduce; virtual;
    destructor Destroy; override;
    procedure AfterOpen; virtual;
    procedure AfterScroll; virtual;
    procedure AfterRefresh; virtual;
    procedure BeforeOpen; virtual;
    procedure BeforeRefresh; virtual;
    procedure BringDialogToFront;
    procedure CapitalizeDatasetLabels;
    function CheckContinueRecordFetch: TJvThreadedDatasetContinueCheckResult;
    function GetNextRecord: Boolean;
    procedure InternalLast; virtual;
    procedure InternalRefresh; virtual;
    procedure MoveTo(Position: Integer);
    procedure SetActive(Value: Boolean); virtual;
    function ThreadIsActive: Boolean;
    property CurrentAction: TJvThreadedDatasetAction read GetCurrentAction;
    property CurrentFetchDuration: tDateTime read GetCurrentFetchDuration;
    property CurrentOpenDuration: tDateTime read GetCurrentOpenDuration;
    property CurrentOperation: TJvThreadedDatasetOperation read GetCurrentOperation;
    property CurrentOperationAction: string read GetCurrentOperationAction;
    property CurrentRow: Integer read FCurrentRow;
    property Dataset: TDataSet read FDataset;
    property EofReached: Boolean read FEofReached write SetEofReached;
    property ErrorException: Exception read FErrorException;
    property ErrorMessage: string read FErrorMessage;
  published
    property AfterOpenFetch: TDataSetNotifyEvent read FAfterOpenFetch write FAfterOpenFetch;
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions write SetDialogOptions;
    property EnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions read FEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read FThreadOptions write SetThreadOptions;
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read FAfterThreadExecution write FAfterThreadExecution;
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read FBeforeThreadExecution write FBeforeThreadExecution;
    property OnThreadException: TJvThreadedDatasetThreadExceptionEvent read FOnThreadException write FOnThreadException;
  end;


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: jvcl/run/JvBaseDBThreadedDataset.pas $';
    Revision: '$Revision: 5d8806a57b10804dbc1ec6a5ac352f67d2fcb67d $';
    Date: '$Date: 2011-10-26 23:17:50 +0000 $';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Dialogs,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JvDSADialogs, JvResources;

//=== { TJvDatasetThreadDialog } =============================================

function TJvDatasetThreadDialog.CreateDialogOptions: TJvCustomThreadDialogOptions;
begin
  Result := TJvThreadedDatasetDialogOptions.Create(Self);
end;

function TJvDatasetThreadDialog.CreateThreadDialogForm(ConnectedThread: TJvThread): TJvCustomThreadDialogForm;
var
  ThreadDialogForm: TJvDatasetThreadDialogForm;
begin
  if DialogOptions.ShowDialog then
  begin
    if Assigned(ConnectedThread.Owner) and (ConnectedThread.Owner is TWinControl) then
      ThreadDialogForm := TJvDatasetThreadDialogForm.CreateNewFormStyle(ConnectedThread,
        DialogOptions.FormStyle, TWinControl(ConnectedThread.Owner))
    else
    if Assigned(ConnectedThread.Owner) and Assigned(ConnectedThread.Owner.Owner) and
      (ConnectedThread.Owner.Owner is TWinControl) then
      ThreadDialogForm := TJvDatasetThreadDialogForm.CreateNewFormStyle(ConnectedThread,
        DialogOptions.FormStyle, TWinControl(ConnectedThread.Owner.Owner))
    else
      ThreadDialogForm := TJvDatasetThreadDialogForm.CreateNewFormStyle(ConnectedThread,
        DialogOptions.FormStyle);
    ThreadDialogForm.DialogOptions := DialogOptions;
    Result := ThreadDialogForm;
  end
  else
    Result := nil;
end;

function TJvDatasetThreadDialog.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  Result := TJvThreadedDatasetDialogOptions(inherited DialogOptions);
end;

procedure TJvDatasetThreadDialog.SetDialogOptions(const Value: TJvThreadedDatasetDialogOptions);
begin
  inherited DialogOptions.Assign(Value);
end;

//=== { TJvDatasetThreadDialogForm } =========================================

constructor TJvDatasetThreadDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InternalTimerInterval := 250;
end;

procedure TJvDatasetThreadDialogForm.CreateFormControls;
var
  MainPanel: TWinControl;
  ITmpPanel: IJvDynControlPanel;
  ITmpControl: IJvDynControlCaption;
  ITmpAlign: IJvDynControlAlign;
begin
  Inherited CreateFormControls;
  MainPanel := DynControlEngine.CreatePanelControl(Self, Self, 'MainPanel', '', alClient);
  if not Supports(MainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);

  CreateTextPanel(Self, MainPanel, FTimePanel, FTimeLabel, FTimeStaticText, 'Time');
  if Supports(FTimeLabel, IJvDynControlCaption, ITmpControl) then
    ITmpControl.ControlSetCaption(RsODSOpenFetch);
  FTimeLabel.Top := 0;
  CreateTextPanel(Self, MainPanel, FRowsPanel, FRowsLabel, FRowsStaticText, 'Rows');
  if Supports(FRowsLabel, IJvDynControlCaption, ITmpControl) then
    ITmpControl.ControlSetCaption(RsODSCurrentRecord);
  FRowsPanel.Top := FTimeLabel.Top + FTimeLabel.Height + 1;

  FProgressbarPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ProgressbarPanel', '', alTop);
  if not Supports(FProgressbarPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, 2);
  FProgressbar := DynControlEngine.CreateProgressbarControl(Self, FProgressbarPanel, 'Progressbar');
  Supports(FProgressbar, IJvDynControlProgressbar, IProgressBarControl);
  FProgressbarPanel.Height := FRowsStaticText.Height + 4;
  if Supports(FProgressbar, IJvDynControlAlign, ITmpAlign) then
    ITmpAlign.ControlSetAlign(alClient);
  FProgressbarPanel.Top := FRowsPanel.Top + FRowsPanel.Height + 1;

  FCancelButtonPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ButtonPanel', '', alTop);
  FCancelBtn := DynControlEngine.CreateButton(Self, FCancelButtonPanel,
    'CancelBtn', RsButtonCancelCaption, '', DefaultCancelBtnClick, True, True);
  FCancelBtn.Anchors := [akTop];
  FCancelBtn.Top := 2;
  FCancelButtonPanel.Top := FProgressbarPanel.Top + FProgressbarPanel.Height + 1;
  FCancelButtonPanel.Height := FCancelBtn.Height + 3;

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

procedure TJvDatasetThreadDialogForm.CreateTextPanel(AOwner: TComponent; AParent: TWinControl; var Panel: TWinControl;
    var LabelCtrl: TControl; var StaticText: TWinControl; const BaseName: string);
var
  ITmpPanel: IJvDynControlPanel;
  ITmpAutoSize: IJvDynControlAutoSize;
  ITmpAlignment: IJvDynControlAlignment;
begin
  Panel := DynControlEngine.CreatePanelControl(AOwner, AParent, BaseName + 'Panel', '', alTop);
  if not Supports(Panel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, 3);
  LabelCtrl := DynControlEngine.CreateLabelControl(AOwner, Panel, BaseName + 'Label', '', nil);
  LabelCtrl.Top := 1;
  LabelCtrl.Left := 1;
  LabelCtrl.Width := 90;
  StaticText := DynControlEngine.CreateStaticTextControl(AOwner, Panel, BaseName + 'StaticText', '');
  if Supports(StaticText, IJvDynControlAutoSize, ITmpAutoSize) then
    ITmpAutoSize.ControlSetAutoSize(False);
  if Supports(StaticText, IJvDynControlAlignment, ITmpAlignment) then
    ITmpAlignment.ControlSetAlignment(taCenter);
  StaticText.Top := 1;
  StaticText.Left := 95;
  StaticText.Height := 18;
  Panel.Height := StaticText.Height + 6;
end;

procedure TJvDatasetThreadDialogForm.FillDialogData;
var
  ITmpControl: IJvDynControlCaption;
begin
  if Assigned(IProgressBarControl) then
    IProgressBarControl.ControlSetMarquee(True);
  if Assigned(ConnectedDatasetHandler) then
  begin
    if DialogOptions.Caption <> '' then
      Caption := DialogOptions.Caption +' - '+ConnectedDatasetHandler.CurrentOperationAction
    else
      Caption := ConnectedDatasetHandler.CurrentOperationAction ;
    if Supports(FRowsStaticText, IJvDynControlCaption, ITmpControl) then
      ITmpControl.ControlSetCaption(IntToStr(ConnectedDatasetHandler.CurrentRow));
    if Supports(FTimeStaticText, IJvDynControlCaption, ITmpControl) then
      ITmpControl.ControlSetCaption(
        FormatDateTime('hh:nn:ss', ConnectedDatasetHandler.CurrentOpenDuration) + ' / ' +
          FormatDateTime('hh:nn:ss', ConnectedDatasetHandler.CurrentFetchDuration));
  end
  else
  begin
    Caption := DialogOptions.Caption;
    if Supports(FRowsStaticText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(IntToStr(0));
    if Supports(FTimeStaticText, IJvDynControl, ITmpControl) then
      ITmpControl.ControlSetCaption(
        FormatDateTime('hh:nn:ss', 0) + ' / ' +
        FormatDateTime('hh:nn:ss', 0));
  end;
  FRowsStaticText.Width:= FRowsPanel.Width - FRowsLabel.Width;
  FTimeStaticText.Width:= FTimePanel.Width - FTimeLabel.Width;
end;

procedure TJvDatasetThreadDialogForm.FreeFormControls;
begin
  if Assigned(IProgressBarControl) then
    IProgressBarControl.ControlSetMarquee(False);// To deactivate the toolbar marquee in rare circumstances
  IProgressBarControl := nil;
  inherited;
end;

function TJvDatasetThreadDialogForm.GetConnectedDataset: TDataSet;
begin
  if Assigned(ConnectedDatasetHandler) then
    Result := ConnectedDatasetHandler.Dataset
  else
    Result := nil;
end;

function TJvDatasetThreadDialogForm.GetConnectedDatasetHandler: TJvBaseDatasetThreadHandler;
begin
  if Assigned(ConnectedDataComponent) and (ConnectedDataComponent is TJvBaseDatasetThreadHandler) then
    Result := TJvBaseDatasetThreadHandler(ConnectedDataComponent)
  else
    Result := nil;
end;

function TJvDatasetThreadDialogForm.GetDialogOptions: TJvThreadedDatasetDialogOptions;
begin
  Result := FDialogOptions;
end;

procedure TJvDatasetThreadDialogForm.InitializeFormContents;
begin
  if Assigned(ConnectedDatasetHandler) then
    ConnectedDatasetHandler.InitOperation;
end;

procedure TJvDatasetThreadDialogForm.SetDialogOptions(const Value: TJvThreadedDatasetDialogOptions);
begin
  inherited DialogOptions := Value;
  FDialogOptions := Value;
  if Assigned(FDialogOptions) then
    DynControlEngine := DialogOptions.DynControlEngine
  else
    DynControlEngine := nil;
end;

procedure TJvDatasetThreadDialogForm.TransferDialogOptions;
var
  H: Integer;
begin
  Inherited;
  ClientWidth := 220;
  FCancelButtonPanel.Visible := DialogOptions.ShowCancelButton;
  FCancelBtn.Enabled := DialogOptions.EnableCancelButton;
  FCancelBtn.Left := Round((FCancelButtonPanel.Width - FCancelBtn.Width) / 2);
  FRowsPanel.Visible := DialogOptions.ShowRowsLabel;
  FTimePanel.Visible := DialogOptions.ShowTimeLabel;
  FProgressbarPanel.Visible := DialogOptions.ShowProgressBar;
  H := 10;
  if FRowsPanel.Visible then
    H := H + FRowsPanel.Height;
  if FTimePanel.Visible then
    H := H + FTimePanel.Height;
  if FProgressbarPanel.Visible then
    H := H + FProgressbarPanel.Height;
  if FCancelButtonPanel.Visible then
    H := H + FCancelButtonPanel.Height;
  ClientHeight := H;
end;

procedure TJvDatasetThreadDialogForm.UpdateFormContents;
begin
  inherited UpdateFormContents;
  FillDialogData;
end;

//=== { TJvThreadedDatasetDialogOptions } ====================================

constructor TJvThreadedDatasetDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create(AOwner);
  FEnableCancelButton := True;
  FShowCancelButton := True;
  FShowRowsLabel := True;
  FShowTimeLabel := True;
  FShowProgressBar := true;
end;

destructor TJvThreadedDatasetDialogOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TJvThreadedDatasetDialogOptions.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TJvThreadedDatasetDialogOptions.SetDynControlEngine(const Value: TJvDynControlEngine);
begin
  FDynControlEngine := Value;
end;

procedure TJvThreadedDatasetDialogOptions.SetEnableCancelButton(const Value: Boolean);
begin
  FEnableCancelButton := Value;
end;

procedure TJvThreadedDatasetDialogOptions.SetFormStyle(const Value: TFormStyle);
begin
  FFormStyle := Value;
end;

procedure TJvThreadedDatasetDialogOptions.SetShowCancelButton(const Value: Boolean);
begin
  FShowCancelButton := Value;
end;

procedure TJvThreadedDatasetDialogOptions.SetShowProgressBar(const Value: Boolean);
begin
  FShowProgressBar := Value;
end;

procedure TJvThreadedDatasetDialogOptions.SetShowRowsLabel(const Value: Boolean);
begin
  FShowRowsLabel := Value;
end;

procedure TJvThreadedDatasetDialogOptions.SetShowTimeLabel(const Value: Boolean);
begin
  FShowTimeLabel := Value;
end;

//=== { TJvBaseDatasetThread } ===============================================

procedure TJvBaseDatasetThread.InternalAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm);
begin
  DialogForm.ConnectedDataComponent := ConnectedDatasetThreadHandler;
end;

procedure TJvBaseDatasetThread.CancelExecute;
begin
  if ConnectedDatasetThreadHandler.SupportsBreakExecution then
    ConnectedDatasetThreadHandler.BreakExecution
  else
    inherited CancelExecute;
end;

procedure TJvBaseDatasetThread.SetConnectedDataset(const Value: TDataSet);
begin
  FConnectedDataset := Value;
  if Assigned(Value) then
    if not Supports(Value, IJvThreadedDatasetInterface, FConnectedDatasetInterface) then
      raise EIntfCastError.CreateRes(@RsEIntfCastError)
    else
  else
    FConnectedDatasetInterface := nil;

end;

procedure TJvBaseDatasetThread.SetConnectedDatasetThreadHandler(const Value: TJvBaseDatasetThreadHandler);
begin
  FConnectedDatasetThreadHandler := Value;
  if Assigned(Value) then
    SetConnectedDataset (ConnectedDatasetThreadHandler.Dataset)
  else
    SetConnectedDataset (nil);
end;

//=== { TJvThreadedDatasetThreadOptions } ====================================

constructor TJvThreadedDatasetThreadOptions.Create;
begin
  inherited Create;
  FLastInThread := False;
  FOpenInThread := False;
  FPriority := tpIdle;
  FRefreshInThread := False;
  FShowExceptionMessage := True;
end;

//=== { TJvThreadedDatasetEnhancedOptions } ==================================

constructor TJvBaseThreadedDatasetEnhancedOptions.Create;
begin
  inherited Create;
  FRefreshAsOpenClose := False;
  FRefreshLastPosition := False;
  FCapitalizeLabelOptions := TJvThreadedDatasetCapitalizeLabelOptions.Create;
  FAllowedContinueRecordFetchOptions := CreateAllowedContinueRecordFetchOptions;
  FFetchRowsCheck := 2000;
  FFetchRowsFirst := 1000;
end;

destructor TJvBaseThreadedDatasetEnhancedOptions.Destroy;
begin
  FreeAndNil(FAllowedContinueRecordFetchOptions);
  FreeAndNil(FCapitalizeLabelOptions);
  inherited Destroy;
end;

function TJvBaseThreadedDatasetEnhancedOptions.CreateAllowedContinueRecordFetchOptions:
    TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions;
begin
  Result := TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions.Create;
end;

procedure TJvBaseThreadedDatasetEnhancedOptions.SetCapitalizeLabelOptions(const Value:
    TJvThreadedDatasetCapitalizeLabelOptions);
begin
  FCapitalizeLabelOptions.Assign(Value);
end;

procedure TJvBaseThreadedDatasetEnhancedOptions.SetFetchRowsCheck(const Value: Integer);
begin
  FFetchRowsCheck := Value;
end;

procedure TJvBaseThreadedDatasetEnhancedOptions.SetFetchRowsFirst(const Value: Integer);
begin
  FFetchRowsFirst := Value;
end;

procedure TJvBaseThreadedDatasetEnhancedOptions.SetRefreshAsOpenClose(Value: Boolean);
begin
  FRefreshAsOpenClose := Value;
end;

procedure TJvBaseThreadedDatasetEnhancedOptions.SetRefreshLastPosition(const Value: Boolean);
begin
  FRefreshLastPosition := Value;
end;

//=== { TJvThreadedDatasetCapitalizeLabelOptions } ===========================

constructor TJvThreadedDatasetCapitalizeLabelOptions.Create;
begin
  inherited Create;
  FAutoExecuteAfterOpen := False;
  FTrimToFirstBlank := False;
end;

//=== { TJvBaseDatasetThreadHandler } ========================================

constructor TJvBaseDatasetThreadHandler.Create(AOwner: TComponent; ADataset: TDataSet);
begin
  inherited Create (AOwner);
  FDataset := ADataset;
  if not Supports(ADataset, IJvThreadedDatasetInterface, FIThreadedDatasetInterface) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);

  FThreadOptions := TJvThreadedDatasetThreadOptions.Create;
  FExecuteThread := TJvBaseDatasetThread.Create(Self);
  FThreadDialog := TJvDatasetThreadDialog.Create(Self);
  FExecuteThread.Exclusive := True;
  FExecuteThread.OnExecute := ThreadExecute;
  FExecuteThread.ConnectedDatasetThreadHandler := Self;
  FExecuteThread.ThreadDialog := ThreadDialog;
  FEnhancedOptions := CreateEnhancedOptions;
  IntCurrentOperation := tdoNothing;
  MessageDlgIsActive := False;
end;

destructor TJvBaseDatasetThreadHandler.Destroy;
begin
  FreeAndNil(FEnhancedOptions);
  FreeAndNil(FThreadDialog);
  FreeAndNil(FExecuteThread);
  FreeAndNil(FThreadOptions);
  inherited Destroy;
end;

procedure TJvBaseDatasetThreadHandler.AfterOpen;
begin
  ExecuteThreadSynchronize(IntSynchAfterOpen);
  if not ExecuteThreadIsActive and not OperationWasHandledInThread and (IntCurrentOperation <> tdoRefresh) then
    HandleAfterOpenRefresh;
end;

procedure TJvBaseDatasetThreadHandler.AfterScroll;
begin
  IThreadedDatasetInterface.DoInheritedAfterScroll;
  EofReached := EofReached or Dataset.Eof;
end;

procedure TJvBaseDatasetThreadHandler.AfterRefresh;
begin
  if not ExecuteThreadIsActive and not OperationWasHandledInThread then
    HandleAfterOpenRefresh;
  ExecuteThreadSynchronize(IntSynchAfterRefresh);
end;

procedure TJvBaseDatasetThreadHandler.BeforeOpen;
begin
  if (IntCurrentOperation <> tdoRefresh) then
  begin
    FAfterOpenRecordPosition := -1;
    HandleBeforeOpenRefresh;
  end;
  ExecuteThreadSynchronize(IntSynchBeforeOpen);
end;

procedure TJvBaseDatasetThreadHandler.BeforeRefresh;
begin
  if EnhancedOptions.RefreshLastPosition then
    FAfterOpenRecordPosition := Dataset.RecNo
  else
    FAfterOpenRecordPosition := -1;
  HandleBeforeOpenRefresh;
  ExecuteThreadSynchronize(IntSynchBeforeRefresh);
end;

procedure TJvBaseDatasetThreadHandler.BreakExecution;
begin
  IntCurrentAction := tdaCancel;
  if (FetchMode = tdfmFetch) and
   (EnhancedOptions.AllowedContinueRecordFetchOptions.Pause or
    EnhancedOptions.AllowedContinueRecordFetchOptions.Cancel) then
    if EnhancedOptions.AllowedContinueRecordFetchOptions.Pause then
      FetchMode := tdfmBreak
    else
      FetchMode := tdfmStop
  else
  begin
    IThreadedDatasetInterface.BreakExecution;
    FetchMode := tdfmStop;
  end;
  IntRowCheckEnabled := False;
end;

procedure TJvBaseDatasetThreadHandler.BringDialogToFront;
begin
  if Assigned(ExecuteThread) and ExecuteThread.OneThreadIsRunning
    and Assigned(ExecuteThread.ThreadDialogForm)
    and not MessageDlgIsActive then
    ExecuteThread.ThreadDialogForm.BringToFront;
end;

procedure TJvBaseDatasetThreadHandler.CapitalizeDatasetLabels;
var
  I, J: Integer;
  S: string;
  Upper: Boolean;
begin
  if Dataset.Active then
    for I := 0 to Dataset.FieldCount - 1 do
    begin
      S := LowerCase(Dataset.Fields[I].DisplayLabel);
      Upper := True;
      for J := 1 to Length(S) do
        if CharInSet(S[J], ['_', '$', ' ']) then
        begin
          Upper := True;
          S[J] := ' ';
        end
        else
        if Upper then
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
      Dataset.Fields[I].DisplayLabel := S;
    end;
end;

function TJvBaseDatasetThreadHandler.CheckContinueRecordFetch: TJvThreadedDatasetContinueCheckResult;
begin
  Result := tdccrContinue;
  FCurrentRow := Dataset.RecordCount;
  if MaxRowCheckExceeded or ((CurrentRow > fMaxRowChecked) and (fMaxRowChecked >0))then
  begin
    Result := tdccrPause;
    FetchMode := tdfmFetch;
    Exit;
  end;
  case FetchMode of
    tdfmBreak:
      begin
        Result := tdccrPause;
        FetchMode := tdfmFetch;
        Exit;
      end;
    tdfmStop:
      begin
        Result := tdccrStop;
        Exit;
      end;
  end;
  if IntRowCheckEnabled then
  begin
    if (fLastRowChecked = 0) and
       (CurrentRow >= EnhancedOptions.FetchRowsFirst) and
       (CurrentRow < FLastRowChecked + EnhancedOptions.FetchRowsCheck)then
      begin
        Result := tdccrContinue;
        if CurrentRow > 0  then
          Exit;
      end;
    if (EnhancedOptions.FetchRowsCheck > 0) and
      (CurrentRow >= FLastRowChecked + EnhancedOptions.FetchRowsCheck) then
      begin
        IntCurrentFetchDuration := IntCurrentFetchDuration + Now - IntCurrentOperationStart;
        IntCurrentAction := tdaNothing;
        FLastRowChecked := CurrentRow;
        FSynchMessageDlgMsg := Format(RsODSRowsFetchedContinue, [CurrentRow]);
        ExecuteThreadSynchronize(SynchContinueFetchMessageDlg);
        case FSynchMessageDlgBtn of
          mrYes:
            Result := tdccrContinue;
          mrAll:
            begin
              Result := tdccrContinue;
              IntRowCheckEnabled := False;
            end;
          mrAbort:
            Result := tdccrCancel;
          mrCancel:
            Result := tdccrPause;
          mrNo:
            Result := tdccrStop;
        else
          Result := tdccrStop;
        end;
        IntCurrentAction := tdaFetch;
        FetchMode := tdfmFetch;
        if Result = tdccrStop then
          fMaxRowChecked := CurrentRow;

        IntCurrentOperationStart := Now;
      end;
  end;
end;

function TJvBaseDatasetThreadHandler.CreateEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions;
begin
  Result := TJvBaseThreadedDatasetEnhancedOptions.Create;
end;

procedure TJvBaseDatasetThreadHandler.DoThreadLast;
begin
  IThreadedDatasetInterface.DoInheritedInternalLast;
end;

procedure TJvBaseDatasetThreadHandler.DoThreadOpen;
begin
  try
    IThreadedDatasetInterface.DoInheritedSetActive(True);
  finally
    HandleAfterOpenRefreshThread;
  end;
end;

procedure TJvBaseDatasetThreadHandler.DoThreadRefresh;
begin
  try
    if not EnhancedOptions.RefreshAsOpenClose then
    begin
      IThreadedDatasetInterface.DoInheritedInternalRefresh;
    end
    else
    begin
      if Dataset.Active then
        ExecuteThreadSynchronize(Dataset.Close);
      IThreadedDatasetInterface.DoInheritedSetActive(True);
    end;
  finally
    HandleAfterOpenRefreshThread;
  end;
end;

function TJvBaseDatasetThreadHandler.ExecuteThreadIsActive: Boolean;
begin
  Result := Not ExecuteThread.Terminated;
end;

procedure TJvBaseDatasetThreadHandler.ExecuteThreadSynchronize(Method: TThreadMethod);
begin
  ExecuteThread.Synchronize(Method);
end;

function TJvBaseDatasetThreadHandler.GetCurrentAction: TJvThreadedDatasetAction;
begin
  Result := GetIntCurrentAction;
end;

function TJvBaseDatasetThreadHandler.GetCurrentFetchDuration: tDateTime;
begin
  Result := IntCurrentFetchDuration;
end;

function TJvBaseDatasetThreadHandler.GetCurrentOpenDuration: tDateTime;
begin
  Result := IntCurrentOpenDuration;
end;

function TJvBaseDatasetThreadHandler.GetIntCurrentAction: TJvThreadedDatasetAction;
begin
  Result := FIntCurrentAction;
end;

function TJvBaseDatasetThreadHandler.GetIntCurrentFetchDuration: TDateTime;
begin
  case IntCurrentAction of
    tdaOpen:
      Result := 0;
    tdaNothing, tdaCancel:
      Result := FIntCurrentFetchDuration;
    tdaFetch:
      Result := FIntCurrentFetchDuration + (Now - IntCurrentOperationStart);
  else
    Result := FIntCurrentFetchDuration;
  end;
end;

function TJvBaseDatasetThreadHandler.GetIntCurrentOpenDuration: TDateTime;
begin
  if IntCurrentAction = tdaOpen then
    Result := Now - IntCurrentOperationStart
  else
    Result := FIntCurrentOpenDuration;
end;

function TJvBaseDatasetThreadHandler.GetCurrentOperation:
    TJvThreadedDatasetOperation;
begin
  Result := IntCurrentOperation;
end;

function TJvBaseDatasetThreadHandler.GetIntCurrentOperation: TJvThreadedDatasetOperation;
begin
  Result := FIntCurrentOperation;
end;

function TJvBaseDatasetThreadHandler.GetCurrentOperationAction: string;
begin
  case IntCurrentOperation of
    tdoOpen:
      case IntCurrentAction of
        tdaOpen:
          Result := RsODSOpenQuery;
        tdaFetch:
          Result := RsODSOpenQueryFetchRecords;
        tdaCancel :
          Result := RsODSOpenQueryCancel;
      end;
    tdoRefresh:
      case IntCurrentAction of
        tdaOpen:
          Result := RsODSRefreshQuery;
        tdaFetch:
          Result := RsODSRefreshQueryFetchRecords;
        tdaCancel :
          Result := RsODSRefreshQueryCancel;
      end;
    tdoFetch:
      case IntCurrentAction of
        tdaFetch:
          Result := RsODSFetchRecords;
        tdaCancel :
          Result := RsODSFetchRecordsCancel;
      end;
    tdoLast:
      Result := RsODSGotoLastFetchRecords;
  end;
end;

function TJvBaseDatasetThreadHandler.GetDatasetFetchAllRecords: Boolean;
begin
  if Assigned(IThreadedDatasetInterface) then
    Result := IThreadedDatasetInterface.DatasetFetchAllRecords
  else
    Result := False;
end;

function TJvBaseDatasetThreadHandler.GetDialogOptions:
    TJvThreadedDatasetDialogOptions;
begin
  Result := ThreadDialog.DialogOptions;
end;

function TJvBaseDatasetThreadHandler.GetFetchMode: TJvThreadedDatasetFetchMode;
begin
  Result := FFetchMode;
end;

function TJvBaseDatasetThreadHandler.GetNextRecord: Boolean;
begin
  if MaxRowCheckExceeded then
    Result := False
  else
    Result := IThreadedDatasetInterface.DoGetInheritedNextRecord;
end;

procedure TJvBaseDatasetThreadHandler.HandleAfterOpenRefresh;
begin
  try
    IntCurrentOpenDuration := Now - IntCurrentOperationStart;
    IntCurrentOperationStart := Now;
    DatasetFetchAllRecords := FIntDatasetFetchAllRecords;
    if (IntCurrentAction <> tdaCancel) and not (FetchMode in [tdfmBreak, tdfmStop]) then
    begin
      IntCurrentAction := tdaFetch;
      FetchMode := tdfmFetch;
      if Dataset.Active then
      begin
        Dataset.First;
        if DatasetFetchAllRecords then
          IThreadedDatasetInterface.DoInheritedInternalLast
        else
          if (EnhancedOptions.FetchRowsFirst > Dataset.RecordCount) or
            (FAfterOpenRecordPosition > Dataset.RecordCount) then
          begin
            if FAfterOpenRecordPosition > EnhancedOptions.FetchRowsFirst then
              Dataset.MoveBy(FAfterOpenRecordPosition - 1)
            else
              Dataset.MoveBy(EnhancedOptions.FetchRowsFirst - 1);
            EofReached := EofReached or Dataset.Eof;
          end;
      end;
    end;
    Dataset.Filtered := FIntDatasetWasFiltered;
  finally
    ExecuteThreadSynchronize(Dataset.EnableControls);
    IntCurrentAction := tdaNothing;
  end;
  if Dataset.Active and (IntCurrentAction <> tdaCancel) then
  begin
    ExecuteThreadSynchronize(MoveToRecordPositionAfterOpen);
    ExecuteThreadSynchronize(SynchAfterOpenFetch);
  end;
end;

procedure TJvBaseDatasetThreadHandler.HandleAfterOpenRefreshThread;
begin
  HandleAfterOpenRefresh;
end;

procedure TJvBaseDatasetThreadHandler.HandleBeforeOpenRefresh;
begin
  FEofReached := False;
  OperationWasHandledInThread := False;
  ExecuteThreadSynchronize(Dataset.DisableControls);
  IntCurrentOpenDuration := 0;
  IntCurrentFetchDuration := 0;
  IntRowCheckEnabled := True;
  FCurrentRow := 0;
  IntCurrentOperationStart := Now;
  IntCurrentAction := tdaOpen;
  FetchMode := tdfmNothing;
  FIntDatasetFetchAllRecords := DatasetFetchAllRecords;
  FLastRowChecked := 0;
  FMaxRowChecked := 0;
  FIntDatasetWasFiltered := Dataset.Filtered;
  Dataset.Filtered := False;
  DatasetFetchAllRecords := False;
end;

procedure TJvBaseDatasetThreadHandler.InitOperation;
begin
  IntCurrentOperationStart := Now;
end;

procedure TJvBaseDatasetThreadHandler.IntAfterThreadExecution(DataSet: TDataSet;
  Operation: TJvThreadedDatasetOperation);
begin
  if Assigned(FAfterThreadExecution) then
    FAfterThreadExecution(DataSet, Operation);
end;

procedure TJvBaseDatasetThreadHandler.IntAfterOpenFetch(DataSet: TDataSet);
begin
  if Assigned(FAfterOpenFetch) then
    FAfterOpenFetch(DataSet);
end;

procedure TJvBaseDatasetThreadHandler.IntBeforeThreadExecution(DataSet: TDataSet;
  Operation: TJvThreadedDatasetOperation);
begin
  if Assigned(FBeforeThreadExecution) then
    FBeforeThreadExecution(DataSet, Operation);
end;

procedure TJvBaseDatasetThreadHandler.IntOnThreadException(DataSet: TDataSet; Operation: TJvThreadedDatasetOperation;
    E: Exception);
begin
  if Assigned(FOnThreadException) then
    FOnThreadException(DataSet, Operation, E);
end;

procedure TJvBaseDatasetThreadHandler.InternalLast;
var
  ShowModal: Boolean;
begin
  if FIntCurrentOperation <> tdoNothing then
    Exit;
  IntCurrentOperation := tdoLast;
  if not ThreadOptions.LastInThread or ThreadIsActive or (csDesigning in ComponentState) then
  begin
    IThreadedDatasetInterface.DoInheritedInternalLast;
    IntCurrentOperation := tdoNothing;
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

procedure TJvBaseDatasetThreadHandler.InternalRefresh;
begin
  if FIntCurrentOperation <> tdoNothing then
    Exit;
  IntCurrentOperation := tdoRefresh;
  if not ThreadOptions.RefreshInThread or not IThreadedDatasetInterface.IsThreadAllowed or
    ThreadIsActive or (csDesigning in ComponentState) then
  begin
    if not EnhancedOptions.RefreshAsOpenClose then
      IThreadedDatasetInterface.DoInheritedInternalRefresh
    else
    begin
      if Dataset.Active then
        SetActive(False);
      SetActive(True);
    end;
    IntCurrentOperation := tdoNothing;
  end
  else
    ExecuteThread.ExecuteWithDialog(nil);
end;

procedure TJvBaseDatasetThreadHandler.IntSynchAfterOpen;
begin
  if EnhancedOptions.CapitalizeLabelOptions.AutoExecuteAfterOpen then
    CapitalizeDatasetLabels;
  IThreadedDatasetInterface.DoInheritedAfterOpen;
  if ExecuteThreadIsActive then
  begin
    // Added because in the afteropen event the filtered could be activated, and it should be deactivated for the dialog
    FIntDatasetWasFiltered := Dataset.Filtered or FIntDatasetWasFiltered;
    Dataset.Filtered := False;
  end;
end;

procedure TJvBaseDatasetThreadHandler.IntSynchAfterRefresh;
begin
  IThreadedDatasetInterface.DoInheritedAfterRefresh;
end;

procedure TJvBaseDatasetThreadHandler.IntSynchBeforeOpen;
begin
  IThreadedDatasetInterface.DoInheritedBeforeOpen;
end;

procedure TJvBaseDatasetThreadHandler.IntSynchBeforeRefresh;
begin
  IThreadedDatasetInterface.DoInheritedBeforeRefresh;
end;

function TJvBaseDatasetThreadHandler.MaxRowCheckExceeded: Boolean;
begin
  Result :=(fMaxRowChecked > 0) and ((Dataset.RecNo >= fMaxRowChecked));
end;

procedure TJvBaseDatasetThreadHandler.MoveTo(Position: Integer);
begin
  Dataset.MoveBy(Position - Dataset.RecNo);
  EofReached := EofReached or Dataset.Eof;
end;

procedure TJvBaseDatasetThreadHandler.SetActive(Value: Boolean);
begin
  if not Value then
  begin
    IntCurrentOpenDuration := 0;
    IntCurrentFetchDuration := 0;
    IThreadedDatasetInterface.DoInheritedSetActive(Value);
  end
  else
  begin
    if (IntCurrentOperation <> tdoNothing) and
       (IntCurrentOperation <> tdoRefresh) then
      Exit;
    if IntCurrentOperation <> tdoRefresh then
      IntCurrentOperation := tdoOpen;
    if not (   (ThreadOptions.OpenInThread and (IntCurrentOperation = tdoOpen))
            or (ThreadOptions.RefreshInThread and (IntCurrentOperation = tdoRefresh))
           )
      or ThreadIsActive or (csDesigning in ComponentState) then
    begin
      try
        IThreadedDatasetInterface.DoInheritedSetActive(Value);
      finally
        if IntCurrentOperation <> tdoRefresh then
          IntCurrentOperation := tdoNothing;
      end;
    end
    else
      ExecuteThread.ExecuteWithDialog(nil);
  end;
end;

procedure TJvBaseDatasetThreadHandler.SetIntCurrentAction(const Value: TJvThreadedDatasetAction);
begin
  FIntCurrentAction := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetIntCurrentFetchDuration(const Value: TDateTime);
begin
  FIntCurrentFetchDuration := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetIntCurrentOpenDuration(const Value: TDateTime);
begin
  FIntCurrentOpenDuration := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetIntCurrentOperationStart(const Value: TDateTime);
begin
  FIntCurrentOperationStart := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetDatasetFetchAllRecords(const Value: Boolean);
begin
   if Assigned(IThreadedDatasetInterface) then
    IThreadedDatasetInterface.DatasetFetchAllRecords := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
begin
  ThreadDialog.DialogOptions.Assign(Value);
end;

procedure TJvBaseDatasetThreadHandler.SetEnhancedOptions(Value: TJvBaseThreadedDatasetEnhancedOptions);
begin
  FEnhancedOptions.Assign(Value);
end;

procedure TJvBaseDatasetThreadHandler.SetError(const Msg: string = ''; Excep:
    Exception = nil);
begin
  FErrorMessage := Msg;
  FErrorException := Excep;
end;

procedure TJvBaseDatasetThreadHandler.SetFetchMode(const Value: TJvThreadedDatasetFetchMode);
begin
  FFetchMode := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetIntCurrentOperation(const Value: TJvThreadedDatasetOperation);
begin
  FIntCurrentOperation := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
begin
  FThreadOptions.Assign(Value);
end;

function TJvBaseDatasetThreadHandler.SupportsBreakExecution: Boolean;
begin
  Result := True;
end;

procedure TJvBaseDatasetThreadHandler.SynchAfterThreadExecution;
begin
  IntAfterThreadExecution(Dataset, IntCurrentOperation);
end;

procedure TJvBaseDatasetThreadHandler.SynchBeforeThreadExecution;
begin
  IntBeforeThreadExecution(Dataset, IntCurrentOperation);
end;

procedure TJvBaseDatasetThreadHandler.SynchContinueFetchMessageDlg;
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
  AddButton(RsODSContinueYes, Integer(mrYes));
  if EnhancedOptions.AllowedContinueRecordFetchOptions.Pause then
    AddButton(RsODSContinuePause, Integer(mrCancel));
  AddButton(RsODSContinueNo, Integer(mrNo));
  if EnhancedOptions.AllowedContinueRecordFetchOptions.All then
    AddButton(RsODSContinueAll, Integer(mrAll));
  if EnhancedOptions.AllowedContinueRecordFetchOptions.Cancel then
    AddButton(RsODSContinueClose, Integer(mrAbort));
  MessageDlgIsActive := True;
  try
    FSynchMessageDlgBtn := JvDSADialogs.MessageDlgEx(FSynchMessageDlgMsg,
        mtConfirmation, Buttons, Results, 0, dckActiveForm, 0,
        0, 1, -1, DialogOptions.DynControlEngine);
  finally
    MessageDlgIsActive := False;
  end;
end;

procedure TJvBaseDatasetThreadHandler.SynchErrorMessageDlg;
begin
  FSynchMessageDlgBtn := JvDSADialogs.MessageDlg(FSynchMessageDlgMsg,
    mtError, [mbOK], 0, dckScreen, 0,
    mbDefault, mbDefault, mbHelp, DialogOptions.DynControlEngine);
end;

procedure TJvBaseDatasetThreadHandler.SynchOnThreadException;
begin
  IntOnThreadException(Dataset, IntCurrentOperation, IntThreadException);
end;

procedure TJvBaseDatasetThreadHandler.ThreadExecute(Sender: TObject; Params: Pointer);
begin
  OperationWasHandledInThread := True;
  ExecuteThread.ThreadDialogAllowed := True;
  try
    SetError('', nil);
    ExecuteThreadSynchronize(SynchBeforeThreadExecution);
    try
      case FIntCurrentOperation of
        tdoOpen:
          DoThreadOpen;
        tdoRefresh:
          DoThreadRefresh;
        tdoLast:
          DoThreadLast;
      end;
    except
      on E: Exception do
      begin
        SetError(E.Message, E);
        ExecuteThread.ThreadDialogAllowed := False;
        if Assigned(FOnThreadException) then
        begin
          IntThreadException := e;
          ExecuteThreadSynchronize(SynchOnThreadException);
        end;
        if ThreadOptions.ShowExceptionMessage then
        begin
          FSynchMessageDlgMsg := E.Message;
          ExecuteThreadSynchronize(SynchErrorMessageDlg);
        end;
      end;
    end;
  finally
    ExecuteThreadSynchronize(SynchAfterThreadExecution);
    IntCurrentOperation := tdoNothing;
    ExecuteThread.ThreadDialogAllowed := False;
  end;
end;

function TJvBaseDatasetThreadHandler.ThreadIsActive: Boolean;
begin
  Result := not ExecuteThread.Terminated;
end;

procedure TJvBaseDatasetThreadHandler.MoveToRecordPositionAfterOpen;
begin
  if FAfterOpenRecordPosition > 0 then
    MoveTo(FAfterOpenRecordPosition)
  else
    Dataset.First;
end;

procedure TJvBaseDatasetThreadHandler.SetEofReached(const Value: Boolean);
begin
  if FEofReached <> Value then
    FEofReached := Value;
end;

procedure TJvBaseDatasetThreadHandler.SynchAfterOpenFetch;
begin
  IntAfterOpenFetch(Dataset);
end;

//=== { TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions } ============

constructor TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions.Create;
begin
  inherited Create;
  FAll := False;
  FCancel := False;
  FPause := False;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
end.
