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
located at http://jvcl.sourceforge.net

Description:
  Oracle Dataset with Threaded Functions

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBaseDBThreadedDataset;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, StdCtrls, ExtCtrls, Forms, Controls,
  DB,
  JvThread, JvThreadDialog, JvDynControlEngine;

type
  TJvThreadedDatasetOperation = (tdoOpen, tdoFetch, tdoLast, tdoRefresh, tdoNothing);
  TJvThreadedDatasetAction = (tdaOpen, tdaFetch, tdaNothing, tdaCancel);

  TJvThreadedDatasetFetchMode = (tdfmFetch, tdfmBreak, tdfmStop, tdfmNothing);

  TJvThreadedDatasetContinueCheckResult = (tdccrContinue, tdccrPause, tdccrStop, tdccrAll, tdccrCancel);

  TJvThreadedDatasetContinueAllowButton = (tdcaPause, tdcaStop, tdcaAll);
  TJvThreadedDatasetContinueAllowButtons = set of TJvThreadedDatasetContinueAllowButton;

  TJvBaseDatasetThreadHandler = class;

  TJvThreadedDatasetThreadEvent = procedure(DataSet: TDataset;
    Operation: TJvThreadedDatasetOperation) of object;

  IJvThreadedDatasetInterface = interface
    ['{220CC94D-AA41-4195-B90C-ECA24BAD3CDB}']
    procedure BreakExecution;
    function CurrentFetchDuration: TDateTime;
    function CurrentOpenDuration: TDateTime;
    procedure doInheritedInternalLast;
    procedure doInheritedInternalRefresh;
    procedure doInheritedSetActive(Active: Boolean);
    procedure doInternalOpen;
    function IsThreadAllowed: Boolean;
    function ThreadIsActive: Boolean;
    procedure DoInheritedBeforeOpen;
    procedure DoInheritedAfterOpen;
    procedure DoInheritedBeforeRefresh;
    procedure DoInheritedAfterRefresh;
    function ErrorMessage: string;
    function GetDatasetFetchAllRecords: Boolean;
    procedure SetDatasetFetchAllRecords(const Value: Boolean);
    property DatasetFetchAllRecords: Boolean read GetDatasetFetchAllRecords write
        SetDatasetFetchAllRecords;
  end;


  TJvThreadedDatasetDialogOptions = class(TJvCustomThreadDialogOptions)
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
    property ShowExceptionMessage: Boolean read FShowExceptionMessage
      write FShowExceptionMessage default True;
  end;

  TJvThreadedDatasetCapitalizeLabelOptions = class(TPersistent)
  private
    FAutoExecuteAfterOpen: Boolean;
    FTrimToFirstBlank: Boolean;
  public
    constructor Create; virtual;
  published
    property AutoExecuteAfterOpen: Boolean read FAutoExecuteAfterOpen
      write FAutoExecuteAfterOpen default False;
    property TrimToFirstBlank: Boolean read FTrimToFirstBlank write FTrimToFirstBlank default False;
  end;

  TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions = class(TPersistent)
  private
    FAll: Boolean;
    FPause: Boolean;
    FCancel: Boolean;
  protected
    property Pause: Boolean read FPause write FPause default false;
    property Cancel: Boolean read FCancel write FCancel default false;
  public
    constructor Create; virtual;
  published
    property All: Boolean read FAll write FAll default false;
  end;

  TJvBaseThreadedDatasetEnhancedOptions = class(TPersistent)
  private
    FAllowedContinueRecordFetchOptions:
        TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions;
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
    function CreateAllowedContinueRecordFetchOptions:
        TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property AllowedContinueRecordFetchOptions:
        TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions read
        FAllowedContinueRecordFetchOptions write FAllowedContinueRecordFetchOptions;
    property CapitalizeLabelOptions: TJvThreadedDatasetCapitalizeLabelOptions read
      FCapitalizeLabelOptions write SetCapitalizeLabelOptions;
    property FetchRowsCheck: Integer read FFetchRowsCheck write SetFetchRowsCheck
        default 2000;
    property FetchRowsFirst: Integer read FFetchRowsFirst write SetFetchRowsFirst
        default 1000;
    property RefreshAsOpenClose: Boolean read FRefreshAsOpenClose
      write SetRefreshAsOpenClose default False;
    property RefreshLastPosition: Boolean read FRefreshLastPosition
      write SetRefreshLastPosition default False;
  end;

  TJvDatasetThreadDialogForm = class(TJvCustomThreadDialogForm)
  private
    FRowsLabel: TControl;
    FTimeLabel: TControl;
    FRowsStaticText: TWinControl;
    FTimeStaticText: TWinControl;
    FCancelBtn: TButton;
    FCancelButtonPanel: TWinControl;
    FRowsPanel: TWinControl;
    FTimePanel: TWinControl;
    FDialogOptions: TJvThreadedDatasetDialogOptions;
    FDynControlEngine: TJvDynControlEngine;
    procedure CreateTextPanel(AOwner: TComponent; AParent: TWinControl;
      var Panel: TWinControl; var LabelCtrl: TControl; var StaticText: TWinControl;
      const BaseName: string);
    function GetConnectedDataset: TDataset;
    function GetConnectedDatasetHandler: TJvBaseDatasetThreadHandler;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    procedure SetDialogOptions(const Value: TJvThreadedDatasetDialogOptions);
    procedure SetDynControlEngine(const Value: TJvDynControlEngine);
  protected
    procedure FillDialogData;
    procedure UpdateFormContents; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateFormControls;
    procedure TransferDialogOptions; override;
    property ConnectedDataset: TDataset read GetConnectedDataset;
    property ConnectedDatasetHandler: TJvBaseDatasetThreadHandler read
        GetConnectedDatasetHandler;
    property DynControlEngine: TJvDynControlEngine read FDynControlEngine write SetDynControlEngine;
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
    FConnectedDataset: TDataset;
    FConnectedDatasetInterface: IJvThreadedDatasetInterface;
    FConnectedDatasetThreadHandler: TJvBaseDatasetThreadHandler;
    procedure SetConnectedDataset(const Value: TDataset);
    procedure SetConnectedDatasetThreadHandler(const Value:
        TJvBaseDatasetThreadHandler);
  protected
    procedure intAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm); override;
    property ConnectedDatasetInterface: IJvThreadedDatasetInterface read
        FConnectedDatasetInterface;
    property ConnectedDataset: TDataset read FConnectedDataset;
  public
    procedure CancelExecute; override;
    property ConnectedDatasetThreadHandler: TJvBaseDatasetThreadHandler read
        FConnectedDatasetThreadHandler write SetConnectedDatasetThreadHandler;
  end;

  TJvBaseDatasetThreadHandler = class(TComponent)
  private
    FAfterThreadExecution: TJvThreadedDatasetThreadEvent;
    FBeforeThreadExecution: TJvThreadedDatasetThreadEvent;
    FCurrentAction: TJvThreadedDatasetAction;
    FCurrentFetchDuration: TDateTime;
    FCurrentOpenDuration: TDateTime;
    FCurrentOperation: TJvThreadedDatasetOperation;
    FCurrentOperationStart: TDateTime;
    FCurrentRow: Integer;
    FDataset: TDataset;
    FEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions;
    FErrorMessage: string;
    FExecuteThread: TJvBaseDatasetThread;
    FFetchMode: TJvThreadedDatasetFetchMode;
    FIntDatasetWasFiltered: Boolean;
    FIntDatasetFetchAllRecords: Boolean;
    FIntRowCheckEnabled: Boolean;
    FIThreadedDatasetInterface: IJvThreadedDatasetInterface;
    FLastRowChecked: Integer;
    FMoveToRecordAfterOpen: Longint;
    FOperationWasHandledInThread: Boolean;
    FSynchMessageDlgBtn: Word;
    FSynchMessageDlgMsg: string;
    FThreadDialog: TJvDatasetThreadDialog;
    FThreadOptions: TJvThreadedDatasetThreadOptions;
    function GetCurrentAction: TJvThreadedDatasetAction;
    function GetCurrentFetchDuration: TDateTime;
    function GetCurrentOpenDuration: TDateTime;
    function GetCurrentOperation: TJvThreadedDatasetOperation;
    function GetCurrentOperationAction: string;
    function GetDatasetFetchAllRecords: Boolean;
    function GetDialogOptions: TJvThreadedDatasetDialogOptions;
    function GetFetchMode: TJvThreadedDatasetFetchMode;
    procedure HandleAfterOpenRefresh;
    procedure SetCurrentAction(const Value: TJvThreadedDatasetAction);
    procedure SetCurrentFetchDuration(const Value: TDateTime);
    procedure SetCurrentOpenDuration(const Value: TDateTime);
    procedure SetDatasetFetchAllRecords(const Value: Boolean);
    procedure SetDialogOptions(Value: TJvThreadedDatasetDialogOptions);
    procedure SetEnhancedOptions(Value: TJvBaseThreadedDatasetEnhancedOptions);
    procedure SetFetchMode(const Value: TJvThreadedDatasetFetchMode);
    procedure SetThreadOptions(const Value: TJvThreadedDatasetThreadOptions);
    procedure SynchAfterThreadExecution;
    procedure SynchBeforeThreadExecution;
    procedure SynchContinueFetchMessageDlg;
    procedure SynchErrorMessageDlg;
    property DatasetFetchAllRecords: Boolean read GetDatasetFetchAllRecords write
        SetDatasetFetchAllRecords;
    property OperationWasHandledInThread: Boolean read FOperationWasHandledInThread
        write FOperationWasHandledInThread;
  protected
    procedure BreakExecution;
    function CreateEnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions; virtual;
    procedure DoThreadLast;
    procedure DoThreadOpen;
    procedure DoThreadRefresh;
    function ExecuteThreadIsActive: Boolean;
    procedure ExecuteThreadSynchronize(Method: TThreadMethod);
    procedure HandleAfterOpenRefreshThread;
    procedure HandleBeforeOpenRefresh;
    procedure IntAfterThreadExecution(DataSet: TDataset; Operation:
        TJvThreadedDatasetOperation);
    procedure IntBeforeThreadExecution(DataSet: TDataset; Operation:
        TJvThreadedDatasetOperation);
    procedure IntSynchAfterOpen;
    procedure IntSynchAfterRefresh;
    procedure IntSynchBeforeOpen;
    procedure IntSynchBeforeRefresh;
    procedure SetErrorMessage(const Value: string);
    function SupportsBreakExecution: Boolean; virtual;
    procedure ThreadExecute(Sender: TObject; Params: Pointer);
    property CurrentOperation: TJvThreadedDatasetOperation read GetCurrentOperation;
    property ExecuteThread: TJvBaseDatasetThread read FExecuteThread;
    property FetchMode: TJvThreadedDatasetFetchMode read GetFetchMode write
        SetFetchMode;
    property IntRowCheckEnabled: Boolean read FIntRowCheckEnabled write
        FIntRowCheckEnabled;
    property IThreadedDatasetInterface: IJvThreadedDatasetInterface read
        FIThreadedDatasetInterface;
    property ThreadDialog: TJvDatasetThreadDialog read FThreadDialog;
  public
    constructor Create(AOwner: TComponent; ADataset: TDataset); reintroduce;
        virtual;
    destructor Destroy; override;
    procedure AfterOpen;
    procedure AfterRefresh;
    procedure BeforeOpen;
    procedure BeforeRefresh;
    procedure CapitalizeDatasetLabels;
    function CheckContinueRecordFetch: TJvThreadedDatasetContinueCheckResult;
    procedure InternalLast; virtual;
    procedure InternalRefresh; virtual;
    procedure MoveTo(Position: Integer);
    procedure SetActive(Value: Boolean); virtual;
    function ThreadIsActive: Boolean;
    property CurrentAction: TJvThreadedDatasetAction read GetCurrentAction write
        SetCurrentAction;
    property CurrentFetchDuration: TDateTime read GetCurrentFetchDuration write
        SetCurrentFetchDuration;
    property CurrentOpenDuration: TDateTime read GetCurrentOpenDuration write
        SetCurrentOpenDuration;
    property CurrentOperationAction: string read GetCurrentOperationAction;
    property CurrentRow: Integer read FCurrentRow;
    property Dataset: TDataset read FDataset;
    property ErrorMessage: string read FErrorMessage write SetErrorMessage;
  published
    property DialogOptions: TJvThreadedDatasetDialogOptions read GetDialogOptions
        write SetDialogOptions;
    property EnhancedOptions: TJvBaseThreadedDatasetEnhancedOptions read
        FEnhancedOptions write SetEnhancedOptions;
    property ThreadOptions: TJvThreadedDatasetThreadOptions read FThreadOptions write
        SetThreadOptions;
    property AfterThreadExecution: TJvThreadedDatasetThreadEvent read
        FAfterThreadExecution write FAfterThreadExecution;
    property BeforeThreadExecution: TJvThreadedDatasetThreadEvent read
        FBeforeThreadExecution write FBeforeThreadExecution;
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
  Dialogs, 
  JvDynControlEngineIntf, JvDSADialogs, JvResources;

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
    ThreadDialogForm.CreateFormControls;
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

//=== { TJvDatasetThreadDialogForm } ===================================

constructor TJvDatasetThreadDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DynControlEngine := nil;
  InternalTimerInterval := 100;
end;

procedure TJvDatasetThreadDialogForm.CreateFormControls;
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

procedure TJvDatasetThreadDialogForm.CreateTextPanel(AOwner: TComponent;
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
    Width := 90;
  end;
  StaticText := DynControlEngine.CreateStaticTextControl(AOwner, Panel, BaseName + 'StaticText', '');
  if Supports(StaticText, IJvDynControlAutoSize, ITmpAutoSize) then
    ITmpAutoSize.ControlSetAutoSize(False);
  if Supports(StaticText, IJvDynControlAlignment, ITmpAlignment) then
    ITmpAlignment.ControlSetAlignment(taCenter);
  with StaticText do
  begin
    Top := 1;
    Left := 95;
    Height := 18;
    Panel.Height := Height + 6;
  end;
end;

procedure TJvDatasetThreadDialogForm.FillDialogData;
var
  ITmpControl: IJvDynControl;
begin
  if Assigned(ConnectedDatasetHandler) then
  with ConnectedDatasetHandler do
    begin
      if DialogOptions.Caption <> '' then
        Caption := DialogOptions.Caption +' - '+CurrentOperationAction
      else
        Caption := CurrentOperationAction;
      if Supports(FRowsStaticText, IJvDynControl, ITmpControl) then
        ITmpControl.ControlSetCaption(IntToStr(CurrentRow));
      if Supports(FTimeStaticText, IJvDynControl, ITmpControl) then
        ITmpControl.ControlSetCaption(
          FormatDateTime('hh:nn:ss', CurrentOpenDuration) + ' / ' +
          FormatDateTime('hh:nn:ss', CurrentFetchDuration));
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
  FRowsStaticText.Width:= FRowsPanel.Width - FRowsLabel.Width;
  FTimeStaticText.Width:= FTimePanel.Width - FTimeLabel.Width;
end;

function TJvDatasetThreadDialogForm.GetConnectedDataset: TDataset;
begin
  if Assigned(ConnectedDatasetHandler) then
    Result := ConnectedDatasetHandler.Dataset
  else
    Result := nil;
end;

function TJvDatasetThreadDialogForm.GetConnectedDatasetHandler:
    TJvBaseDatasetThreadHandler;
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

procedure TJvDatasetThreadDialogForm.SetDialogOptions(const Value: TJvThreadedDatasetDialogOptions);
begin
  FDialogOptions := Value;
  DynControlEngine := DialogOptions.DynControlEngine;
end;

procedure TJvDatasetThreadDialogForm.SetDynControlEngine(const Value: TJvDynControlEngine);
begin
  if not Assigned(Value) then
    FDynControlEngine := DefaultDynControlEngine
  else
    FDynControlEngine := Value;
end;

procedure TJvDatasetThreadDialogForm.TransferDialogOptions;
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

procedure TJvDatasetThreadDialogForm.UpdateFormContents;
begin
  inherited UpdateFormContents;
  FillDialogData;
end;

//=== { TJvThreadedDatasetDialogOptions } ======================================
constructor TJvThreadedDatasetDialogOptions.Create(AOwner: TJvCustomThreadDialog);
begin
  inherited Create(AOwner);
  FEnableCancelButton := True;
  FShowCancelButton := True;
  FShowRowsLabel := True;
  FShowTimeLabel := True;
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

procedure TJvThreadedDatasetDialogOptions.SetShowRowsLabel(const Value: Boolean);
begin
  FShowRowsLabel := Value;
end;

procedure TJvThreadedDatasetDialogOptions.SetShowTimeLabel(const Value: Boolean);
begin
  FShowTimeLabel := Value;
end;

procedure TJvBaseDatasetThread.intAfterCreateDialogForm(DialogForm: TJvCustomThreadDialogForm);
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

procedure TJvBaseDatasetThread.SetConnectedDataset(const Value: TDataset);
begin
  FConnectedDataset := Value;
  if Assigned(Value) then
    if not Supports(Value, IJvThreadedDatasetInterface, FConnectedDatasetInterface) then
      raise EIntfCastError.CreateRes(@RsEIntfCastError)
    else
  else
    FConnectedDatasetInterface := nil;

end;

procedure TJvBaseDatasetThread.SetConnectedDatasetThreadHandler(const Value:
    TJvBaseDatasetThreadHandler);
begin
  FConnectedDatasetThreadHandler := Value;
  if Assigned(Value) then
    SetConnectedDataset (ConnectedDatasetThreadHandler.Dataset)
  else
    SetConnectedDataset (nil);
end;

// { TJvThreadedDatasetThreadOptions } =========================================

constructor TJvThreadedDatasetThreadOptions.Create;
begin
  inherited Create;
  FLastInThread := False;
  FOpenInThread := False;
  FPriority := tpIdle;
  FRefreshInThread := False;
  FShowExceptionMessage := True;
end;

//=== { TJvThreadedDatasetEnhancedOptions } ====================================

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

function
    TJvBaseThreadedDatasetEnhancedOptions.CreateAllowedContinueRecordFetchOptions:
    TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions;
begin
  Result := TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions.Create;
end;

procedure TJvBaseThreadedDatasetEnhancedOptions.SetCapitalizeLabelOptions(const Value: TJvThreadedDatasetCapitalizeLabelOptions);
begin
  FCapitalizeLabelOptions.Assign(Value);
end;

procedure TJvBaseThreadedDatasetEnhancedOptions.SetFetchRowsCheck(const Value:
    Integer);
begin
  FFetchRowsCheck := Value;
end;

procedure TJvBaseThreadedDatasetEnhancedOptions.SetFetchRowsFirst(const Value:
    Integer);
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

//=== { TJvThreadedDatasetCapitalizeLabelOptions } =============================

constructor TJvThreadedDatasetCapitalizeLabelOptions.Create;
begin
  inherited Create;
  FAutoExecuteAfterOpen := False;
  FTrimToFirstBlank := False;
end;

constructor TJvBaseDatasetThreadHandler.Create(AOwner: TComponent; ADataset:
    TDataset);
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
  FExecuteThread.ConnectedDatasetThreadHandler := self;
  FExecuteThread.ThreadDialog := ThreadDialog;
  FEnhancedOptions := CreateEnhancedOptions;
  FCurrentOperation := tdoNothing;
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
  if not ExecuteThreadIsActive and not OperationWasHandledInThread and (CurrentOperation <> tdoRefresh) then
    HandleAfterOpenRefresh;
  ExecuteThreadSynchronize(IntSynchAfterOpen);
end;

procedure TJvBaseDatasetThreadHandler.AfterRefresh;
begin
  if not ExecuteThreadIsActive and not OperationWasHandledInThread then
    HandleAfterOpenRefresh;
  ExecuteThreadSynchronize(IntSynchAfterRefresh);
end;

procedure TJvBaseDatasetThreadHandler.BeforeOpen;
begin
  if (CurrentOperation <> tdoRefresh) then
  begin
    FMoveToRecordAfterOpen := -1;
    HandleBeforeOpenRefresh;
  end;
  ExecuteThreadSynchronize(IntSynchBeforeOpen);
end;

procedure TJvBaseDatasetThreadHandler.BeforeRefresh;
begin
  if EnhancedOptions.RefreshLastPosition then
    FMoveToRecordAfterOpen := Dataset.RecNo
  else
    FMoveToRecordAfterOpen := -1;
  HandleBeforeOpenRefresh;
  ExecuteThreadSynchronize(IntSynchBeforeRefresh);
end;

procedure TJvBaseDatasetThreadHandler.BreakExecution;
begin
  CurrentAction := tdaCancel;
  if (FetchMode = tdfmFetch) and (EnhancedOptions.AllowedContinueRecordFetchOptions.Pause or EnhancedOptions.AllowedContinueRecordFetchOptions.Cancel) then
    if EnhancedOptions.AllowedContinueRecordFetchOptions.Pause then
      FetchMode := tdfmBreak
    else
      FetchMode := tdfmStop
  else
    IThreadedDatasetInterface.BreakExecution;
  IntRowCheckEnabled := False;
end;

procedure TJvBaseDatasetThreadHandler.CapitalizeDatasetLabels;
var
  I, J: Integer;
  S: string;
  Upper: Boolean;
begin
  With Dataset do
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

function TJvBaseDatasetThreadHandler.CheckContinueRecordFetch:
    TJvThreadedDatasetContinueCheckResult;
begin
  Result := tdccrContinue;
  FCurrentRow := Dataset.RecordCount;
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
  if (EnhancedOptions.FetchRowsCheck > 0) and IntRowCheckEnabled then
    if CurrentRow >= FLastRowChecked + EnhancedOptions.FetchRowsCheck then
    begin
      fCurrentFetchDuration := fCurrentFetchDuration + Now - FCurrentOperationStart;
      CurrentAction := tdaNothing;
      FLastRowChecked := CurrentRow;
      FSynchMessageDlgMsg := Format(SODSRowsFetchedContinue, [CurrentRow]);
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
      CurrentAction := tdaFetch;
      FetchMode := tdfmFetch;
      FCurrentOperationStart := Now;
    end;
end;

function TJvBaseDatasetThreadHandler.CreateEnhancedOptions:
    TJvBaseThreadedDatasetEnhancedOptions;
begin
  Result := TJvBaseThreadedDatasetEnhancedOptions.Create;
end;

procedure TJvBaseDatasetThreadHandler.DoThreadLast;
begin
  IThreadedDatasetInterface.doInheritedInternalLast;
end;

procedure TJvBaseDatasetThreadHandler.DoThreadOpen;
begin
  try
    IThreadedDatasetInterface.doInheritedSetActive(True);
  finally
    HandleAfterOpenRefreshThread;
  end;
end;

procedure TJvBaseDatasetThreadHandler.DoThreadRefresh;
begin
  try
    if not EnhancedOptions.RefreshAsOpenClose then
    begin
      IThreadedDatasetInterface.doInheritedInternalRefresh;
    end
    else
    begin
      Dataset.Close;
      IThreadedDatasetInterface.doInternalOpen;
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
  if ExecuteThreadIsActive then
    ExecuteThread.Synchronize(Method)
  else
    Method;
end;

function TJvBaseDatasetThreadHandler.GetCurrentAction: TJvThreadedDatasetAction;
begin
  Result := FCurrentAction;
end;

function TJvBaseDatasetThreadHandler.GetCurrentFetchDuration: TDateTime;
begin
  case CurrentAction of
    tdaOpen:
      Result := 0;
    tdaNothing, tdaCancel:
      Result := FCurrentFetchDuration;
    tdaFetch:
      Result := FCurrentFetchDuration + (Now - FCurrentOperationStart);
  else
    Result := 0;
  end;
end;

function TJvBaseDatasetThreadHandler.GetCurrentOpenDuration: TDateTime;
begin
  if CurrentAction = tdaOpen then
    Result := Now - FCurrentOperationStart
  else
    Result := FCurrentOpenDuration;
end;

function TJvBaseDatasetThreadHandler.GetCurrentOperation: TJvThreadedDatasetOperation;
begin
  Result := FCurrentOperation;
end;

function TJvBaseDatasetThreadHandler.GetCurrentOperationAction: string;
begin
  case CurrentOperation of
    tdoOpen:
      case CurrentAction of
        tdaOpen:
          Result := SODSOpenQuery;
        tdaFetch:
          Result := SODSOpenQueryFetchRecords;
        tdaCancel :
          Result := SODSOpenQueryCancel;
      end;
    tdoRefresh:
      case CurrentAction of
        tdaOpen:
          Result := SODSRefreshQuery;
        tdaFetch:
          Result := SODSRefreshQueryFetchRecords;
        tdaCancel :
          Result := SODSRefreshQueryCancel;
      end;
    tdoFetch:
      case CurrentAction of
        tdaFetch:
          Result := SODSFetchRecords;
        tdaCancel :
          Result := SODSFetchRecordsCancel;
      end;
    tdoLast:
      Result := SODSGotoLastFetchRecords;
  end;
end;

function TJvBaseDatasetThreadHandler.GetDatasetFetchAllRecords: Boolean;
begin
  if Assigned(IThreadedDatasetInterface) then
    Result := IThreadedDatasetInterface.DatasetFetchAllRecords
  else
    Result := false;
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

procedure TJvBaseDatasetThreadHandler.HandleAfterOpenRefresh;
begin
  CurrentOpenDuration := Now - FCurrentOperationStart;
  FCurrentOperationStart := Now;
  DatasetFetchAllRecords := FIntDatasetFetchAllRecords;
  CurrentFetchDuration := 0;
  if CurrentAction <> tdaCancel then
  begin
    CurrentAction := tdaFetch;
    FetchMode := tdfmFetch;
    if Dataset.Active then
    begin
      Dataset.First;
      if DatasetFetchAllRecords then
        IThreadedDatasetInterface.doInheritedInternalLast
      else
        if (EnhancedOptions.FetchRowsFirst > Dataset.RecordCount) or (FMoveToRecordAfterOpen > Dataset.RecordCount) then
          if FMoveToRecordAfterOpen > EnhancedOptions.FetchRowsFirst then
            Dataset.MoveBy(FMoveToRecordAfterOpen - 1)
          else
            Dataset.MoveBy(EnhancedOptions.FetchRowsFirst - 1);
    end;
  end;
  try
    Dataset.Filtered := FIntDatasetWasFiltered;
    if Dataset.Active and (CurrentAction <> tdaCancel) then
      if FMoveToRecordAfterOpen > 0 then
        MoveTo(FMoveToRecordAfterOpen)
      else
        Dataset.First;
    CurrentAction := tdaNothing;
  finally
    ExecuteThreadSynchronize(Dataset.EnableControls);
  end;
end;

procedure TJvBaseDatasetThreadHandler.HandleAfterOpenRefreshThread;
begin
  HandleAfterOpenRefresh;
end;

procedure TJvBaseDatasetThreadHandler.HandleBeforeOpenRefresh;
begin
  OperationWasHandledInThread := False;
  ExecuteThreadSynchronize(Dataset.DisableControls);
  CurrentOpenDuration := 0;
  CurrentFetchDuration := 0;
  IntRowCheckEnabled := True;
  FCurrentRow := 0;
  FCurrentOperationStart := Now;
  CurrentAction := tdaOpen;
  FetchMode := tdfmNothing;
  FIntDatasetFetchAllRecords := DatasetFetchAllRecords;
  FLastRowChecked := 0;
  FIntDatasetWasFiltered := Dataset.Filtered;
  Dataset.Filtered := False;
  DatasetFetchAllRecords := False;
end;

procedure TJvBaseDatasetThreadHandler.IntAfterThreadExecution(DataSet: TDataset;
    Operation: TJvThreadedDatasetOperation);
begin
  if Assigned(FAfterThreadExecution) then
    FAfterThreadExecution(DataSet, Operation);
end;

procedure TJvBaseDatasetThreadHandler.IntBeforeThreadExecution(DataSet: TDataset;
    Operation: TJvThreadedDatasetOperation);
begin
  if Assigned(FBeforeThreadExecution) then
    FBeforeThreadExecution(DataSet, Operation);
end;

procedure TJvBaseDatasetThreadHandler.InternalLast;
var
  ShowModal    :Boolean;
begin
  if FCurrentOperation <> tdoNothing then
    Exit;
  FCurrentOperation := tdoLast;
  if not ThreadOptions.LastInThread or ThreadIsActive or (csDesigning in ComponentState) then
  begin
    IThreadedDatasetInterface.doInheritedInternalLast;
    FCurrentOperation := tdoNothing;
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
  if FCurrentOperation <> tdoNothing then
    Exit;
  FCurrentOperation := tdoRefresh;
  if not ThreadOptions.RefreshInThread or not IThreadedDatasetInterface.IsThreadAllowed or
    ThreadIsActive or (csDesigning in ComponentState) then
  begin
    IThreadedDatasetInterface.doInheritedInternalRefresh;
    FCurrentOperation := tdoNothing;
  end
  else
    ExecuteThread.ExecuteWithDialog(nil);
end;

procedure TJvBaseDatasetThreadHandler.IntSynchAfterOpen;
begin
  if EnhancedOptions.CapitalizeLabelOptions.AutoExecuteAfterOpen then
    CapitalizeDatasetLabels;
  IThreadedDatasetInterface.DoInheritedAfterOpen;
  FIntDatasetWasFiltered := Dataset.Filtered or FIntDatasetWasFiltered; // Added because in the afteropen event the filtered could be activated, and it should be deactivated for the dialog
  Dataset.Filtered := False;
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

procedure TJvBaseDatasetThreadHandler.MoveTo(Position: Integer);
begin
  Dataset.MoveBy(Position - Dataset.RecNo);
end;

procedure TJvBaseDatasetThreadHandler.SetActive(Value: Boolean);
begin
  if not Value then
  begin
    CurrentOpenDuration := 0;
    CurrentFetchDuration := 0;
    IThreadedDatasetInterface.doInheritedSetActive(Value);
  end
  else
  begin
    if (FCurrentOperation <> tdoNothing) and
       (CurrentOperation <> tdoRefresh) then
      Exit;
    if CurrentOperation <> tdoRefresh then
      FCurrentOperation := tdoOpen;
    if not ThreadOptions.OpenInThread or ThreadIsActive or (csDesigning in ComponentState) then
    begin
      IThreadedDatasetInterface.doInheritedSetActive(Value);
      if CurrentOperation <> tdoRefresh then
        FCurrentOperation := tdoNothing;
    end
    else
      ExecuteThread.ExecuteWithDialog(nil);
  end;
end;

procedure TJvBaseDatasetThreadHandler.SetCurrentAction(const Value:
    TJvThreadedDatasetAction);
begin
  FCurrentAction := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetCurrentFetchDuration(const Value: TDateTime);
begin
  FCurrentFetchDuration := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetCurrentOpenDuration(const Value: TDateTime);
begin
  FCurrentOpenDuration := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetDatasetFetchAllRecords(const Value:
    Boolean);
begin
   if Assigned(IThreadedDatasetInterface) then
    IThreadedDatasetInterface.DatasetFetchAllRecords := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetDialogOptions(Value:
    TJvThreadedDatasetDialogOptions);
begin
  ThreadDialog.DialogOptions.Assign(Value);
end;

procedure TJvBaseDatasetThreadHandler.SetEnhancedOptions(Value:
    TJvBaseThreadedDatasetEnhancedOptions);
begin
  FEnhancedOptions.Assign(Value);
end;

procedure TJvBaseDatasetThreadHandler.SetErrorMessage(const Value: string);
begin
  FErrorMessage := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetFetchMode(const Value:
    TJvThreadedDatasetFetchMode);
begin
  FFetchMode := Value;
end;

procedure TJvBaseDatasetThreadHandler.SetThreadOptions(const Value:
    TJvThreadedDatasetThreadOptions);
begin
  FThreadOptions.Assign(Value);
end;

function TJvBaseDatasetThreadHandler.SupportsBreakExecution: Boolean;
begin
  Result := True;
end;

procedure TJvBaseDatasetThreadHandler.SynchAfterThreadExecution;
begin
  IntAfterThreadExecution(Dataset, CurrentOperation);
end;

procedure TJvBaseDatasetThreadHandler.SynchBeforeThreadExecution;
begin
  IntBeforeThreadExecution(Dataset, CurrentOperation);
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
  AddButton(SODSContinueYes, Integer(mrYes));
  if EnhancedOptions.AllowedContinueRecordFetchOptions.Pause then
    AddButton(SODSContinuePause, Integer(mrCancel));
  AddButton(SODSContinueNo, Integer(mrNo));
  if EnhancedOptions.AllowedContinueRecordFetchOptions.All then
    AddButton(SODSContinueAll, Integer(mrAll));
  if EnhancedOptions.AllowedContinueRecordFetchOptions.Cancel then
    AddButton(SODSContinueClose, Integer(mrAbort));
  FSynchMessageDlgBtn := JvDSADialogs.MessageDlgEx(FSynchMessageDlgMsg,
      mtConfirmation, Buttons, Results, 0, dckActiveForm, 0,
      0, 1, -1, DialogOptions.DynControlEngine);
end;

procedure TJvBaseDatasetThreadHandler.SynchErrorMessageDlg;
begin
  FSynchMessageDlgBtn := JvDSADialogs.MessageDlg(FSynchMessageDlgMsg,
    mtError, [mbOK], 0, dckScreen, 0,
    mbDefault, mbDefault, mbHelp, DialogOptions.DynControlEngine);
end;

procedure TJvBaseDatasetThreadHandler.ThreadExecute(Sender: TObject; Params: Pointer);
begin
  OperationWasHandledInThread := True;
  try
    SetErrorMessage('');
    ExecuteThreadSynchronize(SynchBeforeThreadExecution);
    try
      case FCurrentOperation of
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
    FCurrentOperation := tdoNothing;
  end;
end;

function TJvBaseDatasetThreadHandler.ThreadIsActive: Boolean;
begin
  Result := not ExecuteThread.Terminated;
end;

constructor TJvBaseThreadedDatasetAllowedContinueRecordFetchOptions.Create;
begin
  inherited Create;
  FAll := false;
  FCancel := false;
  FPause := false;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
end.

