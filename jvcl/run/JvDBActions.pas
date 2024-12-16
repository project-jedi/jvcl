{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActions.Pas, released on 2004-12-30.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBActions;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, ActnList, Graphics,
  Forms, Controls, Classes, DB,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGridCustomTableView,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  {$IFDEF USE_3RDPARTY_SMEXPORT}
  SMEWIZ, ExportDS, SMEEngine,
  {$ENDIF USE_3RDPARTY_SMEXPORT}
  {$IFDEF USE_3RDPARTY_SMIMPORT}
  SMIWiz, SMIBase,
  {$ENDIF USE_3RDPARTY_SMIMPORT}
  DBGrids, JvActionsEngine, JvDBActionsEngine, JvDynControlEngineDBTools,
  JvDynControlEngineDB, JvParameterListParameter;

type

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDatabaseActionList = class(TJvActionBaseActionList)
  //The idea of the Action Classes is to work with any databased enabled control.
  //But not all of this controls already have a dataset or datasource control.
  //
  //So the connection is made by the "DataComponent".
  //Datacomponent can be any type of TComponent. TDatasource, TDataset, TDBGrid,
  //TDBEdit, also for examle the DevExpress Controls.
  //
  //Then we have a list of DatabaseControlEngines which have the availability to
  //find for a Component the based Dataset and Datasource Controls (if they exist).
  //For each new type of controls with specific need of handles a new Engine
  //must be created and registered. An example for these engines can be found
  //in "JvDBActionsEngineControlCxGrid.pas".
  //
  //When a datacomponent is assigned the action tries to find the correct
  //engine based on the component and uses the engine for all further operations.
  //
  //There are two ways to assign a datacomponent:
  //1. Assigning the component to the action list, then all actions in
  //   this list (which are based on TJvDatabaseBaseAction class)
  //   gets the datacomponent assigned also.
  //2. Using the active control, like the normal action handling.
  private
    function GetDataComponent: TComponent;
    function GetOnChangeDataComponent: TJvChangeActionComponent;
    procedure SetOnChangeDataComponent(const Value: TJvChangeActionComponent);
    procedure SetDataComponent(Value: TComponent);
  published
    property DataComponent: TComponent read GetDataComponent write SetDataComponent;
    property OnChangeDataComponent: TJvChangeActionComponent read GetOnChangeDataComponent write SetOnChangeDataComponent;
  end;

  TJvDatabaseActionBaseEngineClass = class of TJvDatabaseActionBaseControlEngine;

  TJvDatabaseBeforeExecuteEvent = procedure(Sender: TObject; ControlEngine: TJvDatabaseActionBaseControlEngine;
      DataComponent: TComponent; var ContinueExecute: Boolean) of object;
  TJvDatabaseExecuteEvent = procedure(Sender: TObject; ControlEngine: TJvDatabaseActionBaseControlEngine;
    DataComponent: TComponent) of object;
  TJvDatabaseExecuteDataSourceEvent = procedure(Sender: TObject; DataSource: TDataSource) of object;

  TJvDatabaseActionCheckEnabledEvent = procedure(aDataset : TDataset;aDataComponent : TComponent; aDatabaseControlEngine:
      TJvDatabaseActionBaseControlEngine; var aEnabled : Boolean) of object;

  TJvDatabaseBaseAction = class(TJvActionEngineBaseAction)
  private
    FDatabaseControlEngine: TJvDatabaseActionBaseControlEngine;
    FOnExecute: TJvDatabaseExecuteEvent;
    FOnExecuteDataSource: TJvDatabaseExecuteDataSourceEvent;
    fAfterExecute: TJvDatabaseExecuteEvent;
    FBeforeExecute: TJvDatabaseBeforeExecuteEvent;
    FDatasetEngine: TJvDatabaseActionBaseDatasetEngine;
    FOnCheckEnabled: TJvDatabaseActionCheckEnabledEvent;
    function GetOnChangeDataComponent: TJvChangeActionComponent;
    procedure SetOnChangeDataComponent(const Value: TJvChangeActionComponent);
  protected
    //1 This Procedure is called when the ActionComponent is changed
    procedure ChangeActionComponent(const AActionComponent: TComponent); override;
    procedure CheckEnabled(var AEnabled: Boolean); override;
    function GetDataComponent: TComponent;
    procedure SetDataComponent(Value: TComponent);
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function EngineIsActive: Boolean;
    function EngineHasData: Boolean;
    function EngineFieldCount: Integer;
    function EngineRecordCount: Integer;
    function EngineRecNo: Integer;
    function EngineCanInsert: Boolean;
    function EngineCanUpdate: Boolean;
    function EngineCanDelete: Boolean;
    function EngineEof: Boolean;
    function EngineBof: Boolean;
    function EngineCanNavigate: Boolean;
    function EngineCanRefresh: Boolean;
    function EngineCanRefreshRecord: Boolean;
    function EngineControlsDisabled: Boolean;
    function EngineEditModeActive: Boolean;
    function EngineSelectedRowsCount: Integer;
    function GetEngineList: TJvActionEngineList; override;
    property DatabaseControlEngine: TJvDatabaseActionBaseControlEngine read FDatabaseControlEngine;
    property DatasetEngine: TJvDatabaseActionBaseDatasetEngine read FDatasetEngine;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    property DataSource: TDataSource read GetDataSource;
    property DataSet: TDataSet read GetDataSet;
  published
    property OnChangeDataComponent: TJvChangeActionComponent read GetOnChangeDataComponent write SetOnChangeDataComponent;
    property OnCheckEnabled: TJvDatabaseActionCheckEnabledEvent read FOnCheckEnabled write FOnCheckEnabled;
    property OnExecute: TJvDatabaseExecuteEvent read FOnExecute write FOnExecute;
    property AfterExecute: TJvDatabaseExecuteEvent read FAfterExecute write FAfterExecute;
    property BeforeExecute: TJvDatabaseBeforeExecuteEvent read FBeforeExecute write FBeforeExecute;
    property OnExecuteDataSource: TJvDatabaseExecuteDataSourceEvent read FOnExecuteDataSource write FOnExecuteDataSource;
    property DataComponent: TComponent read GetDataComponent write SetDataComponent;
  end;

  TJvDatabaseSimpleAction = class(TJvDatabaseBaseAction)
  private
    FIsActive: Boolean;
    FHasData: Boolean;
    FCanInsert: Boolean;
    FCanUpdate: Boolean;
    FCanDelete: Boolean;
    FEditModeActive: Boolean;
    FManualEnabled: Boolean;
    procedure SetManualEnabled(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    // If this paramater is active, the Action will be enabled if for the datacomponent-dataset is active
    property IsActive: Boolean read FIsActive write FIsActive default True;
    // If this paramater is active, the Action will be enabled if for the datacomponent-dataset contains records
    property HasData: Boolean read FHasData write FHasData default True;
    // If this paramater is active, the Action will be enabled if insert is allowed for the datacomponent-dataset
    property CanInsert: Boolean read FCanInsert write FCanInsert default False;
    // If this paramater is active, the Action will be enabled if update is allowed for the datacomponent-dataset
    property CanUpdate: Boolean read FCanUpdate write FCanUpdate default False;
    // If this paramater is active, the Action will be enabled if delete is allowed for the datacomponent-dataset
    property CanDelete: Boolean read FCanDelete write FCanDelete default False;
    // If this paramater is active, the Action will be enabled if the datacomponent-dataset is in edit mode
    property EditModeActive: Boolean read FEditModeActive write FEditModeActive default False;
    // This property allows you enable / disable the action independently from the
    // automatic handling by IsActive, HasData, CanInsert, CanUpdate, EditModeActive
    property ManualEnabled: Boolean read FManualEnabled write SetManualEnabled default True;
  end;

  TJvDatabaseBaseActiveAction = class(TJvDatabaseBaseAction)
  public
    procedure UpdateTarget(Target: TObject); override;
  end;

  TJvDatabaseBaseEditAction = class(TJvDatabaseBaseActiveAction)
  public
    procedure UpdateTarget(Target: TObject); override;
  end;

  TJvDatabaseBaseNavigateAction = class(TJvDatabaseBaseActiveAction)
  end;

  TJvDatabaseFirstAction = class(TJvDatabaseBaseNavigateAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabaseLastAction = class(TJvDatabaseBaseNavigateAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabasePriorAction = class(TJvDatabaseBaseNavigateAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabaseNextAction = class(TJvDatabaseBaseNavigateAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabasePriorBlockAction = class(TJvDatabaseBaseNavigateAction)
  public
    FBlockSize: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BlockSize: Integer read FBlockSize write FBlockSize default 50;
  end;

  TJvDatabaseNextBlockAction = class(TJvDatabaseBaseNavigateAction)
  private
    FBlockSize: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BlockSize: Integer read FBlockSize write FBlockSize default 50;
  end;

  TJvDatabaseRefreshAction = class(TJvDatabaseBaseActiveAction)
  private
    FRefreshLastPosition: Boolean;
    FRefreshAsOpenClose: Boolean;
  protected
    procedure Refresh;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property RefreshLastPosition: Boolean read FRefreshLastPosition write FRefreshLastPosition default True;
    property RefreshAsOpenClose: Boolean read FRefreshAsOpenClose write FRefreshAsOpenClose default False;
  end;

  TJvDatabasePositionAction = class(TJvDatabaseBaseNavigateAction)
  private
    FMinCountSelectedRows: Integer;
    FShowSelectedRows: Boolean;
  protected
    procedure SetCaption(Value: string); {$IFDEF RTL240_UP}reintroduce;{$ENDIF RTL240_UP}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowPositionDialog;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property MinCountSelectedRows: Integer read FMinCountSelectedRows write FMinCountSelectedRows default 2;
    property ShowSelectedRows: Boolean read FShowSelectedRows write FShowSelectedRows default True;
  end;

  TJvDatabaseInsertType = ( itInsert, itAppend);

  TJvDatabaseSingleRecordWindowAction = class;

  TJvDatabaseInsertAction = class(TJvDatabaseBaseEditAction)
  private
    FInsertType: TJvDatabaseInsertType;
    FSingleRecordWindowAction: TJvDatabaseSingleRecordWindowAction;
    procedure SetSingleRecordWindowAction(const Value: TJvDatabaseSingleRecordWindowAction);
    procedure SingleRecordOnFormShowEvent(ADatacomponent : TComponent; ADynControlEngineDB: TJvDynControlEngineDB);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    //1 The property defines that the new record is created via the insert or the append method
    property InsertType: TJvDatabaseInsertType read FInsertType write FInsertType default itInsert;
    //1 Use this property to show a single record window after inserting a new record
    property SingleRecordWindowAction: TJvDatabaseSingleRecordWindowAction read FSingleRecordWindowAction write
        SetSingleRecordWindowAction;
  end;

  TJvDatabaseOnCopyRecord = procedure(Field: TField; OldValue: Variant) of object;
  TJvDatabaseBeforeCopyRecord = procedure(DataSet: TDataSet; var RefreshAllowed: Boolean) of object;
  TJvDatabaseAfterCopyRecord = procedure(DataSet: TDataSet) of object;

  TJvDatabaseCopyAction = class(TJvDatabaseBaseEditAction)
  private
    FBeforeCopyRecord: TJvDatabaseBeforeCopyRecord;
    FAfterCopyRecord: TJvDatabaseAfterCopyRecord;
    FInsertType: TJvDatabaseInsertType;
    FOnCopyRecord: TJvDatabaseOnCopyRecord;
    FSingleRecordWindowAction: TJvDatabaseSingleRecordWindowAction;
    procedure SetSingleRecordWindowAction(const Value: TJvDatabaseSingleRecordWindowAction);
    procedure SingleRecordOnFormShowEvent(ADatacomponent : TComponent; ADynControlEngineDB: TJvDynControlEngineDB);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CopyRecord;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    //1 The property defines that the new record is created via the insert or the append method
    property InsertType: TJvDatabaseInsertType read FInsertType write FInsertType default itInsert;
    //1 Use this property to show a single record window after inserting a new record
    property SingleRecordWindowAction: TJvDatabaseSingleRecordWindowAction read FSingleRecordWindowAction write
        SetSingleRecordWindowAction;
    property BeforeCopyRecord: TJvDatabaseBeforeCopyRecord read FBeforeCopyRecord write FBeforeCopyRecord;
    property AfterCopyRecord: TJvDatabaseAfterCopyRecord read FAfterCopyRecord write FAfterCopyRecord;
    property OnCopyRecord: TJvDatabaseOnCopyRecord read FOnCopyRecord write FOnCopyRecord;
  end;

  TJvDatabaseEditAction = class(TJvDatabaseBaseEditAction)
  private
    FSingleRecordWindowAction: TJvDatabaseSingleRecordWindowAction;
    procedure SetSingleRecordWindowAction(const Value: TJvDatabaseSingleRecordWindowAction);
    procedure SingleRecordOnFormShowEvent(ADatacomponent : TComponent; ADynControlEngineDB: TJvDynControlEngineDB);
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    //1 Use this property to show a single record window after inserting a new record
    property SingleRecordWindowAction: TJvDatabaseSingleRecordWindowAction read FSingleRecordWindowAction write
        SetSingleRecordWindowAction;
  end;

  TJvDatabaseDeleteAction = class(TJvDatabaseBaseEditAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabasePostAction = class(TJvDatabaseBaseEditAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabaseCancelAction = class(TJvDatabaseBaseEditAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabaseSingleRecordWindowAction = class(TJvDatabaseBaseActiveAction)
  private
    FOnCreateDataControlsEvent: TJvDataSourceEditDialogCreateDataControlsEvent;
    FOptions: TJvShowSingleRecordWindowOptions;
  protected
    FOnFormShow: TJvDataSourceEditDialogOnFormShowEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure ShowSingleRecordWindow;
    property OnFormShow: TJvDataSourceEditDialogOnFormShowEvent read FOnFormShow write FOnFormShow;
  published
    property OnCreateDataControlsEvent: TJvDataSourceEditDialogCreateDataControlsEvent read FOnCreateDataControlsEvent
        write FOnCreateDataControlsEvent;
    property Options: TJvShowSingleRecordWindowOptions read FOptions write FOptions;
  end;

  TJvDatabaseOpenAction = class(TJvDatabaseBaseActiveAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabaseCloseAction = class(TJvDatabaseBaseActiveAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  {$IFDEF USE_3RDPARTY_SMEXPORT}

  TJvDatabaseSMExportOptions = class(TPersistent)
  private
    FOnAfterExecuteExport: TNotifyEvent;
    FOnBeforeExecuteExport: TNotifyEvent;
    FHelpContext: THelpContext;
    FFormats: TExportFormatTypes;
    FTitle: TCaption;
    FDefaultOptionsDirectory: string;
    FKeyGenerator: string;
    FOptions: TSMOptions;
    procedure SetDefaultOptionsDirectory(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SMEWizardDlgGetCellParams(Sender: TObject; Field: TField; var Text: TSMEString; AFont: TFont; var Alignment:
        TAlignment; var Background: TColor; var CellType: TCellType);
    procedure SMEWizardDlgOnBeforeExecute(Sender: TObject);
  published
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property Formats: TExportFormatTypes read FFormats write FFormats;
    property Title: TCaption read FTitle write FTitle;
    property DefaultOptionsDirectory: string read FDefaultOptionsDirectory write SetDefaultOptionsDirectory;
    property KeyGenerator: string read FKeyGenerator write FKeyGenerator;
    property Options: TSMOptions read FOptions write FOptions;
    property OnAfterExecuteExport: TNotifyEvent read FOnAfterExecuteExport write FOnAfterExecuteExport;
    property OnBeforeExecuteExport: TNotifyEvent read FOnBeforeExecuteExport write FOnBeforeExecuteExport;
  end;

  TJvDatabaseSMExportAction = class(TJvDatabaseBaseActiveAction)
  private
    FOptions: TJvDatabaseSMExportOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure ExportData;
  published
    property Options: TJvDatabaseSMExportOptions read FOptions write FOptions;
  end;

  {$ENDIF USE_3RDPARTY_SMEXPORT}

  {$IFDEF USE_3RDPARTY_SMIMPORT}

  TJvDatabaseSMImportOptions = class(TPersistent)
  private
    FHelpContext: THelpContext;
    FFormats: TImportFormatTypes;
    FTitle: TCaption;
    FDefaultOptionsDirectory: string;
    FOptions: TSMIOptions;
    FWizardStyle: TSMIWizardStyle;
    procedure SetDefaultOptionsDirectory(const Value: string);
  public
    constructor Create;
  published
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property Formats: TImportFormatTypes read FFormats write FFormats;
    property Title: TCaption read FTitle write FTitle;
    property DefaultOptionsDirectory: string read FDefaultOptionsDirectory write SetDefaultOptionsDirectory;
    property Options: TSMIOptions read FOptions write FOptions;
    property WizardStyle: TSMIWizardStyle read FWizardStyle write FWizardStyle;
  end;

  TJvDatabaseSMImportAction = class(TJvDatabaseBaseEditAction)
  private
    FOptions: TJvDatabaseSMImportOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure ImportData;
  published
    property Options: TJvDatabaseSMImportOptions read FOptions write FOptions;
  end;

  {$ENDIF USE_3RDPARTY_SMIMPORT}

  TJvDatabaseModifyAllAction = class(TJvDatabaseBaseEditAction)
  private
    FEnabledOnlyIfSelectedRows: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure ModifyAll;
    procedure UpdateTarget(Target: TObject); override;
  published
    property EnabledOnlyIfSelectedRows: Boolean read FEnabledOnlyIfSelectedRows write FEnabledOnlyIfSelectedRows default
        True;
  end;

  TJvDatabaseShowSQLStatementAction = class(TJvDatabaseBaseActiveAction)
  private
    FWordWrap: Boolean;
    MemoParameter: TJvMemoParameter;
    CheckBoxParameter : TJvCheckBoxParameter;
    FShowWordWrapCheckBox: Boolean;
    procedure CheckBoxOnChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure ShowSQLStatement;
    procedure UpdateTarget(Target: TObject); override;
  published
    //1 Defines if there is a visible checkbox to customize the word wrap behaviour of the memo control
    property ShowWordWrapCheckBox: Boolean read FShowWordWrapCheckBox write FShowWordWrapCheckBox default False;
    //1 Defines if the memo for the sql-statement is word-wrapped
    property WordWrap: Boolean read FWordWrap write FWordWrap default True;
  end;

type
  TJvDatabaseRefreshRecordAction = class(TJvDatabaseBaseActiveAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
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
  SysUtils, StrUtils,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGrid,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  {$IFDEF USE_3RDPARTY_SMEXPORT}
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  SMEEngCx,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  sme2sql,
  {$ENDIF USE_3RDPARTY_SMEXPORT}
  JvResources, JvParameterList, JvDSADialogs,
  Variants, Dialogs, StdCtrls, Clipbrd, JvJVCLUtils, JclFileUtils;

function TJvDatabaseActionList.GetDataComponent: TComponent;
begin
  Result := ActionComponent;
end;

function TJvDatabaseActionList.GetOnChangeDataComponent: TJvChangeActionComponent;
begin
  Result := OnChangeActionComponent;
end;

//=== { TJvDatabaseActionList } ==============================================

procedure TJvDatabaseActionList.SetDataComponent(Value: TComponent);
begin
  ActionComponent := Value;
end;

procedure TJvDatabaseActionList.SetOnChangeDataComponent(const Value: TJvChangeActionComponent);
begin
  OnChangeActionComponent := Value;
end;

//=== { TJvDatabaseBaseAction } ==============================================

constructor TJvDatabaseBaseAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatabaseControlEngine := Nil;
  UpdateTarget(nil);
end;

//=== { TJvActionEngineBaseAction } ========================================

procedure TJvDatabaseBaseAction.ChangeActionComponent(const AActionComponent: TComponent);
begin
  inherited ChangeActionComponent(AActionComponent);
  if Assigned(ControlEngine) and (ControlEngine is TJvDatabaseActionBaseControlEngine) then
    FDatabaseControlEngine := TJvDatabaseActionBaseControlEngine(ControlEngine)
  else
  begin
    FDatabaseControlEngine := Nil;
    UpdateTarget(nil);
  end;
  if Assigned(Dataset) then
  begin
    if Assigned(EngineList) and (EngineList is TJvDatabaseActionEngineList) then
      FDatasetEngine := TJvDatabaseActionEngineList(EngineList).GetDatasetEngine(Dataset)
    else
      FDatasetEngine := Nil;
  end
  else
    FDatasetEngine := nil;
end;

procedure TJvDatabaseBaseAction.CheckEnabled(var AEnabled: Boolean);
begin
  if Assigned(fOnCheckEnabled) then
    fOnCheckEnabled (DataSet, DataComponent, DatabaseControlEngine, aEnabled);
end;

function TJvDatabaseBaseAction.GetDataSet: TDataSet;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.DataSet(ActionComponent)
  else
    Result := nil;
end;

function TJvDatabaseBaseAction.GetDataSource: TDataSource;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.DataSource(ActionComponent)
  else
    Result := nil;
end;

procedure TJvDatabaseBaseAction.SetDataComponent(Value: TComponent);
begin
  ActionComponent := Value;
end;

function TJvDatabaseBaseAction.EngineIsActive: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.IsActive (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineHasData: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.HasData (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineFieldCount: Integer;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.FieldCount (DataComponent)
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.EngineRecordCount: Integer;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.RecordCount (DataComponent)
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.EngineRecNo: Integer;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.RecNo (DataComponent)
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.EngineCanInsert: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.CanInsert (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineCanUpdate: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.CanUpdate (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineCanDelete: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.CanDelete (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineEof: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.EOF (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineBof: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.Bof (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineCanNavigate: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.CanNavigate (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineCanRefresh: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.CanRefresh (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineCanRefreshRecord: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.CanRefreshRecord (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineControlsDisabled: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.ControlsDisabled (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineEditModeActive: Boolean;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.EditModeActive (DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineSelectedRowsCount: Integer;
begin
  if Assigned(DatabaseControlEngine) then
    Result := DatabaseControlEngine.SelectedRowsCount (DataComponent)
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.Execute: Boolean;
var
  ContinueExecute: Boolean;
begin
  Result := False;
  if Assigned(FBeforeExecute) then
    FBeforeExecute(Self, DatabaseControlEngine, DataComponent, ContinueExecute)
  else
    ContinueExecute := True;
  if ContinueExecute then
  begin
    Result := inherited Execute;
    if Result and Assigned(FAfterExecute) then
      FAfterExecute(Self, DatabaseControlEngine, DataComponent)
  end;
end;

function TJvDatabaseBaseAction.HandlesTarget(Target: TObject): Boolean;
begin
  //  Result := inherited HandlesTarget(Target);
  Result := Assigned(ControlEngine);
end;

procedure TJvDatabaseBaseAction.UpdateTarget(Target: TObject);
begin
  if Assigned(DataSet) and not EngineControlsDisabled then
    SetEnabled(True)
  else
    SetEnabled(False);
end;

procedure TJvDatabaseBaseAction.ExecuteTarget(Target: TObject);
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self, DatabaseControlEngine, DataComponent)
  else
    if Assigned(FOnExecuteDataSource) then
      FOnExecuteDataSource(Self, DataSource)
    else
      inherited ExecuteTarget(Target);
end;

function TJvDatabaseBaseAction.GetDataComponent: TComponent;
begin
  Result := ActionComponent;
end;

function TJvDatabaseBaseAction.GetEngineList: TJvActionEngineList;
begin
  Result := RegisteredDatabaseActionEngineList;
end;

function TJvDatabaseBaseAction.GetOnChangeDataComponent: TJvChangeActionComponent;
begin
  Result := OnChangeActionComponent;
end;

procedure TJvDatabaseBaseAction.SetOnChangeDataComponent(const Value: TJvChangeActionComponent);
begin
  OnChangeActionComponent := Value;
end;

//=== { TJvDatabaseSimpleAction } ============================================

constructor TJvDatabaseSimpleAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsActive := True;
  FHasData := True;
  FCanInsert := False;
  FCanUpdate := False;
  FCanDelete := False;
  FEditModeActive := False;
  FManualEnabled := True;
end;

procedure TJvDatabaseSimpleAction.SetManualEnabled(const Value: Boolean);
begin
  FManualEnabled := Value;
  UpdateTarget(Self);
end;

procedure TJvDatabaseSimpleAction.UpdateTarget(Target: TObject);
var
  Res: Boolean;
begin
  if Assigned(DataSet) and not EngineControlsDisabled then
  begin
    Res := ManualEnabled;
    if IsActive then
      Res := Res and EngineIsActive;
    if HasData then
      Res := Res and EngineHasData;
    if CanInsert then
      Res := Res and EngineCanInsert;
    if CanUpdate then
      Res := Res and EngineCanUpdate;
    if CanDelete then
      Res := Res and EngineCanDelete;
    if EditModeActive then
      Res := Res and EngineEditModeActive;
    SetEnabled(Res);
  end
  else
    SetEnabled(False);
end;

//=== { TJvDatabaseBaseActiveAction } ========================================

procedure TJvDatabaseBaseActiveAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive);
end;

//=== { TJvDatabaseBaseEditAction } ==========================================

procedure TJvDatabaseBaseEditAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and
    (EngineCanInsert or EngineCanUpdate or EngineCanDelete));
end;

//=== { TJvDatabaseFirstAction } =============================================

procedure TJvDatabaseFirstAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DatabaseControlEngine) and not EngineControlsDisabled and EngineIsActive and
    not EngineBof and EngineCanNavigate);
end;

procedure TJvDatabaseFirstAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  DatabaseControlEngine.First (DataComponent);
end;

//=== { TJvDatabaseLastAction } ==============================================

procedure TJvDatabaseLastAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DatabaseControlEngine) and not EngineControlsDisabled and EngineIsActive and
    not EngineEof and EngineCanNavigate);
end;

procedure TJvDatabaseLastAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  DatabaseControlEngine.Last (DataComponent);
end;

//=== { TJvDatabasePriorAction } =============================================

procedure TJvDatabasePriorAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DatabaseControlEngine) and not EngineControlsDisabled and EngineIsActive and
    not EngineBof and EngineCanNavigate);
end;

procedure TJvDatabasePriorAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  DatabaseControlEngine.MoveBy(DataComponent,-1);
end;

//=== { TJvDatabaseNextAction } ==============================================

procedure TJvDatabaseNextAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DatabaseControlEngine) and not EngineControlsDisabled and EngineIsActive and
    not EngineEof and EngineCanNavigate);
end;

procedure TJvDatabaseNextAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  DatabaseControlEngine.MoveBy(DataComponent,1);
end;

//=== { TJvDatabasePriorBlockAction } ========================================

constructor TJvDatabasePriorBlockAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlockSize := 50;
end;

procedure TJvDatabasePriorBlockAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DatabaseControlEngine) and not EngineControlsDisabled and EngineIsActive and
    not EngineBof and EngineCanNavigate);
end;

procedure TJvDatabasePriorBlockAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  try
    DatabaseControlEngine.DisableControls(DataComponent);
    DatabaseControlEngine.MoveBy(DataComponent, -BlockSize);
  finally
    DatabaseControlEngine.EnableControls(DataComponent);
  end;
end;

//=== { TJvDatabaseNextBlockAction } =========================================

constructor TJvDatabaseNextBlockAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlockSize := 50;
end;

procedure TJvDatabaseNextBlockAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DatabaseControlEngine) and not EngineControlsDisabled and EngineIsActive and
    not EngineEof and EngineCanNavigate);
end;

procedure TJvDatabaseNextBlockAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  try
    DatabaseControlEngine.DisableControls(DataComponent);
    DatabaseControlEngine.MoveBy(DataComponent, BlockSize);
  finally
    DatabaseControlEngine.EnableControls(DataComponent);
  end;
end;

//=== { TJvDatabaseRefreshAction } ===========================================

constructor TJvDatabaseRefreshAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRefreshLastPosition := True;
  FRefreshAsOpenClose := False;
end;

procedure TJvDatabaseRefreshAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  Refresh;
end;

procedure TJvDatabaseRefreshAction.Refresh;
var
  MyBookmark: TBookmark;
begin
  if Assigned(Dataset) then
  begin
    MyBookmark := nil;
    if RefreshLastPosition then
      MyBookmark := Dataset.GetBookmark;

    try
      if RefreshAsOpenClose then
      begin
        Dataset.Close;
        Dataset.Open;
      end
      else
        Dataset.Refresh;

      if RefreshLastPosition then
        if Dataset.Active then
          if Assigned(MyBookmark) then
            if Dataset.BookmarkValid(MyBookmark) then
            try
              Dataset.GotoBookmark(MyBookmark);
            except
            end;
    finally
      if RefreshLastPosition then
        Dataset.FreeBookmark(MyBookmark);
    end;
  end;
end;

//=== { TJvDatabaseBaseActiveAction } ========================================

procedure TJvDatabaseRefreshAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and EngineCanRefresh);
end;

//=== { TJvDatabasePositionAction } ==========================================

constructor TJvDatabasePositionAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowSelectedRows := True;
  FMinCountSelectedRows := 2;
end;

procedure TJvDatabasePositionAction.UpdateTarget(Target: TObject);
var
  RecCount : Integer;
  SelCount : Integer;
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and EngineHasData and EngineCanNavigate);
  try
    if not EngineIsActive then
      SetCaption(RsDBPosPositionInactive)
    else
    begin
      RecCount := EngineRecordCount;
      if RecCount = 0 then
        SetCaption(Format(RsDBPosPositionNormal, [0, 0]))
      else
      begin
        SelCount := EngineSelectedRowsCount;
        if ShowSelectedRows and (SelCount >= MinCountSelectedRows) then
          SetCaption(Format(RsDBPosPositionSelected, [EngineRecNo, RecCount, SelCount]))
        else
          SetCaption(Format(RsDBPosPositionNormal, [EngineRecNo, RecCount]));
      end;
    end;
  except
    SetCaption(RsDBPosPositionInactive);
  end;
end;

procedure TJvDatabasePositionAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  ShowPositionDialog;
end;

procedure TJvDatabasePositionAction.SetCaption(Value: string);
begin
  if Value <> Caption then
  {$IFDEF RTL240_UP}
    inherited SetCaption (Value);
  {$ELSE}
    Caption := Value;
  {$ENDIF RTL240_UP}
end;

procedure TJvDatabasePositionAction.ShowPositionDialog;
const
  cCurrentPosition = 'CurrentPosition';
  cNewPosition = 'NewPosition';
  cKind = 'Kind';
var
  ParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
  S: string;
  Kind: Integer;
begin
  if not Assigned(DataSet) then
    Exit;
  ParameterList := TJvParameterList.Create(Self);
  try
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    Parameter.SearchName := cCurrentPosition;
    Parameter.ReadOnly := True;
    Parameter.Caption := RsDBPosCurrentPosition;
    Parameter.AsString := IntToStr(EngineRecNo + 1) + ' / ' + IntToStr(EngineRecordCount);
    Parameter.Width := 150;
    TJvEditParameter(Parameter).LabelWidth := 80;
    Parameter.Enabled := False;
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    Parameter.Caption := RsDBPosNewPosition;
    Parameter.SearchName := cNewPosition;
    Parameter.Width := 150;
    TJvEditParameter(Parameter).LabelWidth := 80;
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvRadioGroupParameter.Create(ParameterList));
    Parameter.Caption := RsDBPosMovementType;
    Parameter.SearchName := cKind;
    Parameter.Width := 305;
    Parameter.Height := 54;
    TJvRadioGroupParameter(Parameter).Columns := 2;
    TJvRadioGroupParameter(Parameter).ItemList.Add(RsDBPosAbsolute);
    TJvRadioGroupParameter(Parameter).ItemList.Add(RsDBPosForward);
    TJvRadioGroupParameter(Parameter).ItemList.Add(RsDBPosBackward);
    TJvRadioGroupParameter(Parameter).ItemList.Add(RsDBPosPercental);
    TJvRadioGroupParameter(Parameter).ItemIndex := 0;
    ParameterList.AddParameter(Parameter);
    ParameterList.ArrangeSettings.WrapControls := True;
    ParameterList.ArrangeSettings.MaxWidth := 350;
    ParameterList.Messages.Caption := RsDBPosDialogCaption;
    if ParameterList.ShowParameterDialog then
    begin
      S := ParameterList.ParameterByName(cNewPosition).AsString;
      if S = '' then
        Exit;
      Kind := TJvRadioGroupParameter(ParameterList.ParameterByName(cKind)).ItemIndex;
      DataSet.DisableControls;
      try
        case Kind of
          0:
            begin
              DataSet.First;
              DataSet.MoveBy(StrToInt(S) - 1);
            end;
          1:
            DataSet.MoveBy(StrToInt(S));
          2:
            DataSet.MoveBy(StrToInt(S) * -1);
          3:
            begin
              DataSet.First;
              DataSet.MoveBy(Round((EngineRecordCount / 100.0) * StrToInt(S)) - 1);
            end;
        end;
      finally
        DataSet.EnableControls;
      end;
    end;
  finally
    ParameterList.Free;
  end;
end;

//=== { TJvDatabaseInsertAction } ============================================

constructor TJvDatabaseInsertAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInsertType := itInsert;
end;

procedure TJvDatabaseInsertAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and
    EngineIsActive and EngineCanInsert and not EngineEditModeActive);
end;

procedure TJvDatabaseInsertAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  if Assigned(SingleRecordWindowAction) then
  begin
    try
      FSingleRecordWindowAction.OnFormShow := SingleRecordOnFormShowEvent;
      FSingleRecordWindowAction.Execute;
    finally
      FSingleRecordWindowAction.OnFormShow := nil;
    end;
  end
  else
    SingleRecordOnFormShowEvent(nil, nil);
end;

procedure TJvDatabaseInsertAction.Notification(AComponent: TComponent;
    Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FSingleRecordWindowAction then
      SingleRecordWindowAction := nil;
end;

procedure TJvDatabaseInsertAction.SetSingleRecordWindowAction(const Value: TJvDatabaseSingleRecordWindowAction);
begin
  ReplaceComponentReference(Self, Value, TComponent(FSingleRecordWindowAction));
end;

procedure TJvDatabaseInsertAction.SingleRecordOnFormShowEvent(ADatacomponent : TComponent; ADynControlEngineDB:
    TJvDynControlEngineDB);
begin
  if InsertType = itAppend then
    DataSet.Append
  else
    DataSet.Insert;
end;

//=== { TJvDatabaseCopyAction } ==============================================

constructor TJvDatabaseCopyAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInsertType := itInsert;
end;

procedure TJvDatabaseCopyAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and
    EngineCanInsert and EngineHasData and not EngineEditModeActive);
end;

procedure TJvDatabaseCopyAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  if Assigned(SingleRecordWindowAction) then
  begin
    try
      FSingleRecordWindowAction.OnFormShow := SingleRecordOnFormShowEvent;
      FSingleRecordWindowAction.Execute;
    finally
      FSingleRecordWindowAction.OnFormShow := nil;
    end;
  end
  else
    SingleRecordOnFormShowEvent(nil, nil);
end;

procedure TJvDatabaseCopyAction.CopyRecord;
var
  Values: array of Variant;
  I: Integer;
  Value: Variant;
  Allowed: Boolean;
begin
  if not DataSet.Active then
    Exit;
  if DataSet.State in [dsInsert, dsEdit] then
    DataSet.Post;
  if DataSet.State <> dsBrowse then
    Exit;
  Allowed := True;
  if Assigned(FBeforeCopyRecord) then
    FBeforeCopyRecord(DataSet, Allowed);
  if not Allowed then
    Exit;
  SetLength(Values, DataSet.FieldCount);
  for I := 0 to DataSet.FieldCount - 1 do
    Values[I] := DataSet.Fields[I].AsVariant;
  if InsertType = itAppend then
    DataSet.Append
  else
    DataSet.Insert;
  if Assigned(FOnCopyRecord) then
    for I := 0 to DataSet.FieldCount - 1 do
    begin
      Value := Values[I];
      FOnCopyRecord(DataSet.Fields[I], Value);
    end
  else
    for I := 0 to DataSet.FieldCount - 1 do
      if (Dataset.Fields[i].FieldName <> 'ROWID') and
         (not Dataset.Fields[i].ReadOnly or Dataset.Fields[i].Required) then
        DataSet.Fields[I].AsVariant := Values[I];
  if Assigned(FAfterCopyRecord) then
    FAfterCopyRecord(DataSet);
end;

procedure TJvDatabaseCopyAction.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FSingleRecordWindowAction then
      SingleRecordWindowAction := nil;
end;

procedure TJvDatabaseCopyAction.SetSingleRecordWindowAction(const Value: TJvDatabaseSingleRecordWindowAction);
begin
  ReplaceComponentReference(Self, Value, TComponent(FSingleRecordWindowAction));
end;

procedure TJvDatabaseCopyAction.SingleRecordOnFormShowEvent(ADatacomponent : TComponent; ADynControlEngineDB:
    TJvDynControlEngineDB);
begin
  CopyRecord;
end;

//=== { TJvDatabaseEditAction } ==============================================

procedure TJvDatabaseEditAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and
    EngineCanUpdate and EngineHasData and not EngineEditModeActive);
end;

procedure TJvDatabaseEditAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  if Assigned(SingleRecordWindowAction) then
  begin
    try
      FSingleRecordWindowAction.OnFormShow := SingleRecordOnFormShowEvent;
      FSingleRecordWindowAction.Execute;
    finally
      FSingleRecordWindowAction.OnFormShow := nil;
    end;
  end
  else
    SingleRecordOnFormShowEvent(nil, nil);
end;

procedure TJvDatabaseEditAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FSingleRecordWindowAction then
      SingleRecordWindowAction := nil;
end;

procedure TJvDatabaseEditAction.SetSingleRecordWindowAction(const Value: TJvDatabaseSingleRecordWindowAction);
begin
  ReplaceComponentReference(Self, Value, TComponent(FSingleRecordWindowAction));
end;

procedure TJvDatabaseEditAction.SingleRecordOnFormShowEvent(ADatacomponent : TComponent; ADynControlEngineDB:
    TJvDynControlEngineDB);
begin
  DataSet.Edit;
end;

//=== { TJvDatabaseDeleteAction } ============================================

procedure TJvDatabaseDeleteAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and
    EngineCanDelete and EngineHasData and not EngineEditModeActive);
end;

procedure TJvDatabaseDeleteAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  DataSet.Delete;
end;

//=== { TJvDatabasePostAction } ==============================================

procedure TJvDatabasePostAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and EngineEditModeActive);
end;

procedure TJvDatabasePostAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  DataSet.Post;
end;

//=== { TJvDatabaseCancelAction } ============================================

procedure TJvDatabaseCancelAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and EngineEditModeActive);
end;

procedure TJvDatabaseCancelAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  DataSet.Cancel;
end;

//=== { TJvDatabaseSingleRecordWindowAction } ================================

constructor TJvDatabaseSingleRecordWindowAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TJvShowSingleRecordWindowOptions.Create;
end;

destructor TJvDatabaseSingleRecordWindowAction.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

procedure TJvDatabaseSingleRecordWindowAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  ShowSingleRecordWindow;
end;

procedure TJvDatabaseSingleRecordWindowAction.ShowSingleRecordWindow;
begin
  DatabaseControlEngine.ShowSingleRecordWindow(DataComponent, Options, onCreateDataControlsEvent, OnFormShow);
end;

//=== { TJvDatabaseOpenAction } ==============================================

procedure TJvDatabaseOpenAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineIsActive);
end;

procedure TJvDatabaseOpenAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  DataSet.Open;
end;

//=== { TJvDatabaseCloseAction } =============================================

procedure TJvDatabaseCloseAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and EngineIsActive and not EngineEditModeActive);
end;

procedure TJvDatabaseCloseAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  DataSet.Close;
end;

{$IFDEF USE_3RDPARTY_SMEXPORT}

//=== { TJvDatabaseSMExportOptions } =========================================

constructor TJvDatabaseSMExportOptions.Create;
var
  Fmt: TTableTypeExport;
  Option: TSMOption;
begin
  inherited Create;
  FFormats := [];
  for Fmt := Low(Fmt) to High(Fmt) do
    FFormats := FFormats + [Fmt];
  FOptions := [];
  for Option := Low(Option) to High(Option) do
    FOptions := FOptions + [Option];
  //  FDataFormats := TSMEDataFormats.Create;
end;

destructor TJvDatabaseSMExportOptions.Destroy;
begin
  //  FreeAndNil(FDataFormats);
  inherited Destroy;
end;

procedure TJvDatabaseSMExportOptions.SetDefaultOptionsDirectory(const Value: string);
begin
  FDefaultOptionsDirectory := trim(Value);
  if (FDefaultOptionsDirectory <> '') then
    FDefaultOptionsDirectory := PathAddSeparator(FDefaultOptionsDirectory);
end;

procedure TJvDatabaseSMExportOptions.SMEWizardDlgGetCellParams(Sender: TObject; Field: TField; var Text: TSMEString;
    AFont: TFont; var Alignment: TAlignment; var Background: TColor; var CellType: TCellType);
const
  SToDateFormatLong = 'TO_DATE(''%s'', ''DD.MM.YYYY HH24:MI:SS'')';
  SToDateFormatShort = 'TO_DATE(''%s'', ''DD.MM.YYYY'')';
  SFormatLong = 'dd.mm.yyyy hh:nn:ss';
  SFormatShort = 'dd.mm.yyyy';
  SNull = 'NULL';
var
  DT: TDateTime;
begin
  if Sender is TSMExportToSQL then
    if Assigned(Field) then
    begin
      if Field.IsNull or (Field.AsString = '') then
      begin
        Text := SNull;
        CellType := ctBlank;
      end
      else
        case Field.DataType of
          ftFloat, ftBCD, ftCurrency:
            Text := AnsiReplaceStr(Text, ',', '.');
          ftDate, ftDateTime:
            begin
              DT := Field.AsDateTime;
              if DT <= 0 then
                Text := SNull
              else
                if DT = Trunc(DT) then
                  Text := Format(SToDateFormatShort, [FormatDateTime(SFormatShort, DT)])
                else
                  Text := Format(StoDateFormatLong, [FormatDateTime(SFormatLong, DT)]);
              CellType := ctBlank;
            end;
        end;
    end
    else
      if Text = '' then
      begin
        Text := SNull;
        CellType := ctBlank;
      end
      else
        case CellType of
          ctDouble, ctCurrency:
            Text := AnsiReplaceStr(Text, ',', '.');
          ctDateTime, ctDate, ctTime:
            begin
              try
                DT := VarToDateTime(Text);
                if DT <= 0 then
                  Text := SNull
                else
                  if DT = Trunc(DT) then
                    Text := Format(SToDateFormatShort, [FormatDateTime(SFormatShort, DT)])
                  else
                    Text := Format(StoDateFormatLong, [FormatDateTime(SFormatLong, DT)]);
              except
                Text := Format(StoDateFormatLong, [Text]);
              end;
              CellType := ctBlank;
            end;
        end;
end;

procedure TJvDatabaseSMExportOptions.SMEWizardDlgOnBeforeExecute(Sender: TObject);
begin
  if Assigned(FOnBeforeExecuteExport) then
    FOnBeforeExecuteExport(Sender)
  else
    if Sender is TSMExportToSQL then
      TSMExportToSQL(Sender).SQLQuote := '''';
end;

//=== { TJvDatabaseSMExportAction } ==========================================

constructor TJvDatabaseSMExportAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TJvDatabaseSMExportOptions.Create;
end;

destructor TJvDatabaseSMExportAction.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

procedure TJvDatabaseSMExportAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  ExportData;
end;

procedure TJvDatabaseSMExportAction.ExportData;
const
  cLastExport = '\Last Export.SME';
var
  SMEWizardDlg: TSMEWizardDlg;
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  SMEEngineCx: TSMEcxCustomGridTableViewDataEngine;
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
begin
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  SMEEngineCx := nil;
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  SMEWizardDlg := TSMEWizardDlg.Create(nil);
  try
    SMEWizardDlg.ColumnSource := csDataSet;
    SMEWizardDlg.OnGetCellParams := Options.SMEWizardDlgGetCellParams;
    SMEWizardDlg.OnBeforeExecute := Options.SMEWizardDlgOnBeforeExecute;
    SMEWizardDlg.OnAfterExecute := Options.OnAfterExecuteExport;
    SMEWizardDlg.DataSet := DataSource.DataSet;
    SMEWizardDlg.Title := Options.Title;
    SMEWizardDlg.KeyGenerator := Options.Title;
    SMEWizardDlg.WizardStyle := smewiz.wsWindows2000;
    if (Options.DefaultOptionsDirectory <> '') and DirectoryExists(Options.DefaultOptionsDirectory) then
      SMEWizardDlg.SpecificationDir := ExcludeTrailingPathDelimiter(Options.DefaultOptionsDirectory)
    else
      SMEWizardDlg.SpecificationDir := GetCurrentDir;
    if DataComponent is TCustomDBGrid then
    begin
      SMEWizardDlg.DBGrid := TCustomControl(DataComponent);
      SMEWizardDlg.ColumnSource := csDBGrid;
    end
    {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
    else
      if (DataComponent is TcxGrid) and (TcxGrid(DataComponent).FocusedView is TcxCustomGridTableView) then
      begin
        SMEEnginecx := TSMEcxCustomGridTableViewDataEngine.Create(Self);
        SMEEngineCx.cxCustomGridTableView := TcxCustomGridTableView(TcxGrid(DataComponent).FocusedView);
        SMEWizardDlg.DataEngine := SMEEngineCx;
        SMEWizardDlg.ColumnSource := csDataEngine;
      end
      else
        if DataComponent is TcxCustomGridTableView then
        begin
          SMEEnginecx := TSMEcxCustomGridTableViewDataEngine.Create(Self);
          SMEEngineCx.cxCustomGridTableView := TcxCustomGridTableView(DataComponent);
          SMEWizardDlg.DataEngine := SMEEngineCx;
          SMEWizardDlg.ColumnSource := csDataEngine;
        end
    {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
        else
        begin
          SMEWizardDlg.DataSet := DataSet;
          SMEWizardDlg.ColumnSource := csDataSet;
        end;

    SMEWizardDlg.Formats := Options.Formats;
    SMEWizardDlg.Options := Options.Options;
    SMEWizardDlg.HelpContext := Options.HelpContext;
    if FileExists(SMEWizardDlg.SpecificationDir + cLastExport) then
      SMEWizardDlg.LoadSpecification(SMEWizardDlg.SpecificationDir + cLastExport);
    SMEWizardDlg.Execute;
    SMEWizardDlg.SaveSpecification('Last Export', SMEWizardDlg.SpecificationDir + cLastExport, False);
  finally
    FreeAndNil(SMEWizardDlg);
    {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
    FreeAndNil(SMEEngineCx);
    {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  end;
end;

{$ENDIF USE_3RDPARTY_SMEXPORT}

{$IFDEF USE_3RDPARTY_SMIMPORT}

//=== { TJvDatabaseSMImportOptions } =========================================

constructor TJvDatabaseSMImportOptions.Create;
var
  Fmt: TTableTypeImport;
  Option: TSMIOption;
begin
  inherited Create;
  FFormats := [];
  for Fmt := Low(Fmt) to High(Fmt) do
    FFormats := FFormats + [Fmt];
  FOptions := [];
  for Option := Low(Option) to High(Option) do
    FOptions := FOptions + [Option];
end;

procedure TJvDatabaseSMImportOptions.SetDefaultOptionsDirectory(const Value: string);
begin
  FDefaultOptionsDirectory := trim(Value);
  if (FDefaultOptionsDirectory <> '') then
    FDefaultOptionsDirectory := PathAddSeparator(FDefaultOptionsDirectory);
end;

//=== { TJvDatabaseSMImportAction } ==========================================

constructor TJvDatabaseSMImportAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TJvDatabaseSMImportOptions.Create;
end;

destructor TJvDatabaseSMImportAction.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

procedure TJvDatabaseSMImportAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  ImportData;
end;

procedure TJvDatabaseSMImportAction.ImportData;
var
  SMIWizardDlg: TSMIWizardDlg;
begin
  SMIWizardDlg := TSMIWizardDlg.Create(Self);
  try
    //    SMIWizardDlg.OnGetSpecifications := Options.SMIWizardDlgGetSpecifications;
    if (Options.DefaultOptionsDirectory <> '') and DirectoryExists(Options.DefaultOptionsDirectory) then
      SMIWizardDlg.SpecificationDir := ExcludeTrailingPathDelimiter(Options.DefaultOptionsDirectory)
    else
      SMIWizardDlg.SpecificationDir := GetCurrentDir;
    SMIWizardDlg.DataSet := DataSource.DataSet;
    SMIWizardDlg.Title := Options.Title;
    SMIWizardDlg.Formats := Options.Formats;
    SMIWizardDlg.HelpContext := Options.HelpContext;
    SMIWizardDlg.WizardStyle := Options.WizardStyle;
    SMIWizardDlg.Options := Options.Options;
    //    IF FileExists (Options.DefaultOptionsDirectory+'\Last Import.SMI') THEN
    //      SMIWizardDlg.LoadSpecification(Options.DefaultOptionsDirectory+'\Last Import.SMI');
    SMIWizardDlg.Execute;
    SMIWizardDlg.SaveSpecification('Last Import', SMIWizardDlg.SpecificationDir + '\Last Import.SMI', False);
  finally
    FreeAndNil(SMIWizardDlg);
  end;
end;

{$ENDIF USE_3RDPARTY_SMIMPORT}

//=== { TJvDatabaseModifyAllAction } =========================================

constructor TJvDatabaseModifyAllAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabledOnlyIfSelectedRows := True;
end;

procedure TJvDatabaseModifyAllAction.ExecuteTarget(Target: TObject);
begin
  ModifyAll;
end;

procedure TJvDatabaseModifyAllAction.ModifyAll;
var
  JvParameterList: TJvParameterList;
  Parameter: TJvBaseParameter;
  I: Integer;
  Field: TField;
  FieldName: string;
  ChangeTo: string;
  ClearField: Boolean;
  OnlyIfNull: Boolean;
begin
  if not Assigned(DatabaseControlEngine) then
    Exit;
  JvParameterList := TJvParameterList.Create(Self);
  try
    JvParameterList.Messages.Caption := SModifyAllCaption;
    JvParameterList.Messages.OkButton := SModifyAllOkButton;
    Parameter := TJvBaseParameter(TJvComboBoxParameter.Create(JvParameterList));
    TJvComboBoxParameter(Parameter).LabelArrangeMode := lamAbove;
    Parameter.SearchName := 'ModifyField';
    Parameter.Caption := SModifyAllModifyField;
    Parameter.Width := 330;
    for I := 0 to EngineFieldCount - 1 do
    begin
      Field := DatabaseControlEngine.FieldById(DataComponent, I);
      if Assigned(Field) then
        if not DatabaseControlEngine.IsFieldReadOnly(DataComponent,Field.FieldName) and
          DatabaseControlEngine.IsFieldVisible(DataComponent,Field.FieldName) then
          TJvComboBoxParameter(Parameter).ItemList.Add(Field.FieldName);
      if Assigned(DatabaseControlEngine.SelectedField(DataComponent)) then
        TJvComboBoxParameter(Parameter).ItemIndex :=
           TJvComboBoxParameter(Parameter).ItemList.IndexOf(DatabaseControlEngine.SelectedField(DataComponent).FieldName);
      if (TJvComboBoxParameter(Parameter).ItemIndex < 0) or
         (TJvComboBoxParameter(Parameter).ItemIndex >= TJvComboBoxParameter(Parameter).ItemList.Count) then
        TJvComboBoxParameter(Parameter).ItemIndex := 0;
    end;
    JvParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(JvParameterList);
    Parameter.SearchName := 'ClearFieldValues';
    Parameter.Caption := SModifyAllClearFieldValues;
    Parameter.Width := 150;
    JvParameterList.AddParameter(Parameter);
    Parameter := TJvEditParameter.Create(JvParameterList);
    Parameter.SearchName := 'ChangeTo';
    Parameter.Caption := SModifyAllChangeTo;
    Parameter.Width := 330;
    TJvEditParameter(Parameter).LabelArrangeMode := lamAbove;
    Parameter.DisableReasons.AddReason('ClearFieldValues', True);
    JvParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(JvParameterList);
    Parameter.SearchName := 'OnlyIfNull';
    Parameter.Caption := SModifyAllOnlyIfNull;
    Parameter.Width := 150;
    Parameter.DisableReasons.AddReason('ClearFieldValues', True);
    JvParameterList.AddParameter(Parameter);
    JvParameterList.MaxWidth := 360;
    if JvParameterList.ShowParameterDialog then
    begin
      FieldName := JvParameterList.ParameterByName('ModifyField').AsString;
      ClearField := JvParameterList.ParameterByName('ClearFieldValues').AsBoolean;
      OnlyIfNull := JvParameterList.ParameterByName('OnlyIfNull').AsBoolean;
      ChangeTo := JvParameterList.ParameterByName('ChangeTo').AsString;
      Field := DatabaseControlEngine.FieldByName(DataComponent, FieldName);
      if Assigned(Field) then
      try
        DatabaseControlEngine.DisableControls(DataComponent);
        for I := 0 to DatabaseControlEngine.SelectedRowsCount(DataComponent) - 1 do
          if DatabaseControlEngine.GotoSelectedRow(DataComponent,I) then
          begin
            try
              if (ClearField and not Field.IsNull) or
                not (OnlyIfNull and not Field.IsNull) then
              begin
                DatabaseControlEngine.Dataset(DataComponent).Edit;
                if ClearField then
                  Field.Clear
                else
                  Field.AsString := ChangeTo;
                if Assigned(DatabaseControlEngine.Dataset(DataComponent)) then
                  DatabaseControlEngine.Dataset(DataComponent).Post;
              end;
            except
              on E: Exception do
              begin
                if Assigned(DatabaseControlEngine.Dataset(DataComponent)) then
                  DatabaseControlEngine.Dataset(DataComponent).Cancel;
                JvDSADialogs.MessageDlg(E.Message, mtError, [mbOK], 0);
              end;
            end;
          end;
      finally
        DatabaseControlEngine.EnableControls(DataComponent);
      end;
    end;
  finally
    FreeAndNil(JvParameterList);
  end;
end;

procedure TJvDatabaseModifyAllAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and
    EngineIsActive and EngineCanUpdate and not EngineEditModeActive and
    (not EnabledOnlyIfSelectedRows or (EngineSelectedRowsCount > 1)));
end;

//=== { TJvDatabaseShowSQLStatementAction } ==================================

constructor TJvDatabaseShowSQLStatementAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWordWrap := True;
  FShowWordWrapCheckBox := False;
  CheckBoxParameter := nil;
  MemoParameter := nil;
end;

procedure TJvDatabaseShowSQLStatementAction.CheckBoxOnChange(Sender: TObject);
begin
  if Assigned(MemoParameter) and Assigned(CheckBoxParameter) then
  begin
    CheckBoxParameter.GetData;
    MemoParameter.WordWrap := CheckBoxParameter.AsBoolean;
    if CheckBoxParameter.AsBoolean then
      MemoParameter.ScrollBars := ssVertical
    else
      MemoParameter.ScrollBars := ssBoth;
  end;
end;

procedure TJvDatabaseShowSQLStatementAction.ExecuteTarget(Target: TObject);
begin
  ShowSQLStatement;
end;

procedure TJvDatabaseShowSQLStatementAction.ShowSQLStatement;
var
  ParameterList: TJvParameterList;
begin
  if not Assigned(DatasetEngine) then
    Exit;
  ParameterList := TJvParameterList.Create(Self);
  try
    MemoParameter := TJvMemoParameter.Create(ParameterList);
    MemoParameter.SearchName := 'SQLStatement';
    if Self.WordWrap then
      MemoParameter.ScrollBars := ssVertical
    else
      MemoParameter.ScrollBars := ssBoth;
    MemoParameter.WordWrap := Self.WordWrap;
    MemoParameter.ReadOnly := True;
    MemoParameter.AsString := DatasetEngine.GetSQL(DataSet);
    MemoParameter.Width := 450;
    MemoParameter.Height := 350;
    ParameterList.AddParameter(MemoParameter);
    if ShowWordWrapCheckBox then
    begin
      CheckboxParameter := TJvCheckboxParameter.Create(ParameterList);
      CheckboxParameter.SearchName := 'CheckBox';
      CheckBoxParameter.AsBoolean := Self.WordWrap;
      CheckboxParameter.Width:= 300;
      CheckboxParameter.OnChange := CheckboxOnChange;
      CheckBoxParameter.Caption := SSQLStatementWordWrapped;
      ParameterList.AddParameter(CheckboxParameter);
    end;
    ParameterList.ArrangeSettings.WrapControls := True;
    ParameterList.ArrangeSettings.MaxWidth := 650;
    ParameterList.MaxHeight := 650;
    ParameterList.Messages.Caption := SShowSQLStatementCaption;
    ParameterList.Messages.OkButton := SSQLStatementClipboardButton;
    if ParameterList.ShowParameterDialog then
      ClipBoard.AsText := MemoParameter.AsString;
  finally
    FreeAndNil(ParameterList);
    CheckBoxParameter := nil;
    MemoParameter := nil;
  end;
end;

procedure TJvDatabaseShowSQLStatementAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and Assigned(DatasetEngine) and DatasetEngine.SupportsGetSQL(DataComponent));
end;

procedure TJvDatabaseRefreshRecordAction.ExecuteTarget(Target: TObject);
begin
  inherited ExecuteTarget(Target);
  if Assigned(DatasetEngine) then
    DatasetEngine.RefreshRecord (DataSet);
end;

//=== { TJvDatabaseBaseActiveAction } ========================================

procedure TJvDatabaseRefreshRecordAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and EngineCanRefreshRecord);
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
