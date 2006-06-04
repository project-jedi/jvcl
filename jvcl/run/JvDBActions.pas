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
located at http://jvcl.sourceforge.net

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
{$IFDEF MSWINDOWS}
  Windows, ActnList, ImgList, Graphics,
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
  QActnList, QWindows, QImgList, QGraphics,
{$ENDIF UNIX}
  Forms, Controls, Classes, DB,
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGridCustomTableView, cxDBData,
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
{$IFDEF USE_3RDPARTY_SMEXPORT}
  SMEWIZ, ExportDS, SMEEngine,
{$ENDIF USE_3RDPARTY_SMEXPORT}
{$IFDEF USE_3RDPARTY_SMIMPORT}
  SMIWiz, SMIBase,
{$ENDIF USE_3RDPARTY_SMIMPORT}
  DBGrids, JvDBActionsEngine;

type
  TJvChangeDataComponent = procedure(DataComponent: TComponent) of object;

  TJvDatabaseActionList = class(TActionList)
  private
    FDataComponent: TComponent;
    FOnChangeDataComponent: TJvChangeDataComponent;
  protected
    procedure SetDataComponent(Value: TComponent);
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property DataComponent: TComponent read FDataComponent write SetDataComponent;
    property OnChangeDataComponent: TJvChangeDataComponent read
      FOnChangeDataComponent write FOnChangeDataComponent;
  end;

  TJvDatabaseActionBaseEngineClass = class of TJvDatabaseActionBaseControlEngine;

  TJvDatabaseExecuteEvent = procedure(Sender: TObject; ControlEngine: TJvDatabaseActionBaseControlEngine;
    DataComponent: TComponent) of object;
  TJvDatabaseExecuteDataSourceEvent = procedure(Sender: TObject; DataSource: TDataSource) of object;

  TJvDatabaseBaseAction = class(TAction)
  private
    FOnExecute: TJvDatabaseExecuteEvent;
    FOnExecuteDataSource: TJvDatabaseExecuteDataSourceEvent;
    FControlEngine: TJvDatabaseActionBaseControlEngine;
    FDatasetEngine: TJvDatabaseActionBaseDatasetEngine;
    FDataComponent: TComponent;
    FOnChangeDataComponent: TJvChangeDataComponent;
  protected
    procedure SetDataComponent(Value: TComponent);
    procedure SetEnabled(Value: boolean);
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function EngineIsActive: boolean;
    function EngineHasData: boolean;
    function EngineFieldCount: integer;
    function EngineRecordCount: integer;
    function EngineRecNo: integer;
    function EngineCanInsert: boolean;
    function EngineCanUpdate: boolean;
    function EngineCanDelete: boolean;
    function EngineEof: boolean;
    function EngineBof: boolean;
    function EngineControlsDisabled: boolean;
    function EngineEditModeActive: boolean;
    function EngineSelectedRowsCount: integer;
    property ControlEngine: TJvDatabaseActionBaseControlEngine read FControlEngine;
    property DatasetEngine: TJvDatabaseActionBaseDatasetEngine read FDatasetEngine;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property DataSource: TDataSource read GetDataSource;
    property DataSet: TDataSet read GetDataSet;
  published
    property OnChangeDataComponent: TJvChangeDataComponent read
      FOnChangeDataComponent write FOnChangeDataComponent;
    property OnExecute: TJvDatabaseExecuteEvent read FOnExecute write FOnExecute;
    property OnExecuteDataSource: TJvDatabaseExecuteDataSourceEvent
      read FOnExecuteDataSource write FOnExecuteDataSource;
    property DataComponent: TComponent read FDataComponent write SetDataComponent;
  end;

  TJvDatabaseSimpleAction = class(TJvDatabaseBaseAction)
  private
    FIsActive: boolean;
    FHasData: boolean;
    FCanInsert: boolean;
    FCanUpdate: boolean;
    FCanDelete: boolean;
    FEditModeActive: boolean;
    FManualEnabled: Boolean;
    procedure SetManualEnabled(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    // If this paramater is active, the Action will be enabled if for the datacomponent-dataset is active
    property IsActive: boolean read FIsActive write FIsActive default True;
    // If this paramater is active, the Action will be enabled if for the datacomponent-dataset contains records
    property HasData: boolean read FHasData write FHasData default True;
    // If this paramater is active, the Action will be enabled if insert is allowed for the datacomponent-dataset
    property CanInsert: boolean read FCanInsert write FCanInsert default False;
    // If this paramater is active, the Action will be enabled if update is allowed for the datacomponent-dataset
    property CanUpdate: boolean read FCanUpdate write FCanUpdate default False;
    // If this paramater is active, the Action will be enabled if delete is allowed for the datacomponent-dataset
    property CanDelete: boolean read FCanDelete write FCanDelete default False;
    // If this paramater is active, the Action will be enabled if the datacomponent-dataset is in edit mode
    property EditModeActive: boolean read FEditModeActive write FEditModeActive default False;
    // This property allows you enable / disable the action independently from the
    // automatic handling by IsActive, HasData, CanInsert, CanUpdate, EditModeActive
    property ManualEnabled: Boolean read FManualEnabled write SetManualEnabled
      default True;
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
    FBlockSize: integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BlockSize: integer read FBlockSize write FBlockSize default 50;
  end;

  TJvDatabaseNextBlockAction = class(TJvDatabaseBaseNavigateAction)
  private
    FBlockSize: integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BlockSize: integer read FBlockSize write FBlockSize default 50;
  end;

  TJvDatabaseRefreshAction = class(TJvDatabaseBaseActiveAction)
  private
    FRefreshLastPosition: boolean;
    FRefreshAsOpenClose: boolean;
  protected
    procedure Refresh;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property RefreshLastPosition: boolean read FRefreshLastPosition write FRefreshLastPosition default True;
    property RefreshAsOpenClose: boolean read FRefreshAsOpenClose write FRefreshAsOpenClose default False;
  end;

  TJvDatabasePositionAction = class(TJvDatabaseBaseNavigateAction)
  private
    FMinCountSelectedRows: Integer;
    FShowSelectedRows: Boolean;
  protected
    procedure SetCaption(Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowPositionDialog;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property MinCountSelectedRows: Integer read FMinCountSelectedRows write
        FMinCountSelectedRows default 2;
    property ShowSelectedRows: Boolean read FShowSelectedRows write
      FShowSelectedRows default True;
  end;

  TJvDatabaseInsertAction = class(TJvDatabaseBaseEditAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabaseOnCopyRecord = procedure(Field: TField; OldValue: variant) of object;
  TJvDatabaseBeforeCopyRecord = procedure(DataSet: TDataSet; var RefreshAllowed: boolean) of object;
  TJvDatabaseAfterCopyRecord = procedure(DataSet: TDataSet) of object;

  TJvDatabaseCopyAction = class(TJvDatabaseBaseEditAction)
  private
    FBeforeCopyRecord: TJvDatabaseBeforeCopyRecord;
    FAfterCopyRecord: TJvDatabaseAfterCopyRecord;
    FOnCopyRecord: TJvDatabaseOnCopyRecord;
  public
    procedure CopyRecord;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BeforeCopyRecord: TJvDatabaseBeforeCopyRecord read FBeforeCopyRecord write FBeforeCopyRecord;
    property AfterCopyRecord: TJvDatabaseAfterCopyRecord read FAfterCopyRecord write FAfterCopyRecord;
    property OnCopyRecord: TJvDatabaseOnCopyRecord read FOnCopyRecord write FOnCopyRecord;
  end;

  TJvDatabaseEditAction = class(TJvDatabaseBaseEditAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
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
    FOptions: TJvShowSingleRecordWindowOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure ShowSingleRecordWindow;
  published
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
  public
    constructor Create;
    destructor Destroy; override;
    procedure SMEWizardDlgGetCellParams(Sender: TObject; Field: TField; var Text: string;
      AFont: TFont; var Alignment: TAlignment; var Background: TColor; var CellType: TCellType);
    procedure SMEWizardDlgOnBeforeExecute(Sender: TObject);
  published
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property Formats: TExportFormatTypes read FFormats write FFormats;
    property Title: TCaption read FTitle write FTitle;
    property DefaultOptionsDirectory: string read FDefaultOptionsDirectory write FDefaultOptionsDirectory;
    property KeyGenerator: string read FKeyGenerator write FKeyGenerator;
    property Options: TSMOptions read FOptions write FOptions;
    property OnAfterExecuteExport: TNotifyEvent read FOnAfterExecuteExport write
        FOnAfterExecuteExport;
    property OnBeforeExecuteExport: TNotifyEvent read FOnBeforeExecuteExport write
        FOnBeforeExecuteExport;
  end;

  TJvDatabaseSMExportAction = class(TJvDatabaseBaseActiveAction)
  private
    FOptions: TJvDatabaseSMExportOptions;
  protected
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
  public
    constructor Create;
  published
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property Formats: TImportFormatTypes read FFormats write FFormats;
    property Title: TCaption read FTitle write FTitle;
    property DefaultOptionsDirectory: string read FDefaultOptionsDirectory write FDefaultOptionsDirectory;
    property Options: TSMIOptions read FOptions write FOptions;
    property WizardStyle: TSMIWizardStyle read FWizardStyle write FWizardStyle;
  end;

  TJvDatabaseSMImportAction = class(TJvDatabaseBaseEditAction)
  private
    FOptions: TJvDatabaseSMImportOptions;
  protected
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
    property EnabledOnlyIfSelectedRows: Boolean read FEnabledOnlyIfSelectedRows
      write FEnabledOnlyIfSelectedRows default True;
  end;

  TJvDatabaseShowSQLStatementAction = class(TJvDatabaseBaseActiveAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure ShowSQLStatement;
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
  SysUtils, Grids,
{$IFDEF HAS_UNIT_STRUTILS}
  StrUtils,
{$ENDIF HAS_UNIT_STRUTILS}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGrid, cxGridDBDataDefinitions,
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
{$IFDEF USE_3RDPARTY_SMEXPORT}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  SMEEngCx,
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  sme2sql, IniFiles,
{$ENDIF USE_3RDPARTY_SMEXPORT}
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxCustomData,
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  JvResources, JvParameterList, JvParameterListParameter, TypInfo,
  JvDSADialogs,
{$IFDEF HAS_UNIT_VARIANTS}
  Variants,
{$ENDIF HAS_UNIT_VARIANTS}
  Dialogs, StdCtrls, Clipbrd;


  //=== { TJvDatabaseActionList } ==============================================

procedure TJvDatabaseActionList.SetDataComponent(Value: TComponent);
var
  I: integer;
begin
  if Value <> FDataComponent then
  begin
    FDataComponent := Value;
    if FDataComponent <> nil then
      FDataComponent.FreeNotification(Self);
    for I := 0 to ActionCount - 1 do
      if Actions[I] is TJvDatabaseBaseAction then
        TJvDatabaseBaseAction(Actions[I]).DataComponent := Value;
    if Assigned(OnChangeDataComponent) then
      OnChangeDataComponent(Value);
  end;
end;

procedure TJvDatabaseActionList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FDataComponent then
      DataComponent := nil;
end;

//=== { TJvDatabaseBaseAction } ==============================================

constructor TJvDatabaseBaseAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(AOwner) and (AOwner is TJvDatabaseActionList) then
    DataComponent := TJvDatabaseActionList(AOwner).DataComponent;
end;

function TJvDatabaseBaseAction.GetDataSet: TDataSet;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.DataSet
  else
    Result := nil;
end;

function TJvDatabaseBaseAction.GetDataSource: TDataSource;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.DataSource
  else
    Result := nil;
end;

procedure TJvDatabaseBaseAction.SetDataComponent(Value: TComponent);
var EngineList : TJvDatabaseActionEngineList;
begin
  FDataComponent := Value;
  if FDataComponent <> nil then
    FDataComponent.FreeNotification(Self);
  EngineList := RegisteredDatabaseActionEngineList;
  if Assigned(EngineList) then
  begin
    FControlEngine := EngineList.GetControlEngine(FDataComponent);
    if Assigned(FControlEngine) then
    begin
      FControlEngine.Datacomponent := FDatacomponent;
      if Assigned(Dataset) then
      begin
        FDatasetEngine := EngineList.GetDatasetEngine(Dataset);
        if Assigned(FDatasetEngine) then
          FDatasetEngine.Datacomponent := Dataset;
      end;
    end
    else
      FDatasetEngine := nil;
  end
  else
  begin
    FControlEngine := nil;
    FDatasetEngine := nil;
  end;
  if Assigned(OnChangeDataComponent) then
    OnChangeDataComponent(Value);
end;

procedure TJvDatabaseBaseAction.SetEnabled(Value: boolean);
begin
  if Enabled <> Value then
    Enabled := Value;
end;

function TJvDatabaseBaseAction.EngineIsActive: boolean;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.IsActive
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineHasData: boolean;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.HasData
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineFieldCount: integer;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.FieldCount
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.EngineRecordCount: integer;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.RecordCount
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.EngineRecNo: integer;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.RecNo
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.EngineCanInsert: boolean;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.CanInsert
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineCanUpdate: boolean;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.CanUpdate
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineCanDelete: boolean;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.CanDelete
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineEof: boolean;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.EOF
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineBof: boolean;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.Bof
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineControlsDisabled: boolean;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.ControlsDisabled
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineEditModeActive: boolean;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.EditModeActive
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineSelectedRowsCount: integer;
begin
  if Assigned(ControlEngine) then
    Result := ControlEngine.SelectedRowsCount
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.HandlesTarget(Target: TObject): boolean;
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
    FOnExecute(Self, ControlEngine, DataComponent)
  else
    if Assigned(FOnExecuteDataSource) then
      FOnExecuteDataSource(Self, DataSource)
    else
      inherited ExecuteTarget(Target);
end;

procedure TJvDatabaseBaseAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDataComponent) then
    DataComponent := nil;
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
  Res: boolean;
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
  SetEnabled(Assigned(ControlEngine) and not EngineControlsDisabled and EngineIsActive and not EngineBof);
end;

procedure TJvDatabaseFirstAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  ControlEngine.First;
end;

//=== { TJvDatabaseLastAction } ==============================================

procedure TJvDatabaseLastAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(ControlEngine) and not EngineControlsDisabled and EngineIsActive and not EngineEof);
end;

procedure TJvDatabaseLastAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  ControlEngine.Last;
end;

//=== { TJvDatabasePriorAction } =============================================

procedure TJvDatabasePriorAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(ControlEngine) and not EngineControlsDisabled and EngineIsActive and not EngineBof);
end;

procedure TJvDatabasePriorAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  ControlEngine.MoveBy(-1);
end;

//=== { TJvDatabaseNextAction } ==============================================

procedure TJvDatabaseNextAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(ControlEngine) and not EngineControlsDisabled and EngineIsActive and not EngineEof);
end;

procedure TJvDatabaseNextAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  ControlEngine.MoveBy(1);
end;

//=== { TJvDatabasePriorBlockAction } ========================================

constructor TJvDatabasePriorBlockAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlockSize := 50;
end;

procedure TJvDatabasePriorBlockAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(ControlEngine) and not EngineControlsDisabled and EngineIsActive and not EngineBof);
end;

procedure TJvDatabasePriorBlockAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  with ControlEngine do
  try
    DisableControls;
    MoveBy(-BlockSize);
  finally
    EnableControls;
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
  SetEnabled(Assigned(ControlEngine) and not EngineControlsDisabled and EngineIsActive and not EngineEof);
end;

procedure TJvDatabaseNextBlockAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  with ControlEngine do
  try
    DisableControls;
    MoveBy(BlockSize);
  finally
    EnableControls;
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
  inherited;
  Refresh;
end;

procedure TJvDatabaseRefreshAction.Refresh;
var
  MyBookmark: TBookmark;
begin
  with ControlEngine.DataSet do
  begin
    MyBookmark := nil;
    if RefreshLastPosition then
      MyBookmark := GetBookmark;

    try
      if RefreshAsOpenClose then
      begin
        Close;
        Open;
      end
      else
        Refresh;

      if RefreshLastPosition then
        if Active then
          if Assigned(MyBookmark) then
            if BookmarkValid(MyBookmark) then
            try
              GotoBookmark(MyBookmark);
            except
            end;
    finally
      if RefreshLastPosition then
        FreeBookmark(MyBookmark);
    end;
  end;
end;

constructor TJvDatabasePositionAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowSelectedRows := True;
  FMinCountSelectedRows := 2;
end;

//=== { TJvDatabasePositionAction } ==========================================

procedure TJvDatabasePositionAction.UpdateTarget(Target: TObject);
const
  cFormat = ' %3d / %3d ';
  cFormatSelected = ' %3d / %3d (%d)';
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and EngineHasData);
  try
    if not EngineIsActive then
      SetCaption(Format(cFormat, [0, 0]))
    else
      if EngineRecordCount = 0 then
        SetCaption(Format(cFormat, [0, 0]))
      else
        if ShowSelectedRows and (EngineSelectedRowsCount >= MinCountSelectedRows) then
          SetCaption(Format(cFormatSelected, [EngineRecNo, EngineRecordCount, EngineSelectedRowsCount]))
        else
          SetCaption(Format(cFormat, [EngineRecNo, EngineRecordCount]));
  except
    SetCaption(Format(cFormat, [0, 0]));
  end;
end;

procedure TJvDatabasePositionAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  ShowPositionDialog;
end;

procedure TJvDatabasePositionAction.SetCaption(Value: string);
begin
  if Value <> Caption then
    Caption := Value;
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
  Kind: integer;
begin
  if not Assigned(DataSet) then
    Exit;
  ParameterList := TJvParameterList.Create(Self);
  try
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      SearchName := cCurrentPosition;
      ReadOnly := True;
      Caption := RsDBPosCurrentPosition;
      AsString := IntToStr(EngineRecNo + 1) + ' / ' + IntToStr(EngineRecordCount);
      Width := 150;
      LabelWidth := 80;
      Enabled := False;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvEditParameter.Create(ParameterList));
    with TJvEditParameter(Parameter) do
    begin
      Caption := RsDBPosNewPosition;
      SearchName := cNewPosition;
      // EditMask := '999999999;0;_';
      Width := 150;
      LabelWidth := 80;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter := TJvBaseParameter(TJvRadioGroupParameter.Create(ParameterList));
    with TJvRadioGroupParameter(Parameter) do
    begin
      Caption := RsDBPosMovementType;
      SearchName := cKind;
      Width := 305;
      Height := 54;
      Columns := 2;
      ItemList.Add(RsDBPosAbsolute);
      ItemList.Add(RsDBPosForward);
      ItemList.Add(RsDBPosBackward);
      ItemList.Add(RsDBPosPercental);
      ItemIndex := 0;
    end;
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

procedure TJvDatabaseInsertAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and
    EngineIsActive and EngineCanInsert and not EngineEditModeActive);
end;

procedure TJvDatabaseInsertAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  DataSet.Insert;
end;

//=== { TJvDatabaseCopyAction } ==============================================

procedure TJvDatabaseCopyAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and
    EngineCanInsert and EngineHasData and not EngineEditModeActive);
end;

procedure TJvDatabaseCopyAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  CopyRecord;
end;

procedure TJvDatabaseCopyAction.CopyRecord;
var
  Values: array of variant;
  I: integer;
  Value: variant;
  Allowed: boolean;
begin
  with DataSet do
  begin
    if not Active then
      Exit;
    if State in [dsInsert, dsEdit] then
      Post;
    if State <> dsBrowse then
      Exit;
    Allowed := True;
  end;
  if Assigned(FBeforeCopyRecord) then
    FBeforeCopyRecord(DataSet, Allowed);
  with DataSet do
  begin
    // (rom) this suppresses AfterCopyRecord. Is that desired?
    if not Allowed then
      Exit;
    SetLength(Values, FieldCount);
    for I := 0 to FieldCount - 1 do
      Values[I] := Fields[I].AsVariant;
    Insert;
    if Assigned(FOnCopyRecord) then
      for I := 0 to FieldCount - 1 do
      begin
        Value := Values[I];
        FOnCopyRecord(Fields[I], Value);
      end
    else
      for I := 0 to FieldCount - 1 do
        Fields[I].AsVariant := Values[I];
  end;
  if Assigned(FAfterCopyRecord) then
    FAfterCopyRecord(DataSet);
end;

//=== { TJvDatabaseEditAction } ==============================================

procedure TJvDatabaseEditAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and
    EngineCanUpdate and EngineHasData and not EngineEditModeActive);
end;

procedure TJvDatabaseEditAction.ExecuteTarget(Target: TObject);
begin
  inherited;
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
  inherited;
  DataSet.Delete;
end;

//=== { TJvDatabasePostAction } ==============================================

procedure TJvDatabasePostAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and EngineEditModeActive);
end;

procedure TJvDatabasePostAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  DataSet.Post;
end;

//=== { TJvDatabaseCancelAction } ============================================

procedure TJvDatabaseCancelAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineControlsDisabled and EngineIsActive and EngineEditModeActive);
end;

procedure TJvDatabaseCancelAction.ExecuteTarget(Target: TObject);
begin
  inherited;
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
  inherited;
  ShowSingleRecordWindow
end;

procedure TJvDatabaseSingleRecordWindowAction.ShowSingleRecordWindow;
begin
  ControlEngine.ShowSingleRecordWindow(Options);
end;

//=== { TJvDatabaseOpenAction } ==============================================

procedure TJvDatabaseOpenAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and not EngineIsActive);
end;

procedure TJvDatabaseOpenAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  DataSet.Open;
end;

//=== { TJvDatabaseCloseAction } =============================================

procedure TJvDatabaseCloseAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and EngineIsActive and not EngineEditModeActive);
end;

procedure TJvDatabaseCloseAction.ExecuteTarget(Target: TObject);
begin
  inherited;
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

procedure TJvDatabaseSMExportOptions.SMEWizardDlgGetCellParams(Sender: TObject; Field: TField;
  var Text: string; AFont: TFont; var Alignment: TAlignment; var Background: TColor; var CellType: TCellType);
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
        Case Field.DataType of
          ftFloat, ftBCD, ftCurrency:
            Text := AnsiReplaceStr(Text, ',', '.');
          ftDate, ftDateTime :
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
        Case CellType of
          ctDouble, ctCurrency :
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
                on e:exception do
                  Text := Format(StoDateFormatLong, [Text]);
              end;
              CellType := ctBlank;
            end;
        end;
end;

procedure TJvDatabaseSMExportOptions.SMEWizardDlgOnBeforeExecute(Sender: TObject);
begin
  if Assigned(FOnBeforeExecuteExport) then
    FOnBeforeExecuteExport (Sender)
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
  SMEWizardDlg := TSMEWizardDlg.Create(Self);
  try
    SMEWizardDlg.ColumnSource := csDataSet;
    SMEWizardDlg.OnGetCellParams := Options.SMEWizardDlgGetCellParams;
    SMEWizardDlg.OnBeforeExecute := Options.SMEWizardDlgOnBeforeExecute;
    SMEWizardDlg.OnAfterExecute := Options.OnAfterExecuteExport;
    SMEWizardDlg.DataSet := DataSource.DataSet;
    SMEWizardDlg.Title := Options.Title;
    SMEWizardDlg.KeyGenerator := Options.Title;
    SMEWizardDlg.WizardStyle := smewiz.wsWindows2000;
    SMEWizardDlg.SpecificationDir := Options.DefaultOptionsDirectory + '\';
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
    if FileExists(Options.DefaultOptionsDirectory + cLastExport) then
      SMEWizardDlg.LoadSpecification(Options.DefaultOptionsDirectory + cLastExport);
    SMEWizardDlg.Execute;
    SMEWizardDlg.SaveSpecification('Last Export', Options.DefaultOptionsDirectory + cLastExport, False);
  finally
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
    FreeAndNil(SMEEngineCx);
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
    FreeAndNil(SMEWizardDlg);
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
  inherited;
  ImportData;
end;

procedure TJvDatabaseSMImportAction.ImportData;
var
  SMIWizardDlg: TSMIWizardDlg;
begin
  SMIWizardDlg := TSMIWizardDlg.Create(Self);
  try
    //    SMIWizardDlg.OnGetSpecifications := Options.SMIWizardDlgGetSpecifications;
    SMIWizardDlg.SpecificationDir := Options.DefaultOptionsDirectory + '\';
    SMIWizardDlg.DataSet := DataSource.DataSet;
    SMIWizardDlg.Title := Options.Title;
    SMIWizardDlg.Formats := Options.Formats;
    SMIWizardDlg.HelpContext := Options.HelpContext;
    SMIWizardDlg.WizardStyle := Options.WizardStyle;
    SMIWizardDlg.Options := Options.Options;
    //    IF FileExists (Options.DefaultOptionsDirectory+'\Last Import.SMI') THEN
    //      SMIWizardDlg.LoadSpecification(Options.DefaultOptionsDirectory+'\Last Import.SMI');
    SMIWizardDlg.Execute;
    SMIWizardDlg.SaveSpecification('Last Import', Options.DefaultOptionsDirectory + '\Last Import.SMI', False);
  finally
    FreeAndNil(SMIWizardDlg);
  end;
end;

{$ENDIF USE_3RDPARTY_SMIMPORT}

//=== { TJvDatabaseModifyAllAction } ============================================

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
  i: Integer;
  Field: TField;
  FieldName: string;
  ChangeTo: string;
  ClearField: Boolean;
  OnlyIfNull: Boolean;
begin
  if not Assigned(ControlEngine) then
    Exit;
  JvParameterList := TJvParameterList.Create(self);
  try
    JvParameterList.Messages.Caption := SModifyAllCaption;
    JvParameterList.Messages.OkButton := SModifyAllOkButton;
    Parameter := TJvBaseParameter(TJvComboBoxParameter.Create(JvParameterList));
    with TJvComboBoxParameter(Parameter) do
    begin
      LabelArrangeMode := lamAbove;
      SearchName := 'ModifyField';
      Caption := SModifyAllModifyField;
      Width := 330;
      for i := 0 to EngineFieldCount - 1 do
      begin
        Field := ControlEngine.FieldById(i);
        if Assigned(Field) then
          if not ControlEngine.IsFieldReadOnly(Field.FieldName)
            and ControlEngine.IsFieldVisible(Field.FieldName) then
            ItemList.Add(Field.FieldName);
        if Assigned(ControlEngine.SelectedField) then
          ItemIndex := ItemList.IndexOf(ControlEngine.SelectedField.FieldName);
        if (ItemIndex < 0) or (ItemIndex >= ItemList.Count) then
          ItemIndex := 0;
      end;
    end;
    JvParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(JvParameterList);
    with TJvCheckBoxParameter(Parameter) do
    begin
      SearchName := 'ClearFieldValues';
      Caption := SModifyAllClearFieldValues;
      Width := 150;
    end;
    JvParameterList.AddParameter(Parameter);
    Parameter := TJvEditParameter.Create(JvParameterList);
    with TJvEditParameter(Parameter) do
    begin
      SearchName := 'ChangeTo';
      Caption := SModifyAllChangeTo;
      Width := 330;
      LabelArrangeMode := lamAbove;
      DisableReasons.AddReason('ClearFieldValues', True);
    end;
    JvParameterList.AddParameter(Parameter);
    Parameter := TJvCheckBoxParameter.Create(JvParameterList);
    with TJvCheckBoxParameter(Parameter) do
    begin
      SearchName := 'OnlyIfNull';
      Caption := SModifyAllOnlyIfNull;
      Width := 150;
      DisableReasons.AddReason('ClearFieldValues', True);
    end;
    JvParameterList.AddParameter(Parameter);
    JvParameterList.MaxWidth := 360;
    if JvParameterList.ShowParameterDialog then
    begin
      FieldName := JvParameterList.ParameterByName('ModifyField').AsString;
      ClearField := JvParameterList.ParameterByName('ClearFieldValues').AsBoolean;
      OnlyIfNull := JvParameterList.ParameterByName('OnlyIfNull').AsBoolean;
      ChangeTo := JvParameterList.ParameterByName('ChangeTo').AsString;
      Field := ControlEngine.FieldByName(FieldName);
      if Assigned(Field) then
      try
        ControlEngine.DisableControls;
        for I := 0 to ControlEngine.SelectedRowsCount - 1 do
          if ControlEngine.GotoSelectedRow(i) then
          begin
            try
              if (ClearField and not Field.IsNull) or
                not (OnlyIfNull and not Field.IsNull) then
              begin
                ControlEngine.Dataset.Edit;
                if ClearField then
                  Field.Clear
                else
                  Field.AsString := ChangeTo;
                if Assigned(ControlEngine.Dataset) then
                  ControlEngine.Dataset.Post;
              end;
            except
              on e: exception do
              begin
                ControlEngine.Dataset.Cancel;
                JvDSADialogs.MessageDlg(e.Message, mtError, [mbOK], 0);
              end;
            end;
          end;
      finally
        ControlEngine.EnableControls;
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

procedure TJvDatabaseShowSQLStatementAction.ExecuteTarget(Target: TObject);
begin
  ShowSQLStatement;
end;

procedure TJvDatabaseShowSQLStatementAction.ShowSQLStatement;
var ParameterList : TJvParameterList;
    Parameter : TJvBaseParameter;
begin
  if not Assigned(DatasetEngine) then
    Exit;
  ParameterList := TJvParameterList.Create(self);
  try
    Parameter := TJvBaseParameter(TJvMemoParameter.Create(ParameterList));
    with TJvMemoParameter(Parameter) do
    begin
      SearchName := 'SQLStatement';
      ScrollBars := ssBoth;
      WordWrap := False;
      ReadOnly := True;
      //Caption := '&SQL Statement';
      AsString := DatasetEngine.SQL;
      Width := 500;
      Height := 350;
    end;
    ParameterList.AddParameter(Parameter);
    ParameterList.ArrangeSettings.WrapControls := True;
    ParameterList.ArrangeSettings.MaxWidth := 650;
    ParameterList.Messages.Caption := SShowSQLStatementCaption;
    ParameterList.Messages.OkButton := SSQLStatementClipboardButton;
    if ParameterList.ShowParameterDialog then
      ClipBoard.AsText := DatasetEngine.SQL;
  finally
    FreeAndNil(ParameterList);
  end;
end;

procedure TJvDatabaseShowSQLStatementAction.UpdateTarget(Target: TObject);
begin
  SetEnabled(Assigned(DataSet) and Assigned(DatasetEngine) and DatasetEngine.SupportsGetSQL);
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

