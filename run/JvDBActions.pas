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
  {$IFDEF MSWINDOWS}
  ActnList,
  Windows, ImgList,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QActnList,
  QWindows, QImgList,
  {$ENDIF UNIX}
  Forms,
  Controls,
  Classes, DB, JvDynControlEngineDB, JvDynControlEngineDBTools;

type

  TComponentClass = class of TComponent;

  TJvShowSingleRecordWindowOptions = class(tPersistent)
  private
    FDialogCaption : string;
    FPostButtonCaption : string;
    FCancelButtonCaption : string;
    FCloseButtonCaption : string;
    FBorderStyle : TFormBorderStyle;
    FPosition : TPosition;
    FTop : integer;
    FLeft : Integer;
    FWidth : Integer;
    FHeight : Integer;
  public
    constructor Create;
    procedure SetOptionsToDialog (ADialog : TJvDynControlDataSourceEditDialog);
  published
    property DialogCaption : string read FDialogCaption write FDialogCaption;
    property PostButtonCaption : string read FPostButtonCaption write FPostButtonCaption;
    property CancelButtonCaption : string read FCancelButtonCaption write FCancelButtonCaption;
    property CloseButtonCaption : string read FCloseButtonCaption write FCloseButtonCaption;
    property BorderStyle : TFormBorderStyle read FBorderStyle write FBorderStyle default bsDialog	;
    property Position : TPosition read FPosition write FPosition default poScreenCenter;
    property Top : integer read FTop write FTop default 0;
    property Left : Integer read FLeft write FLeft default 0;
    property Width : Integer read FWidth write FWidth default 640;
    property Height : Integer read FHeight write FHeight default 480;
  end;

  TJvDatabaseActionList = class(tActionList)
  private
    FDataComponent : TComponent;
  protected
    procedure SetDataComponent (Value : TComponent);
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property DataComponent : TComponent read FDataComponent write SetDataComponent;
  end;

  TJvDatabaseActionBaseEngine = class(tComponent)
  private
  protected
    function GetDatasource (ADataComponent : TComponent): TDatasource; virtual;
    function GetDataset (ADataComponent : TComponent): TDataset; virtual;
  public
    function Supports (ADataComponent : TComponent) : Boolean; virtual;
    function IsActive (ADataComponent : TComponent) : Boolean; virtual;
    function HasData (ADataComponent : TComponent) : Boolean; virtual;
    function FieldCount (ADataComponent : TComponent) : Integer; virtual;
    function RecordCount (ADataComponent : TComponent) : Integer; virtual;
    function RecNo (ADataComponent : TComponent) : Integer; virtual;
    function CanModify (ADataComponent : TComponent) : Boolean; virtual;
    function Eof (ADataComponent : TComponent) : Boolean; virtual;
    function Bof (ADataComponent : TComponent) : Boolean; virtual;
    procedure DisableControls(ADataComponent : TComponent); virtual;
    procedure EnableControls(ADataComponent : TComponent); virtual;
    function ControlsDisabled (ADataComponent : TComponent) : Boolean; virtual;
    function EditModeActive (ADataComponent : TComponent) : Boolean; virtual;
    procedure First(ADataComponent : TComponent); virtual;
    procedure Last(ADataComponent : TComponent); virtual;
    procedure MoveBy (ADataComponent : TComponent; Distance : Integer); virtual;
    procedure ShowSingleRecordWindow (AOptions : TJvShowSingleRecordWindowOptions;ADataComponent : TComponent); virtual;
  published
  end;

  TJvDatabaseActionBaseEngineClass = class of TJvDatabaseActionBaseEngine;

  TJvDatabaseActionDBGridEngine = class(TJvDatabaseActionBaseEngine)
  private
    FCurrentDataComponent : TComponent;
  protected
    function GetDatasource (ADataComponent : TComponent): TDatasource; override;
    procedure OnCreateDataControls (ADynControlEngineDB : TJvDynControlEngineDB; AParentControl : TWinControl);
  public
    function Supports (ADataComponent : TComponent) : Boolean; override;
    procedure ShowSingleRecordWindow (AOptions : TJvShowSingleRecordWindowOptions;ADataComponent : TComponent); override;
  end;

  TJvDatabaseExecuteEvent =procedure (Sender: TObject; DataEngine: TJvDatabaseActionBaseEngine; DataComponent: TComponent) of object;
  TJvDatabaseExecuteDatasourceEvent =procedure (Sender: TObject; Datasource : TDatasource) of object;
  TJvDatabaseBaseAction = class(TAction)
  private
    FOnExecute :  TJvDatabaseExecuteEvent;
    FOnExecuteDatasource : TJvDatabaseExecuteDatasourceEvent;
    FDataEngine: TJvDatabaseActionBaseEngine;
    FDataComponent : TComponent;
  protected
    procedure SetDataComponent (Value : TComponent);
    procedure SetEnabled (Value : Boolean);
    function  GetDataset : TDataset;
    function  GetDatasource : TDatasource;
    function EngineIsActive : Boolean;
    function EngineHasData : Boolean;
    function EngineFieldCount : Integer;
    function EngineRecordCount : Integer;
    function EngineRecNo : Integer;
    function EngineCanModify : Boolean;
    function EngineEof : Boolean;
    function EngineBof : Boolean;
    function EngineControlsDisabled : Boolean;
    function EngineEditModeActive : Boolean;
    property DataEngine: TJvDatabaseActionBaseEngine read FDataEngine;
  public
    constructor Create(AOwner : TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    function  HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Datasource : TDatasource read GetDatasource;
    property DataSet : TDataset read GetDataset;
  published
    property OnExecute :  TJvDatabaseExecuteEvent read FOnExecute write FOnExecute;
    property OnExecuteDatasource : TJvDatabaseExecuteDatasourceEvent read FOnExecuteDatasource write FOnExecuteDatasource;
    property DataComponent : TComponent read FDataComponent write SetDataComponent;
  end;

  TJvDatabaseSimpleAction = class(TJvDatabaseBaseAction)
  private
    FIsActive: Boolean;
    FHasData: Boolean;
    FCanModify: Boolean;
    FEditModeActive: Boolean;
  protected
  public
    constructor Create(AOwner : TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
  published
    property IsActive: Boolean read FIsActive write FIsActive default true;
    property HasData: Boolean read FHasData write FHasData default true;
    property CanModify: Boolean read FCanModify write FCanModify default false;
    property EditModeActive: Boolean read FEditModeActive write FEditModeActive default false;
  end;

  TJvDatabaseBaseActiveAction = class(TJvDatabaseBaseAction)
  private
  protected
  public
    procedure UpdateTarget(Target: TObject); override;
  published
  end;

  TJvDatabaseBaseEditAction = class(TJvDatabaseBaseActiveAction)
  private
  protected
  public
    procedure UpdateTarget(Target: TObject); override;
  published
  end;

  TJvDatabaseBaseNavigateAction = class(TJvDatabaseBaseActiveAction)
  private
  protected
  public
  published
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
    constructor Create (AOwner : TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BlockSize: Integer read FBlockSize write FBlockSize default 50;
  end;
  TJvDatabaseNextBlockAction = class(TJvDatabaseBaseNavigateAction)
  private
    FBlockSize: Integer;
  public
    constructor Create (AOwner : TComponent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BlockSize: Integer read FBlockSize write FBlockSize default 50;
  end;

  TJvDatabaseRefreshAction = class(TJvDatabaseBaseActiveAction)
  private
    FRefreshLastPosition : Boolean;
    FRefreshAsOpenClose : Boolean;
  protected
    procedure Refresh;
  public
    constructor Create (AOwner : TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property RefreshLastPosition : Boolean read FRefreshLastPosition write FRefreshLastPosition default true;
    property RefreshAsOpenClose : Boolean read FRefreshAsOpenClose write FRefreshAsOpenClose default false;
  end;
  TJvDatabasePositionAction = class(TJvDatabaseBaseNavigateAction)
  public
    procedure ShowPositionDialog;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabaseInsertAction = class(TJvDatabaseBaseEditAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TJvDatabaseOnCopyRecord = procedure (Field : tField; OldValue : Variant) of Object;
  TJvDatabaseBeforeCopyRecord = procedure (Dataset : TDataset; var RefreshAllowed : Boolean) of object;
  TJvDatabaseAfterCopyRecord = procedure (Dataset : TDataset) of object;
  TJvDatabaseCopyAction = class(TJvDatabaseBaseEditAction)
  private
    FBeforeCopyRecord : TJvDatabaseBeforeCopyRecord;
    FAfterCopyRecord  : TJvDatabaseAfterCopyRecord;
    FOnCopyRecord     : TJvDatabaseOnCopyRecord;
  public
    procedure CopyRecord;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property BeforeCopyRecord : TJvDatabaseBeforeCopyRecord read fBeforeCopyRecord write fBeforeCopyRecord;
    property AfterCopyRecord  : TJvDatabaseAfterCopyRecord read fAfterCopyRecord write fAfterCopyRecord;
    property OnCopyRecord     : TJvDatabaseOnCopyRecord read fOnCopyRecord write fOnCopyRecord;
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
    FOptions : TJvShowSingleRecordWindowOptions;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property Options : TJvShowSingleRecordWindowOptions read FOptions write FOptions;
  end;

  TJvDatabaseOpenAction = class(TJvDatabaseBaseActiveAction)
  private
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
  end;

  TJvDatabaseCloseAction = class(TJvDatabaseBaseActiveAction)
  private
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
  end;



procedure RegisterActionEngine (AEngineClass : TJvDatabaseActionBaseEngineClass);

implementation

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  SysUtils,
  DBGrids,
  JvResources, JvParameterList, JvParameterListParameter, JvPanel,
  Grids;

type
  TJvDatabaseActionEngineList = class(TList)
  public
    destructor Destroy; override;
    procedure RegisterEngine (AEngineClass : TJvDatabaseActionBaseEngineClass);
    function GetEngine (AComponent: TComponent) : TJvDatabaseActionBaseEngine;
  end;

var
  RegisteredActionEngineList : TJvDatabaseActionEngineList;

//=== { TJvDatabaseActionList } ==================================================

constructor TJvDatabaseActionList.Create (AOwner : TComponent);
begin
  Inherited Create (AOwner);
end;

destructor TJvDatabaseActionList.Destroy;
begin
  Inherited Destroy;
end;

procedure TJvDatabaseActionList.SetDataComponent (Value : TComponent);
var
  I : Integer;
begin
  FDataComponent := Value;
  if FDataComponent <> nil then
    FDataComponent.FreeNotification(self);
  for I := 0 to ActionCount-1 do
    if Actions[i] is TJvDatabaseBaseAction then
      TJvDatabaseBaseAction(Actions[i]).DataComponent := Value;
end;


procedure TJvDatabaseActionList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification (AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FDataComponent then
      DataComponent := nil;
end;

//=== { TJvShowSingleRecordWindowOptions } ==================================================

constructor TJvShowSingleRecordWindowOptions.Create;
begin
  inherited Create;
  FDialogCaption := '';
  FPostButtonCaption := RsSRWPostButtonCaption;
  FCancelButtonCaption := RsSRWCancelButtonCaption;
  FCloseButtonCaption := RsSRWCloseButtonCaption;
  FBorderStyle := bsDialog;
  FTop := 0;
  FLeft := 0;
  FWidth := 640;
  FHeight := 480;
  FPosition := poScreenCenter;
end;

procedure TJvShowSingleRecordWindowOptions.SetOptionsToDialog (ADialog : TJvDynControlDataSourceEditDialog);
begin
  if Assigned(ADialog) then
  begin
    ADialog.DialogCaption := DialogCaption;
    ADialog.PostButtonCaption := PostButtonCaption ;
    ADialog.CancelButtonCaption := CancelButtonCaption ;
    ADialog.CloseButtonCaption := CloseButtonCaption ;
    ADialog.Position := Position ;
    ADialog.BorderStyle := BorderStyle ;
    ADialog.Top := Top ;
    ADialog.Left := Left ;
    ADialog.Width := Width ;
    ADialog.Height := Height ;
  end;
end;

//=== { TJvDatabaseActionBaseEngine } ==================================================

function TJvDatabaseActionBaseEngine.GetDatasource (ADataComponent : TComponent): TDatasource;
begin
  if Assigned(ADataComponent) and (ADataComponent IS TDatasource) then
    Result := TDatasource(ADataComponent)
  else
    Result := nil;
end;

function TJvDatabaseActionBaseEngine.GetDataset (ADataComponent : TComponent): TDataset;
begin
  if Assigned(GetDatasource(ADataComponent)) then
    Result := GetDatasource(ADataComponent).Dataset
  else
    Result := nil;
end;

function TJvDatabaseActionBaseEngine.Supports (ADataComponent : TComponent) : Boolean;
begin
  Result := Assigned(ADataComponent) and (ADataComponent IS TDatasource);
end;

function TJvDatabaseActionBaseEngine.IsActive (ADataComponent : TComponent) : Boolean;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.Active
  else
    Result := False;
end;

function TJvDatabaseActionBaseEngine.HasData (ADataComponent : TComponent) : Boolean;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.RecordCount > 0
  else
    Result:= False;
end;

function TJvDatabaseActionBaseEngine.FieldCount (ADataComponent : TComponent) : Integer;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.FieldCount
  else
    Result := -1;
end;

function TJvDatabaseActionBaseEngine.RecordCount (ADataComponent : TComponent) : Integer;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.RecordCount
  else
    Result := -1;
end;

function TJvDatabaseActionBaseEngine.RecNo (ADataComponent : TComponent) : Integer;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.Recno
  else
    Result := -1;
end;

function TJvDatabaseActionBaseEngine.CanModify (ADataComponent : TComponent) : Boolean;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.CanModify
  else
    Result := False;
end;

function TJvDatabaseActionBaseEngine.Eof (ADataComponent : TComponent) : Boolean;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.Eof
  else
    Result := False;
end;

function TJvDatabaseActionBaseEngine.Bof (ADataComponent : TComponent) : Boolean;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.Bof
  else
    Result := False;
end;

procedure TJvDatabaseActionBaseEngine.DisableControls(ADataComponent : TComponent);
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Dataset.DisableControls;
end;

procedure TJvDatabaseActionBaseEngine.EnableControls(ADataComponent : TComponent);
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Dataset.EnableControls;
end;

function TJvDatabaseActionBaseEngine.ControlsDisabled (ADataComponent : TComponent) : Boolean;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.ControlsDisabled
  else
    Result := False;
end;

function TJvDatabaseActionBaseEngine.EditModeActive (ADataComponent : TComponent) : Boolean;
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Result := Dataset.State in [dsInsert, dsEdit]
  else
    Result := False;
end;

procedure TJvDatabaseActionBaseEngine.First (ADataComponent : TComponent);
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Dataset.First;
end;

procedure TJvDatabaseActionBaseEngine.Last (ADataComponent : TComponent);
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Dataset.Last;
end;

procedure TJvDatabaseActionBaseEngine.MoveBy (ADataComponent : TComponent; Distance : Integer);
var
  DataSet : TDataset;
begin
  DataSet := GetDataset(ADataComponent);
  if Assigned(Dataset) then
    Dataset.MoveBy(Distance);
end;

procedure TJvDatabaseActionBaseEngine.ShowSingleRecordWindow (AOptions : TJvShowSingleRecordWindowOptions;ADataComponent : TComponent);
var
  Dialog : TJvDynControlDataSourceEditDialog;
begin
  Dialog := TJvDynControlDataSourceEditDialog.Create;
  try
    AOptions.SetOptionsToDialog (Dialog);
    Dialog.DataSource := GetDatasource(ADataComponent);
    Dialog.ShowDialog;
  finally
    Dialog.Free;
  end;
end;

//=== { TJvDatabaseActionDBGridEngine } ==================================================

function TJvDatabaseActionDBGridEngine.GetDatasource (ADataComponent : TComponent): TDatasource;
begin
  if Assigned(ADatacomponent) and
     (ADatacomponent is TCustomDBGrid) then
    Result := TCustomDBGrid(ADatacomponent).Datasource
  else
    Result := nil;
end;

type
  TAccessCustomDBGrid = Class(TCustomDBGrid);
  TAccessCustomControl = class(TCustomControl);

procedure TJvDatabaseActionDBGridEngine.OnCreateDataControls (ADynControlEngineDB : TJvDynControlEngineDB; AParentControl : TWinControl);
var
  I : Integer;
  ds : TDatasource;
  Field : TField;
//  LabelControl : TControl;
  Control : TWinControl;
  Column : TColumn;
begin
  if Assigned(FCurrentDataComponent) and
     (FCurrentDataComponent is TCustomDBGrid) then
  begin
    ds := GetDatasource(FCurrentDataComponent);
    for I := 0 to TAccessCustomDBGrid(FCurrentDataComponent).ColCount-2 do
    begin
      Column := TAccessCustomDBGrid(FCurrentDataComponent).Columns[I];
      if Column.Visible then
      begin
        Field := Column.Field;
        Control := ADynControlEngineDB.CreateDBFieldControl(Field, AParentControl, AParentControl, '', ds);
        if Field.Size > 0 then
          Control.Width :=
            TAccessCustomControl(AParentControl).Canvas.TextWidth(' ') * Field.Size;
{        LabelControl := }ADynControlEngineDB.DynControlEngine.CreateLabelControlPanel(AParentControl, AParentControl,
          '', '&' + Column.Title.Caption, Control, True, 0);
//        if (AFieldSizeStep > 0) then
//          if ((LabelControl.Width mod AFieldSizeStep) <> 0) then
//            LabelControl.Width := ((LabelControl.Width div AFieldSizeStep) + 1) * AFieldSizeStep;
      end;
    end;
  end;
end;

function TJvDatabaseActionDBGridEngine.Supports (ADataComponent : TComponent) : Boolean;
begin
  Result := Assigned(ADatacomponent) and (ADatacomponent is TCustomDBGrid);
end;

procedure TJvDatabaseActionDBGridEngine.ShowSingleRecordWindow (AOptions : TJvShowSingleRecordWindowOptions;ADataComponent : TComponent);
var
  Dialog : TJvDynControlDataSourceEditDialog;
begin
  Dialog := TJvDynControlDataSourceEditDialog.Create;
  try
    AOptions.SetOptionsToDialog (Dialog);
    FCurrentDataComponent := ADataComponent;
    Dialog.DataSource := GetDatasource(ADataComponent);
    Dialog.OnCreateDataControlsEvent := OnCreateDataControls;
    Dialog.ShowDialog;
  finally
    Dialog.Free;
  end;
end;

//=== { TJvDatabaseBaseAction } ==================================================

constructor TJvDatabaseBaseAction.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  if Assigned(AOwner) and
     (AOwner is TJvDatabaseActionList) then
    Datacomponent := TJvDatabaseActionList(AOwner).Datacomponent;
end;


function  TJvDatabaseBaseAction.GetDataset : TDataset;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.GetDataSet(DataComponent)
  else
    Result := nil;
end;

function  TJvDatabaseBaseAction.GetDatasource : TDatasource;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.GetDatasource(DataComponent)
  else
    Result := nil;
end;

procedure TJvDatabaseBaseAction.SetDataComponent (Value : TComponent);
begin
  FDataComponent := Value;
  if FDataComponent <> nil then
    FDataComponent.FreeNotification(self);
  if Assigned(RegisteredActionEngineList) then
    FDataEngine := RegisteredActionEngineList.GetEngine(FDataComponent)
  else
    FDataEngine := nil;
end;

procedure TJvDatabaseBaseAction.SetEnabled (Value : Boolean);
begin
  if Enabled <> Value then
    Enabled := Value;
end;

function TJvDatabaseBaseAction.EngineIsActive : Boolean;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.IsActive(DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineHasData : Boolean;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.HasData(DataComponent)
  else
    Result:= False;
end;

function TJvDatabaseBaseAction.EngineFieldCount : Integer;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.FieldCount(DataComponent)
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.EngineRecordCount : Integer;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.RecordCount(DataComponent)
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.EngineRecNo : Integer;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.Recno(DataComponent)
  else
    Result := -1;
end;

function TJvDatabaseBaseAction.EngineCanModify : Boolean;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.CanModify(DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineEof : Boolean;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.Eof(DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineBof : Boolean;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.Bof(DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineControlsDisabled : Boolean;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.ControlsDisabled(DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.EngineEditModeActive : Boolean;
begin
  if Assigned(DataEngine) then
    Result := DataEngine.EditModeActive(DataComponent)
  else
    Result := False;
end;

function TJvDatabaseBaseAction.HandlesTarget(Target: TObject): Boolean;
begin
//  Result := inherited HandlesTarget(Target);
  Result := Assigned(DataEngine);
end;

procedure TJvDatabaseBaseAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and not EngineControlsDisabled then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseBaseAction.ExecuteTarget(Target: TObject);
begin
  if Assigned (fOnExecute) then
    fOnExecute(Self, DataEngine, DataComponent)
  else if Assigned(fOnExecuteDatasource) then
    fOnExecuteDatasource(Self, Datasource)
  else
    inherited ExecuteTarget (Target);
end;

procedure TJvDatabaseBaseAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification (AComponent, Operation);
end;

//=== { TJvDatabaseSimpleAction } ==================================================

constructor TJvDatabaseSimpleAction.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FIsActive:= true;
  FHasData:= true;
  FCanModify:= false;
  FEditModeActive:= false;
end;

procedure TJvDatabaseSimpleAction.UpdateTarget(Target: TObject);
var
  Res : Boolean;
begin
  if Assigned(Dataset) and not EngineControlsDisabled then
  begin
    Res := False;
    if IsActive then
      Res := Res and EngineIsActive;
    if HasData then
      Res := Res and EngineHasData;
    if CanModify then
      Res := Res and EngineCanModify;
    if EditModeActive then
      Res := Res and EngineEditModeActive;
    SetEnabled (Res)
  end
  else
    SetEnabled (false);
end;

//=== { TJvDatabaseBaseActiveAction } ==================================================
procedure TJvDatabaseBaseActiveAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and
     not EngineControlsDisabled and
     EngineIsActive then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

//=== { TJvDatabaseBaseEditAction } ==================================================
procedure TJvDatabaseBaseEditAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and
     not EngineControlsDisabled and
     EngineIsActive and
     EngineCanModify then
    SetEnabled (true)
  else
    SetEnabled (false);
end;


//=== { TJvDatabaseFirstAction } ==================================================
procedure TJvDatabaseFirstAction.UpdateTarget(Target: TObject);
begin
  if Assigned(DataEngine) and
     not EngineControlsDisabled and
     EngineIsActive and
     not EngineBof then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseFirstAction.ExecuteTarget(Target: TObject);
begin
  DataEngine.First(DataComponent);
end;

//=== { TJvDatabaseLastAction } ==================================================
procedure TJvDatabaseLastAction.UpdateTarget(Target: TObject);
begin
  if Assigned(DataEngine) and
     not EngineControlsDisabled and
     EngineIsActive and
     not EngineEoF then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseLastAction.ExecuteTarget(Target: TObject);
begin
  DataEngine.Last(DataComponent);
end;

//=== { TJvDatabasePriorAction } ==================================================
procedure TJvDatabasePriorAction.UpdateTarget(Target: TObject);
begin
  if Assigned(DataEngine) and
     not EngineControlsDisabled and
     EngineIsActive and
     not EngineBof then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabasePriorAction.ExecuteTarget(Target: TObject);
begin
  DataEngine.MoveBy(DataComponent, -1);
end;

//=== { TJvDatabaseNextAction } ==================================================
procedure TJvDatabaseNextAction.UpdateTarget(Target: TObject);
begin
  if Assigned(DataEngine) and
     not EngineControlsDisabled and
     EngineIsActive and
     not EngineEoF then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseNextAction.ExecuteTarget(Target: TObject);
begin
  DataEngine.MoveBy(DataComponent, 1);
end;

//=== { TJvDatabasePriorBlockAction } ==================================================
constructor TJvDatabasePriorBlockAction.Create (AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FBlockSize := 50;
end;

procedure TJvDatabasePriorBlockAction.UpdateTarget(Target: TObject);
begin
  if Assigned(DataEngine) and
     not EngineControlsDisabled and
     EngineIsActive and
     not EngineBof then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabasePriorBlockAction.ExecuteTarget(Target: TObject);
begin
  with DataEngine do
  try
    DisableControls(DataComponent);
    MoveBy (DataComponent, -BlockSize);
  finally
    EnableControls(DataComponent);
  end;
end;

//=== { TJvDatabaseNextBlockAction } ==================================================
constructor TJvDatabaseNextBlockAction.Create (AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FBlockSize := 50;
end;

procedure TJvDatabaseNextBlockAction.UpdateTarget(Target: TObject);
begin
  if Assigned(DataEngine) and
     not EngineControlsDisabled and
     EngineIsActive and
     not EngineEoF then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseNextBlockAction.ExecuteTarget(Target: TObject);
begin
  with DataEngine do
  try
    DisableControls (DataComponent);
    MoveBy (DataComponent,BlockSize);
  finally
    EnableControls(DataComponent);
  end;
end;

//=== { TJvDatabaseRefreshAction } ==================================================
constructor TJvDatabaseRefreshAction.Create (AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FRefreshLastPosition := true;
  FRefreshAsOpenClose := false;
end;

procedure TJvDatabaseRefreshAction.ExecuteTarget(Target: TObject);
begin
  Refresh;
end;

procedure TJvDatabaseRefreshAction.Refresh;
var MyBookmark: TBookmark;
begin
  With DataEngine.GetDataset(DataComponent) do
  begin
    MyBookmark := nil;
    if RefreshLastPosition then
      MyBookmark := GetBookMark;

    try
      if RefreshAsOpenClose then
      begin
        close;
        open;
      end
      else
        Refresh;
        
      if RefreshLastPosition then
        if Active then
          if Assigned (MyBookMark) then
            if BookMarkValid (MyBookMark) then
              try
                GotoBookMark(MyBookMark);
              except
              end;
    finally
      if RefreshLastPosition then
        FreeBookmark(MyBookmark);
    end;
  end;
end;

//=== { TJvDatabasePositionAction } ==================================================
procedure TJvDatabasePositionAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and
     not EngineControlsDisabled and
     EngineIsActive  and
     EngineHasData then
    SetEnabled (true)
  else
    SetEnabled (false);
  try
    if not (EngineIsActive) then
      Caption := Format(' %3d / %3d ', [0,0])
    else if (EngineRecordCount = 0) then
      Caption := Format(' %3d / %3d ', [0,0])
    else
      Caption := Format(' %3d / %3d ', [EngineRecNo+1,EngineRecordCount]);
  except
    on e:exception do
      Caption := Format(' %3d / %3d ', [0,0])
  end;

end;

procedure TJvDatabasePositionAction.ExecuteTarget(Target: TObject);
begin
  ShowPositionDialog;
end;

procedure TJvDatabasePositionAction.ShowPositionDialog;
var ParameterList : TJvParameterList;
    Parameter : TJvBaseParameter;
    s : String;
    Kind : Integer;
begin
  if not Assigned(Dataset) then
    exit;
  ParameterList := TJvParameterList.Create(self);
  try
    Parameter:= TJvBaseParameter(tJvEditParameter.Create(ParameterList));
    with tJvEditParameter(Parameter) do
    begin
      SearchName := 'CurrentPosition';
      ReadOnly := TRUE;
      Caption := RsDBPosCurrentPosition;
      AsString := inttostr(EngineRecNo+1)+' / '+inttostr(EngineRecordCount);
      Width := 150;
      LabelWidth := 80;
      Enabled := False;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter:= TJvBaseParameter(tJvEditParameter.Create(ParameterList));
    with tJvEditParameter(Parameter) do
    begin
      Caption := RsDBPosNewPosition;
      SearchName := 'NewPosition';
     // EditMask := '999999999;0;_';
      Width := 150;
      LabelWidth := 80;
    end;
    ParameterList.AddParameter(Parameter);
    Parameter:= TJvBaseParameter(TJvRadioGroupParameter.Create(ParameterList));
    with TJvRadioGroupParameter(Parameter) do
    begin
      Caption := RsDBPosMovementType;
      SearchName := 'Kind';
      Width := 305;
      Height := 54;
      Columns := 2;
      ItemList.Add (RsDBPosAbsolute);
      ItemList.Add (RsDBPosForward);
      ItemList.Add (RsDBPosBackward);
      ItemList.Add (RsDBPosPercental);
      ItemIndex := 0;
    end;
    ParameterList.AddParameter(Parameter);
    ParameterList.ArrangeSettings.WrapControls := True;
    ParameterList.ArrangeSettings.MaxWidth := 350;
    ParameterList.Messages.Caption := RsDBPosDialogCaption;
    if ParameterList.ShowParameterDialog then
    begin
      s := ParameterList.ParameterbyName ('NewPosition').AsString;
      if s = '' then
        Exit;
      Kind := TJvRadioGroupParameter(ParameterList.ParameterbyName ('Kind')).ItemIndex;
      DataSet.DisableControls;
      try
        CASE Kind OF
          0 : Begin
                Dataset.First;
                Dataset.MoveBy (strtoint(s)-1);
              end;
          1 : Dataset.MoveBy (strtoint(s));
          2 : Dataset.MoveBy (strtoint(s)*-1);
          3 : Begin
                Dataset.First;
                Dataset.MoveBy(Round ((EngineRecordCount/100)*strtoint(s))-1);
              end;
        end;   {*** CASE Kind OF***}
      finally
        DataSet.EnableControls;
      end;
    end;   {*** if ParameterList.ShowParameterDialog then ***}
  finally
    ParameterList.Free;
  end;
end;

//=== { TJvDatabaseInsertAction } ==================================================
procedure TJvDatabaseInsertAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and
     not EngineControlsDisabled and
     EngineIsActive and
     EngineCanModify and
     not EngineEditModeActive then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseInsertAction.ExecuteTarget(Target: TObject);
begin
  Dataset.Insert;
end;

//=== { TJvDatabaseCopyAction } ==================================================
procedure TJvDatabaseCopyAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and
     not EngineControlsDisabled and
     EngineIsActive and
     EngineCanModify and
     EngineHasData and
     not EngineEditModeActive then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseCopyAction.ExecuteTarget(Target: TObject);
begin
  CopyRecord;
end;

Procedure TJvDatabaseCopyAction.CopyRecord;
var VALUES : array of Variant;
    i : Integer;
    Value : Variant;
    Allowed : Boolean;
begin
  with DataSet do
  begin
    if not Active then
      Exit;
    if State  in [dsInsert, dsEdit] then
      Post;
    if State <> dsBrowse then
      Exit;
    Allowed := True;
  end;
  if assigned (fBeforeCopyRecord) then
    fBeforeCopyRecord(Dataset, Allowed);
  with DataSet do
  begin
    if not Allowed then
      Exit;
    SetLength (Values, FieldCount);
    for i := 0 TO FieldCount-1 do
      Values [i] := Fields[i].AsVariant;
    Insert;
    if Assigned (fOnCopyRecord) then
      for i := 0 TO FieldCount-1 do
      begin
        Value := Values[i];
        fOnCopyRecord (Fields[i], Value);
      end   {*** for i := 0 TO FieldCount-1 do ***}
    else
      for i := 0 TO FieldCount-1 do
        Fields[i].AsVariant := Values[i];
  end;   {*** with DataSource.DataSet do ***}
  if assigned (fAfterCopyRecord) then
    fAfterCopyRecord(DataSet);
end;   {*** Procedure TJvDatabaseCopyAction.CopyRecord ***}

//=== { TJvDatabaseEditAction } ==================================================
procedure TJvDatabaseEditAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and
     not EngineControlsDisabled and
     EngineIsActive and
     EngineCanModify and
     EngineHasData and
     not EngineEditModeActive then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseEditAction.ExecuteTarget(Target: TObject);
begin
  Dataset.Edit;
end;

//=== { TJvDatabaseDeleteAction } ==================================================
procedure TJvDatabaseDeleteAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and
     not EngineControlsDisabled and
     EngineIsActive and
     EngineCanModify and
     EngineHasData and
     not EngineEditModeActive then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseDeleteAction.ExecuteTarget(Target: TObject);
begin
  Dataset.Delete;
end;

//=== { TJvDatabasePostAction } ==================================================
procedure TJvDatabasePostAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and
     not EngineControlsDisabled and
     EngineIsActive and
     EngineEditModeActive then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabasePostAction.ExecuteTarget(Target: TObject);
begin
  Dataset.Post;
end;

//=== { TJvDatabaseCancelAction } ==================================================
procedure TJvDatabaseCancelAction.UpdateTarget(Target: TObject);
begin
  if Assigned(Dataset) and
     not EngineControlsDisabled and
     EngineIsActive and
     EngineEditModeActive then
    SetEnabled (true)
  else
    SetEnabled (false);
end;

procedure TJvDatabaseCancelAction.ExecuteTarget(Target: TObject);
begin
  Dataset.Cancel;
end;

//=== { TJvDatabaseSingleRecordWindowAction } ==================================================
constructor TJvDatabaseSingleRecordWindowAction.Create (AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FOptions := TJvShowSingleRecordWindowOptions.Create;
end;

destructor TJvDatabaseSingleRecordWindowAction.Destroy;
begin
  FOptions.Free;
  Inherited Destroy;
end;

procedure TJvDatabaseSingleRecordWindowAction.ExecuteTarget(Target: TObject);
begin
  DataEngine.ShowSingleRecordWindow(Options, DataComponent);
end;


//=== { TJvDatabaseOpenAction } ==================================================
procedure TJvDatabaseOpenAction.UpdateTarget(Target: TObject);
begin
  If Assigned(Dataset) and Not EngineIsActive then
    SetEnabled (true)
  else
    SetEnabled (False);
end;

procedure TJvDatabaseOpenAction.ExecuteTarget(Target: TObject);
begin
  Dataset.Open;
end;

//=== { TJvDatabaseCloseAction } ==================================================
procedure TJvDatabaseCloseAction.UpdateTarget(Target: TObject);
begin
  If Assigned(Dataset) and EngineIsActive and
     Not EngineEditModeActive then
    SetEnabled (true)
  else
    SetEnabled (False);
end;

procedure TJvDatabaseCloseAction.ExecuteTarget(Target: TObject);
begin
  Dataset.Close;
end;

//=== { TJvDatabaseActionEngineList } ==================================================
destructor TJvDatabaseActionEngineList.Destroy;
begin
  while Count > 0 do
  begin
    TJvDatabaseActionBaseEngine(Items[0]).Free;
    Delete (0);
  end;
  Inherited Destroy;
end;

procedure TJvDatabaseActionEngineList.RegisterEngine (AEngineClass : TJvDatabaseActionBaseEngineClass);
begin
  Add (AEngineClass.Create(nil));
end;

function TJvDatabaseActionEngineList.GetEngine (AComponent: TComponent) : TJvDatabaseActionBaseEngine;
var
  Ind : Integer;
begin
  Result := nil;
  for Ind := 0 to Count -1 do
    if TJvDatabaseActionBaseEngine(Items[Ind]).Supports(AComponent) then
    begin
      Result := TJvDatabaseActionBaseEngine(Items[Ind]);
      break;
    end;
end;

//=== { Global } ==================================================
procedure RegisterActionEngine (AEngineClass : TJvDatabaseActionBaseEngineClass);
begin
  if Assigned(RegisteredActionEngineList) then
    RegisteredActionEngineList.RegisterEngine(AEngineClass);
end;


procedure CreateActionEngineList;
begin
  RegisteredActionEngineList := TJvDatabaseActionEngineList.Create;
end;

procedure DestroyActionEngineList;
begin
  if Assigned(RegisteredActionEngineList) then
    RegisteredActionEngineList.Free;
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

initialization
  CreateActionEngineList;
  RegisterActionEngine (TJvDatabaseActionBaseEngine);
  RegisterActionEngine (TJvDatabaseActionDBGridEngine);

{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}

finalization
{$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
  DestroyActionEngineList;

end.

