{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActionsEngine.Pas, released on 2004-12-30.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBActionsEngine;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Graphics,
  Forms, Controls, Classes, DB,
  JvActionsEngine,
  DBGrids, JvPanel, JvDynControlEngineDB, JvDynControlEngineDBTools;

type
  TJvDatabaseActionBaseEngine = class(TJvActionBaseEngine)
  private
  public
  end;

  TJvDatabaseActionBaseDatasetEngine = class(TJvDatabaseActionBaseEngine)
  private
  protected
  public
    function GetDataset(aActionComponent: TComponent): TDataset; virtual;
    function GetSQL(aActionComponent: TComponent): string; virtual;
    procedure RefreshRecord(AActionComponent : TComponent); virtual;
    procedure SetSQL(aActionComponent: TComponent); virtual;
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
    function SupportsGetSQL(aActionComponent: TComponent): Boolean; virtual;
    function SupportsRefreshRecord(aActionComponent: TComponent): Boolean; virtual;
    function SupportsSetSQL(aActionComponent: TComponent): Boolean; virtual;
  end;

  TJvShowSingleRecordWindowOptions = class(TPersistent)
  private
    FDialogCaption: string;
    FPostButtonCaption: string;
    FCancelButtonCaption: string;
    FCloseButtonCaption: string;
    FBorderStyle: TFormBorderStyle;
    FPosition: TPosition;
    FTop: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FArrangeConstraints: TJvDynControlSizeConstraints;
    FArrangeSettings: TJvArrangeSettings;
    FFieldCreateOptions: TJvCreateDBFieldsOnControlOptions;
    FIncludeNavigator: Boolean;
  protected
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
    procedure SetArrangeConstraints(Value: TJvDynControlSizeConstraints);
    procedure SetFieldCreateOptions(Value: TJvCreateDBFieldsOnControlOptions);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetOptionsToDialog(ADialog: TJvDynControlDataSourceEditDialog);
  published
    property DialogCaption: string read FDialogCaption write FDialogCaption;
    property PostButtonCaption: string read FPostButtonCaption write FPostButtonCaption;
    property CancelButtonCaption: string read FCancelButtonCaption write FCancelButtonCaption;
    property CloseButtonCaption: string read FCloseButtonCaption write FCloseButtonCaption;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle default bsDialog;
    property Position: TPosition read FPosition write FPosition default {$IFDEF COMPILER7_UP} poOwnerFormCenter {$ELSE} poScreenCenter{$ENDIF COMPILER7_UP};
    property Top: Integer read FTop write FTop default 0;
    property Left: Integer read FLeft write FLeft default 0;
    property Width: Integer read FWidth write FWidth default 640;
    property Height: Integer read FHeight write FHeight default 480;
    property ArrangeConstraints: TJvDynControlSizeConstraints read FArrangeConstraints write SetArrangeConstraints;
    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write SetArrangeSettings;
    property FieldCreateOptions: TJvCreateDBFieldsOnControlOptions read FFieldCreateOptions
      write SetFieldCreateOptions;
    property IncludeNavigator: Boolean read FIncludeNavigator write
        FIncludeNavigator default false;
  end;

  TJvDatabaseActionBaseControlEngine = class(TJvDatabaseActionBaseEngine)
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function Bof(aActionComponent: TComponent): Boolean; virtual;
    function CanDelete(aActionComponent: TComponent): Boolean; virtual;
    function CanInsert(aActionComponent: TComponent): Boolean; virtual;
    function CanNavigate(aActionComponent: TComponent): Boolean; virtual;
    function CanRefresh(aActionComponent: TComponent): Boolean; virtual;
    function CanRefreshRecord(aActionComponent: TComponent): Boolean; virtual;
    function CanUpdate(aActionComponent: TComponent): Boolean; virtual;
    function ControlsDisabled(aActionComponent: TComponent): Boolean; virtual;
    procedure DisableControls(aActionComponent: TComponent); virtual;
    function EditModeActive(aActionComponent: TComponent): Boolean; virtual;
    procedure EnableControls(aActionComponent: TComponent); virtual;
    function EOF(aActionComponent: TComponent): Boolean; virtual;
    function FieldById(aActionComponent: TComponent; const FieldId: Integer): TField; virtual;
    function FieldByName(aActionComponent: TComponent; const FieldName: string): TField; virtual;
    function FieldCount(aActionComponent: TComponent): Integer; virtual;
    procedure FillFieldList(aActionComponent: TComponent; var AFieldList: TStrings; const AOnlyVisible: Boolean); virtual;
    procedure First(aActionComponent: TComponent); virtual;
    function DataSet(aActionComponent: TComponent): TDataSet; virtual;
    function DataSource(aActionComponent: TComponent): TDataSource; virtual;
    function GotoSelectedRow(aActionComponent: TComponent;const ASelectedRow: Integer): Boolean; virtual;
    function HasData(aActionComponent: TComponent): Boolean; virtual;
    function IsActive(aActionComponent: TComponent): Boolean; virtual;
    function IsFieldReadOnly(aActionComponent: TComponent;const AFieldName: string): Boolean; virtual;
    function IsFieldVisible(aActionComponent: TComponent; const AFieldName: string): Boolean; virtual;
    procedure Last(aActionComponent: TComponent); virtual;
    procedure MoveBy(aActionComponent: TComponent; Distance: Integer); virtual;
    function RecNo(aActionComponent: TComponent): Integer; virtual;
    function RecordCount(aActionComponent: TComponent): Integer; virtual;
    function SelectedField(aActionComponent: TComponent): TField; virtual;
    function SelectedRowsCount(aActionComponent: TComponent): Integer; virtual;
    procedure ShowSingleRecordWindow(aActionComponent: TComponent; AOptions: TJvShowSingleRecordWindowOptions;
        ACreateDataControlsEvent: TJvDataSourceEditDialogCreateDataControlsEvent = nil; AOnFormShowEvent:
        TJvDataSourceEditDialogOnFormShowEvent = nil); virtual;
    function SupportsAction(AAction: TJvActionEngineBaseAction): Boolean; override;
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
  end;

  TJvDatabaseActionBaseEngineClass = class of TJvDatabaseActionBaseEngine;

  TJvDatabaseActionDBGridControlEngine = class(TJvDatabaseActionBaseControlEngine)
  private
  protected
    function CustomDBGrid(aActionComponent: TComponent): TCustomDBGrid; virtual;
    procedure OnCreateDataControls(ADatacomponent: TComponent; ADynControlEngineDB:
        TJvDynControlEngineDB; AParentControl: TWinControl; AFieldCreateOptions:
        TJvCreateDBFieldsOnControlOptions);
  public
    constructor Create(AOwner: TComponent); override;
    function DataSource(aActionComponent: TComponent): TDataSource; override;
    function GotoSelectedRow(aActionComponent: TComponent; const ASelectedRow:
        Integer): Boolean; override;
    function SelectedField(aActionComponent: TComponent): TField; override;
    function SelectedRowsCount(aActionComponent: TComponent): Integer; override;
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
    procedure ShowSingleRecordWindow(aActionComponent: TComponent; AOptions:
        TJvShowSingleRecordWindowOptions; ACreateDataControlsEvent:
        TJvDataSourceEditDialogCreateDataControlsEvent = nil; AOnFormShowEvent:
        TJvDataSourceEditDialogOnFormShowEvent = nil); override;
  end;

  TJvDatabaseActionEngineList = class(TJvActionEngineList)
  public
    procedure RegisterEngine(AEngineClass: TJvDatabaseActionBaseEngineClass);
    function GetDatasetEngine(AComponent: TComponent): TJvDatabaseActionBaseDatasetEngine;
    function GetDatabaseControlEngine(AComponent: TComponent):
        TJvDatabaseActionBaseControlEngine;
    function SupportsDataset(AComponent: TComponent): Boolean;
  end;

procedure RegisterDatabaseActionEngine(AEngineClass:
    TJvDatabaseActionBaseEngineClass);

function RegisteredDatabaseActionEngineList: TJvDatabaseActionEngineList;

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
  SysUtils, Grids, TypInfo,
  JvResources,
  Variants, JvDBActions, Dialogs;

var
  IntRegisteredActionEngineList: TJvDatabaseActionEngineList;

constructor TJvDatabaseActionBaseControlEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TJvDatabaseActionBaseControlEngine.Bof(aActionComponent: TComponent): Boolean;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).Bof
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.CanDelete(aActionComponent: TComponent): Boolean;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).CanModify
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.CanInsert(aActionComponent: TComponent): Boolean;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).CanModify
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.CanNavigate(aActionComponent: TComponent): Boolean;
begin
  Result := Assigned(DataSet(aActionComponent));
end;

function TJvDatabaseActionBaseControlEngine.CanRefresh(aActionComponent: TComponent): Boolean;
begin
  Result := Assigned(DataSet(aActionComponent));
end;

function TJvDatabaseActionBaseControlEngine.CanRefreshRecord(aActionComponent: TComponent): Boolean;
begin
  Result := Assigned(DataSet(aActionComponent));
end;

function TJvDatabaseActionBaseControlEngine.CanUpdate(aActionComponent: TComponent): Boolean;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).CanModify
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.ControlsDisabled(aActionComponent: TComponent): Boolean;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).ControlsDisabled
  else
    Result := False;
end;

procedure TJvDatabaseActionBaseControlEngine.DisableControls(aActionComponent: TComponent);
begin
  if Assigned(DataSet(aActionComponent)) then
    DataSet(aActionComponent).DisableControls;
end;

function TJvDatabaseActionBaseControlEngine.EditModeActive(aActionComponent: TComponent): Boolean;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).State in [dsInsert, dsEdit]
  else
    Result := False;
end;

procedure TJvDatabaseActionBaseControlEngine.EnableControls(aActionComponent: TComponent);
begin
  if Assigned(DataSet(aActionComponent)) then
    DataSet(aActionComponent).EnableControls;
end;

function TJvDatabaseActionBaseControlEngine.EOF(aActionComponent: TComponent): Boolean;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).EOF
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.FieldById(aActionComponent: TComponent; const FieldId: Integer): TField;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).Fields[FieldId]
  else
    Result := nil;
end;

function TJvDatabaseActionBaseControlEngine.FieldByName(aActionComponent: TComponent; const FieldName: string): TField;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).FieldByName(FieldName)
  else
    Result := nil;
end;

function TJvDatabaseActionBaseControlEngine.FieldCount(aActionComponent: TComponent): Integer;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).FieldCount
  else
    Result := -1;
end;

procedure TJvDatabaseActionBaseControlEngine.FillFieldList(aActionComponent: TComponent; var AFieldList: TStrings;
    const AOnlyVisible: Boolean);
var
  I: Integer;
  ds : TDataset;
begin
  AFieldList.Clear;
  ds := DataSet(aActionComponent);
  if Assigned(ds) then
  begin
    for I := 0 to ds.Fields.Count - 1 do
      if not AOnlyVisible or IsFieldVisible(aActionComponent,ds.Fields[I].FieldName) then
        AFieldList.Add(ds.Fields[I].FieldName);
  end;
end;

procedure TJvDatabaseActionBaseControlEngine.First(aActionComponent: TComponent);
begin
  if Assigned(DataSet(aActionComponent)) then
    DataSet(aActionComponent).First;
end;

function TJvDatabaseActionBaseControlEngine.DataSet(aActionComponent: TComponent): TDataSet;
begin
  if Assigned(DataSource(aActionComponent)) then
    Result := DataSource(aActionComponent).DataSet
  else if Assigned(aActionComponent) and (aActionComponent is TDataSet) then
    Result := TDataSet(aActionComponent)
  else
    Result := nil;
end;

function TJvDatabaseActionBaseControlEngine.DataSource(aActionComponent: TComponent): TDataSource;
begin
  if Assigned(aActionComponent) and (aActionComponent is TDataSource) then
    Result := TDataSource(aActionComponent)
  else
    Result := nil;
end;

function TJvDatabaseActionBaseControlEngine.GotoSelectedRow(aActionComponent: TComponent;const ASelectedRow: Integer):
    Boolean;
begin
  Result := False;
end;

function TJvDatabaseActionBaseControlEngine.HasData(aActionComponent: TComponent): Boolean;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).RecordCount > 0
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.IsActive(aActionComponent: TComponent): Boolean;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).Active
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.IsFieldReadOnly(aActionComponent: TComponent;const AFieldName: string):
    Boolean;
var
  Field: TField;
begin
  Field := FieldByName(aActionComponent, aFieldName);
  if Assigned(Field) then
    Result := Field.ReadOnly
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.IsFieldVisible(aActionComponent: TComponent; const AFieldName: string):
    Boolean;
var
  Field: TField;
begin
  Field := FieldByName(aActionComponent, AFieldName);
  if Assigned(Field) then
    Result := Field.Visible
  else
    Result := False;
end;

procedure TJvDatabaseActionBaseControlEngine.Last(aActionComponent: TComponent);
begin
  if Assigned(DataSet(aActionComponent)) then
    DataSet(aActionComponent).Last;
end;

procedure TJvDatabaseActionBaseControlEngine.MoveBy(aActionComponent: TComponent; Distance: Integer);
begin
  if Assigned(DataSet(aActionComponent)) then
    DataSet(aActionComponent).MoveBy(Distance);
end;

function TJvDatabaseActionBaseControlEngine.RecNo(aActionComponent: TComponent): Integer;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).RecNo
  else
    Result := -1;
end;

function TJvDatabaseActionBaseControlEngine.RecordCount(aActionComponent: TComponent): Integer;
begin
  if Assigned(DataSet(aActionComponent)) then
    Result := DataSet(aActionComponent).RecordCount
  else
    Result := -1;
end;

function TJvDatabaseActionBaseControlEngine.SelectedField(aActionComponent: TComponent): TField;
begin
  Result := nil;
end;

function TJvDatabaseActionBaseControlEngine.SelectedRowsCount(aActionComponent: TComponent): Integer;
begin
  Result := 0;
end;

procedure TJvDatabaseActionBaseControlEngine.ShowSingleRecordWindow(aActionComponent: TComponent; AOptions:
    TJvShowSingleRecordWindowOptions; ACreateDataControlsEvent: TJvDataSourceEditDialogCreateDataControlsEvent = nil;
    AOnFormShowEvent: TJvDataSourceEditDialogOnFormShowEvent = nil);
var
  Dialog: TJvDynControlDataSourceEditDialog;
begin
  Dialog := TJvDynControlDataSourceEditDialog.Create(Self);
  try
    if Dialog.DynControlEngineDB.SupportsDataComponent(aActionComponent) then
      Dialog.DataComponent := aActionComponent
    else
      Dialog.DataComponent := DataSource(aActionComponent);
    Dialog.OnCreateDataControlsEvent := ACreateDataControlsEvent;
    Dialog.OnFormShowEvent := AOnFormShowEvent;
    AOptions.SetOptionsToDialog(Dialog);
    if Assigned(Dialog.DataComponent) then
      Dialog.ShowDialog;
  finally
    Dialog.Free;
  end;
end;

function TJvDatabaseActionBaseControlEngine.SupportsAction(AAction: TJvActionEngineBaseAction): Boolean;
begin
  Result := (AAction is TJvDatabaseBaseAction) ;
end;

function TJvDatabaseActionBaseControlEngine.SupportsComponent(aActionComponent: TComponent): Boolean;
begin
  Result := Assigned(aActionComponent) and (aActionComponent is TDataSource);
end;

function TJvDatabaseActionDBGridControlEngine.DataSource(aActionComponent:
    TComponent): TDataSource;
begin
  if Assigned(aActionComponent) and (aActionComponent is TCustomDBGrid) then
    Result := TCustomDBGrid(aActionComponent).DataSource
  else
    Result := nil;
end;

type
  TAccessCustomDBGrid = class(TCustomDBGrid);
  TAccessCustomControl = class(TCustomControl);

constructor TJvDatabaseActionDBGridControlEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TJvDatabaseActionDBGridControlEngine.CustomDBGrid(aActionComponent:
    TComponent): TCustomDBGrid;
begin
  if Assigned(aActionComponent) and (aActionComponent is TCustomDBGrid) then
    Result := TCustomDBGrid(aActionComponent)
  else
    Result := nil;
end;

function TJvDatabaseActionDBGridControlEngine.GotoSelectedRow(aActionComponent:
    TComponent; const ASelectedRow: Integer): Boolean;
var
  ds : TDataSet;
begin
  ds := Dataset(aActionComponent);
  if (ASelectedRow >= 0) and (ASelectedRow < SelectedRowsCount(aActionComponent)) and
    Assigned(ds) and ds.Active then
  begin
    ds.GotoBookmark({$IFNDEF RTL200_UP}Pointer{$ENDIF ~RTL200_UP}(TAccessCustomDBGrid(CustomDBGrid(aActionComponent)).SelectedRows[ASelectedRow]));
    Result := True;
  end
  else
    Result := False;
end;

procedure TJvDatabaseActionDBGridControlEngine.OnCreateDataControls(
    ADatacomponent: TComponent; ADynControlEngineDB: TJvDynControlEngineDB;
    AParentControl: TWinControl; AFieldCreateOptions:
    TJvCreateDBFieldsOnControlOptions);
var
  I: Integer;
  ds: TDataSource;
  Field: TField;
  LabelControl: TControl;
  Control: TWinControl;
  Column: TColumn;
  grid : TCustomDBGrid;
begin
  grid := CustomDBGrid(ADatacomponent);
  if Assigned(grid) then
  begin
    ds := grid.DataSource;
    for I := 0 to TAccessCustomDBGrid(grid).ColCount - 2 do
    begin
      Column := TAccessCustomDBGrid(grid).Columns[I];
      if Column.Visible or AFieldCreateOptions.ShowInvisibleFields then
      begin
        Field := Column.Field;
        Control := ADynControlEngineDB.CreateDBFieldControl(Field, AParentControl, AParentControl, '', ds);
        Control.Enabled := Field.CanModify;
        if AFieldCreateOptions.FieldDefaultWidth > 0 then
          Control.Width := AFieldCreateOptions.FieldDefaultWidth
        else
        begin
          if AFieldCreateOptions.UseFieldSizeForWidth then
            if Field.Size > 0 then
              Control.Width :=
                TAccessCustomControl(AParentControl).Canvas.TextWidth('X') * Field.Size
            else
            begin
              if (ADynControlEngineDB.GetFieldControlType(Field)= jctDBMemo) and
               (AFieldCreateOptions.FieldMaxWidth > 0) then
                Control.Width := AFieldCreateOptions.FieldMaxWidth;
            end
          else
            if Field.DisplayWidth > 0 then
              Control.Width :=
                TAccessCustomControl(AParentControl).Canvas.TextWidth('X') * Field.DisplayWidth;
          if (AFieldCreateOptions.FieldMaxWidth > 0) and (Control.Width > AFieldCreateOptions.FieldMaxWidth) then
            Control.Width := AFieldCreateOptions.FieldMaxWidth
          else
            if (AFieldCreateOptions.FieldMinWidth > 0) and (Control.Width < AFieldCreateOptions.FieldMinWidth) then
              Control.Width := AFieldCreateOptions.FieldMinWidth;
        end;
        if AFieldCreateOptions.UseParentColorForReadOnly then
          if (Assigned(ds.DataSet) and not ds.DataSet.CanModify) or not Field.CanModify then
            if isPublishedProp(Control, 'ParentColor') then
              SetOrdProp(Control, 'ParentColor', Ord(True));
        LabelControl := ADynControlEngineDB.DynControlEngine.CreateLabelControlPanel(AParentControl,
          AParentControl, '', '&' + Column.Title.Caption, Control, True, 0);
        if AFieldCreateOptions.FieldWidthStep > 0 then
          if (LabelControl.Width mod AFieldCreateOptions.FieldWidthStep) <> 0 then
            LabelControl.Width := ((LabelControl.Width div AFieldCreateOptions.FieldWidthStep) + 1) * AFieldCreateOptions.FieldWidthStep;
      end;
    end;
  end;
end;

function TJvDatabaseActionDBGridControlEngine.SelectedField(aActionComponent:
    TComponent): TField;
begin
  if Assigned(CustomDBGrid(aActionComponent)) then
    Result := CustomDBGrid(aActionComponent).SelectedField
  else
    Result := nil;
end;

function TJvDatabaseActionDBGridControlEngine.SelectedRowsCount(
    aActionComponent: TComponent): Integer;
begin
  if Assigned(CustomDBGrid(aActionComponent)) then
    Result := TAccessCustomDBGrid(CustomDBGrid(aActionComponent)).SelectedRows.Count
  else
    Result := 0;
end;

function TJvDatabaseActionDBGridControlEngine.SupportsComponent(
    aActionComponent: TComponent): Boolean;
begin
  Result := Assigned(aActionComponent) and (aActionComponent is TCustomDBGrid);
end;

procedure TJvDatabaseActionDBGridControlEngine.ShowSingleRecordWindow(
    aActionComponent: TComponent; AOptions: TJvShowSingleRecordWindowOptions;
    ACreateDataControlsEvent: TJvDataSourceEditDialogCreateDataControlsEvent =
    nil; AOnFormShowEvent: TJvDataSourceEditDialogOnFormShowEvent = nil);
var
  Dialog: TJvDynControlDataSourceEditDialog;
begin
  Dialog := TJvDynControlDataSourceEditDialog.Create(self);
  try
    AOptions.SetOptionsToDialog(Dialog);
    if Dialog.DynControlEngineDB.SupportsDataComponent(aActionComponent) then
      Dialog.DataComponent := aActionComponent
    else
      Dialog.DataComponent := DataSource(aActionComponent);
    if Assigned(Dialog.DataComponent) then
    begin
      if not Assigned(ACreateDataControlsEvent) then
        Dialog.OnCreateDataControlsEvent := OnCreateDataControls
      else
        Dialog.OnCreateDataControlsEvent := ACreateDataControlsEvent;
      Dialog.OnFormShowEvent := AOnFormShowEvent;
      Dialog.ShowDialog;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TJvDatabaseActionEngineList.RegisterEngine(AEngineClass: TJvDatabaseActionBaseEngineClass);
begin
  Add(AEngineClass.Create(nil));
end;

function TJvDatabaseActionEngineList.GetDatasetEngine(AComponent: TComponent): TJvDatabaseActionBaseDatasetEngine;
var
  Ind: Integer;
begin
  Result := nil;
  for Ind := 0 to Count - 1 do
    if Engine[Ind] is TJvDatabaseActionBaseDatasetEngine then
      if TJvDatabaseActionBaseDatasetEngine(Engine[Ind]).SupportsComponent(AComponent) then
      begin
        Result := TJvDatabaseActionBaseDatasetEngine(Engine[Ind]);
        Break;
      end;
end;

function TJvDatabaseActionEngineList.GetDatabaseControlEngine(AComponent:
    TComponent): TJvDatabaseActionBaseControlEngine;
var
  Ind: Integer;
begin
  Result := nil;
  for Ind := 0 to Count - 1 do
    if Engine[Ind] is TJvDatabaseActionBaseControlEngine then
      if TJvDatabaseActionBaseControlEngine(Engine[Ind]).SupportsComponent(AComponent) then
      begin
        Result := TJvDatabaseActionBaseControlEngine(Engine[Ind]);
        Break;
      end;
end;

function TJvDatabaseActionEngineList.SupportsDataset(AComponent: TComponent): Boolean;
begin
  Result := Assigned(GetDatasetEngine(AComponent));
end;

function TJvDatabaseActionBaseDatasetEngine.GetDataset(aActionComponent:
    TComponent): TDataset;
begin
  if aActionComponent is TDataset then
    Result := TDataset(aActionComponent)
  else
    Result := NIL;
end;

function TJvDatabaseActionBaseDatasetEngine.GetSQL(aActionComponent:
    TComponent): string;
begin
  Result := '';
end;

procedure TJvDatabaseActionBaseDatasetEngine.RefreshRecord(AActionComponent :
    TComponent);
begin
  
end;

procedure TJvDatabaseActionBaseDatasetEngine.SetSQL(aActionComponent:
    TComponent);
begin

end;

function TJvDatabaseActionBaseDatasetEngine.SupportsComponent(aActionComponent:
    TComponent): Boolean;
begin
  Result := False;
end;

function TJvDatabaseActionBaseDatasetEngine.SupportsGetSQL(aActionComponent:
    TComponent): Boolean;
begin
  Result := False;
end;

function TJvDatabaseActionBaseDatasetEngine.SupportsRefreshRecord(
    aActionComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TJvDatabaseActionBaseDatasetEngine.SupportsSetSQL(aActionComponent:
    TComponent): Boolean;
begin
  Result := False;
end;

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
  {$IFDEF COMPILER7_UP}
  FPosition := poOwnerFormCenter;
  {$ELSE}
  FPosition := poScreenCenter;
  {$ENDIF COMPILER7_UP};  
  FArrangeSettings := TJvArrangeSettings.Create(Self);
  FArrangeSettings.AutoSize := asBoth;
  FArrangeSettings.DistanceHorizontal := 3;
  FArrangeSettings.DistanceVertical := 3;
  FArrangeSettings.BorderLeft := 3;
  FArrangeSettings.BorderTop := 3;
  FArrangeSettings.WrapControls := True;
  FArrangeConstraints := TJvDynControlSizeConstraints.Create;
  FArrangeConstraints.MaxHeight := 480;
  FArrangeConstraints.MaxWidth := 640;
  FFieldCreateOptions := TJvCreateDBFieldsOnControlOptions.Create;
  FIncludeNavigator := false;
end;

destructor TJvShowSingleRecordWindowOptions.Destroy;
begin
  FFieldCreateOptions.Free;
  FArrangeConstraints.Free;
  FArrangeSettings.Free;
  inherited Destroy;
end;

procedure TJvShowSingleRecordWindowOptions.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
end;

procedure TJvShowSingleRecordWindowOptions.SetArrangeConstraints(Value: TJvDynControlSizeConstraints);
begin
  FArrangeConstraints.Assign(Value);
end;

procedure TJvShowSingleRecordWindowOptions.SetFieldCreateOptions(Value: TJvCreateDBFieldsOnControlOptions);
begin
  FFieldCreateOptions.Assign(Value);
end;

procedure TJvShowSingleRecordWindowOptions.SetOptionsToDialog(ADialog: TJvDynControlDataSourceEditDialog);
begin
  if Assigned(ADialog) then
  begin
    ADialog.DialogCaption := DialogCaption;
    ADialog.PostButtonCaption := PostButtonCaption;
    ADialog.CancelButtonCaption := CancelButtonCaption;
    ADialog.CloseButtonCaption := CloseButtonCaption;
    ADialog.Position := Position;
    ADialog.BorderStyle := BorderStyle;
    ADialog.Top := Top;
    ADialog.Left := Left;
    ADialog.Width := Width;
    ADialog.Height := Height;
    ADialog.ArrangeConstraints := ArrangeConstraints;
    ADialog.ArrangeSettings := ArrangeSettings;
    ADialog.FieldCreateOptions := FieldCreateOptions;
    ADialog.IncludeNavigator := IncludeNavigator;
  end;
end;

function RegisteredDatabaseActionEngineList: TJvDatabaseActionEngineList;
begin
  Result := IntRegisteredActionEngineList;
end;

procedure RegisterDatabaseActionEngine(AEngineClass:
    TJvDatabaseActionBaseEngineClass);
begin
  if Assigned(IntRegisteredActionEngineList) then
    IntRegisteredActionEngineList.RegisterEngine(AEngineClass);
end;

procedure CreateActionEngineList;
begin
  IntRegisteredActionEngineList := TJvDatabaseActionEngineList.Create;
end;

procedure DestroyActionEngineList;
begin
  IntRegisteredActionEngineList.Free;
  IntRegisteredActionEngineList := nil;
end;

procedure InitActionEngineList;
begin
  CreateActionEngineList;
  RegisterDatabaseActionEngine(TJvDatabaseActionBaseControlEngine);
  RegisterDatabaseActionEngine(TJvDatabaseActionDBGridControlEngine);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  InitActionEngineList;

finalization
  DestroyActionEngineList;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

