{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActionsEngineControlCxGrid.Pas, released on 2004-12-30.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBActionsEngineControlCxGrid;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Forms, Controls, Classes, DB,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGridCustomTableView, cxDBData, cxGridCustomView,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  JvDBActionsEngine;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
type
  TJvDatabaseActionDevExpCxGridControlEngine = class(TJvDatabaseActionBaseControlEngine)
  private
    function DBDataController(AActionComponent: TComponent): TcxDBDataController;
  protected
    function GridView(AActionComponent: TComponent): TcxCustomGridView;
    function GridTableView(AActionComponent: TComponent): TcxCustomGridTableView;
    function IsGridMode(AActionComponent: TComponent): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function Bof(AActionComponent: TComponent): Boolean; override;
    function RecNo(AActionComponent: TComponent): Integer; override;
    function RecordCount(AActionComponent: TComponent): Integer; override;
    function CanInsert(AActionComponent: TComponent): Boolean; override;
    function CanUpdate(AActionComponent: TComponent): Boolean; override;
    function CanDelete(AActionComponent: TComponent): Boolean; override;
    function CanNavigate(AActionComponent: TComponent): Boolean; override;
    function DataSource(AActionComponent: TComponent): TDataSource; override;
    procedure First(AActionComponent: TComponent); override;
    function GotoSelectedRow(AActionComponent: TComponent; const ASelectedRow:
      Integer): Boolean; override;
    procedure Last(AActionComponent: TComponent); override;
    procedure MoveBy(AActionComponent: TComponent; Distance: Integer); override;
    function SelectedField(AActionComponent: TComponent): TField; override;
    function SelectedRowsCount(AActionComponent: TComponent): Integer; override;
    function SupportsComponent(AActionComponent: TComponent): Boolean; override;
  end;

  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}

  {$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile:
    '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
  {$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGrid, cxGridDBDataDefinitions, cxGridDBChartView,
  cxCustomData, cxGridTableView,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  Variants, SysUtils, Grids;

//=== { TJvDatabaseActionDevExpCxGridControlEngine } =========================

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}

constructor TJvDatabaseActionDevExpCxGridControlEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'TJvDatabaseActionDevExpCxGridControlEngine';
end;

function TJvDatabaseActionDevExpCxGridControlEngine.GridView(AActionComponent:
  TComponent): TcxCustomGridView;
begin
  if Assigned(AActionComponent) then
    if AActionComponent is TcxGrid then
      if TcxGrid(AActionComponent).FocusedView is TcxCustomGridView then
        Result := TcxCustomGridView(TcxGrid(AActionComponent).FocusedView)
      else
        Result := nil
    else if AActionComponent is TcxCustomGridView then
      Result := TcxCustomGridView(AActionComponent)
    else
      Result := nil
  else
    Result := nil;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.GridTableView(
  AActionComponent: TComponent): TcxCustomGridTableView;
begin
  if Assigned(AActionComponent) then
    if AActionComponent is TcxGrid then
      if TcxGrid(AActionComponent).FocusedView is TcxCustomGridTableView then
        Result := TcxCustomGridTableView(TcxGrid(AActionComponent).FocusedView)
      else
        Result := nil
    else if AActionComponent is TcxCustomGridTableView then
      Result := TcxCustomGridTableView(AActionComponent)
    else
      Result := nil
  else
    Result := nil;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.DataSource(
  AActionComponent: TComponent): TDataSource;
begin
  if Assigned(DBDataController(AActionComponent)) then
    Result := DBDataController(AActionComponent).DataSource
  else
    Result := inherited DataSource(AActionComponent)
end;

function TJvDatabaseActionDevExpCxGridControlEngine.SupportsComponent(
  AActionComponent: TComponent): Boolean;
begin
  Result := Assigned(GridView(AActionComponent));
end;

function TJvDatabaseActionDevExpCxGridControlEngine.Bof(AActionComponent:
  TComponent): Boolean;
begin
  if Assigned(GridView(AActionComponent)) then
    Result := GridView(AActionComponent).DataController.FocusedRowIndex = 0
  else
    Result := inherited Bof(AActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.RecNo(AActionComponent:
  TComponent): Integer;
begin
  if Assigned(GridView(AActionComponent)) then
    if GridView(AActionComponent).DataController.IsGridMode then
      Result := inherited RecNo(AActionComponent)
    else
      Result := GridView(AActionComponent).DataController.FocusedRowIndex + 1
  else
    Result := inherited RecNo(AActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.RecordCount(
  AActionComponent: TComponent): Integer;
begin
  if Assigned(GridView(AActionComponent)) then
    if GridView(AActionComponent).DataController.IsGridMode then
      Result := inherited RecordCount(AActionComponent)
    else
      Result := GridView(AActionComponent).DataController.FilteredRecordCount
  else
    Result := inherited RecordCount(AActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanInsert(AActionComponent:
  TComponent): Boolean;
begin
  if Assigned(GridTableView(AActionComponent)) then
    Result := GridTableView(AActionComponent).OptionsData.Inserting and inherited
      CanInsert(AActionComponent)
  else if Assigned(GridView(AActionComponent)) then
    Result := False // GridView must be a ChartView,  and then Insert makes no sense
  else
    Result := inherited CanInsert(AActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanUpdate(AActionComponent:
  TComponent): Boolean;
begin
  if Assigned(GridTableView(AActionComponent)) then
    Result := GridTableView(AActionComponent).OptionsData.Editing and inherited
      CanUpdate(AActionComponent)
  else if Assigned(GridView(AActionComponent)) then
    Result := False // GridView must be a ChartView,  and then Update makes no sense
  else
    Result := inherited CanUpdate(AActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanDelete(AActionComponent:
  TComponent): Boolean;
begin
  if Assigned(GridTableView(AActionComponent)) then
    Result := GridTableView(AActionComponent).OptionsData.Deleting and inherited
      CanDelete(AActionComponent)
  else if Assigned(GridView(AActionComponent)) then
    Result := False // GridView must be a ChartView,  and then Delete makes no sense
  else
    Result := inherited CanDelete(AActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanNavigate(
  AActionComponent: TComponent): Boolean;
begin
  Result := Assigned(GridTableView(AActionComponent));
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.First(AActionComponent:
  TComponent);
begin
  if Assigned(GridTableView(AActionComponent)) then
    GridTableView(AActionComponent).DataController.GotoFirst
  else
    inherited First(AActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.DBDataController(
  AActionComponent: TComponent): TcxDBDataController;

begin
  if Assigned(GridView(AActionComponent)) and (GridView(AActionComponent).DataController is
    TcxDBDataController) then
    Result := TcxDBDataController(GridView(AActionComponent).DataController)
  else
    Result := nil;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.GotoSelectedRow(
  AActionComponent: TComponent; const ASelectedRow: Integer): Boolean;
var
  Bkm: {$IFDEF RTL200_UP}TBookmark{$ELSE}TBookmarkStr{$ENDIF RTL200_UP};
  RecIdx: Integer;
  RecID: Variant;
begin
  if Assigned(DBDataController(AActionComponent)) and Assigned(Dataset(AActionComponent)) and
    Assigned(GridTableView(AActionComponent)) then
    try
      if IsGridMode(AActionComponent) then
      begin
        Bkm := DBDataController(AActionComponent).GetSelectedBookmark(ASelectedRow);
        if Dataset(AActionComponent).BookmarkValid(TBookmark(Bkm)) then
        begin
          Dataset(AActionComponent).Bookmark := Bkm;
          Result := True;
        end
        else
          Result := False;
      end
      else
      begin
        RecIdx :=
          GridTableView(AActionComponent).Controller.SelectedRecords[ASelectedRow].RecordIndex;
        RecID := GridTableView(AActionComponent).DataController.GetRecordId(RecIdx);
        Result :=
          Dataset(AActionComponent).Locate(DBDataController(AActionComponent).KeyFieldNames, RecID,
          [loPartialKey]);
      end;
    except
      Result := False;
    end
  else
    Result := False;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.IsGridMode(AActionComponent
  : TComponent): Boolean;
begin
  if Assigned(DBDataController(AActionComponent)) then
    Result := DBDataController(AActionComponent).DataModeController.GridMode
  else
    Result := True;
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.Last(AActionComponent:
  TComponent);
begin
  if Assigned(GridView(AActionComponent)) then
    GridView(AActionComponent).DataController.GotoLast
  else
    inherited Last(AActionComponent);
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.MoveBy(AActionComponent:
  TComponent; Distance: Integer);
begin
  if Assigned(GridView(AActionComponent)) then
    GridView(AActionComponent).DataController.MoveBy(Distance)
  else
    inherited MoveBy(AActionComponent, Distance);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.SelectedField(
  AActionComponent: TComponent): TField;
var Item : TcxCustomGridTableItem;
begin
  Result := nil;
  if GridView(AActionComponent).Controller is TcxCustomGridTableController then
  begin
    Item := TcxCustomGridTableController(GridView(AActionComponent).Controller).FocusedItem;
    if Assigned(Item) and
      (Item is TcxGridColumn) and
      (Item.DataBinding is TcxGridItemDBDataBinding) then
      Result := TcxGridItemDBDataBinding(Item.DataBinding).Field;
  end;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.SelectedRowsCount(
  AActionComponent: TComponent): Integer;
begin
  if Assigned(GridView(AActionComponent)) then
    Result := GridView(AActionComponent).DataController.GetSelectedCount
  else
    Result := inherited SelectedRowsCount(AActionComponent);
end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}

procedure InitActionEngineList;
begin
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  RegisterDatabaseActionEngine(TJvDatabaseActionDevExpCxGridControlEngine);
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  InitActionEngineList;

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
