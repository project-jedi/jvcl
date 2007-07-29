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
located at http://jvcl.sourceforge.net

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
    function IsGridMode(AActionComponent : TComponent): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function Bof(aActionComponent: TComponent): Boolean; override;
    function RecNo(aActionComponent: TComponent): Integer; override;
    function RecordCount(aActionComponent: TComponent): Integer; override;
    function CanInsert(aActionComponent: TComponent): Boolean; override;
    function CanUpdate(aActionComponent: TComponent): Boolean; override;
    function CanDelete(aActionComponent: TComponent): Boolean; override;
    function CanNavigate(aActionComponent: TComponent): Boolean; override;
    function DataSource(AActionComponent: TComponent): TDataSource; override;
    procedure First(aActionComponent: TComponent); override;
    function GotoSelectedRow(aActionComponent: TComponent;const ASelectedRow:
        Integer): Boolean; override;
    procedure Last(aActionComponent: TComponent); override;
    procedure MoveBy(aActionComponent: TComponent;Distance: Integer); override;
    function SelectedRowsCount(aActionComponent: TComponent): Integer; override;
    function SupportsComponent(AActionComponent: TComponent): Boolean; override;
  end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}

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
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGrid, cxGridDBDataDefinitions, cxGridDBChartView,
  cxCustomData,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils, Grids;

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
    else
      if AActionComponent is TcxCustomGridView then
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
    else
      if AActionComponent is TcxCustomGridTableView then
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

function TJvDatabaseActionDevExpCxGridControlEngine.Bof(aActionComponent:
    TComponent): Boolean;
begin
  if Assigned(GridView(aActionComponent)) then
    Result := GridView(aActionComponent).DataController.FocusedRowIndex = 0
  else
    Result := inherited Bof(aActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.RecNo(aActionComponent:
    TComponent): Integer;
begin
  if Assigned(GridView(aActionComponent)) then
    if GridView(aActionComponent).DataController.IsGridMode then
      Result := inherited RecNo(aActionComponent)
    else
      Result := GridView(aActionComponent).DataController.FocusedRowIndex + 1
  else
    Result := inherited RecNo(aActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.RecordCount(
    aActionComponent: TComponent): Integer;
begin
  if Assigned(GridView(aActionComponent)) then
    if GridView(aActionComponent).DataController.IsGridMode then
      Result := inherited RecordCount(aActionComponent)
    else
      Result := GridView(aActionComponent).DataController.RecordCount
  else
    Result := inherited RecordCount(aActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanInsert(aActionComponent:
    TComponent): Boolean;
begin
  if Assigned(GridTableView(aActionComponent)) then
    Result := GridTableView(aActionComponent).OptionsData.Inserting and inherited CanInsert(aActionComponent)
  else
    Result := inherited CanInsert(aActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanUpdate(aActionComponent:
    TComponent): Boolean;
begin
  if Assigned(GridTableView(aActionComponent)) then
    Result := GridTableView(aActionComponent).OptionsData.Editing and inherited CanUpdate(aActionComponent)
  else
    Result := inherited CanUpdate(aActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanDelete(aActionComponent:
    TComponent): Boolean;
begin
  if Assigned(GridTableView(aActionComponent)) then
    Result := GridTableView(aActionComponent).OptionsData.Deleting and inherited CanDelete(aActionComponent)
  else
    Result := inherited CanDelete(aActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanNavigate(
    aActionComponent: TComponent): Boolean;
begin
  Result := Assigned(GridTableView(aActionComponent));
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.First(aActionComponent:
    TComponent);
begin
  if Assigned(GridTableView(aActionComponent)) then
    GridTableView(aActionComponent).DataController.GotoFirst
  else
    inherited First(aActionComponent);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.DBDataController(
    AActionComponent: TComponent): TcxDBDataController;

begin
  if Assigned(GridView(aActionComponent)) and (GridView(aActionComponent).DataController is TcxDBDataController) then
    Result := TcxDBDataController(GridView(aActionComponent).DataController)
  else
    Result := nil;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.GotoSelectedRow(
    aActionComponent: TComponent;const ASelectedRow: Integer): Boolean;
var
  Bkm: TBookmarkStr;
  RecIdx: Integer;
  RecID: Variant;
begin
  if Assigned(DBDataController(aActionComponent)) and Assigned(Dataset(aActionComponent)) and Assigned(GridTableView(aActionComponent)) then
    try
      if IsGridMode(aActionComponent) then
      begin
        Bkm := DBDataController(aActionComponent).GetSelectedBookmark(ASelectedRow);
        if DataSet(aActionComponent).BookmarkValid(TBookmark(Bkm)) then
        begin
          Dataset(aActionComponent).Bookmark := Bkm;
          Result := True;
        end
        else
          Result := False;
      end
      else
      begin
        RecIdx := GridTableView(aActionComponent).Controller.SelectedRecords[ASelectedRow].RecordIndex;
        RecID := GridTableView(aActionComponent).DataController.GetRecordId(RecIdx);
        Result := DataSet(aActionComponent).Locate(DBDataController(aActionComponent).KeyFieldNames, RecID, [loPartialKey]);
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
  if Assigned(DBDataController(aActionComponent)) then
    Result := DBDataController(aActionComponent).DataModeController.GridMode
  else
    Result := True;
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.Last(aActionComponent:
    TComponent);
begin
  if Assigned(GridView(aActionComponent)) then
    GridView(aActionComponent).DataController.GotoLast
  else
    inherited Last(aActionComponent);
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.MoveBy(aActionComponent:
    TComponent;Distance: Integer);
begin
  if Assigned(GridView(aActionComponent)) then
    GridView(aActionComponent).DataController.MoveBy(Distance)
  else
    inherited MoveBy(aActionComponent, Distance);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.SelectedRowsCount(
    aActionComponent: TComponent): Integer;
begin
  if Assigned(GridView(aActionComponent)) then
    Result := GridView(aActionComponent).DataController.GetSelectedCount
  else
    Result := inherited SelectedRowsCount(aActionComponent);
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

