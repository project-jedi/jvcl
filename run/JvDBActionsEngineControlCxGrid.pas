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
  TJvDatabaseActionDevExpCxGridControlEngine = class(
      TJvDatabaseActionBaseControlEngine)
  private
    FGridView: TcxCustomGridView;
    FGridTableView: TcxCustomGridTableView;
    function GetDBDataController: TcxDBDataController;
  protected
    function GetGridView(ADataComponent: TComponent): TcxCustomGridView;
    function GetGridTableView(ADataComponent: TComponent): TcxCustomGridTableView;
    function GetDataSource(ADataComponent: TComponent): TDataSource; override;
    function IsGridMode: Boolean;
    procedure SetDatacomponent(const Value: TComponent); override;
    property DBDataController: TcxDBDataController read GetDBDataController;
  public
    constructor Create(AOwner: TComponent); override;
    function Bof: boolean; override;
    function RecNo: integer; override;
    function RecordCount: integer; override;
    function CanInsert: boolean; override;
    function CanUpdate: boolean; override;
    function CanDelete: boolean; override;
    function CanNavigate: boolean; override;
    procedure First; override;
    function GotoSelectedRow(const ASelectedRow: Integer): Boolean; override;
    procedure Last; override;
    procedure MoveBy(Distance: Integer); override;
    function SelectedRowsCount: Integer; override;
    function Supports(ADataComponent: TComponent): boolean; override;
    property GridView: TcxCustomGridView read FGridView;
    property GridTableView: TcxCustomGridTableView read FGridTableView;
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
//  Dialogs;
  SysUtils, Grids;


//=== { TJvDatabaseActionDevExpCxGridControlEngine } ================================

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}

constructor TJvDatabaseActionDevExpCxGridControlEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGridView := nil;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.GetGridView(ADataComponent:
  TComponent): TcxCustomGridView;
begin
  if Assigned(ADataComponent) then
    if ADataComponent is TcxGrid then
      if TcxGrid(ADataComponent).FocusedView is TcxCustomGridView then
        Result := TcxCustomGridView(TcxGrid(ADataComponent).FocusedView)
      else
        Result := nil
    else
      if ADataComponent is TcxCustomGridView then
        Result := TcxCustomGridView(ADataComponent)
      else
        Result := nil
    else
      Result := nil;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.GetGridTableView(ADataComponent:
  TComponent): TcxCustomGridTableView;
begin
  if Assigned(ADataComponent) then
    if ADataComponent is TcxGrid then
      if TcxGrid(ADataComponent).FocusedView is TcxCustomGridTableView then
        Result := TcxCustomGridTableView(TcxGrid(ADataComponent).FocusedView)
      else
        Result := nil
    else
      if ADataComponent is TcxCustomGridTableView then
        Result := TcxCustomGridTableView(ADataComponent)
      else
        Result := nil
    else
      Result := nil;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.GetDataSource(ADataComponent:
  TComponent): TDataSource;
begin
  if Assigned(ADataComponent) then
    if Assigned(GridView) then
      if GridView.DataController is TcxGridDBDataController then
        Result := TcxGridDBDataController(GridView.DataController).DataSource
      else if GridView.DataController is  TcxGridDBChartDataController then
        Result := TcxGridDBChartDataController(GridView.DataController).DataSource
      else
        Result := nil
    else
      Result := inherited GetDataSource(ADataComponent)
  else
    Result := nil;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.Supports(ADataComponent: TComponent): boolean;
begin
  Result := Assigned(GetGridView(ADataComponent));
end;

function TJvDatabaseActionDevExpCxGridControlEngine.Bof: boolean;
begin
  if Assigned(GridView) then
    Result := GridView.DataController.FocusedRowIndex = 0
  else
    Result := inherited Bof;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.RecNo: integer;
begin
  if Assigned(GridView) then
    if GridView.DataController.IsGridMode then
      Result := inherited RecNo
    else
      Result := GridView.DataController.FocusedRowIndex + 1
  else
    Result := inherited RecNo;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.RecordCount: integer;
begin
  if Assigned(GridView) then
    if GridView.DataController.IsGridMode then
      Result := inherited RecordCount
    else
      Result := GridView.DataController.RecordCount
  else
    Result := inherited RecordCount;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanInsert: boolean;
begin
  if Assigned(GridTableView) then
    Result := GridTableView.OptionsData.Inserting and inherited CanInsert
  else
    Result := inherited CanInsert;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanUpdate: boolean;
begin
  if Assigned(GridTableView) then
    Result := GridTableView.OptionsData.Editing and inherited CanUpdate
  else
    Result := inherited CanUpdate;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanDelete: boolean;
begin
  if Assigned(GridTableView) then
    Result := GridTableView.OptionsData.Deleting and inherited CanDelete
  else
    Result := inherited CanDelete;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.CanNavigate: boolean;
begin
  Result := Assigned(GridTableView);
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.First;
begin
  if Assigned(GridTableView) then
    GridTableView.DataController.GotoFirst
  else
    inherited First;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.GetDBDataController:
  TcxDBDataController;
begin
  if Assigned(GridView) and (GridView.DataController is TcxDBDataController) then
    Result := TcxDBDataController(GridView.DataController)
  else
    Result := nil;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.GotoSelectedRow(const
  ASelectedRow: Integer): Boolean;
var
  Bkm: TBookmarkStr;
  RecIdx : Integer;
  RecID : Variant;
begin
  if Assigned(DBDataController) and Assigned(Dataset) and Assigned(GridTableView) then
    try
      if IsGridMode then
      begin
        Bkm := DBDataController.GetSelectedBookmark(ASelectedRow);
        if DataSet.BookmarkValid(TBookmark(Bkm)) then
        begin
          Dataset.Bookmark := Bkm;
          Result := true;
        end
        else
          Result := False;
      end
      else
      begin
        RecIdx := GridTableView.Controller.SelectedRecords[ASelectedRow].RecordIndex;
        RecID := GridTableView.DataController.GetRecordId(RecIdx);
        Result := DataSet.Locate(DBDataController.KeyFieldNames, RecID, [loPartialKey]);
      end;
    except
      on e:exception do
        Result := false;
    end
  else
    Result := False;
end;

function TJvDatabaseActionDevExpCxGridControlEngine.IsGridMode: Boolean;
begin
  if Assigned(DBDataController) then
    Result := DBDataController.DataModeController.GridMode
  else
    Result := True;
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.Last;
begin
  if Assigned(GridView) then
    GridView.DataController.GotoLast
  else
    inherited Last;
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.MoveBy(Distance: Integer);
begin
  if Assigned(GridView) then
    GridView.DataController.MoveBy(Distance)
  else
    inherited MoveBy(Distance);
end;

function TJvDatabaseActionDevExpCxGridControlEngine.SelectedRowsCount: Integer;
begin
  if Assigned(GridView) then
    Result := GridView.DataController.GetSelectedCount
  else
    Result := 0;
end;

procedure TJvDatabaseActionDevExpCxGridControlEngine.SetDatacomponent(const Value:
  TComponent);
begin
  FGridView := GetGridView(Value);
  FGridTableView := GetGridTableView(Value);
  inherited SetDatacomponent(Value);
end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}


procedure InitActionEngineList;
begin
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  RegisterActionEngine(TJvDatabaseActionDevExpCxGridControlEngine);
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

