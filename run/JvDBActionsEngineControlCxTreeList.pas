{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActionsEngineControlCxTreeList.Pas, released on 2004-12-30.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBActionsEngineControlCxTreeList;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Forms, Controls, Classes, DB,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
  cxTL, cxDBTL,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
  JvDBActionsEngine;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
type
  TJvDatabaseActionDevExpCxTreeListControlEngine = class(TJvDatabaseActionBaseControlEngine)
  private
    function DBDataController(AActionComponent: TComponent): TcxDBTreeListDataController;
  protected
    function TreeList(AActionComponent: TComponent): TcxCustomTreeList;
    function DBTreeList(AActionComponent: TComponent): TcxDBTreeList;
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
    procedure Last(AActionComponent: TComponent); override;
    procedure MoveBy(AActionComponent: TComponent; Distance: Integer); override;
    function SelectedField(AActionComponent: TComponent): TField; override;
    function SupportsComponent(AActionComponent: TComponent): Boolean; override;
  end;

  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}

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
  Variants, SysUtils, Grids;

//=== { TJvDatabaseActionDevExpCxTreeListControlEngine } =========================

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}

constructor TJvDatabaseActionDevExpCxTreeListControlEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'TJvDatabaseActionDevExpCxTreeListControlEngine';
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.DataSource(AActionComponent: TComponent): TDataSource;
begin
  if Assigned(DBDataController(AActionComponent)) then
    Result := DBDataController(AActionComponent).DataSource
  else
    Result := inherited DataSource(AActionComponent)
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.SupportsComponent(AActionComponent: TComponent): Boolean;
begin
  Result := Assigned(DBTreeList(AActionComponent));
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.Bof(AActionComponent: TComponent): Boolean;
begin
  if Assigned(DBTreeList(AActionComponent)) then
    Result := DBTreeList(AActionComponent).DataController.FocusedRowIndex = 0
  else
    Result := inherited Bof(AActionComponent);
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.RecNo(AActionComponent: TComponent): Integer;
begin
  if Assigned(TreeList(AActionComponent)) then
    if Assigned(TreeList(AActionComponent).FocusedNode) then
      Result := TreeList(AActionComponent).FocusedNode.AbsoluteIndex+1
    else
      Result := inherited RecNo(AActionComponent)
  else
    Result := inherited RecNo(AActionComponent);
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.RecordCount(AActionComponent: TComponent): Integer;
begin
  if Assigned(TreeList(AActionComponent)) then
    if Assigned(TreeList(AActionComponent).FocusedNode) then
      Result := TreeList(AActionComponent).AbsoluteCount
    else
      Result := inherited RecordCount(AActionComponent)
  else
    Result := inherited RecordCount(AActionComponent);
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.CanInsert(AActionComponent: TComponent): Boolean;
begin
  if Assigned(DBTreeList(AActionComponent)) then
    Result := DBTreeList(AActionComponent).OptionsData.Inserting and inherited CanInsert(AActionComponent)
  else
    Result := inherited CanInsert(AActionComponent);
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.CanUpdate(AActionComponent: TComponent): Boolean;
begin
  if Assigned(DBTreeList(AActionComponent)) then
    Result := DBTreeList(AActionComponent).OptionsData.Editing and inherited CanUpdate(AActionComponent)
  else
    Result := inherited CanUpdate(AActionComponent);
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.CanDelete(AActionComponent: TComponent): Boolean;
begin
  if Assigned(DBTreeList(AActionComponent)) then
    Result := DBTreeList(AActionComponent).OptionsData.Deleting and inherited CanDelete(AActionComponent)
  else
    Result := inherited CanDelete(AActionComponent);
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.CanNavigate(AActionComponent: TComponent): Boolean;
begin
  Result := Assigned(DBTreeList(AActionComponent));
end;

procedure TJvDatabaseActionDevExpCxTreeListControlEngine.First(AActionComponent: TComponent);
begin
  if Assigned(DBTreeList(AActionComponent)) then
    DBTreeList(AActionComponent).DataController.GotoFirst
  else
    inherited First(AActionComponent);
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.DBDataController(AActionComponent: TComponent):
    TcxDBTreeListDataController;

begin
  if Assigned(DBTreeList(AActionComponent)) then
    Result := DBTreeList(AActionComponent).DataController
  else
    Result := nil;
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.TreeList(AActionComponent: TComponent): TcxCustomTreeList;
begin
  if Assigned(AActionComponent) and (AActionComponent is TcxCustomTreeList) then
    Result := TcxCustomTreeList(AActionComponent)
  else
    Result := nil;
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.DBTreeList(AActionComponent: TComponent): TcxDBTreeList;
begin
  if Assigned(AActionComponent) and (AActionComponent is TcxDBTreeList) then
    Result := TcxDBTreeList(AActionComponent)
  else
    Result := nil;
end;

procedure TJvDatabaseActionDevExpCxTreeListControlEngine.Last(AActionComponent: TComponent);
begin
  if Assigned(DBTreeList(AActionComponent)) then
    DBTreeList(AActionComponent).DataController.GotoLast
  else
    inherited Last(AActionComponent);
end;

procedure TJvDatabaseActionDevExpCxTreeListControlEngine.MoveBy(AActionComponent: TComponent; Distance: Integer);
begin
  if Assigned(DBTreeList(AActionComponent)) then
    DBTreeList(AActionComponent).DataController.MoveBy(Distance)
  else
    inherited MoveBy(AActionComponent, Distance);
end;

function TJvDatabaseActionDevExpCxTreeListControlEngine.SelectedField(AActionComponent: TComponent): TField;
var Column : TcxTreeListColumn;
begin
  Result := nil;
  if Assigned(DBTreeList(AActionComponent)) then
  begin
    Column := DBTreeList(AActionComponent).FocusedColumn;
    if Assigned(Column) and (Column is TCxDBTreeListColumn) then
      Result := TCxDBTreeListColumn(Column).DataBinding.Field;
  end;
end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}

procedure InitActionEngineList;
begin
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
  RegisterDatabaseActionEngine(TJvDatabaseActionDevExpCxTreeListControlEngine);
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXTREELIST}
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
