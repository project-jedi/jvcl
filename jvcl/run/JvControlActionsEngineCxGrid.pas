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

unit JvControlActionsEngineCxGrid;

{$I jvcl.inc}

interface

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  Forms, Controls, Classes, DB,
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGridCustomTableView, cxDBData, cxGridCustomView, cxGrid,
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  JvControlActionsEngine;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
type
  TJvControlActioncxGridEngine = class(TJvControlActionEngine)
  private
  protected
    procedure ExportGrid(aGrid: TcxGrid);
    function GetGridTableView(AActionComponent: TComponent):
      TcxCustomGridTableView;
    function GetGridView(AActionComponent: TComponent): TcxCustomGridView;
    function GetGrid(AActionComponent: TComponent): TcxGrid;
    function GetSupportedOperations: TJvControlActionOperations; override;
  public
    function ExecuteOperation(const aOperation: TJvControlActionOperation; const
      aActionControl: TControl): Boolean; override;
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
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
  cxGridDBDataDefinitions, cxGridDBChartView,
  cxCustomData, cxGridExportLink,
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
{$IFDEF HAS_UNIT_VARIANTS}
  Variants,
{$ENDIF HAS_UNIT_VARIANTS}
  SysUtils, Dialogs;

//=== { TJvDatabaseActionDevExpCxGridControlEngine } =========================

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}

function TJvControlActioncxGridEngine.ExecuteOperation(const aOperation:
  TJvControlActionOperation; const aActionControl: TControl): Boolean;
begin
  Result := true;
  case aOperation of
    caoCollapse: if Assigned(GetGridTableView(aActionControl)) then
        GetGridTableView(aActionControl).Datacontroller.Groups.FullCollapse
      else
        Result := false;
    caoExpand: if Assigned(GetGridTableView(aActionControl)) then
        GetGridTableView(aActionControl).Datacontroller.Groups.FullExpand
      else
        Result := false;
    caoOptimizeColumns: if Assigned(GetGridTableView(aActionControl)) then
        GetGridTableView(aActionControl).ApplyBestFit
      else
        Result := false;
    caoExport: if Assigned(GetGridView(aActionControl)) then
        ExportGrid(GetGrid(aActionControl))
      else
        Result := false;
  else
    Result := False
  end;
end;

procedure TJvControlActioncxGridEngine.ExportGrid(aGrid: TcxGrid);
var
  SaveDialog: TSaveDialog;
begin
  if not Assigned(aGrid) then
    Exit;
  SaveDialog := TSaveDialog.Create(Self);
  try
    with SaveDialog do
    begin
      Name := 'SaveDialog';
      DefaultExt := 'XLS';
      Filter :=
        'MS-Excel-Files (*.XLS)|*.XLS|XML-Files (*.XML)|*.HTM|HTML-Files (*.HTM)|*.HTM|Text-Files (*.TXT)|*.TXT|All Files (*.*)|*.*';
      Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist];
    end;
    if SaveDialog.Execute then
      if SaveDialog.FileName <> '' then
      begin
        if (Pos('.XLS', UpperCase(SaveDialog.FileName)) =
          Length(SaveDialog.FileName) - 3) then
          ExportGridToExcel(SaveDialog.FileName, aGrid)
        else if (Pos('.XML', UpperCase(SaveDialog.FileName)) =
          Length(SaveDialog.FileName) - 3) then
          ExportGridToXML(SaveDialog.FileName, aGrid)
        else if ((Pos('.HTM', UpperCase(SaveDialog.FileName)) =
          Length(SaveDialog.FileName) - 3) or
          (Pos('.HTML', UpperCase(SaveDialog.FileName)) =
            Length(SaveDialog.FileName) - 4)) then
          ExportGridToHTML(SaveDialog.FileName, aGrid)
        else
          ExportGridToText(SaveDialog.FileName, aGrid);
      end;
  finally
    SaveDialog.Free;
  end;
end;

function TJvControlActioncxGridEngine.GetGridTableView(AActionComponent:
  TComponent): TcxCustomGridTableView;
begin
  if Assigned(AActionComponent) then
    if AActionComponent is TcxGridSite then
      if Assigned(TcxGridSite(AActionComponent).GridView) and
        (TcxGridSite(AActionComponent).GridView is TcxCustomGridTableView) then
        Result := TcxCustomGridTableView(TcxGridSite(AActionComponent).GridView)
      else
        Result := nil
    else
      Result := nil
  else
    Result := nil;
end;

function TJvControlActioncxGridEngine.GetGridView(AActionComponent:
  TComponent): TcxCustomGridView;
begin
  if Assigned(AActionComponent) then
    if AActionComponent is TcxGridSite then
      Result := TcxGridSite(AActionComponent).GridView
    else
      Result := nil
  else
    Result := nil;
end;

function TJvControlActioncxGridEngine.GetGrid(AActionComponent: TComponent):
  TcxGrid;
begin
  if Assigned(AActionComponent) then
    if AActionComponent is TcxGridSite then
      Result := TcxGrid(TcxGridSite(AActionComponent).Container)
    else
      Result := nil
  else
    Result := nil;
end;

function TJvControlActioncxGridEngine.GetSupportedOperations:
  TJvControlActionOperations;
begin
  Result := [caoCollapse, caoExpand, caoOptimizeColumns, caoExport];
end;

function TJvControlActioncxGridEngine.SupportsComponent(aActionComponent:
  TComponent): Boolean;
begin
  Result := Assigned(GetGridView(AActionComponent));
end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}

procedure InitActionEngineList;
begin
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  RegisterControlActionEngine(TJvControlActioncxGridEngine);
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

