{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActions.Pas, released on 2007-03-11.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvControlActionsEngineStringGrid;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, Graphics,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows, QImgList, QGraphics, QComCtrls,
  {$ENDIF UNIX}
  Forms, Controls, Classes, Grids, JvControlActionsEngine;

type


  TJvControlActionStringGridEngine = class(TJvControlActionEngine)
  private
  protected
    function GetGrid(AActionComponent: TComponent): TStringGrid;
    function GetSupportedOperations: TJvControlActionOperations; override;
  public
    function ExecuteOperation(const aOperation: TJvControlActionOperation; const
        aActionControl: TControl): Boolean; override;
    procedure OptimizeColumns(Grid: TStringGrid);
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
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
  SysUtils, Variants;

Type TAccessStringGrid = class(TStringGrid);

procedure InitActionEngineList;
begin
  RegisterControlActionEngine (TJvControlActionStringGridEngine);
end;

function TJvControlActionStringGridEngine.ExecuteOperation(const aOperation:
    TJvControlActionOperation; const aActionControl: TControl): Boolean;
begin
  Result := true;
  if Assigned(aActionControl) and (aActionControl is TStringGrid) then
    Case aOperation of
      caoOptimizeColumns: OptimizeColumns(GetGrid(aActionControl));
    else
      Result := false;
    End
  else
    Result := false;
end;

function TJvControlActionStringGridEngine.GetGrid(AActionComponent:
    TComponent): TStringGrid;
begin
  if Assigned(AActionComponent) then
    if AActionComponent is TStringGrid then
      Result := TStringGrid(AActionComponent)
    else
      Result := nil
  else
    Result := nil;
end;

function TJvControlActionStringGridEngine.GetSupportedOperations:
    TJvControlActionOperations;
begin
  Result := [caoOptimizeColumns];
end;

procedure TJvControlActionStringGridEngine.OptimizeColumns(Grid: TStringGrid);
var
  Row: Integer;
  Col: Integer;
begin
  if not Assigned(Grid) then
    Exit;
  try
    for Col := 0 to Grid.ColCount - 1 do
    begin
      TAccessStringGrid(Grid).ColWidths[Col] := 0;
      for Row := 0 to Grid.RowCount - 1 do
      begin
        if TAccessStringGrid(Grid).Canvas.TextWidth(Grid.Cells[Col, Row] + '  ') > TAccessStringGrid(Grid).ColWidths[Col] then
          TAccessStringGrid(Grid).ColWidths[Col] := TAccessStringGrid(Grid).Canvas.TextWidth(Grid.Cells[Col, Row] + '  ')
      end; {*** FOR Spalte := 0 TO DataSet.FieldCount-1 DO ***}
    end;
  finally
  end;
end; {*** procedure TxSQLGrid.OptimizeColumns; ***}

function TJvControlActionStringGridEngine.SupportsComponent(aActionComponent:
    TComponent): Boolean;
begin
  Result := aActionComponent is TStringGrid;
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
