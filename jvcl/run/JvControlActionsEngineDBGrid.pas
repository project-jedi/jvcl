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

unit JvControlActionsEngineDBGrid;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, ImgList, Graphics, ComCtrls,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows, QImgList, QGraphics, QComCtrls,
  {$ENDIF UNIX}
  Forms, Controls, Classes, JvControlActionsEngine, DBGrids;

type

  TJvControlActionDBGridEngine = class(TJvControlActionEngine)
  private
    FNoOfRowsForOptimize: Integer;
  protected
    function GetGrid(AActionComponent: TComponent): TCustomDBGrid;
    function GetSupportedOperations: TJvControlActionOperations; override;
  public
    function ExecuteOperation(const aOperation: TJvControlActionOperation; const
      aActionControl: TControl): Boolean; override;
    procedure OptimizeColumns(DBGrid: TCustomDBGrid);
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
    //1 Number of rows which should be used for column optimization
    property NoOfRowsForOptimize: Integer read FNoOfRowsForOptimize write
        FNoOfRowsForOptimize default 0;
  end;

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
  SysUtils, Grids, TypInfo, StrUtils,
  Variants, Clipbrd, DB;

Type TAccessCustomDBGrid = class(TCustomDBGrid);

procedure InitActionEngineList;
begin
  RegisterControlActionEngine(TJvControlActionDBGridEngine);
end;

function TJvControlActionDBGridEngine.ExecuteOperation(const aOperation:
  TJvControlActionOperation; const aActionControl: TControl): Boolean;
begin
  Result := true;
  if Assigned(aActionControl) and (aActionControl is TCustomDBGrid) then
    Case aOperation of
      caoOptimizeColumns: OptimizeColumns(GetGrid(aActionControl));
    else
      Result := false;
    End
  else
    Result := false;
end;

function TJvControlActionDBGridEngine.GetGrid(AActionComponent: TComponent):
  TCustomDBGrid;
begin
  if Assigned(AActionComponent) then
    if AActionComponent is TCustomDBGrid then
      Result := TCustomDBGrid(AActionComponent)
    else
      Result := nil
  else
    Result := nil;
end;

function TJvControlActionDBGridEngine.GetSupportedOperations:
  TJvControlActionOperations;
begin
  Result := [caoOptimizeColumns];
end;

procedure TJvControlActionDBGridEngine.OptimizeColumns(DBGrid: TCustomDBGrid);
var
  Bookmark: TBookmark;
  Row: Integer;
  Col: Integer;
  DataSet: TDataset;
  Column: TColumn;
begin
  if not Assigned(DBGrid) or not Assigned(DBgrid.Datasource) or not Assigned(DBgrid.Datasource.Dataset) then
    Exit;
  Dataset := DBgrid.Datasource.Dataset;
  if not DataSet.Active then
    Exit;
  BookMark := DataSet.GetBookmark;
  try
    DataSet.DisableControls;
    TAccessCustomDBGrid(DBGrid).BeginUpdate;
    for Col := 0 to TAccessCustomDBGrid(DBGrid).Columns.Count - 1 do
    begin
      Column := TAccessCustomDBGrid(DBGrid).Columns[Col];
      if Assigned(Column.Field) and Column.Field.Visible and Column.Visible then
          Column.Width := TAccessCustomDBGrid(DBGrid).Canvas.TextWidth(Column.Title.Caption + '  ')
    end; {*** FOR Spalte := 0 TO DataSet.FieldCount-1 DO ***}
    Row := 0;
    while ((Row <= NoOfRowsForOptimize) or (NoOfRowsForOptimize <= 0))and not DataSet.EoF do
    begin
      for Col := 0 to TAccessCustomDBGrid(DBGrid).Columns.Count - 1 do
      begin
        Column := TAccessCustomDBGrid(DBGrid).Columns[Col];
        if Assigned(Column.Field) then
          if Column.Field.DataType in [ftString, ftWideString, ftSmallint, ftInteger, ftWord, ftBoolean,
            ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
            ftBytes, ftVarBytes, ftAutoInc, ftMemo, ftFmtMemo
            {$IFDEF COMPILER10_UP}, ftOraTimestamp, ftWideMemo, ftFixedWideChar{$ENDIF COMPILER10_UP}
            {$IFDEF COMPILER12_UP}, ftLongWord, ftShortint, ftByte, ftExtended{$ENDIF COMPILER12_UP}] then
            if Column.Field.Visible and Column.Visible and not Column.Field.IsNull then
              if TAccessCustomDBGrid(DBGrid).Canvas.TextWidth(Column.Field.AsString + '  ') > Column.Width then
                Column.Width := TAccessCustomDBGrid(DBGrid).Canvas.TextWidth(Column.Field.AsString + '  ');
      end; {*** FOR Spalte := 0 TO DataSet.FieldCount-1 DO ***}
      DataSet.Next;
      Inc(Row);
    end; {*** WHILE (Row < 10) AND NOT DataSet.EoF DO ***}
  finally
    if Assigned(Bookmark) then
    begin
      DataSet.GotoBookmark(Bookmark);
      DataSet.FreeBookmark(Bookmark);
    end; {*** IF Assigned (Bookmark) THEN ***}
    DataSet.EnableControls;
    TAccessCustomDBGrid(DBGrid).EndUpdate;
  end;
end; {*** procedure TxSQLGrid.OptimizeColumns; ***}

function TJvControlActionDBGridEngine.SupportsComponent(aActionComponent:
  TComponent): Boolean;
begin
  Result := aActionComponent is TCustomDBGrid;
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
