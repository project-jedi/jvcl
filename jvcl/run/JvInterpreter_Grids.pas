{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Grids.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_Grids;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, Grids,
  JvInterpreter_Windows;

{ EInvalidGridOperation }

{ TInplaceEdit }

{ constructor Create(AOwner: TComponent) }

procedure TInplaceEdit_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TInplaceEdit.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Deselect; }

procedure TInplaceEdit_Deselect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TInplaceEdit(Args.Obj).Deselect;
end;

{ procedure Hide; }

procedure TInplaceEdit_Hide(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TInplaceEdit(Args.Obj).Hide;
end;

{ procedure Invalidate; }

procedure TInplaceEdit_Invalidate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TInplaceEdit(Args.Obj).Invalidate;
end;

{ procedure Move(const Loc: TRect); }

procedure TInplaceEdit_Move(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TInplaceEdit(Args.Obj).Move(Var2Rect(Args.Values[0]));
end;

{  function PosEqual(const Rect: TRect): Boolean; }

procedure TInplaceEdit_PosEqual(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TInplaceEdit(Args.Obj).PosEqual(Var2Rect(Args.Values[0]));
end;

{ procedure SetFocus; }

procedure TInplaceEdit_SetFocus(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TInplaceEdit(Args.Obj).SetFocus;
end;

{ procedure UpdateLoc(const Loc: TRect); }

procedure TInplaceEdit_UpdateLoc(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TInplaceEdit(Args.Obj).UpdateLoc(Var2Rect(Args.Values[0]));
end;

{ function Visible: Boolean; }

procedure TInplaceEdit_Visible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TInplaceEdit(Args.Obj).Visible;
end;

{ TCustomGrid }

{ function MouseCoord(X, Y: Integer): TGridCoord; }

procedure TCustomGrid_MouseCoord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Point2Var(TPoint(TCustomGrid(Args.Obj).MouseCoord(Args.Values[0], Args.Values[1])));
end;

{ TDrawGrid }

{ constructor Create(AOwner: TComponent) }

procedure TDrawGrid_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDrawGrid.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function CellRect(ACol, ARow: Longint): TRect; }

procedure TDrawGrid_CellRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TDrawGrid(Args.Obj).CellRect(Args.Values[0], Args.Values[1]));
end;

{ procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint); }

procedure TDrawGrid_MouseToCell(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDrawGrid(Args.Obj).MouseToCell(Args.Values[0], Args.Values[1], Longint(TVarData(Args.Values[2]).vInteger),
    Longint(TVarData(Args.Values[3]).vInteger));
end;

{ TStringGrid }

{ constructor Create(AOwner: TComponent) }

procedure TStringGrid_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStringGrid.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Cols[Integer]: TStrings }

procedure TStringGrid_Read_Cols(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStringGrid(Args.Obj).Cols[Args.Values[0]]);
end;

{ property Write Cols[Integer]: TStrings }

procedure TStringGrid_Write_Cols(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringGrid(Args.Obj).Cols[Args.Values[0]] := V2O(Value) as TStrings;
end;

{ property Read Rows[Integer]: TStrings }

procedure TStringGrid_Read_Rows(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStringGrid(Args.Obj).Rows[Args.Values[0]]);
end;

{ property Write Rows[Integer]: TStrings }

procedure TStringGrid_Write_Rows(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringGrid(Args.Obj).Rows[Args.Values[0]] := V2O(Value) as TStrings;
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cGrids = 'Grids';
begin
  with JvInterpreterAdapter do
  begin
    { EInvalidGridOperation }
    AddClass(cGrids, EInvalidGridOperation, 'EInvalidGridOperation');
    { TGridState }
    AddConst(cGrids, 'gsNormal', Ord(gsNormal));
    AddConst(cGrids, 'gsSelecting', Ord(gsSelecting));
    AddConst(cGrids, 'gsRowSizing', Ord(gsRowSizing));
    AddConst(cGrids, 'gsColSizing', Ord(gsColSizing));
    AddConst(cGrids, 'gsRowMoving', Ord(gsRowMoving));
    AddConst(cGrids, 'gsColMoving', Ord(gsColMoving));
    { TInplaceEdit }
    AddClass(cGrids, TInplaceEdit, 'TInplaceEdit');
    AddGet(TInplaceEdit, 'Create', TInplaceEdit_Create, 1, [varEmpty], varEmpty);
    AddGet(TInplaceEdit, 'Deselect', TInplaceEdit_Deselect, 0, [varEmpty], varEmpty);
    AddGet(TInplaceEdit, 'Hide', TInplaceEdit_Hide, 0, [varEmpty], varEmpty);
    AddGet(TInplaceEdit, 'Invalidate', TInplaceEdit_Invalidate, 0, [varEmpty], varEmpty);
    AddGet(TInplaceEdit, 'Move', TInplaceEdit_Move, 1, [varEmpty], varEmpty);
    AddGet(TInplaceEdit, 'PosEqual', TInplaceEdit_PosEqual, 1, [varEmpty], varEmpty);
    AddGet(TInplaceEdit, 'SetFocus', TInplaceEdit_SetFocus, 0, [varEmpty], varEmpty);
    AddGet(TInplaceEdit, 'UpdateLoc', TInplaceEdit_UpdateLoc, 1, [varEmpty], varEmpty);
    AddGet(TInplaceEdit, 'Visible', TInplaceEdit_Visible, 0, [varEmpty], varEmpty);
    { TGridOption }
    AddConst(cGrids, 'goFixedVertLine', Ord(goFixedVertLine));
    AddConst(cGrids, 'goFixedHorzLine', Ord(goFixedHorzLine));
    AddConst(cGrids, 'goVertLine', Ord(goVertLine));
    AddConst(cGrids, 'goHorzLine', Ord(goHorzLine));
    AddConst(cGrids, 'goRangeSelect', Ord(goRangeSelect));
    AddConst(cGrids, 'goDrawFocusSelected', Ord(goDrawFocusSelected));
    AddConst(cGrids, 'goRowSizing', Ord(goRowSizing));
    AddConst(cGrids, 'goColSizing', Ord(goColSizing));
    AddConst(cGrids, 'goRowMoving', Ord(goRowMoving));
    AddConst(cGrids, 'goColMoving', Ord(goColMoving));
    AddConst(cGrids, 'goEditing', Ord(goEditing));
    AddConst(cGrids, 'goTabs', Ord(goTabs));
    AddConst(cGrids, 'goRowSelect', Ord(goRowSelect));
    AddConst(cGrids, 'goAlwaysShowEditor', Ord(goAlwaysShowEditor));
    AddConst(cGrids, 'goThumbTracking', Ord(goThumbTracking));
    { TGridDrawState }
    AddConst(cGrids, 'gdSelected', Ord(gdSelected));
    AddConst(cGrids, 'gdFocused', Ord(gdFocused));
    AddConst(cGrids, 'gdFixed', Ord(gdFixed));
    { TGridScrollDirection }
    AddConst(cGrids, 'sdLeft', Ord(sdLeft));
    AddConst(cGrids, 'sdRight', Ord(sdRight));
    AddConst(cGrids, 'sdUp', Ord(sdUp));
    AddConst(cGrids, 'sdDown', Ord(sdDown));
    { TCustomGrid }
    AddClass(cGrids, TCustomGrid, 'TCustomGrid');
    AddGet(TCustomGrid, 'MouseCoord', TCustomGrid_MouseCoord, 2, [varEmpty, varEmpty], varEmpty);
    { TDrawGrid }
    AddClass(cGrids, TDrawGrid, 'TDrawGrid');
    AddGet(TDrawGrid, 'Create', TDrawGrid_Create, 1, [varEmpty], varEmpty);
    AddGet(TDrawGrid, 'CellRect', TDrawGrid_CellRect, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TDrawGrid, 'MouseToCell', TDrawGrid_MouseToCell, 4, [varEmpty, varEmpty, varByRef, varByRef], varEmpty);
    { TStringGrid }
    AddClass(cGrids, TStringGrid, 'TStringGrid');
    AddGet(TStringGrid, 'Create', TStringGrid_Create, 1, [varEmpty], varEmpty);
    AddGet(TStringGrid, 'Cols', TStringGrid_Read_Cols, 1, [varEmpty], varEmpty);
    AddSet(TStringGrid, 'Cols', TStringGrid_Write_Cols, 1, [varNull]);
    AddGet(TStringGrid, 'Rows', TStringGrid_Read_Rows, 1, [varEmpty], varEmpty);
    AddSet(TStringGrid, 'Rows', TStringGrid_Write_Rows, 1, [varNull]);
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

