{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvExStringgrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Grids, JVCLVer;

const
  GM_ACTIVATECELL = WM_USER + 123;

type
  TGMActivateCell = record
    msg: Cardinal;
    aCol, aRow: Integer;
    result: Integer;
  end;

  TJvExStringgrid = class;
  TExitCellEvent = procedure(Sender: TJvExStringgrid; aCol, aRow: Integer;
    const edittext: string) of object;
  TGetCellAlignmentEvent = procedure(Sender: TJvExStringgrid;
    aCol, aRow: Integer;
    State: TGridDrawState;
    var cellAlignment: TAlignment)
    of object;
  TCaptionClickEvent = procedure(sender: TJvExStringgrid;
    aCol, aRow: Integer) of object;

  TJvExStringgrid = class(Tstringgrid)
  private
    FExitCell: TExitCellEvent;
    FAlignment: TAlignment;
    FSetCanvasProperties: TDrawCellEvent;
    FGetCellAlignment: TGetCellAlignmentEvent;
    FCaptionClick: TCaptionClickEvent;
    FCellOnMouseDown: TGridCoord;
    FAboutJVCL: TJVCLAboutInfo;

    procedure GMActivateCell(var msg: TGMActivateCell); message GM_ACTIVATECELL;
    procedure SetAlignment(const Value: TAlignment);
  protected
    function CreateEditor: TInplaceEdit; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ExitCell(const edittext: string; aCol, aRow: Integer); virtual;
    procedure SetCanvasProperties(ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState); virtual;
    procedure DrawCell(ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState); override;
    procedure CaptionClick(aCol, aRow: LongInt); dynamic;
  public
    function GetCellAlignment(Acol, aRow: Longint;
      State: TGridDrawState): TAlignment; virtual;
    procedure DefaultDrawCell(ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState); virtual;
    procedure ActivateCell(aCol, aRow: Integer);
    procedure InvalidateCell(aCol, aRow: Integer);
    procedure InvalidateCol(aCol: Integer);
    procedure InvalidateRow(aRow: Integer);
    property InplaceEditor;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;

    property OnExitCell: TExitCellEvent read FExitCell write FExitCell;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property OnSetCanvasProperties: TDrawCellEvent
      read FSetCanvasProperties write FSetCanvasProperties;
    property OnGetCellAlignment: TGetCellAlignmentEvent
      read FGetCellAlignment write FGetCellAlignment;
    property OnCaptionClick: TCaptionClickEvent
      read FCaptionClick write FCaptionClick;
  end;

implementation

type
  TExInplaceEdit = class(TInplaceEdit)
  private
    FLastCol, FLastRow: Integer;

    procedure WMKillFocus(var msg: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var msg: TMessage); message WM_SETFOCUS;
  public
    procedure CreateParams(var params: TCreateParams); override;
  end;
  { TJvExStringgrid }

procedure TJvExStringgrid.ActivateCell(aCol, aRow: Integer);
begin
  PostMessage(handle, GM_ACTIVATECELL, aCol, aRow);
end;

procedure TJvExStringgrid.CaptionClick(aCol, aRow: LongInt);
begin
  if Assigned(FCaptionClick) then
    FCaptionClick(self, aCol, aRow);
end;

function TJvExStringgrid.CreateEditor: TInplaceEdit;
begin
  Result := TExInplaceEdit.Create(self);
end;

procedure TJvExStringgrid.DefaultDrawCell(ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
const
  flags: array[TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  S: string;
begin
  Canvas.FillRect(Rect);
  S := Cells[aCol, aRow];
  if Length(S) > 0 then
  begin
    InflateRect(rect, -2, -2);
    DrawText(Canvas.Handle, PChar(S), Length(S), rect,
      DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or
      flags[GetCellAlignment(acol, arow, state)]);
  end;
end;

procedure TJvExStringgrid.DrawCell(ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
begin
  if Assigned(OnDrawCell) then
    inherited
  else
  begin
    SetCanvasProperties(aCol, aRow, rect, State);
    DefaultDrawCell(aCol, aRow, rect, State);
    Canvas.Font := Font;
    Canvas.Brush := Brush;
  end;
end;

procedure TJvExStringgrid.ExitCell(const edittext: string; aCol, aRow: Integer);
begin
  if Assigned(FExitCell) then
    FExitCell(self, aCol, aRow, edittext);
end;

function TJvExStringgrid.GetCellAlignment(Acol, aRow: Integer;
  State: TGridDrawState): TAlignment;
begin
  Result := FAlignment;
  if Assigned(FGetCellAlignment) then
    FGetCellAlignment(self, acol, arow, state, result);
end;

procedure TJvExStringgrid.GMActivateCell(var msg: TGMActivateCell);
begin
  Col := msg.aCol;
  Row := msg.aRow;
  EditorMode := true;
  InplaceEditor.SelectAll;
end;

procedure TJvExStringgrid.InvalidateCell(aCol, aRow: Integer);
begin
  inherited InvalidateCell(aCol, aRow);
end;

procedure TJvExStringgrid.InvalidateCol(aCol: Integer);
begin
  inherited InvalidateCol(aCol);
end;

procedure TJvExStringgrid.InvalidateRow(aRow: Integer);
begin
  inherited InvalidateRow(aRow);
end;

procedure TJvExStringgrid.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    MouseToCell(X, Y, FCellOnMouseDown.X, FCellOnMouseDown.Y)
  else
    FCellOnMouseDown := TGridCoord(Point(-1, -1));
end;

procedure TJvExStringgrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  cell: TGridCoord;
begin
  if Button = mbLeft then
    MouseToCell(X, Y, Cell.X, Cell.Y);
  if CompareMem(@Cell, @FCellOnMouseDown, Sizeof(cell))
    and
    ((Cell.X < FixedCols) or (Cell.Y < FixedRows)) then
    CaptionClick(Cell.X, Cell.Y);
  FCellOnMouseDown := TGridCoord(Point(-1, -1));
  inherited;
end;

procedure TJvExStringgrid.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
    if Assigned(InplaceEditor) then
      TExInplaceEdit(InplaceEditor).RecreateWnd;
  end;
end;

procedure TJvExStringgrid.SetCanvasProperties(ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  if Assigned(FSetCanvasProperties) then
    FSetCanvasProperties(self, aCol, aRow, Rect, State);
end;

{ TExInplaceEdit }

procedure TExInplaceEdit.CreateParams(var params: TCreateParams);
const
  flags: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited;
  params.Style :=
    params.Style or flags[TJvExStringgrid(grid).Alignment];
end;

procedure TExInplaceEdit.WMKillFocus(var msg: TMessage);
begin
  TJvExStringgrid(Grid).ExitCell(Text, FLastCol, FLastRow);
  inherited;
end;

procedure TExInplaceEdit.WMSetFocus(var msg: TMessage);
begin
  FLastCol := TJvExStringgrid(Grid).Col;
  FLastRow := TJvExStringgrid(Grid).Row;
  inherited;
end;

end.
