{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvButtonDrawGrid.PAS, released on 2000-11-22.

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

unit JvButtonDrawGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Grids, JVCLVer;

type
  TJvButtonDrawGrid = class(TDrawGrid)
  private
    FCellDown: TGridCoord;
    FAboutJVCL: TJVCLAboutInfo;
  protected
    { Protected declarations }
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
  public
    constructor Create(aOwner: Tcomponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

{ TButtonDrawGrid }

constructor TJvButtonDrawGrid.Create(aOwner: Tcomponent);
begin
  inherited;
  FCellDown.X := -1;
  FCellDown.Y := -1;
end;

procedure TJvButtonDrawGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  r: TRect;
  style: DWORD;
begin
  r := ARect;
  if not (gdFixed in aState) then
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Font.Color := clBtnText;
    style := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if (FCellDown.X = aCol) and (FCellDown.Y = aRow) then
      style := style or DFCS_PUSHED;
    DrawFrameControl(Canvas.Handle, r, DFC_BUTTON, style);
  end;

  inherited DrawCell(ACol, aRow, r, aState);
end;

procedure TJvButtonDrawGrid.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cell: TGridCoord;
begin
  if (Button = mbLeft) and ((Shift - [ssLeft]) = []) then
  begin
    MousetoCell(X, Y, cell.X, cell.Y);
    if (cell.X >= FixedCols) and (cell.Y >= FixedRows) then
    begin
      FCellDown := cell;
      InvalidateCell(cell.X, cell.Y);
    end;
  end;
  inherited;
end;

procedure TJvButtonDrawGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  cell: TGridCoord;
begin
  if Shift = [ssLeft] then
  begin
    MousetoCell(X, Y, cell.X, cell.Y);
    if not CompareMem(@cell, @FCellDown, Sizeof(cell)) then
    begin
      if (FCellDown.X >= 0) and (FCellDown.Y >= 0) then
        InvalidateCell(FCellDown.X, FCellDown.Y);
      FCellDown := cell;
      InvalidateCell(cell.X, cell.Y);
    end;
  end;
  inherited;
end;

procedure TJvButtonDrawGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (Shift = []) then
  begin
    InvalidateCell(FCellDown.X, FCellDown.Y);
    FCellDown.X := -1;
    FCellDown.Y := -1;
  end;
  inherited;
end;

function TJvButtonDrawGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  result := false;
end;

end.
