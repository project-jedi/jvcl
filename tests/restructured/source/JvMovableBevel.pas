{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMovableBevel.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer@Excite.com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):


Last Modified: 2002-07-12

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvMovableBevel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TDirection = (TDNone, TDUp2Down, TDDown2Up, TDLeft2Right, TDRight2Left,
    TDTopLeft2BottomRight, TDTopRight2BottomLeft, TDBottomLeft2TopRight,
    TDBottomRight2TopLeft);

  TJvMovableBevel = class(TBevel)
  private
    { Private declarations }
    VStartX, VStartY: Integer;
    VStartPoint: Tpoint;
    VMoving: Boolean; // If true then we are moving the object around.
    vMinSize: Integer;
    VSizing: Boolean; // if true then we are sizing the object;
    VDirection: TDirection;
    VBorderSize: Byte;
  protected
    { Protected declarations }
    procedure DoMove(Shift: TShiftState; DeltaX, DeltaY: integer);
    procedure DoSize(Shift: TShiftState; DeltaX, DeltaY: Integer);
    procedure SelectCursor(X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property BorderSize: Byte read VBorderSize write VBorderSize;
  end;

implementation

procedure TJvMovableBevel.DoMove(Shift: TShiftState; DeltaX, DeltaY: integer);
begin
  // Must work on it in order to make expand and shrink the way coreldraw does when
  // shift and ctrl keys are pressed.
  {  If ssCtrl in shift then begin
      if abs(VStartPoint.x - Left) < abs(VStartPoint.y - Top) then begin
         top := top + deltay;
         Left:=VStartX;
      end;
      if abs(VStartPoint.x - Left) > abs(VStartPoint.y - Top) then begin
        Left := left + DeltaX;
        Top := VStartY;
      end;
      if abs(VStartPoint.x - Left) = abs(VStartPoint.y - Top) then begin
        top := top + deltay;
        Left := left + DeltaX;
      end
    end else begin{}
  top := top + deltay;
  Left := left + DeltaX;
  //  end
end;

procedure TJvMovableBevel.DoSize(Shift: TShiftState; DeltaX, DeltaY: integer);
begin
  case VDirection of
    TDUp2Down:
      begin
        Height := Height + DeltaY;
        top := top - deltay;
      end;
    TDDown2Up:
      begin
        Height := VStartY - deltay
      end;
    TDLeft2Right:
      begin
        Width := Width + DeltaX;
        left := left - deltaX;
      end;
    TDRight2Left:
      begin
        Width := VStartX - deltaX
      end;
    TDTopLeft2BottomRight:
      begin
        top := top - deltay;
        left := left - deltaX;
        Height := Height + DeltaY;
        Width := Width + DeltaX;
      end;
    TDTopRight2BottomLeft:
      begin
        Height := Height + DeltaY;
        Width := VStartX - DeltaX;
        Top := Top - DeltaY;
      end;
    TDBottomLeft2TopRight:
      begin
        Left := Left - DeltaX;
        Height := VStartY - DeltaY;
        Width := Width + DeltaX;
      end;
    TDBottomRight2TopLeft:
      begin
        Height := VStartY - DeltaY;
        Width := VStartX - DeltaX;
      end;
  end;
end;

procedure TJvMovableBevel.SelectCursor(X, Y: longint);
begin
  if (y > 0) and (y <= VBorderSize) then
  begin
    if (x > 0) and (x <= VBorderSize) then
    begin
      screen.cursor := crsizenwse;
      VDirection := TDTopLeft2BottomRight;
    end else
      if (x >= Width - VBorderSize) and (x < Width) then
      begin
        screen.cursor := crsizenesw;
        Vdirection := TDTopRight2BottomLeft;
      end else
      begin
        screen.cursor := crsizens;
        VDirection := TDUp2Down;
      end;
  end else
    if (y >= height - VBorderSize) and (y < height) then
    begin
      if (x > 0) and (x <= VBorderSize) then
      begin
        screen.cursor := crsizenesw;
        Vdirection := TDBottomLeft2TopRight;
      end else
        if (x >= width - VBorderSize) and (x < width) then
        begin
          screen.cursor := crsizenwse;
          Vdirection := TDBottomRight2TopLeft;
        end else
        begin
          screen.cursor := crsizens;
          VDirection := TDDown2Up;
        end;
    end else
      if (x >= 1) and (x <= VBorderSize) then
      begin
        screen.Cursor := crsizeWE;
        VDirection := TDLeft2Right;
      end else if (x >= Width - VBorderSize) and (x < width) then
      begin
        screen.Cursor := crsizeWE;
        VDirection := TDRight2Left;
      end else
      begin
        screen.Cursor := crdefault;
        Vdirection := TDNone;
      end
end; {}

procedure TJvMovableBevel.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  SC_DragMove = $F012;
  WM_MOVE = $0003;
begin
  if VMoving then
    DoMove(Shift, x - VStartX, y - Vstarty)
  else
    if VSizing then
      DoSize(Shift, VStartX - X, VStartY - Y)
    else
      SelectCursor(x, y);
  inherited;
end;

procedure TJvMovableBevel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if VDirection > TDNone then
    VSizing := true
  else
    VMoving := true;
  VStartPoint := point(left, top);
  VStartX := X;
  VStartY := Y;
  inherited;
end;

procedure TJvMovableBevel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  VMoving := False;
  VSizing := False;
  SelectCursor(x, y);
  VStartX := 0;
  VStartY := 0;
  if Height < 0 then
  begin
    top := Top + Height;
    Height := ABS(Height);
  end;
  if Width < 0 then
  begin
    Left := Left + Width;
    Width := ABS(Width);
  end;
  inherited;
end;

//Procedure TJvMovableBevel.SelectCursor(X,Y:longint);
//begin
//  if y in[0..VBorderSize] then
//  begin
//    If x in[0..VBorderSize] then
//    begin
//      screen.cursor:= crsizenwse;
//      VDirection := TDTopLeft2BottomRight;
//    end
//    else
//      if x in[Width-VBorderSize..Width] then
//      begin
//        screen.cursor := crsizenesw;
//        Vdirection := TDTopRight2BottomLeft;
//      end
//      else
//      begin
//        screen.cursor := crsizens;
//        VDirection := TDUp2Down;
//      end;
//  end
//  else
//    if y in [height-VBorderSize..height] then
//    begin
//      If x in[0..VBorderSize] then
//      begin
//        screen.cursor:= crsizenesw;
//        Vdirection := TDBottomLeft2TopRight;
//      end
//      else
//        if x in[Width-VBorderSize..Width] then
//        begin
//          screen.cursor := crsizenwse;
//          Vdirection := TDBottomRight2TopLeft;
//        end
//        else
//        begin
//          screen.cursor := crsizens;
//          VDirection := TDDown2Up;
//        end;
//  end
//  else
//    if (x in [1..VBorderSize]) then
//    begin
//      screen.Cursor := crsizeWE;
//      VDirection := TDLeft2Right;
//    end
//    else
//      if  (x in [Width-VBorderSize..width]) then
//      begin
//        screen.Cursor := crsizeWE;
//        VDirection := TDRight2Left;
//      end
//      else
//      begin
//        screen.Cursor := crdefault;
//        Vdirection := TDNone;
//      end
//end;{}

constructor TJvMovableBevel.Create(AOwner: TComponent);
begin
  inherited;
  Shape := bsFrame;
  Style := bsRaised;
  VBorderSize := 4;
  vMinSize := 8;
end;

procedure TJvMovableBevel.CMMouseEnter(var Msg: TMessage);
var
  Pos: TPoint;
begin
  Pos := ScreenToClient(Mouse.CursorPos);
  SelectCursor(Pos.X, Pos.Y);
end;

procedure TJvMovableBevel.CMMouseLeave(var Msg: TMessage);
begin
  if (not VMoving) and (not VSizing) then
  begin
    Screen.Cursor := crdefault;
    VDirection := TDNone;
  end;
end;

end.

