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
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  ExtCtrls;

type
  TJvScrollTextDirection = (tdNone, tdUp2Down, tdDown2Up, tdLeft2Right, tdRight2Left,
    tdTopLeft2BottomRight, tdTopRight2BottomLeft, tdBottomLeft2TopRight,
    tdBottomRight2TopLeft);

  TJvMovableBevel = class(TBevel)
  private
    FStartX: Integer;
    FStartY: Integer;
    FStartPoint: TPoint;
    FMoving: Boolean; // If True then we are moving the object around.
    FMinSize: Integer;
    FSizing: Boolean; // if True then we are sizing the object;
    FDirection: TJvScrollTextDirection;
    FBorderSize: Byte;
  protected
    procedure DoMove(Shift: TShiftState; DeltaX, DeltaY: Integer);
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
    constructor Create(AOwner: TComponent); override;
  published
    property BorderSize: Byte read FBorderSize write FBorderSize default 4;
  end;

implementation

constructor TJvMovableBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Shape := bsFrame;
  Style := bsRaised;
  FBorderSize := 4;
  FMinSize := 8;
end;

procedure TJvMovableBevel.DoMove(Shift: TShiftState; DeltaX, DeltaY: Integer);
begin
  // Must work on it in order to make expand and shrink the way coreldraw does when
  // shift and ctrl keys are pressed.
  {  If ssCtrl in shift then begin
      if Abs(FStartPoint.X - Left) < Abs(FStartPoint.Y - Top) then begin
         Top := Top + DeltaY;
         Left:=FStartX;
      end;
      if Abs(FStartPoint.X - Left) > Abs(FStartPoint.Y - Top) then begin
        Left := Left + DeltaX;
        Top := FStartY;
      end;
      if Abs(FStartPoint.X - Left) = Abs(FStartPoint.Y - Top) then begin
        Top := Top + DeltaY;
        Left := Left + DeltaX;
      end
    end else begin{}
  Top := Top + DeltaY;
  Left := Left + DeltaX;
  //  end
end;

procedure TJvMovableBevel.DoSize(Shift: TShiftState; DeltaX, DeltaY: Integer);
begin
  case FDirection of
    tdUp2Down:
      begin
        Height := Height + DeltaY;
        Top := Top - DeltaY;
      end;
    tdDown2Up:
        Height := FStartY - DeltaY;
    tdLeft2Right:
      begin
        Width := Width + DeltaX;
        Left := Left - DeltaX;
      end;
    tdRight2Left:
        Width := FStartX - DeltaX;
    tdTopLeft2BottomRight:
      begin
        Top := Top - DeltaY;
        Left := Left - DeltaX;
        Height := Height + DeltaY;
        Width := Width + DeltaX;
      end;
    tdTopRight2BottomLeft:
      begin
        Height := Height + DeltaY;
        Width := FStartX - DeltaX;
        Top := Top - DeltaY;
      end;
    tdBottomLeft2TopRight:
      begin
        Left := Left - DeltaX;
        Height := FStartY - DeltaY;
        Width := Width + DeltaX;
      end;
    tdBottomRight2TopLeft:
      begin
        Height := FStartY - DeltaY;
        Width := FStartX - DeltaX;
      end;
  end;
end;

procedure TJvMovableBevel.SelectCursor(X, Y: longint);
begin
  if (Y > 0) and (Y <= FBorderSize) then
  begin
    if (X > 0) and (X <= FBorderSize) then
    begin
      Screen.Cursor := crSizeNWSE;
      FDirection := tdTopLeft2BottomRight;
    end
    else
    if (X >= Width - FBorderSize) and (X < Width) then
    begin
      Screen.Cursor := crSizeNESW;
      FDirection := tdTopRight2BottomLeft;
    end
    else
    begin
      Screen.Cursor := crSizeNS;
      FDirection := tdUp2Down;
    end;
  end
  else
  if (Y >= Height - FBorderSize) and (Y < Height) then
  begin
    if (X > 0) and (X <= FBorderSize) then
    begin
      Screen.Cursor := crSizeNESW;
      FDirection := tdBottomLeft2TopRight;
    end
    else
    if (X >= Width - FBorderSize) and (X < Width) then
    begin
      Screen.Cursor := crSizeNWSE;
      FDirection := tdBottomRight2TopLeft;
    end
    else
    begin
      Screen.Cursor := crSizeNS;
      FDirection := tdDown2Up;
    end;
  end
  else
  if (X >= 1) and (X <= FBorderSize) then
  begin
    Screen.Cursor := crSizeWE;
    FDirection := tdLeft2Right;
  end
  else
  if (X >= Width - FBorderSize) and (X < Width) then
  begin
    Screen.Cursor := crSizeWE;
    FDirection := tdRight2Left;
  end
  else
  begin
    Screen.Cursor := crDefault;
    FDirection := tdNone;
  end
end;

procedure TJvMovableBevel.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  SC_DragMove = $F012;
  WM_MOVE = $0003;
begin
  if FMoving then
    DoMove(Shift, X - FStartX, Y - FStartY)
  else
  if FSizing then
    DoSize(Shift, FStartX - X, FStartY - Y)
  else
    SelectCursor(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvMovableBevel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FDirection > tdNone then
    FSizing := True
  else
    FMoving := True;
  FStartPoint := Point(Left, Top);
  FStartX := X;
  FStartY := Y;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvMovableBevel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMoving := False;
  FSizing := False;
  SelectCursor(X, Y);
  FStartX := 0;
  FStartY := 0;
  if Height < 0 then
  begin
    Top := Top + Height;
    Height := Abs(Height);
  end;
  if Width < 0 then
  begin
    Left := Left + Width;
    Width := Abs(Width);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

//Procedure TJvMovableBevel.SelectCursor(X,Y:longint);
//begin
//  if Y in[0..FBorderSize] then
//  begin
//    If X in[0..FBorderSize] then
//    begin
//      Screen.Cursor:= crsizenwse;
//      FDirection := tdTopLeft2BottomRight;
//    end
//    else
//      if X in[Width-FBorderSize..Width] then
//      begin
//        Screen.Cursor := crsizenesw;
//        FDirection := tdTopRight2BottomLeft;
//      end
//      else
//      begin
//        Screen.Cursor := crsizens;
//        FDirection := tdUp2Down;
//      end;
//  end
//  else
//    if Y in [Height-FBorderSize..Height] then
//    begin
//      If X in[0..FBorderSize] then
//      begin
//        Screen.Cursor:= crsizenesw;
//        FDirection := tdBottomLeft2TopRight;
//      end
//      else
//        if X in[Width-FBorderSize..Width] then
//        begin
//          Screen.Cursor := crsizenwse;
//          FDirection := tdBottomRight2TopLeft;
//        end
//        else
//        begin
//          Screen.Cursor := crsizens;
//          FDirection := tdDown2Up;
//        end;
//  end
//  else
//    if (X in [1..FBorderSize]) then
//    begin
//      Screen.Cursor := crsizeWE;
//      FDirection := tdLeft2Right;
//    end
//    else
//      if  (X in [Width-FBorderSize..Width]) then
//      begin
//        Screen.Cursor := crsizeWE;
//        FDirection := tdRight2Left;
//      end
//      else
//      begin
//        Screen.Cursor := crdefault;
//        FDirection := tdNone;
//      end
//end;{}

procedure TJvMovableBevel.CMMouseEnter(var Msg: TMessage);
var
  Pos: TPoint;
begin
  Pos := ScreenToClient(Mouse.CursorPos);
  SelectCursor(Pos.X, Pos.Y);
end;

procedure TJvMovableBevel.CMMouseLeave(var Msg: TMessage);
begin
  if (not FMoving) and (not FSizing) then
  begin
    Screen.Cursor := crDefault;
    FDirection := tdNone;
  end;
end;

end.

