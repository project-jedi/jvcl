{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMovableBevel.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer att Excite dott com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQMovableBevel;

interface

uses
  SysUtils, Classes,  
  QControls, QForms, QExtCtrls, Types, 
  JvQExExtCtrls;

type
  TJvBevelScrollTextDirection = (tdNone, tdUpToDown, tdDownToUp, tdLeftToRight,
    tdRightToLeft, tdTopLeftToBottomRight, tdTopRightToBottomLeft,
    tdBottomLeftToTopRight, tdBottomRightToTopLeft);

  TJvMovableBevel = class(TJvExBevel)
  private
    FStartX: Integer;
    FStartY: Integer;
    FStartPoint: TPoint;
    FMoving: Boolean; // If True then we are moving the object around.
    FMinSize: Integer;
    FSizing: Boolean; // if True then we are sizing the object;
    FDirection: TJvBevelScrollTextDirection;
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
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
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
  {  If ssCtrl in shift then
     begin
      if Abs(FStartPoint.X - Left) < Abs(FStartPoint.Y - Top) then
      begin
         Top := Top + DeltaY;
         Left:=FStartX;
      end;
      if Abs(FStartPoint.X - Left) > Abs(FStartPoint.Y - Top) then
      begin
        Left := Left + DeltaX;
        Top := FStartY;
      end;
      if Abs(FStartPoint.X - Left) = Abs(FStartPoint.Y - Top) then
      begin
        Top := Top + DeltaY;
        Left := Left + DeltaX;
      end
    end
    else
    begin }
  Top := Top + DeltaY;
  Left := Left + DeltaX;
  //  end
end;

procedure TJvMovableBevel.DoSize(Shift: TShiftState; DeltaX, DeltaY: Integer);
begin
  case FDirection of
    tdUpToDown:
      begin
        Height := Height + DeltaY;
        Top := Top - DeltaY;
      end;
    tdDownToUp:
        Height := FStartY - DeltaY;
    tdLeftToRight:
      begin
        Width := Width + DeltaX;
        Left := Left - DeltaX;
      end;
    tdRightToLeft:
        Width := FStartX - DeltaX;
    tdTopLeftToBottomRight:
      begin
        Top := Top - DeltaY;
        Left := Left - DeltaX;
        Height := Height + DeltaY;
        Width := Width + DeltaX;
      end;
    tdTopRightToBottomLeft:
      begin
        Height := Height + DeltaY;
        Width := FStartX - DeltaX;
        Top := Top - DeltaY;
      end;
    tdBottomLeftToTopRight:
      begin
        Left := Left - DeltaX;
        Height := FStartY - DeltaY;
        Width := Width + DeltaX;
      end;
    tdBottomRightToTopLeft:
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
      FDirection := tdTopLeftToBottomRight;
    end
    else
    if (X >= Width - FBorderSize) and (X < Width) then
    begin
      Screen.Cursor := crSizeNESW;
      FDirection := tdTopRightToBottomLeft;
    end
    else
    begin
      Screen.Cursor := crSizeNS;
      FDirection := tdUpToDown;
    end;
  end
  else
  if (Y >= Height - FBorderSize) and (Y < Height) then
  begin
    if (X > 0) and (X <= FBorderSize) then
    begin
      Screen.Cursor := crSizeNESW;
      FDirection := tdBottomLeftToTopRight;
    end
    else
    if (X >= Width - FBorderSize) and (X < Width) then
    begin
      Screen.Cursor := crSizeNWSE;
      FDirection := tdBottomRightToTopLeft;
    end
    else
    begin
      Screen.Cursor := crSizeNS;
      FDirection := tdDownToUp;
    end;
  end
  else
  if (X >= 1) and (X <= FBorderSize) then
  begin
    Screen.Cursor := crSizeWE;
    FDirection := tdLeftToRight;
  end
  else
  if (X >= Width - FBorderSize) and (X < Width) then
  begin
    Screen.Cursor := crSizeWE;
    FDirection := tdRightToLeft;
  end
  else
  begin
    Screen.Cursor := crDefault;
    FDirection := tdNone;
  end
end;

procedure TJvMovableBevel.MouseMove(Shift: TShiftState; X, Y: Integer);
//const
//  WM_MOVE = $0003;
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

//Procedure TJvMovableBevel.SelectCursor(X, Y: Longint);
//begin
//  if Y in [0..FBorderSize] then
//  begin
//    If X in [0..FBorderSize] then
//    begin
//      Screen.Cursor:= crsizenwse;
//      FDirection := tdTopLeftToBottomRight;
//    end
//    else
//      if X in [Width-FBorderSize..Width] then
//      begin
//        Screen.Cursor := crsizenesw;
//        FDirection := tdTopRightToBottomLeft;
//      end
//      else
//      begin
//        Screen.Cursor := crsizens;
//        FDirection := tdUpToDown;
//      end;
//  end
//  else
//    if Y in [Height-FBorderSize..Height] then
//    begin
//      If X in [0..FBorderSize] then
//      begin
//        Screen.Cursor:= crsizenesw;
//        FDirection := tdBottomLeftToTopRight;
//      end
//      else
//        if X in [Width-FBorderSize..Width] then
//        begin
//          Screen.Cursor := crsizenwse;
//          FDirection := tdBottomRightToTopLeft;
//        end
//        else
//        begin
//          Screen.Cursor := crSizeNS;
//          FDirection := tdDownToUp;
//        end;
//  end
//  else
//    if (X in [1..FBorderSize]) then
//    begin
//      Screen.Cursor := crsizeWE;
//      FDirection := tdLeftToRight;
//    end
//    else
//      if  (X in [Width-FBorderSize..Width]) then
//      begin
//        Screen.Cursor := crsizeWE;
//        FDirection := tdRightToLeft;
//      end
//      else
//      begin
//        Screen.Cursor := crdefault;
//        FDirection := tdNone;
//      end
//end;{}

procedure TJvMovableBevel.MouseEnter(Control: TControl);
var
  Pos: TPoint;
begin
  if csDesigning in ComponentState then
    Exit;
  Pos := ScreenToClient(Mouse.CursorPos);
  SelectCursor(Pos.X, Pos.Y);
  inherited MouseEnter(Control);
end;

procedure TJvMovableBevel.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if (not FMoving) and (not FSizing) then
  begin
    Screen.Cursor := crDefault;
    FDirection := tdNone;
  end;
  inherited MouseLeave(Control);
end;

end.

