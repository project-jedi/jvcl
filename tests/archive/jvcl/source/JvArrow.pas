{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvArrow.pas, released November 1999.

The Initial Developer of the Original Code is Russell Fox.
Portions created by Anthony Steele are Copyright (C) 1999-2001 Russell Fox.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvArrow;

interface

uses Messages, Windows, Classes, Controls, Graphics;

type
  TArrowType = (atDownRight, atDownLeft, atUpRight, atUpLeft,
      atRightDown, atLeftDown, atRightUp, atLeftUp,
      atTopLeftBottomRight, atBottomRightTopLeft,
      atTopRightBottomLeft, atBottomLeftTopRight);

  TCustomArrow = class(TGraphicControl)
  private
    FPen: TPen;
    FBrush: TBrush;
    FShape: TArrowType;
    fiArrowSize: integer;
    fiArrowWidth: integer;
    procedure SetBrush(Value: TBrush);
    procedure SetPen(Value: TPen);
    procedure SeTArrow(Value: TArrowType);
    procedure DrawArrow(FromX, FromY, ToX, ToY, Size, Width: integer);
    procedure SetArrowSize(const piValue: integer);
    procedure SetArrowWidth(const piValue: integer);
  protected
    procedure Paint; override;

    property ArrowSize: integer read fiArrowSize write SetArrowSize;
    property ArrowWidth: integer read fiArrowWidth write SetArrowWidth;
    property Brush: TBrush read FBrush write SetBrush;
    property Pen: TPen read FPen write SetPen;
    property Shape: TArrowType read FShape write SeTArrow default atDownRight;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StyleChanged(Sender: TObject);
  published
  end;

  TJvArrow = class(TCustomArrow)
    published
    property Align;
    property Anchors;
    property Brush;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property ParentShowHint;
    property Pen;
    property Shape;
    property ShowHint;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property ArrowSize;
    property ArrowWidth;
  end;


implementation


constructor TCustomArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width        := 65;
  Height       := 65;
  ArrowSize    := 5;
  ArrowWidth   := 5;
  FPen         := TPen.Create;
  FPen.OnChange := StyleChanged;
  FBrush       := TBrush.Create;
  FBrush.OnChange := StyleChanged;
end;

destructor TCustomArrow.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TCustomArrow.Paint;
var
  X, Y, W, H: integer;
  lcpntArray: array [1..3] of TPoint;
  liSign:     integer;
  Arrow_FromX,
  Arrow_FromY,
  Arrow_ToX,
  Arrow_ToY:  integer;
  GUI_PAD:    integer;
begin
  if ArrowWidth > ArrowSize then
    GUI_PAD := ArrowWidth + 2
  else
    GUI_PAD := ArrowSize + 2;

  with Canvas do
  begin
    Pen   := FPen;
    Brush := FBrush;
    X     := Pen.Width div 2;
    Y     := X;
    W     := Width - Pen.Width + 1;
    H     := Height - Pen.Width + 1;

    if Pen.Width = 0 then
    begin
      Dec(W);
      Dec(H);
    end;

    if FShape in [atDownRight] then
    begin
      lcpntArray[1].x := X + GUI_PAD;
      lcpntArray[1].y := Y + GUI_PAD;
      lcpntArray[2].x := lcpntArray[1].x;
      lcpntArray[2].y := (Y + (H - GUI_PAD));
      lcpntArray[3].x := (X + (W - GUI_PAD));
      lcpntArray[3].y := (Y + (H - GUI_PAD));
    end;

    if FShape in [atRightDown] then
    begin
      lcpntArray[1].x := X + GUI_PAD;
      lcpntArray[1].y := Y + GUI_PAD;
      lcpntArray[2].x := (X + (W - GUI_PAD));
      lcpntArray[2].y := Y + GUI_PAD;
      lcpntArray[3].x := lcpntArray[2].x;
      lcpntArray[3].y := (Y + (H - GUI_PAD));
    end;

    if FShape in [atDownLeft] then
    begin
      lcpntArray[1].x := (X + (W - GUI_PAD));
      lcpntArray[1].y := Y + GUI_PAD;
      lcpntArray[2].x := lcpntArray[1].x;
      lcpntArray[2].y := (Y + (H - GUI_PAD));
      lcpntArray[3].x := X + GUI_PAD;
      lcpntArray[3].y := (Y + (H - GUI_PAD));
    end;

    if FShape in [atLeftDown] then
    begin
      lcpntArray[1].x := (X + (W - GUI_PAD));
      lcpntArray[1].y := Y + GUI_PAD;
      lcpntArray[2].x := X + GUI_PAD;
      lcpntArray[2].y := lcpntArray[1].y;
      lcpntArray[3].x := lcpntArray[2].x;
      lcpntArray[3].y := (Y + (H - GUI_PAD));
    end;

    if FShape in [atUpLeft] then
    begin
      lcpntArray[1].x := (X + (W - GUI_PAD));
      lcpntArray[1].y := (Y + (H - GUI_PAD));
      lcpntArray[2].x := lcpntArray[1].x;
      lcpntArray[2].y := Y + GUI_PAD;
      lcpntArray[3].x := X + GUI_PAD;
      lcpntArray[3].y := Y + GUI_PAD;
    end;

    if FShape in [atLeftUp] then
    begin
      lcpntArray[1].x := (X + (W - GUI_PAD));
      lcpntArray[1].y := (Y + (H - GUI_PAD));
      lcpntArray[2].x := X + GUI_PAD;
      lcpntArray[2].y := lcpntArray[1].y;
      lcpntArray[3].x := lcpntArray[2].x;
      lcpntArray[3].y := Y + GUI_PAD;
    end;

    if FShape in [atUpRight] then
    begin
      lcpntArray[1].x := X + GUI_PAD;
      lcpntArray[1].y := (Y + (H - GUI_PAD));
      lcpntArray[2].x := lcpntArray[1].x;
      lcpntArray[2].y := Y + GUI_PAD;
      lcpntArray[3].x := (X + (W - GUI_PAD));
      lcpntArray[3].y := Y + GUI_PAD;
    end;

    if FShape in [atRightUp] then
    begin
      lcpntArray[1].x := X + GUI_PAD;
      lcpntArray[1].y := (Y + (H - GUI_PAD));
      lcpntArray[2].x := (X + (W - GUI_PAD));
      lcpntArray[2].y := lcpntArray[1].y;
      lcpntArray[3].x := lcpntArray[2].x;
      lcpntArray[3].y := Y + GUI_PAD;
    end;

    if FShape in [atTopLeftBottomRight] then
    begin
      lcpntArray[1].x := X + GUI_PAD;
      lcpntArray[1].y := Y + GUI_PAD;
      lcpntArray[2].x := (X + (W - GUI_PAD));
      lcpntArray[2].y := (Y + (H - GUI_PAD));
      lcpntArray[3].x := lcpntArray[2].x;
      lcpntArray[3].y := lcpntArray[2].y;
    end;

    if FShape in [atBottomRightTopLeft] then
    begin
      lcpntArray[2].x := X + GUI_PAD;
      lcpntArray[2].y := Y + GUI_PAD;
      lcpntArray[1].x := (X + (W - GUI_PAD));
      lcpntArray[1].y := (Y + (H - GUI_PAD));
      lcpntArray[3].x := lcpntArray[2].x;
      lcpntArray[3].y := lcpntArray[2].y;
    end;

    if FShape in [atTopRightBottomLeft] then
    begin
      lcpntArray[1].x := (X + (W - GUI_PAD));
      lcpntArray[1].y := Y + GUI_PAD;
      lcpntArray[2].x := X + GUI_PAD;
      lcpntArray[2].y := (Y + (H - GUI_PAD));
      lcpntArray[3].x := lcpntArray[2].x;
      lcpntArray[3].y := lcpntArray[2].y;
    end;

    if FShape in [atBottomLeftTopRight] then
    begin
      lcpntArray[2].x := (X + (W - GUI_PAD));
      lcpntArray[2].y := Y + GUI_PAD;
      lcpntArray[1].x := X + GUI_PAD;
      lcpntArray[1].y := (Y + (H - GUI_PAD));
      lcpntArray[3].x := lcpntArray[2].x;
      lcpntArray[3].y := lcpntArray[2].y;
    end;

    {draw lines}
    Canvas.PolyLine(lcpntArray);

{------------------------ARROWS----------------------------}

    if FShape in [atDownLeft, atDownRight, atUpLeft, atUpRight] then
    begin
      {left or right}
      if FShape in [atUpLeft, atDownLeft] then
        liSign := -1
      else
        liSign := +1;
      Arrow_FromX := lcpntArray[3].x;
      Arrow_FromY := lcpntArray[3].y;
      Arrow_ToX   := lcpntArray[3].x + (ArrowSize * liSign);
      Arrow_ToY   := lcpntArray[3].y;
    end 
    else if FShape in [atTopLeftBottomRight, atBottomLeftTopRight] then
    begin
//      Arrow_FromX := 0;
//      Arrow_FromY := 0;
//      Arrow_ToY := lcpntArray[3].y + ArrowSize;
      Arrow_ToX := lcpntArray[3].x + ArrowSize;

      {down or up}
      if FShape in [atBottomLeftTopRight] then
        Arrow_ToY := lcpntArray[3].y - ArrowSize
      else
        Arrow_ToY := lcpntArray[3].y + ArrowSize;

      Arrow_FromX := lcpntArray[3].x;
      Arrow_FromY := lcpntArray[3].y;
    end 
    else if FShape in [atBottomRightTopLeft, atTopRightBottomLeft] then
    begin
//      Arrow_FromX := 0;
//      Arrow_FromY := 0;
      Arrow_ToX := lcpntArray[3].X - ArrowSize;
      {down or up}
      if FShape in [atBottomRightTopLeft] then
        Arrow_ToY := lcpntArray[3].y - ArrowSize
      else
        Arrow_ToY := lcpntArray[3].y + ArrowSize;
      Arrow_FromX := lcpntArray[3].x;
      Arrow_FromY := lcpntArray[3].y;
    end 
    else
    begin
      {down or up}
      if FShape in [atLeftUp, atRightUp] then
        liSign := -1
      else
        liSign := +1;
      Arrow_FromX := lcpntArray[3].x;
      Arrow_FromY := lcpntArray[3].y;
      Arrow_ToX   := lcpntArray[3].x;
      Arrow_ToY   := lcpntArray[3].y + (ArrowSize * liSign);;
    end;

    DrawArrow(Arrow_FromX, Arrow_FromY, Arrow_ToX, Arrow_ToY, ArrowSize, ArrowWidth);
  end;
end;

procedure TCustomArrow.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomArrow.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TCustomArrow.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TCustomArrow.SeTArrow(Value: TArrowType);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    Invalidate;
  end;
end;





procedure TCustomArrow.DrawArrow(FromX, FromY, ToX, ToY, Size, Width: integer);

{ *** DrawArrow Procedure  ***
  Written By Scott M. Straley (straley@fast.net) -- March 15, 1995}
var
  Line1, Line2, ShortLine1, ShortLine2, ArrowX,
  ArrowY, Point1X, Point1Y, Point2X, Point2Y: integer;
  Angle:                                      real;
begin
  {determining angle of X2 of line based on:

     X1
     |\
     | \  hypotneus
  L1 |  \
     |   \
     -----X2
       L2                                     }

  Line1 := (FromY - ToY);
  Line2 := (FromX - ToX);

  {We need this code to prevent DivByZero errors}

  if (Line2 <> 0) then
  begin
    Angle := arctan(Line1 / Line2);
  end
  else
  begin
    if Line1 > 0 then
      Angle := -1.5707
    else
      Angle := 1.5707;
  end;

  {now determine where the back of the arrow is}

  if (ToX > FromX) then
  begin
    ShortLine1 := Round(Size * sin(Angle));
    ShortLine2 := Round(Size * cos(Angle));
    ArrowX     := ToX - ShortLine2;
    ArrowY     := ToY - ShortLine1;
  end
  else
  begin
    ShortLine1 := Round(Size * sin(Angle));
    ShortLine2 := Round(Size * cos(Angle));
    ArrowX     := ToX + ShortLine2;
    ArrowY     := ToY + ShortLine1;
  end;

  {now determine points perpendictular to the
   arrow line}

  Point1X := ArrowX - Round(Width * (sin(Angle)));
  Point1Y := ArrowY + Round(Width * (cos(Angle)));
  Point2X := ArrowX + Round(Width * (sin(Angle)));
  Point2Y := ArrowY - Round(Width * (cos(Angle)));

  Canvas.MoveTo(FromX, FromY);
  Canvas.LineTo(ToX, ToY);
// 11/18/99 Michael Beck
//need to adjust for "FromX=ToX" as the current Polygon is drawing Arrowhead  in the other direction
  if FromX = ToX then
    Canvas.Polygon([Point(Point2X, ToY - (Point2Y - ToY)),
      Point(Point1X, ToY - (Point2Y - ToY)), Point(ToX, ToY)])
  else
//end of Beck's correction
    Canvas.Polygon([Point(Point2X, Point2Y), Point(Point1X, Point1Y), Point(ToX, ToY)]);
end;

procedure TCustomArrow.SetArrowSize(const piValue: integer);
begin
  fiArrowSize := piValue;
  Invalidate;
end;

procedure TCustomArrow.SetArrowWidth(const piValue: integer);
begin
  fiArrowWidth := piValue;
  Invalidate;
end;

end.
