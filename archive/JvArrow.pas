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
Last Modified: 2003-06-11

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvArrow;

interface

uses
  Messages, Windows, Classes, Controls, Graphics,
  JvComponent;

type
  TArrowType = (atDownRight, atDownLeft, atUpRight, atUpLeft,
    atRightDown, atLeftDown, atRightUp, atLeftUp,
    atTopLeftBottomRight, atBottomRightTopLeft,
    atTopRightBottomLeft, atBottomLeftTopRight,
    atLeftRight, atRightLeft, atUpDown, atDownUp
  );

  TCustomArrow = class(TJvGraphicControl)
  private
    FPen: TPen;
    FBrush: TBrush;
    FShape: TArrowType;
    FArrowSize: Integer;
    FArrowWidth: Integer;
    procedure SetBrush(Value: TBrush);
    procedure SetPen(Value: TPen);
    procedure SetArrow(Value: TArrowType);
    procedure DrawArrow(FromX, FromY, ToX, ToY, Size, Width: Integer);
    procedure SetArrowSize(const piValue: Integer);
    procedure SetArrowWidth(const piValue: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ArrowSize: Integer read FArrowSize write SetArrowSize default 5;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 5;
    property Brush: TBrush read FBrush write SetBrush;
    property Pen: TPen read FPen write SetPen;
    property Shape: TArrowType read FShape write SetArrow default atDownRight;

    procedure StyleChanged(Sender: TObject);
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
  Width := 65;
  Height := 65;
  ArrowSize := 5;
  ArrowWidth := 5;
  FPen := TPen.Create;
  FPen.OnChange := StyleChanged;
  FBrush := TBrush.Create;
  FBrush.OnChange := StyleChanged;
  FShape := atDownRight;
end;

destructor TCustomArrow.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TCustomArrow.Paint;
var
  X, Y, W, H: Integer;
  ArrowPoints: array [1..3] of TPoint;
  liSign: Integer;
  Arrow_FromX: Integer;
  Arrow_FromY: Integer;
  Arrow_ToX: Integer;
  Arrow_ToY: Integer;
  GUI_PAD: Integer;
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
    case Shape of
      atRightDown:
        begin
          ArrowPoints[1].x := X + GUI_PAD;
          ArrowPoints[1].y := Y + GUI_PAD;
          ArrowPoints[2].x := (X + (W - GUI_PAD));
          ArrowPoints[2].y := Y + GUI_PAD;
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := (Y + (H - GUI_PAD));
        end;
      atDownLeft:
        begin
          ArrowPoints[1].x := (X + (W - GUI_PAD));
          ArrowPoints[1].y := Y + GUI_PAD;
          ArrowPoints[2].x := ArrowPoints[1].x;
          ArrowPoints[2].y := (Y + (H - GUI_PAD));
          ArrowPoints[3].x := X + GUI_PAD;
          ArrowPoints[3].y := (Y + (H - GUI_PAD));
        end;
      atLeftDown:
        begin
          ArrowPoints[1].x := (X + (W - GUI_PAD));
          ArrowPoints[1].y := Y + GUI_PAD;
          ArrowPoints[2].x := X + GUI_PAD;
          ArrowPoints[2].y := ArrowPoints[1].y;
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := (Y + (H - GUI_PAD));
        end;
      atUpLeft:
        begin
          ArrowPoints[1].x := (X + (W - GUI_PAD));
          ArrowPoints[1].y := (Y + (H - GUI_PAD));
          ArrowPoints[2].x := ArrowPoints[1].x;
          ArrowPoints[2].y := Y + GUI_PAD;
          ArrowPoints[3].x := X + GUI_PAD;
          ArrowPoints[3].y := Y + GUI_PAD;
        end;
      atLeftUp:
        begin
          ArrowPoints[1].x := (X + (W - GUI_PAD));
          ArrowPoints[1].y := (Y + (H - GUI_PAD));
          ArrowPoints[2].x := X + GUI_PAD;
          ArrowPoints[2].y := ArrowPoints[1].y;
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := Y + GUI_PAD;
        end;
      atUpRight:
        begin
          ArrowPoints[1].x := X + GUI_PAD;
          ArrowPoints[1].y := (Y + (H - GUI_PAD));
          ArrowPoints[2].x := ArrowPoints[1].x;
          ArrowPoints[2].y := Y + GUI_PAD;
          ArrowPoints[3].x := (X + (W - GUI_PAD));
          ArrowPoints[3].y := Y + GUI_PAD;
        end;
      atRightUp:
        begin
          ArrowPoints[1].x := X + GUI_PAD;
          ArrowPoints[1].y := (Y + (H - GUI_PAD));
          ArrowPoints[2].x := (X + (W - GUI_PAD));
          ArrowPoints[2].y := ArrowPoints[1].y;
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := Y + GUI_PAD;
        end;
      atTopLeftBottomRight:
        begin
          ArrowPoints[1].x := X + GUI_PAD;
          ArrowPoints[1].y := Y + GUI_PAD;
          ArrowPoints[2].x := (X + (W - GUI_PAD));
          ArrowPoints[2].y := (Y + (H - GUI_PAD));
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := ArrowPoints[2].y;
        end;
      atBottomRightTopLeft:
        begin
          ArrowPoints[2].x := X + GUI_PAD;
          ArrowPoints[2].y := Y + GUI_PAD;
          ArrowPoints[1].x := (X + (W - GUI_PAD));
          ArrowPoints[1].y := (Y + (H - GUI_PAD));
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := ArrowPoints[2].y;
        end;
      atTopRightBottomLeft:
        begin
          ArrowPoints[1].x := (X + (W - GUI_PAD));
          ArrowPoints[1].y := Y + GUI_PAD;
          ArrowPoints[2].x := X + GUI_PAD;
          ArrowPoints[2].y := (Y + (H - GUI_PAD));
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := ArrowPoints[2].y;
        end;
      atBottomLeftTopRight:
        begin
          ArrowPoints[2].x := (X + (W - GUI_PAD));
          ArrowPoints[2].y := Y + GUI_PAD;
          ArrowPoints[1].x := X + GUI_PAD;
          ArrowPoints[1].y := (Y + (H - GUI_PAD));
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := ArrowPoints[2].y;
        end;
      atLeftRight:
        begin
          ArrowPoints[1].x := X + GUI_PAD;
          ArrowPoints[1].y := Y + GUI_PAD;
          ArrowPoints[2].x := (X + (W - GUI_PAD));
          ArrowPoints[2].y := Y + GUI_PAD;
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := ArrowPoints[2].y;
        end;
      atRightLeft:
        begin
          ArrowPoints[1].x := (X + (W - GUI_PAD));
          ArrowPoints[1].y := Y + GUI_PAD;
          ArrowPoints[2].x := X + GUI_PAD;
          ArrowPoints[2].y := ArrowPoints[1].y;
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := ArrowPoints[2].y;
        end;
      atUpDown:
        begin
          ArrowPoints[1].x := X + GUI_PAD;
          ArrowPoints[1].y := Y + GUI_PAD;
          ArrowPoints[2].x := X + GUI_PAD;
          ArrowPoints[2].y := Y + GUI_PAD;
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := (Y + (H - GUI_PAD));
        end;
      atDownUp:
        begin
          ArrowPoints[1].x := X + GUI_PAD;
          ArrowPoints[1].y :=(Y + (H - GUI_PAD));
          ArrowPoints[2].x := X + GUI_PAD;
          ArrowPoints[2].y := (Y + (H - GUI_PAD));
          ArrowPoints[3].x := ArrowPoints[2].x;
          ArrowPoints[3].y := Y + GUI_PAD;
        end;
    else
      ArrowPoints[1].x := X + GUI_PAD;
      ArrowPoints[1].y := Y + GUI_PAD;
      ArrowPoints[2].x := ArrowPoints[1].x;
      ArrowPoints[2].y := (Y + (H - GUI_PAD));
      ArrowPoints[3].x := (X + (W - GUI_PAD));
      ArrowPoints[3].y := (Y + (H - GUI_PAD));
    end;
    {draw lines}
    Canvas.PolyLine(ArrowPoints);

    {------------------------ARROWS----------------------------}

    if Shape in [atDownLeft, atDownRight, atUpLeft, atUpRight] then
    begin
      {left or right}
      if Shape in [atUpLeft, atDownLeft] then
        liSign := -1
      else
        liSign := +1;
      Arrow_FromX := ArrowPoints[3].x;
      Arrow_FromY := ArrowPoints[3].y;
      Arrow_ToX   := ArrowPoints[3].x + (ArrowSize * liSign);
      Arrow_ToY   := ArrowPoints[3].y;
    end
    else
    if Shape in [atTopLeftBottomRight, atBottomLeftTopRight] then
    begin
//      Arrow_FromX := 0;
//      Arrow_FromY := 0;
//      Arrow_ToY := ArrowPoints[3].y + ArrowSize;
      Arrow_ToX := ArrowPoints[3].x + ArrowSize;

      {down or up}
      if Shape in [atBottomLeftTopRight] then
        Arrow_ToY := ArrowPoints[3].y - ArrowSize
      else
        Arrow_ToY := ArrowPoints[3].y + ArrowSize;

      Arrow_FromX := ArrowPoints[3].x;
      Arrow_FromY := ArrowPoints[3].y;
    end
    else if Shape in [atBottomRightTopLeft, atTopRightBottomLeft] then
    begin
//      Arrow_FromX := 0;
//      Arrow_FromY := 0;
      Arrow_ToX := ArrowPoints[3].X - ArrowSize;
      {down or up}
      if Shape in [atBottomRightTopLeft] then
        Arrow_ToY := ArrowPoints[3].y - ArrowSize
      else
        Arrow_ToY := ArrowPoints[3].y + ArrowSize;
      Arrow_FromX := ArrowPoints[3].x;
      Arrow_FromY := ArrowPoints[3].y;
    end
    else if Shape in [atLeftRight, atRightLeft] then
    begin
      {left or right}
      if Shape in [atRightLeft] then
        liSign := -1
      else
        liSign := +1;
      Arrow_FromX := ArrowPoints[3].x;
      Arrow_FromY := ArrowPoints[3].y;
      Arrow_ToX   := ArrowPoints[3].x + (ArrowSize * liSign);
      Arrow_ToY   := ArrowPoints[3].y;
    end
    else
    begin
      {down or up}
      if Shape in [atLeftUp, atRightUp, atDownUp] then
        liSign := -1
      else
        liSign := +1;
      Arrow_FromX := ArrowPoints[3].x;
      Arrow_FromY := ArrowPoints[3].y;
      Arrow_ToX   := ArrowPoints[3].x;
      Arrow_ToY   := ArrowPoints[3].y + (ArrowSize * liSign);
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

procedure TCustomArrow.SetArrow(Value: TArrowType);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    Invalidate;
  end;
end;


{ *** DrawArrow Procedure  ***
  Written By Scott M. Straley (straley@fast.net) -- March 15, 1995}
procedure TCustomArrow.DrawArrow(FromX, FromY, ToX, ToY, Size, Width: Integer);
var
  Line1, Line2, ShortLine1, ShortLine2, ArrowX,
  ArrowY, Point1X, Point1Y, Point2X, Point2Y: Integer;
  Angle: Real;
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

  if Line2 <> 0 then
  begin
    Angle := ArcTan(Line1 / Line2);
  end
  else
  begin
    if Line1 > 0 then
      Angle := -1.5707
    else
      Angle := 1.5707;
  end;

  {now determine where the back of the arrow is}

  if ToX > FromX then
  begin
    ShortLine1 := Round(Size * Sin(Angle));
    ShortLine2 := Round(Size * Cos(Angle));
    ArrowX     := ToX - ShortLine2;
    ArrowY     := ToY - ShortLine1;
  end
  else
  begin
    ShortLine1 := Round(Size * Sin(Angle));
    ShortLine2 := Round(Size * Cos(Angle));
    ArrowX     := ToX + ShortLine2;
    ArrowY     := ToY + ShortLine1;
  end;

  {now determine points perpendictular to the
   arrow line}

  Point1X := ArrowX - Round(Width * (Sin(Angle)));
  Point1Y := ArrowY + Round(Width * (Cos(Angle)));
  Point2X := ArrowX + Round(Width * (Sin(Angle)));
  Point2Y := ArrowY - Round(Width * (Cos(Angle)));

  Canvas.MoveTo(FromX, FromY);
  Canvas.LineTo(ToX, ToY);
  // 11/18/99 Michael Beck
  // need to adjust for "FromX=ToX" as the current Polygon is drawing Arrowhead in the other direction
  if FromX = ToX then
    Canvas.Polygon([Point(Point2X, ToY - (Point2Y - ToY)),
      Point(Point1X, ToY - (Point2Y - ToY)), Point(ToX, ToY)])
  else
  //end of Beck's correction
    Canvas.Polygon([Point(Point2X, Point2Y), Point(Point1X, Point1Y), Point(ToX, ToY)]);
end;

procedure TCustomArrow.SetArrowSize(const piValue: Integer);
begin
  FArrowSize := piValue;
  Invalidate;
end;

procedure TCustomArrow.SetArrowWidth(const piValue: Integer);
begin
  FArrowWidth := piValue;
  Invalidate;
end;

end.

