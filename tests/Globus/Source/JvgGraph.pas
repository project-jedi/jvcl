{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgGraph.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

UNIT JvgGraph;

INTERFACE

USES
   Windows,
   Messages,
   Classes,
   Controls,
   Graphics,
   JvgTypes,
   JvgCommClasses,
   JvgUtils,
   ExtCtrls,
   JVComponent,
   JvgBevel;

CONST
   MaxPointsCount             = 30;
TYPE

   TJvgGraph = CLASS(TJvGraphicControl) //TJvgBevel)
   PRIVATE
      //    procedure SetBevelInner(Value: TPanelBevel);
      //    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
      //    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   PROTECTED
      PROCEDURE Paint; OVERRIDE;

   PUBLIC
      PenWidth: integer;
      MaxValue: integer;
      PointsCount: integer;
      DrawPointsCount: integer;
      yPoints: ARRAY[0..MaxPointsCount] OF integer;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

   PUBLISHED
      PROPERTY Color;
      //    property HorLines: TglGraphLines read FHorLines write FHorLines;
   END;

IMPLEMENTATION
//________________________________________________________ Methods _

CONSTRUCTOR TJvgGraph.Create(AOwner: TComponent);
VAR
   i                          : integer;
BEGIN
   INHERITED;
   //..defaults
   Width := 50;
   Height := 50;
   MaxValue := 100;
   PointsCount := MaxPointsCount;
   DrawPointsCount := MaxPointsCount;
   //  for i:=0 to 10 do yPoints[i] := Random(100);//( 1,70,60,40,9,10,10,10,88,10, 5 );
END;

DESTRUCTOR TJvgGraph.Destroy;
BEGIN
   INHERITED;
END;

PROCEDURE TJvgGraph.Paint;
VAR
   r, r_                      : TRect;
   i                          : integer;
   BoxSides                   : TglSides;
   Points                     : ARRAY[0..MaxPointsCount] OF TPoint;
   ShadowPoints               : ARRAY[0..MaxPointsCount] OF TPoint;

   PROCEDURE OffsetPoints(x, y: integer);
   VAR
      i                       : integer;
   BEGIN
      FOR i := 0 TO PointsCount DO
      BEGIN
         inc(Points[i].x, X);
         inc(Points[i].y, Y);
      END;
   END;
BEGIN
   INHERITED;
   r := ClientRect;
   InflateRect(r, -2, -2);

   FOR i := 0 TO PointsCount DO
   BEGIN
      Points[i].x := MulDiv(i, Width, PointsCount);
      Points[i].y := Height - MulDiv(yPoints[i], Height, MaxValue);
      ShadowPoints[i].x := Points[i].x + 6;
      ShadowPoints[i].y := Points[i].y + 6;
   END;

   Canvas.Pen.Width := PenWidth;
   //  Canvas.Pen.Color := clBtnShadow;
   //  Canvas.Polyline( ShadowPoints );

     //..3D shadow
   Canvas.Pen.Color := DecColor(Color, 100);
   Canvas.Polyline(Slice(Points, DrawPointsCount + 1));

   OffsetPoints(-2, -2);
   Canvas.Pen.Color := IncColor(Color, 200);
   Canvas.Polyline(Slice(Points, DrawPointsCount + 1));

   OffsetPoints(1, 1);
   Canvas.Pen.Color := Color;
   Canvas.Polyline(Slice(Points, DrawPointsCount + 1));

END;

END.

