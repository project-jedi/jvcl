{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgGraph.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgGraph;

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ExtCtrls,
  JvgTypes, JvgCommClasses, JvgUtils, JvComponent;

const
  MaxPointsCount = 30;

type
  TJvgGraph = class(TJvGraphicControl)
  private
  protected
    procedure Paint; override;
  public
    PenWidth: Integer;
    MaxValue: Integer;
    PointsCount: Integer;
    DrawPointsCount: Integer;
    YPoints: array [0..MaxPointsCount] of Integer;
    constructor Create(AOwner: TComponent); override;
  published
    property Color;
    property Height default 50;
    property Width default 50;
  end;

implementation

constructor TJvgGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 50;
  Height := 50;
  MaxValue := 100;
  PointsCount := MaxPointsCount;
  DrawPointsCount := MaxPointsCount;
end;

procedure TJvgGraph.Paint;
var
  R: TRect;
  I: Integer;
  Points: array [0..MaxPointsCount] of TPoint;
  ShadowPoints: array [0..MaxPointsCount] of TPoint;

  procedure OffsetPoints(X, Y: Integer);
  var
    I: Integer;
  begin
    for I := 0 to PointsCount do
    begin
      Inc(Points[I].X, X);
      Inc(Points[I].Y, Y);
    end;
  end;

begin
  inherited Paint;
  R := ClientRect;
  InflateRect(R, -2, -2);

  for I := 0 to PointsCount do
  begin
    Points[I].X := MulDiv(I, Width, PointsCount);
    Points[I].Y := Height - MulDiv(YPoints[I], Height, MaxValue);
    ShadowPoints[I].X := Points[I].X + 6;
    ShadowPoints[I].Y := Points[I].Y + 6;
  end;

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
end;

end.

