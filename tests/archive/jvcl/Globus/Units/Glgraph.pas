unit glGraph;

interface
{$I glDEF.INC}
uses
  Windows, Messages, Classes, Controls, Graphics, glTypes, glCommCl,
  glUtils, ExtCtrls, glBevel;

const
  MaxPointsCount = 30;
type

  TglGraph = class(TgraphicControl)//TglBevel)
  private
//    procedure SetBevelInner(Value: TPanelBevel);
//    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
//    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;

  public
    PenWidth: integer;
    MaxValue: integer;
    PointsCount: integer;
    DrawPointsCount: integer;
    yPoints: array[0..MaxPointsCount] of integer;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

  published
    property Color;
//    property HorLines: TglGraphLines read FHorLines write FHorLines;
  end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('Proba', [TglGraph]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _
constructor TglGraph.Create( AOwner : TComponent );
var i: integer;
begin
  inherited;
  //..defaults
  Width:=50; Height:=50;
  MaxValue := 100;
  PointsCount := MaxPointsCount;
  DrawPointsCount := MaxPointsCount;
//  for i:=0 to 10 do yPoints[i] := Random(100);//( 1,70,60,40,9,10,10,10,88,10, 5 );
end;

destructor TglGraph.Destroy;
begin
  inherited;
end;

procedure TglGraph.Paint;
var
  r, r_: TRect;
  i: integer;
  BoxSides: TglSides;
  Points: array[0..MaxPointsCount] of TPoint;
  ShadowPoints: array[0..MaxPointsCount] of TPoint;
  procedure OffsetPoints( x,y: integer );
  var i: integer;
  begin
    for i := 0 to PointsCount do
    begin
      inc( Points[i].x, X );
      inc( Points[i].y, Y );
    end;
  end;
begin
  inherited;
  r := ClientRect;
  InflateRect( r, -2, -2 );

  for i := 0 to PointsCount do
  begin
    Points[i].x := MulDiv( i, Width, PointsCount);
    Points[i].y := Height - MulDiv( yPoints[i], Height, MaxValue);
    ShadowPoints[i].x := Points[i].x+6;
    ShadowPoints[i].y := Points[i].y+6;
  end;

  Canvas.Pen.Width := PenWidth;
//  Canvas.Pen.Color := clBtnShadow;
//  Canvas.Polyline( ShadowPoints );

  //..3D shadow
  Canvas.Pen.Color :=  DecColor( Color, 100 );
  Canvas.Polyline( Slice(Points, DrawPointsCount+1) );

  OffsetPoints( -2, -2 );
  Canvas.Pen.Color := IncColor( Color, 200 );
  Canvas.Polyline( Slice(Points, DrawPointsCount+1) );

  OffsetPoints( 1, 1 );
  Canvas.Pen.Color := Color;
  Canvas.Polyline( Slice(Points, DrawPointsCount+1) );

end;


end.
