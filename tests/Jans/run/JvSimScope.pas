{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimScope.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
{$A+,B-,D-,L-,M-,O+,Q-,R-,S-,X+,Y-}
{  TJvSimScope Properties:
     Active           Starts/Stops scope
     Color            Backgroundcolor
     Gridcolor        Grid mask color
     Gridsize         Size of grid mask in pixels
     Height           Scopes height in pixels
     Interval         Scroll speed in 1/100's seconds
     LineColor        Scope dataline color
     Position         Dataline value (range 0-100)
     Width            Scopes width in pixels
     Basecolor        Color of baseline
     Baseline         Baseline value (range 0-100)

  TJvSimScope Methods:
     Clear            Clears the control and redraws grid}

unit JvSimScope;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TJvSimScope = class(TGraphicControl)

  private
    fAllowed: boolean;
    fOnUpdate: TNotifyEvent;
    DrawBuffer: TBitmap;
    DrawTimer: TTimer;
    fActive: boolean;
    fBaseColor, { Baseline color }
    fColor, { Background color }
    fGridColor, { Grid line color }
    fLineColor: TColor; { Position line color }
    fBaseLine,
      fGridSize,
      fPosition, { Value to plot }
    fInterval: integer;
    FPosition2: integer;
    FLineColor2: Tcolor; { Update speed in 1/10 seconds }
    procedure SetActive(value: boolean);
    procedure SetGridSize(value: integer);
    procedure SetBaseLine(value: integer);
    procedure SetInterval(value: integer);

  protected
    Oldpos, PrevPos: integer;
    Oldpos2, PrevPos2: integer;
    CalcBase, Counter: integer;
    procedure UpdateScope(Sender: TObject);
    procedure Loaded; override;
  public
    procedure Paint; override;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Free;
    procedure Clear;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Baseline: integer read fBaseline write SetBaseLine;
    property Gridsize: integer read fGridSize write SetGridSize;
    property Active: boolean read fActive write SetActive;
    property Position: Integer read fPosition write fPosition;
    property Position2: integer read FPosition2 write fPosition2;
    property Interval: Integer read fInterval write SetInterval;
    { Color properties }
    property Color: TColor read fColor write fColor;
    property Gridcolor: TColor read fGridColor write fGridColor;
    property Linecolor: TColor read fLineColor write fLineColor;
    property LineColor2: Tcolor read FLineColor2 write fLineColor2;
    property Basecolor: TColor read fBaseColor write fBaseColor;

    property OnUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;
    { Standard properties }
    property Height;
    property Width;
    { Standard events }
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

{ --- TJvSimScope ----------------------------------------------------------------- }

constructor TJvSimScope.Create(AnOwner: TComponent);
{ Create control and background draw buffer and timer
}
begin
  inherited Create(AnOwner);
  fAllowed := FALSE;
  DrawBuffer := TBitmap.Create;
  DrawBuffer.Canvas.Brush.Color := FColor;
  DrawBuffer.Canvas.Brush.Style := bsSolid;
  DrawBuffer.Canvas.Pen.Width := 1;
  DrawBuffer.Canvas.Pen.Style := psSolid;

  DrawTimer := TTimer.Create(SELF);
  DrawTimer.Enabled := FALSE;
  DrawTimer.OnTimer := UpdateScope;
  DrawTimer.Interval := 500;

  Height := 120;
  Width := 208;

  Color := clBlack;
  GridColor := clGreen;
  LineColor := clLime;
  LineColor2 := clFuchsia;
  BaseColor := clRed;

  BaseLine := 50;
  GridSize := 16;

  Position := 50;
  Position2 := 50;
  Interval := 50;
  Counter := 1;

  ControlStyle := [csDesignInteractive, csFramed, csOpaque];
  fAllowed := TRUE;
end;

procedure TJvSimScope.Loaded;
{ Finished loading, now allow redraw when control is changed
}
begin
  inherited Loaded;
  fAllowed := TRUE;
end;

procedure TJvSimScope.Clear;
{ Redraw control, re-calculate grid etc
}
var
  a: integer;
begin
  CalcBase := (height - round(height / 100 * FBaseline));
  with DrawBuffer.Canvas do
  begin
    Brush.Color := FColor;
    Pen.Style := psClear;
    Rectangle(0, 0, Width + 1, height + 1);
    Pen.Style := psSolid;
    Pen.Color := FGridColor;
    Pen.Width := 1;
    { Vertical lines }
    a := Width;
    while a > 0 do
    begin
      MoveTo(a - 1, 0);
      LineTo(a - 1, Height);
      dec(a, FGridSize);
    end;
    { Horizontal lines - above Baseline }
    a := CalcBase;
    while a < height do
    begin
      inc(a, FGridSize);
      MoveTo(0, a);
      LineTo(Width, a);
    end;
    { Horizontal lines - below Baseline }
    a := CalcBase;
    while a > 0 do
    begin
      Dec(a, FGridSize);
      MoveTo(0, a);
      LineTo(Width, a);
    end;
    { Baseline }
    Pen.Color := FBaseColor;
    MoveTo(0, CalcBase);
    LineTo(Width, CalcBase);

    { Start new position-line on baseline... }
    OldPos := CalcBase;
    PrevPos := CalcBase;
    OldPos2 := CalcBase;
    PrevPos2 := CalcBase;

    {
    // Draws a line from 0,baseline to width, new pos
    Pen.Color:=FLineColor;
    MoveTo(0,height);
    LineTo(Width,height-round(height/100*position));
    }
    counter := 1;
  end;
end;

procedure TJvSimScope.Free;
{ Free control and all internal objects
}
begin
  DrawTimer.Free;
  DrawBuffer.Free;
  inherited Free;
end;

destructor TJvSimScope.Destroy;
begin
  if DrawTimer <> nil then
    DrawTimer.Destroy;
  if DrawBuffer <> nil then
    DrawBuffer.Destroy;
  inherited Destroy;
end;

procedure TJvSimScope.SetBaseLine(value: integer);
{ Set base-linje value
}
begin
  fBaseLine := value;
  CalcBase := (height - round(height / 100 * FBaseline));
  if fAllowed then
  begin
    Clear;
    if parent <> nil then Paint;
  end;
end;

procedure TJvSimScope.SetInterval(value: integer);
{ Set Scroll delay
}
begin
  DrawTimer.Enabled := FALSE;
  CalcBase := (height - round(height / 100 * FBaseline));
  DrawTimer.Interval := value * 10;
  fInterval := value;
  DrawTimer.Enabled := FActive;
end;

procedure TJvSimScope.SetGridSize(value: integer);
{ Set grid size }
begin
  fGridSize := (value div 2) * 2;
  if fAllowed then
  begin
    Clear;
    if parent <> nil then Paint;
  end;
end;

procedure TJvSimScope.SetActive(value: boolean);
{ Start scrolling
}
begin
  CalcBase := (height - round(height / 100 * FBaseline));
  DrawTimer.Interval := Interval * 10;
  DrawTimer.Enabled := value;
  fActive := Value;
end;

procedure TJvSimScope.UpdateScope(Sender: TObject);
{ All drawings is performed on in the drawbuffer to speed up
  proceedings and eliminate flicker. The Paint procedure merely
  copies the contents of the drawbuffer.
}
var
  a: integer;
  Des, Src: TRect;
begin
  with DrawBuffer.Canvas do
  begin
    Pen.Color := FGridColor;

    Des.Top := 0;
    Des.Left := 0;
    Des.Right := Width - 2;
    Des.Bottom := Height;

    Src.Top := 0;
    Src.Left := 2;
    Src.Right := Width;
    Src.Bottom := Height;
    { Copy bitmap leftwards }
    CopyRect(Des, DrawBuffer.Canvas, Src);

    { Draw new area }
    Pen.Color := FColor;
    Pen.Width := 2;
    MoveTo(Width - 1, 0);
    LineTo(Width - 1, Height);
    Pen.Color := FGridColor;
    Pen.Width := 1;
    { Draw vertical line if needed }
    if counter = (GridSize div 2) then
    begin
      MoveTo(Width - 1, 0);
      LineTo(Width - 1, Height);
      counter := 0;
    end;
    Inc(counter);
    { Horizontal lines - above Baseline }
    a := CalcBase;
    while a < height do
    begin
      inc(a, FGridSize);
      MoveTo(Width - 2, a);
      LineTo(Width, a);
    end;
    { Horizontal lines - below Baseline }
    a := CalcBase;
    while a > 0 do
    begin
      Dec(a, FGridSize);
      MoveTo(Width - 2, a);
      LineTo(Width, a);
    end;
    { Baseline }
    Pen.Color := FBaseColor;
    MoveTo(Width - 2, CalcBase);
    LineTo(Width, CalcBase);
    { Draw position for line 1}
    Pen.Color := FLineColor;
    a := height - round(height / 100 * position);
    MoveTo(Width - 4, OldPos);
    LineTo(Width - 2, PrevPos);
    LineTo(Width - 0, a);
    OldPos := PrevPos;
    PrevPos := a;
    { Draw position for line 2}
    Pen.Color := FLineColor2;
    a := height - round(height / 100 * position2);
    MoveTo(Width - 4, OldPos2);
    LineTo(Width - 2, PrevPos2);
    LineTo(Width - 0, a);
    OldPos2 := PrevPos2;
    PrevPos2 := a;
  end;
  paint;
  if assigned(FOnUpdate) then fOnUpdate(SELF);
end;

procedure TJvSimScope.Paint;
{ Called by timer to show updates
}
var
  Rect: TRect;
begin
  inherited Paint;
  DrawBuffer.Height := Height;
  DrawBuffer.Width := Width;
  Rect.Top := 0;
  Rect.Left := 0;
  Rect.Right := Width;
  Rect.Bottom := Height;
  Canvas.CopyRect(Rect, DrawBuffer.Canvas, Rect);
  fAllowed := TRUE;
end;

procedure TJvSimScope.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
{ Recalulate control after move and/or resize
}
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  DrawBuffer.Height := Height;
  DrawBuffer.Width := Width;
  if (csDesigning in ComponentState) and (fAllowed) then
  begin
    Clear;
  end;
end;

end.
