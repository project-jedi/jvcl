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
  TJvScopeLine = class(TCollectionItem)
  private
    FPosition, FOldPos, FPrevPos: integer;
    FColor: TColor;
    FName: string;
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name:string read FName write FName;
    property Color:TColor read FColor write FColor default clLime;
    property Position:integer read FPosition write FPosition default 50;
  end;

  TJvScopeLines = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TJvScopeLine;
    procedure SetItem(Index: integer; const Value: TJvScopeLine);
  public
    constructor Create(AOwner:TPersistent);
    procedure Assign(Source: TPersistent); override;

    function Add:TJvScopeLine;
    function IndexOfName(const AName:string):integer;
    property Items[Index:integer]:TJvScopeLine read GetItem write SetItem;default;
  end;

  TJvSimScope = class(TGraphicControl)
  private
    fAllowed: boolean;
    fOnUpdate: TNotifyEvent;
    DrawBuffer: TBitmap;
    DrawTimer: TTimer;
    fActive: boolean;
    fBaseColor,
    fGridColor : TColor;
    fBaseLine, fGridSize, fInterval: integer;
    FLines: TJvScopeLines;
    procedure SetActive(value: boolean);
    procedure SetGridSize(value: integer);
    procedure SetBaseLine(value: integer);
    procedure SetInterval(value: integer);
    procedure SetLines(const Value: TJvScopeLines);
    procedure UpdateDisplay(ClearFirst:boolean);

  protected
    CalcBase, Counter: integer;
    procedure UpdateScope(Sender: TObject);
    procedure Loaded; override;
  public
    procedure Paint; override;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property BaseLine: integer read fBaseline write SetBaseLine;
    property GridSize: integer read fGridSize write SetGridSize;
    property Active: boolean read fActive write SetActive;
    property Interval: Integer read fInterval write SetInterval;
    { Color properties }
    property Color;
    property GridColor: TColor read fGridColor write fGridColor;
    property Lines:TJvScopeLines read FLines write SetLines;
    property BaseColor: TColor read fBaseColor write fBaseColor;

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

{ --- TJvScopeLine --------------------------------------------------------------- }

procedure TJvScopeLine.Assign(Source: TPersistent);
begin
  if Source is TJvScopeLine then
  begin
    Name := TJvScopeLine(Source).Name;
    Color := TJvScopeLine(Source).Color;
    Position := TJvScopeLine(Source).Position;
    Exit;
  end;
  inherited;
end;

constructor TJvScopeLine.Create(Collection: TCollection);
begin
  inherited;
  FPosition := 50;
  FColor := clLime;
end;

function TJvScopeLine.GetDisplayName: String;
begin
  if Name = '' then
    Result := inherited GetDisplayName
  else
    Result := Name;
end;

{ --- TJvScopeLines -------------------------------------------------------------- }

function TJvScopeLines.Add: TJvScopeLine;
begin
  Result := TJvScopeLine(inherited Add);
end;

procedure TJvScopeLines.Assign(Source: TPersistent);
var i:integer;
begin
  if Source is TJvScopeLines then
  begin
    Clear;
    for i := 0 to TJvScopeLines(Source).Count - 1 do
      Add.Assign(TJvScopeLines(Source)[i]);
    Exit;
  end;
  inherited;
end;

constructor TJvScopeLines.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvScopeLine);
end;

function TJvScopeLines.GetItem(Index: integer): TJvScopeLine;
begin
  Result := TJvScopeLine(inherited Items[Index])
end;

function TJvScopeLines.IndexOfName(const AName: string): integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiSameStr(Items[Result].Name, AName) then Exit;
  Result := -1;
end;

procedure TJvScopeLines.SetItem(Index: integer; const Value: TJvScopeLine);
begin
  inherited Items[Index] := Value;
end;

{ --- TJvSimScope ----------------------------------------------------------------- }

constructor TJvSimScope.Create(AnOwner: TComponent);
{ Create control and background draw buffer and timer
}
begin
  inherited Create(AnOwner);
  fAllowed := FALSE;
  DrawBuffer := TBitmap.Create;
  DrawBuffer.Canvas.Brush.Color := Color;
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
  BaseColor := clRed;

  BaseLine := 50;
  GridSize := 16;

  FLines := TJvScopeLines.Create(self);
  Interval := 50;
  Counter := 1;

  ControlStyle := [csFramed, csOpaque];
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
  a, i: integer;
begin
  if not fAllowed then Exit;
  CalcBase := (height - round(height / 100 * FBaseline));
  with DrawBuffer.Canvas do
  begin
    Brush.Color := Color;
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
    for i := 0 to FLines.Count - 1 do
    begin
      FLines[i].FOldPos := CalcBase;
      FLines[i].FPrevPos := CalcBase;
    end;
    {
    // Draws a line from 0,baseline to width, new pos
    Pen.Color:=FLineColor;
    MoveTo(0,height);
    LineTo(Width,height-round(height/100*position));
    }
    counter := 1;
  end;
end;


destructor TJvSimScope.Destroy;
begin
  DrawTimer.Free;
  DrawBuffer.Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TJvSimScope.SetBaseLine(value: integer);
{ Set base-linje value
}
begin
  fBaseLine := value;
  CalcBase := (height - round(height / 100 * FBaseline));
  UpdateDisplay(true);
end;

procedure TJvSimScope.SetInterval(Value: integer);
{ Set Scroll delay
}
begin
  DrawTimer.Enabled := FALSE;
  CalcBase := (Height - round(Height / 100 * FBaseline));
  DrawTimer.Interval := Value * 10;
  fInterval := Value;
  DrawTimer.Enabled := FActive;
end;

procedure TJvSimScope.SetGridSize(value: integer);
{ Set grid size }
begin
  Value := (Value div 2) * 2;
  if fGridSize <> Value then
  begin
    fGridSize := Value;
    UpdateDisplay(true);
  end;
end;

procedure TJvSimScope.SetActive(Value: boolean);
{ Start scrolling
}
begin
  CalcBase := (Height - round(Height / 100 * FBaseline));
  DrawTimer.Interval := Interval * 10;
  DrawTimer.Enabled := Value;
  fActive := Value;
end;

procedure TJvSimScope.UpdateScope(Sender: TObject);
{ All drawings is performed on in the drawbuffer to speed up
  proceedings and eliminate flicker. The Paint procedure merely
  copies the contents of the drawbuffer.
}
var
  a, i: integer;
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
    Pen.Color := Color;
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
    { Draw position for lines}
    for i := 0 to FLines.Count - 1 do
    begin
      Pen.Color := FLines[i].Color;
      a := height - round(height / 100 * FLines[i].Position);
      MoveTo(Width - 4, FLines[i].FOldPos);
      LineTo(Width - 2, FLines[i].FPrevPos);
      LineTo(Width - 0, a);
      FLines[i].FOldPos := FLines[i].fPrevPos;
      FLines[i].FPrevPos := a;
    end;
  end;
  Paint;
  if Assigned(FOnUpdate) then FOnUpdate(self);
end;

procedure TJvSimScope.Paint;
{ Called by timer to show updates
}
var
  Rect: TRect;
begin
//  inherited Paint;
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
  Clear;
end;

procedure TJvSimScope.SetLines(const Value: TJvScopeLines);
begin
  FLines.Assign(Value);
  Clear;
end;

procedure TJvSimScope.UpdateDisplay(ClearFirst: boolean);
begin
  if Parent = nil then Exit;
  if ClearFirst then Clear;
  Paint;
end;

end.
