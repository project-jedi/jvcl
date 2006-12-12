{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimScope.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  TJvSimScope Properties:
     Active              Starts/Stops scope
     Color               Backgroundcolor
     GridColor           Grid mask color
     HorizontalGridSize  Size of horiontal grid mask in logical units
     VerticalGridSize    Size of vertical grid mask in logical units
     Interval            Scroll speed in 1/100's seconds
     LineColor           Scope dataline color
     Position            Dataline value
     BaseColor           Color of BaseLine
     BaseLine            BaseLine value 

  TJvSimScope Methods:
     Clear            Clears the control and redraws grid

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSimScope;

interface

{$I jvcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Graphics, Controls, Forms, ExtCtrls,
  SysUtils, Classes;

type
  TJvScopeLineUnit = (jluPercent, jluAbsolute);

  TJvScopeLine = class(TCollectionItem)
  private
    FPosition: Integer;
    FOldPos: Integer;
    FPrevPos: Integer;
    FColor: TColor;
    FName: string;
    FPositionUnit: TJvScopeLineUnit;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write FName;
    property Color: TColor read FColor write FColor default clLime;
    property Position: Integer read FPosition write FPosition default 50;
    property PositionUnit: TJvScopeLineUnit read FPositionUnit write FPositionUnit default jluPercent;
  end;

  TJvScopeLines = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvScopeLine;
    procedure SetItem(Index: Integer; const Value: TJvScopeLine);
  public
    constructor Create(AOwner: TPersistent);
    procedure Assign(Source: TPersistent); override;

    function Add: TJvScopeLine;
    function IndexOfName(const AName: string): Integer;
    property Lines[Index: Integer]: TJvScopeLine read GetItem write SetItem; default;
  end;

  TJvSimScopeDisplayUnit = (jduPixels, jduLogical);

  TJvSimScope = class(TGraphicControl)
  private
    FAllowed: Boolean;
    FOnUpdate: TNotifyEvent;
    FDrawBuffer: TBitmap;
    FDrawTimer: TTimer;
    FActive: Boolean;
    FBaseColor: TColor;
    FGridColor: TColor;
    FBaseLine: Integer;
    FInterval: Integer;
    FLines: TJvScopeLines;
    FHorizontalGridSize: Integer;
    FVerticalGridSize: Integer;
    FDisplayUnits: TJvSimScopeDisplayUnit;
    FMaximum: Integer;
    FMinimum: Integer;
    FBaseLineUnit: TJvScopeLineUnit;
    
    procedure SetActive(Value: Boolean);
    procedure SetGridSize(Value: Integer);
    procedure SetBaseLine(Value: Integer);
    procedure SetInterval(Value: Integer);
    procedure SetLines(const Value: TJvScopeLines);
    procedure UpdateDisplay(ClearFirst: Boolean);
    procedure SetHorizontalGridSize(const Value: Integer);
    procedure SetVerticalGridSize(const Value: Integer);
    function GetGridSize: Integer;
    procedure SetDisplayUnits(const Value: TJvSimScopeDisplayUnit);
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure UpdateCalcBase;
    procedure SetBaseLineUnit(const Value: TJvScopeLineUnit);
  protected
    FCalcBase: Integer;
    FCounter: Integer;
    procedure UpdateScope(Sender: TObject);
    procedure Loaded; override;
  public
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Active: Boolean read FActive write SetActive;
    property BaseColor: TColor read FBaseColor write FBaseColor default clRed;
    property BaseLine: Integer read FBaseLine write SetBaseLine default 50;
    property BaseLineUnit: TJvScopeLineUnit read FBaseLineUnit write SetBaseLineUnit default jluPercent;
    property Color default clBlack;
    property DisplayUnits: TJvSimScopeDisplayUnit read FDisplayUnits write SetDisplayUnits default jduPixels;
    property GridColor: TColor read FGridColor write FGridColor default clGreen;
    property GridSize: Integer read GetGridSize write SetGridSize stored False default 16;
    property HorizontalGridSize: Integer read FHorizontalGridSize write SetHorizontalGridSize default 16;
    property Height default 120;
    property Interval: Integer read FInterval write SetInterval default 50;
    property Lines: TJvScopeLines read FLines write SetLines;
    property Minimum: Integer read FMinimum write SetMinimum;
    property Maximum: Integer read FMaximum write SetMaximum default 120;
    property VerticalGridSize: Integer read FVerticalGridSize write SetVerticalGridSize default 16;
    property Width default 208;

    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;

    property Align;
    property Anchors;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    {$IFDEF COMPILER10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF COMPILER10_UP}
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF COMPILER6_UP}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF COMPILER6_UP}
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation


//=== { TJvScopeLine } =======================================================

constructor TJvScopeLine.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPosition := 50;
  FColor := clLime;
end;

procedure TJvScopeLine.Assign(Source: TPersistent);
begin
  if Source is TJvScopeLine then
  begin
    Name := TJvScopeLine(Source).Name;
    Color := TJvScopeLine(Source).Color;
    Position := TJvScopeLine(Source).Position;
  end
  else
    inherited Assign(Source);
end;

function TJvScopeLine.GetDisplayName: string;
begin
  if Name = '' then
    Result := inherited GetDisplayName
  else
    Result := Name;
end;

//=== { TJvScopeLines } ======================================================

constructor TJvScopeLines.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvScopeLine);
end;

function TJvScopeLines.Add: TJvScopeLine;
begin
  Result := TJvScopeLine(inherited Add);
end;

procedure TJvScopeLines.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvScopeLines then
  begin
    Clear;
    for I := 0 to TJvScopeLines(Source).Count - 1 do
      Add.Assign(TJvScopeLines(Source)[I]);
  end
  else
    inherited Assign(Source);
end;

function TJvScopeLines.GetItem(Index: Integer): TJvScopeLine;
begin
  Result := TJvScopeLine(inherited Items[Index]);
end;

function TJvScopeLines.IndexOfName(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if AnsiSameStr(Lines[Result].Name, AName) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TJvScopeLines.SetItem(Index: Integer; const Value: TJvScopeLine);
begin
  inherited Items[Index] := Value;
end;

//=== { TJvSimScope } ========================================================

constructor TJvSimScope.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowed := False;
  FDrawBuffer := TBitmap.Create;
  FDrawBuffer.Canvas.Brush.Style := bsSolid;
  FDrawBuffer.Canvas.Pen.Width := 1;
  FDrawBuffer.Canvas.Pen.Style := psSolid;

  FDrawTimer := TTimer.Create(Self);
  FDrawTimer.Enabled := False;
  FDrawTimer.OnTimer := UpdateScope;
  FDrawTimer.Interval := 500;

  FDisplayUnits := jduPixels;

  Height := 120;
  Width := 208;

  Color := clBlack;
  FGridColor := clGreen;
  FBaseColor := clRed;

  BaseLine := 50;
  GridSize := 16;

  FLines := TJvScopeLines.Create(Self);
  Interval := 50;
  FCounter := 1;

  ControlStyle := [csFramed, csOpaque];
  FAllowed := True;
end;

destructor TJvSimScope.Destroy;
begin
  FDrawBuffer.Free;
  FLines.Free;
  inherited Destroy;
end;

function TJvSimScope.GetGridSize: Integer;
begin
  Result := -1;
  if HorizontalGridSize = VerticalGridSize then
    Result := HorizontalGridSize;
end;

procedure TJvSimScope.Loaded;
begin
  inherited Loaded;
  FAllowed := True;
end;

procedure TJvSimScope.Clear;
var
  A, I: Integer;
begin
  if not FAllowed then
    Exit;
  UpdateCalcBase;
  with FDrawBuffer.Canvas do
  begin
    Brush.Color := Color;
    Pen.Style := psClear;
    Rectangle(0, 0, Width + 1, Height + 1);
    Pen.Style := psSolid;
    Pen.Color := GridColor;
    Pen.Width := 1;
    { Vertical lines }
    A := Width;
    while A > 0 do
    begin
      MoveTo(A - 1, 0);
      LineTo(A - 1, Height);
      Dec(A, VerticalGridSize);
    end;
    { Horizontal lines - above BaseLine }
    A := FCalcBase;
    while A < Height do
    begin
      Inc(A, Round(HorizontalGridSize * Height / (Maximum - Minimum)));
      MoveTo(0, A);
      LineTo(Width, A);
    end;
    { Horizontal lines - below BaseLine }
    A := FCalcBase;
    while A > 0 do
    begin
      Dec(A, Round(HorizontalGridSize * Height / (Maximum - Minimum)));
      MoveTo(0, A);
      LineTo(Width, A);
    end;
    { BaseLine }
    Pen.Color := BaseColor;
    MoveTo(0, FCalcBase);
    LineTo(Width, FCalcBase);

    { Start new position-line on BaseLine... }
    for I := 0 to FLines.Count - 1 do
    begin
      FLines[I].FOldPos := FCalcBase;
      FLines[I].FPrevPos := FCalcBase;
    end;
    {
    // Draws a line from 0,BaseLine to width, new pos
    Pen.Color:=FLineColor;
    MoveTo(0,Height);
    LineTo(Width,Height-Round(Height/100*position));
    }
    FCounter := 1;
  end;
end;

procedure TJvSimScope.SetBaseLine(Value: Integer);
begin
  FBaseLine := Value;
  UpdateCalcBase;
  UpdateDisplay(True);
end;

procedure TJvSimScope.SetBaseLineUnit(const Value: TJvScopeLineUnit);
begin
  if FBaseLineUnit <> Value then
  begin
    FBaseLineUnit := Value;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.SetInterval(Value: Integer);
begin
  if FInterval <> Value then
  begin
    FDrawTimer.Enabled := False;
    UpdateCalcBase;
    FDrawTimer.Interval := Value * 10;
    FInterval := Value;
    FDrawTimer.Enabled := FActive;
  end;
end;

procedure TJvSimScope.SetGridSize(Value: Integer);
begin
  if ((Value <> FHorizontalGridSize) or (Value <> FVerticalGridSize)) and (Value > 0) then
  begin
    FHorizontalGridSize := Value;
    FVerticalGridSize := Value;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.SetHorizontalGridSize(const Value: Integer);
begin
  if (FHorizontalGridSize <> Value) and (Value > 0) then
  begin
    FHorizontalGridSize := Value;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    UpdateCalcBase;
    FDrawTimer.Interval := Interval * 10;
    FDrawTimer.Enabled := Value;
    FActive := Value;
  end;
end;

{ All drawings is performed on in the FDrawBuffer to speed up
  proceedings and eliminate flicker. The Paint procedure merely
  copies the contents of the FDrawBuffer. }

procedure TJvSimScope.UpdateScope(Sender: TObject);
var
  A, I: Integer;
  Dest, Src: TRect;
begin
  with FDrawBuffer.Canvas do
  begin
    Pen.Color := FGridColor;

    Dest.Top := 0;
    Dest.Left := 0;
    Dest.Right := Width - 2;
    Dest.Bottom := Height;

    Src.Top := 0;
    Src.Left := 2;
    Src.Right := Width;
    Src.Bottom := Height;
    { Copy bitmap leftwards }
    CopyRect(Dest, FDrawBuffer.Canvas, Src);

    { Draw new area }
    Pen.Color := Color;
    Pen.Width := 2;
    MoveTo(Width - 1, 0);
    LineTo(Width - 1, Height);
    Pen.Color := GridColor;
    Pen.Width := 1;
    { Draw vertical line if needed }
    if FCounter = (VerticalGridSize div 2) then
    begin
      MoveTo(Width - 1, 0);
      LineTo(Width - 1, Height);
      FCounter := 0;
    end;
    Inc(FCounter);
    { Horizontal lines - above BaseLine }
    A := FCalcBase;
    while A < Height do
    begin
      Inc(A, Round(HorizontalGridSize * Height / (Maximum - Minimum)));
      MoveTo(Width - 2, A);
      LineTo(Width, A);
    end;
    { Horizontal lines - below BaseLine }
    A := FCalcBase;
    while A > 0 do
    begin
      Dec(A, Round(HorizontalGridSize * Height / (Maximum - Minimum)));
      MoveTo(Width - 2, A);
      LineTo(Width, A);
    end;
    { BaseLine }
    Pen.Color := BaseColor;
    MoveTo(Width - 2, FCalcBase);
    LineTo(Width, FCalcBase);
    { Draw position for lines}
    for I := 0 to FLines.Count - 1 do
    begin
      Pen.Color := FLines[I].Color;
      case FLines[I].PositionUnit of
        jluPercent:
          A := Height - Round(Height * FLines[I].Position / 100);
        jluAbsolute:
          A := Height - Round(Height * FLines[I].Position / (Maximum - Minimum));
      end;
      MoveTo(Width - 4, FLines[I].FOldPos);
      LineTo(Width - 2, FLines[I].FPrevPos);
      LineTo(Width - 0, A);
      FLines[I].FOldPos := FLines[I].FPrevPos;
      FLines[I].FPrevPos := A;
    end;
  end;
  Repaint;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

{ Called by timer to show updates }

procedure TJvSimScope.Paint;
var
  Rect: TRect;
begin
  //  inherited Paint;
  FDrawBuffer.Height := Height;
  FDrawBuffer.Width := Width;
  Rect.Top := 0;
  Rect.Left := 0;
  Rect.Right := Width;
  Rect.Bottom := Height;
  {$IFDEF VCL}
  Canvas.CopyRect(Rect, FDrawBuffer.Canvas, Rect);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Canvas.CopyRect(Bounds(Left, Top, Width, Height), FDrawBuffer.Canvas, Rect);
  {$ENDIF VisualCLX}
  FAllowed := True;
end;

{ Recalulate control after move and/or resize }

procedure TJvSimScope.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  FDrawBuffer.Height := Height;
  FDrawBuffer.Width := Width;
  if DisplayUnits = jduPixels then
  begin
    FMinimum := 0;
    FMaximum := AHeight;
  end;
  Clear;
end;

procedure TJvSimScope.UpdateCalcBase;
begin
  case FBaseLineUnit of
    jluPercent:
      FCalcBase := (Height - Round(Height * FBaseLine / 100));
    jluAbsolute:
      FCalcBase := (Height - Round(Height * FBaseLine / (Maximum - Minimum)));
  end;
end;

procedure TJvSimScope.SetDisplayUnits(const Value: TJvSimScopeDisplayUnit);
begin
  if FDisplayUnits <> Value then
  begin
    FDisplayUnits := Value;
    if FDisplayUnits = jduPixels then
    begin
      FMinimum := 0;
      FMaximum := Height;
    end;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.SetLines(const Value: TJvScopeLines);
begin
  FLines.Assign(Value);
  Clear;
end;

procedure TJvSimScope.SetMaximum(const Value: Integer);
begin
  if (DisplayUnits <> jduPixels) and (FMaximum <> Value) then
  begin
    FMaximum := Value;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.SetMinimum(const Value: Integer);
begin
  if (DisplayUnits <> jduPixels) and (FMinimum <> Value) then
  begin
    FMinimum := Value;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.SetVerticalGridSize(const Value: Integer);
begin
  if (FVerticalGridSize <> Value) and (Value > 0) then
  begin
    FVerticalGridSize := Value;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.UpdateDisplay(ClearFirst: Boolean);
begin
  if Parent <> nil then
  begin
    if ClearFirst then
      Clear;
    Repaint;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

