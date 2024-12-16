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
located at http://jvcl.delphi-jedi.org

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

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, ExtCtrls;

const
  JvScopeDefaultCapacity = 128;
  JvMinimumScopeWidth = 20;
  JvMinimumScopeHeight = 20;


type
  TJvSimScope = class;

  TJvScopeLineUnit = (jluPercent, jluAbsolute);

  TValues = array of Integer;
  
  TJvScopeLineValues = class
  private
    FValues: TValues;
    FCount: Integer;
    FZeroIndex: Integer;

    procedure SetCapacity(const Value: Integer);
    function GetCapacity: Integer;
    function GetItem(Index: Integer): Integer;
  public
    procedure Assign(Source: TJvScopeLineValues);
    procedure Add(Value: Integer);
    procedure Clear;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount;
    property Items[Index: Integer]: Integer read GetItem; default;
  end;

  TJvScopeLine = class(TCollectionItem)
  private
    FPosition: Integer;
    FColor: TColor;
    FName: string;
    FPositionUnit: TJvScopeLineUnit;
    FValues: TJvScopeLineValues;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: Classes.TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property Values: TJvScopeLineValues read FValues;
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
  protected
    function GetOwner: TJvSimScope; reintroduce;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TJvSimScope);
    procedure Assign(Source: TPersistent); override;
    procedure ClearValues;

    function Add: TJvScopeLine;
    function IndexOfName(const AName: string): Integer;
    property Lines[Index: Integer]: TJvScopeLine read GetItem write SetItem; default;
  end;

  TJvSimScopeDisplayUnit = (jduPixels, jduLogical);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
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
    FTotalTimeSteps: Integer;
    FUpdateTimeSteps: Integer;

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
    procedure UpdateComputedValues;
    procedure SetBaseLineUnit(const Value: TJvScopeLineUnit);
    procedure SetTotalTimeSteps(const Value: Integer);
    procedure SetUpdateTimeSteps(const Value: Integer);
  protected
    FCalcBase: Integer;
    FStepPixelWidth: Double;
    FCounter: Double;
    procedure DrawTimerTimer(Sender: TObject);
    function GetLinePixelPosition(Line: TJvScopeLine; Position: Integer): Integer;
    procedure Loaded; override;
  public
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    procedure UpdateScope;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearValues;
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
    property TotalTimeSteps: Integer read FTotalTimeSteps write SetTotalTimeSteps default 208;
    property UpdateTimeSteps: Integer read FUpdateTimeSteps write SetUpdateTimeSteps default 2;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
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

uses
  {$IFDEF RTL330_UP}
  System.Generics.Collections, // for TCollectionNotification items
  {$ENDIF RTL330_UP}
  Math;

//=== { TJvScopeLineValues } =================================================

procedure TJvScopeLineValues.Add(Value: Integer);
begin
  Assert(Assigned(Self));
  if Length(FValues)=Count then // auto-growby JvScopeDefaultCapacity
      SetCapacity( GetCapacity+JvScopeDefaultCapacity);

  if Count < Capacity then
  begin
    FValues[FCount] := Value;
    Inc(FCount);
  end
  else
  begin
    FValues[FZeroIndex] := Value;
    FZeroIndex := (FZeroIndex + 1) mod FCount;
  end;
end;

procedure TJvScopeLineValues.Assign(Source: TJvScopeLineValues);
var
  I: Integer;
begin
  if (not Assigned(Source)) then
      raise Exception.Create('TJvScopeLineValues.Assign:Source not assigned');
  FCount := Source.FCount;
  FZeroIndex := Source.FZeroIndex;
  Capacity := Source.Capacity;
  for I := 0 to Source.Capacity - 1 do
    FValues[I] := Source.FValues[I];
end;

procedure TJvScopeLineValues.Clear;
begin
  FCount := 0;
  FZeroIndex := 0;

  // Always need to have two values in the queue
  Add(0);
  Add(0);
end;

function TJvScopeLineValues.GetCapacity: Integer;
begin
  if Assigned(FValues) then
    Result := Length(FValues)
  else
    Result := 0;
end;

function TJvScopeLineValues.GetItem(Index: Integer): Integer;
begin
  if FCount = 0 then
    Result := FValues[0]
  else
    Result := FValues[(Index + FZeroIndex) mod FCount];
end;

procedure TJvScopeLineValues.SetCapacity(const Value: Integer);
begin
  if Value <> Capacity then
  begin
    SetLength(FValues, Value);
  end;
end;

//=== { TJvScopeLine } =======================================================

procedure TJvScopeLine.Clear;
begin
  FValues.Clear;
end;

constructor TJvScopeLine.Create(Collection: Classes.TCollection);
begin
  // MUST be created before, inherited create will call Notify...
  FValues := TJvScopeLineValues.Create;

  inherited Create(Collection);

  FPosition := 50;
  FColor := clLime;
end;

destructor TJvScopeLine.Destroy;
begin
  FValues.Free;

  inherited Destroy;
end;

procedure TJvScopeLine.Assign(Source: TPersistent);
begin
  if Source is TJvScopeLine then
  begin
    Name := TJvScopeLine(Source).Name;
    Color := TJvScopeLine(Source).Color;
    Position := TJvScopeLine(Source).Position;
    FValues.Assign(TJvScopeLine(Source).FValues);
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

procedure TJvScopeLines.ClearValues;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Lines[I].Clear;
  end;
end;

constructor TJvScopeLines.Create(AOwner: TJvSimScope);
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

function TJvScopeLines.GetOwner: TJvSimScope;
begin
  Result := inherited GetOwner as TJvSimScope;
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

procedure TJvScopeLines.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);

  if Action = cnAdded then
  begin
    TJvScopeLine(Item).FValues.Capacity := GetOwner.TotalTimeSteps;
  end;
end;

procedure TJvScopeLines.SetItem(Index: Integer; const Value: TJvScopeLine);
begin
  inherited Items[Index] := Value;
end;

//=== { TJvSimScope } ========================================================

procedure TJvSimScope.ClearValues;
begin
  FLines.ClearValues;
end;

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
  FDrawTimer.OnTimer := DrawTimerTimer;
  FDrawTimer.Interval := 500;

  FDisplayUnits := jduPixels;
  FUpdateTimeSteps := 2;

  Height := 120;  { property default }
  Width := 208;   { property default }

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
  FDrawTimer.Free;
  FDrawBuffer.Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TJvSimScope.DrawTimerTimer(Sender: TObject);
begin
  UpdateScope;
end;

function TJvSimScope.GetGridSize: Integer;
begin
  Result := -1;
  if HorizontalGridSize = VerticalGridSize then
    Result := HorizontalGridSize;
end;

function TJvSimScope.GetLinePixelPosition(Line: TJvScopeLine;
  Position: Integer): Integer;
begin
  Result := 0;
  case Line.PositionUnit of
    jluPercent:
      Result := Height - Round(Height * Position / 100);
    jluAbsolute:
      Result := Height - Round(Height * (Position - Minimum) / (Maximum - Minimum));
  end;
end;

procedure TJvSimScope.Loaded;
begin
  inherited Loaded;

  // To force having enough values in the scope.
  ClearValues;

  FAllowed := True;
end;

procedure TJvSimScope.Clear;
var
  A: Double;
  I: Integer;
  J: Integer;
  Position: Double;
begin
  if not FAllowed then
    Exit;
  UpdateComputedValues;
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
      MoveTo(Round(A - 1), 0);
      LineTo(Round(A - 1), Height);
      A := A - VerticalGridSize * FStepPixelWidth;
    end;
    { Horizontal lines - below BaseLine }
    A := FCalcBase;
    while A < Height do
    begin
      A := A + HorizontalGridSize * Height / (Maximum - Minimum);
      MoveTo(0, Round(A));
      LineTo(Width, Round(A));
    end;
    { Horizontal lines - above BaseLine }
    A := FCalcBase;
    while A > 0 do
    begin
      A := A - HorizontalGridSize * Height / (Maximum - Minimum);
      MoveTo(0, Round(A));
      LineTo(Width, Round(A));
    end;
    { BaseLine }
    Pen.Color := BaseColor;
    MoveTo(0, FCalcBase);
    LineTo(Width, FCalcBase);

    // Redraw old values to keep history of values
    for I := 0 to FLines.Count - 1 do
    begin
      Pen.Color := FLines[I].Color;

      if FLines[I].FValues.Count > 0 then
      begin
        Position := (TotalTimeSteps - FLines[I].FValues.Count) * FStepPixelWidth;

        MoveTo(Round(Position), GetLinePixelPosition(FLines[I], FLines[I].FValues[0]));
        J := UpdateTimeSteps - 1;
        while J < FLines[I].FValues.Count - 1 do
        begin
          Position := Position + UpdateTimeSteps * FStepPixelWidth;
          LineTo(Round(Position), GetLinePixelPosition(FLines[I], FLines[I].FValues[J]));
          Inc(J, UpdateTimeSteps);
        end;

      end
      else
      begin
        FLines[I].FValues.Clear;
      end;
    end;

    FCounter := 1;
  end;
end;

procedure TJvSimScope.SetBaseLine(Value: Integer);
begin
  FBaseLine := Value;
  UpdateComputedValues;
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
    UpdateComputedValues;
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
    UpdateComputedValues;
    FDrawTimer.Interval := Interval * 10;
    FDrawTimer.Enabled := Value;
    FActive := Value;
  end;
end;

{ All drawings is performed on in the FDrawBuffer to speed up
  proceedings and eliminate flicker. The Paint procedure merely
  copies the contents of the FDrawBuffer. }

procedure TJvSimScope.UpdateScope;
var
  A: Double;
  I: Integer;
  Dest, Src: TRect;
  UpdateWidth: Integer;
  J: Integer;
  PosMinusOne: Double;
  PosMinusTwo: Double;
begin
  with FDrawBuffer.Canvas do
  begin
    Pen.Color := FGridColor;

    UpdateWidth := Round(UpdateTimeSteps * FStepPixelWidth);

    Dest.Top := 0;
    Dest.Left := 0;
    Dest.Right := Round(Width - UpdateWidth);
    Dest.Bottom := Height;

    Src.Top := 0;
    Src.Left := Round(UpdateTimeSteps * FStepPixelWidth);
    Src.Right := Width;
    Src.Bottom := Height;
    { Copy bitmap leftwards }
    CopyRect(Dest, FDrawBuffer.Canvas, Src);

    { Draw new area }
    Pen.Color := Color;
    Brush.Color := Color;
    BRush.Style := bsSolid;
    Dest.Top := 0;
    Dest.Left := Width - UpdateWidth;
    Dest.Right := Width;
    Dest.Bottom := Height;
    FilLRect(Dest);
(*    Pen.Width := UpdateWidth;
    MoveTo(Width - Round(UpdateWidth / 2), 0);
    LineTo(Width - Round(UpdateWidth / 2), Height);   *)


    Pen.Color := GridColor;
    Pen.Width := 1;
    { Draw vertical line if needed }
    if FCounter >= Round(VerticalGridSize * FStepPixelWidth / UpdateWidth) then
    begin
      MoveTo(Width - 1, 0);
      LineTo(Width - 1, Height);
      FCounter := 0;
    end;
    FCounter := FCounter + 1;
    { Horizontal lines - below BaseLine }
    A := FCalcBase;
    while A < Height do
    begin
      A := A + HorizontalGridSize * Height / (Maximum - Minimum);
      MoveTo(Width - UpdateWidth, Round(A));
      LineTo(Width, Round(A));
    end;
    { Horizontal lines - above BaseLine }
    A := FCalcBase;
    while A > 0 do
    begin
      A := A - HorizontalGridSize * Height / (Maximum - Minimum);
      MoveTo(Width - UpdateWidth, Round(A));
      LineTo(Width, Round(A));
    end;
    { BaseLine }
    Pen.Color := BaseColor;
    MoveTo(Width - UpdateWidth, FCalcBase);
    LineTo(Width, FCalcBase);
    { Draw position for lines}
    for I := 0 to FLines.Count - 1 do
    begin
      Pen.Color := FLines[I].Color;

      A := GetLinePixelPosition(FLines[I], FLines[I].Position);
      PosMinusOne := GetLinePixelPosition(FLines[I], FLines[I].FValues[FLines[I].FValues.Count - 1 * UpdateTimeSteps]);
      PosMinusTwo := GetLinePixelPosition(FLines[I], FLines[I].FValues[FLines[I].FValues.Count - 2 * UpdateTimeSteps]);

      MoveTo(Width - UpdateWidth * 2, Round(PosMinusTwo));
      LineTo(Width - UpdateWidth, Round(PosMinusOne));
      LineTo(Width - 0, Round(A));
      for J := 0 to UpdateTimeSteps - 1 do
        FLines[I].FValues.Add(FLines[I].Position);
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
  Canvas.CopyRect(Rect, FDrawBuffer.Canvas, Rect);
  FAllowed := True;
end;

{ Recalulate control after move and/or resize }

procedure TJvSimScope.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  { BUGFIX/Workaround:JAN 2009 - ACCESS VIOLATIONS AND ODD BEHAVIOUR - SIZE/WIDTH BEING ZAPPED TO ZERO.}
  if AWidth < JvMinimumScopeWidth then
      AWidth := JvMinimumScopeWidth;
  if AHeight < JvMinimumScopeHeight then
      AHeight := JvMinimumScopeHeight;


  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  FDrawBuffer.Height := Height;
  FDrawBuffer.Width := Width;
  if DisplayUnits = jduPixels then
  begin
    FMinimum := 0;
    FMaximum := AHeight;
    FTotalTimeSteps := AWidth;
  end;
  Clear;
end;

procedure TJvSimScope.UpdateComputedValues;
begin
  case FBaseLineUnit of
    jluPercent:
      FCalcBase := Height - Round(Height * FBaseLine / 100);
    jluAbsolute:
      FCalcBase := Height - Round(Height * (FBaseLine - Minimum) / (Maximum - Minimum));
  end;
  FStepPixelWidth := Width / TotalTimeSteps;
  if FUpdateTimeSteps * FStepPixelWidth < 2 then
    UpdateTimeSteps := 2;
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
  if (FDisplayUnits <> jduPixels) and (FMaximum <> Value) then
  begin
    FMaximum := Value;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.SetMinimum(const Value: Integer);
begin
  if (FDisplayUnits <> jduPixels) and (FMinimum <> Value) then
  begin
    FMinimum := Value;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.SetTotalTimeSteps(const Value: Integer);
begin
  if (FDisplayUnits <> jduPixels) and (FTotalTimeSteps <> Value) then
  begin
    FTotalTimeSteps := Value;
    UpdateDisplay(True);
  end;
end;

procedure TJvSimScope.SetUpdateTimeSteps(const Value: Integer);
begin
  if (FUpdateTimeSteps <> Value) and (Value > 0) then
  begin
    FUpdateTimeSteps := Value;
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
