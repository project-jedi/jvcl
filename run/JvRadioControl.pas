{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRadioGroup.PAS, released on 2002-07-16.

The Initial Developer of the Original Code is Rudolph Velthuis
Portions created by Rudolph Velthuis are Copyright (C) 1997 drs. Rudolph Velthuis.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-16

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}


unit JvRadioControl;

{ TJvRadioControl component, a button like the dial on a radio. }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, ComCtrls;

type
  TJvRadioPointerShape = (psLine, psTriangle, psDot, psOwnerDraw);
  TJvTickLength = (tlShort, tlMiddle, tlLong);
  TJvRadioAngle = 0..3600; // 0.0 - 360.0 deg
  TJvRepeatValue = 10..1000; // mouse repeat values
  TJvCustomRadioControl = class;
  TJvRadioDrawEvent = procedure(Sender: TJvCustomRadioControl; ARect: TRect) of object;

  PTick = ^TTick;
  TTick = record
    Value: Integer;
    Length: Integer;
    Color: TColor;
    Changed: Boolean;
  end;

  TJvCustomRadioControl = class(TCustomControl)
  private
    FBitmap: TBitmap;
    FBitmapRect: TRect;
    FBitmapInvalid: Boolean;
    FBorderStyle: TBorderStyle;
    FButtonEdge: Integer;
    FDefaultPos: Integer;
    FFrequency: Integer;
    FLargeChange: Integer;
    FMax: Integer;
    FMaxAngle: TJvRadioAngle;
    FMin: Integer;
    FMinAngle: TJvRadioAngle;
    FPointerRect: TRect;
    FPointerColor: TColor;
    FPointerSize: Integer;
    FPointerShape: TJvRadioPointerShape;
    FPosition: Integer;
    FRadius: Integer;
    FSize: Integer;
    FSmallChange: Integer;
    FTicks: TList;
    FTickStyle: TTickStyle;
    FIncrementing: Boolean;
    FRepeatTimer: TTimer;
    FRepeatRate: TJvRepeatValue;
    FRepeatDelay: TJvRepeatValue;
    FOnChange: TNotifyEvent;
    FOnDrawPointer: TJvRadioDrawEvent;
    function CalcBounds(var AWidth, AHeight: Integer): Boolean;
    function GetAngle: TJvRadioAngle;
    function GetCenter: TPoint;
    procedure SetAngle(Value: TJvRadioAngle);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetButtonEdge(Value: Integer);
    procedure SetDefaultPos(Value: Integer);
    procedure SetFrequency(Value: Integer);
    procedure SetLargeChange(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMinAngle(Value: TJvRadioAngle);
    procedure SetMax(Value: Integer);
    procedure SetMaxAngle(Value: TJvRadioAngle);
    procedure SetPointerColor(Value: TColor);
    procedure SetPointerSize(Value: Integer);
    procedure SetPointerShape(Value: TJvRadioPointerShape);
    procedure SetPosition(Value: Integer);
    procedure SetRadius(Value: Integer);
    procedure SetSmallChange(Value: Integer);
    procedure SetTickStyle(Value: TTickStyle);
    procedure UpdateSize;
    procedure TimerExpired(Sender: TObject);
  protected
    function AngleToPos(AnAngle: TJvRadioAngle): Integer;
    procedure BitmapNeeded; dynamic;
    procedure Change; dynamic;
    procedure ClearTicks;
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBorder; dynamic;
    procedure DrawButton; dynamic;
    procedure DrawPointer; dynamic;
    procedure DrawTick(ACanvas: TCanvas; var T: TTick); dynamic;
    procedure DrawTicks; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function PosToAngle(Pos: Integer): TJvRadioAngle;
    procedure SetTicks(Value: TTickStyle); virtual;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSysColorChange(var Msg: TMessage); message WM_SYSCOLORCHANGE;
    procedure WndProc(var Msg: TMessage); override;
    procedure IncPos(Shift: TShiftState); dynamic;
    procedure DecPos(Shift: TShiftState); dynamic;
    property Ticks: TList read FTicks write FTicks stored True;
    // to be published later:
    property Angle: TJvRadioAngle read GetAngle write SetAngle stored False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property ButtonEdge: Integer read FButtonEdge write SetButtonEdge default 2;
    property DefaultPos: Integer read FDefaultPos write SetDefaultPos;
    property Frequency: Integer read FFrequency write SetFrequency default 10;
    property LargeChange: Integer read FLargeChange write SetLargeChange default 2;
    property Max: Integer read FMax write SetMax default 100;
    property MaxAngle: TJvRadioAngle read FMaxAngle write SetMaxAngle default 3300;
    property Min: Integer read FMin write SetMin default 0;
    property MinAngle: TJvRadioAngle read FMinAngle write SetMinAngle default 300;
    property PointerColor: TColor read FPointerColor write SetPointerColor default clBtnText;
    property PointerSize: Integer read FPointerSize write SetPointerSize default 33;
    property PointerShape: TJvRadioPointerShape read FPointerShape write SetPointerShape default psLine;
    property Position: Integer read FPosition write SetPosition default 0;
    property Radius: Integer read FRadius write SetRadius;
    property RepeatDelay: TJvRepeatValue read FRepeatDelay write FRepeatDelay default 400;
    property RepeatRate: TJvRepeatValue read FRepeatRate write FRepeatRate default 100;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property TickStyle: TTickStyle read FTickStyle write SetTickStyle stored True;
    property TabStop default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawPointer: TJvRadioDrawEvent read FOnDrawPointer write FOnDrawPointer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AngleToPoint(AnAngle: TJvRadioAngle; ACenter: TPoint; ARadius: Integer): TPoint;
    procedure SetAngleParams(AnAngle, AMin, AMax: TJvRadioAngle); virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetParams(APosition, AMin, AMax: Integer); virtual;
    procedure SetTick(Value: Integer; Length: TJvTickLength); virtual;
    function RadToAngle(const Radian: Double): TJvRadioAngle;
    function AngleToRad(AnAngle: TJvRadioAngle): Double;
    property Bitmap: TBitmap read FBitmap;
    property Center: TPoint read GetCenter;
  end;

  TJvRadioControl = class(TJvCustomRadioControl)
  published
    // properties
    property Align;
    property Angle;
    property BorderStyle;
    property ButtonEdge;
    property Color;
    property Ctl3D;
    property Cursor;
    property DefaultPos;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Frequency;
    property LargeChange;
    property Max;
    property MaxAngle;
    property Min;
    property MinAngle;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PointerColor;
    property PointerSize;
    property PointerShape;
    property PopupMenu;
    property Position;
    property Radius;
    property RepeatDelay;
    property RepeatRate;
    property ShowHint;
    property SmallChange;
    property TickStyle;
    property TabOrder;
    property TabStop;
    property Visible;
    // events
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawPointer;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


implementation

uses
  Consts, Math;

const
  dAngleToRadian = Pi / 1800;
  dRadianToAngle = 1800 / Pi;
  rcMaxEdge = 100;
  rcMinEdge = 0;
  rcMinRadius = 15;
  tlLongLen = 10;
  tlMiddleLen = 6;
  tlShortLen = 4;

  MinBorder = 1;
  TickBorder = tlLongLen;

function GetShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
end;

constructor TJvCustomRadioControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents, csCaptureMouse];
  FTicks := TList.Create;
  FBorderStyle := bsNone;
  FButtonEdge := 5;
  FDefaultPos := 0;
  FFrequency := 10;
  FLargeChange := 2;
  FMax := 100;
  FMaxAngle := 3300;
  FMin := 0;
  FMinAngle := 300;
  FPointerColor := clBtnText;
  FPointerSize := 33;
  FRadius := rcMinRadius;
  FSmallChange := 1;
  TabStop := True;
  FTickStyle := tsAuto;
  FBitmapInvalid := True;
  FPointerRect.Left := -1; // Only on start up
  Width := 51;
  Height := 51;
  FRepeatDelay := 400;
  FRepeatRate := 100;
  SetTicks(FTickStyle);
  Position := 0;
end;

destructor TJvCustomRadioControl.Destroy;
begin
  FBitmap.Free;
  ClearTicks;
  FTicks.Free;
  FRepeatTimer.Free;
  inherited Destroy;
end;

// Convert position Pos to an angle.

function TJvCustomRadioControl.PosToAngle(Pos: Integer): TJvRadioAngle;
begin
  Result := FMinAngle + ((FMaxAngle - FMinAngle) * (Pos - FMin) div (FMax - FMin));
end;

// Convert angle AnAngle to a position.

function TJvCustomRadioControl.AngleToPos(AnAngle: TJvRadioAngle): Integer;
begin
  Result := FMin + ((FMax - FMin) * (AnAngle - FMinAngle) div (FMaxAngle - FMinAngle));
end;

// Convert polar coordinates defined by AnAngle, ACenter and ARadius to a TPoint.

function TJvCustomRadioControl.AngleToPoint(AnAngle: TJvRadioAngle; ACenter: TPoint;
  ARadius: Integer): TPoint;
var
  RadAngle: Double;
begin
  RadAngle := AngleToRad(AnAngle);
  Result.X := ACenter.X - Round(ARadius * Sin(RadAngle));
  Result.Y := ACenter.Y + Round(ARadius * Cos(RadAngle));
end;

// Convert a APoint to an angle (relative to ACenter) in radians, where
// bottom is 0, left is Pi/2, top is Pi and so on.

function PointToRad(const APoint, ACenter: TPoint): Double;
var
  N: Integer;
begin
  N := APoint.X - ACenter.X;
  if N = 0 then
    Result := 0.5 * Pi
  else
    Result := ArcTan((ACenter.Y - APoint.Y) / N);
  if N < 0 then
    Result := Result + Pi;
  Result := 1.5 * Pi - Result;
end;

// Get current angle (from position).

function TJvCustomRadioControl.GetAngle: TJvRadioAngle;
begin
  Result := PosToAngle(FPosition);
end;

// Set current angle. Sets Position.

procedure TJvCustomRadioControl.SetAngle(Value: TJvRadioAngle);
begin
  SetAngleParams(Value, FMinAngle, FMaxAngle);
end;

// Set border style. Redraw if necessary.

procedure TJvCustomRadioControl.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    if HandleAllocated then
    begin
      RecreateWnd;
      DrawBorder;
    end;
  end;
end;

// Set positional (Cartesian) parameters, value checked and invalidate if
// necessary.

procedure TJvCustomRadioControl.SetParams(APosition, AMin, AMax: Integer);
var
  Invalid: Boolean;
  Changed: Boolean;
begin
  Changed := False;

  // Ensure minimum and maximum in right order.
  if AMax < AMin then
    raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.ClassName]);

  // Limit Position to Min and Max.
  if APosition < AMin then
    APosition := AMin;
  if APosition > AMax then
    APosition := AMax;

  Invalid := False;

  // Change Min if necessary and flag redrawing if so.
  if FMin <> AMin then
  begin
    FMin := AMin;
    Invalid := True;
  end;

  // Change Max if necessary and flag redrawing if so.
  if FMax <> AMax then
  begin
    FMax := AMax;
    Invalid := True;
  end;

  // Change Position if necessary and draw pointer accordingly.
  if APosition <> FPosition then
  begin
    FPosition := APosition;
    DrawPointer;
    Changed := True;
  end;

  // If redrawing flagged, cause a redraw, redoing the bitmap too.
  if Invalid then
  begin
    FBitmapInvalid := True;
    Changed := True;
    Invalidate;
  end;

  if Changed then
    // Notify the user of changes.
    Change;
end;

// Set all angle parameters at once.

procedure TJvCustomRadioControl.SetAngleParams(AnAngle, AMin, AMax: TJvRadioAngle);
var
  Invalid: Boolean;
  Pos: Integer;
begin
  // Error if AMax < AMin
  if AMax < AMin then
    raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);

  // Confine AnAngle to limits.
  if AnAngle < AMin then
    AnAngle := AMin;
  if AnAngle > AMax then
    AnAngle := AMax;
  Invalid := False;

  // Set MinAngle.
  if FMinAngle <> AMin then
  begin
    FMinAngle := AMin;
    Invalid := True;
  end;

  // Set MaxAngle.
  if FMaxAngle <> AMax then
  begin
    FMaxAngle := AMax;
    Invalid := True;
  end;

  // Redraw if necessary
  if Invalid then
  begin
    FBitmapInvalid := True;
    Invalidate;
  end;

  // Set Position.
  Pos := AngleToPos(AnAngle);
  if Pos <> FPosition then
    SetParams(Pos, FMin, FMax);
end;

procedure TJvCustomRadioControl.SetDefaultPos(Value: Integer);
begin
  // Change this if side effects are needed, e.g. to show a default pos marker.
  if Value <> FDefaultPos then
    FDefaultPos := Value;
end;

procedure TJvCustomRadioControl.SetFrequency(Value: Integer);
begin
  if Value <> FFrequency then
  begin
    FFrequency := Value;
    if FFrequency < 1 then FFrequency := 1; 
    if FTickStyle = tsAuto then
    begin
      ClearTicks;
      SetTicks(FTickStyle);
    end;
    FBitmapInvalid := True;
    Invalidate;
  end;
end;

procedure TJvCustomRadioControl.SetMin(Value: Integer);
begin
  SetParams(FPosition, Value, FMax);
end;

procedure TJvCustomRadioControl.SetMinAngle(Value: TJvRadioAngle);
begin
  SetAngleParams(PosToAngle(FPosition), Value, FMaxAngle);
end;

procedure TJvCustomRadioControl.SetMax(Value: Integer);
begin
  SetParams(FPosition, FMin, Value);
end;

procedure TJvCustomRadioControl.SetMaxAngle(Value: TJvRadioAngle);
begin
  SetAngleParams(PosToAngle(FPosition), FMinAngle, Value);
end;

procedure TJvCustomRadioControl.SetPosition(Value: Integer);
begin
  SetParams(Value, FMin, FMax);
end;

function TJvCustomRadioControl.CalcBounds(var AWidth, AHeight: Integer): Boolean;
var
  ASize: Integer;
begin
  Result := False;
  ASize := rcMinRadius + MinBorder + TickBorder;
  if FBorderStyle = bsSingle then
    Inc(ASize, GetSystemMetrics(SM_CXBORDER));
  ASize := 2 * ASize + 1;
  if AWidth < ASize then
  begin
    AWidth := ASize;
    Result := True;
  end;
  if AHeight < ASize then
  begin
    AHeight := ASize;
    Result := True;
  end;
end;

procedure TJvCustomRadioControl.SetRadius(Value: Integer);
var
  MaxRadius: Integer;
begin
  if Width <= Height then
    MaxRadius := (Width - 1) div 2 - MinBorder - TickBorder
  else
    MaxRadius := (Height - 1) div 2 - MinBorder - TickBorder;
  if FBorderStyle = bsSingle then
    Dec(MaxRadius, GetSystemMetrics(SM_CXBORDER));
  if Value > MaxRadius then
    Value := MaxRadius;
  if Value < rcMinRadius then
    Value := rcMinRadius;
  if Value <> FRadius then
  begin
    FRadius := Value;
    FBitmapInvalid := True;
    Invalidate;
  end;
  UpdateSize;
end;

procedure TJvCustomRadioControl.SetTicks(Value: TTickStyle);
var
  L: TJvTickLength;
  I: Integer;
begin
  if Value <> tsNone then
  begin
    SetTick(FMin, tlLong);
    SetTick(FMax, tlLong);
  end;
  if Value = tsAuto then
  begin
    I := FMin + FFrequency;
    L := tlMiddle;
    while I < FMax do
    begin
      SetTick(I, L);
      if L = tlMiddle then
        L := tlLong
      else
        L := tlMiddle;
      Inc(I, FFrequency);
    end;
  end;
end;

procedure TJvCustomRadioControl.SetTickStyle(Value: TTickStyle);
begin
  if FTickStyle <> Value then
  begin
    FTickStyle := Value;
    ClearTicks;
    SetTicks(Value);
    FBitmapInvalid := True;
    Invalidate;
  end;
end;

procedure TJvCustomRadioControl.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCustomRadioControl.SetSmallChange(Value: Integer);
begin
  if Value > FLargeChange then
    Value := FLargeChange div 2;
  if Value < 1 then
    Value := 1;
  FSmallChange := Value;
end;

procedure TJvCustomRadioControl.SetLargeChange(Value: Integer);
begin
  if Value <= FSmallChange + 1 then
    Value := FSmallChange + 1;
  FLargeChange := Value;
end;

procedure TJvCustomRadioControl.SetTick(Value: Integer; Length: TJvTickLength);
const
  Lengths: array [TJvTickLength] of Byte =
    (tlShortLen, tlMiddleLen, tlLongLen);
var
  P: PTick;
  I: Integer;
begin
  if (Value < FMin) or (Value > FMax) then
    raise EInvalidOperation.CreateFmt(SOutOfRange, [FMin, FMax]);
  for I := 0 to FTicks.Count - 1 do
  begin
    P := FTicks.Items[I];
    if P^.Value = Value then
    begin
      if P^.Length <> Lengths[Length] then
      begin
        P^.Length := Lengths[Length];
        P^.Changed := True;
        Invalidate;
      end;
      Exit;
    end;
  end;
  New(P);
  P^.Value := Value;
  P^.Length := Lengths[Length];
  P^.Changed := True;
  P^.Color := clBtnText;
  FTicks.Add(P);
  if HandleAllocated then
  begin
    DrawTick(FBitmap.Canvas, P^);
    DrawTick(Canvas, P^);
  end;
end;

procedure TJvCustomRadioControl.DrawTick(ACanvas: TCanvas; var T: TTick);
var
  Pt: TPoint;
  ValueAngle: Integer;
begin
  ValueAngle := PosToAngle(T.Value);
  ACanvas.Pen.Color := T.Color;
  Pt := AngleToPoint(ValueAngle, Center, FRadius);
  ACanvas.MoveTo(Pt.X, Pt.Y);
  Pt := AngleToPoint(ValueAngle, GetCenter, FRadius + T.Length);
  ACanvas.LineTo(Pt.X, Pt.Y);
  T.Changed := False;
end;

procedure TJvCustomRadioControl.Paint;
begin
  Canvas.Brush.Color := Parent.Brush.Color;
  Canvas.FillRect(ClientRect);
  BitmapNeeded;
  Canvas.CopyRect(FBitmapRect, FBitmap.Canvas, FBitmapRect);
  DrawBorder;
  DrawPointer;
end;

procedure TJvCustomRadioControl.DrawPointer;
var
  Outer, Inner, Extra: TPoint;
  InnerRadius, DotRadius: Integer;
  Region: HRgn;
  SmallRadius: Integer;

  function Lowest(A, B, C: Integer): Integer;
  begin
    if A < B then
      if A < C then
        Result := A
      else
        Result := C
    else
    if B < C then
      Result := B
    else
      Result := C
  end;

  function Highest(A, B, C: Integer): Integer;
  begin
    if A > B then
      if A > C then
        Result := A
      else
        Result := C
    else
    if B > C then
      Result := B
    else
      Result := C;
  end;

begin
  if not HandleAllocated then
    Exit;
  InnerRadius := (100 - FButtonEdge) * FRadius div 100 - 1;
  if FPointerRect.Left < 0 then
    FPointerRect := Rect(Center.X - InnerRadius,
      Center.Y - InnerRadius,
      Center.X + InnerRadius + 1,
      Center.Y + InnerRadius + 1);
  Canvas.CopyRect(FPointerRect, FBitmap.Canvas, FPointerRect);
  // This is for a solid dot. I'd also like to make a Ctl3D type of dot or
  // an open type of dot. We'd also have to make a disabled type of dot.
  Canvas.Pen.Color := FPointerColor;
  Canvas.Brush.Color := FPointerColor;
  case FPointerShape of
    psLine:
      begin
        Outer := AngleToPoint(Angle, Center, InnerRadius);
        Canvas.MoveTo(Outer.X, Outer.Y);
        Inner := AngleToPoint(Angle, Center, (101 - FPointerSize) * InnerRadius div 100);
        Canvas.LineTo(Inner.X, Inner.Y);
        FPointerRect := Rect(Math.Min(Inner.X, Outer.X),
          Math.Min(Inner.Y, Outer.Y),
          Math.Max(Inner.X, Outer.X),
          Math.Max(Inner.Y, Outer.Y));
      end;
    psTriangle:
      begin
        SmallRadius := FPointerSize * InnerRadius div 100;
        Outer := AngleToPoint(Angle, Center, InnerRadius);
        Inner := AngleToPoint(Angle - 1500, Outer, SmallRadius);
        Extra := AngleToPoint(Angle + 1500, Outer, SmallRadius);
        Canvas.Polygon([Outer, Inner, Extra]);
        FPointerRect := Rect(Lowest(Outer.X, Inner.X, Extra.X),
          Lowest(Outer.Y, Inner.Y, Extra.Y),
          Highest(Outer.X, Inner.X, Extra.X),
          Highest(Outer.Y, Inner.Y, Extra.Y));
      end;
    psDot:
      begin
        DotRadius := FPointerSize * InnerRadius div 200;
        Inner := AngleToPoint(Angle, Center, InnerRadius - DotRadius);
        if Inner.X > Center.X then
          Inc(Inner.X);
        if Inner.Y > Center.Y then
          Inc(Inner.Y);
        FPointerRect := Rect(Inner.X - DotRadius,
          Inner.Y - DotRadius,
          Inner.X + DotRadius,
          Inner.Y + DotRadius);
        with FPointerRect do
          Canvas.Ellipse(Left, Top, Right, Bottom);
      end;
    psOwnerDraw:
      if Assigned(FOnDrawPointer) then
      begin
        DotRadius := FPointerSize * InnerRadius div 200;
        Outer := AngleToPoint(Angle, Center, InnerRadius - DotRadius);
        if Outer.X > Center.X then
          Inc(Outer.X);
        if Outer.Y > Center.Y then
          Inc(Outer.Y);
        FPointerRect := Rect(Outer.X - DotRadius,
          Outer.Y - DotRadius,
          Outer.X + DotRadius,
          Outer.Y + DotRadius);

        // Create a clipping region to protect the area outside the button
        // face.
        with FPointerRect do
          Region := CreateEllipticRgn(Left - 1, Top - 1, Right + 1, Bottom + 1);
        SelectClipRgn(Canvas.Handle, Region);
        try
          FOnDrawPointer(Self, FPointerRect);
        except
          DeleteObject(Region);
          SelectClipRgn(Canvas.Handle, 0);
          raise;
        end;
      end;
  end;
  InflateRect(FPointerRect, 1, 1);
end;

procedure TJvCustomRadioControl.BitmapNeeded;
begin
  if FBitmap = nil then
  begin
    FBitmap := TBitmap.Create;
    FBitmapInvalid := True;
  end;
  if FBitmapInvalid then
  begin
    if FBitmap.Width <> FSize + 1 then
    begin
      FBitmap.Width := FSize + 1;
      FBitmap.Height := FSize + 1;
      FBitmapRect := Bounds(0, 0, FSize + 1, FSize + 1);
    end;

    // Draw on bitmap.
    DrawButton;
    DrawTicks;
  end;
end;

function Blend(const Factor: Double; const Color1, Color2: TColor): TColor;
var
  Factor2: Double;
begin
  Factor2 := 1.0 - Factor;
  with TRGBQuad(Result) do
  begin
    rgbBlue := Trunc(Factor * TRGBQuad(Color1).rgbBlue + Factor2 * TRGBQuad(Color2).rgbBlue);
    rgbGreen := Trunc(Factor * TRGBQuad(Color1).rgbGreen + Factor2 * TRGBQuad(Color2).rgbGreen);
    rgbRed := Trunc(Factor * TRGBQuad(Color1).rgbRed + Factor2 * TRGBQuad(Color2).rgbRed);
    rgbReserved := 0;
  end;
end;

procedure TJvCustomRadioControl.DrawButton;
const
  HalfPi = 1.57079632679489661923;
var
  Edge: Integer;
  ButtonRect: TRect;
  Face, Highlight, Shadow: TColor;
  Size: Integer;
  OldOrg: TPoint;
  C: TCanvas;
  I: Integer;
begin
  Size := 2 * FRadius + 1;
  ButtonRect := Bounds(0, 0, Size, Size);
  C := FBitmap.Canvas;
  C.Brush.Color := Parent.Brush.Color;
  C.Brush.Style := bsSolid;
  C.FillRect(FBitmapRect);
  SetViewPortOrgEx(C.Handle, FSize div 2 - FRadius, FSize div 2 - FRadius,
    @OldOrg);

  // Draw edge.
  C.Pen.Style := psClear;

  Highlight := ColorToRGB(clBtnHighlight);
  Face := ColorToRGB(Color);
  Shadow := (ColorToRGB(Color) and $00FEFEFE) shr 1;

  for I := 0 to Size do
  begin
    C.Brush.Color := Blend(Cos(I * HalfPi / Size), Highlight, Face);
    C.Pie(0, 0, Size, Size, I + 1, 0, I - 1, 0);
    C.Pie(0, 0, Size, Size, 0, I - 1, 0, I + 1);
  end;

  for I := 0 to Size do
  begin
    C.Brush.Color := Blend(1.0 - Sin(I * HalfPi / Size), Face, Shadow);
    C.Pie(0, 0, Size, Size, Size, I + 1, Size, I - 1);
    C.Pie(0, 0, Size, Size, I - 1, Size, I + 1, Size);
  end;

  // Draw top of disk.
  C.Pen.Style := psSolid;
  C.Pen.Color := Color;
  C.Brush.Color := Color;
  Edge := FButtonEdge * FRadius div 100 + 1;
  C.Ellipse(0 + Edge, 0 + Edge, 0 + Size - Edge, 0 + Size - Edge);

  // Draw bounding circle.
  C.Pen.Color := clBtnText;
  C.Brush.Style := bsClear;
  C.Ellipse(0, 0, Size, Size);

  // Reset viewport origin.
  SetViewportOrgEx(C.Handle, OldOrg.X, OldOrg.Y, nil);
  FBitmapInvalid := False;
end;

procedure TJvCustomRadioControl.SetPointerShape(Value: TJvRadioPointerShape);
begin
  if Value <> FPointerShape then
  begin
    FPointerShape := Value;
    Invalidate;
  end;
end;

procedure TJvCustomRadioControl.DrawBorder;
var
  ARect: TRect;
begin
  ARect := ClientRect;
  InflateRect(ARect, -1, -1);
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := Parent.Brush.Color;
  Canvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  Canvas.Brush.Style := bsSolid;
  if GetFocus = Self.Handle then
    Canvas.DrawFocusRect(ARect);
end;

procedure TJvCustomRadioControl.DrawTicks;
var
  I: Integer;
begin
  if (FTickStyle = tsNone) or (FTicks = nil) or (FTicks.Count = 0) then
    Exit;
  for I := 0 to FTicks.Count - 1 do
    DrawTick(FBitmap.Canvas, PTick(FTicks.List[I])^);
end;

procedure TJvCustomRadioControl.UpdateSize;
begin
  FSize := 2 * (MinBorder + FRadius + TickBorder) + 1;
end;

procedure TJvCustomRadioControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if CalcBounds(AWidth, AHeight) then
    FBitmapInvalid := True;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetRadius(AWidth + AHeight);
end;

procedure TJvCustomRadioControl.CMParentColorChanged(var Msg: TMessage);
begin
  FBitmapInvalid := True;
  inherited;
end;

// Set button edge in percent (0 - 100).

procedure TJvCustomRadioControl.SetButtonEdge(Value: Integer);
begin
  if Value < rcMinEdge then
    Value := rcMinEdge;
  if Value > rcMaxEdge then
    Value := rcMaxEdge;
  if Value <> FButtonEdge then
  begin
    FButtonEdge := Value;
    if not FBitmapInvalid then
    begin
      FBitmapInvalid := True;
      Invalidate;
    end;
  end;
end;

procedure TJvCustomRadioControl.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if HandleAllocated then
    DrawBorder;
end;

procedure TJvCustomRadioControl.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if HandleAllocated then
    DrawBorder;
end;

procedure TJvCustomRadioControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  A: TJvRadioAngle;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Focused then
  begin
    SetFocus;
    Invalidate;
  end;
  if PtInRect(FPointerRect, Point(X, Y)) then
    MouseCapture := True
  else
  begin
    A := RadToAngle(PointToRad(Point(X, Y), GetCenter));
    if A < Angle then
    begin
      DecPos(Shift);
      FIncrementing := False;
    end
    else
    begin
      IncPos(Shift);
      FIncrementing := True;
    end;
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);
    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := FRepeatDelay;
    FRepeatTimer.Enabled := True;
  end;
end;

procedure TJvCustomRadioControl.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Enabled := False;
  FRepeatTimer.Interval := FRepeatRate;
  if FIncrementing then
    IncPos(GetShiftState)
  else
    DecPos(GetShiftState);
  FRepeatTimer.Enabled := True;
end;

procedure TJvCustomRadioControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
    SetAngle(RadToAngle(PointToRad(Point(X, Y), GetCenter)));
end;

procedure TJvCustomRadioControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;
  MouseCapture := False;
end;

function TJvCustomRadioControl.GetCenter: TPoint;
begin
  with Result do
  begin
    X := FSize div 2;
    Y := X;
  end;
end;

procedure TJvCustomRadioControl.ClearTicks;
var
  I: Integer;
begin
  if FTicks <> nil then
    with FTicks do
    begin
      for I := 0 to Count - 1 do
        if List[I] <> nil then
          Dispose(PTick(List[I]));
      Clear;
    end;
end;

procedure TJvCustomRadioControl.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of Cardinal = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BorderStyles[FBorderStyle];
  if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
  begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_STATICEDGE;
  end;
end;

procedure TJvCustomRadioControl.SetPointerColor(Value: TColor);
begin
  if Value <> FPointerColor then
  begin
    FPointerColor := Value;
    DrawPointer;
  end;
end;

procedure TJvCustomRadioControl.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  FBitmapInvalid := True;
  RecreateWnd;
end;

procedure TJvCustomRadioControl.IncPos(Shift: TShiftState);
begin
  if ssShift in Shift then
    Position := Position + FLargeChange
  else
  if ssCtrl in Shift then
    Position := FMax
  else
    Position := Position + FSmallChange;
end;

procedure TJvCustomRadioControl.DecPos(Shift: TShiftState);
begin
  if ssShift in Shift then
    Position := Position - FLargeChange
  else
  if ssCtrl in Shift then
    Position := FMin
  else
    Position := Position - FSmallChange;
end;

procedure TJvCustomRadioControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_RIGHT:
      IncPos(Shift);
    VK_DOWN, VK_LEFT:
      DecPos(Shift);
    VK_PRIOR:
      IncPos(Shift + [ssShift]);
    VK_NEXT:
      DecPos(Shift + [ssShift]);
    VK_HOME:
      Position := FMin;
    VK_END:
      Position := FMax;
  else
    begin
      inherited KeyDown(Key, Shift);
      Exit;
    end;
  end;
  // (rom) unreachable code
  //Key := 0;
  //inherited KeyDown(Key, Shift);
end;

procedure TJvCustomRadioControl.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = CN_KEYDOWN then
    DoKeyDown(TWMKey(Msg));
  inherited WndProc(Msg);
end;

procedure TJvCustomRadioControl.WMSysColorChange(var Msg: TMessage);
begin
  FBitmapInvalid := True;
  Invalidate;
end;

procedure TJvCustomRadioControl.SetPointerSize(Value: Integer);
begin
  if Value > 100 then
    Value := 100
  else
  if Value < 1 then
    Value := 1;
  if Value <> FPointerSize then
  begin
    FPointerSize := Value;
    DrawPointer;
  end;
end;

function TJvCustomRadioControl.AngleToRad(AnAngle: TJvRadioAngle): Double;
begin
  Result := dAngleToRadian * AnAngle;
end;

procedure TJvCustomRadioControl.CMColorChanged(var Msg: TMessage);
begin
  FBitmapInvalid := True;
  inherited;
end;

procedure TJvCustomRadioControl.Loaded;
begin
  inherited Loaded;
  Change;
end;

function TJvCustomRadioControl.RadToAngle(const Radian: Double): TJvRadioAngle;
begin
  Result := Round(dRadianToAngle * Radian);
end;

end.

