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

{ TRadioControl component, a button like the dial on a radio. }

unit JvRadioCtl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Math;

const
  dAngleToDouble = Pi / 1800;
  dDoubleToAngle = 1800 / Pi;
  rcMaxEdge      = 10;
  rcMinEdge      = 2;
  rcMinRadius    = 15;
  tlLongLen      = 10;
  tlMiddleLen    = 6;
  tlShortLen     = 4;

type
  TJvRadioPointerShape = (psLine, psTriangle, psDot, psOwnerDraw);
  TJvTickLength = (tlShort, tlMiddle, tlLong);
  TJvRadioAngle = 0..3600;        // 0.0 - 360.0 deg
  TJvRepeatValue = 10..1000;      // mouse repeat values
  TJvRadioControl = class;
  TJvRadioDrawEvent = procedure (Sender: TJvRadioControl; ARect: TRect) of object;
  
  TJvRadioControl = class(TCustomControl)
  private
    FBitmap: TBitmap;
    FBitmapRect: TRect;
    FBitmapInvalid: Boolean;
    FBorderStyle: TBorderStyle;
    FButtonEdge: Integer;
//    FCenter: TPoint;
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
//    FOnScroll: TScrollEvent;
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
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBorder; dynamic;
    procedure DrawButton; dynamic;
    procedure DrawPointer; dynamic;
    procedure DrawTick(ACanvas: TCanvas; P: Pointer); dynamic;
    procedure DrawTicks; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function PosToAngle(Pos: Integer): TJvRadioAngle;
    procedure SetTicks(Value: TTickStyle); virtual;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSysColorChange(var Message: TMessage); message WM_SYSCOLORCHANGE;
    procedure WndProc(var Message: TMessage); override;
    procedure IncPos(Shift: TShiftState); dynamic;
    procedure DecPos(Shift: TShiftState); dynamic;
    property Ticks: TList read FTicks write FTicks stored True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function AngleToPoint(AnAngle: TJvRadioAngle; ACenter: TPoint; ARadius: Integer): TPoint;
    procedure SetAngleParams(AnAngle, AMin, AMax: TJvRadioAngle); virtual;
    procedure SetParams(APosition, AMin, AMax: Integer); virtual;
    procedure SetTick(Value: Integer; Length: TJvTickLength); virtual;
    property Bitmap: TBitmap read FBitmap;
    property Center: TPoint read GetCenter;
  published
    property Align;
    property Angle: TJvRadioAngle read GetAngle write SetAngle stored False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property ButtonEdge: Integer read FButtonEdge write SetButtonEdge default 2;
    property Color;
    property Ctl3D;
    property Cursor;
    property DefaultPos: Integer read FDefaultPos write SetDefaultPos;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Frequency: Integer read FFrequency write SetFrequency default 10;
    property LargeChange: Integer read FLargeChange write SetLargeChange default 2;
    property Max: Integer read FMax write SetMax default 100;
    property MaxAngle: TJvRadioAngle read FMaxAngle write SetMaxAngle default 3300;
    property Min: Integer read FMin write SetMin default 0;
    property MinAngle: TJvRadioAngle read FMinAngle write SetMinAngle default 300;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PointerColor: TColor read FPointerColor write SetPointerColor default clBtnText;
    property PointerSize: Integer read FPointerSize write SetPointerSize default 33;
    property PointerShape: TJvRadioPointerShape read FPointerShape write SetPointerShape default psLine;
    property PopupMenu;
    property Position: Integer read FPosition write SetPosition default 0;
    property Radius: Integer read FRadius write SetRadius;
    property RepeatDelay: TJvRepeatValue read FRepeatDelay write FRepeatDelay default 400;
    property RepeatRate: TJvRepeatValue read FRepeatRate write FRepeatRate default 100;
    property ShowHint;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property TickStyle: TTickStyle read FTickStyle write SetTickStyle stored True;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawPointer: TJvRadioDrawEvent read FOnDrawPointer write FOnDrawPointer;
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

uses Consts;

type
  TFakeControl = class(TWinControl);    // TWinControl.Color is not public.

const
  MinBorder = 1;
  TickBorder = tlLongLen;

type
  PTickRec = ^TTickRec;
  TTickRec = record
    Value: Integer;
    Length: Byte;
    Changed: Boolean;
  end;

function GetShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;
 

constructor TJvRadioControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents, csCaptureMouse];
  FTicks := TList.Create;
  FBorderStyle := bsNone;
  FButtonEdge := 2;
  FDefaultPos := 0;
  FFrequency := 10;
  FLargeChange := 2;
  FMax := 100;
  FMaxAngle := 3300;
  FMin := 0;
  FMinAngle := 300;
  FPointerColor := clBtnText;
  FPointerSize := 33;
  FPosition := 0;
  FRadius := rcMinRadius;
  FSmallChange := 1;
  TabStop := True;
  FTickStyle := tsAuto;
  FBitmapInvalid := True;
  FPointerRect.Left := -1;      // Only on start up
  Width := 51;
  Height := 51;
  FRepeatDelay := 400;
  FRepeatRate := 100;
  SetTicks(FTickStyle);
end;

destructor TJvRadioControl.Destroy;
begin
  FBitmap.Free;
  ClearTicks;
  FTicks.Free;
  FRepeatTimer.Free;
  inherited Destroy;
end;

function TJvRadioControl.PosToAngle(Pos: Integer): TJvRadioAngle;
begin
  Result := FMinAngle + ((FMaxAngle - FMinAngle) * (Pos - FMin) div (FMax - FMin));
end;

function TJvRadioControl.AngleToPos(AnAngle: TJvRadioAngle): Integer;
begin
  Result := FMin + ((FMax - FMin) * (AnAngle - FMinAngle) div (FMaxAngle - FMinAngle));
end;

function TJvRadioControl.AngleToPoint(AnAngle: TJvRadioAngle; ACenter: TPoint;
  ARadius: Integer): TPoint;
var
  RadAngle: Double;
begin
  RadAngle := AnAngle * dAngleToDouble;
  Result.X := ACenter.X - Round(ARadius * Sin(RadAngle));
  Result.Y := ACenter.Y + Round(ARadius * Cos(RadAngle));
end;

function PointToRad(APoint, ACenter: TPoint): Double;
var
  N: Integer;
begin
  N := APoint.X - ACenter.X;
  if N = 0 then
    Result := Pi
  else
    Result := ArcTan((ACenter.Y - APoint.Y) / N);
  if N < 0 then
    Result := Result + Pi;
  Result := 1.5 * Pi - Result;
end;

function TJvRadioControl.GetAngle: TJvRadioAngle;
begin
  Result := PosToAngle(FPosition);
end;

procedure TJvRadioControl.SetAngle(Value: TJvRadioAngle);
begin
  SetAngleParams(Value, FMinAngle, FMaxAngle);
end;

procedure TJvRadioControl.SetBorderStyle(Value: TBorderStyle);
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

procedure TJvRadioControl.SetParams(APosition, AMin, AMax: Integer);
var
  Invalid: Boolean;
begin
  if AMax < AMin then
    raise EInvalidOperation.CreateResFmt(@SPropertyOutOfRange, [Self.ClassName]);
  if APosition < AMin then APosition := AMin;
  if APosition > AMax then APosition := AMax;
  Invalid := False;
  if FMin <> AMin then
  begin
    FMin := AMin;
    Invalid := True;
  end;
  if FMax <> AMax then
  begin
    FMax := AMax;
    Invalid := True;
  end;
  if APosition <> FPosition then
  begin
    FPosition := APosition;
    DrawPointer;
  end;
  if Invalid then
  begin
    ClearTicks;
    SetTicks(FTickStyle);
    FBitmapInvalid := True;
    Invalidate;
  end;
  Change;
end;

procedure TJvRadioControl.SetAngleParams(AnAngle, AMin, AMax: TJvRadioAngle);
var
  Invalid: Boolean;
begin
  if AMax < AMin then
    raise EInvalidOperation.CreateResFmt(@SPropertyOutOfRange, [Self.Classname]);
  if AnAngle < AMin then AnAngle := AMin;
  if AnAngle > AMax then AnAngle := AMax;
  Invalid := False;
  if FMinAngle <> AMin then
  begin
    FMinAngle := AMin;
    Invalid := True;
  end;
  if FMaxAngle <> AMax then
  begin
    FMaxAngle := AMax;
    Invalid := True;
  end;
  if Invalid then
  begin
    FBitmapInvalid := True;
    Invalidate;
  end;
  if AnAngle <> GetAngle then
    SetParams(AngleToPos(AnAngle), FMin, FMax);
end;

procedure TJvRadioControl.SetDefaultPos(Value: Integer);
begin
  if Value <> FDefaultPos then
  begin
    FDefaultPos := Value;
  end;
end;

procedure TJvRadioControl.SetFrequency(Value: Integer);
begin
  if Value <> FFrequency then
  begin
    FFrequency := Value;
    if FTickStyle = tsAuto then
    begin
      ClearTicks;
      SetTicks(FTickStyle);
    end;
    FBitmapInvalid := True;
    Invalidate;
  end;
end;

procedure TJvRadioControl.SetMin(Value: Integer);
begin
  SetParams(FPosition, Value, FMax);
end;

procedure TJvRadioControl.SetMinAngle(Value: TJvRadioAngle);
begin
  SetAngleParams(PosToAngle(FPosition), Value, FMaxAngle);
end;

procedure TJvRadioControl.SetMax(Value: Integer);
begin
  SetParams(FPosition, FMin, Value);
end;

procedure TJvRadioControl.SetMaxAngle(Value: TJvRadioAngle);
begin
  SetAngleParams(PosToAngle(FPosition), FMinAngle, Value);
end;

procedure TJvRadioControl.SetPosition(Value: Integer);
begin
  SetParams(Value, FMin, FMax);
end;

function TJvRadioControl.CalcBounds(var AWidth, AHeight: Integer): Boolean;
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

procedure TJvRadioControl.SetRadius(Value: Integer);
var
  MaxRadius: Integer;
begin
  if Width <= Height then
    MaxRadius := (Width - 1) div 2 - MinBorder - TickBorder
  else
    MaxRadius := (Height - 1) div 2 - MinBorder - TickBorder;
  if FBorderStyle = bsSingle then
    Dec(MaxRadius, GetSystemMetrics(SM_CXBORDER));
  if (Value > MaxRadius) then
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

procedure TJvRadioControl.SetTicks(Value: TTickStyle);
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
      if L = tlMiddle then L := tlLong else L := tlMiddle;
      Inc(I, FFrequency);
    end;
  end;
end;

procedure TJvRadioControl.SetTickStyle(Value: TTickStyle);
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

procedure TJvRadioControl.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvRadioControl.SetSmallChange(Value: Integer);
begin
  if Value > FLargeChange then
    Value := FLargeChange div 2;
  if Value < 1 then
    Value := 1;
  if Value <> FSmallChange then
    FSmallChange := Value;
end;

procedure TJvRadioControl.SetLargeChange(Value: Integer);
begin
  if Value <= FSmallChange + 1 then
    Value := FSmallChange + 1;
  if Value <> FLargeChange then
    FLargeChange := Value;
end;

procedure TJvRadioControl.SetTick(Value: Integer; Length: TJvTickLength);
const
  Lengths: array[TJvTickLength] of Byte = (tlShortLen, tlMiddleLen, tlLongLen);
var
  P: PTickRec;
  I: Integer;
begin
  if (Value < FMin) or (Value > FMax) then
    raise EInvalidOperation.CreateResFmt(@SOutOfRange, [FMin, FMax]);
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
  FTicks.Add(P);
  if HandleAllocated then
  begin
    DrawTick(FBitmap.Canvas, P);
    DrawTick(Canvas, P);
  end;
end;

procedure TJvRadioControl.DrawTick(ACanvas: TCanvas; P: Pointer);
var
  Pt: TPoint;
  TR: PTickRec absolute P;
  ValueAngle: Integer;
begin
  with ACanvas do
  begin
    ValueAngle := PosToAngle(TR^.Value);
    Pen.Color := clBtnText;
    Pt := AngleToPoint(ValueAngle, GetCenter, FRadius);
    MoveTo(Pt.X, Pt.Y);
    Pt := AngleToPoint(ValueAngle, GetCenter, FRadius + TR^.Length);
    LineTo(Pt.X, Pt.Y);
    TR^.Changed := False;
  end;
end;

procedure TJvRadioControl.Paint;
begin
  with Canvas do
  begin
    Brush.Color := TFakeControl(Parent).Color;
    FillRect(ClientRect);
  end;
  BitmapNeeded;
  Canvas.CopyRect(FBitmapRect, FBitmap.Canvas, FBitmapRect);
  DrawBorder;
  DrawPointer;
end;

procedure TJvRadioControl.DrawPointer;
var
  Outer, Inner, Inner2: TPoint;
  InnerRadius, DotRadius: Integer;
  Region: HRgn;

  function Min(A, B: Integer): Integer;
  begin
    if A < B then Result := A else Result := B;
  end;

  function Max(A, B: Integer): Integer;
  begin
    if A > B then Result := A else Result := B;
  end;

  function Lowest(A, B, C: Integer): Integer;
  begin
    if A < B then
      if A < C then Result := A else Result := C
    else
      if B < C then Result := B else Result := C
  end;

  function Highest(A, B, C: Integer): Integer;
  begin
    if A > B then
      if A > C then Result := A else Result := C
    else
      if B > C then Result := B else Result := C;
  end;

begin
  if not HandleAllocated then
    Exit;
  InnerRadius := FRadius - FButtonEdge - 1;
  if FPointerRect.Left < 0 then
    FPointerRect := Rect(GetCenter.X - InnerRadius, GetCenter.Y - InnerRadius,
                         GetCenter.X + InnerRadius + 1, GetCenter.Y + InnerRadius + 1);
  Canvas.CopyRect(FPointerRect, FBitmap.Canvas, FPointerRect);
  with Canvas do
  begin
  
    // This is for a solid dot. I'd also like to make a Ctl3D type of dot or
    // an open type of dot. We'd also have to make a disabled type of dot.
    Pen.Color := FPointerColor;
    Brush.Color := FPointerColor;
    case FPointerShape of
      psLine:
        begin
          Pen.Color := FPointerColor;
          Outer := AngleToPoint(GetAngle, GetCenter, InnerRadius);
          MoveTo(Outer.X, Outer.Y);
          Inner := AngleToPoint(GetAngle, GetCenter, (101 - FPointerSize) * InnerRadius div 100);
          LineTo(Inner.X, Inner.Y);
          FPointerRect := Rect(Min(Inner.X, Outer.X), Min(Inner.Y, Outer.Y),
                               Max(Inner.X, Outer.X), Max(Inner.Y, Outer.Y));
        end;
      psTriangle:
        begin
          Pen.Color := FPointerColor;
          Brush.Color := FPointerColor;
          Outer := AngleToPoint(GetAngle, GetCenter, InnerRadius);
          Inner := AngleToPoint(GetAngle - 1500, Outer, FPointerSize * InnerRadius div 100);
          Inner2 := AngleToPoint(GetAngle + 1500, Outer, FPointerSize * InnerRadius div 100);
          Polygon([Outer, Inner, Inner2]);
          FPointerRect := Rect(Lowest(Outer.X, Inner.X, Inner2.X),
                               Lowest(Outer.Y, Inner.Y, Inner2.Y),
                               Highest(Outer.X, Inner.X, Inner2.X),
                               Highest(Outer.Y, Inner.Y, Inner2.Y));
        end;
      psDot:
        begin
          Pen.Color := FPointerColor;
          Brush.Color := FPointerColor;
          DotRadius := FPointerSize * (FRadius - FButtonEdge) div 200;
          Inner := AngleToPoint(GetAngle, GetCenter, FRadius - FButtonEdge - DotRadius - 1);
          if Inner.X > GetCenter.X then Inc(Inner.X);
          if Inner.Y > GetCenter.Y then Inc(Inner.Y);
          FPointerRect := Rect(Inner.X - DotRadius, Inner.Y - DotRadius,
                               Inner.X + DotRadius, Inner.Y + DotRadius);
          with FPointerRect do
            Ellipse(Left, Top, Right, Bottom);
        end;
      psOwnerDraw:
        if Assigned(FOnDrawPointer) then
        begin
          FPointerRect := Rect(GetCenter.X - InnerRadius,
                               GetCenter.Y - InnerRadius,
                               GetCenter.X + InnerRadius + 1,
                               GetCenter.Y + InnerRadius + 1);

          // Create a clipping region to protect the area outside the button
          // face.
          with FPointerRect do
            Region := CreateEllipticRgn(Left, Top, Right, Bottom);
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
end;

procedure TJvRadioControl.BitmapNeeded;
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

procedure TJvRadioControl.DrawButton;
var
  Edge: Integer;
  ButtonRect: TRect;
  Face, Light, Shadow: TColor;
  Size, Size20, Size50, Size80: Integer;
  OldOrg: TPoint;

  function ColorMix(Color1, Color2: Longint): TColor;
  var
    Quad1: TRGBQuad absolute Color1;
    Quad2: TRGBQuad absolute Color2;
    Quad3: TRGBQuad absolute Result;
  begin
    Quad3.rgbBlue := (Quad1.rgbBlue + Quad2.rgbBlue) shr 1;
    Quad3.rgbGreen := (Quad1.rgbGreen + Quad2.rgbGreen) shr 1;
    Quad3.rgbRed := (Quad1.rgbRed + Quad2.rgbRed) shr 1;
  end;

begin
  Size := 2 * FRadius + 1;
  Size20 := (Size + 2) div 5;
  Size50 := (Size + 1) div 2;
  Size80 := 4 * Size div 5;
  ButtonRect := Bounds(0, 0, Size, Size);
  with FBitmap.Canvas do
  begin
    Brush.Color := TFakeControl(Parent).Color;
    Brush.Style := bsSolid;
    FillRect(FBitmapRect);
    SetViewPortOrgEx(Handle, FSize div 2 - FRadius, FSize div 2 - FRadius,
      @OldOrg);

    // Draw edge.
    Pen.Style := psClear;

    // Draw top left half of edge in clBtnHighlight.
    Brush.Color := clBtnHighlight;
    Chord(0, 0, Size, Size, Size, 1, 1, Size);

    // Draw bottom left half of button in clBtnShadow.
    Brush.Color := clBtnShadow;
    Chord(0, 0, Size, Size, 0, Size, Size, 0);

    // Get color values and mix them for the gradient.
    Shadow := ColortoRGB(clBtnShadow);
    Light := ColorToRGB(clBtnHighlight);
    Face := ColorMix(Light, Shadow);
    Light := ColorMix(Light, Face);
    Shadow := ColorMix(Shadow, Face);

    // Draw top left lighter parts.
    Brush.Color := Light;
    Pie(0, 0, Size, Size, Size80 + 1, 0, Size50 + 1, 0);
    Pie(0, 0, Size, Size, 0, Size50 + 1, 0, Size80 + 1);

    // Draw center normal parts.
    Brush.Color := Face;
    Pie(0, 0, Size, Size, Size, Size20, Size80, 0);
    Pie(0, 0, Size, Size, 0, Size80, Size20, Size);

    // Draw bottom right darker parts.
    Brush.Color := Shadow;
    Pie(0, 0, Size, Size, Size, Size50, Size, Size20);
    Pie(0, 0, Size, Size, Size20, Size, Size50, Size);

    // Draw top of disk.
    Pen.Style := psSolid;
    Pen.Color := clBtnFace;
    Brush.Color := clBtnFace;
    Edge := FButtonEdge + 1;
    Ellipse(0 + Edge, 0 + Edge, 0 + Size - Edge, 0 + Size - Edge);

    // Draw bounding circle.
    Pen.Color := clBtnText;
    Pen.Style := psSolid;
    Brush.Style := bsClear;
    Ellipse(0, 0, Size, Size);

    // Reset viewport origin.
    SetViewPortOrgEx(Handle, OldOrg.X, OldOrg.Y, nil);
  end;
  FBitmapInvalid := False;
end;

procedure TJvRadioControl.SetPointerShape(Value: TJvRadioPointerShape);
begin
  if Value <> FPointerShape then
  begin
    FPointerShape := Value;
    Invalidate;
  end;
end;

procedure TJvRadioControl.DrawBorder;
var
  ARect: TRect;
begin
  ARect := ClientRect;
  InflateRect(ARect, -1, -1);
  with Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Color := TFakeControl(Parent).Color;
    with ARect do
      Rectangle(Left, Top, Right, Bottom);
    Brush.Style := bsSolid;
    if GetFocus = Self.Handle then
      DrawFocusRect(ARect);
  end;
end;

procedure TJvRadioControl.DrawTicks;
var
  I: Integer;
begin
  if (FTickStyle = tsNone) or (FTicks = nil) or (FTicks.Count = 0) then
    Exit;
  for I := 0 to FTicks.Count - 1 do
    DrawTick(FBitmap.Canvas, FTicks.List[I]);
end;

procedure TJvRadioControl.UpdateSize;
begin
  FSize := 2 * (MinBorder + FRadius + TickBorder) + 1;
end;

procedure TJvRadioControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if CalcBounds(AWidth, AHeight) then
    FBitmapInvalid := True;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetRadius(AWidth + AHeight);
end;

procedure TJvRadioControl.CMParentColorChanged(var Message: TMessage);
begin
  FBitmapInvalid := True;
  inherited;
end;

procedure TJvRadioControl.SetButtonEdge(Value: Integer);
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

procedure TJvRadioControl.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if HandleAllocated then
    DrawBorder;
end;

procedure TJvRadioControl.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if HandleAllocated then
    DrawBorder;
end;

procedure TJvRadioControl.MouseDown(Button: TMouseButton;
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
    A := Round(dDoubleToAngle * PointToRad(Point(X, Y), GetCenter));
    if A < GetAngle then
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

procedure TJvRadioControl.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Enabled := False;
  FRepeatTimer.Interval := FRepeatRate;
  if FIncrementing then
    IncPos(GetShiftState)
  else
    DecPos(GetShiftState);
  FRepeatTimer.Enabled := True;
end;

procedure TJvRadioControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
    SetAngle(Round(dDoubleToAngle * PointToRad(Point(X, Y), GetCenter)));
end;

procedure TJvRadioControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;
  MouseCapture := False;
end;

function TJvRadioControl.GetCenter: TPoint;
begin
  with Result do
  begin
    X := FSize div 2;
    Y := X;
  end;
end;

procedure TJvRadioControl.ClearTicks;
var
  I: Integer;
begin
  if FTicks <> nil then
    with FTicks do
    begin
      for I := 0 to Count - 1 do
        if List[I] <> nil then
          Dispose(PTickRec(List[I]));
      Clear;
    end;
end;

procedure TJvRadioControl.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of Cardinal = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_STATICEDGE;
    end;
  end;
end;

procedure TJvRadioControl.SetPointerColor(Value: TColor);
begin
  if Value <> FPointerColor then
  begin
    FPointerColor := Value;
    DrawPointer;
  end;
end;

procedure TJvRadioControl.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  FBitmapInvalid := True;
  RecreateWnd;
end;

procedure TJvRadioControl.IncPos(Shift: TShiftState);
begin
  if ssShift in Shift then
    Position := Position + FLargeChange
  else if ssCtrl in Shift then
    Position := FMax
  else
    Position := Position + FSmallChange;
end;

procedure TJvRadioControl.DecPos(Shift: TShiftState);
begin
  if ssShift in Shift then
    Position := Position - FLargeChange
  else if ssCtrl in Shift then
    Position := FMin
  else
    Position := Position - FSmallChange;
end;

procedure TJvRadioControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_UP) or (Key = VK_RIGHT) then
  begin
    IncPos(Shift);
    Key := 0;
  end
  else if (Key = VK_DOWN) or (Key = VK_LEFT) then
  begin
    DecPos(Shift);
    Key := 0;
  end
  else if (Key = VK_HOME) then
  begin
    Position := FMin;
    Key := 0;
  end
  else if (Key = VK_END) then
  begin
    Position := FMax;
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TJvRadioControl.WndProc(var Message: TMessage);
begin
  if Message.Msg = CN_KeyDown then
    DoKeyDown(TWMKey(Message));
  inherited WndProc(Message);
end;

procedure TJvRadioControl.WMSysColorChange(var Message: TMessage);
begin
  FBitmapInvalid := True;
  Invalidate;
end;

procedure TJvRadioControl.SetPointerSize(Value: Integer);
begin
  if Value > 100 then
    Value := 100
  else if Value < 1 then
    Value := 1;
  if Value <> FPointerSize then
  begin
    FPointerSize := Value;
    DrawPointer;
  end;
end;

end.
