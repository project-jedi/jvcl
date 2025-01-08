{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorBar.PAS, released on 2004-03-15.

The Initial Developer of the Original Code is Stefano Pessina [stefano dott pessina sanbiagiomonza dott it]
Portions created by Stefano Pessina are Copyright (C) 2004 Stefano Pessina.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvColorTrackbar;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Types, Classes, Controls, Graphics, Forms,
  JvComponent, JvJVCLUtils;

type
  TControlBorderStyle = bsNone..bsSingle;
  TJvColorTrackBarIndicator = (tbiArrow, tbiLine);
  TJvColorTrackBarIndicators = set of TJvColorTrackBarIndicator;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvColorTrackBar = class(TJvGraphicControl)
  private
    //FShowValue: Boolean;
    FPosition, FMin, FMax: Integer;
    FButtonDown: Boolean;
    FOnPosChanged: TNotifyEvent;
    FBmpImage: TBitmap;
    FColorFrom: TColor;
    FColorTo: TColor;
    FArrowColor: TColor;
    FOnMaxChange: TNotifyEvent;
    FOnMinChange: TNotifyEvent;
    FBorderStyle: TControlBorderStyle;
    FReadOnly: Boolean;
    FIndicators: TJvColorTrackBarIndicators;
    FFillDirection: TFillDirection;
    procedure SetPosition(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetArrowColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TControlBorderStyle);
    procedure SetIndicators(const Value: TJvColorTrackBarIndicators);
    procedure SetFillDirection(const Value: TFillDirection);
  protected
    procedure Changed; virtual;
    procedure MinChanged; virtual;
    procedure MaxChanged; virtual;
    procedure Paint; override;
  public
    property Canvas;
    procedure UpdateGradient;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function WindowToPos(WindowCoord: Integer): Integer;
    function PosToWindow(APos: Integer): Integer;
  published
    property Indicators: TJvColorTrackBarIndicators read FIndicators write SetIndicators default [tbiArrow, tbiLine];
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clBlack;

    property BorderStyle: TControlBorderStyle read FBorderStyle write SetBorderStyle;

    property ColorFrom: TColor read FColorFrom write SetColorFrom default clBlack;
    property ColorTo: TColor read FColorTo write SetColorTo default clBlue;
    property FillDirection: TFillDirection read FFillDirection write SetFillDirection default fdLeftToRight;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property OnPosChange: TNotifyEvent read FOnPosChanged write FOnPosChanged;
    property OnMinChange: TNotifyEvent read FOnMinChange write FOnMinChange;
    property OnMaxChange: TNotifyEvent read FOnMaxChange write FOnMaxChange;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragKind;
    property DragCursor;
    property OnCanResize;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property Hint;
    property ParentColor;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property Height default 24;
    property Width default 120;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
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

const
  ArrowOffset = 8;
  BitmapOffset = 4;

constructor TJvColorTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FBmpImage := TBitmap.Create;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FColorFrom := clBlack;
  FColorTo := clBlue;
  FArrowColor := clBlack;
  FBorderStyle := bsSingle;
  FIndicators := [tbiArrow, tbiLine];
  Height := 24;
  Width := 120;
  FFillDirection := fdLeftToRight;
  UpdateGradient;
end;

destructor TJvColorTrackBar.Destroy;
begin
  FBmpImage.Free;
  inherited Destroy;
end;

procedure TJvColorTrackBar.UpdateGradient;
var
  R: TRect;
begin
  if Parent = nil then
    Exit;
  FBmpImage.PixelFormat := pf24bit;
  if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
  begin
    FBmpImage.Width := Width - ArrowOffset;
    FBmpImage.Height := Height - BitmapOffset;
  end else
  begin
    FBmpImage.Width := Width - BitmapOffset;
    FBmpImage.Height := Height - ArrowOffset;
  end;
  R := Rect(0, 0, FBmpImage.Width, FBmpImage.Height);

  GradientFillRect(FBmpImage.Canvas, R, ColorFrom, ColorTo, FillDirection, 255);
  if BorderStyle = bsSingle then
    DrawEdge(FBmpImage.Canvas.Handle, R, EDGE_SUNKEN, BF_TOP or BF_RIGHT or BF_BOTTOM or BF_LEFT);
end;

procedure TJvColorTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FButtonDown := not ReadOnly;
    if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
      Position := WindowToPos(Y)
    else
      Position := WindowToPos(X);
  end;
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TJvColorTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FButtonDown then
  begin
    if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
      Position := WindowToPos(Y)
    else
      Position := WindowToPos(X);
  end;
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, X, Y);
end;

procedure TJvColorTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and FButtonDown then
  begin
    if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
      Position := WindowToPos(Y)
    else
      Position := WindowToPos(X);
  end;
  FButtonDown := False;
  if Assigned(OnMouseUp) then
    OnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TJvColorTrackBar.Paint;
var
  ArrowPosition: Integer;
  N: Integer;
  R: TRect;
  P: array [0..2] of TPoint;
  AHorizontalOffset, AVerticalOffset: Integer;
begin
  if Parent = nil then
    Exit;

  if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
  begin
    AVerticalOffset := BitmapOffset;
    AHorizontalOffset := ArrowOffset;
  end else
  begin
    AVerticalOffset := ArrowOffset;
    AHorizontalOffset := BitmapOffset;
  end;
  if (Width - AHorizontalOffset <> FBmpImage.Width) or (Height <> FBmpImage.Height - AVerticalOffset) then
      UpdateGradient;

  Canvas.Pen.Color := Color;
  Canvas.Brush.Color := Color;
  if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
    BitBlt(Canvas.Handle, ArrowOffset, BitmapOffset div 2, Width, Height, FBmpImage.Canvas.Handle, 0, 0, SrcCopy)
  else
    BitBlt(Canvas.Handle, BitmapOffset div 2, ArrowOffset, Width, Height, FBmpImage.Canvas.Handle, 0, 0, SrcCopy);
  if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
    R := Rect(0, 0, ArrowOffset, Height)
  else
    R := Rect(0, 0, Width, ArrowOffset);
  Canvas.FillRect(R);
  ArrowPosition := PosToWindow(Position);
  if tbiArrow in Indicators then
  begin
    Canvas.Pen.Color := ArrowColor;
    Canvas.Brush.Color := ArrowColor;
    if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
    begin
      P[0] := Point(0, ArrowPosition - 5);
      P[1] := Point(5, ArrowPosition);
      P[2] := Point(0, ArrowPosition + 5);
    end else
    begin
      P[0] := Point(ArrowPosition - 5, 0);
      P[1] := Point(ArrowPosition, 5);
      P[2] := Point(ArrowPosition + 5, 0);
    end;
    Canvas.Polygon(P);
  end;
  if tbiLine in Indicators then
    with Canvas do
    begin
      N := Ord(BorderStyle = bsSingle) * 2;
      if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
      begin
        Pen.Color := Pixels[ArrowOffset + 4, ArrowPosition] xor clWhite;
        MoveTo(ArrowOffset + N, ArrowPosition - 1);
        LineTo(Width - N, ArrowPosition - 1);
        MoveTo(ArrowOffset + N, ArrowPosition);
        LineTo(Width - N, ArrowPosition);
        MoveTo(ArrowOffset + N, ArrowPosition + 1);
        LineTo(Width - N, ArrowPosition + 1);
      end else
      begin
        Pen.Color := Pixels[ArrowPosition, ArrowOffset + 4] xor clWhite;
        MoveTo(ArrowPosition - 1, ArrowOffset + N);
        LineTo(ArrowPosition - 1, Height - N);
        MoveTo(ArrowPosition, ArrowOffset + N);
        LineTo(ArrowPosition, Height - N);
        MoveTo(ArrowPosition + 1, ArrowOffset + N);
        LineTo(ArrowPosition + 1, Height - N);
      end;
    end;
end;

procedure TJvColorTrackBar.Resize;
begin
  inherited Resize;
  UpdateGradient;
end;

procedure TJvColorTrackBar.SetMax(const Value: Integer);
begin
  if Value > Min then
  begin
    FMax := Value;
    if FMax < Position then
      Position := FMax;
    Invalidate;
    MaxChanged;
  end;
end;

procedure TJvColorTrackBar.SetMin(const Value: Integer);
begin
  if Value < Max then
  begin
    FMin := Value;
    if FMin > Position then
      Position := FMin;
    Invalidate;
    MinChanged;
  end;
end;

procedure TJvColorTrackBar.SetPosition(const Value: Integer);
begin
  if (Value >= Min) and (Value <= Max) and (Value <> FPosition) then
  begin
    FPosition := Value;
    Invalidate;
    Changed;
  end;
end;

procedure TJvColorTrackBar.Changed;
begin
  if Assigned(FOnPosChanged) then
    FOnPosChanged(Self);
end;

procedure TJvColorTrackBar.SetColorFrom(const Value: TColor);
begin
  if FColorFrom <> Value then
  begin
    FColorFrom := Value;
    UpdateGradient;
    Invalidate;
  end;
end;

procedure TJvColorTrackBar.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    UpdateGradient;
    Invalidate;
  end;
end;

function TJvColorTrackBar.WindowToPos(WindowCoord: Integer): Integer;
var
  MaxWindowCoord: Integer;
begin
  if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
    MaxWindowCoord := Height
  else
    MaxWindowCoord := Width;
  if (Max - Min > 0) and (MaxWindowCoord - BitmapOffset > 0) then
  begin
    Result := WindowCoord * (Max - Min) div (MaxWindowCoord - BitmapOffset);
    if (FillDirection=fdRightToLeft) or (FillDirection=fdBottomToTop) then
      Result := Max - Result
    else
      Result := Result + Min;
  end
  else
    Result := Min;
  if Result < Min then
    Result := Min;
  if Result > Max then
    Result := Max;
end;

function TJvColorTrackBar.PosToWindow(APos: Integer): Integer;
var
  MaxWindowCoord: Integer;
begin
  if (FillDirection=fdTopToBottom) or (FillDirection=fdBottomToTop) then
    MaxWindowCoord := Height
  else
    MaxWindowCoord := Width;
  if (Max - Min > 0) and (MaxWindowCoord > 0) then
    Result := MaxWindowCoord * (APos - Min) div (Max - Min)
  else
    Result := BitmapOffset;
  if Result < BitmapOffset * 2 then
    Result := BitmapOffset * 2;
  if Result > MaxWindowCoord - BitmapOffset * 2 then
    Result := MaxWindowCoord - BitmapOffset * 2;
  if (FillDirection=fdRightToLeft) or (FillDirection=fdBottomToTop) then
    Result := MaxWindowCoord - Result;
end;

procedure TJvColorTrackBar.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Invalidate;
  end;
end;

procedure TJvColorTrackBar.MaxChanged;
begin
  if Assigned(FOnMaxChange) then
    FOnMaxChange(Self);
end;

procedure TJvColorTrackBar.MinChanged;
begin
  if Assigned(FOnMinChange) then
    FOnMinChange(Self);
end;

procedure TJvColorTrackBar.SetBorderStyle(const Value: TControlBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TJvColorTrackBar.SetIndicators(const Value: TJvColorTrackBarIndicators);
begin
  if FIndicators <> Value then
  begin
    FIndicators := Value;
    Invalidate;
  end;
end;

procedure TJvColorTrackBar.SetFillDirection(const Value: TFillDirection);
begin
  if FFillDirection <> Value then
  begin
    FFillDirection := Value;
    UpdateGradient;
    Invalidate;
  end;
end;

{$IFDEF UNITVERSIONING}

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
