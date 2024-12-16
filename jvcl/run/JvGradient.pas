{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing Rights and limitations under the License.

The Original Code is: JvGradient.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGradient;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Graphics, Controls,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}  
  SysUtils, Classes,
  JvTypes, JvComponent;

type
  TJvGradientPaintEvent = procedure(Sender: TObject; Canvas: TCanvas) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvGradient = class(TJvGraphicControl)
  private
    FStyle: TJvGradientStyle;
    FStartColor: TColor;
    FEndColor: TColor;
    FSteps: Word;
    FBuffer: TBitmap;
    FBufferWidth: Integer;
    FBufferHeight: Integer;
    FLoadedLeft: Integer;
    FLoadedTop: Integer;
    FLoadedWidth: Integer;
    FLoadedHeight: Integer;
    FOnPaint: TJvGradientPaintEvent;
    procedure SetSteps(Value: Word);
    procedure SetStartColor(Value: TColor);
    procedure SetEndColor(Value: TColor);
    procedure SetStyle(Value: TJvGradientStyle);
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
  protected
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align default alClient;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property ShowHint;
    property Visible;
    property ParentShowHint;
    property Enabled;
    property PopupMenu;
    property Style: TJvGradientStyle read FStyle write SetStyle default grHorizontal;
    property StartColor: TColor read FStartColor write SetStartColor default clBlue;
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    property Steps: Word read FSteps write SetSteps default 100;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TJvGradientPaintEvent read FOnPaint write FOnPaint;
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


constructor TJvGradient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FBufferWidth := 0;
  FBufferHeight := 0;
  FSteps := 100;
  FBuffer := TBitmap.Create;
  FStyle := grHorizontal;
  FEndColor := clBlack;
  FStartColor := clBlue;
  Align := alClient;
end;

destructor TJvGradient.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

function TJvGradient.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TJvGradient.GetLeft: Integer;
begin
  Result := inherited Left;
end;

function TJvGradient.GetTop: Integer;
begin
  Result := inherited Top;
end;

function TJvGradient.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TJvGradient.Loaded;
begin
  inherited Loaded;
  if not (Align in [alLeft, alTop, alRight, alBottom]) then
  begin
    inherited Left := FLoadedLeft;
    inherited Top := FLoadedTop;
  end;
  if Align <> alClient then
  begin
    inherited Width := FLoadedWidth;
    inherited Height := FLoadedHeight;
  end;
end;

procedure TJvGradient.Paint;
var
  I: Integer;
  J, K: Real;
  Deltas: array [0..2] of Double; // R,G,B
  R: TRect;
  LStartRGB, LEndRGB: TColor;
  LSteps: Word;
begin
  if csDestroying in ComponentState then
    Exit;
  if (FBufferWidth <> Width) or (FBufferHeight <> Height) then
  begin
    LSteps := FSteps;
    LStartRGB := ColorToRGB(FStartColor);
    LEndRGB := ColorToRGB(FEndColor);

    FBufferWidth := Width;
    FBufferHeight := Height;
    if (FBufferWidth = 0) or (FBufferHeight = 0) then
      Exit;

    FBuffer.Width := FBufferWidth;
    FBuffer.Height := FBufferHeight;
    case FStyle of
      grFilled:
        begin
          FBuffer.Canvas.Brush.Color := LStartRGB;
          FBuffer.Canvas.Brush.Style := bsSolid;
          FBuffer.Canvas.FillRect(Rect(0, 0, Width, Height));
        end;
      grEllipse:
        begin
          FBuffer.Canvas.Brush.Color := LStartRGB;
          FBuffer.Canvas.Brush.Style := bsSolid;
          FBuffer.Canvas.FillRect(Rect(0, 0, Width, Height));
          if LSteps > (Width div 2) then
            LSteps := Trunc(Width / 2);
          if LSteps > (Height div 2) then
            LSteps := Trunc(Height / 2);
          if LSteps < 1 then
            LSteps := 1;
          Deltas[0] := (GetRValue(LEndRGB) - GetRValue(LStartRGB)) / LSteps;
          Deltas[1] := (GetGValue(LEndRGB) - GetGValue(LStartRGB)) / LSteps;
          Deltas[2] := (GetBValue(LEndRGB) - GetBValue(LStartRGB)) / LSteps;
          FBuffer.Canvas.Brush.Style := bsSolid;
          J := (Width / LSteps) / 2;
          K := (Height / LSteps) / 2;
          for I := 0 to LSteps do
          begin
            R.Top := Round(I * K);
            R.Bottom := Height - R.Top;
            R.Right := Round(I * J);
            R.Left := Width - R.Right;
            FBuffer.Canvas.Brush.Color := RGB(
              Round(GetRValue(LStartRGB) + I * Deltas[0]),
              Round(GetGValue(LStartRGB) + I * Deltas[1]),
              Round(GetBValue(LStartRGB) + I * Deltas[2]));
            FBuffer.Canvas.Pen.Color := FBuffer.Canvas.Brush.Color;
            FBuffer.Canvas.Ellipse(R.Right, R.Top, R.Left, R.Bottom);
          end;
        end;
      grHorizontal:
        begin
          if LSteps > Width then
            LSteps := Width;
          if LSteps < 1 then
            LSteps := 1;
          Deltas[0] := (GetRValue(LEndRGB) - GetRValue(LStartRGB)) / LSteps;
          Deltas[1] := (GetGValue(LEndRGB) - GetGValue(LStartRGB)) / LSteps;
          Deltas[2] := (GetBValue(LEndRGB) - GetBValue(LStartRGB)) / LSteps;
          FBuffer.Canvas.Brush.Style := bsSolid;
          J := Width / LSteps;
          for I := 0 to LSteps do
          begin
            R.Top := 0;
            R.Bottom := Height;
            R.Left := Round(I * J);
            R.Right := Round((I + 1) * J);
            FBuffer.Canvas.Brush.Color := RGB(
              Round(GetRValue(LStartRGB) + I * Deltas[0]),
              Round(GetGValue(LStartRGB) + I * Deltas[1]),
              Round(GetBValue(LStartRGB) + I * Deltas[2]));
            FBuffer.Canvas.FillRect(R);
          end;
        end;
      grVertical:
        begin
          if LSteps > Height then
            LSteps := Height;
          if LSteps < 1 then
            LSteps := 1;
          Deltas[0] := (GetRValue(LEndRGB) - GetRValue(LStartRGB)) / LSteps;
          Deltas[1] := (GetGValue(LEndRGB) - GetGValue(LStartRGB)) / LSteps;
          Deltas[2] := (GetBValue(LEndRGB) - GetBValue(LStartRGB)) / LSteps;
          FBuffer.Canvas.Brush.Style := bsSolid;
          J := Height / LSteps;
          for I := 0 to LSteps do
          begin
            R.Left := Width;
            R.Right := 0;
            R.Top := Round(I * J);
            R.Bottom := Round((I + 1) * J);
            FBuffer.Canvas.Brush.Color := RGB(
              Round(GetRValue(LStartRGB) + I * Deltas[0]),
              Round(GetGValue(LStartRGB) + I * Deltas[1]),
              Round(GetBValue(LStartRGB) + I * Deltas[2]));
            FBuffer.Canvas.FillRect(R);
          end;
        end;
      grMount:
        begin
          FBuffer.Canvas.Brush.Color := LStartRGB;
          FBuffer.Canvas.Brush.Style := bsSolid;
          FBuffer.Canvas.FillRect(Rect(0, 0, Width, Height));
          if LSteps > (Width div 2) then
            LSteps := Trunc(Width / 2);
          if LSteps > (Height div 2) then
            LSteps := Trunc(Height / 2);
          if LSteps < 1 then
            LSteps := 1;
          Deltas[0] := (GetRValue(LEndRGB) - GetRValue(LStartRGB)) / LSteps;
          Deltas[1] := (GetGValue(LEndRGB) - GetGValue(LStartRGB)) / LSteps;
          Deltas[2] := (GetBValue(LEndRGB) - GetBValue(LStartRGB)) / LSteps;
          FBuffer.Canvas.Brush.Style := bsSolid;
          J := (Width / LSteps) / 2;
          K := (Height / LSteps) / 2;
          for I := 0 to LSteps do
          begin
            R.Top := Round(I * K);
            R.Bottom := Height - R.Top;
            R.Right := Round(I * J);
            R.Left := Width - R.Right;
            FBuffer.Canvas.Brush.Color := RGB(
              Round(GetRValue(LStartRGB) + I * Deltas[0]),
              Round(GetGValue(LStartRGB) + I * Deltas[1]),
              Round(GetBValue(LStartRGB) + I * Deltas[2]));
            FBuffer.Canvas.Pen.Color := FBuffer.Canvas.Brush.Color;
            FBuffer.Canvas.RoundRect(R.Right, R.Top, R.Left, R.Bottom,
              ((R.Left - R.Right) div 2), ((R.Bottom - R.Top) div 2));
          end;
        end;
      grPyramid:
        begin
          FBuffer.Canvas.Brush.Color := LStartRGB;
          FBuffer.Canvas.Brush.Style := bsSolid;
          FBuffer.Canvas.FillRect(Rect(0, 0, Width, Height));
          if LSteps > (Width div 2) then
            LSteps := Trunc(Width / 2);
          if LSteps > (Height div 2) then
            LSteps := Trunc(Height / 2);
          if LSteps < 1 then
            LSteps := 1;
          Deltas[0] := (GetRValue(LEndRGB) - GetRValue(LStartRGB)) / LSteps;
          Deltas[1] := (GetGValue(LEndRGB) - GetGValue(LStartRGB)) / LSteps;
          Deltas[2] := (GetBValue(LEndRGB) - GetBValue(LStartRGB)) / LSteps;
          FBuffer.Canvas.Brush.Style := bsSolid;
          J := (Width / LSteps) / 2;
          K := (Height / LSteps) / 2;
          for I := 0 to LSteps do
          begin
            R.Top := Round(I * K);
            R.Bottom := Height - R.Top;
            R.Right := Round(I * J);
            R.Left := Width - R.Right;
            FBuffer.Canvas.Brush.Color := RGB(
              Round(GetRValue(LStartRGB) + I * Deltas[0]),
              Round(GetGValue(LStartRGB) + I * Deltas[1]),
              Round(GetBValue(LStartRGB) + I * Deltas[2]));
            FBuffer.Canvas.Pen.Color := FBuffer.Canvas.Brush.Color;
            FBuffer.Canvas.FillRect(Rect(R.Right, R.Top, R.Left, R.Bottom));
          end;
        end;
    end;
    if Assigned(FOnPaint) then
      FOnPaint(Self, FBuffer.Canvas);
  end;
  Canvas.Draw(0, 0, FBuffer);
end;

procedure TJvGradient.SetStyle(Value: TJvGradientStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    FBufferWidth := 0;
    Invalidate;
  end;
end;

procedure TJvGradient.SetTop(const Value: Integer);
begin
  FLoadedTop := Value;
  inherited Top := Value;
end;

procedure TJvGradient.SetWidth(const Value: Integer);
begin
  FLoadedWidth := Value;
  inherited Width := Value;
end;

procedure TJvGradient.SetStartColor(Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    FBufferWidth := 0;
    Invalidate;
  end;
end;

procedure TJvGradient.SetSteps(Value: Word);
begin
  if FSteps <> Value then
  begin
    FSteps := Value;
    FBufferWidth := 0;
    Invalidate;
  end;
end;

procedure TJvGradient.SetEndColor(Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    FBufferWidth := 0;
    Invalidate;
  end;
end;

procedure TJvGradient.SetHeight(const Value: Integer);
begin
  FLoadedHeight := Value;
  inherited Height := Value;
end;

procedure TJvGradient.SetLeft(const Value: Integer);
begin
  FLoadedLeft := Value;
  inherited Left := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
