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
Portions created by Sébastien Buysse are CopyRight (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGradient;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Graphics, Controls,
  SysUtils, Classes,
  JvTypes, JvComponent;

type
  TJvGradient = class(TJvGraphicControl)
  private
    FStyle: TJvGradientStyle;
    FStartColor: TColor;
    FEndColor: TColor;
    FSteps: Word;
    FBuffer: TBitmap;
    FBufferWidth: Integer;
    FBufferHeight: Integer;
    procedure SetSteps(Value: Word);
    procedure SetStartColor(Value: TColor);
    procedure SetEndColor(Value: TColor);
    procedure SetStyle(Value: TJvGradientStyle);
  protected
    { Note: No need to respond to WM_ERASEBKGND; this is not a TWinControl }
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align default alClient;
    property ShowHint;
    property Visible;
    property ParentShowHint;
    property Enabled;
    property PopupMenu;
    property Style: TJvGradientStyle read FStyle write SetStyle default grHorizontal;
    property StartColor: TColor read FStartColor write SetStartColor default clBlue;
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    property Steps: Word read FSteps write SetSteps default 100;
  end;

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

procedure TJvGradient.Paint;
var
  I: Integer;
  J, K: Real;
  Deltas: array [0..2] of Real; // R,G,B
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
    //bt := TBitmap.Create;
    //bt.Width := Width;
    //bt.Height := Height;
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
    //FBuffer.Assign(bt);
    //bt.Free;
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

end.

