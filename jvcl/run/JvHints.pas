{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHints.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvHints;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Graphics, Controls, Forms, Classes,
  JvTypes;

type
  THintStyle = (hsRectangle, hsRoundRect, hsEllipse);
  THintPos = (hpTopRight, hpTopLeft, hpBottomRight, hpBottomLeft);
  THintShadowSize = 0..15;

  TJvHintWindow = class(THintWindow)
  private
    FSrcImage: TBitmap;
    FImage: TBitmap;
    FPos: THintPos;
    FRect: TRect;
    FTextRect: TRect;
    FTileSize: TPoint;
    FRoundFactor: Integer;
    {$IFDEF VCL}
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    {$ENDIF VCL}
    function CreateRegion(Shade: Boolean): HRGN;
    procedure FillRegion(Rgn: HRGN; Shade: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect;
      const AHint: THintString); override;
    procedure ActivateHintData(Rect: TRect;
      const AHint: THintString; AData: Pointer); override;
    function CalcHintRect(MaxWidth: Integer;
      const AHint: THintString; AData: Pointer): TRect;override;
  end;

procedure SetHintStyle(Style: THintStyle; ShadowSize: THintShadowSize;
  Tail: Boolean; Alignment: TAlignment);
procedure SetStandardHints;
procedure RegisterHintWindow(AClass: THintWindowClass);
function GetHintControl: TControl;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Math,
  JvJCLUtils;

var
  HintStyle: THintStyle = hsRectangle;
  HintShadowSize: Integer = 0;
  HintTail: Boolean = False;
  HintAlignment: TAlignment = taLeftJustify;

procedure RegisterHintWindow(AClass: THintWindowClass);
begin
  HintWindowClass := AClass;
  with Application do
    if ShowHint then
    begin
      ShowHint := False;
      ShowHint := True;
    end;
end;

procedure SetStandardHints;
begin
  RegisterHintWindow(THintWindow);
end;

procedure SetHintStyle(Style: THintStyle; ShadowSize: THintShadowSize;
  Tail: Boolean; Alignment: TAlignment);
begin
  HintStyle := Style;
  HintShadowSize := ShadowSize;
  HintTail := Tail;
  HintAlignment := Alignment;
  RegisterHintWindow(TJvHintWindow);
end;

function GetHintControl: TControl;
var
  CursorPos: TPoint;
begin
  GetCursorPos(CursorPos);
  Result := FindDragTarget(CursorPos, True);
  while (Result <> nil) and not Result.ShowHint do
    Result := Result.Parent;
  if (Result <> nil) and (csDesigning in Result.ComponentState) then
    Result := nil;
end;

{$IFDEF VCL}
procedure StandardHintFont(AFont: TFont);
var
  NonClientMetrics: TNonClientMetrics;
begin
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    AFont.Handle := CreateFontIndirect(NonClientMetrics.lfStatusFont)
  else
  begin
    AFont.Name := 'MS Sans Serif';
    AFont.Size := 8;
  end;
  AFont.Color := clInfoText;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure StandardHintFont(AFont: TFont);
begin
  AFont.Name := 'Helvetica';
  AFont.Height := 11;
  AFont.Color := clInfoText;
end;
{$ENDIF VisualCLX}

constructor TJvHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StandardHintFont(Canvas.Font);
  FImage := TBitmap.Create;
  FSrcImage := TBitmap.Create;
end;

destructor TJvHintWindow.Destroy;
begin
  FSrcImage.Free;
  FImage.Free;
  inherited Destroy;
end;

{$IFDEF VCL}

procedure TJvHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_BORDER;
end;

procedure TJvHintWindow.WMNCPaint(var Msg: TMessage);
begin
end;

procedure TJvHintWindow.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

{$ENDIF VCL}

function TJvHintWindow.CreateRegion(Shade: Boolean): HRGN;
var
  R: TRect;
  W, TileOffs: Integer;
  Tail, Dest: HRGN;
  P: TPoint;

  function CreatePolyRgn(const Points: array of TPoint): HRGN;
  begin
    Result := CreatePolygonRgn(Points[0], High(Points) + 1, WINDING);
  end;

begin
  R := FRect;
  Result := 0;
  if Shade then
    OffsetRect(R, HintShadowSize, HintShadowSize);
  case HintStyle of
    hsRoundRect:
      Result := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom,
        FRoundFactor, FRoundFactor);
    hsEllipse:
      Result := CreateEllipticRgnIndirect(R);
    hsRectangle:
      Result := CreateRectRgnIndirect(R);
  end;
  if HintTail then
  begin
    R := FTextRect;
    GetCursorPos(P);
    TileOffs := 0;
    if FPos in [hpTopLeft, hpBottomLeft] then
      TileOffs := Width;
    if Shade then
    begin
      OffsetRect(R, HintShadowSize, HintShadowSize);
      Inc(TileOffs, HintShadowSize);
    end;
    W := Min(Max(8, Min(RectWidth(R), RectHeight(R)) div 4), RectWidth(R) div 2);
    case FPos of
      hpTopRight:
        Tail := CreatePolyRgn([Point(TileOffs, Height - HintShadowSize),
          Point(R.Left + W div 4, R.Bottom), Point(R.Left + 2 * W, R.Bottom)]);
      hpTopLeft:
        Tail := CreatePolyRgn([Point(TileOffs, Height - HintShadowSize),
          Point(R.Right - W div 4, R.Bottom), Point(R.Right - 2 * W, R.Bottom)]);
      hpBottomRight:
        Tail := CreatePolyRgn([Point(TileOffs, 0),
          Point(R.Left + W div 4, R.Top), Point(R.Left + 2 * W, R.Top)]);
    else
      Tail := CreatePolyRgn([Point(TileOffs, 0),
        Point(R.Right - W div 4, R.Top), Point(R.Right - 2 * W, R.Top)]);
    end;
    try
      Dest := Result;
      Result := CreateRectRgnIndirect(R);
      try
        CombineRgn(Result, Dest, Tail, RGN_OR);
      finally
        if Dest <> 0 then
          DeleteObject(Dest);
      end;
    finally
      DeleteObject(Tail);
    end;
  end;
end;

procedure TJvHintWindow.FillRegion(Rgn: HRGN; Shade: Boolean);
begin
  if Shade then
  begin
    FImage.Canvas.Brush.Bitmap :=
      AllocPatternBitmap(clBtnFace, clWindowText);
    FImage.Canvas.Pen.Style := psClear;
  end
  else
  begin
    FImage.Canvas.Pen.Style := psSolid;
    FImage.Canvas.Brush.Color := Color;
  end;
  try
    PaintRgn(FImage.Canvas.Handle, Rgn);
    if not Shade then
    begin
      FImage.Canvas.Brush.Color := Font.Color;
      if (HintStyle = hsRectangle) and not HintTail then
        DrawEdge(FImage.Canvas.Handle, FRect, BDR_RAISEDOUTER, BF_RECT)
      else
        FrameRgn(FImage.Canvas.Handle, Rgn, FImage.Canvas.Brush.Handle, 1, 1);
    end;
  finally
    if Shade then
    begin
      FImage.Canvas.Brush.Bitmap := nil;
      FImage.Canvas.Pen.Style := psSolid;
    end;
    FImage.Canvas.Brush.Color := Color;
  end;
end;

procedure TJvHintWindow.Paint;
var
  R: TRect;
  FShadeRgn, FRgn: HRGN;

  procedure PaintText(R: TRect);
  const
    Flag: array [TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  begin
    DrawText(FImage.Canvas, Caption, -1, R,
      DT_NOPREFIX or DT_WORDBREAK or Flag[HintAlignment]);
  end;

begin
  R := ClientRect;
  FImage.Handle := CreateCompatibleBitmap(Canvas.Handle,
    RectWidth(ClientRect), RectHeight(ClientRect));
  FImage.Canvas.Font := Self.Canvas.Font;
  if (HintStyle <> hsRectangle) or (HintShadowSize > 0) or HintTail then
    FImage.Canvas.Draw(0, 0, FSrcImage);
  FRgn := CreateRegion(False);
  FShadeRgn := CreateRegion(True);
  try
    FillRegion(FShadeRgn, True);
    FillRegion(FRgn, False);
  finally
    DeleteObject(FShadeRgn);
    DeleteObject(FRgn);
  end;
  R := FTextRect;
  if HintAlignment = taLeftJustify then
    Inc(R.Left, 2);
  PaintText(R);
  Canvas.Draw(0, 0, FImage);
end;

procedure TJvHintWindow.ActivateHint(Rect: TRect;
  const AHint: THintString);
var
  R: TRect;
  ScreenDC: HDC;
  P: TPoint;
begin
  Caption := AHint;
  GetCursorPos(P);
  FPos := hpBottomRight;
  R := CalcHintRect(Screen.Width, AHint, nil);
  OffsetRect(R, Rect.Left - R.Left, Rect.Top - R.Top);
  Rect := R;
  BoundsRect := Rect;

  if HintTail then
  begin
    Rect.Top := P.Y - Height - 3;
    if Rect.Top < 0 then
      Rect.Top := BoundsRect.Top
    else
      Rect.Bottom := Rect.Top + RectHeight(BoundsRect);

    Rect.Left := P.X + 1;
    if Rect.Left < 0 then
      Rect.Left := BoundsRect.Left
    else
      Rect.Right := Rect.Left + RectWidth(BoundsRect);
  end;

  if Rect.Top + Height > Screen.Height then
  begin
    Rect.Top := Screen.Height - Height;
    if Rect.Top <= P.Y then
      Rect.Top := P.Y - Height - 3;
  end;
  if Rect.Left + Width > Screen.Width then
  begin
    Rect.Left := Screen.Width - Width;
    if Rect.Left <= P.X then
      Rect.Left := P.X - Width - 3;
  end;
  if Rect.Left < 0 then
  begin
    Rect.Left := 0;
    if Rect.Left + Width >= P.X then
      Rect.Left := P.X - Width - 1;
  end;
  if Rect.Top < 0 then
  begin
    Rect.Top := 0;
    if Rect.Top + Height >= P.Y then
      Rect.Top := P.Y - Height - 1;
  end;

  if (HintStyle <> hsRectangle) or (HintShadowSize > 0) or HintTail then
  begin
    FPos := hpBottomRight;
    if Rect.Top + Height < P.Y then
      FPos := hpTopRight;
    if Rect.Left + Width < P.X then
    begin
      if FPos = hpBottomRight then
        FPos := hpBottomLeft
      else
        FPos := hpTopLeft;
    end;
    if HintTail then
    begin
      if FPos in [hpBottomRight, hpBottomLeft] then
      begin
        OffsetRect(FRect, 0, FTileSize.Y);
        OffsetRect(FTextRect, 0, FTileSize.Y);
      end;
      if FPos in [hpBottomRight, hpTopRight] then
      begin
        OffsetRect(FRect, FTileSize.X, 0);
        OffsetRect(FTextRect, FTileSize.X, 0);
      end;
    end;
    if HandleAllocated then
    begin
      SetWindowPos(Handle, HWND_BOTTOM, 0, 0, 0, 0,
        SWP_HIDEWINDOW or SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE);
      if Screen.ActiveForm <> nil then
        UpdateWindow(Screen.ActiveForm.Handle);
    end;
    ScreenDC := GetDC(0);
    try
      with FSrcImage do
      begin
        Width := RectWidth(BoundsRect);
        Height := RectHeight(BoundsRect);
        BitBlt(Canvas.Handle, 0, 0, Width, Height, ScreenDC,
          Rect.Left, Rect.Top, SRCCOPY);
      end;
    finally
      ReleaseDC(0, ScreenDC);
    end;
  end;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, 0, 0,
    SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
end;

function TJvHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: THintString;
  AData: Pointer): TRect;
const
  Flag: array [TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  A: Integer;
  X, Y, Factor: Double;
begin
  Result := Rect(0, 0, MaxWidth, 0);
  DrawText(Canvas, AHint, -1, Result,
    DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX or Flag[HintAlignment] or {$IFDEF VCL} DrawTextBiDiModeFlagsReadingOnly {$ENDIF});
  Inc(Result.Right, 8);
  Inc(Result.Bottom, 4);
  FRect := Result;
  FTextRect := Result;
  InflateRect(FTextRect, -1, -1);
  case HintAlignment of
    taCenter:
      OffsetRect(FTextRect, -1, 0);
    taRightJustify:
      OffsetRect(FTextRect, -4, 0);
  end;
  FRoundFactor := Max(6, Min(RectWidth(Result), RectHeight(Result)) div 4);
  if HintStyle = hsRoundRect then
    InflateRect(FRect, FRoundFactor div 4, FRoundFactor div 4)
  else
  if HintStyle = hsEllipse then
  begin
    X := RectWidth(FRect) / 2;
    Y := RectHeight(FRect) / 2;
    if (X <> 0) and (Y <> 0) then
    begin
      Factor := Round(Y / 3);
      A := Round(Sqrt((Sqr(X) * Sqr(Y + Factor)) / (Sqr(Y + Factor) - Sqr(Y))));
      InflateRect(FRect, A - Round(X), Round(Factor));
    end;
  end;
  Result := FRect;
  OffsetRect(FRect, -Result.Left, -Result.Top);
  OffsetRect(FTextRect, -Result.Left, -Result.Top);
  Inc(Result.Right, HintShadowSize);
  Inc(Result.Bottom, HintShadowSize);
  if HintTail then
  begin
    FTileSize.Y := Max(14, Min(RectWidth(FTextRect), RectHeight(FTextRect)) div 2);
    FTileSize.X := FTileSize.Y - 8;
    Inc(Result.Right, FTileSize.X);
    Inc(Result.Bottom, FTileSize.Y);
  end;
end;

procedure TJvHintWindow.ActivateHintData(Rect: TRect;
  const AHint: THintString; AData: Pointer);
begin
  ActivateHint(Rect, AHint);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

