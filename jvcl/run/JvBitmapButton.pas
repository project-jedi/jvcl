{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBitmapButton.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBitmapButton;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Types, Classes, Graphics, Controls,
  JvComponent, JvTypes;

type
  PJvRGBTriple = ^TJvRGBTriple;

  TPixelTransform = procedure(Dest, Source: PJvRGBTriple);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvBitmapButton = class(TJvGraphicControl)
  private
    FBitmap: TBitmap;
    FLighter: TBitmap;
    FDarker: TBitmap;
    FNormal: TBitmap;
    FPushDown: Boolean;
    FMouseOver: Boolean;
    FLatching: Boolean;
    FDown: Boolean;
    FHotTrack: Boolean;
    FCaption: string;
    FFont: TFont;
    FCaptionLeft: Integer;
    FCaptionTop: Integer;
    FLighterFontColor: TColor;
    FDarkerFontColor: TColor;
    procedure SetBitmap(const Value: TBitmap);
    procedure MakeNormal;
    procedure MakeDarker;
    procedure MakeLighter;
    procedure MakeHelperBitmap(Target: TBitmap; Transform: TPixelTransform);
    procedure MakeCaption(Target: TBitmap; FontColor: TColor);
    procedure SetLatching(const Value: Boolean);
    procedure SetDown(const Value: Boolean);
    procedure SetHotTrack(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetCaptionLeft(const Value: Integer);
    procedure SetCaptionTop(const Value: Integer);
    procedure UpdateBitmaps;
    procedure SetDarkerFontColor(const Value: TColor);
    procedure SetLighterFontColor(const Value: TColor);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure Click; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure DoBitmapChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Caption: string read FCaption write SetCaption;
    property CaptionLeft: Integer read FCaptionLeft write SetCaptionLeft;
    property CaptionTop: Integer read FCaptionTop write SetCaptionTop;
    property DarkerFontColor: TColor read FDarkerFontColor write SetDarkerFontColor;
    property Down: Boolean read FDown write SetDown default False;
    property Font: TFont read FFont write SetFont;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property Height default 24;
    property Hint;
    property Latching: Boolean read FLatching write SetLatching default False;
    property LighterFontColor: TColor read FLighterFontColor write SetLighterFontColor;
    property ShowHint;
    property Width default 24;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property Visible;
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


constructor TJvBitmapButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 24;
  Height := 24;
  FPushDown := False;
  FMouseOver := False;
  FLatching := False;
  FHotTrack := True;
  FDown := False;
  FBitmap := TBitmap.Create;
  FBitmap.Width := 24;
  FBitmap.Height := 24;
  FBitmap.PixelFormat := pf24bit;
  FBitmap.Canvas.Brush.Color := clGray;
  FBitmap.Canvas.FillRect(Rect(1, 1, 23, 23));
  FBitmap.OnChange := DoBitmapChange;
  FLighter := TBitmap.Create;
  FDarker := TBitmap.Create;
  FNormal := TBitmap.Create;
  FFont := TFont.Create;
end;

destructor TJvBitmapButton.Destroy;
begin
  FBitmap.Free;
  FLighter.Free;
  FDarker.Free;
  FNormal.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TJvBitmapButton.Click;
begin
  if FPushDown then
    if Assigned(OnClick) then
      inherited Click;
end;

procedure TJvBitmapButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FPushDown := not FBitmap.Transparent and
    (FBitmap.Canvas.Pixels[X, Y] <> FBitmap.Canvas.Pixels[0, FBitmap.Height - 1]);
  Repaint;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvBitmapButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FPushDown := False;
  if Latching then
    FDown := not FDown
  else
    FDown := False;
  Repaint;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvBitmapButton.Paint;
begin
  inherited Paint;
  if Assigned(FBitmap) then
  begin
    if FPushDown then
      Canvas.Draw(1, 1, FDarker)
    else
    begin
      if Down then
        Canvas.Draw(1, 1, FDarker)
      else
      if FMouseOver and FHotTrack then
        Canvas.Draw(0, 0, FLighter)
      else
        Canvas.Draw(0, 0, FNormal);
    end;
  end;
end;

procedure TJvBitmapButton.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  FBitmap.Transparent := True;
end;

procedure TJvBitmapButton.UpdateBitmaps;
begin
  MakeLighter;
  MakeDarker;
  MakeNormal;
  Repaint;
end;

procedure LighterTransform(Dest, Source: PJvRGBTriple);
begin
  Dest.rgbBlue  := $FF - Round(0.8 * Abs($FF - Source.rgbBlue));
  Dest.rgbGreen := $FF - Round(0.8 * Abs($FF - Source.rgbGreen));
  Dest.rgbRed   := $FF - Round(0.8 * Abs($FF - Source.rgbRed));
end;

procedure DarkerTransform(Dest, Source: PJvRGBTriple);
begin
  Dest.rgbBlue  := Round(0.7 * Source.rgbBlue);
  Dest.rgbGreen := Round(0.7 * Source.rgbGreen);
  Dest.rgbRed   := Round(0.7 * Source.rgbRed);
end;

procedure TJvBitmapButton.MakeLighter;
begin
  MakeHelperBitmap(FLighter, LighterTransform);
  MakeCaption(FLighter, FLighterFontColor);
end;


procedure TJvBitmapButton.MakeDarker;
begin
  MakeHelperBitmap(FDarker, DarkerTransform);
  MakeCaption(FDarker, FDarkerFontColor);
end;

procedure TJvBitmapButton.MouseLeave(AControl: TControl);
begin
  FMouseOver := False;
  MakeDarker;
  MakeNormal;
  Repaint;
end;

procedure TJvBitmapButton.Loaded;
begin
  inherited Loaded;
  if not FBitmap.Empty then
  begin
    MakeDarker;
    MakeLighter;
    MakeNormal;
  end;
  Resize;
end;

procedure TJvBitmapButton.SetLatching(const Value: Boolean);
begin
  FLatching := Value;
  if not FLatching then
  begin
    FDown := False;
    Invalidate;
  end;
end;

procedure TJvBitmapButton.SetDown(const Value: Boolean);
begin
  if FLatching then
    FDown := Value
  else
    FDown := False;
  Invalidate;
end;

procedure TJvBitmapButton.Resize;
begin
  inherited Resize;
  if Assigned(FBitmap) then
  begin
    Width := FBitmap.Width;
    Height := FBitmap.Height;
  end
  else
  begin
    Width := 24;
    Height := 24;
  end;
end;

procedure TJvBitmapButton.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
end;

procedure TJvBitmapButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Value: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  Value := FBitmap.Canvas.Pixels[X, Y] <> FBitmap.Canvas.Pixels[0, FBitmap.Height - 1];
  if Value <> FMouseOver then
  begin
    FMouseOver := Value;
    Repaint;
  end;
end;

procedure TJvBitmapButton.SetCaption(const Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.SetFont(const Value: TFont);
begin
  if Value <> FFont then
  begin
    FFont := Value;
    Canvas.Font.Assign(FFont);
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.SetCaptionLeft(const Value: Integer);
begin
  if Value <> FCaptionLeft then
  begin
    FCaptionLeft := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.SetCaptionTop(const Value: Integer);
begin
  if Value <> FCaptionTop then
  begin
    FCaptionTop := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.MakeNormal;
begin
   FNormal.Assign(FBitmap);
   MakeCaption(FNormal, Font.Color);
end;

procedure TJvBitmapButton.SetDarkerFontColor(const Value: TColor);
begin
  if Value <> FDarkerFontColor then
  begin
    FDarkerFontColor := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.SetLighterFontColor(const Value: TColor);
begin
  if Value <> FLighterFontColor then
  begin
    FLighterFontColor := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.DoBitmapChange(Sender: TObject);
begin
  if FBitmap.PixelFormat <> pf24bit then
  begin
    FBitmap.OnChange := nil;
    try
      FBitmap.PixelFormat := pf24bit;
    finally
      FBitmap.OnChange := DoBitmapChange;
    end;
  end;
  Width := FBitmap.Width;
  Height := FBitmap.Height;
  UpdateBitmaps;
end;

procedure TJvBitmapButton.MakeCaption(Target: TBitmap; FontColor: TColor);
var
  R: TRect;
begin
  if FCaption <> '' then
    with Target.Canvas do
    begin
      Brush.Style := bsClear;
      Font.Assign(FFont);
      Font.Color := FontColor;
      R := Rect(0, 0, Width, Height);
      TextRect(R, FCaptionLeft, FCaptionTop, FCaption);
    end;
end;

procedure TJvBitmapButton.MakeHelperBitmap(Target: TBitmap; Transform: TPixelTransform);
var
  P1, P2: PJvRGBTriple;
  X, Y: Integer;
  RT, GT, BT: Byte;
  LColor: TColor;
begin
  Target.Width := FBitmap.Width;
  Target.Height := FBitmap.Height;
  Target.Transparent := FBitmap.Transparent;
  if FBitmap.Transparent then
  begin
    LColor := FBitmap.TransparentColor;
    Target.TransparentColor := LColor;
  end
  else
    LColor := clNone;
  RT := GetRValue(LColor);
  GT := GetGValue(LColor);
  BT := GetBValue(LColor);
  Target.PixelFormat := pf24bit;
  Assert(FBitmap.PixelFormat = pf24bit);
  for Y := 0 to FBitmap.Height - 1 do
  begin
    P1 := FBitmap.ScanLine[Y];
    P2 := Target.ScanLine[Y];
    for X := 1 to FBitmap.Width do
    begin
      if (LColor <> clNone) and
        (P1.rgbBlue = BT) and (P1.rgbGreen = GT) and (P1.rgbRed = RT) then
        P2^ := P1^
      else
        Transform(P2, P1);
      Inc(P1);
      Inc(P2);
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
