{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBitmapButton.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2003-10-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQBitmapButton;

interface

uses
  
  
  Types, QWindows, QGraphics, QControls, QForms, QDialogs,
  
  SysUtils, Classes,
  JvQComponent, JvQTypes;

type
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
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
  end;

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
  FBitmap.Canvas.Brush.Color := clGray;
  FBitmap.Canvas.FillRect(Rect(1, 1, 23, 23));
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
  FPushDown :=
    FBitmap.Canvas.Pixels[X, Y] <> FBitmap.Canvas.Pixels[0, FBitmap.Height-1];
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
var
  AColor: TColor;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    AColor := FBitmap.Canvas.Pixels[0, FBitmap.Height - 1];
    FBitmap.Transparent := True;
    FBitmap.TransparentColor := AColor;
    FLighter.Transparent := True;
    Flighter.TransparentColor := AColor;
    FDarker.Transparent := True;
    FDarker.TransparentColor := AColor;
    FNormal.Transparent := True;
    FNormal.TransparentColor := AColor;
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
  FBitmap.TransparentColor := FBitmap.Canvas.Pixels[0, FBitmap.Height - 1];
  Width := FBitmap.Width;
  Height := FBitmap.Height;
  UpdateBitmaps;
end;

procedure TJvBitmapButton.UpdateBitmaps;
begin
  MakeLighter;
  MakeDarker;
  MakeNormal;
  Repaint;
end;

procedure TJvBitmapButton.MakeLighter;
var
  p1, p2: PJvRGBArray;
  X, Y: Integer;
  rt, gt, bt: Byte;
  AColor: TColor;
  ARect: TRect;
begin
  FLighter.Width := FBitmap.Width;
  FLighter.Height := FBitmap.Height;
  AColor := ColorToRGB(FBitmap.Canvas.Pixels[0, FBitmap.Height - 1]);
  rt := GetRValue(AColor);
  gt := GetGValue(AColor);
  bt := GetBValue(AColor);
  FBitmap.PixelFormat := pf24bit;
  FLighter.PixelFormat := pf24bit;
  for Y := 0 to FBitmap.Height - 1 do
  begin
    p1 := FBitmap.ScanLine[Y];
    p2 := FLighter.ScanLine[Y];
    for X := 0 to FBitmap.Width - 1 do
      if (p1[X].rgbBlue = bt) and (p1[X].rgbGreen = gt) and (p1[X].rgbRed = rt) then
        p2[X] := p1[X]
      else
      begin
        p2[X].rgbBlue  := $FF - Round(0.8 * Abs($FF - p1[X].rgbBlue));
        p2[X].rgbGreen := $FF - Round(0.8 * Abs($FF - p1[X].rgbGreen));
        p2[X].rgbRed   := $FF - Round(0.8 * Abs($FF - p1[X].rgbRed));
      end;
  end;
  if FCaption <> '' then
  begin
    Flighter.Canvas.Brush.Style := bsClear;
    Flighter.Canvas.Font.Assign(FFont);
    FLighter.Canvas.Font.Color := FLighterFontColor;
    ARect := Rect(0, 0, Width, Height);
    FLighter.Canvas.TextRect(ARect, FCaptionLeft, FCaptionTop, FCaption);
  end;
end;

procedure TJvBitmapButton.MakeDarker;
var
  p1, p2: PJvRGBArray;
  X, Y: Integer;
  rt, gt, bt: Byte;
  AColor: TColor;
  ARect: TRect;
begin
  FDarker.Width := FBitmap.Width;
  FDarker.Height := FBitmap.Height;
  AColor := ColorToRGB(FBitmap.Canvas.Pixels[0, FBitmap.Height - 1]);
  rt := GetRValue(AColor);
  gt := GetGValue(AColor);
  bt := GetBValue(AColor);
  FBitmap.PixelFormat := pf24bit;
  FDarker.PixelFormat := pf24bit;
  for Y := 0 to FBitmap.Height - 1 do
  begin
    p1 := FBitmap.ScanLine[Y];
    p2 := FDarker.ScanLine[Y];
    for X := 0 to FBitmap.Width - 1 do
    begin
      if (p1[X].rgbBlue = bt) and (p1[X].rgbGreen = gt) and (p1[X].rgbRed = rt) then
        p2[X] := p1[X]
      else
      begin
        p2[X].rgbBlue  := Round(0.7 * p1[X].rgbBlue);
        p2[X].rgbGreen := Round(0.7 * p1[X].rgbGreen);
        p2[X].rgbRed   := Round(0.7 * p1[X].rgbRed);
      end
    end;
  end;
  if FCaption <> '' then
  begin
    FDarker.Canvas.Brush.Style := bsClear;
    FDarker.Canvas.Font.Assign(FFont);
    FDarker.Canvas.Font.Color := FDarkerFontColor;
    ARect := Rect(0, 0, Width, Height);
    FDarker.Canvas.TextRect(ARect, FCaptionLeft, FCaptionTop, FCaption);
  end;
end;

procedure TJvBitmapButton.MouseLeave(AControl: TControl);
begin
  FMouseOver := False;
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
var
  ARect: TRect;
begin
  FNormal.Assign(FBitmap);
  if FCaption <> '' then
  begin
    FNormal.Canvas.Brush.Style := bsclear;
    FNormal.Canvas.Font.Assign(FFont);
    ARect := Rect(0, 0, Width, Height);
    FNormal.Canvas.TextRect(ARect, FCaptionLeft, FCaptionTop, FCaption);
  end;
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

end.

