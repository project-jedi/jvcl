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
unit JvBitmapButton;

interface

uses
  {$IFDEF VCL}
  Windows, Messages,  Graphics, Controls, Forms, Dialogs,
  {$ENDIF}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, Types, JvTypes,
  {$ENDIF}
  SysUtils, Classes;

type
  TJvBitmapButton = class(TGraphicControl)
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
    procedure SeTFont(const Value: TFont);
    procedure SetCaptionLeft(const Value: Integer);
    procedure SetCaptionTop(const Value: Integer);
    procedure UpDateBitmaps;
    procedure SetDarkerFontColor(const Value: TColor);
    procedure SetLighterFontColor(const Value: TColor);
    { Private declarations }
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    {$IFDEF VCL}
    procedure CMMouseLeave(var Message: TMessage); message CM_MouseLeave;
    {$ENDIF}
    {$IFDEF VisualCLX}
    procedure MouseLeave(AControl: TControl); override;
    {$ENDIF}
    procedure Loaded; override;
    procedure Resize; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    { Published declarations }
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Down: Boolean read FDown write SetDown;
    property Latching: Boolean read FLatching write SetLatching;
    property HotTrack: Boolean read FHotTrack write SetHotTrack;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property Hint;
    property ShowHint;
    property Caption: string read FCaption write SetCaption;
    property CaptionLeft: Integer read FCaptionLeft write SetCaptionLeft;
    property CaptionTop: Integer read FCaptionTop write SetCaptionTop;
    property Font: TFont read FFont write SeTFont;
    property DarkerFontColor: TColor read FDarkerFontColor write SetDarkerFontColor;
    property LighterFontColor: TColor read FLighterFontColor write SetLighterFontColor;
  end;

implementation

{ TJvBitmapButton }

procedure TJvBitmapButton.Click;
begin
  if FPushDown then
    if Assigned(OnClick) then
      OnClick(Self);
end;

constructor TJvBitmapButton.Create(AOwner: TComponent);
begin
  inherited;
  Width := 24;
  Height := 24;
  FPushDown := False;
  FMouseOver := False;
  FLatching := False;
  FHotTrack := True;
  FDown := False;
  FBitmap := TBitmap.create;
  FBitmap.Width := 24;
  FBitmap.Height := 24;
  FBitmap.Canvas.Brush.Color := clgray;
  FBitmap.Canvas.FillRect(Rect(1, 1, 23, 23));
  FLighter := TBitmap.create;
  FDarker := TBitmap.create;
  FNormal := TBitmap.create;
  FFont := TFont.Create;
end;

destructor TJvBitmapButton.Destroy;
begin
  FBitmap.free;
  FLighter.free;
  FDarker.free;
  FNormal.Free;
  FFont.free;
  inherited;
end;

procedure TJvBitmapButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FBitmap.Canvas.Pixels[x, y] <> FBitmap.Canvas.Pixels[0, FBitmap.Height - 1] then
    FPushDown := True
  else
    FPushDown := False;
  Paint;
  if Assigned(onmousedown) then
    onmousedown(Self, button, Shift, x, y);
end;

procedure TJvBitmapButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FPushDown := False;
  if Latching then
    FDown := not FDown
  else
    FDown := False;
  Paint;
  if Assigned(onmouseup) then
    onmouseup(Self, button, Shift, x, y);
end;

procedure TJvBitmapButton.Paint;
var
  AColor: TColor;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    AColor := FBitmap.Canvas.Pixels[0, FBitmap.Height - 1];
    FBitmap.transparent := True;
    FBitmap.transparentColor := AColor;
    FLighter.transparent := True;
    Flighter.TransparentColor := AColor;
    FDarker.transparent := True;
    FDarker.TransparentColor := AColor;
    FNormal.transparent := True;
    FNormal.TransparentColor := AColor;
    if FPushdown then
    begin
      Canvas.Draw(1, 1, FDarker)
    end
    else
    begin
      if Down then
        Canvas.Draw(1, 1, FDarker)
      else if (FMouseOver and FHotTrack) then
        Canvas.Draw(0, 0, FLighter)
      else
        Canvas.Draw(0, 0, FNormal);
    end;
  end;
end;

procedure TJvBitmapButton.SetBitmap(const Value: TBitmap);
begin
  FBitmap.assign(Value);
  FBitmap.transparent := True;
  FBitmap.TransparentColor := FBitmap.Canvas.Pixels[0, FBitmap.Height - 1];
  Width := FBitmap.Width;
  Height := FBitmap.Height;
  Updatebitmaps;
end;

procedure TJvBitmapButton.UpDateBitmaps;
begin
  MakeLighter;
  MakeDarker;
  MakeNormal;
  Paint;
end;

procedure TJvBitmapButton.MakeLighter;
var
  p1, p2: PByteArray;
  x, y: Integer;
  rt, gt, bt: Byte;

  AColor: TColor;
  ARect: TRect;
begin
  FLighter.Width := FBitmap.Width;
  FLighter.Height := FBitmap.Height;
  AColor := ColorToRGB(FBitmap.Canvas.Pixels[0, FBitmap.Height - 1]);
  rt := GetRValue(AColor);
  gt := GetGValue(AColor);
  bt := getBValue(AColor);
  FBitmap.PixelFormat := pf24bit;
  FLighter.PixelFormat := pf24bit;
  for y := 0 to FBitmap.Height - 1 do
  begin
    p1 := FBitmap.ScanLine[y];
    p2 := FLighter.ScanLine[y];
    for x := 0 to FBitmap.Width - 1 do
    begin
      if (p1[x * 3] = bt) and (p1[x * 3 + 1] = gt) and (p1[x * 3 + 2] = rt) then
      begin
        p2[x * 3] := p1[x * 3];
        p2[x * 3 + 1] := p1[x * 3 + 1];
        p2[x * 3 + 2] := p1[x * 3 + 2];
      end
      else
      begin
        p2[x * 3] := $FF - round(0.8 * abs($FF - p1[x * 3]));
        p2[x * 3 + 1] := $FF - round(0.8 * abs($FF - p1[x * 3 + 1]));
        p2[x * 3 + 2] := $FF - round(0.8 * abs($FF - p1[x * 3 + 2]));
      end;
    end;
  end;
  if FCaption <> '' then
  begin
    Flighter.Canvas.Brush.Style := bsclear;
    Flighter.Canvas.Font.Assign(FFont);
    FLighter.Canvas.Font.Color := FLighterFontColor;
    ARect := Rect(0, 0, Width, Height);
    FLighter.Canvas.TextRect(ARect, FCaptionLeft, FCaptionTop, FCaption);
  end;
end;

procedure TJvBitmapButton.MakeDarker;
var
  p1, p2: PByteArray;
  x, y: Integer;
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
  for y := 0 to FBitmap.Height - 1 do
  begin
    p1 := FBitmap.ScanLine[y];
    p2 := FDarker.ScanLine[y];
    for x := 0 to FBitmap.Width - 1 do
    begin
      if (p1[x * 3] = bt) and (p1[x * 3 + 1] = gt) and (p1[x * 3 + 2] = rt) then
      begin
        p2[x * 3] := p1[x * 3];
        p2[x * 3 + 1] := p1[x * 3 + 1];
        p2[x * 3 + 2] := p1[x * 3 + 2];
      end
      else
      begin
        p2[x * 3] := round(0.7 * p1[x * 3]);
        p2[x * 3 + 1] := round(0.7 * p1[x * 3 + 1]);
        p2[x * 3 + 2] := round(0.7 * p1[x * 3 + 2]);
      end
    end;
  end;
  if FCaption <> '' then
  begin
    FDarker.Canvas.Brush.Style := bsclear;
    FDarker.Canvas.Font.Assign(FFont);
    FDarker.Canvas.Font.Color := FDarkerFontColor;
    ARect := Rect(0, 0, Width, Height);
    FDarker.Canvas.TextRect(ARect, FCaptionLeft, FCaptionTop, FCaption);
  end;
end;

{$IFDEF VisualCLX}
procedure TJvBitmapButton.MouseLeave(AControl: TControl);
{$ENDIF}
{$IFDEF VCL}
procedure TJvBitmapButton.CMMouseLeave(var Message: TMessage);
{$ENDIF}
begin
  FMouseOver := False;
  Paint;
end;

procedure TJvBitmapButton.Loaded;
begin
  inherited;
  if not FBitmap.Empty then
  begin
    MakeDarker;
    MakeLighter;
    MakeNormal;
  end;
end;

procedure TJvBitmapButton.SetLatching(const Value: Boolean);
begin
  FLatching := Value;
  if not FLatching then
  begin
    FDown := False;
    Paint;
  end;
end;

procedure TJvBitmapButton.SetDown(const Value: Boolean);
begin
  if FLatching then
  begin
    FDown := Value;
    Paint;
  end
  else
  begin
    FDown := False;
    Paint;
  end;
end;

procedure TJvBitmapButton.Resize;
begin
  inherited;
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
  inherited;
  Value := FBitmap.Canvas.Pixels[x, y] <> FBitmap.Canvas.Pixels[0, FBitmap.Height - 1];
  if Value <> FMouseOver then
  begin
    FMouseOver := Value;
    Paint;
  end;
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, x, y);
end;

procedure TJvBitmapButton.SetCaption(const Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.SeTFont(const Value: TFont);
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
