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

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
unit JvBitmapButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TJvBitmapButton = class(TGraphicControl)
  private
    FBitmap: TBitmap;
    FLighter: TBitmap;
    FDarker: Tbitmap;
    FNormal: Tbitmap;
    FPushDown: boolean;
    FMouseOver: boolean;
    FLatching: boolean;
    FDown: boolean;
    FHotTrack: boolean;
    FCaption: string;
    FFont: Tfont;
    FCaptionLeft: integer;
    FCaptionTop: integer;
    FLighterFontColor: Tcolor;
    FDarkerFontcolor: Tcolor;
    procedure SetBitmap(const Value: TBitmap);
    procedure MakeNormal;
    procedure MakeDarker;
    procedure MakeLighter;
    procedure SetLatching(const Value: boolean);
    procedure SetDown(const Value: boolean);
    procedure SetHotTrack(const Value: boolean);
    procedure SetCaption(const Value: string);
    procedure SetFont(const Value: Tfont);
    procedure SetCaptionLeft(const Value: integer);
    procedure SetCaptionTop(const Value: integer);
    procedure UpDateBitmaps;
    procedure SetDarkerFontcolor(const Value: Tcolor);
    procedure SetLighterFontColor(const Value: Tcolor);
    { Private declarations }
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    procedure CMMouseLeave(var Message: TMessage); message CM_MouseLeave;
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
    property Down: boolean read FDown write SetDown;
    property Latching: boolean read FLatching write SetLatching;
    property HotTrack: boolean read FHotTrack write SetHotTrack;
    property onclick;
    property onmousedown;
    property onmouseup;
    property Hint;
    property ShowHint;
    property Caption: string read FCaption write SetCaption;
    property CaptionLeft: integer read FCaptionLeft write SetCaptionLeft;
    property CaptionTop: integer read FCaptionTop write SetCaptionTop;
    property Font: Tfont read FFont write SetFont;
    property DarkerFontcolor: Tcolor read FDarkerFontcolor write SetDarkerFontcolor;
    property LighterFontColor: Tcolor read FLighterFontColor write SetLighterFontColor;
  end;

implementation

{ TJvBitmapButton }

procedure TJvBitmapButton.Click;
begin
  if FPushDown then
    if assigned(onclick) then
      onclick(self);
end;

constructor TJvBitmapButton.Create(AOwner: TComponent);
begin
  inherited;
  width := 24;
  height := 24;
  FPushDown := false;
  FMouseOver := false;
  FLatching := false;
  FHotTrack := true;
  FDown := false;
  FBitmap := TBitmap.create;
  Fbitmap.width := 24;
  Fbitmap.Height := 24;
  Fbitmap.canvas.brush.color := clgray;
  FBitmap.canvas.FillRect(rect(1, 1, 23, 23));
  FLighter := Tbitmap.create;
  FDarker := Tbitmap.create;
  FNormal := Tbitmap.create;
  Ffont := Tfont.Create;
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
  if FBitmap.canvas.pixels[x, y] <> Fbitmap.canvas.pixels[0, FBitmap.height - 1] then
    FPushDown := true
  else
    FPushDown := false;
  Paint;
  if assigned(onmousedown) then
    onmousedown(self, button, shift, x, y);
end;

procedure TJvBitmapButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FPushDown := false;
  if Latching then
    FDown := not FDown
  else
    FDown := false;
  Paint;
  if assigned(onmouseup) then
    onmouseup(self, button, shift, x, y);
end;

procedure TJvBitmapButton.Paint;
var
  Acolor: TColor;
begin
  inherited;
  if assigned(FBitmap) then
  begin
    AColor := FBitmap.canvas.pixels[0, FBitmap.height - 1];
    Fbitmap.transparent := true;
    Fbitmap.transparentcolor := Acolor;
    FLighter.transparent := true;
    Flighter.TransparentColor := AColor;
    FDarker.transparent := true;
    FDarker.TransparentColor := AColor;
    FNormal.transparent := true;
    FNormal.TransparentColor := AColor;
    if FPushdown then
    begin
      canvas.draw(1, 1, FDarker)
    end
    else
    begin
      if Down then
        canvas.Draw(1, 1, FDarker)
      else if (FMouseOver and FHotTrack) then
        canvas.draw(0, 0, FLighter)
      else
        canvas.Draw(0, 0, FNormal);
    end;
  end;
end;

procedure TJvBitmapButton.SetBitmap(const Value: TBitmap);
begin
  FBitmap.assign(Value);
  FBitmap.transparent := true;
  FBitmap.TransparentColor := FBitmap.Canvas.pixels[0, FBitmap.Height - 1];
  width := FBitmap.Width;
  height := FBitmap.Height;
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
  p1, p2: Pbytearray;
  x, y: integer;
  rt, gt, bt: byte;

  AColor: TColor;
  ARect: Trect;
begin
  FLighter.Width := FBitmap.Width;
  FLighter.Height := FBitmap.height;
  Acolor := colortorgb(FBitmap.canvas.pixels[0, FBitmap.height - 1]);
  rt := GetRValue(Acolor);
  gt := GetGValue(AColor);
  bt := getBValue(AColor);
  FBitmap.PixelFormat := pf24bit;
  FLighter.PixelFormat := pf24bit;
  for y := 0 to Fbitmap.height - 1 do
  begin
    p1 := Fbitmap.ScanLine[y];
    p2 := FLighter.ScanLine[y];
    for x := 0 to FBitmap.width - 1 do
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
    Flighter.canvas.brush.Style := bsclear;
    Flighter.canvas.Font.Assign(FFont);
    FLighter.canvas.font.color := FLighterFontColor;
    ARect := rect(0, 0, width, height);
    FLighter.canvas.TextRect(ARect, FCaptionLeft, FCaptionTop, FCaption);
  end;
end;

procedure TJvBitmapButton.MakeDarker;
var
  p1, p2: Pbytearray;
  x, y: integer;
  rt, gt, bt: byte;
  AColor: TColor;
  Arect: TRect;
begin
  FDarker.Width := FBitmap.Width;
  FDarker.Height := FBitmap.height;
  Acolor := colortorgb(FBitmap.canvas.pixels[0, FBitmap.height - 1]);
  rt := GetRValue(Acolor);
  gt := GetGValue(AColor);
  bt := getBValue(AColor);
  FBitmap.PixelFormat := pf24bit;
  FDarker.PixelFormat := pf24bit;
  for y := 0 to Fbitmap.height - 1 do
  begin
    p1 := Fbitmap.ScanLine[y];
    p2 := FDarker.ScanLine[y];
    for x := 0 to FBitmap.width - 1 do
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
    FDarker.canvas.brush.Style := bsclear;
    FDarker.canvas.Font.Assign(FFont);
    FDarker.canvas.font.color := FDarkerFontColor;
    ARect := rect(0, 0, width, height);
    FDarker.canvas.TextRect(ARect, FCaptionLeft, FCaptionTop, FCaption);
  end;
end;

procedure TJvBitmapButton.CMMouseLeave(var Message: TMessage);
begin
  FMouseOver := false;
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

procedure TJvBitmapButton.SetLatching(const Value: boolean);
begin
  FLatching := Value;
  if not FLatching then
  begin
    FDown := false;
    paint;
  end;
end;

procedure TJvBitmapButton.SetDown(const Value: boolean);
begin
  if FLatching then
  begin
    FDown := Value;
    paint;
  end
  else
  begin
    FDown := false;
    paint;
  end;
end;

procedure TJvBitmapButton.Resize;
begin
  inherited;
  if assigned(Fbitmap) then
  begin
    width := FBitmap.width;
    height := FBitmap.Height;
  end
  else
  begin
    width := 24;
    height := 24;
  end;
end;

procedure TJvBitmapButton.SetHotTrack(const Value: boolean);
begin
  FHotTrack := Value;
end;

procedure TJvBitmapButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Value: boolean;
begin
  inherited;
  Value := FBitmap.canvas.pixels[x, y] <> Fbitmap.canvas.pixels[0, FBitmap.height - 1];
  if value <> FMouseOver then
  begin
    FMouseOver := value;
    Paint;
  end;
  if assigned(onmousemove) then
    onmousemove(self, shift, x, y);
end;

procedure TJvBitmapButton.SetCaption(const Value: string);
begin
  if value <> FCaption then
  begin
    FCaption := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.SetFont(const Value: Tfont);
begin
  if value <> FFont then
  begin
    FFont := Value;
    canvas.Font.Assign(FFont);
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.SetCaptionLeft(const Value: integer);
begin
  if value <> FCaptionLeft then
  begin
    FCaptionLeft := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.SetCaptionTop(const Value: integer);
begin
  if value <> FCaptionTop then
  begin
    FCaptionTop := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.MakeNormal;
var
  Arect: TRect;
begin
  FNormal.Assign(FBitmap);
  if FCaption <> '' then
  begin
    FNormal.canvas.brush.Style := bsclear;
    FNormal.canvas.Font.Assign(FFont);
    ARect := rect(0, 0, width, height);
    FNormal.canvas.TextRect(ARect, FCaptionLeft, FCaptionTop, FCaption);
  end;
end;

procedure TJvBitmapButton.SetDarkerFontcolor(const Value: Tcolor);
begin
  if value <> FDarkerFontColor then
  begin
    FDarkerFontcolor := Value;
    UpdateBitmaps;
  end;
end;

procedure TJvBitmapButton.SetLighterFontColor(const Value: Tcolor);
begin
  if value <> FLighterFontColor then
  begin
    FLighterFontColor := Value;
    UpdateBitmaps;
  end;
end;

end.
