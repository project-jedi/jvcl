{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgImage.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  JvComponent, JvgTypes, JvgUtils, JvgCommClasses;

type
  TJvgBitmapImage = class(TJvGraphicControl)
  private
    FAutoSize: Boolean;
    FImageAlign: TJvg2DAlign;
    FBitmapOption: TglWallpaperOption;
    FDrawState: TglDrawState;
    FTransparent: Boolean;
    FTransparentColor: TColor;
    FMasked: Boolean;
    FMaskedColor: TColor;
    FMaskedToColor: TColor;
    FDisabledMaskColor: TColor;
    FBitmap: TBitmap;
    FImage: TImage;
    FAutoTransparentColor: TglAutoTransparentColor;
    FFastDraw: Boolean;
    FBmp: TBitmap;
    FChanged: Boolean;
    FOnChangeParams: TNotifyEvent;
    // FOldClientRect: TRect;
    FOldWidth: Integer;
    FOldHeight: Integer;
    procedure CreateResBitmap;
    procedure Changed;
    procedure SmthChanged(Sender: TObject);
    function CalcAlignOffset: TPoint;
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure SetImage(Value: TImage);
    procedure SetBitmapOption(Value: TglWallpaperOption);
    procedure SetDrawState(Value: TglDrawState);
    procedure SetTransparent(Value: Boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetMasked(Value: Boolean);
    procedure SetMaskedColor(Value: TColor);
    procedure SetMaskedToColor(Value: TColor);
    procedure SetDisabledMaskColor(Value: TColor);
    procedure SetAutoTransparentColor(Value: TglAutoTransparentColor);
    procedure SetFastDraw(Value: Boolean);
  protected
    procedure SetAutoSize(Value: Boolean); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    FResBitmap: TBitmap; //...you can use it!
    //    procedure PaintTo(Canvas: TCanvas);
    procedure Paint; override;
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemakeBackground; //...for users
    //    procedure RepaintBackground; //...for users
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Image: TImage read FImage write SetImage;
    property ImageAlign: TJvg2DAlign read FImageAlign write FImageAlign;
    property BitmapOption: TglWallpaperOption read FBitmapOption  write SetBitmapOption default fwoNone;
    property DrawState: TglDrawState read FDrawState write SetDrawState default fdsDefault;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor default clOlive;
    property Masked: Boolean read FMasked write SetMasked default False;
    property MaskedColor: TColor read FMaskedColor write SetMaskedColor default clOlive;
    property MaskedToColor: TColor read FMaskedToColor write SetMaskedToColor default clBtnFace;
    property DisabledMaskColor: TColor read FDisabledMaskColor write SetDisabledMaskColor default clBlack;
    property AutoTransparentColor: TglAutoTransparentColor read FAutoTransparentColor
      write SetAutoTransparentColor default ftcLeftBottomPixel;
    property FastDraw: Boolean read FFastDraw write SetFastDraw default False;
    property Anchors;
    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnChangeParams: TNotifyEvent read FOnChangeParams write FOnChangeParams;
  end;

implementation

uses
  Math;

constructor TJvgBitmapImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 105;
  Height := 105;

  FResBitmap := TBitmap.Create;
  FImageAlign := TJvg2DAlign.Create;
  FImageAlign.OnChanged := SmthChanged;
  FChanged := True;
  // FOldClientRect := Rect(left, top, left + Width, top + Height);
  //...defaults
  FAutoSize := False;
  FBitmapOption := fwoNone;
  FDrawState := fdsDefault;
  FTransparent := False;
  FTransparentColor := clOlive;
  FMasked := False;
  FMaskedColor := clOlive;
  FMaskedToColor := clBtnFace;
  FDisabledMaskColor := clBlack;
  FAutoTransparentColor := ftcLeftBottomPixel;
  FFastDraw := False;
  OnChangeParams := nil;
end;

destructor TJvgBitmapImage.Destroy;
begin
  FResBitmap.Free;
  FBitmap.Free;
  FImageAlign.Free;
  inherited Destroy;
end;

procedure TJvgBitmapImage.Loaded;
begin
  inherited Loaded;
  if Assigned(FBitmap) and not FBitmap.Empty then
    FBmp := FBitmap;
  SetAutoTransparentColor(FAutoTransparentColor);
end;

procedure TJvgBitmapImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Image) and (Operation = opRemove) then
    Image := nil;
end;

procedure TJvgBitmapImage.Paint;
var
  //R, IntersectR: TRect;
  Pt: TPoint;
begin
  if Assigned(Bitmap) then
    FBmp := Bitmap;
  if Assigned(Image) then
    FBmp := Image.Picture.Bitmap;

  if Assigned(FBmp) and (FBmp.Handle <> 0) then
  begin
    if (FOldWidth <> Width) or (FOldHeight <> Height) then
    begin
      FChanged := True;
      {if (OldLeft=Left)and(OldTop=Top) then
      begin
        R:=Rect( left, top, left+Width, top+Height );
        IntersectRect( IntersectR, FOldClientRect, R );
        InvalidateRect( Parent.Handle, @R, False );
        ValidateRect( Parent.Handle, @IntersectR );
        FOldClientRect := R;
       end;}
    end;
    //OldLeft := Left; OldTop := Top;
    FOldWidth := Width;
    FOldHeight := Height;

    if FChanged or not FFastDraw then
    begin
      CreateResBitmap;
      FChanged := False;
    end;
    Pt := CalcAlignOffset;
    BitBlt(Canvas.Handle, Pt.X, Pt.Y, FResBitmap.Width, FResBitmap.Height,
      FResBitmap.canvas.Handle, 0, 0, SRCCOPY);
  end;
  if (csDesigning in ComponentState) and (Tag <> 9999) then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;

procedure TJvgBitmapImage.RemakeBackground;
begin
  FChanged := True;
  Repaint;
end;

//procedure TJvgBitmapImage.WMSize(var Message: TWMSize);
//var R,IntersectR: TRect;
//begin
{  Exit;
  if FAutoSize then
  begin Width:=FResBitmap.Width; Height:=FResBitmap.Height; end;
  if not FTransparent then
  begin
    R:=Rect( left, top, left+Width, top+Height );
    IntersectRect( IntersectR, FOldClientRect, R );
    InvalidateRect( Parent.Handle, @R, False );
    ValidateRect( Parent.Handle, @IntersectR );
    FOldClientRect := R;
  end
  else
    Invalidate;
  Changed;}
//end;

procedure TJvgBitmapImage.CreateResBitmap;
var
  Pt: TPoint;
  //  BmpInfo: Windows.TBitmap;
begin
  if (FBitmapOption = fwoStretch) or (FBitmapOption = fwoPropStretch) or
    (FBitmapOption = fwoTile) then
  begin
    FResBitmap.Width := Width;
    FResBitmap.Height := Height;
  end
  else
  begin
    FResBitmap.Width := FBmp.Width;
    FResBitmap.Height := FBmp.Height;
  end;

  with FResBitmap do
  begin
    // if FTransparent then Canvas.Brush.Color := FTransparentColor
    Canvas.Brush.Color := clBtnFace;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Bounds(0, 0, Width, Height));
  end;

  Pt := CalcAlignOffset;
  if FTransparent then
    GetParentImageRect(Self, Bounds(Left + Pt.X, Top + Pt.Y, FResBitmap.Width,
      FResBitmap.Height),
      FResBitmap.Canvas.Handle);
  //BringParentWindowToTop(parent);
//    BitBlt( FResBitmap.Canvas.Handle, 0,0, Width, Height, canvas.Handle, 0, 0, SRCCOPY);

  CreateBitmapExt(FResBitmap.Canvas.Handle, FBmp, ClientRect, 0, 0,
    FBitmapOption, FDrawState,
    FTransparent, FTransparentColor, FDisabledMaskColor);

  if FMasked then
    ChangeBitmapColor(FResBitmap, FMaskedColor, FMaskedToColor);

  {  GetObject( FResBitmap.Handle, SizeOf(Windows.TBitmap), @BmpInfo );
    if BmpInfo.bmBitsPixel >= 8 then
    with FResBitmap,BmpInfo do
    begin
      for i := 1 to bmWidth*bmHeight*(bmBitsPixel div 8)-1 do
        begin
   asm
    inc BmpInfo.bmBits
   end;
   Byte(bmBits^):=1;
        end;
    end;}
end;

procedure TJvgBitmapImage.Changed;
begin
  FChanged := True;
  if Assigned(OnChangeParams) then
    OnChangeParams(Self);
end;

procedure TJvgBitmapImage.SmthChanged(Sender: TObject);
begin
  Changed;
  Invalidate;
end;

function TJvgBitmapImage.CalcAlignOffset: TPoint;
var
  D, D1: Double;
  Pt: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
  if (FBitmapOption = fwoNone) or (FBitmapOption = fwoPropStretch) then
  begin
    Pt.X := FBmp.Width;
    Pt.Y := FBmp.Height;
    if FBitmapOption = fwoPropStretch then
    begin
      D1 := Width / Pt.X;
      D := Height / Pt.Y;
      if D > D1 then
        D := D1; //...D == Min
      Pt.X := Trunc(Pt.X * D);
      Pt.Y := Trunc(Pt.Y * D);
    end;
    case ImageAlign.Horizontal of
      fhaCenter:
        Result.X := Max(0, (Width - Pt.X) div 2);
      fhaRight:
        Result.X := Max(0, Width - Pt.X);
    end;
    case ImageAlign.Vertical of
      fvaCenter:
        Result.Y := Max(0, (Height - Pt.Y) div 2);
      fvaBottom:
        Result.Y := Max(0, Height - Pt.Y);
    end;
  end;
end;

procedure TJvgBitmapImage.SetAutoSize(Value: Boolean);
begin
  if (FAutoSize = Value) or not Assigned(FBmp) then
    Exit;
  FAutoSize := Value;
  if FAutoSize and (FBitmapOption = fwoNone) and
    // (rom) strange  this evaluates to FBmp.Width <> FBmp.Height
    ((FBmp.Width and FBmp.Height) <> 0) then
  begin
    Width := FBmp.Width;
    Height := FBmp.Height;
    Changed;
    Invalidate;
  end;
end;

function TJvgBitmapImage.GetBitmap: TBitmap;
begin
  if not Assigned(FBitmap) then
    FBitmap := TBitmap.Create;
  Result := FBitmap;
end;

procedure TJvgBitmapImage.SetBitmap(Value: TBitmap);
begin
  FBitmap.Free;
  FBitmap := TBitmap.Create;
  FBitmap.Assign(Value);
  if Assigned(Value) then
    FBmp := FBitmap
  else
  if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
    FBmp := nil;
  SetAutoTransparentColor(FAutoTransparentColor);
  Changed;
  Invalidate;
end;

procedure TJvgBitmapImage.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage) and Assigned(FImage.Picture) and
    Assigned(FImage.Picture.Bitmap) then
    FBmp := FImage.Picture.Bitmap
  else
  if Assigned(FBitmap) then
    FBmp := FBitmap
  else
    FBmp := nil;
  SetAutoTransparentColor(FAutoTransparentColor);
  Changed;
  Invalidate;
end;

procedure TJvgBitmapImage.SetBitmapOption(Value: TglWallpaperOption);
begin
  if FBitmapOption <> Value then
  begin
    FBitmapOption := Value;
    Changed;
    Invalidate;
  end;
end;

procedure TJvgBitmapImage.SetDrawState(Value: TglDrawState);
begin
  if FDrawState <> Value then
  begin
    FDrawState := Value;
    Changed;
    Invalidate;
  end;
end;

procedure TJvgBitmapImage.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
    Invalidate;
  end;
end;

procedure TJvgBitmapImage.SetTransparentColor(Value: TColor);
begin
  if (FAutoTransparentColor <> ftcUser) or (FTransparentColor = Value) then
    Exit;
  FTransparentColor := Value;
  Changed;
  Invalidate;
end;

procedure TJvgBitmapImage.SetMasked(Value: Boolean);
begin
  if FMasked <> Value then
  begin
    FMasked := Value;
    Changed;
    Invalidate;
  end;
end;

procedure TJvgBitmapImage.SetMaskedColor(Value: TColor);
begin
  if FMaskedColor <> Value then
  begin
    FMaskedColor := Value;
    Changed;
    Invalidate;
  end;
end;

procedure TJvgBitmapImage.SetMaskedToColor(Value: TColor);
begin
  if FMaskedToColor <> Value then
  begin
    FMaskedToColor := Value;
    Changed;
    Invalidate;
  end;
end;

procedure TJvgBitmapImage.SetDisabledMaskColor(Value: TColor);
begin
  if FDisabledMaskColor <> Value then
  begin
    FDisabledMaskColor := Value;
    Changed;
    Invalidate;
  end;
end;

procedure TJvgBitmapImage.SetAutoTransparentColor(Value: TglAutoTransparentColor);
begin
  FAutoTransparentColor := Value;
  if not Assigned(FBmp) then
    Exit;
  if Value <> ftcUser then
    FTransparentColor := GetTransparentColor(FBmp, Value);
  Changed;
  Invalidate;
end;

procedure TJvgBitmapImage.SetFastDraw(Value: Boolean);
begin
  if FFastDraw <> Value then
  begin
    FFastDraw := Value;
    Changed;
    Invalidate;
  end;
end;

{procedure TJvgBitmapImage.SetWidth(Value: Integer);
begin
  if FWidth = Value then Exit;
  FWidth := Value; Invalidate;
end;

procedure TJvgBitmapImage.SetHeight(Value: Integer);
begin
  if FHeight = Value then Exit;
  FHeight := Value; Invalidate;
end;}

end.

