{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgImage.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

UNIT JvgImage;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   ExtCtrls,
   JvgTypes,
   JvgUtils,
   JVComponent,
   JvgCommClasses;

TYPE
   TJvgBitmapImage = CLASS(TJvGraphicControl)
   PRIVATE
      FAutoSize: boolean;
      FImageAlign: TJvg2DAlign;
      FBitmapOption: TglWallpaperOption;
      FDrawState: TglDrawState;
      FTransparent: boolean;
      FTransparentColor: TColor;
      FMasked: boolean;
      FMaskedColor: TColor;
      FMaskedToColor: TColor;
      FDisabledMaskColor: TColor;
      FBitmap: TBitmap;
      FImage: TImage;
      FAutoTrColor: TglAutoTransparentColor;
      FFastDraw: boolean;
      Bmp: TBitmap;
      fChanged: boolean;
      OldClientRect: TRect;
      OldWidth, OldHeight: integer;
      PROCEDURE CreateResBitmap;
      PROCEDURE Changed;
      PROCEDURE SmthChanged(Sender: TObject);
      FUNCTION CalcAlignOffset: TPoint;

      PROCEDURE SetAutoSize(Value: boolean);
      FUNCTION GetBitmap: TBitmap;
      PROCEDURE SetBitmap(Value: TBitmap);
      PROCEDURE SetImage(Value: TImage);
      PROCEDURE SetBitmapOption(Value: TglWallpaperOption);
      PROCEDURE SetDrawState(Value: TglDrawState);
      PROCEDURE SetTransparent(Value: boolean);
      PROCEDURE SetTransparentColor(Value: TColor);
      PROCEDURE SetMasked(Value: boolean);
      PROCEDURE SetMaskedColor(Value: TColor);
      PROCEDURE SetMaskedToColor(Value: TColor);
      PROCEDURE SetDisabledMaskColor(Value: TColor);
      PROCEDURE SetAutoTrColor(Value: TglAutoTransparentColor);
      PROCEDURE SetFastDraw(Value: boolean);

   PROTECTED
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;

   PUBLIC
      OnChangeParams: TNotifyEvent;
      FResBitmap: TBitmap;              //...you can use it!
      //    procedure PaintTo(Canvas: TCanvas);
      PROCEDURE Paint; OVERRIDE;
      PROPERTY Canvas;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE RemakeBackground;       //...for users
      //    procedure RepaintBackground;//...for users

   PUBLISHED
      {$IFDEF COMPILER5_UP}
      PROPERTY Anchors;
      {$ENDIF}
      PROPERTY Align;
      PROPERTY Color;
      PROPERTY DragCursor;
      PROPERTY DragMode;
      PROPERTY Enabled;
      PROPERTY Font;
      PROPERTY ParentColor;
      PROPERTY ParentFont;
      PROPERTY ParentShowHint;
      PROPERTY PopupMenu;
      PROPERTY ShowHint;
      PROPERTY Visible;
      PROPERTY OnClick;
      PROPERTY OnDblClick;
      PROPERTY OnDragDrop;
      PROPERTY OnDragOver;
      PROPERTY OnEndDrag;
      PROPERTY OnMouseDown;
      PROPERTY OnMouseMove;
      PROPERTY OnMouseUp;
      PROPERTY OnStartDrag;

      PROPERTY AutoSize: boolean READ FAutoSize WRITE SetAutoSize
         DEFAULT false;
      PROPERTY Bitmap: TBitmap READ GetBitmap WRITE SetBitmap;
      PROPERTY Image: TImage READ FImage WRITE SetImage;
      PROPERTY ImageAlign: TJvg2DAlign READ FImageAlign WRITE FImageAlign;
      PROPERTY BitmapOption: TglWallpaperOption READ FBitmapOption
         WRITE SetBitmapOption DEFAULT fwoNone;
      PROPERTY DrawState: TglDrawState READ FDrawState WRITE SetDrawState
         DEFAULT fdsDefault;
      PROPERTY Transparent: boolean READ FTransparent WRITE SetTransparent
         DEFAULT false;
      PROPERTY TransparentColor: TColor READ FTransparentColor
         WRITE SetTransparentColor DEFAULT clOlive;
      PROPERTY Masked: boolean READ FMasked WRITE SetMasked
         DEFAULT false;
      PROPERTY MaskedColor: TColor READ FMaskedColor
         WRITE SetMaskedColor DEFAULT clOlive;
      PROPERTY MaskedToColor: TColor READ FMaskedToColor
         WRITE SetMaskedToColor DEFAULT clBtnFace;
      PROPERTY DisabledMaskColor: TColor READ FDisabledMaskColor
         WRITE SetDisabledMaskColor DEFAULT clBlack;
      PROPERTY AutoTransparentColor: TglAutoTransparentColor
         READ FAutoTrColor WRITE SetAutoTrColor DEFAULT ftcLeftBottomPixel;
      PROPERTY FastDraw: boolean READ FFastDraw WRITE SetFastDraw
         DEFAULT false;

   END;

PROCEDURE Register;

IMPLEMENTATION

PROCEDURE Register;
BEGIN
END;

//*****************************************_____________LowLevel METHODS
//________________________________________________________

CONSTRUCTOR TJvgBitmapImage.Create(AOwner: TComponent);
BEGIN

   INHERITED Create(AOwner);
   //  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
     {inherited } Width := 105;
   {inherited } Height := 105;

   FResBitmap := TBitmap.create;
   FImageAlign := TJvg2DAlign.create;
   FImageAlign.OnChanged := SmthChanged;
   fChanged := true;
   OldClientRect := Rect(left, top, left + width, top + height);
   //...defaults
   FAutoSize := false;
   FBitmapOption := fwoNone;
   FDrawState := fdsDefault;
   FTransparent := false;
   FTransparentColor := clOlive;
   FMasked := false;
   FMaskedColor := clOlive;
   FMaskedToColor := clBtnFace;
   FDisabledMaskColor := clBlack;
   FAutoTrColor := ftcLeftBottomPixel;
   FFastDraw := false;
   OnChangeParams := NIL;
END;
//________________________________________________________

DESTRUCTOR TJvgBitmapImage.Destroy;
BEGIN
   FResBitmap.free;
   IF Assigned(FBitmap) THEN
      FBitmap.Free;
   FImageAlign.Free;
   INHERITED;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.Loaded;
BEGIN
   INHERITED;
   IF Assigned(FBitmap) AND (NOT FBitmap.Empty) THEN
      Bmp := FBitmap;
   SetAutoTrColor(FAutoTrColor);
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
   IF (AComponent = Image) AND (Operation = opRemove) THEN
      Image := NIL;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.Paint;
VAR                                     //R,IntersectR: TRect;
   pt                         : TPoint;
BEGIN
   IF Assigned(Bitmap) THEN
      bmp := Bitmap;
   IF Assigned(Image) THEN
      bmp := Image.Picture.Bitmap;

   IF Assigned(Bmp) AND (Bmp.handle <> 0) THEN
   BEGIN
      IF (OldWidth <> Width) OR (OldHeight <> Height) THEN
      BEGIN
         fChanged := true;
         {if (OldLeft=Left)and(OldTop=Top) then
         begin
           R:=Rect( left, top, left+width, top+height );
           IntersectRect( IntersectR, OldClientRect, R );
           InvalidateRect( Parent.Handle, @R, false );
           ValidateRect( Parent.Handle, @IntersectR );
           OldClientRect := R;
          end;}
      END;                              //OldLeft := Left; OldTop := Top;
      OldWidth := Width;
      OldHeight := Height;

      IF fChanged OR NOT FFastDraw THEN
      BEGIN
         CreateResBitmap;
         fChanged := false;
      END;
      pt := CalcAlignOffset;
      BitBlt(Canvas.Handle, pt.x, pt.y, FResBitmap.Width, FResBitmap.Height,
         FResBitmap.canvas.Handle, 0, 0, SRCCOPY);
   END;
   IF (csDesigning IN ComponentState) AND (tag <> 9999) THEN
      WITH Canvas DO
      BEGIN
         Pen.Color := clBlack;
         Pen.Style := psDash;
         Brush.Style := bsClear;
         Rectangle(0, 0, width, height);
      END;

END;

PROCEDURE TJvgBitmapImage.RemakeBackground;
BEGIN
   fChanged := true;
   Repaint;
END;
//________________________________________________________
//procedure TJvgBitmapImage.WMSize(var Message: TWMSize);
//var R,IntersectR: TRect;
//begin
{  exit;
  if FAutoSize then
  begin Width:=FResBitmap.Width; Height:=FResBitmap.Height; end;
  if not FTransparent then
  begin
    R:=Rect( left, top, left+width, top+height );
    IntersectRect( IntersectR, OldClientRect, R );
    InvalidateRect( Parent.Handle, @R, false );
    ValidateRect( Parent.Handle, @IntersectR );
    OldClientRect := R;
  end else Invalidate;
  Changed;}
//end;
//________________________________________________________

PROCEDURE TJvgBitmapImage.CreateResBitmap;
VAR
   pt                         : TPoint;
   //  BmpInfo: Windows.TBitmap;
BEGIN
   IF (FBitmapOption = fwoStretch) OR (FBitmapOption = fwoPropStretch) OR
      (FBitmapOption = fwoTile) THEN
   BEGIN
      FResBitmap.Width := Width;
      FResBitmap.Height := Height;
   END
   ELSE
   BEGIN
      FResBitmap.Width := Bmp.Width;
      FResBitmap.Height := Bmp.Height;
   END;

   WITH FResBitmap DO
   BEGIN
      //	if FTransparent then Canvas.Brush.Color := FTransparentColor
      Canvas.Brush.Color := clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Bounds(0, 0, Width, Height));
   END;

   pt := CalcAlignOffset;
   IF FTransparent THEN
      GetParentImageRect(self, Bounds(Left + pt.x, Top + pt.y, FResBitmap.Width,
         FResBitmap.Height),
         FResBitmap.Canvas.Handle);
   //BringParentWindowToTop(parent);
 //    BitBlt( FResBitmap.Canvas.Handle, 0,0, Width, Height, canvas.Handle, 0, 0, SRCCOPY);

   CreateBitmapExt(FResBitmap.Canvas.Handle, Bmp, ClientRect, 0, 0,
      FBitmapOption, FDrawState,
      FTransparent, FTransparentColor, FDisabledMaskColor);

   IF FMasked THEN
      ChangeBitmapColor(FResBitmap, FMaskedColor, FMaskedToColor);

   {  GetObject( FResBitmap.Handle, sizeof(Windows.TBitmap), @BmpInfo );
     if BmpInfo.bmBitsPixel >= 8 then
     with FResBitmap,BmpInfo do begin
       for i := 1 to bmWidth*bmHeight*(bmBitsPixel div 8)-1 do
         begin
    asm
     inc BmpInfo.bmBits
    end;
    byte(bmBits^):=1;
         end;
     end;}

END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.Changed;
BEGIN
   fChanged := true;
   IF Assigned(OnChangeParams) THEN
      OnChangeParams(self);
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SmthChanged(Sender: TObject);
BEGIN
   Changed;
   Invalidate;
END;

FUNCTION TJvgBitmapImage.CalcAlignOffset: TPoint;
VAR
   D, D_                      : double;
   bmp_                       : TPoint;
BEGIN
   Result.x := 0;
   Result.y := 0;
   IF (FBitmapOption = fwoNone) OR (FBitmapOption = fwoPropStretch) THEN
   BEGIN
      bmp_.x := Bmp.Width;
      bmp_.y := Bmp.Height;
      IF FBitmapOption = fwoPropStretch THEN
      BEGIN
         D_ := Width / bmp_.x;
         D := Height / bmp_.y;
         IF D > D_ THEN
            D := D_;                    //...D == min
         bmp_.x := trunc(bmp_.x * D);
         bmp_.y := trunc(bmp_.y * D);
      END;
      CASE ImageAlign.Horizontal OF
         fhaCenter: Result.x := max(0, (Width - bmp_.x) DIV 2);
         fhaRight: Result.x := max(0, Width - bmp_.x);
      END;
      CASE ImageAlign.Vertical OF
         fvaCenter: Result.y := max(0, (Height - bmp_.y) DIV 2);
         fvaBottom: Result.y := max(0, Height - bmp_.y);
      END;
   END;
END;
//*****************************************_____________PROPERTY METHODS
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetAutoSize(Value: boolean);
BEGIN
   IF (FAutoSize = Value) OR NOT Assigned(Bmp) THEN
      exit;
   FAutoSize := Value;
   IF FAutoSize AND (FBitmapOption = fwoNone)
      AND ((Bmp.Width AND Bmp.Height) <> 0) THEN
   BEGIN
      Width := Bmp.Width;
      Height := Bmp.Height;
      Changed;
      Invalidate;
   END;
END;
//________________________________________________________

FUNCTION TJvgBitmapImage.GetBitmap: TBitmap;
BEGIN
   IF NOT Assigned(FBitmap) THEN
      FBitmap := TBitmap.Create;
   Result := FBitmap;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetBitmap(Value: TBitmap);
BEGIN
   IF Assigned(FBitmap) THEN
      FBitmap.Free;
   FBitmap := TBitmap.Create;
   FBitmap.Assign(Value);
   IF Assigned(Value) THEN
      Bmp := FBitmap
   ELSE IF Assigned(FImage) AND Assigned(FImage.Picture) AND
      Assigned(FImage.Picture.Bitmap) THEN
      Bmp := FImage.Picture.Bitmap
   ELSE
      Bmp := NIL;
   SetAutoTrColor(FAutoTrColor);
   Changed;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetImage(Value: TImage);
BEGIN
   FImage := Value;
   IF Assigned(FImage) AND Assigned(FImage.Picture) AND
      Assigned(FImage.Picture.Bitmap) THEN
      Bmp := FImage.Picture.Bitmap
   ELSE IF Assigned(FBitmap) THEN
      Bmp := FBitmap
   ELSE
      Bmp := NIL;
   SetAutoTrColor(FAutoTrColor);
   Changed;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetBitmapOption(Value: TglWallpaperOption);
BEGIN
   IF FBitmapOption = Value THEN
      exit;
   FBitmapOption := Value;
   Changed;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetDrawState(Value: TglDrawState);
BEGIN
   IF FDrawState = Value THEN
      exit;
   FDrawState := Value;
   Changed;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetTransparent(Value: boolean);
BEGIN
   IF FTransparent = Value THEN
      exit;
   FTransparent := Value;
   Changed;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetTransparentColor(Value: TColor);
BEGIN
   IF (FAutoTrColor <> ftcUser) OR (FTransparentColor = Value) THEN
      exit;
   FTransparentColor := Value;
   Changed;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetMasked(Value: boolean);
BEGIN
   IF FMasked = Value THEN
      exit;
   FMasked := Value;
   Changed;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetMaskedColor(Value: TColor);
BEGIN
   IF FMaskedColor = Value THEN
      exit;
   FMaskedColor := Value;
   Changed;
   Invalidate;
END;

PROCEDURE TJvgBitmapImage.SetMaskedToColor(Value: TColor);
BEGIN
   IF FMaskedToColor = Value THEN
      exit;
   FMaskedToColor := Value;
   Changed;
   Invalidate;
END;

PROCEDURE TJvgBitmapImage.SetDisabledMaskColor(Value: TColor);
BEGIN
   IF FDisabledMaskColor = Value THEN
      exit;
   FDisabledMaskColor := Value;
   Changed;
   Invalidate;
END;

//________________________________________________________

PROCEDURE TJvgBitmapImage.SetAutoTrColor(Value: TglAutoTransparentColor);
BEGIN
   FAutoTrColor := Value;
   IF NOT Assigned(bmp) THEN
      exit;
   IF Value <> ftcUser THEN
      FTransparentColor := GetTransparentColor(bmp, Value);
   Changed;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgBitmapImage.SetFastDraw(Value: boolean);
BEGIN
   IF FFastDraw = Value THEN
      exit;
   FFastDraw := Value;
   Changed;
   Invalidate;
END;
//________________________________________________________
{procedure TJvgBitmapImage.SetWidth(Value: integer);
begin
  if FWidth = Value then exit;
  FWidth := Value; Invalidate;
end;
//________________________________________________________
procedure TJvgBitmapImage.SetHeight(Value: integer);
begin
  if FHeight = Value then exit;
  FHeight := Value; Invalidate;
end;}
//________________________________________________________
END.

