{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgImageGroup.PAS, released on 2003-01-15.

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

UNIT JvgImageGroup;

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
   TJvgImageGroup = CLASS(TJvGraphicControl)
   PRIVATE
      FImageList: TImageList;
      FPassiveMask: TBitmap;
      FActiveMask: TBitmap;
      FSelectedMask: TBitmap;
      FSingleSelected: boolean;
      FTransparent: boolean;
      FTransparentColor: TColor;
      FMasked: boolean;
      FMaskedColor: TColor;
      FDisabledMaskColor: TColor;

      FAutoTrColor: TglAutoTransparentColor;
      FFastDraw: boolean;
      fNeedRemakeBackground: boolean;
      Image: TBitmap;
      OldWidth, OldHeight,
         OldLeft, OldTop: integer;

      PROCEDURE SmthChanged(Sender: TObject);

      PROCEDURE SetImageList(Value: TImageList);
      PROCEDURE SetTransparent(Value: boolean);
      PROCEDURE SetTransparentColor(Value: TColor);
      PROCEDURE SetMasked(Value: boolean);
      PROCEDURE SetMaskedColor(Value: TColor);
      PROCEDURE SetDisabledMaskColor(Value: TColor);
      PROCEDURE SetAutoTrColor(Value: TglAutoTransparentColor);
      PROCEDURE SetFastDraw(Value: boolean);

   PROTECTED
      PROCEDURE Paint; OVERRIDE;
      PROCEDURE WMSize(VAR Msg: TMessage);
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE CreateResBitmap;
      PROCEDURE RemakeBackground;
   PUBLISHED
      PROPERTY Align;
      PROPERTY DragCursor;
      PROPERTY DragMode;
      PROPERTY Enabled;
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

      PROPERTY Images: TImageList READ FImageList WRITE SetImageList;
      PROPERTY Transparent: boolean READ FTransparent WRITE SetTransparent
         DEFAULT false;
      PROPERTY TransparentColor: TColor READ FTransparentColor
         WRITE SetTransparentColor DEFAULT clOlive;
      PROPERTY Masked: boolean READ FMasked WRITE SetMasked
         DEFAULT false;
      PROPERTY MaskedColor: TColor READ FMaskedColor
         WRITE SetMaskedColor DEFAULT clOlive;
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

CONSTRUCTOR TJvgImageGroup.Create(AOwner: TComponent);
BEGIN

   INHERITED Create(AOwner);
   //  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
   Width := 105;
   Height := 105;

   Image := TBitmap.create;
   //...defaults
   FTransparent := false;
   FTransparentColor := clOlive;
   FMasked := false;
   FMaskedColor := clOlive;
   FDisabledMaskColor := clBlack;
   FAutoTrColor := ftcLeftBottomPixel;
   FFastDraw := false;
END;
//________________________________________________________

DESTRUCTOR TJvgImageGroup.Destroy;
BEGIN
   Image.free;
   INHERITED;
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.WMSize(VAR Msg: TMessage);
BEGIN
   IF csDesigning IN ComponentState THEN
      CreateResBitmap;
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.Paint;
BEGIN
   //  if fNeedRebuildImage then
   BEGIN
      CreateResBitmap;
   END;
   BitBlt(Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, 0, 0,
      SRCCOPY);
END;

PROCEDURE TJvgImageGroup.RemakeBackground;
BEGIN
   fNeedRemakeBackground := true;
   Repaint;
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.CreateResBitmap;
VAR
   i                          : integer;
   Bitmap                     : TBitmap;
BEGIN
   IF (FImageList = NIL) OR (FImageList.Count = 0) THEN
      exit;

   Bitmap := TBitmap.Create;

   Image.Width := FImageList.Width * FImageList.Count;
   Image.Height := FImageList.Height;
   Width := max(Image.Width, Width);
   Height := max(Image.Height, Height);
   WITH Image DO
   BEGIN
      Canvas.Brush.Color := clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Bounds(0, 0, Width, Height));
   END;

   IF FTransparent THEN
      GetParentImageRect(self, Bounds(Left, Top, Image.Width, Image.Height),
         Image.Canvas.Handle);

   FOR i := 0 TO FImageList.Count - 1 DO
   BEGIN
      FImageList.GetBitmap(i, Bitmap);

      IF FMasked THEN
         ChangeBitmapColor(Image, FMaskedColor, clBtnFace);

      CreateBitmapExt(Image.Canvas.Handle, Bitmap, ClientRect,
         i * FImageList.Width, 0,
         fwoNone, fdsDefault,
         FTransparent, FTransparentColor, FDisabledMaskColor);

   END;
   Bitmap.Free;
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.SmthChanged(Sender: TObject);
BEGIN
   Invalidate;
END;

//*****************************************_____________PROPERTY METHODS
//________________________________________________________

PROCEDURE TJvgImageGroup.SetImageList(Value: TImageList);
BEGIN
   FImageList := Value;
   //  SetAutoTrColor( FAutoTrColor );
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.SetTransparent(Value: boolean);
BEGIN
   IF FTransparent = Value THEN
      exit;
   FTransparent := Value;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.SetTransparentColor(Value: TColor);
BEGIN
   IF FTransparentColor = Value THEN
      exit;
   //  FAutoTrColor:=ftcUser;
   FTransparentColor := Value;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.SetMasked(Value: boolean);
BEGIN
   IF FMasked = Value THEN
      exit;
   FMasked := Value;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.SetMaskedColor(Value: TColor);
BEGIN
   IF FMaskedColor = Value THEN
      exit;
   FMaskedColor := Value;
   Invalidate;
END;

PROCEDURE TJvgImageGroup.SetDisabledMaskColor(Value: TColor);
BEGIN
   IF FDisabledMaskColor = Value THEN
      exit;
   FDisabledMaskColor := Value;
   Invalidate;
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.SetAutoTrColor(Value: TglAutoTransparentColor);
//var x,y :integer;
BEGIN {
   FAutoTrColor := Value;
   if (FAutoTrColor=ftcUser)or((FBitmap.Width or FBitmap.Height)=0)then
     exit;
   case FAutoTrColor of
     ftcLeftTopPixel: begin x:=0; y:=0; end;
     ftcLeftBottomPixel: begin x:=0; y:=FBitmap.Height-1; end;
     ftcRightTopPixel: begin x:=FBitmap.Width-1; y:=0; end;
     ftcRightBottomPixel: begin x:=FBitmap.Width-1; y:=FBitmap.Height-1; end;
   end;
   FTransparentColor := GetPixel(FBitmap.Canvas.Handle,x,y);
   Invalidate;}
END;
//________________________________________________________

PROCEDURE TJvgImageGroup.SetFastDraw(Value: boolean);
BEGIN
   FFastDraw := Value;
END;

END.

