{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgProgress.PAS, released on 2003-01-15.

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

UNIT JvgProgress;

INTERFACE

USES
   Windows,
   Messages,
   Classes,
   Controls,
   Graphics,
   SysUtils,
   JvgTypes,
   JvgCommClasses,
   JvgUtils,
   JVComponent,
   ExtCtrls{$IFDEF COMPILER5_UP},
   Imglist{$ENDIF};
TYPE

   TJvgProgress = CLASS(TJvGraphicControl)
   PRIVATE
      FBevelInner: TPanelBevel;
      FBevelOuter: TPanelBevel;
      FBevelBold: boolean;
      FColors: TJvgSimleLabelColors;
      FGradientF: TJvgGradient;
      FGradientB: TJvgGradient;
      FPercent: TglPercent;
      FCaptionAlignment: TAlignment;
      FCaptionDirection: TglLabelDir;
      FCaptionStyle: TglTextStyle;
      FStep: integer;
      FInterspace: integer;
      FOptions: TglProgressOptions;
      Image: TBitmap;
      BackImage: TBitmap;
      PROCEDURE SetBevelInner(Value: TPanelBevel);
      PROCEDURE SetBevelOuter(Value: TPanelBevel);
      PROCEDURE SetBevelBold(Value: boolean);
      PROCEDURE SetPercent(Value: TglPercent);
      PROCEDURE SetCaptionAlignment(Value: TAlignment);
      PROCEDURE SetCaptionDirection(Value: TglLabelDir);
      PROCEDURE SetCaptionStyle(Value: TglTextStyle);
      PROCEDURE SetStep(Value: integer);
      PROCEDURE SetInterspace(Value: integer);
      PROCEDURE SetOptions(Value: TglProgressOptions);

      PROCEDURE OnSmthChanged(Sender: TObject);
      PROCEDURE CMTextChanged(VAR Message: TMessage); MESSAGE CM_TEXTCHANGED;
   PROTECTED
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE Paint; OVERRIDE;

   PUBLIC
      fNeedRebuildBackground: boolean;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

   PUBLISHED
      {$IFDEF COMPILER5_UP}
      PROPERTY Anchors;
      {$ENDIF}
      PROPERTY Align;
      PROPERTY Caption;
      PROPERTY Font;
      PROPERTY ParentShowHint;
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
      PROPERTY DragCursor;
      PROPERTY DragMode;

      PROPERTY BevelInner: TPanelBevel READ FBevelInner WRITE SetBevelInner
         DEFAULT bvLowered;
      PROPERTY BevelOuter: TPanelBevel READ FBevelOuter WRITE SetBevelOuter
         DEFAULT bvNone;
      PROPERTY BevelBold: boolean READ FBevelBold WRITE SetBevelBold
         DEFAULT false;
      PROPERTY Colors: TJvgSimleLabelColors READ FColors WRITE FColors;
      PROPERTY Gradient: TJvgGradient READ FGradientF WRITE FGradientF;
      PROPERTY GradientBack: TJvgGradient READ FGradientB WRITE FGradientB;
      PROPERTY Percent: TglPercent READ FPercent WRITE SetPercent;
      PROPERTY CaptionAlignment: TAlignment READ FCaptionAlignment WRITE
         SetCaptionAlignment;
      PROPERTY CaptionDirection: TglLabelDir READ FCaptionDirection WRITE
         SetCaptionDirection;
      PROPERTY CaptionStyle: TglTextStyle READ FCaptionStyle WRITE
         SetCaptionStyle;
      PROPERTY Step: integer READ FStep WRITE SetStep;
      PROPERTY Interspace: integer READ FInterspace WRITE SetInterspace;
      PROPERTY Options: TglProgressOptions READ FOptions WRITE SetOptions;

   END;

PROCEDURE Register;

IMPLEMENTATION
{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

CONSTRUCTOR TJvgProgress.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   ControlStyle := [csOpaque, csDoubleClicks];
   FColors := TJvgSimleLabelColors.Create;
   FGradientB := TJvgGradient.Create;
   FGradientF := TJvgGradient.Create;
   //..defaults
   IF csDesigning IN ComponentState THEN
      WITH FColors DO
      BEGIN
         FGradientF.Orientation := fgdVertical;
         FGradientB.Orientation := fgdVertical;
         FGradientF.Active := true;
         FGradientB.Active := true;
         FGradientF.FromColor := clGreen;
         FGradientF.ToColor := clYellow;
         FGradientB.FromColor := 0;
         FGradientB.ToColor := clGreen;
         FGradientF.PercentFilling := FPercent;
         Delineate := clGray;
         Shadow := 0;
         Background := 0;
         Caption := 'progress...[%d%%]';
      END;
   FColors.OnChanged := OnSmthChanged;
   FGradientB.OnChanged := OnSmthChanged;
   FGradientF.OnChanged := OnSmthChanged;
   Image := TBitmap.Create;
   BackImage := TBitmap.Create;
   Width := 150;
   height := 15;

   FCaptionDirection := fldLeftRight;
   FCaptionAlignment := taLeftJustify;
   //...
   FStep := 3;
   FInterspace := 1;
   FCaptionStyle := fstShadow;
   FCaptionAlignment := taCenter;
   Font.Color := clWhite;
   FBevelInner := bvLowered;
   //...
   Color := clBlack;
END;

DESTRUCTOR TJvgProgress.Destroy;
BEGIN
   FGradientF.Free;
   FGradientB.Free;
   FColors.Free;
   BackImage.Free;
   Image.Free;
   INHERITED;
END;

PROCEDURE TJvgProgress.Loaded;
BEGIN
   INHERITED loaded;
   {  Image.Width := Width; Image.Height := Height;
     BackImage.Width := Width; BackImage.Height := Height;
     if fpoTransparent in Options then
     GetParentImageRect( self, Bounds(Left,Top,Width,Height),
                  Image.Canvas.Handle );}

END;

PROCEDURE TJvgProgress.CMTextChanged(VAR Message: TMessage);
BEGIN
   Repaint;
END;

PROCEDURE TJvgProgress.Paint;
CONST
   ShadowDepth                = 2;
VAR
   r                          : TRect;
   i, x, x2, y                : integer;
   Size, TextSize             : TSize;
   Capt                       : STRING;
BEGIN
   IF (Image.Width <> Width) OR (Image.Height <> Height) THEN
   BEGIN
      Image.Width := Width;
      Image.Height := Height;
      BackImage.Width := Width;
      BackImage.Height := Height;
      fNeedRebuildBackground := true;
   END;
   r := ClientRect;
   IF (fpoTransparent IN Options) AND fNeedRebuildBackground THEN
   BEGIN
      GetParentImageRect(self, Bounds(Left, Top, Width, Height),
         BackImage.Canvas.Handle);
      fNeedRebuildBackground := false;
   END;
   BitBlt(Image.Canvas.Handle, 0, 0, Width, Height, BackImage.Canvas.Handle, 0,
      0, SRCCOPY);
   WITH Image.Canvas DO
   BEGIN
      dec(r.bottom);
      dec(r.right);
      r := DrawBoxEx(Handle, r, [fsdLeft, fsdTop, fsdRight, fsdBottom],
         FBevelInner, FBevelOuter,
         FBevelBold, Colors.Background, fpoTransparent IN Options);

      //    PercentWidth := trunc( Width * Percent / 100 );
      //    PercentWidth := Width;
      Brush.Color := Colors.Background;
      inc(r.top);
      IF Percent > 0 THEN
      BEGIN
         GradientBox(handle, r, FGradientB, integer(psSolid), 1);
         GradientBox(handle, r, FGradientF, integer(psSolid), 1);
         x := r.left;
         IF NOT (fpoTransparent IN Options) THEN
            FOR i := r.left TO Width DIV (FStep + FInterspace) + 1 DO
            BEGIN
               x2 := x + FInterspace;
               IF x2 > r.right THEN
                  IF x < r.right THEN
                     x2 := r.right
                  ELSE
                     break;
               FillRect(Rect(x, r.top, x2, r.Bottom));
               inc(x, FStep + FInterspace);
            END;

      END;
      //...CALC POSITION
      TRY
         Capt := Format(Caption, [Percent]);
      EXCEPT Capt := Caption;
      END;
      GetTextExtentPoint32(Self.Canvas.Handle, PChar(Capt), length(Capt), Size);

      x := 2;
      y := 0;
      //  Size.cx:=Size.cx+2+trunc(Size.cx*0.01);
      //  Size.cy := Size.cy+2;
      TextSize := Size;
      IF (FCaptionStyle = fstShadow) OR (FCaptionStyle = fstVolumetric) THEN
      BEGIN
         inc(Size.cy, ShadowDepth);
         inc(Size.cx, ShadowDepth);
      END;
      IF fpoDelineatedText IN FOptions THEN
      BEGIN
         inc(Size.cy, 2);
         inc(Size.cx, 2);
      END;

      CASE FCaptionDirection OF
         fldLeftRight:
            BEGIN
               CASE FCaptionAlignment OF
                  taCenter: x := (Width - Size.cx) DIV 2;
                  taRightJustify: x := Width - Size.cx;
               END;
               y := (Height - Size.cy) DIV 2;
            END;
         fldRightLeft:
            BEGIN
               CASE FCaptionAlignment OF
                  taCenter: x := (Width + Size.cx) DIV 2;
                  taLeftJustify: x := Width - (Size.cx - TextSize.cx) - 2;
               ELSE
                  x := TextSize.cx;
               END;
               y := TextSize.cy;
            END;
         fldDownUp:
            BEGIN
               CASE FCaptionAlignment OF
                  taCenter: y := (Height + TextSize.cx - (Size.cy - TextSize.cy))
                     DIV 2;
                  taRightJustify: y := TextSize.cx - 4;
               ELSE
                  y := Height - (Size.cy - TextSize.cy) - 2;
               END;
            END;
         fldUpDown:
            BEGIN
               CASE FCaptionAlignment OF
                  taCenter: y := (Height - Size.cx) DIV 2;
                  taRightJustify: y := Height - Size.cx;
               ELSE
                  y := 1;
               END;
               x := TextSize.cy;
            END;

      END;
      //...CALC POSITION end

      ExtTextOutExt(Handle, x, y, GetClientRect, Capt,
         FCaptionStyle, fpoDelineatedText IN FOptions,
         false, Self.Font.Color, FColors.Delineate,
         FColors.Highlight, FColors.Shadow,
         NIL, NIL, Self.Font);

   END;
   Canvas.Draw(0, 0, Image);

END;

PROCEDURE TJvgProgress.OnSmthChanged(Sender: TObject);
BEGIN
   Repaint;
END;
//...______________________________________________PROPERTIES METHODS

PROCEDURE TJvgProgress.SetBevelOuter(Value: TPanelBevel);
BEGIN
   FBevelOuter := Value;
   Repaint;
END;

PROCEDURE TJvgProgress.SetBevelInner(Value: TPanelBevel);
BEGIN
   FBevelInner := Value;
   Invalidate;
END;

PROCEDURE TJvgProgress.SetBevelBold(Value: boolean);
BEGIN
   FBevelBold := Value;
   Repaint;
END;

PROCEDURE TJvgProgress.SetPercent(Value: TglPercent);
BEGIN
   IF FPercent = Value THEN
      exit;
   FPercent := Value;
   FGradientF.PercentFilling := FPercent;
END;

PROCEDURE TJvgProgress.SetCaptionAlignment(Value: TAlignment);
BEGIN
   FCaptionAlignment := Value;
   Repaint;
END;

PROCEDURE TJvgProgress.SetCaptionDirection(Value: TglLabelDir);
BEGIN
   FCaptionDirection := Value;
   Repaint;
END;

PROCEDURE TJvgProgress.SetCaptionStyle(Value: TglTextStyle);
BEGIN
   FCaptionStyle := Value;
   Repaint;
END;

PROCEDURE TJvgProgress.SetStep(Value: integer);
BEGIN
   FStep := Value;
   Repaint;
END;

PROCEDURE TJvgProgress.SetInterspace(Value: integer);
BEGIN
   FInterspace := Value;
   Repaint;
END;

PROCEDURE TJvgProgress.SetOptions(Value: TglProgressOptions);
BEGIN
   FOptions := Value;
   Repaint;
END;

END.

