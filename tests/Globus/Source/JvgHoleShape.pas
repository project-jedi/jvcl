{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHoleShape.PAS, released on 2003-01-15.

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

UNIT JvgHoleShape;

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
   TRGNCombineMode = (cmAND, cmCOPY, cmDIFF, cmOR, cmXOR);
   THoleShapeType = (stRectangle, stSquare, stRoundRect, stRoundSquare,
      stEllipse, stCircle);
   TJvgHoleShape = CLASS(TJvGraphicControl)
   PRIVATE
      FShape: THoleShapeType;
      FShapeBitmap: TBitmap;
      FBevelInner: TPanelBevel;
      FBevelOuter: TPanelBevel;
      FBoldInner: boolean;
      FBoldOuter: boolean;
      FRectEllipse: TJvgPointClass;
      FBevelOffset: integer;
      fNeedUpdateRGN: boolean;
      fDestroyed: boolean;
      fRunOnce: boolean;
      fNeedRebuildBitmapShape: boolean;
      OldX, OldY, OldW, OldH: integer;
      PROCEDURE SetEnabled(Value: boolean);
      PROCEDURE SetEnabledDT(Value: boolean);
      PROCEDURE SetShape(Value: THoleShapeType);
      PROCEDURE SetShapeBitmap(Value: TBitmap);
      PROCEDURE SetBevelInner(Value: TPanelBevel);
      PROCEDURE SetBevelOuter(Value: TPanelBevel);
      PROCEDURE SetBoldInner(Value: boolean);
      PROCEDURE SetBoldOuter(Value: boolean);
      PROCEDURE SetCombineMode(Value: TRGNCombineMode);
      PROCEDURE SetBevelOffset(Value: integer);

      PROCEDURE Update_;
      PROCEDURE CalcRGNs;
      PROCEDURE SmthChanged(Sender: TObject);
      PROCEDURE SayAllDTEnabledState(EnabledDT: boolean);
   PROTECTED
      PROCEDURE Paint; OVERRIDE;
   PUBLIC
      RGNOuter, RGNInner: HRGN;
      FCombineMode: TRGNCombineMode;
      FEnabledDT: boolean;
      FEnabled: boolean;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE UpdateRGN;
      PROCEDURE Loaded; OVERRIDE;
   PUBLISHED
      PROPERTY Align;
      PROPERTY ShowHint;
      PROPERTY ParentShowHint;
      PROPERTY PopupMenu;
      //    property Visible;
      PROPERTY Enabled: boolean READ FEnabled WRITE SetEnabled
         DEFAULT true;
      PROPERTY EnabledAllInDesignTime: boolean READ FEnabledDT WRITE SetEnabledDT
         DEFAULT true;
      PROPERTY Shape: THoleShapeType READ FShape WRITE SetShape
         DEFAULT stEllipse;
      PROPERTY BevelInner: TPanelBevel READ FBevelInner WRITE SetBevelInner
         DEFAULT bvNone;
      PROPERTY BevelOuter: TPanelBevel READ FBevelOuter WRITE SetBevelOuter
         DEFAULT bvLowered;
      PROPERTY BevelInnerBold: boolean READ FBoldInner WRITE SetBoldInner
         DEFAULT true;
      PROPERTY BevelOuterBold: boolean READ FBoldOuter WRITE SetBoldOuter
         DEFAULT true;
      PROPERTY CombineMode: TRGNCombineMode READ FCombineMode WRITE
         SetCombineMode
         DEFAULT cmDIFF;
      PROPERTY BevelOffset: integer READ FBevelOffset WRITE SetBevelOffset
         DEFAULT 0;
      PROPERTY RectEllipse: TJvgPointClass READ FRectEllipse WRITE FRectEllipse;
      PROPERTY ShapeBitmap: TBitmap READ FShapeBitmap WRITE SetShapeBitmap;
   END;

PROCEDURE Register;

IMPLEMENTATION
CONST
   aCombMode                     : ARRAY[0..4] OF integer = (RGN_AND, RGN_COPY,
      RGN_DIFF, RGN_OR, RGN_XOR);
   {~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

CONSTRUCTOR TJvgHoleShape.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FShapeBitmap := TBitmap.Create;
   FEnabled := (Owner IS TWinControl);

   ControlStyle := ControlStyle - [csOpaque];
   FEnabledDT := FEnabled;
   fDestroyed := false;
   FRectEllipse := TJvgPointClass.Create;
   FRectEllipse.x := 30;
   FRectEllipse.y := 30;
   FRectEllipse.OnChanged := SmthChanged;
   FShape := stEllipse;
   FBevelOuter := bvLowered;
   FBevelInner := bvNone;
   FCombineMode := cmDIFF;
   FBoldInner := true;
   FBoldOuter := true;
   FRectEllipse.y := 45;
   FRectEllipse.x := 45;
   FBevelOffset := 0;
   Width := 112;
   Height := 112;
   fNeedUpdateRGN := false;
   fRunOnce := true;
END;

DESTRUCTOR TJvgHoleShape.Destroy;
BEGIN
   FShapeBitmap.Free;
   FRectEllipse.Free;
   IF NOT (csDestroying IN Owner.ComponentState) THEN
   BEGIN
      FEnabledDT := false;
      FEnabled := false;
      UpdateRGN();
   END;
   INHERITED;
END;

PROCEDURE TJvgHoleShape.Paint;
VAR
   r                          : TRect;
   H, W, EH, EW, i            : integer;

   PROCEDURE DrawShape(Bevel: TPanelBevel; fBold, fRect: boolean);  //_______LOCAL PROC_

      PROCEDURE SetPenAndBrush(c: Tcolor);
      BEGIN
         Canvas.Pen.Color := c;
         IF fRect AND ((EW AND EH) = 0) THEN
         BEGIN
            Canvas.Brush.Style := bsClear;
         END
         ELSE
         BEGIN
            Canvas.Brush.Color := c;
         END
      END;
   BEGIN
      Canvas.Brush.Style := bsClear;    //bsSolid;//bsClear;
      i := integer(fBold);
      WITH Canvas DO
         CASE Bevel OF
            bvLowered:
               BEGIN
                  SetPenAndBrush(clBtnHighlight);
                  IF fRect THEN
                     RoundRect(R.Left, R.Top, R.Right, R.Bottom, EW, EH)
                  ELSE
                     Ellipse(R.Left, R.Top, R.Right, R.Bottom);
                  SetPenAndBrush(clBtnShadow);
                  IF fRect THEN
                     RoundRect(R.Left, R.Top, R.Right - 1, R.Bottom - 1, EW, EH)
                  ELSE
                     Ellipse(R.Left, R.Top, R.Right - 1, R.Bottom - 1);
                  IF FBold THEN
                  BEGIN
                     SetPenAndBrush(cl3DDkShadow);
                     IF fRect THEN
                        RoundRect(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom -
                           1, EW, EH)
                     ELSE
                        Ellipse(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom -
                           1);
                  END;
                  InflateRect(R, -1, -1);
                  inc(R.Left, i);
                  inc(R.Top, i);
               END;
            bvRaised:
               BEGIN
                  SetPenAndBrush(clBtnHighlight);
                  IF fRect THEN
                     RoundRect(R.Left, R.Top, R.Right, R.Bottom, EW, EH)
                  ELSE
                     Ellipse(R.Left, R.Top, R.Right, R.Bottom);
                  IF FBold THEN
                  BEGIN
                     SetPenAndBrush(cl3DDkShadow);
                     IF fRect THEN
                        RoundRect(R.Left + 1, R.Top + 1, R.Right, R.Bottom, EW,
                           EH)
                     ELSE
                        Ellipse(R.Left + 1, R.Top + 1, R.Right, R.Bottom);
                  END;
                  SetPenAndBrush(clBtnShadow);
                  IF fRect THEN
                     RoundRect(R.Left + 1, R.Top + 1, R.Right - i, R.Bottom - i,
                        EW, EH)
                  ELSE
                     Ellipse(R.Left + 1, R.Top + 1, R.Right - i, R.Bottom - i);
                  InflateRect(R, -1, -1);
                  dec(R.Right, i);
                  dec(R.Bottom, i);
               END;
         ELSE
            BEGIN
               //Brush.Color:=clBlack;
               //FrameRect( Rect(Left, Top, Left+W, Top+H) );
            END;
         END;
      SetPenAndBrush(clBtnFace);

   END; //____________________________________END LOCAL PROC_

BEGIN //_________________________________________________________PAINT_
   fNeedUpdateRGN := fNeedUpdateRGN OR (OldX <> Left) OR (OldY <> Top) OR (OldW
      <> Width) OR (OldH <> Height);

   IF fNeedUpdateRGN THEN
      UpdateRGN();
   OldX := Left;
   OldY := Top;
   OldW := Width;
   OldH := Height;

   IF IsItAFilledBitmap(FShapeBitmap) THEN
   BEGIN
      BitBlt(Canvas.handle, -1, -1, Width, Height, FShapeBitmap.Canvas.handle,
         0, 0, SRCCopy);
      exit;
   END;

   CASE FShape OF
      stRectangle, stRoundRect, stEllipse:
         BEGIN
            H := Height;
            W := Width;
         END
   ELSE
      BEGIN
         H := min(Height, Width);
         W := H;
      END;
   END;
   R := Bounds(0, 0, W, H);
   WITH Canvas DO
      CASE FShape OF
         stRectangle, stSquare, stRoundRect, stRoundSquare:
            BEGIN
               IF (FShape = stRectangle) OR (FShape = stSquare) THEN
               BEGIN
                  EW := 0;
                  EH := 0;
               END;
               IF (FShape = stRoundRect) OR (FShape = stRoundSquare) THEN
               BEGIN
                  EW := FRectEllipse.x;
                  EH := FRectEllipse.y;
               END;

               DrawShape(FBevelOuter, FBoldOuter, true);
               InflateRect(R, -FBevelOffset, -FBevelOffset);
               DrawShape(FBevelInner, FBoldInner, true);

               //Pen.Color:=clBtnFace;
               //Rect( R.Left, R.Top, R.Right, R.Bottom );
            END;
         stEllipse, stCircle:
            BEGIN
               DrawShape(FBevelOuter, FBoldOuter, false);
               InflateRect(R, -FBevelOffset, -FBevelOffset);
               DrawShape(FBevelInner, FBoldInner, false);
            END;
      END;
END;
//-------------------------------------------------------

PROCEDURE TJvgHoleShape.CalcRGNs;
VAR
   H, W, xOffs, yOffs         : integer;
   R                          : TRect;
   BmpInfo                    : Windows.TBitmap;
   BorderStyle                : TFormBorderStyle;

   PROCEDURE CalcShape(Bevel: TPanelBevel; fBold: boolean); //____LOCAL PROC_
   VAR
      i                       : integer;
   BEGIN
      i := integer(fBold);
      CASE Bevel OF
         bvLowered:
            BEGIN
               InflateRect(R, -1, -1);
               inc(R.Left, i);
               inc(R.Top, i);
            END;
         bvRaised:
            BEGIN
               InflateRect(R, -1, -1);
               dec(R.Right, i);
               dec(R.Bottom, i);
            END;
      END;
   END; //____________________________________END LOCAL PROC_

   PROCEDURE CalcBmpRgn(VAR rgn: HRGN);
   VAR
      i, j                    : integer;
      rgn2                    : HRGN;
      TransparentColor        : TColor;
   BEGIN
      TransparentColor := FShapeBitmap.Canvas.Pixels[0, FShapeBitmap.Height -
         1];
      FOR j := 0 TO FShapeBitmap.Height DO
         FOR i := 0 TO FShapeBitmap.Width DO
         BEGIN
            IF FShapeBitmap.Canvas.Pixels[i, j] <> TransparentColor THEN
               continue;
            RGN2 := CreateRectRgn(i, j, i + 1, j + 1);
            CombineRgn(RGN, RGN2, RGN, RGN_OR);
            DeleteObject(RGN2);
         END;
   END; //____________________________________END LOCAL PROC_
BEGIN
   IF NOT FShapeBitmap.Empty THEN
   BEGIN
      {if fNeedRebuildBitmapShape then} WITH FShapeBitmap DO
      BEGIN
         GetObject(FShapeBitmap.Handle, sizeof(Windows.TBitmap), @BmpInfo);
         DeleteObject(RGNOuter);
         DeleteObject(RGNInner);
         RGNInner := CreateRectRgn(0, 0, 0, 0);
         CalcBmpRgn(RGNInner);
         fNeedRebuildBitmapShape := false;
      END;
   END
   ELSE
   BEGIN
      CASE FShape OF
         stRectangle, stRoundRect, stEllipse:
            BEGIN
               H := Height;
               W := Width;
            END
      ELSE
         BEGIN
            H := min(Height, Width);
            W := H;
         END;
      END;
      R := Bounds(0, 0, W, H);
      DeleteObject(RGNOuter);
      DeleteObject(RGNInner);

      IF FBevelOffset <> 0 THEN
      BEGIN
         CalcShape(FBevelOuter, FBoldOuter);
         OffsetRect(R, 1, 1);
      END;
      CASE FShape OF
         stRectangle, stSquare:
            RGNOuter := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
         stRoundRect, stRoundSquare:
            RGNOuter := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom,
               FRectEllipse.x, FRectEllipse.y);
         stEllipse, stCircle:
            RGNOuter := CreateEllipticRgn(R.Left, R.Top, R.Right, R.Bottom);
      END;
      IF FBevelOffset = 0 THEN
         CalcShape(FBevelOuter, FBoldOuter);
      InflateRect(R, -FBevelOffset, -FBevelOffset);
      IF FBevelOffset = 0 THEN
         CalcShape(FBevelInner, FBoldInner)
      ELSE
         OffsetRect(R, -1, -1);
      CASE FShape OF
         stRectangle, stSquare:
            RGNInner := CreateRectRgn(R.Left + 1, R.Top + 1, R.Right + 1,
               R.Bottom + 1);
         stRoundRect, stRoundSquare:
            RGNInner := CreateRoundRectRgn(R.Left + 1, R.Top + 1, R.Right + 2,
               R.Bottom + 2, FRectEllipse.x, FRectEllipse.y);
         stEllipse, stCircle:
            RGNInner := CreateEllipticRgn(R.Left + 1, R.Top + 1, R.Right + 2,
               R.Bottom + 2);
      END;
   END;

   { calc offsets }
   IF Owner IS TForm THEN
   BEGIN
      IF csDesigning IN ComponentState THEN
         BorderStyle := bsSizeable
      ELSE
         BorderStyle := TForm(Owner).BorderStyle;
      CASE BorderStyle OF
         bsSizeable:
            BEGIN
               xOffs := GetSystemMetrics(SM_CXFRAME) - 1;
               yOffs := GetSystemMetrics(SM_CYFRAME) - 1;
               inc(yOffs, GetSystemMetrics(SM_CYCAPTION));
            END;
         bsDialog:
            BEGIN
               xOffs := GetSystemMetrics(SM_CXDLGFRAME) - 1;
               yOffs := GetSystemMetrics(SM_CYDLGFRAME) - 1;
               inc(yOffs, GetSystemMetrics(SM_CYCAPTION));
            END;
         bsSingle:
            BEGIN
               xOffs := GetSystemMetrics(SM_CXBORDER);
               yOffs := GetSystemMetrics(SM_CYBORDER);
               inc(yOffs, GetSystemMetrics(SM_CYCAPTION));
            END;
         bsToolWindow:
            BEGIN
               xOffs := GetSystemMetrics(SM_CXBORDER);
               yOffs := GetSystemMetrics(SM_CYBORDER);
               inc(yOffs, GetSystemMetrics(SM_CYSMCAPTION));
            END;
         bsSizeToolWin:
            BEGIN
               xOffs := GetSystemMetrics(SM_CXSIZEFRAME);
               yOffs := GetSystemMetrics(SM_CYSIZEFRAME);
               inc(yOffs, GetSystemMetrics(SM_CYSMCAPTION));
            END;
      ELSE
         BEGIN
            xOffs := -1;
            yOffs := -1;
         END;
      END;

      OffsetRgn(RGNInner, Left + xOffs, Top + yOffs);
      OffsetRgn(RGNOuter, Left + xOffs, Top + yOffs);
   END;

   fRunOnce := false;
END;
//-------------------------------------------------------
//...set all enabled/disabled in design time

PROCEDURE TJvgHoleShape.SayAllDTEnabledState(EnabledDT: boolean);
VAR
   i                          : integer;
BEGIN
   FOR i := 0 TO TWinControl(Owner).ControlCount - 1 DO
      WITH TWinControl(Owner) DO
      BEGIN
         IF (Controls[i] IS TJvgHoleShape) THEN
         BEGIN
            TJvgHoleShape(Controls[i]).FEnabledDT := EnabledDT;
         END;
      END;

END;
//-------------------------------------------------------

PROCEDURE TJvgHoleShape.UpdateRGN;
VAR
   i                          : integer;
   NewRGN                     : HRGN;
BEGIN
   IF NOT (Owner IS TWinControl) THEN
      exit;
   NewRGN := CreateRectRgn(0, 0, 2000, 1000);

   FOR i := 0 TO TWinControl(Owner).ControlCount - 1 DO
      WITH TWinControl(Owner) DO
      BEGIN
         IF Controls[i] IS TJvgHoleShape THEN
            WITH TJvgHoleShape(Controls[i]) DO
               IF ((csDesigning IN ComponentState) AND FEnabledDT)
                  OR ((NOT (csDesigning IN ComponentState)) AND FEnabled) THEN
               BEGIN
                  CalcRGNs;
                  CombineRgn(NewRGN, NewRGN, RGNInner,
                     aCombMode[integer(FCombineMode)])
               END;
      END;

   SetWindowRgn(TWinControl(Owner).Handle, NewRGN, true);
   fNeedUpdateRGN := false;
END;

PROCEDURE TJvgHoleShape.Update_;
BEGIN
   IF csLoading IN ComponentState THEN
      exit;
   UpdateRGN();
   Refresh;
END;

PROCEDURE TJvgHoleShape.SmthChanged(Sender: TObject);
BEGIN
   Update_;
END;

//________________________________________________________ Properties _

PROCEDURE TJvgHoleShape.SetEnabled(Value: boolean);
BEGIN
   IF (FEnabled = Value) OR NOT (Owner IS TWinControl) THEN
      exit;
   FEnabled := Value;
   Update_;
END;

PROCEDURE TJvgHoleShape.SetEnabledDT(Value: boolean);
BEGIN
   IF (FEnabledDT = Value) OR NOT (Owner IS TWinControl) THEN
      exit;
   FEnabledDT := Value;
   SayAllDTEnabledState(FEnabledDT);
   Update_;
END;

PROCEDURE TJvgHoleShape.SetShape(Value: THoleShapeType);
BEGIN
   IF FShape = Value THEN
      exit;
   FShape := Value;
   Update_;
END;

PROCEDURE TJvgHoleShape.SetShapeBitmap(Value: TBitmap);
BEGIN
   IF FShapeBitmap = Value THEN
      exit;
   fNeedRebuildBitmapShape := true;
   FShapeBitmap.Assign(Value);
   IF Assigned(FShapeBitmap) THEN
   BEGIN
      Width := FShapeBitmap.Width;
      Height := FShapeBitmap.Width;
   END;
   Update_();
END;

PROCEDURE TJvgHoleShape.SetBevelInner(Value: TPanelBevel);
BEGIN
   IF FBevelInner = Value THEN
      exit;
   FBevelInner := Value;
   Update_;
END;

PROCEDURE TJvgHoleShape.SetBevelOuter(Value: TPanelBevel);
BEGIN
   IF FBevelOuter = Value THEN
      exit;
   FBevelOuter := Value;
   Update_;
END;

PROCEDURE TJvgHoleShape.SetBoldInner(Value: boolean);
BEGIN
   IF FBoldInner = Value THEN
      exit;
   FBoldInner := Value;
   Update_;
END;

PROCEDURE TJvgHoleShape.SetBoldOuter(Value: boolean);
BEGIN
   IF FBoldOuter = Value THEN
      exit;
   FBoldOuter := Value;
   Update_;
END;

PROCEDURE TJvgHoleShape.SetCombineMode(Value: TRGNCombineMode);
BEGIN
   IF FCombineMode = Value THEN
      exit;
   FCombineMode := Value;
   Update_;
END;

PROCEDURE TJvgHoleShape.SetBevelOffset(Value: integer);
BEGIN
   IF (FBevelOffset = Value) OR (Value < 0) THEN
      exit;
   IF (Value > width - 2) OR (Value > height - 2) THEN
      Value := min(width, height) - 2;
   FBevelOffset := Value;
   Update_;
END;

PROCEDURE TJvgHoleShape.Loaded;
BEGIN
   INHERITED;
   fNeedRebuildBitmapShape := true;
   UpdateRGN();
   Refresh;
END;

END.

