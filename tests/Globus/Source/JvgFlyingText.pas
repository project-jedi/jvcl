{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgFlyingText.PAS, released on 2003-01-15.

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

UNIT JvgFlyingText;

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
   TMyRect = RECORD
      Left, Top, Width, Height: integer;
   END;

   TTextLineChangingEvent = PROCEDURE(Sender: TObject; LineNum: integer) OF
      OBJECT;

   TJvgFlyingText = CLASS(TJvCustomPanel)
   PRIVATE
      FHorAlign: TglHorAlign;
      FVertAlign: TglVertAlign;
      FTransparent: boolean;
      FBackgrColor: TColor;
      FGradient: TJvgGradient;
      FThreeDGradient: TJvg3DGradient;
      FInteriorOffset: word;
      FScalePercent: TSpPercent;
      FStepScaleFactor: single;
      FResultFont: TFont;
      FTimerInterval: word;
      FActive: boolean;
      FClearOldText: boolean;
      FClearOldTextWhileDrawing: boolean;
      FCycled: boolean;
      FText: TStringList;
      FView3D: TJvg2DAlign;
      FDirection: TglScalingDir;
      FFastDraw: boolean;
      FShowTextWhilePassive: boolean;

      FOnTextLineChanging: TTextLineChangingEvent;

      FR, OldFR: TMyRect;
      FP, OldFP: TPoint;
      FBackgrBitmap,
         FTxtBitmap,
         FResBitmap,
         FScaledTxtBitmap,
         FPartTxtBitmap: TBitmap;
      fNeedRebuildBitmaps: boolean;
      FTimer: TTimer;
      FScaledWidth,
         FScaledHeight,
         FOldScaledWidth,
         FOldScaledHeight: single;
      FVisible: boolean;
      FStepShift: TJvgPointClass;

      Shift, OldShift: TPoint;
      uCurTextLine: Word;
      fNeedRepaintBackground: boolean;
      fNeedRemakeBackground: boolean;
      fLoaded: boolean;
      PROCEDURE CalcTxtBitmapWidth;
      PROCEDURE BuildBitmaps;
      PROCEDURE BuildTxtBitmap;
      PROCEDURE WMSize(VAR Message: TWMSize); MESSAGE WM_SIZE;
      PROCEDURE OnParamsChanged(Sender: TObject);
      PROCEDURE Repaint_;

      PROCEDURE SetHorAlign(Value: TglHorAlign);
      PROCEDURE SetVertAlign(Value: TglVertAlign);
      PROCEDURE SetTransparent(Value: boolean);
      PROCEDURE SetBackgrColor(Value: TColor);
      PROCEDURE SetInteriorOffset(Value: word);
      PROCEDURE SetScalePercent(Value: TSpPercent);
      PROCEDURE SetStepScaleFactor(Value: Single);
      PROCEDURE SetResultFont(Value: TFont);
      PROCEDURE SetTimerInterval(Value: word);
      PROCEDURE SetActive(Value: boolean);
      PROCEDURE SetText(Value: TStringList);
      PROCEDURE SetFastDraw(Value: boolean);
      PROCEDURE SetDirection(Value: TglScalingDir);
      PROCEDURE SetShowTextWhilePassive(Value: boolean);
      PROCEDURE SetVisible(Value: boolean);

   PROTECTED
      PROPERTY Color;                   //...hide
      PROCEDURE Paint; OVERRIDE;

   PUBLIC
      PROPERTY Canvas;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

      PROCEDURE OnTimerProc(Sender: TObject);
      PROCEDURE RepaintBackground;      //...for users
      PROCEDURE RemakeBackground;       //...for users
   PUBLISHED
      {$IFDEF COMPILER5_UP}
      PROPERTY Anchors;
      {$ENDIF}
      PROPERTY Align;
      PROPERTY AlignTextHorizontal: TglHorAlign READ FHorAlign WRITE SetHorAlign
         DEFAULT fhaCenter;
      PROPERTY AlignTextVertical: TglVertAlign READ FVertAlign WRITE SetVertAlign
         DEFAULT fvaCenter;
      PROPERTY Transparent: boolean READ FTransparent WRITE SetTransparent
         DEFAULT false;
      PROPERTY BackgrColor: TColor READ FBackgrColor WRITE SetBackgrColor
         DEFAULT clBlack;
      PROPERTY Gradient: TJvgGradient READ FGradient WRITE FGradient;
      PROPERTY Gradient3D: TJvg3DGradient READ FThreeDGradient WRITE
         FThreeDGradient;
      PROPERTY InteriorOffset: word READ FInteriorOffset WRITE SetInteriorOffset
         DEFAULT 0;
      PROPERTY ScalePercent: TSpPercent READ FScalePercent WRITE SetScalePercent
         DEFAULT 5;
      PROPERTY StepScaleFactor: single READ FStepScaleFactor WRITE
         SetStepScaleFactor;
      PROPERTY ResultFont: TFont READ FResultFont WRITE SetResultFont;
      PROPERTY TimerInterval: word READ FTimerInterval WRITE SetTimerInterval
         DEFAULT 10;
      PROPERTY Active: boolean READ FActive WRITE SetActive
         DEFAULT false;
      PROPERTY ClearOldText: boolean READ FClearOldText WRITE FClearOldText
         DEFAULT true;
      PROPERTY ClearOldTextWhileDrawing: boolean
         READ FClearOldTextWhileDrawing WRITE FClearOldTextWhileDrawing
         DEFAULT true;
      PROPERTY Cycled: boolean READ FCycled WRITE FCycled
         DEFAULT false;
      PROPERTY Text: TStringList READ FText WRITE SetText;
      PROPERTY FastDraw: boolean READ FFastDraw WRITE SetFastDraw
         DEFAULT true;
      PROPERTY View3D: TJvg2DAlign READ FView3D WRITE FView3D;
      PROPERTY Direction: TglScalingDir READ FDirection WRITE SetDirection
         DEFAULT fsdRaising;
      PROPERTY ShowTextWhilePassive: boolean
         READ FShowTextWhilePassive WRITE SetShowTextWhilePassive DEFAULT true;
      PROPERTY OnTextLineChanging: TTextLineChangingEvent
         READ FOnTextLineChanging WRITE FOnTextLineChanging;
      PROPERTY Visible: boolean READ FVisible WRITE SetVisible
         DEFAULT true;
      PROPERTY StepShift: TJvgPointClass READ FStepShift WRITE FStepShift;
   END;

IMPLEMENTATION

//*****************************************_____________LowLevel METHODS
//________________________________________________________

CONSTRUCTOR TJvgFlyingText.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);

   Visible := false;
   Width := 105;
   Height := 105;

   FTxtBitmap := TBitmap.create;
   FBackgrBitmap := TBitmap.create;
   FResBitmap := TBitmap.create;
   FScaledTxtBitmap := TBitmap.create;
   FPartTxtBitmap := TBitmap.create;
   FResultFont := TFont.Create;
   FTimer := TTimer.Create(self);
   FGradient := TJvgGradient.Create;
   FThreeDGradient := TJvg3DGradient.Create;
   FText := TStringList.Create;
   FView3D := TJvg2DAlign.Create;
   FStepShift := TJvgPointClass.Create;
   uCurTextLine := 0;

   FThreeDGradient.FromColor := clBlack;
   FThreeDGradient.ToColor := clGray;

   FGradient.Active := true;
   FGradient.FromColor := clWhite;

   FGradient.OnChanged := OnParamsChanged;
   FThreeDGradient.OnChanged := OnParamsChanged;
   FView3D.OnChanged := OnParamsChanged;

   FTimer.Enabled := false;
   FTimer.OnTimer := OnTimerProc;
   FTimer.Interval := 10;

   //...defaults
   FHorAlign := fhaCenter;
   FVertAlign := fvaCenter;
   FTransparent := false;
   FBackgrColor := clBlack;
   FInteriorOffset := 0;
   FScalePercent := 5;
   FStepScaleFactor := 1.1;

   FResultFont.Color := clWhite;
   FResultFont.Name := 'Times';
   FResultFont.Style := [fsBold];
   FResultFont.size := 90;
   FResultFont.Color := clWhite;

   FTimerInterval := 10;
   FActive := false;
   FClearOldText := true;
   FClearOldTextWhileDrawing := true;
   FCycled := false;
   FFastDraw := true;
   FDirection := fsdRaising;
   FShowTextWhilePassive := true;
   FVisible := true;
   FStepShift.x := 0;
   FStepShift.y := 0;
   Shift := Point(0, 0);
   FText.Add('Hello');
   FText.Add('World');

   FScaledTxtBitmap.Canvas.Brush.Color := clBlack;
   FScaledTxtBitmap.Canvas.Brush.Style := bsSolid;
   FResBitmap.Canvas.Brush.Color := clBlack;
   FResBitmap.Canvas.Brush.Style := bsSolid;
   FPartTxtBitmap.Canvas.Brush.Color := clBlack;
   FPartTxtBitmap.Canvas.Brush.Style := bsSolid;

   fNeedRebuildBitmaps := true;
   fNeedRemakeBackground := false;
   fLoaded := false;
END;
//________________________________________________________

DESTRUCTOR TJvgFlyingText.Destroy;
BEGIN
   FResBitmap.Free;
   FBackgrBitmap.Free;
   FTxtBitmap.Free;
   FScaledTxtBitmap.Free;
   FPartTxtBitmap.Free;
   FResultFont.Free;
   FGradient.Free;
   FThreeDGradient.Free;
   FText.Free;
   FView3D.Free;
   FStepShift.Free;
   INHERITED Destroy;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.Paint;
VAR
   r                          : TRect;
   //~~~~~~~~~~~~~~~~~LOCAL PROC

   PROCEDURE CalcPos(VAR FR: TMyRect; VAR FP: TPoint;
      FScaledWidth, FScaledHeight: integer; Shift: TPoint);
   BEGIN
      CASE FHorAlign OF
         fhaLeft:
            BEGIN
               FR.Left := 0;
               FP.x := 0;
            END;
         fhaCenter:
            BEGIN
               FR.Left := max(0, (Width - FScaledWidth) DIV 2);
               FP.x := max(0, -(Width - FScaledWidth) DIV 2);
            END;
         fhaRight:
            BEGIN
               FR.Left := max(0, Width - FScaledWidth);
               FP.x := max(0, FScaledWidth - Width);
            END;
      END;
      CASE FVertAlign OF
         fvaTop:
            BEGIN
               FR.Top := 0;
               FP.y := 0;
            END;
         fvaCenter:
            BEGIN
               FR.Top := max(0, (Height - FScaledHeight) DIV 2);
               FP.y := max(0, -(Height - FScaledHeight) DIV 2);
            END;
         fvaBottom:
            BEGIN
               FR.Top := max(0, Height - FScaledHeight);
               FP.y := max(0, FScaledHeight - Height);
            END;
      END;
      FR.Left := FR.Left + Shift.x;
      FR.Top := FR.Top + Shift.y;
      FR.Width := min(Width, FScaledWidth);
      FR.Height := min(Height, FScaledHeight);
   END;
   //~~~~~~~~~~~~~~~~~END LOCAL PROC
BEGIN
   //fNeedRebuildBitmaps := (OldLeft<>Left)or(OldTop<>Top);
   //OldLeft := Left; OldTop := Top;
   IF fNeedRebuildBitmaps THEN
   BEGIN
      fLoaded := true;
      fNeedRemakeBackground := true;
      BuildBitmaps;
      fNeedRebuildBitmaps := false;
      Visible := FVisible;
   END;

   StretchBlt(FScaledTxtBitmap.Canvas.Handle, 0, 0,
      trunc(FScaledWidth), trunc(FScaledHeight),
      FTxtBitmap.canvas.Handle, 0, 0,
      FTxtBitmap.Width, FTxtBitmap.Height, SRCCOPY);

   CalcPos(FR, FP, trunc(FScaledWidth), trunc(FScaledHeight), Shift);
   CalcPos(OldFR, OldFP, trunc(FOldScaledWidth),
      trunc(FOldScaledHeight), OldShift);

   IF FFastDraw THEN
   BEGIN
      IF fNeedRepaintBackground THEN
      BEGIN
         IF FClearOldText OR NOT FActive THEN
            BitBlt(Canvas.Handle, 0, 0, Width, Height,
               FBackgrBitmap.canvas.Handle, 0, 0, SRCCOPY);
      END;
      fNeedRepaintBackground := true;

      FPartTxtBitmap.Width := FR.Width;
      FPartTxtBitmap.Height := FR.Height;
      OldFR.Width := OldFR.Width + abs(Shift.x - OldShift.x);
      OldFR.Height := OldFR.Height + abs(Shift.y - OldShift.y);
      FResBitmap.Width := OldFR.Width;
      FResBitmap.Height := OldFR.Height;

      IF FDirection = fsdRecessing THEN
      BEGIN
         r := rect(0, 0, FPartTxtBitmap.Width, FPartTxtBitmap.Height);
         FillRect(FPartTxtBitmap.Canvas.Handle, r,
            FPartTxtBitmap.Canvas.Brush.Handle);
      END;

      BitBlt(FPartTxtBitmap.Canvas.Handle, 0, 0,
         FR.Width, FR.Height,
         FScaledTxtBitmap.canvas.Handle, FP.x, FP.y, SRCCOPY);
      {PartBackgr}
      IF FClearOldTextWhileDrawing THEN
         BitBlt(FResBitmap.Canvas.Handle, 0, 0, OldFR.Width, OldFR.Height,
            FBackgrBitmap.canvas.Handle, OldFR.Left, OldFR.Top, SRCCOPY)
      ELSE
         BitBlt(FResBitmap.Canvas.Handle, 0, 0, OldFR.Width, OldFR.Height,
            canvas.Handle, OldFR.Left, OldFR.Top, SRCCOPY);

      IF FShowTextWhilePassive OR FActive THEN
         CreateBitmapExt(FResBitmap.Canvas.Handle, FPartTxtBitmap,
            ClientRect,
            (FR.Left - OldFR.Left), (FR.Top - OldFR.Top),
            fwoNone, fdsDefault, FTransparent, 0, clBlack);

      BitBlt(Canvas.Handle, OldFR.Left, OldFR.Top,
         FResBitmap.Width, FResBitmap.Height,
         FResBitmap.canvas.Handle, 0, 0, SRCCOPY);
   END
   ELSE                                 {SLOW DRAWING}
   BEGIN
      FPartTxtBitmap.Width := Width;
      FPartTxtBitmap.Height := Height;
      FResBitmap.Width := Width;
      FResBitmap.Height := Height;

      BitBlt(FResBitmap.Canvas.Handle, 0, 0, Width, Height,
         FBackgrBitmap.canvas.Handle, 0, 0, SRCCOPY);
      IF FDirection = fsdRecessing THEN
      BEGIN
         r := rect(0, 0, FPartTxtBitmap.Width, FPartTxtBitmap.Height);
         FillRect(FPartTxtBitmap.Canvas.Handle, r,
            FPartTxtBitmap.Canvas.Brush.Handle);
      END;
      IF FShowTextWhilePassive OR FActive THEN
      BEGIN
         BitBlt(FPartTxtBitmap.Canvas.Handle,
            FR.Left, FR.Top, FR.Width, FR.Height,
            FScaledTxtBitmap.canvas.Handle, FP.x, FP.y, SRCCOPY);

         CreateBitmapExt(FResBitmap.Canvas.Handle, FPartTxtBitmap, ClientRect,
            0, 0, fwoNone, fdsDefault, FTransparent, 0, clBlack);
      END;
      BitBlt(Canvas.Handle, 0, 0, FResBitmap.Width, FResBitmap.Height,
         FResBitmap.canvas.Handle, 0, 0, SRCCOPY);
   END;
   OldShift := Shift;
   Shift.x := Shift.x + FStepShift.x;
   Shift.y := Shift.y + FStepShift.y;

   IF csDesigning IN ComponentState THEN
      WITH Canvas DO
      BEGIN
         Pen.Color := clBlack;
         Pen.Style := psDash;
         Brush.Style := bsClear;
         Rectangle(0, 0, width, height);
      END;

END;
//________________________________________________________

PROCEDURE TJvgFlyingText.OnTimerProc(Sender: TObject);
BEGIN
   IF (NOT Assigned(FText)) OR (FText.Count = 0) THEN
   BEGIN
      Active := false;
      exit;
   END;

   IF FDirection = fsdRaising THEN
   BEGIN
      FScaledWidth := (FScaledWidth * FStepScaleFactor);
      FScaledHeight := (FScaledHeight * FStepScaleFactor);
      FOldScaledWidth := FScaledWidth;
      FOldScaledHeight := FScaledHeight;
   END
   ELSE                                 { fsdRecession }
   BEGIN
      FOldScaledWidth := FScaledWidth;
      FOldScaledHeight := FScaledHeight;
      FScaledWidth := (FScaledWidth / FStepScaleFactor);
      FScaledHeight := (FScaledHeight / FStepScaleFactor);
   END;

   IF (FScaledWidth < (FTxtBitmap.Width * FScalePercent / 100))
      OR (FScaledWidth > FTxtBitmap.Width)
      OR (FScaledHeight < (FTxtBitmap.Height * FScalePercent / 100))
      OR (FScaledHeight > FTxtBitmap.Height) THEN
   BEGIN
      IF Assigned(FOnTextLineChanging) THEN
         FOnTextLineChanging(self, uCurTextLine);
      IF uCurTextLine < FText.Count - 1 THEN
         inc(uCurTextLine)
      ELSE
      BEGIN
         uCurTextLine := 0;
         IF NOT FCycled THEN
         BEGIN
            Active := false;
            BuildBitmaps;
            exit;
         END;
      END;
      BuildTxtBitmap;
   END
   ELSE
      fNeedRepaintBackground := false;

   Repaint;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.RepaintBackground; //...for users
BEGIN
   fNeedRepaintBackground := true;
   Repaint;
END;

PROCEDURE TJvgFlyingText.RemakeBackground; //...for users
BEGIN
   fNeedRemakeBackground := true;
   Repaint;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.CalcTxtBitmapWidth;
VAR
   Size                       : TSize;
BEGIN
   //if FText.Count <= uCurTextLine then exit;
   WITH FTxtBitmap DO
   BEGIN
      GetTextExtentPoint32(Canvas.handle, PChar(FText[uCurTextLine]),
         Length(FText[uCurTextLine]), Size);
      Width := Size.cx + FThreeDGradient.Depth;
      Height := Size.cy + FThreeDGradient.Depth;
      Shift := Point(0, 0);
      OldShift := Shift;
   END;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.BuildTxtBitmap;
VAR
   R                          : TRect;
   i, x, y                    : integer;
BEGIN
   //if (not Assigned(FText))or(FText.Count=0) then exit;
   FTxtBitmap.Canvas.Font.Assign(FResultFont);
   IF FText.Count <> 0 THEN
      CalcTxtBitmapWidth
   ELSE
   BEGIN
      FTxtBitmap.Width := 0;
      FTxtBitmap.Height := 0;
   END;
   R := rect(0, 0, FTxtBitmap.width, FTxtBitmap.height);

   FTxtBitmap.Canvas.Brush.Color := clBlack;
   FTxtBitmap.Canvas.Brush.Style := bsSolid;
   FTxtBitmap.Canvas.Fillrect(R);

   CASE FView3D.Horizontal OF
      fhaLeft: x := 0;
      fhaCenter: x := FThreeDGradient.Depth DIV 2;
   ELSE                                 {fhaRight}
      x := FThreeDGradient.Depth;
   END;
   CASE FView3D.Vertical OF
      fvaTop: y := 0;
      fvaCenter: y := FThreeDGradient.Depth DIV 2;
   ELSE                                 {fvaBottom}
      y := FThreeDGradient.Depth;
   END;

   SetBkMode(FTxtBitmap.Canvas.Handle, integer(TRANSPARENT));
   FOR i := 0 TO FThreeDGradient.Depth - 1 DO
   BEGIN
      IF FThreeDGradient.GType = fgtFlat THEN
         FThreeDGradient.TextOut(FTxtBitmap.Canvas.Handle, FText[uCurTextLine],
            r, x, y)
      ELSE
      BEGIN                             {fgt3D}
         FTxtBitmap.Canvas.Font.Color
            := FThreeDGradient.GetColorFromGradientLine(FThreeDGradient.Depth,
               i);
         FTxtBitmap.Canvas.TextOut(x, y, FText[uCurTextLine]);
      END;
      IF x < (FThreeDGradient.Depth DIV 2) THEN
         inc(x)
      ELSE
         dec(x);
      IF y < (FThreeDGradient.Depth DIV 2) THEN
         inc(y)
      ELSE
         dec(y);
   END;
   FGradient.TextOut(FTxtBitmap.Canvas.Handle, FText[uCurTextLine], r, x, y);

   {.calc scaling.}
   IF FDirection = fsdRaising THEN
   BEGIN
      FScaledWidth := (FTxtBitmap.Width * FScalePercent / 100);
      FScaledHeight := (FTxtBitmap.Height * FScalePercent / 100);
      FScaledHeight := FScaledHeight + 2;
   END
   ELSE                                 { fsdRecession }
   BEGIN
      FScaledHeight := FTxtBitmap.Height;
      FScaledWidth := FTxtBitmap.Width;
   END;
   FOldScaledWidth := FScaledWidth;
   FOldScaledHeight := FScaledHeight;

   IF FClearOldText THEN
   BEGIN
      R := rect(0, 0, FPartTxtBitmap.width, FPartTxtBitmap.height);
      FPartTxtBitmap.Canvas.Fillrect(R);
   END;

   R := rect(0, 0, FTxtBitmap.width, FTxtBitmap.height);
   FScaledTxtBitmap.Width := FTxtBitmap.Width;
   FScaledTxtBitmap.Height := FTxtBitmap.Height;
   FScaledTxtBitmap.Canvas.Fillrect(R);

   fNeedRepaintBackground := true;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.BuildBitmaps;
VAR
   R                          : TRect;
   fOldTimer                  : boolean;
BEGIN
   IF NOT fLoaded THEN
   BEGIN
      //    FPartTxtBitmap.Width:=Width; FPartTxtBitmap.Height:=Height;
      FResBitmap.Width := Width;
      FResBitmap.Height := Height;
      exit;
   END;

   fOldTimer := FTimer.Enabled;
   FTimer.Enabled := false;
   IF fNeedRemakeBackground THEN
   BEGIN
      //________________________________PrepareBackground____begin
  //    BringParentWindowToTop(self);
      FBackgrBitmap.Width := Width;
      FBackgrBitmap.Height := Height;

      //..prepare tabula rasa
      IF FTransparent THEN
         FBackgrBitmap.Canvas.Brush.Color := Parent.Brush.Color
      ELSE
         FBackgrBitmap.Canvas.Brush.Color := FBackgrColor;
      FBackgrBitmap.Canvas.Brush.Style := bsSolid;
      FBackgrBitmap.Canvas.FillRect(ClientRect);

      IF FTransparent THEN
         GetParentImageRect(self, Bounds(Left, Top, Width, Height),
            FBackgrBitmap.Canvas.Handle);
      //________________________________PrepareBackground____end
    //    if (FResBitmap.Width or FResBitmap.Height)<>0 then
    //	BitBlt( FBackgrBitmap.Canvas.Handle, 0, 0,
    //		Width, Height, canvas.Handle, 0, 0, SRCCOPY);
   END;

   //  if FTransparent then ShowWindow(Handle,SW_SHOW);

   BuildTxtBitmap;

   R := rect(0, 0, FResBitmap.width, FResBitmap.height);
   FResBitmap.Canvas.Fillrect(R);
   FTimer.Enabled := fOldTimer;
   fNeedRemakeBackground := false;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.WMSize(VAR Message: TWMSize);
BEGIN
   fNeedRemakeBackground := true;
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.OnParamsChanged(Sender: TObject);
BEGIN
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.Repaint_;
//var R:TRect;
BEGIN
   IF NOT fLoaded THEN
      exit;
   Repaint;
   //  R:=Rect(0,0,Width,Height);
   //  InvalidateRect(Handle,@R,false);
END;
//________________________________________________________
//*****************************************_____________PROPERTY METHODS
//________________________________________________________

PROCEDURE TJvgFlyingText.SetHorAlign(Value: TglHorAlign);
BEGIN
   FHorAlign := Value;
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetVertAlign(Value: TglVertAlign);
BEGIN
   FVertAlign := Value;
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetTransparent(Value: boolean);
BEGIN
   FTransparent := Value;
   fNeedRemakeBackground := true;
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetBackgrColor(Value: TColor);
BEGIN
   FBackgrColor := Value;
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetInteriorOffset(Value: word);
BEGIN
   FInteriorOffset := Value;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetScalePercent(Value: TSpPercent);
BEGIN
   FScalePercent := Value;
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetStepScaleFactor(Value: Single);
BEGIN
   IF Value < 1 THEN
      Value := 1;
   IF Value > 2 THEN
      Value := 2;
   FStepScaleFactor := Value;
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetResultFont(Value: TFont);
BEGIN
   IF Value <> NIL THEN
      FResultFont.Assign(Value);
   BuildBitmaps;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetTimerInterval(Value: word);
BEGIN
   FTimerInterval := Value;
   FTimer.Interval := FTimerInterval;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetActive(Value: boolean);
BEGIN
   FActive := Value;
   FTimer.Enabled := FActive;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetText(Value: TStringList);
VAR
   OldActive                  : boolean;
   i                          : integer;
BEGIN
   OldActive := FActive;
   Active := false;
   IF Assigned(Value) THEN
      FText.Assign(Value);
   uCurTextLine := 0;
   IF FText.Count <> 0 THEN
      FOR i := 0 TO FText.Count - 1 DO
         FText[i] := Trim(FText[i]);
   BuildBitmaps;
   Repaint_;
   Active := OldActive;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetFastDraw(Value: boolean);
BEGIN
   IF FFastDraw = Value THEN
      exit;
   FFastDraw := Value;
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetDirection(Value: TglScalingDir);
BEGIN
   IF FDirection = Value THEN
      exit;
   FDirection := Value;
   IF csDesigning IN ComponentState THEN
   BEGIN
      BuildBitmaps;
      Repaint_;
   END;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetShowTextWhilePassive(Value: boolean);
BEGIN
   IF FShowTextWhilePassive = Value THEN
      exit;
   FShowTextWhilePassive := Value;
   BuildBitmaps;
   Repaint_;
END;
//________________________________________________________

PROCEDURE TJvgFlyingText.SetVisible(Value: boolean);
BEGIN
   IF FVisible = Value THEN
      exit;
   FVisible := Value;
   INHERITED Visible := FVisible;
   IF FVisible THEN
   BEGIN
      BuildBitmaps;
      Repaint_;
   END;
END;
//________________________________________________________

END.

