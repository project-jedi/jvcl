{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgButton.PAS, released on 2003-01-15.

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

UNIT JvgButton;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   JVComponent,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   ExtCtrls,
   JvgTypes,
   JvgUtils,
   JvgCommClasses{$IFDEF COMPILER5_UP},
   Imglist{$ENDIF};

TYPE
   TDrawMode = (dmUseImageList, dmAutoCtrl3D, dmAutoFlat, dmAutoShadow);

   TglButtonOptions_ = (boBlinkWhenActive, boBlinkWhenInactive,
      boBlinkWhenPushed, boChangeColorWhenActive,
      boChangeColorWhenPushed, boDelicateInactive,
      boDrawPushedAsFlat, boRaisedInactive,
      boRefreshOnActivate, boRefreshOnPush,
      boShadowSurround, boShiftMouseOnPush);

   TglButtonOptions = SET OF TglButtonOptions_;
   TglBtnState = (fbsOriginal, fbsInactive, fbsActive, fbsPushed, fbsDisabled);

   //*************************************{ . TJvgGlyphsIndexes . }
   TJvgGlyphsIndexes = CLASS(TPersistent)
   PRIVATE
      FInactive: integer;
      FPushed: integer;
      FActive: integer;
      FDisabled: integer;
      FMask: integer;
      PROCEDURE SetInactive(Value: integer);
      PROCEDURE SetPushed(Value: integer);
      PROCEDURE SetActive(Value: integer);
      PROCEDURE SetDisabled(Value: integer);
      PROCEDURE SetMask(Value: integer);
   PUBLIC
      OnChanged: TNotifyEvent;
      CONSTRUCTOR Create;
   PUBLISHED
      PROPERTY Inactive: integer READ FInactive WRITE SetInactive DEFAULT 0;
      PROPERTY Pushed: integer READ FPushed WRITE SetPushed DEFAULT 1;
      PROPERTY Active: integer READ FActive WRITE SetActive DEFAULT 2;
      PROPERTY Disabled: integer READ FDisabled WRITE SetDisabled DEFAULT -1;
      PROPERTY Mask: integer READ FMask WRITE SetMask DEFAULT 3;
   END;

   TJvgBtnGlyphs = CLASS(TPersistent)
   PRIVATE
      FGlyphInactive: TBitmap;
      FGlyphMask: TBitmap;
      FGlyphPushed: TBitmap;
      FGlyphActive: TBitmap;
      FGlyphDisabled: TBitmap;

      PROCEDURE SetGlyphInactive(Value: TBitmap);
      PROCEDURE SetGlyphMask(Value: TBitmap);
      PROCEDURE SetGlyphPushed(Value: TBitmap);
      PROCEDURE SetGlyphActive(Value: TBitmap);
      PROCEDURE SetGlyphDisabled(Value: TBitmap);

   PUBLIC
      CONSTRUCTOR Create;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY GlyphInactive: TBitmap READ FGlyphInactive WRITE
         SetGlyphInactive;
      PROPERTY GlyphMask: TBitmap READ FGlyphMask WRITE SetGlyphMask;
      PROPERTY GlyphPushed: TBitmap READ FGlyphPushed WRITE SetGlyphPushed;
      PROPERTY GlyphActive: TBitmap READ FGlyphActive WRITE SetGlyphActive;
      PROPERTY GlyphDisabled: TBitmap READ FGlyphDisabled WRITE
         SetGlyphDisabled;
   END;
   //*************************************{ . TJvgButton . }
   TJvgButton = CLASS(TJvGraphicControl)
   PRIVATE
      FGlyph: TBitmap;
      FGlyphs: TJvgBtnGlyphs;
      FDrawMode: TDrawMode;
      FGlyphsList: TImageList;
      FTransparentColor: TColor;
      FNumGlyphs: integer;
      FShiftMaskWhenPushed: TJvgPointClass;
      FEnabled: boolean;
      FOptions: TglButtonOptions;
      FShadowDepth: word;
      FGlyphsIndexes: TJvgGlyphsIndexes;
      FColorHighlight: TColor;
      FColorShadow: TColor;
      FColorDarkShadow: TColor;
      FDisabledMaskColor: TColor;
      FChangeColorOnActivate: TJvgTwainColors;
      FChangeColorOnPush: TJvgTwainColors;
      FAutoTrColor: TglAutoTransparentColor;
      FBlinkTimer: TTimer;
      FOnMouseEnter: TNotifyEvent;
      FOnMouseLeave: TNotifyEvent;
      FOnClick: TNotifyEvent;

      TmpBMP: TBitmap;
      Img: TBitmap;
      DefaultGlyphsList: TImageList;
      fBimapsCreated: boolean;
      fMouseInControl: boolean;
      fPushed: boolean;
      fShowingAsPushedNow: boolean;
      fActiveNow: boolean;
      fLoaded: boolean;
      fBlinked: boolean;
      fNeedBlink: boolean;
      MShift: TPoint;
      PROCEDURE SetGlyph(Value: TBitmap);
      PROCEDURE SetDrawMode(Value: TDrawMode);
      PROCEDURE SetGlyphsList(Value: TImageList);
      PROCEDURE SetNumGlyphs(Value: integer);
      PROCEDURE SetTransparentColor(Value: TColor);
      PROCEDURE SetEnabled(Value: boolean);
      //    procedure SetDelicateInactive( Value: boolean );
      //    procedure SetRaisedInactive( Value: boolean );
      PROCEDURE SetShadowDepth(Value: word);
      PROCEDURE SetColorHighlight(Value: TColor);
      PROCEDURE SetColorShadow(Value: TColor);
      PROCEDURE SetColorDarkShadow(Value: TColor);
      PROCEDURE SetDisabledMaskColor(Value: TColor);
      PROCEDURE SetOptions(Value: TglButtonOptions);
      PROCEDURE SetAutoTrColor(Value: TglAutoTransparentColor);
      PROCEDURE SetBlinkTimer(Value: TTimer);
      FUNCTION GetBlinkTimer: TTimer;
      PROCEDURE SetTestMode(Value: boolean);
      //    procedure SetShadowSurround( Value: boolean );
      //    procedure SetDrawPushedAsFlat( Value: boolean );

      FUNCTION IsMouseInControl: boolean;
      PROCEDURE GetBitmaps;
      PROCEDURE CreateBitmaps;          //...based on Inactive Glyph
      PROCEDURE GetBitmap_(Index: integer; VAR Bmp: TBitmap);
      PROCEDURE SmthChanged(Sender: TObject);
      PROCEDURE ApplicateGlyph(VAR TargetBMP: TBitmap; State: TglBtnState;
         DrawState: TglDrawState; s: integer);
   PROTECTED
      PROCEDURE Paint; OVERRIDE;
      PROCEDURE Paint_;
      PROCEDURE Loaded; OVERRIDE;
      //    procedure WMSize(var Message: TWMSize); message WM_SIZE;
      //  procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
      PROCEDURE MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
         Integer); OVERRIDE;
      PROCEDURE MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         OVERRIDE;
      PROCEDURE MouseMove(Shift: TShiftState; X, Y: Integer); OVERRIDE;
      //    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;

   PUBLIC
      FTestMode: boolean; //...placed hete to access from SetTestMode method
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE OnBlinkTimer(Sender: TObject);
   PUBLISHED
      PROPERTY ShowHint DEFAULT True;
      PROPERTY Glyphs: TJvgBtnGlyphs READ FGlyphs WRITE FGlyphs;
      PROPERTY DrawMode: TDrawMode READ FDrawMode WRITE SetDrawMode;
      PROPERTY GlyphsList: TImageList READ FGlyphsList WRITE SetGlyphsList;
      PROPERTY Glyph: TBitmap READ FGlyph WRITE SetGlyph;
      PROPERTY NumGlyphs: integer READ FNumGlyphs WRITE SetNumGlyphs;
      PROPERTY TransparentColor: TColor
         READ FTransparentColor WRITE SetTransparentColor DEFAULT clOlive;
      PROPERTY ShiftMaskWhenPushed: TJvgPointClass READ FShiftMaskWhenPushed
         WRITE FShiftMaskWhenPushed;
      //    property RefreshOnActivate: boolean
      //     read FRefreshOnActivate write FRefreshOnActivate default false;
      //    property RefreshOnPush: boolean
      //     read FRefreshOnPush write FRefreshOnPush default true;
      PROPERTY Enabled: boolean READ FEnabled WRITE SetEnabled DEFAULT true;
      PROPERTY GlyphsIndexes: TJvgGlyphsIndexes
         READ FGlyphsIndexes WRITE FGlyphsIndexes;
      PROPERTY ShadowDepth: word READ FShadowDepth WRITE SetShadowDepth
         DEFAULT 5;
      PROPERTY ColorHighlight: TColor READ FColorHighlight WRITE
         SetColorHighlight
         DEFAULT clBtnHighlight;
      PROPERTY ColorShadow: TColor READ FColorShadow WRITE SetColorShadow
         DEFAULT clBtnShadow;
      PROPERTY ColorDarkShadow: TColor READ FColorDarkShadow WRITE
         SetColorDarkShadow
         DEFAULT clBlack;
      PROPERTY DisabledMaskColor: TColor READ FDisabledMaskColor WRITE
         SetDisabledMaskColor
         DEFAULT clBlack;
      PROPERTY Options: TglButtonOptions READ FOptions WRITE SetOptions;
      PROPERTY ChangeColorOnActivate: TJvgTwainColors
         READ FChangeColorOnActivate WRITE FChangeColorOnActivate;
      PROPERTY ChangeColorOnPush: TJvgTwainColors
         READ FChangeColorOnPush WRITE FChangeColorOnPush;
      PROPERTY AutoTransparentColor: TglAutoTransparentColor
         READ FAutoTrColor WRITE SetAutoTrColor DEFAULT ftcUser;
      PROPERTY BlinkTimer: TTimer READ GetBlinkTimer WRITE SetBlinkTimer;
      PROPERTY TestMode: boolean READ FTestMode WRITE SetTestMode DEFAULT false;

      PROPERTY OnMouseEnter: TNotifyEvent READ FOnMouseEnter WRITE
         FOnMouseEnter;
      PROPERTY OnMouseLeave: TNotifyEvent READ FOnMouseLeave WRITE
         FOnMouseLeave;
      PROPERTY OnClick: TNotifyEvent READ FOnClick WRITE FOnClick;
   END;

IMPLEMENTATION
{$R JvgButton.res}

CONSTRUCTOR TJvgBtnGlyphs.Create;
BEGIN
   INHERITED;
   FGlyphInactive := TBitmap.Create;
   FGlyphMask := TBitmap.Create;
   FGlyphPushed := TBitmap.Create;
   FGlyphActive := TBitmap.Create;
   FGlyphDisabled := TBitmap.Create;
END;

DESTRUCTOR TJvgBtnGlyphs.Destroy;
BEGIN
   IF Assigned(FGlyphInactive) THEN
      FGlyphInactive.Free;
   IF Assigned(FGlyphMask) THEN
      FGlyphMask.Free;
   IF Assigned(FGlyphPushed) THEN
      FGlyphPushed.Free;
   IF Assigned(FGlyphActive) THEN
      FGlyphActive.Free;
   IF Assigned(FGlyphDisabled) THEN
      FGlyphDisabled.Free;
   INHERITED;
END;

//procedure TJvgBtnGlyphs.SetGlyphInactive( Value: TBitmap );

PROCEDURE TJvgBtnGlyphs.SetGlyphInactive(Value: TBitmap);
BEGIN
   IF Assigned(GlyphInactive) THEN
      GlyphInactive.Free;
   GlyphInactive.Assign(value);
END;

PROCEDURE TJvgBtnGlyphs.SetGlyphMask(Value: TBitmap);
BEGIN
   IF Assigned(GlyphMask) THEN
      GlyphMask.Free;
   GlyphMask.Assign(value);
END;

PROCEDURE TJvgBtnGlyphs.SetGlyphPushed(Value: TBitmap);
BEGIN
   IF Assigned(GlyphPushed) THEN
      GlyphPushed.Free;
   GlyphPushed.Assign(value);
END;

PROCEDURE TJvgBtnGlyphs.SetGlyphActive(Value: TBitmap);
BEGIN
   IF Assigned(GlyphActive) THEN
      GlyphActive.Free;
   GlyphActive.Assign(value);
END;

PROCEDURE TJvgBtnGlyphs.SetGlyphDisabled(Value: TBitmap);
BEGIN
   IF Assigned(GlyphDisabled) THEN
      GlyphDisabled.Free;
   GlyphDisabled.Assign(value);
END;

//*************************************{ . TJvgButton methods. }

CONSTRUCTOR TJvgButton.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);
   ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
   FGlyph := TBitmap.Create;
   FGlyphs := TJvgBtnGlyphs.Create;
   DefaultGlyphsList := TImageList.CreateSize(30, 30);
   Img := TBitmap.Create;
   TmpBMP := TBitmap.Create;
   FShiftMaskWhenPushed := TJvgPointClass.Create;
   FGlyphsIndexes := TJvgGlyphsIndexes.Create;
   FChangeColorOnActivate := TJvgTwainColors.Create;
   FChangeColorOnPush := TJvgTwainColors.Create;
   FGlyphsIndexes.OnChanged := SmthChanged;
   //...set defaults
   FShiftMaskWhenPushed.x := 0;
   FShiftMaskWhenPushed.y := 0;
   FEnabled := true;
   FGlyphsList := NIL;
   FNumGlyphs := 3;
   FDrawMode := dmUseImageList;
   FShadowDepth := 5;
   FColorHighlight := clBtnHighlight;
   FColorShadow := clBtnShadow;
   FColorDarkShadow := clBlack;
   FDisabledMaskColor := clBlack;
   FTestMode := false;
   ShowHint := true;
   FOptions := [boRaisedInactive, boShadowSurround, boShiftMouseOnPush,
      boChangeColorWhenActive, boChangeColorWhenPushed,
      boBlinkWhenActive];
   //  if (csDesigning in ComponentState)and not(csLoading in ComponentState) then
   IF DefaultGlyphsList.ResourceLoad(rtBitmap, 'FRDEFBUTTON', clNone) THEN
   BEGIN
      //    ShowMessage('qwerty');
      FGlyphsList := DefaultGlyphsList;
      GetBitmaps;
   END;
   fPushed := false;
   FChangeColorOnActivate.FromColor := clBlack;
   FChangeColorOnActivate.ToColor := clBlack;
   FChangeColorOnPush.FromColor := clBlack;
   FChangeColorOnPush.ToColor := clBlack;
   FTransparentColor := clOlive;
   FAutoTrColor := {ftcLeftBottomPixel;} ftcUser;
   Width := 20;
   Height := 20;
   fLoaded := false;                    //(csDesigning in ComponentState);

END;

DESTRUCTOR TJvgButton.Destroy;
BEGIN
   FGlyphsList := NIL;
   Glyphs.Free;
   FGlyph.Free;
   DefaultGlyphsList.Free;
   Img.Free;
   TmpBMP.Free;
   FShiftMaskWhenPushed.Free;
   FGlyphsIndexes.Free;
   FChangeColorOnActivate.Free;
   FChangeColorOnPush.Free;
   IF NOT (csDestroying IN Owner.ComponentState) THEN
      SetBlinkTimer(NIL);
   INHERITED;
END;

PROCEDURE TJvgButton.Loaded;
BEGIN
   INHERITED Loaded; //if csDesigning in ComponentState then exit;
   //AutoTransparentColor := FAutoTrColor;
   fLoaded := true;                     // GetBitmaps;
END;

PROCEDURE TJvgButton.Paint;
VAR
   DrawState                  : TglDrawState;
   i                          : word;
BEGIN
   WITH Glyphs DO
   BEGIN
      IF NOT fLoaded THEN
      BEGIN
         fLoaded := true;
         GetBitmaps;
      END;
      //    if not fBimapsCreated then exit;
      Width := FGlyphInactive.Width + 1;
      Height := FGlyphInactive.Height + 1;
      //fMouseInControl:=IsMouseInControl;
      fShowingAsPushedNow := fPushed AND fMouseInControl;
      fActiveNow := true;

      WITH Img DO
      BEGIN
         Width := Self.Width;
         Height := Self.Height;
         Canvas.Brush.Color := clBtnFace;
         Canvas.Brush.Style := bsSolid;
         Canvas.FillRect(Bounds(0, 0, Width, Height));
      END;
      GetParentImageRect(self, Bounds(Left, Top, Width, Height),
         Img.Canvas.Handle);

      IF boDelicateInactive IN FOptions THEN
         DrawState := fdsDelicate
      ELSE
         DrawState := fdsDefault;

      IF FEnabled THEN                  //..._______________ENABLED_
      BEGIN
         IF fMouseInControl THEN
         BEGIN
            IF fPushed THEN             //..._____________PUSHED_
            BEGIN

               IF (boDrawPushedAsFlat IN FOptions) AND (FDrawMode <>
                  dmUseImageList) THEN
                  ApplicateGlyph(Img, fbsOriginal {fbsPushed}, fdsDefault, 3)
               ELSE
               BEGIN
                  IF FDrawMode = dmAutoFlat THEN
                     i := 2
                  ELSE
                     i := 0;
                  ApplicateGlyph(Img, fbsPushed, fdsDefault, i);
               END;
            END
            ELSE
            BEGIN                       //...__________________ACTIVE_
               fActiveNow := false;
               IF (FDrawMode = dmAutoFlat) THEN
                  i := 1
               ELSE
                  i := 0;
               //	  if {(FGlyphsIndexes.Active=-1)or}(FDrawMode=dmAutoCtrl3D) then
               //	     (FDrawMode=dmAutoShadow) then
               //	    //...__use INACTIVE as ACTIVE_
               //	    ApplicateGlyph( Img, fbsInactive, fdsDefault, 0 )
               //	  else
               ApplicateGlyph(Img, fbsActive, fdsDefault, i);
            END;
         END
         ELSE
         BEGIN                          //..._______________INACTIVE_

            IF (FDrawMode = dmAutoFlat)
               AND ({fPushed or }(NOT (boRaisedInactive IN FOptions))) THEN
               ApplicateGlyph(Img, fbsOriginal, DrawState, 2)
            ELSE
            BEGIN
               IF (FDrawMode = dmAutoFlat) THEN
                  i := 1
               ELSE
                  i := 0;
               ApplicateGlyph(Img, fbsInactive, DrawState, i);
            END;
         END;
      END
      ELSE
      BEGIN                             //..._____________DISABLED_
         IF (FDrawMode = dmAutoFlat) AND (boRaisedInactive IN Options) THEN
            i := 1
         ELSE
            i := 0;
         IF DrawMode <> dmUseImageList THEN //...auto disabled
            ApplicateGlyph(Img, fbsDisabled, fdsDisabled, i)
         ELSE
         BEGIN                          //...user's disabled
            IF FGlyphsIndexes.Disabled = -1 THEN
               CreateBitmapExt(Img.Canvas.Handle, FGlyphInactive, ClientRect, 0,
                  0,
                  fwoNone, fdsDisabled, true, FTransparentColor,
                     DisabledMaskColor)
            ELSE
               CreateBitmapExt(Img.Canvas.Handle, FGlyphDisabled, ClientRect, 0,
                  0,
                  fwoNone, fdsDefault, true, FTransparentColor,
                     DisabledMaskColor);
         END;
      END;
      Canvas.Draw(0, 0, Img);
      //BitBlt( Canvas.Handle, 0, 0, Height, Width, Img.Canvas.Handle, 0, 0, SRCCOPY );
   END;
END;

PROCEDURE TJvgButton.Paint_;
//var R: TRect;
BEGIN
   //  r:=Bounds(left-1,top-1,Width+1,Height+1);
   //InvalidateRect( Parent.Handle, @R, false);
   IF NOT Enabled THEN
      exit;
   IF FChangeColorOnActivate.FromColor <> FChangeColorOnActivate.ToColor THEN
   BEGIN
      rePaint;
      exit;
   END;
   IF ((FDrawMode = dmAutoCtrl3D) OR (FDrawMode = dmAutoShadow)) AND (NOT
      fShowingAsPushedNow) AND (NOT fPushed) AND (NOT (boDelicateInactive IN
      FOptions)) THEN
      exit;
   IF (FDrawMode = dmAutoFlat)
      AND (NOT fShowingAsPushedNow)
      AND (NOT fPushed)
      AND (boRaisedInactive IN FOptions)
      AND (NOT (boDelicateInactive IN FOptions)) THEN
      exit;

   rePaint;
   exit;
   //Refresh;exit;

   IF (FDrawMode = dmAutoFlat)
      AND (fShowingAsPushedNow OR (NOT (boRaisedInactive IN FOptions))) THEN
   BEGIN
      repaint;
      exit;
   END;
   IF fPushed THEN
   BEGIN
      IF (boRefreshOnPush IN FOptions) OR (FDrawMode = dmAutoShadow) THEN
         Repaint
      ELSE
         Paint;
   END
   ELSE IF boRefreshOnActivate IN FOptions THEN
      Repaint
   ELSE
      Paint;
END;

PROCEDURE TJvgButton.ApplicateGlyph(VAR TargetBMP: TBitmap;
   State: TglBtnState;
   DrawState: TglDrawState;
   s: integer);
VAR
   i, j                       : integer;
   fChangeColor, fCanBlink    : boolean;
   DrawState2                 : TglDrawState;
BEGIN
   WITH Glyphs DO
   BEGIN
      i := 1;
      j := 1;
      fChangeColor := false;
      fCanBlink := false;
      IF DrawState = fdsDisabled THEN
      BEGIN
         DrawState := fdsDefault;
         DrawState2 := fdsDisabled      //DrawState;
      END
      ELSE
         DrawState2 := DrawState;
      CASE FDrawMode OF
         dmAutoCtrl3D:
            IF State = fbsPushed THEN
            BEGIN
               i := 2;
               j := 2;
            END;
         dmUseImageList:
            BEGIN
               i := 0;
               j := 0;
               s := 0;
            END;
      END;

      CASE State OF
         fbsOriginal:
            BEGIN
               CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyph, ClientRect, s,
                  s,
                  fwoNone, DrawState, true, FTransparentColor,
                     DisabledMaskColor);
               exit;
            END;
         fbsInactive, fbsDisabled:
            BEGIN
               IF (DrawMode = dmAutoFlat) AND (boRaisedInactive IN FOptions)
                  THEN
                  CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphActive,
                     ClientRect, s, s,
                     fwoNone, DrawState, true, FTransparentColor,
                        DisabledMaskColor)
               ELSE
                  CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphInactive,
                     ClientRect, 0, 0,
                     fwoNone, DrawState, true, FTransparentColor,
                        DisabledMaskColor);

               IF State = fbsDisabled THEN
               BEGIN
                  i := 0;
                  j := 0;
               END;
               fCanBlink := boBlinkWhenInactive IN Options;
            END;
         fbsActive:
            BEGIN
               IF (FDrawMode = dmAutoCtrl3D) OR (DrawMode = dmAutoShadow) THEN
                  CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphInactive,
                     ClientRect, s, s,
                     fwoNone, DrawState, true, FTransparentColor,
                        DisabledMaskColor)
               ELSE
                  CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphActive,
                     ClientRect, s, s,
                     fwoNone, DrawState, true, FTransparentColor,
                        DisabledMaskColor);
               fChangeColor := boChangeColorWhenActive IN Options;
               fCanBlink := boBlinkWhenActive IN Options;
            END;
         fbsPushed:
            BEGIN
               CreateBitmapExt(TargetBMP.Canvas.Handle, FGlyphPushed,
                  ClientRect, s, s,
                  fwoNone, DrawState, true, FTransparentColor,
                     DisabledMaskColor);
               fChangeColor := boChangeColorWhenPushed IN Options;
               fCanBlink := boBlinkWhenPushed IN Options;
            END;
      END;
      GetBitmap_(FGlyphsIndexes.Inactive, TmpBMP);
      fCanBlink := fCanBlink AND fNeedBlink;
      IF fCanBlink THEN
         fBlinked := NOT fBlinked
      ELSE IF State = fbsActive THEN
         fBlinked := FChangeColorOnActivate.FromColor <>
            FChangeColorOnActivate.ToColor
      ELSE
         fBlinked := FChangeColorOnPush.FromColor <> FChangeColorOnPush.ToColor;

      IF fCanBlink THEN
      BEGIN
         IF fBlinked THEN
            IF State = fbsPushed THEN
               WITH FChangeColorOnPush DO
                  ChangeBitmapColor(TmpBMP, FromColor, ToColor)
            ELSE
               WITH FChangeColorOnActivate DO
                  ChangeBitmapColor(TmpBMP, FromColor, ToColor);
      END
      ELSE IF fChangeColor AND (FDrawMode <> dmUseImageList) THEN
         IF State = fbsActive THEN
            WITH FChangeColorOnActivate DO
               ChangeBitmapColor(TmpBMP, FromColor, ToColor)
         ELSE
            WITH FChangeColorOnPush DO
               ChangeBitmapColor(TmpBMP, FromColor, ToColor);
      fNeedBlink := false;
      IF (DrawMode = dmAutoShadow) AND (State = fbsPushed)
         OR (FDrawMode = dmUseImageList) THEN
         exit;

      IF DrawState2 = fdsDisabled THEN
      BEGIN
         TmpBMP.Canvas.Brush.Color := FTransparentColor;
         TmpBMP.Canvas.Font.Color := clBtnFace;
         //    SetBkColor(TmpBMP.Canvas.Handle, FTransparentColor);
         TmpBMP.Monochrome := true;
         TmpBMP.Monochrome := false;
         CreateBitmapExt(TargetBMP.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            i + s, j + s, fwoNone, fdsDefault, true, FTransparentColor,
               FDisabledMaskColor);
         GetBitmap_(FGlyphsIndexes.Inactive, TmpBMP);
      END;

      CreateBitmapExt(TargetBMP.Canvas.Handle, TmpBMP, Rect(0, 0, TmpBMP.Width,
         TmpBMP.Height),
         i + s, j + s, fwoNone, DrawState2, true, FTransparentColor,
            FDisabledMaskColor);
   END;
END;

{procedure TJvgButton.WMSize(var Message: TWMSize);
begin
  Width := FGlyphInactive.Width; Height := FGlyphInactive.Height;
end;}

PROCEDURE TJvgButton.MouseMove(Shift: TShiftState; X, Y: Integer);
VAR
   pt                         : TPoint;
   fMouseInControl_           : boolean;
BEGIN
   INHERITED MouseMove(Shift, X, Y);
   pt.x := X;
   pt.y := Y;
   IF IsPointInRect(pt, ClientRect) THEN
   BEGIN
      fMouseInControl_ := IsMouseInControl;
      IF fMouseInControl_ <> fMouseInControl THEN
      BEGIN
         IF fMouseInControl THEN
            IF Assigned(FOnMouseEnter) THEN
               FOnMouseEnter(self)
            ELSE IF Assigned(FOnMouseLeave) THEN
               FOnMouseLeave(self);
         fMouseInControl := fMouseInControl_;
         Paint_;
      END;
   END;
END;

{procedure TJvgButton.CMMouseEnter(var Message: TMessage);
begin
  fMouseInControl:=true; Paint_;
end;}

PROCEDURE TJvgButton.CMMouseLeave(VAR Message: TMessage);
BEGIN
   INHERITED;
   //fMouseInControl:=IsMouseInControl;
   IF Assigned(FOnMouseLeave) THEN
      FOnMouseLeave(self);
   fMouseInControl := false;
   Paint_;
END;

PROCEDURE TJvgButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
   X, Y: Integer);
VAR
   pt                         : TPoint;
BEGIN
   INHERITED MouseDown(Button, Shift, X, Y);
   IF (Button <> mbLeft) OR (NOT Enabled) OR (NOT IsMouseInControl) THEN
      exit;

   IF boShiftMouseOnPush IN FOptions THEN
   BEGIN
      GetCursorPos(pt);
      SetCursorPos(pt.x + MShift.x, pt.y + MShift.y);
   END;
   //  if not FMouseInControl then FMouseInControl := True;
   fPushed := true;
   Paint_;
END;

PROCEDURE TJvgButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
   X, Y: Integer);
VAR
   pt                         : TPoint;
BEGIN
   INHERITED MouseUp(Button, Shift, X, Y);
   IF fShowingAsPushedNow AND Assigned(FOnClick) THEN
      FOnClick(self);
   IF (boShiftMouseOnPush IN FOptions) AND IsMouseInControl THEN
   BEGIN
      GetCursorPos(pt);
      SetCursorPos(pt.x - MShift.x, pt.y - MShift.y);
   END;
   fPushed := false;
   Paint_;
END;

PROCEDURE TJvgButton.GetBitmaps;
BEGIN
   IF NOT fLoaded THEN
      exit;
   WITH Glyphs DO
   BEGIN
      FGlyphInactive.Width := 0;
      FGlyphPushed.Width := 0;
      FGlyphActive.Width := 0;
      FGlyphDisabled.Width := 0;
      FGlyphMask.Width := 0;
      IF FDrawMode = dmUseImageList THEN
      BEGIN
         IF NOT Assigned(FGlyphsList) THEN
            exit;
         WITH FGlyphsList, FGlyphsIndexes DO
         BEGIN
            IF (Inactive < 0) AND (Inactive > (Count - 1)) THEN
               Inactive := 0;
            IF (Pushed < 0) AND (Pushed > (Count - 1)) THEN
               Pushed := 1;
            IF (Active > (Count - 1)) THEN
               Active := -1;
            IF (Mask > (Count - 1)) THEN
               Mask := -1;

            IF Inactive <> -1 THEN
               GetBitmap_(Inactive, FGlyphInactive);
            IF Pushed <> -1 THEN
               GetBitmap_(Pushed, FGlyphPushed);
            IF Active <> -1 THEN
               GetBitmap_(Active, FGlyphActive); //...optional bitmap
            IF Disabled <> -1 THEN
               GetBitmap_(Disabled, FGlyphDisabled); //...optional bitmap
            IF Mask <> -1 THEN
               GetBitmap_(Mask, FGlyphMask); //...optional bitmap
            FNumGlyphs := Count;
            fBimapsCreated := NOT (FGlyphInactive.Empty OR FGlyphPushed.Empty);
         END;
      END
      ELSE
         CreateBitmaps;
      fBimapsCreated := true;

      CASE FDrawMode OF
         dmAutoShadow:
            IF boDrawPushedAsFlat IN FOptions THEN
            BEGIN
               MShift.x := 1;
               MShift.y := 1;
            END
            ELSE
            BEGIN
               MShift.x := FShadowDepth - 1;
               MShift.y := FShadowDepth - 1;
            END;
         dmAutoCtrl3D:
            BEGIN
               MShift.x := 2;
               MShift.y := 2;
            END;
         dmAutoFlat:
            BEGIN
               MShift.x := 1;
               MShift.y := 1;
            END;
      ELSE
         BEGIN
            MShift.x := FShiftMaskWhenPushed.x;
            MShift.y := FShiftMaskWhenPushed.y;
         END;
      END;

      Width := FGlyphInactive.Width;
      Height := FGlyphInactive.Height;
   END;
END;

PROCEDURE TJvgButton.CreateBitmaps;     //...based on Inactive Glyph
VAR
   MonoBMP, OldMonoBMP        : HBITMAP;
   MonoDC                     : HDC;
   i                          : word;

   PROCEDURE RemakeTmpBMP;
   BEGIN
      SetBkColor(TmpBMP.Canvas.Handle, ColorToRGB(FTransparentColor));
      BitBlt(TmpBMP.Canvas.Handle, 0, 0, TmpBMP.Width, TmpBMP.Height, MonoDC, 0,
         0, SRCCOPY);
   END;
BEGIN

   WITH FGlyphs, FGlyphsList, FGlyphsIndexes DO
   BEGIN
      FInactive := 0;
      FPushed := -1;
      FActive := -1;
      FDisabled := -1;
      FMask := -1;
      GetBitmap_(Inactive, TmpBMP);

      MonoDC := CreateCompatibleDC(TmpBMP.Canvas.Handle);
      MonoBMP := CreateBitmap(TmpBMP.Width, TmpBMP.Height, 1, 1, NIL);
      OldMonoBMP := SelectObject(MonoDC, MonoBMP);
      //  SetMapMode( MonoDC, GetMapMode(TmpBMP.Canvas.Handle) );
      SetBkColor(TmpBMP.Canvas.Handle, ColorToRGB(FTransparentColor));
      BitBlt(MonoDC, 0, 0, TmpBMP.Width, TmpBMP.Height,
         TmpBMP.Canvas.Handle, 0, 0, SRCCOPY);
      //SetBkColor(TmpBMP.Canvas.Handle, OldBkColor);
      TRY

         IF FDrawMode = dmAutoShadow THEN
         BEGIN
            WITH FGlyphInactive DO
            BEGIN
               Width := TmpBMP.Width + FShadowDepth;
               Height := TmpBMP.Height + FShadowDepth;
               Canvas.Brush.Style := bsSolid;
               Canvas.Brush.Color := FTransparentColor;
               Canvas.FillRect(Rect(0, 0, Width, Height));
            END;
            WITH FGlyphPushed DO
            BEGIN
               Width := TmpBMP.Width + FShadowDepth;
               Height := TmpBMP.Height + FShadowDepth;
               Canvas.Brush.Style := bsSolid;
               Canvas.Brush.Color := FTransparentColor;
               Canvas.FillRect(Rect(0, 0, Width, Height));
            END;

            BitBlt(FGlyphPushed.Canvas.Handle, FShadowDepth, FShadowDepth,
               TmpBMP.Width, TmpBMP.Height, TmpBMP.Canvas.Handle, 0, 0,
                  SRCCOPY);

            RemakeTmpBMP;
            ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
            CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               FShadowDepth, FShadowDepth, fwoNone, fdsDefault, true,
                  FTransparentColor, FDisabledMaskColor);
            exit;
         END;

         IF FDrawMode = dmAutoCtrl3D THEN
            i := 3
         ELSE
            i := 3;
         WITH FGlyphInactive DO
         BEGIN
            Width := TmpBMP.Width + i;
            Height := TmpBMP.Height + i;
            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := FTransparentColor;
            Canvas.FillRect(Rect(0, 0, Width, Height));
         END;
         IF NOT (boDrawPushedAsFlat IN FOptions) THEN
            WITH FGlyphPushed DO
            BEGIN
               Width := TmpBMP.Width + i;
               Height := TmpBMP.Height + i;
               Canvas.Brush.Style := bsSolid;
               Canvas.Brush.Color := FTransparentColor;
               Canvas.FillRect(Rect(0, 0, Width, Height));
            END;
         WITH FGlyphActive DO
         BEGIN
            Width := TmpBMP.Width + i;
            Height := TmpBMP.Height + i;
            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := FTransparentColor;
            Canvas.FillRect(Rect(0, 0, Width, Height));
         END;

         //    TmpBMP.Width:=TmpBMP.Width+i;  TmpBMP.Height:=TmpBMP.Height+i;

         //===========================-AUTOCTRL3D-=================================

         IF FDrawMode = dmAutoCtrl3D THEN //..._add 3d border to inactive BEGIN_
         BEGIN
            //============================================.CTRL3D INACTIVE.=
            RemakeTmpBMP;
            //...__________________________________________________Dark Shadow
            IF clBlack <> FColorDarkShadow THEN
               ChangeBitmapColor(TmpBMP, clBlack, FColorDarkShadow);
            IF boShadowSurround IN FOptions THEN
            BEGIN
               CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  0, 3, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
               CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  3, 0, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
            END;
            CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               3, 3, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);

            RemakeTmpBMP;
            //...__________________________________________________Highlight
            ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
            CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               0, 0, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
            IF boShadowSurround IN FOptions THEN
            BEGIN
               CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  2, 0, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
               CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  0, 2, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
            END;
            RemakeTmpBMP;
            //...__________________________________________________Shadow
            ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
            IF boShadowSurround IN FOptions THEN
            BEGIN
               CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  1, 2, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
               CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  2, 1, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
            END;
            CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               2, 2, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
            //============================================.CTRL3D PUSHED.=
            IF boDrawPushedAsFlat IN FOptions THEN
               exit;
            RemakeTmpBMP;
            //...__________________________________________________Highlight
            ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
            IF boShadowSurround IN FOptions THEN
            BEGIN
               CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  0, 3, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
               CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  3, 0, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
            END;
            CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               3, 3, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);

            RemakeTmpBMP;
            //...__________________________________________________Dark Shadow
            IF clBlack <> FColorDarkShadow THEN
               ChangeBitmapColor(TmpBMP, clBlack, FColorDarkShadow);

            CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               0, 0, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
            IF boShadowSurround IN FOptions THEN
            BEGIN
               CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  2, 0, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
               CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  0, 2, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
            END;
            RemakeTmpBMP;
            //...__________________________________________________Shadow
            ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
            IF boShadowSurround IN FOptions THEN
            BEGIN
               CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  1, 2, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
               CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  2, 1, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
            END;
            CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               1, 1, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
            RemakeTmpBMP;
            exit;
         END;

         //===========================-AUTOFLAT-=================================

         IF FDrawMode = dmAutoFlat THEN
         BEGIN
            //============================================.FLAT INACTIVE.=
            CreateBitmapExt(FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               0, 0, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
            //============================================.FLAT ACTIVE.=
            RemakeTmpBMP;
            //...__________________________________________________Shadow
            ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);
            IF boShadowSurround IN FOptions THEN
            BEGIN
               CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  0, 2, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
               CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  2, 0, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
            END;
            CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               2, 2, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
            //...__________________________________________________Highlight
            RemakeTmpBMP;
            ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
            CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               0, 0, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
            IF boShadowSurround IN FOptions THEN
            BEGIN
               CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  1, 0, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
               CreateBitmapExt(FGlyphActive.Canvas.Handle, TmpBMP, Rect(0, 0,
                  TmpBMP.Width, TmpBMP.Height),
                  0, 1, fwoNone, fdsDefault, true, FTransparentColor,
                     FDisabledMaskColor);
            END;
            RemakeTmpBMP;
         END;
         //============================================.FLAT PUSHED.=
         IF boDrawPushedAsFlat IN FOptions THEN
            exit;
         RemakeTmpBMP;
         //...__________________________________________________Highlight
         ChangeBitmapColor(TmpBMP, clBlack, FColorHighlight);
         IF boShadowSurround IN FOptions THEN
         BEGIN
            CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               0, 2, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
            CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               2, 0, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
         END;
         CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            2, 2, fwoNone, fdsDefault, true, FTransparentColor,
               FDisabledMaskColor);
         RemakeTmpBMP;
         //...__________________________________________________Shadow
         IF clBlack <> FColorShadow THEN
            ChangeBitmapColor(TmpBMP, clBlack, FColorShadow);

         CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
            TmpBMP.Width, TmpBMP.Height),
            0, 0, fwoNone, fdsDefault, true, FTransparentColor,
               FDisabledMaskColor);
         IF boShadowSurround IN FOptions THEN
         BEGIN
            CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               1, 0, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
            CreateBitmapExt(FGlyphPushed.Canvas.Handle, TmpBMP, Rect(0, 0,
               TmpBMP.Width, TmpBMP.Height),
               0, 1, fwoNone, fdsDefault, true, FTransparentColor,
                  FDisabledMaskColor);
         END;
         RemakeTmpBMP;
      FINALLY
         {      RemakeTmpBMP;
               ChangeBitmapColor( TmpBMP, clBlack, clBtnFace );
               CreateBitmapExt( FGlyphInactive.Canvas.Handle, TmpBMP, Rect(0,0,TmpBMP.Width,TmpBMP.Height),
                  1, 1, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor );
               CreateBitmapExt( FGlyphActive.Canvas.Handle, TmpBMP, Rect(0,0,TmpBMP.Width,TmpBMP.Height),
                  1, 1, fwoNone, fdsDefault, true, FTransparentColor, FDisabledMaskColor );
              } DeleteObject(SelectObject(MonoDC, OldMonoBMP));
      END;
   END;
END;

FUNCTION TJvgButton.IsMouseInControl: boolean;
VAR
   pt                         : TPoint;
   PixelColor                 : TCOLORREF;
BEGIN
   GetCursorPos(pt);
   pt := ScreenToClient(pt);
   IF fShowingAsPushedNow AND fPushed THEN
   BEGIN
      dec(pt.x, FShiftMaskWhenPushed.x);
      dec(pt.y, FShiftMaskWhenPushed.y);
   END
   ELSE IF FDrawMode = dmAutoShadow THEN
   BEGIN
      inc(pt.x, FShadowDepth);
      inc(pt.y, FShadowDepth);
   END;

   {  if (FDrawMode = dmAutoShadow)and(boDrawPushedAsFlat in Options) then
     begin
       dec( pt.x, FShadowDepth );
       dec( pt.y, FShadowDepth );
     end;}
   dec(pt.x);
   dec(pt.y);

   //	  (FDrawMode = dmAutoFlat)and(fPushed or FRaisedInactive
   IF FGlyphsIndexes.Mask = -1 THEN     //...__mask is absent_
   BEGIN
      WITH FGlyphs DO
         CASE FDrawMode OF
            dmAutoShadow:
               PixelColor := GetPixel(FGlyphPushed.Canvas.Handle, pt.x, pt.y);
            dmAutoFlat:
               PixelColor := GetPixel(FGlyphActive.Canvas.Handle, pt.x, pt.y);
         ELSE
            PixelColor := GetPixel(FGlyphInactive.Canvas.Handle, pt.x, pt.y);
         END;
      Result := (PixelColor <> FTransparentColor) AND (PixelColor <> -1);
   END
   ELSE                                 //...__use mask_
   BEGIN
      WITH FGlyphs DO
         PixelColor := GetPixel(FGlyphMask.Canvas.Handle, pt.x, pt.y);
      Result := (PixelColor = clWhite) AND (PixelColor <> -1);
   END;
END;

PROCEDURE TJvgButton.SmthChanged(Sender: TObject);
BEGIN
   GetBitmaps;
   Invalidate;
END;

PROCEDURE TJvgButton.GetBitmap_(Index: integer; VAR Bmp: TBitmap);
BEGIN
   TRY
      IF FDrawMode = dmUseImageList THEN
         FGlyphsList.GetBitmap(Index, Bmp)
      ELSE
      BEGIN
         IF Assigned(Bmp) THEN
         BEGIN
            Bmp.Free;
            Bmp := TBitmap.Create;
         END;
         Bmp.Assign(Glyph);
      END;
   EXCEPT
      MessageDlg('Error during access GlyphsList or Glyph property',
         mtError, [mbOk], 0);
      RAISE;
   END;
END;

PROCEDURE TJvgButton.OnBlinkTimer(Sender: TObject);
VAR
   ParentForm                 : TForm;
   i                          : integer;

   PROCEDURE Blink(FreeButton: TJvgButton);
   BEGIN
      WITH FreeButton DO
      BEGIN
         fNeedBlink := false;
         IF fShowingAsPushedNow THEN
            WITH FChangeColorOnPush DO
               IF (boBlinkWhenPushed IN Options) AND (FromColor <> ToColor) THEN
               BEGIN
                  fNeedBlink := true;
                  repaint;
                  exit;
               END
               ELSE
                  exit;
         IF fMouseInControl THEN
            WITH FChangeColorOnActivate DO
               IF (boBlinkWhenActive IN Options) AND (FromColor <> ToColor) THEN
               BEGIN
                  fNeedBlink := true;
                  repaint;
                  exit;
               END
               ELSE
                  exit;
         IF NOT fMouseInControl THEN
            WITH FChangeColorOnActivate DO
               IF (boBlinkWhenInactive IN Options) AND (FromColor <> ToColor)
                  THEN
               BEGIN
                  fNeedBlink := true;
                  repaint;
                  exit;
               END
               ELSE
                  exit;
      END;
   END;
BEGIN                                   //...main proc
   IF (NOT FTestMode) AND (csDesigning IN ComponentState) THEN
      exit;
   ParentForm := GetParentForm(self);
   FOR i := 0 TO ParentForm.ComponentCount - 1 DO
      IF (ParentForm.Components[i] IS TJvgButton)
         AND (TJvgButton(ParentForm.Components[i]).BlinkTimer = FBlinkTimer)
            THEN
         WITH ParentForm.Components[i] AS TJvgButton DO
         BEGIN
            Blink(TJvgButton(ParentForm.Components[i]));
         END;

END;

//*************************************{ . TJvgButton properties methods. }

PROCEDURE TJvgButton.SetDrawMode(Value: TDrawMode);
BEGIN
   IF FDrawMode = Value THEN
      exit;
   //  if (Value=dmUseGlyphList)and(not Assigned(GlyphList)) then
   //  if (Value<>dmUseimageList)and(not Assigned(Glyph)) then
   //   FGlyph.Assign(FGlyphInactive);
   FDrawMode := Value;

   GetBitmaps;
   Invalidate;
END;

PROCEDURE TJvgButton.SetGlyphsList(Value: TImageList);
BEGIN
   IF (NOT Assigned(Value)) {or(Value.Count<2)} THEN
      exit;
   FGlyphsList := Value;
   GetBitmaps;
   Invalidate;
END;

PROCEDURE TJvgButton.SetGlyph(Value: TBitmap);
BEGIN
   //  if (not Assigned(Value)) then exit;
   IF Assigned(FGlyph) THEN
   BEGIN
      FGlyph.Free;
      FGlyph := TBitmap.Create;
   END;
   FGlyph.Assign(Value);
   BEGIN
      GetBitmaps;
      Invalidate;
   END;
   AutoTransparentColor := AutoTransparentColor;
   //if FDrawMode <> dmUseImageList then begin GetBitmaps; Invalidate; end;
END;

PROCEDURE TJvgButton.SetNumGlyphs(Value: integer);
BEGIN
   IF (Value < 2) OR (Value > 4) THEN
      exit;
   FNumGlyphs := Value;
END;

PROCEDURE TJvgButton.SetTransparentColor(Value: TColor);
BEGIN
   IF (FAutoTrColor <> ftcUser) OR (FTransparentColor = Value) THEN
      exit;
   FTransparentColor := Value;
   GetBitmaps;
   Invalidate;
END;

PROCEDURE TJvgButton.SetEnabled(Value: boolean);
BEGIN
   IF FEnabled = Value THEN
      exit;
   FEnabled := Value;
   Repaint;
END;

PROCEDURE TJvgButton.SetShadowDepth(Value: word);
BEGIN
   IF FShadowDepth = Value THEN
      exit;
   FShadowDepth := Value;
   IF FDrawMode = dmAutoShadow THEN
   BEGIN
      GetBitmaps;
      Invalidate;
   END;
END;

PROCEDURE TJvgButton.SetColorHighlight(Value: TColor);
BEGIN
   IF FColorHighlight = Value THEN
      exit;
   FColorHighlight := Value;
   IF FDrawMode <> dmUseImageList THEN
   BEGIN
      GetBitmaps;
      Invalidate;
   END;
END;

PROCEDURE TJvgButton.SetColorShadow(Value: TColor);
BEGIN
   IF FColorShadow = Value THEN
      exit;
   FColorShadow := Value;
   IF FDrawMode <> dmUseImageList THEN
   BEGIN
      GetBitmaps;
      Invalidate;
   END;
END;

PROCEDURE TJvgButton.SetColorDarkShadow(Value: TColor);
BEGIN
   IF FColorDarkShadow = Value THEN
      exit;
   FColorDarkShadow := Value;
   IF FDrawMode <> dmUseImageList THEN
   BEGIN
      GetBitmaps;
      Invalidate;
   END;
END;

PROCEDURE TJvgButton.SetDisabledMaskColor(Value: TColor);
BEGIN
   IF FDisabledMaskColor = Value THEN
      exit;
   FDisabledMaskColor := Value;
   IF FDrawMode <> dmUseImageList THEN
   BEGIN
      GetBitmaps;
      Invalidate;
   END;
END;

PROCEDURE TJvgButton.SetOptions(Value: TglButtonOptions);
BEGIN
   IF FOptions = Value THEN
      exit;
   FOptions := Value;
   IF FDrawMode <> dmUseImageList THEN
   BEGIN
      GetBitmaps;
      Invalidate;
   END;
END;

PROCEDURE TJvgButton.SetAutoTrColor(Value: TglAutoTransparentColor);
VAR
   x, y                       : integer;
   TmpBmp_                    : TBitmap;
BEGIN
   FAutoTrColor := Value;
   TmpBmp_ := NIL;
   IF {(FAutoTrColor=ftcUser)or}(FGlyph.Width = 0) OR (FGlyph.Height = 0) THEN
      exit;
   TRY
      WITH FGlyph DO
         CASE FAutoTrColor OF
            ftcLeftTopPixel:
               BEGIN
                  x := 0;
                  y := 0;
               END;
            ftcLeftBottomPixel:
               BEGIN
                  x := 0;
                  y := Height - 1;
               END;
            ftcRightTopPixel:
               BEGIN
                  x := Width - 1;
                  y := 0;
               END;
            ftcRightBottomPixel:
               BEGIN
                  x := Width - 1;
                  y := Height - 1;
               END;
         ELSE
            exit;
         END;
      TmpBmp_ := TBitmap.Create;
      TmpBmp_.Assign(FGlyph);
      //  if not (csDesigning in ComponentState) then
      FTransparentColor := GetPixel(TmpBmp_.Canvas.Handle, x, y);
   FINALLY
      IF Assigned(TmpBmp_) THEN
         TmpBmp_.Free;
      GetBitmaps;
      Invalidate;
   END;
END;

PROCEDURE TJvgButton.SetBlinkTimer(Value: TTimer);
VAR
   ParentForm                 : TForm;
   i                          : integer;
   p1, p2                     : TNotifyEvent;
   Timer                      : TTimer;
BEGIN
   IF FBlinkTimer = Value THEN
      exit;
   IF Assigned(FBlinkTimer) THEN
   BEGIN
      p1 := FBlinkTimer.OnTimer;
      p2 := OnBlinkTimer;
      IF @FBlinkTimer.OnTimer = @p2 THEN //...points at me
      BEGIN
         ParentForm := GetParentForm(self);
         FOR i := 0 TO ParentForm.ComponentCount - 1 DO
            IF (ParentForm.Components[i] IS TJvgButton)
               AND (TJvgButton(ParentForm.Components[i]) <> self)
               AND (TJvgButton(ParentForm.Components[i]).BlinkTimer =
                  FBlinkTimer) THEN
            BEGIN
               Timer := FBlinkTimer;
               FBlinkTimer := NIL;
               Timer.OnTimer :=
                  TJvgButton(ParentForm.Components[i]).OnBlinkTimer;
               break;
            END;
         IF Assigned(FBlinkTimer) AND (@FBlinkTimer.OnTimer = @p2) THEN
            FBlinkTimer.OnTimer := NIL;
      END;
   END
   ELSE
   BEGIN
      FBlinkTimer := NIL;
   END;

   FBlinkTimer := Value;
   IF Assigned(FBlinkTimer) THEN
      FBlinkTimer.OnTimer := OnBlinkTimer;
END;

FUNCTION TJvgButton.GetBlinkTimer: TTimer;
BEGIN
   Result := NIL;
   TRY
      IF Assigned(FBlinkTimer) THEN
         IF Owner.Components[FBlinkTimer.ComponentIndex] = FBlinkTimer THEN
            Result := FBlinkTimer;
   EXCEPT
   END;
END;

PROCEDURE TJvgButton.SetTestMode(Value: boolean);
VAR
   ParentForm                 : TForm;
   i                          : integer;
BEGIN
   ParentForm := GetParentForm(self);
   FOR i := 0 TO ParentForm.ComponentCount - 1 DO
      IF (ParentForm.Components[i] IS TJvgButton) THEN
         TJvgButton(ParentForm.Components[i]).FTestMode := Value;
END;
//*************************************{ . TJvgGlyphsIndexes methods. }

CONSTRUCTOR TJvgGlyphsIndexes.Create;
BEGIN
   INHERITED Create;
   FInactive := 0;
   FPushed := 1;
   FActive := 2;
   FDisabled := -1;
   FMask := 3;
END;

PROCEDURE TJvgGlyphsIndexes.SetInactive(Value: integer);
BEGIN
   FInactive := Value;
   IF Assigned(OnChanged) THEN
      OnChanged(self);
END;

PROCEDURE TJvgGlyphsIndexes.SetPushed(Value: integer);
BEGIN
   FPushed := Value;
   IF Assigned(OnChanged) THEN
      OnChanged(self);
END;

PROCEDURE TJvgGlyphsIndexes.SetActive(Value: integer);
BEGIN
   FActive := Value;
   IF Assigned(OnChanged) THEN
      OnChanged(self);
END;

PROCEDURE TJvgGlyphsIndexes.SetDisabled(Value: integer);
BEGIN
   FDisabled := Value;
   IF Assigned(OnChanged) THEN
      OnChanged(self);
END;

PROCEDURE TJvgGlyphsIndexes.SetMask(Value: integer);
BEGIN
   FMask := Value;
   IF Assigned(OnChanged) THEN
      OnChanged(self);
END;

END.

