{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCheckBox.PAS, released on 2003-01-15.

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

UNIT JvgCheckBox;

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
   StdCtrls,
   ExtCtrls,
   JvgTypes,
   JvgCommClasses,
   JVComponent,
   JvgUtils;

TYPE

   TJvgCheckBox = CLASS(TJvGraphicControl)
   PRIVATE
      FChecked: boolean;
      FColors: TJvgLabelColors;
      FIllumination: TJvgIllumination;
      FGlyph: TBitmap;
      FGlyphOn: TBitmap;
      FGlyphOff: TBitmap;
      FGlyphDisabled: TBitmap;
      FGradient: TJvgGradient;
      FGroupIndex: integer;
      FGlyphShift: TJvgPointClass;
      FOptions: TglCBoxOptions;
      FTransparent: boolean;
      FTextStyles: TJvgLabelTextStyles;
      FDisabledMaskColor: TColor;
      FInterspace: integer;
      FFocusControl: TWinControl;
      FFocusControlMethod: TFocusControlMethod;
      FOnMouseEnter: TNotifyEvent;
      FOnMouseLeave: TNotifyEvent;
      FAfterPaint: TNotifyEvent;

      FGlyphKind: TglGlyphKind;

      FPrevWndProc: Pointer;
      FNewWndProc: Pointer;
      fActiveNow: boolean;
      fShowAsActiveWhileControlFocused: boolean;
      Img: TBitmap;
      fNeedUpdateOnlyMainText: boolean;
      fSuppressCMFontChanged: boolean;
      fOnlyTextStyleChanged: boolean;
      FAlignment: TLeftRight;
      FUNCTION IsCustomGlyph: Boolean;
      PROCEDURE SetChecked(Value: boolean);
      PROCEDURE SetGlyph(Value: TBitmap);
      FUNCTION GetGlyph: TBitmap;
      PROCEDURE SetGlyphOn(Value: TBitmap);
      FUNCTION GetGlyphOn: TBitmap;
      PROCEDURE SetGlyphOff(Value: TBitmap);
      FUNCTION GetGlyphOff: TBitmap;
      PROCEDURE SetGlyphDisabled(Value: TBitmap);
      FUNCTION GetGlyphDisabled: TBitmap;
      PROCEDURE SetGroupIndex(Value: integer);
      PROCEDURE SetOptions(Value: TglCBoxOptions);
      PROCEDURE SetTransparent(Value: boolean);
      PROCEDURE SetDisabledMaskColor(Value: TColor);
      PROCEDURE SetInterspace(Value: integer);
      PROCEDURE SetFocusControl(Value: TWinControl);
      PROCEDURE SetGlyphKind(Value: TglGlyphKind);

      PROCEDURE OnGradientChanged(Sender: TObject);
      PROCEDURE OnIlluminationChanged(Sender: TObject);
      PROCEDURE WMSize(VAR Message: TMessage); MESSAGE WM_SIZE;
      PROCEDURE CMFontChanged(VAR Message: TMessage); MESSAGE CM_FONTCHANGED;
      PROCEDURE OnLButtonUp(VAR Message: TMessage); MESSAGE WM_LBUTTONUP;
      PROCEDURE OnLButtonDown(VAR Message: TMessage); MESSAGE WM_LBUTTONDOWN;
      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
      PROCEDURE CMTextChanged(VAR Message: TMessage); MESSAGE CM_TEXTCHANGED;
      PROCEDURE CallMouseEnter; VIRTUAL;
      PROCEDURE CallMouseLeave; VIRTUAL;
      PROCEDURE SetAlignment(CONST Value: TLeftRight);
   PROTECTED
      PROCEDURE Paint; OVERRIDE;
      PROCEDURE HookFocusControlWndProc;
      PROCEDURE UnhookFocusControlWndProc;
      PROCEDURE FocusControlWndHookProc(VAR Msg_: TMessage);
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;
   PUBLIC
      fNeedRebuildBackground: boolean;
      FUNCTION GetCheckedItemInGroup: TJvgCheckBox;
      PROCEDURE SetCheckedItemInGroup(TagNo: integer);
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

   PUBLISHED
      {$IFDEF COMPILER5_UP}
      PROPERTY Anchors;
      {$ENDIF}
      PROPERTY Align;
      PROPERTY Caption;
      PROPERTY Enabled;
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
      PROPERTY Font;

      PROPERTY Alignment: TLeftRight READ FAlignment WRITE SetAlignment DEFAULT
         taRightJustify;
      PROPERTY GlyphKind: TglGlyphKind READ FGlyphKind WRITE SetGlyphKind DEFAULT
         fgkDefault;
      PROPERTY Checked: boolean READ FChecked WRITE SetChecked DEFAULT false;
      PROPERTY Glyph: TBitmap READ GetGlyph WRITE SetGlyph;
      PROPERTY GlyphOn: TBitmap READ GetGlyphOn WRITE SetGlyphOn STORED
         IsCustomGlyph;
      PROPERTY GlyphOff: TBitmap READ GetGlyphOff WRITE SetGlyphOff STORED
         IsCustomGlyph;
      PROPERTY GlyphDisabled: TBitmap READ GetGlyphDisabled WRITE
         SetGlyphDisabled STORED IsCustomGlyph;
      PROPERTY GroupIndex: integer READ FGroupIndex WRITE SetGroupIndex DEFAULT
         0;
      PROPERTY GlyphShift: TJvgPointClass READ FGlyphShift WRITE FGlyphShift;
      PROPERTY Transparent: boolean READ FTransparent WRITE SetTransparent
         DEFAULT false;
      PROPERTY TextStyles: TJvgLabelTextStyles READ FTextStyles WRITE
         FTextStyles;
      PROPERTY Colors: TJvgLabelColors READ FColors WRITE FColors;
      PROPERTY Options: TglCBoxOptions READ FOptions WRITE SetOptions;
      PROPERTY Gradient: TJvgGradient READ FGradient WRITE FGradient;
      PROPERTY Illumination: TJvgIllumination READ FIllumination WRITE
         FIllumination;
      PROPERTY DisabledMaskColor: TColor READ FDisabledMaskColor WRITE
         SetDisabledMaskColor
         DEFAULT clBlack;
      PROPERTY Interspace: integer READ FInterspace WRITE SetInterspace DEFAULT
         0;
      PROPERTY FocusControl: TWinControl READ FFocusControl WRITE
         SetFocusControl;
      PROPERTY FocusControlMethod: TFocusControlMethod READ FFocusControlMethod
         WRITE FFocusControlMethod
         DEFAULT fcmOnMouseDown;

      PROPERTY OnMouseEnter: TNotifyEvent READ FOnMouseEnter WRITE
         FOnMouseEnter;
      PROPERTY OnMouseLeave: TNotifyEvent READ FOnMouseLeave WRITE
         FOnMouseLeave;
      PROPERTY AfterPaint: TNotifyEvent READ FAfterPaint WRITE FAfterPaint;

   END;

IMPLEMENTATION
{$R JvgCheckBox.res}

//________________________________________________________ Methods _

CONSTRUCTOR TJvgCheckBox.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   ControlStyle := [csCaptureMouse, csOpaque, csClickEvents, csSetCaption,
      csReplicatable];
   //  ControlStyle := ControlStyle + [csOpaque, csReplicatable];

   //  FGlyphOn := TBitmap.Create;
   //  FGlyphOff := TBitmap.Create;
   Img := TBitmap.Create;
   TextStyles := TJvgLabelTextStyles.Create;
   Colors := TJvgLabelColors.Create;
   Gradient := TJvgGradient.Create;
   FIllumination := TJvgIllumination.Create;
   FGlyphShift := TJvgPointClass.Create;

   //..defaults
   Width := 80;
   Height := 17;
   FAlignment := taRightJustify;
   FChecked := false;
   FTransparent := false;
   FGradient.OnChanged := OnGradientChanged;
   FIllumination.OnChanged := OnIlluminationChanged;
   TextStyles.OnChanged := OnIlluminationChanged;
   Colors.OnChanged := OnIlluminationChanged;
   FGlyphShift.OnChanged := OnGradientChanged;
   FOptions := [fcoFastDraw];
   FGroupIndex := 0;
   FInterspace := 0;
   FFocusControlMethod := fcmOnMouseDown;
   fNeedRebuildBackground := true;

   Img.Canvas.Brush.Color := clBtnFace;
   Img.Canvas.Brush.Style := bsSolid;
   //  fNeedUpdateOnlyMainText := false;
   {$IFDEF FR_RUS}
   Font.CharSet := RUSSIAN_CHARSET;
   {$ENDIF}
   GlyphKind := fgkDefault;
END;
//______________________________________________________________

DESTRUCTOR TJvgCheckBox.Destroy;
BEGIN
   IF Assigned(FGlyphOn) THEN
      FGlyphOn.Free;
   IF Assigned(FGlyphOff) THEN
      FGlyphOff.Free;
   IF Assigned(FGlyph) THEN
      FGlyph.Free;
   IF Assigned(FGlyphDisabled) THEN
      FGlyphDisabled.Free;
   Img.Free;
   FTextStyles.Free;
   FColors.Free;
   FGradient.Free;
   FIllumination.Free;
   FGlyphShift.Free;
   SetFocusControl(NIL);
   INHERITED;
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.CMFontChanged(VAR Message: TMessage);
BEGIN
   IF fSuppressCMFontChanged THEN
      exit;
   Img.Canvas.Font.Assign(Font);
   Repaint;
   INHERITED;
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.CMMouseEnter(VAR Message: TMessage);
BEGIN
   IF NOT Enabled OR (fcoIgnoreMouse IN Options) OR
      fShowAsActiveWhileControlFocused THEN
      exit;
   IF Assigned(FocusControl) AND (FocusControlMethod = fcmOnMouseEnter) THEN
      FocusControl.SetFocus;
   fNeedRebuildBackground := true;
   fActiveNow := true;
   WITH TextStyles, Colors DO
      IF (Passive <> Active) OR (fcoUnderlinedActive IN Options) THEN
         Paint
      ELSE IF (fcoDelineatedText IN Options) AND (DelineateActive <> Delineate)
         THEN
         Paint
      ELSE IF (NOT Transparent) AND (Colors.Background <>
         Colors.BackgroundActive) THEN
         Paint
      ELSE IF (TextActive <> Text) OR (fcoUnderlinedActive IN Options) THEN
      BEGIN
         fNeedUpdateOnlyMainText := true;
         Paint;
      END;
   IF Assigned(FOnMouseEnter) THEN
      FOnMouseEnter(self);
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.CMMouseLeave(VAR Message: TMessage);
BEGIN
   IF NOT Enabled OR (fcoIgnoreMouse IN Options) OR
      fShowAsActiveWhileControlFocused THEN
      exit;
   fNeedRebuildBackground := true;
   fActiveNow := false;
   WITH TextStyles, Colors DO
      IF (Passive <> Active) OR (fcoUnderlinedActive IN Options) THEN
         Paint
      ELSE IF (fcoDelineatedText IN Options) AND (DelineateActive <> Delineate)
         THEN
         Paint
      ELSE IF (NOT Transparent) AND (Colors.Background <>
         Colors.BackgroundActive) THEN
         Paint
      ELSE IF TextActive <> Text THEN
      BEGIN
         fNeedUpdateOnlyMainText := true;
         Paint;
      END;
   IF Assigned(FOnMouseLeave) THEN
      FOnMouseLeave(self);
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.OnLButtonUp(VAR Message: TMessage);
VAR
   pt                         : TPoint;
BEGIN
   IF NOT Enabled OR (fcoIgnoreMouse IN Options) THEN
      exit;
   GetCursorPos(pt);
   pt := ScreenToClient(pt);
   IF PtInRect(ClientRect, pt) THEN
      SetChecked(NOT FChecked);
   IF Assigned(FocusControl) THEN
   BEGIN
      IF fcoEnabledFocusControlWhileChecked IN Options THEN
         FocusControl.Enabled := FChecked;
      IF (FocusControlMethod = fcmOnMouseUp)
         AND FocusControl.CanFocus THEN
         FocusControl.SetFocus;
   END;
   INHERITED;
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.OnLButtonDown(VAR Message: TMessage);
BEGIN
   IF NOT Enabled OR (fcoIgnoreMouse IN Options) THEN
      exit;
   INHERITED;
   IF (FocusControlMethod = fcmOnMouseDown)
      AND Assigned(FocusControl)
      AND FocusControl.CanFocus THEN
      FocusControl.SetFocus;
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.WMSize(VAR Message: TMessage);
BEGIN
   INHERITED;
   //  Img.Width := Width; Img.Height := Height;
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.CMTextChanged(VAR Message: TMessage);
BEGIN
   Invalidate;
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.Paint;
VAR
   x, y                       : integer;
   DrawState                  : TglDrawState;
   Bitmap                     : TBitmap;
   FontColor                  : TColor;
   CurrTextStyle              : TglTextStyle;
   CurrDelinColor             : TColor;
   isGradientActive           : boolean;
   Size                       : TSize;
   R                          : TRect;
   BackBrush                  : HBRUSH;
BEGIN
   //fNeedUpdateOnlyMainText := false;
   //fNeedRebuildBackground := false;
   fSuppressCMFontChanged := true;
   IF fcoBoldChecked IN Options THEN
      IF Checked THEN
         Font.Style := Font.Style + [fsBold]
      ELSE
         Font.Style := Font.Style - [fsBold];
   IF Enabled THEN
   BEGIN
      IF Checked THEN
         Bitmap := FGlyphOn
      ELSE
         Bitmap := FGlyphOff;
      DrawState := fdsDefault;
   END
   ELSE
   BEGIN
      IF FGlyphDisabled.Handle <> 0 THEN
      BEGIN
         Bitmap := FGlyphDisabled;
         DrawState := fdsDefault;
      END
      ELSE
      BEGIN
         IF Checked THEN
            Bitmap := FGlyphOn
         ELSE
            Bitmap := FGlyphOff;
         DrawState := fdsDefault;
      END;
   END;

   //...CAPTION
   SetBkMode(Canvas.handle, integer(Transparent));
   WITH TextStyles, Colors DO
   BEGIN
      IF fActiveNow THEN
      BEGIN
         CurrTextStyle := Active;
         CurrDelinColor := DelineateActive;
         FontColor := TextActive;
      END
      ELSE IF Enabled THEN
      BEGIN
         CurrTextStyle := Passive;
         CurrDelinColor := Delineate;
         FontColor := Text;
      END
      ELSE
      BEGIN
         CurrTextStyle := Disabled;
         CurrDelinColor := Delineate;
         FontColor := TextDisabled;
      END;

      IF fcoUnderlinedActive IN Options THEN
         IF fActiveNow THEN
            Font.Style := Font.Style + [fsUnderline]
         ELSE
            Font.Style := Font.Style - [fsUnderline];
   END;
   GetTextExtentPoint32(Img.Canvas.Handle, PChar(Caption),
      length(Caption), Size);
   y := max(0, (Height - Size.cy) DIV 2);
   X := 0;
   IF Assigned(FGlyphOn) THEN
      X := max(X, FGlyphOn.Width);
   IF Assigned(FGlyphOff) THEN
      X := max(X, FGlyphOff.Width);
   IF Assigned(FGlyphDisabled) THEN
      X := max(X, FGlyphDisabled.Width);
   IF Assigned(FGlyph) THEN
      X := max(X, FGlyph.Width);

   Img.Width := Width;
   Img.Height := Height;

   IF (NOT fNeedUpdateOnlyMainText) {and (not Transparent)} THEN
   BEGIN
      R := GetClientRect;
      IF fActiveNow THEN
         BackBrush := CreateSolidBrush(ColorToRGB(Colors.BackgroundActive))
      ELSE
         BackBrush := CreateSolidBrush(ColorToRGB(Colors.Background));
      FillRect(Img.Canvas.Handle, R, BackBrush);
      DeleteObject(BackBrush);
   END;

   IF FTransparent AND (NOT fNeedUpdateOnlyMainText) THEN
      IF (NOT (fcoFastDraw IN Options)) OR fNeedRebuildBackground OR (csDesigning
         IN ComponentState) THEN
         GetParentImageRect(self, Bounds(Left, Top, Width, Height),
            Img.Canvas.Handle);

   IF Alignment = taLeftJustify THEN
   BEGIN
      X := 0;
      IF FGlyph <> NIL THEN
         inc(X, FGlyph.Width);
   END
   ELSE
      inc(X, Interspace);

   //...Supress Gradient if needed
   isGradientActive := Gradient.Active;
   IF fActiveNow AND (Colors.TextActive <> Colors.Text) THEN
      Gradient.FActive := false;

   ExtTextOutExt(Img.Canvas.Handle, X, y, GetClientRect, Caption,
      CurrTextStyle, fcoDelineatedText IN Options,
      fNeedUpdateOnlyMainText, FontColor, CurrDelinColor,
      Colors.Highlight, Colors.Shadow,
      Illumination, Gradient, Font);

   Gradient.FActive := isGradientActive;

   IF NOT fNeedUpdateOnlyMainText THEN
   BEGIN
      IF (NOT (fcoFastDraw IN Options)) OR fNeedRebuildBackground OR (csDesigning
         IN ComponentState) THEN
      BEGIN
         IF FGlyph <> NIL THEN //...TransparentColor -> Left Bottom Pixel
         BEGIN
            IF NOT Transparent THEN
               ChangeBitmapColor(FGlyph, GetPixel(FGlyph.Canvas.Handle, 0,
                  FGlyph.Height - 1), clBtnFace);

            // glyph always left
            CreateBitmapExt(Img.Canvas.Handle, FGlyph, ClientRect, 0, max(0,
               (Height - FGlyph.Height) DIV 2),
               fwoNone, DrawState, Transparent,
               GetPixel(FGlyph.Canvas.Handle, 0, FGlyph.Height - 1)
                  {TransparentColor},
               DisabledMaskColor);
         END;
         fNeedRebuildBackground := false;
      END;
      IF NOT Transparent THEN
         IF fActiveNow THEN
            ChangeBitmapColor(Bitmap, GetPixel(Bitmap.Canvas.Handle, 0,
               Bitmap.Height - 1), Colors.BackgroundActive)
         ELSE
            ChangeBitmapColor(Bitmap, GetPixel(Bitmap.Canvas.Handle, 0,
               Bitmap.Height - 1), Colors.Background);

      IF Alignment = taRightJustify THEN
         X := GlyphShift.X
      ELSE
         X := Width - Bitmap.Width;

      IF Assigned(Bitmap) THEN
         CreateBitmapExt(Img.Canvas.Handle, Bitmap, ClientRect, X,
            integer(GlyphShift.Y + max(0, (Height - Bitmap.Height) DIV 2)),
            fwoNone, DrawState, Transparent,
            GetPixel(Bitmap.Canvas.Handle, 0, Bitmap.Height - 1),
            DisabledMaskColor);
   END;

   BitBlt(Canvas.Handle, 0, 0, Img.Width, Img.Height, Img.Canvas.Handle, 0, 0,
      SRCCOPY);

   fSuppressCMFontChanged := false;
   fOnlyTextStyleChanged := false;
   fNeedUpdateOnlyMainText := false;
   IF Assigned(FAfterPaint) THEN
      FAfterPaint(self);
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
   IF (AComponent = FocusControl) AND (Operation = opRemove) THEN
   BEGIN                                {UnhookFocusControlWndProc;}
      FFocusControl := NIL;
   END;
END;
//______

PROCEDURE TJvgCheckBox.CallMouseEnter;
VAR
   EmptyMsg                   : TMessage;
BEGIN
   CMMouseEnter(EmptyMsg);
END;
//______

PROCEDURE TJvgCheckBox.CallMouseLeave;
VAR
   EmptyMsg                   : TMessage;
BEGIN
   CMMouseLeave(EmptyMsg);
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.HookFocusControlWndProc;
VAR
   P                          : Pointer;
BEGIN
   P := Pointer(GetWindowLong(FocusControl.Handle, GWL_WNDPROC));
   IF (P <> FNewWndProc) THEN
   BEGIN
      FPrevWndProc := P;
      FNewWndProc := MakeObjectInstance(FocusControlWndHookProc);
      SetWindowLong(FocusControl.Handle, GWL_WNDPROC, LongInt(FNewWndProc));
   END;
END;
//______

PROCEDURE TJvgCheckBox.UnhookFocusControlWndProc;
BEGIN
   //  if not(csDesigning in ComponentState) then exit;
   IF (FNewWndProc <> NIL) AND (FPrevWndProc <> NIL)
      AND (Pointer(GetWindowLong(FocusControl.Handle, GWL_WNDPROC)) =
         FNewWndProc) THEN
   BEGIN
      SetWindowLong(FocusControl.Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
      FNewWndProc := NIL;
   END;
END;
//______

PROCEDURE TJvgCheckBox.FocusControlWndHookProc(VAR Msg_: TMessage);
BEGIN
   CASE Msg_.Msg OF
      WM_SETFOCUS:
         BEGIN
            CallMouseEnter;
            fShowAsActiveWhileControlFocused := true;
         END;
      WM_KILLFOCUS:
         BEGIN
            fShowAsActiveWhileControlFocused := false;
            CallMouseLeave;
         END;
      WM_DESTROY: {fNeedRehookFocusControl := true};
   END;
   WITH Msg_ DO
      Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg, WParam,
         LParam);
END;
//______

PROCEDURE TJvgCheckBox.SetFocusControl(Value: TWinControl);
BEGIN
   IF FFocusControl = Value THEN
      exit;
   IF (fcoActiveWhileControlFocused IN Options) AND Assigned(FFocusControl) THEN
      UnhookFocusControlWndProc;
   FFocusControl := Value;
   IF (fcoActiveWhileControlFocused IN Options) AND Assigned(FFocusControl) THEN
      HookFocusControlWndProc;
END;
//______

PROCEDURE TJvgCheckBox.OnGradientChanged(Sender: TObject);
BEGIN
   IF NOT (csLoading IN ComponentState) THEN
      fNeedUpdateOnlyMainText := true;
   Repaint;
END;
//______________________________________________________________

PROCEDURE TJvgCheckBox.OnIlluminationChanged(Sender: TObject);
BEGIN
   CalcShadowAndHighlightColors((Parent AS TWinControl).Brush.Color, Colors);
   Repaint;
END;
//______

FUNCTION TJvgCheckBox.IsCustomGlyph: Boolean;
BEGIN
   Result := FGlyphKind = fgkCustom;
END;

FUNCTION TJvgCheckBox.GetCheckedItemInGroup: TJvgCheckBox;
VAR
   i                          : integer;
BEGIN
   IF FChecked THEN
   BEGIN
      Result := self;
      exit;
   END;
   Result := NIL;
   IF GroupIndex <> 0 THEN
   BEGIN
      FOR i := 0 TO Owner.ComponentCount - 1 DO
         IF (OWner.Components[i] IS TJvgCheckBox)
            AND (TJvgCheckBox(OWner.Components[i]).GroupIndex = GroupIndex)
            AND (TJvgCheckBox(OWner.Components[i]).Checked) THEN
         BEGIN
            Result := TJvgCheckBox(OWner.Components[i]);
            break;
         END;
   END;
END;

PROCEDURE TJvgCheckBox.SetCheckedItemInGroup(TagNo: integer);
VAR
   i                          : integer;
BEGIN
   IF GroupIndex <> 0 THEN
   BEGIN
      FOR i := 0 TO Owner.ComponentCount - 1 DO
         IF (OWner.Components[i] IS TJvgCheckBox)
            AND (TJvgCheckBox(OWner.Components[i]).GroupIndex = GroupIndex)
            AND (TJvgCheckBox(OWner.Components[i]).Tag = TagNo) THEN
         BEGIN
            TJvgCheckBox(OWner.Components[i]).Checked := true;
            break;
         END;
   END;
END;
//...______________________________________________PROPERTIES METHODS

PROCEDURE TJvgCheckBox.SetChecked(Value: boolean);
VAR
   i                          : integer;
BEGIN
   IF FChecked = Value THEN
      exit;
   fNeedRebuildBackground := true;
   IF GroupIndex <> 0 THEN
   BEGIN
      IF NOT FChecked THEN
      BEGIN
         FOR i := 0 TO Owner.ComponentCount - 1 DO
            IF (OWner.Components[i] IS TJvgCheckBox)
               AND (TJvgCheckBox(OWner.Components[i]).GroupIndex = GroupIndex)
               AND (TJvgCheckBox(OWner.Components[i]).Checked)
               AND (OWner.Components[i] <> Self) THEN
            BEGIN
               TJvgCheckBox(OWner.Components[i]).FChecked := false;
               TJvgCheckBox(OWner.Components[i]).fNeedRebuildBackground := true;
               TJvgCheckBox(OWner.Components[i]).Repaint;
            END;
         FChecked := true;
      END;
   END
   ELSE
      FChecked := Value;
   Repaint;
END;

PROCEDURE TJvgCheckBox.SetGlyph(Value: TBitmap);
BEGIN
   IF Assigned(FGlyph) THEN
      FGlyph.Free;
   FGlyph := TBitmap.Create;
   FGlyph.Assign(Value);
   fNeedRebuildBackground := true;
   Invalidate;
END;

FUNCTION TJvgCheckBox.GetGlyph: TBitmap;
BEGIN
   IF NOT Assigned(FGlyph) THEN
      FGlyph := TBitmap.Create;
   Result := FGlyph;
END;

PROCEDURE TJvgCheckBox.SetGlyphOn(Value: TBitmap);
BEGIN
   IF Assigned(FGlyphOn) THEN
      FGlyphOn.Free;
   FGlyphOn := TBitmap.Create;
   FGlyphKind := fgkCustom;
   FGlyphOn.Assign(Value);
   Invalidate;
END;

FUNCTION TJvgCheckBox.GetGlyphOn: TBitmap;
BEGIN
   IF NOT Assigned(FGlyphOn) THEN
      FGlyphOn := TBitmap.Create;
   Result := FGlyphOn;
END;

PROCEDURE TJvgCheckBox.SetGlyphOff(Value: TBitmap);
BEGIN
   IF Assigned(FGlyphOff) THEN
      FGlyphOff.Free;
   FGlyphOff := TBitmap.Create;
   FGlyphKind := fgkCustom;
   FGlyphOff.Assign(Value);
   Invalidate;
END;

FUNCTION TJvgCheckBox.GetGlyphOff: TBitmap;
BEGIN
   IF NOT Assigned(FGlyphOff) THEN
      FGlyphOff := TBitmap.Create;
   Result := FGlyphOff;
END;

PROCEDURE TJvgCheckBox.SetGlyphDisabled(Value: TBitmap);
BEGIN
   IF Assigned(FGlyphDisabled) THEN
      FGlyphDisabled.Free;
   FGlyphDisabled := TBitmap.Create;
   FGlyphDisabled.Assign(Value);
   Invalidate;
END;

FUNCTION TJvgCheckBox.GetGlyphDisabled: TBitmap;
BEGIN
   IF NOT Assigned(FGlyphDisabled) THEN
      FGlyphDisabled := TBitmap.Create;
   Result := FGlyphDisabled;
END;

PROCEDURE TJvgCheckBox.SetGroupIndex(Value: integer);
BEGIN
   IF FGroupIndex = Value THEN
      exit;
   FGroupIndex := Value;
   IF FChecked AND (Value <> 0) THEN
   BEGIN
      FChecked := false;
      //    SetChecked( true );
      FChecked := true;
   END;
END;

PROCEDURE TJvgCheckBox.SetOptions(Value: TglCBoxOptions);
BEGIN
   IF FOptions = Value THEN
      exit;
   FOptions := Value;
   CalcShadowAndHighlightColors((Parent AS TWinControl).Brush.Color, Colors);
   Invalidate;
END;

PROCEDURE TJvgCheckBox.SetTransparent(Value: boolean);
BEGIN
   IF FTransparent = Value THEN
      exit;
   FTransparent := Value;
   Repaint;
END;

PROCEDURE TJvgCheckBox.SetDisabledMaskColor(Value: TColor);
BEGIN
   IF FDisabledMaskColor = Value THEN
      exit;
   FDisabledMaskColor := Value;
   fNeedRebuildBackground := true;
   Invalidate;
END;

PROCEDURE TJvgCheckBox.SetInterspace(Value: integer);
BEGIN
   IF FInterspace = Value THEN
      exit;
   FInterspace := Value;
   fNeedRebuildBackground := true;
   Invalidate;
END;

PROCEDURE TJvgCheckBox.SetGlyphKind(Value: TglGlyphKind);
BEGIN
   IF FGlyphKind <> Value THEN
      FGlyphKind := Value;

   IF (FGlyphKind = fgkCustom) AND (csReading IN ComponentState) THEN
   BEGIN
      GlyphOn := NIL;
      GlyphOff := NIL;
      GlyphDisabled := NIL;
   END
   ELSE
      //if (csDesigning in ComponentState){and not(csLoading in ComponentState)}then
   BEGIN
      IF NOT Assigned(FGlyphOn) THEN
         FGlyphOn := TBitmap.Create;
      IF NOT Assigned(FGlyphOff) THEN
         FGlyphOff := TBitmap.Create;
      IF NOT Assigned(FGlyphDisabled) THEN
         FGlyphDisabled := TBitmap.Create;
      FGlyphOn.LoadFromResourceName(hInstance, 'ON');
      FGlyphOff.LoadFromResourceName(hInstance, 'OFF');
      FGlyphDisabled.LoadFromResourceName(hInstance, 'DISABLED');
   END;

END;

PROCEDURE TJvgCheckBox.SetAlignment(CONST Value: TLeftRight);
BEGIN
   FAlignment := Value;
   Invalidate;
END;

END.

