{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLabel.PAS, released on 2003-01-15.

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

UNIT JvgLabel;

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
   JVComponent,
   JvgCommClasses,
   JvgUtils;
CONST
   FTextAlign                 = DT_LEFT OR DT_SINGLELINE;
   RadianEscapments              : ARRAY[TgllabelDir] OF integer = (0, -1800, -900,
      900);
TYPE

   TJvgCustomLabel = CLASS(TJvGraphicControl)
   PRIVATE
      FAutoSize: boolean;
      FFocusControl: TWinControl;
      FFocusControlMethod: TFocusControlMethod;
      FTransparent: boolean;
      FOnMouseEnter: TNotifyEvent;
      FOnMouseleave: TNotifyEvent;
      FPrevWndProc: Pointer;
      FNewWndProc: Pointer;
      PROCEDURE SetFocusControl(Value: TWinControl);
      PROCEDURE SetTransparent(Value: boolean);
      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
      PROCEDURE WMLMouseUP(VAR Message: TMessage); MESSAGE WM_LBUTTONUP;
      PROCEDURE WMLMouseDown(VAR Message: TMessage); MESSAGE WM_LBUTTONDOWN;
      PROCEDURE CMTextChanged(VAR Message: TMessage); MESSAGE CM_TEXTCHANGED;
      PROCEDURE CallMouseEnter; VIRTUAL;
      PROCEDURE CallMouseLeave; VIRTUAL;
   PROTECTED
      fActiveNow: boolean;
      fShowAsActiveWhileControlFocused: boolean;
      ActiveWhileControlFocused: boolean;
      fNeedRehookFocusControl: boolean;
      FExternalCanvas: TCanvas;
      PROCEDURE HookFocusControlWndProc;
      PROCEDURE UnhookFocusControlWndProc;
      PROCEDURE FocusControlWndHookProc(VAR Msg_: TMessage);
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;

      PROPERTY AutoSize: boolean READ FAutoSize WRITE FAutoSize DEFAULT true;
      PROPERTY FocusControl: TWinControl READ FFocusControl WRITE
         SetFocusControl;
      PROPERTY FocusControlMethod: TFocusControlMethod READ FFocusControlMethod
         WRITE FFocusControlMethod
         DEFAULT fcmOnMouseDown;
      PROPERTY Transparent: boolean READ FTransparent WRITE SetTransparent
         DEFAULT true;
      PROPERTY OnMouseEnter: TNotifyEvent READ FOnMouseEnter WRITE
         FOnMouseEnter;
      PROPERTY OnMouseLeave: TNotifyEvent READ FOnMouseLeave WRITE
         FOnMouseLeave;
      PROPERTY ExternalCanvas: TCanvas READ FExternalCanvas WRITE
         FExternalCanvas;
      PROCEDURE Paint; OVERRIDE;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   END;

   TJvgLabel = CLASS(TJvgCustomLabel)
   PRIVATE
      FDirection: TglLabelDir;
      FTextStyles: TJvgLabelTextStyles;
      FColors: TJvgLabelColors;
      FFontWeight: TFontWeight;
      //    FActiveTextColor	  :TColor;
      FOptions: TglLabelOptions;
      FSupressPaint: boolean;
      FGradient: TJvgGradient;
      FIllumination: TJvgIllumination;
      FTexture: TBitmap;
      FBackground: TBitmap;
      FTextureImage: TImage;
      FBackgroundImage: TImage;
      FAlignment: TAlignment;
      uFontWeight: word;
      fRunOnce: boolean;
      bFirstCreate: boolean;
      fNeedUpdateOnlyMainText: boolean;
      fNeedRemakeTextureMask: boolean;
      Img: TBitmap;
      TextureMask: TBitmap;
      BackgroundBmp: TBitmap;
      TextureBmp: TBitmap;
      TargetCanvas: TCanvas;

      PROCEDURE SetDirection(Value: TglLabelDir);
      PROCEDURE SetFontWeight(Value: TFontWeight);
      PROCEDURE SetOptions(Value: TglLabelOptions);
      PROCEDURE SetTexture(Value: TBitmap);
      PROCEDURE SetBackground(Value: TBitmap);
      FUNCTION GetTexture: TBitmap;
      FUNCTION GetBackground: TBitmap;
      PROCEDURE SetTextureImage(Value: TImage);
      PROCEDURE SetBackgroundImage(Value: TImage);
      PROCEDURE SetAlignment(Value: TAlignment);

      PROCEDURE OnGradientChanged(Sender: TObject);
      PROCEDURE OnIlluminationChanged(Sender: TObject);
      PROCEDURE CreateLabelFont;
      PROCEDURE InvalidateLabel(UpdateBackgr: boolean);
      PROCEDURE CMFontChanged(VAR Message: TMessage); MESSAGE CM_FONTCHANGED;
      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
      PROCEDURE CallMouseEnter; OVERRIDE;
      PROCEDURE CallMouseLeave; OVERRIDE;
      //    procedure WMLMouseUP(var Message: TMessage); message WM_LBUTTONUP;
      //    procedure WMLMouseDown(var Message: TMessage); message WM_LBUTTONDOWN;
   PROTECTED
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;
      PROCEDURE Loaded; OVERRIDE;

   PUBLIC
      FreeFont: TFont;
      PROPERTY Canvas;
      PROPERTY ExternalCanvas;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Paint; OVERRIDE;
      PROPERTY SupressPaint: boolean READ FSupressPaint WRITE FSupressPaint;
   PUBLISHED
      {$IFDEF COMPILER5_UP}
      PROPERTY Anchors;
      {$ENDIF}
      PROPERTY Align;
      PROPERTY Caption;
      PROPERTY DragCursor;
      PROPERTY DragMode;
      PROPERTY Enabled;
      PROPERTY Font;
      PROPERTY ParentColor;
      PROPERTY ParentFont;
      PROPERTY ParentShowHint;
      PROPERTY PopupMenu;
      //    property ShowAccelChar;
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
      PROPERTY OnMouseEnter;
      PROPERTY OnMouseLeave;
      PROPERTY FocusControl;
      PROPERTY FocusControlMethod;
      PROPERTY AutoSize;
      PROPERTY Transparent;

      PROPERTY Direction: TglLabelDir READ FDirection WRITE SetDirection
         DEFAULT fldLeftRight;
      PROPERTY TextStyles: TJvgLabelTextStyles READ FTextStyles WRITE
         FTextStyles;
      PROPERTY Colors: TJvgLabelColors READ FColors WRITE FColors;
      PROPERTY FontWeight: TFontWeight READ FFontWeight WRITE SetFontWeight;
      PROPERTY Options: TglLabelOptions READ FOptions WRITE SetOptions;
      PROPERTY Gradient: TJvgGradient READ FGradient WRITE FGradient;
      PROPERTY Illumination: TJvgIllumination READ FIllumination WRITE
         FIllumination;
      PROPERTY Texture: TBitmap READ GetTexture WRITE SetTexture;
      PROPERTY Background: TBitmap READ GetBackground WRITE SetBackground;
      PROPERTY TextureImage: TImage READ FTextureImage WRITE SetTextureImage;
      PROPERTY BackgroundImage: TImage READ FBackgroundImage WRITE
         SetBackgroundImage;
      PROPERTY Alignment: TAlignment READ FAlignment WRITE SetAlignment;

   END;

   TJvgStaticTextLabel = CLASS(TJvgCustomLabel)
   PRIVATE
      FActiveColor: TColor;
      FAlignment: TglAlignment;
      FOptions: TglStTextOptions;
      FWordWrap: boolean;

      PROCEDURE DrawTextBroadwise(Canvas: TCanvas);
      PROCEDURE AdjustBounds;
      PROCEDURE SetAlignment(Value: TglAlignment);
      PROCEDURE SetOptions(Value: TglStTextOptions);
      PROCEDURE SetWordWrap(Value: boolean);
      PROCEDURE SetAutoSize(Value: boolean);
      FUNCTION GetAutoSize: boolean;
      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
      PROCEDURE CallMouseEnter; OVERRIDE;
      PROCEDURE CallMouseLeave; OVERRIDE;
   PROTECTED
   PUBLIC
      PROCEDURE Paint; OVERRIDE;
      PROPERTY Canvas;
      PROPERTY ExternalCanvas;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
   PUBLISHED
      {$IFDEF COMPILER5_UP}
      PROPERTY Anchors;
      {$ENDIF}
      PROPERTY Align;
      PROPERTY Caption;
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
      PROPERTY OnMouseEnter;
      PROPERTY OnMouseLeave;
      PROPERTY FocusControl;
      PROPERTY FocusControlMethod;
      PROPERTY Transparent;
      PROPERTY ActiveColor: TColor READ FActiveColor WRITE FActiveColor
         DEFAULT clWhite;
      PROPERTY Alignment: TglAlignment READ FAlignment WRITE SetAlignment
         DEFAULT ftaBroadwise;
      PROPERTY AutoSize: boolean READ GetAutoSize WRITE SetAutoSize;
      PROPERTY Options: TglStTextOptions READ FOptions WRITE SetOptions;
      PROPERTY WordWrap: boolean READ FWordWrap WRITE SetWordWrap
         DEFAULT true;
   END;

   TJvgGlyphLabel = CLASS(TJvgLabel)
   PRIVATE
      FGlyphOn: TBitmap;
      FGlyphOff: TBitmap;
      FGlyphDisabled: TBitmap;
      FGlyphKind: TglGlyphKind;

      FUNCTION IsCustomGlyph: Boolean;
      PROCEDURE SetGlyphOn(Value: TBitmap);
      FUNCTION GetGlyphOn: TBitmap;
      PROCEDURE SetGlyphOff(Value: TBitmap);
      FUNCTION GetGlyphOff: TBitmap;
      PROCEDURE SetGlyphDisabled(Value: TBitmap);
      FUNCTION GetGlyphDisabled: TBitmap;
      PROCEDURE SetGlyphKind(Value: TglGlyphKind);
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY GlyphKind: TglGlyphKind READ FGlyphKind WRITE SetGlyphKind DEFAULT
         fgkDefault;
      PROPERTY GlyphOn: TBitmap READ GetGlyphOn WRITE SetGlyphOn STORED true;
      PROPERTY GlyphOff: TBitmap READ GetGlyphOff WRITE SetGlyphOff STORED true;
      PROPERTY GlyphDisabled: TBitmap READ GetGlyphDisabled WRITE
         SetGlyphDisabled STORED IsCustomGlyph;
   END;

PROCEDURE Register;

IMPLEMENTATION

{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ TJvgCustomLabel _

CONSTRUCTOR TJvgCustomLabel.Create(AOwner: TComponent);
BEGIN
   ControlStyle := ControlStyle + [csOpaque, csReplicatable];
   ActiveWhileControlFocused := true;
   FAutoSize := true;
   FTransparent := true;
   FFocusControlMethod := fcmOnMouseDown;
   INHERITED Create(AOwner);
END;
//______

DESTRUCTOR TJvgCustomLabel.Destroy;
BEGIN
   SetFocusControl(NIL);
   INHERITED Destroy;
END;
//______

PROCEDURE TJvgCustomLabel.Paint;
BEGIN //...if FocusControl have changed his parent in Run-Time...
   IF fNeedRehookFocusControl THEN
      HookFocusControlWndProc;
   //don't inherited;
END;

PROCEDURE TJvgCustomLabel.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
   IF (AComponent = FocusControl) AND (Operation = opRemove) THEN
   BEGIN                                {UnhookFocusControlWndProc;}
      FFocusControl := NIL;
   END;
END;
//______

PROCEDURE TJvgCustomLabel.CMMouseEnter(VAR Message: TMessage);
BEGIN
   IF Assigned(FOnMouseEnter) THEN
      FOnMouseEnter(self);
   IF Assigned(FocusControl) AND (FocusControlMethod = fcmOnMouseEnter) THEN
      FocusControl.SetFocus;
END;
//______

PROCEDURE TJvgCustomLabel.CMMouseLeave(VAR Message: TMessage);
BEGIN
   IF Assigned(FOnMouseLeave) THEN
      FOnMouseLeave(self);
END;
//______

PROCEDURE TJvgCustomLabel.WMLMouseUP(VAR Message: TMessage);
BEGIN
   INHERITED;
   IF Enabled AND (FocusControlMethod = fcmOnMouseUp)
      AND Assigned(FocusControl)
      AND FocusControl.CanFocus THEN
      FocusControl.SetFocus;
END;
//______

PROCEDURE TJvgCustomLabel.WMLMouseDown(VAR Message: TMessage);
BEGIN
   INHERITED;
   IF Enabled AND (FocusControlMethod = fcmOnMouseDown)
      AND Assigned(FocusControl)
      AND FocusControl.CanFocus THEN
      FocusControl.SetFocus;
END;
//______

PROCEDURE TJvgCustomLabel.CMTextChanged(VAR Message: TMessage);
BEGIN
   Invalidate;
END;
//______

PROCEDURE TJvgCustomLabel.CallMouseEnter;
VAR
   EmptyMsg                   : TMessage;
BEGIN
   CMMouseEnter(EmptyMsg);
END;
//______

PROCEDURE TJvgCustomLabel.CallMouseLeave;
VAR
   EmptyMsg                   : TMessage;
BEGIN
   CMMouseLeave(EmptyMsg);
END;
//______

PROCEDURE TJvgCustomLabel.HookFocusControlWndProc;
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

PROCEDURE TJvgCustomLabel.UnhookFocusControlWndProc;
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

PROCEDURE TJvgCustomLabel.FocusControlWndHookProc(VAR Msg_: TMessage);
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
      WM_DESTROY: fNeedRehookFocusControl := true;
   END;
   WITH Msg_ DO
      Result := CallWindowProc(FPrevWndProc, TForm(Owner).Handle, Msg, WParam,
         LParam);
END;
//______

PROCEDURE TJvgCustomLabel.SetFocusControl(Value: TWinControl);
BEGIN
   IF FFocusControl = Value THEN
      exit;
   IF ActiveWhileControlFocused AND Assigned(FFocusControl) THEN
      UnhookFocusControlWndProc;
   FFocusControl := Value;
   IF ActiveWhileControlFocused AND Assigned(FFocusControl) THEN
      HookFocusControlWndProc;
END;
//______

PROCEDURE TJvgCustomLabel.SetTransparent(Value: boolean);
BEGIN
   FTransparent := Value;
   Invalidate;
END;
//______

//________________________________________________________ TJvgLabel _

CONSTRUCTOR TJvgLabel.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);
   TextStyles := TJvgLabelTextStyles.Create;
   Colors := TJvgLabelColors.Create;
   Gradient := TJvgGradient.Create;
   FIllumination := TJvgIllumination.Create;
   Img := TBitmap.Create;

   bFirstCreate := true;
   FreeFont := TFont.Create;
   IF csDesigning IN ComponentState THEN
      Self.Font.Name := 'Arial';
   AutoSize := true;
   //  fRunOnce:=false;
   //  fActiveNow := false;

   FDirection := fldLeftRight;
   FFontWeight := fwDONTCARE;
   //  FSupressPaint := false;
   uFontWeight := word(fwDONTCARE);
   //  fNeedUpdateOnlyMainText:=false;
   FGradient.OnChanged := OnGradientChanged;
   FIllumination.OnChanged := OnIlluminationChanged;
   TextStyles.OnChanged := OnIlluminationChanged;
   Colors.OnChanged := OnIlluminationChanged;
   FOptions := [floActiveWhileControlFocused];
   TargetCanvas := Canvas;
   FTransparent := true;
   Width := 100;
   Height := 16;
END;
//______

DESTRUCTOR TJvgLabel.Destroy;
BEGIN
   TextStyles.Free;
   Colors.Free;
   Gradient.Free;
   FIllumination.Free;
   IF Assigned(FTexture) THEN
      FTexture.Free;
   IF Assigned(FBackground) THEN
      FBackground.Free;
   IF Assigned(TextureMask) THEN
      TextureMask.Free;
   Img.Free;
   INHERITED Destroy;
   DeleteObject(FreeFont.Handle);
   FreeFont.Free;
END;
//______

PROCEDURE TJvgLabel.Notification(AComponent: TComponent; Operation: TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
   IF (AComponent = BackgroundImage) AND (Operation = opRemove) THEN
      BackgroundImage := NIL
   ELSE IF (AComponent = TextureImage) AND (Operation = opRemove) THEN
      TextureImage := NIL;
END;
//______

PROCEDURE TJvgLabel.CMFontChanged(VAR Message: TMessage);
BEGIN
   INHERITED;
   CreateLabelFont;
   Invalidate;
END;
//______

PROCEDURE TJvgLabel.CMMouseEnter(VAR Message: TMessage);
BEGIN
   IF NOT Enabled OR (floIgnoreMouse IN Options) OR
      fShowAsActiveWhileControlFocused THEN
      exit;
   //inherited;
   fActiveNow := true;
   WITH TextStyles, Colors DO
      IF (Passive <> Active) OR ((Background <> BackgroundActive) AND NOT
         Transparent) THEN
      BEGIN
         IF floBufferedDraw IN Options THEN
            Paint
         ELSE
            InvalidateLabel(True);
      END
      ELSE IF (floDelineatedText IN Options) AND (DelineateActive <> Delineate)
         THEN
         Paint
      ELSE IF TextActive <> Text THEN
      BEGIN
         fNeedUpdateOnlyMainText := true;
         Paint;
      END;
   INHERITED;
END;
//______

PROCEDURE TJvgLabel.CMMouseLeave(VAR Message: TMessage);
BEGIN
   IF NOT Enabled OR (floIgnoreMouse IN Options) OR
      fShowAsActiveWhileControlFocused THEN
      exit;
   //inherited;
   fActiveNow := false;
   WITH TextStyles, Colors DO
      IF (Passive <> Active) OR ((Background <> BackgroundActive) AND NOT
         Transparent) THEN
      BEGIN
         IF floBufferedDraw IN Options THEN
            Paint
         ELSE
            InvalidateLabel(True);
      END
      ELSE IF (floDelineatedText IN Options) AND (DelineateActive <> Delineate)
         THEN
         Paint
      ELSE IF TextActive <> Text THEN
      BEGIN
         fNeedUpdateOnlyMainText := true;
         Paint;
      END;
   INHERITED;
END;
//______

PROCEDURE TJvgLabel.CallMouseEnter;
VAR
   EmptyMsg                   : TMessage;
BEGIN
   CMMouseEnter(EmptyMsg);
END;
//______

PROCEDURE TJvgLabel.CallMouseLeave;
VAR
   EmptyMsg                   : TMessage;
BEGIN
   CMMouseLeave(EmptyMsg);
END;
//______

PROCEDURE TJvgLabel.Loaded;
BEGIN
   INHERITED;
   IF FTexture <> NIL THEN
      TextureBmp := FTexture
   ELSE IF Assigned(FTextureImage) THEN
      TextureBmp := FTextureImage.Picture.Bitmap
   ELSE
      TextureBmp := NIL;
   IF FBackground <> NIL THEN
      BackgroundBmp := FBackground
   ELSE IF (FBackgroundImage <> NIL) THEN
      BackgroundBmp := FBackgroundImage.Picture.Bitmap
   ELSE
      BackgroundBmp := NIL;
END;
//______

PROCEDURE TJvgLabel.Paint;
VAR
   R                          : TRect;
   x, y, x_, y_, Tx, Ty       : integer;
   Size, TextSize             : TSIZE;
   FontColor                  : TColor;
   CurrTextStyle              : TglTextStyle;
   {ShadowColor_, HighlightColor_,CurrTextColor,} CurrDelinColor: TColor;
   OldGradientFActive, fUseBackgroundBmp, fUseTextureBmp, fBufferedDraw:
      boolean;
BEGIN
   INHERITED;
   IF FSupressPaint OR (length(Caption) = 0) THEN
      exit;
   IF floTransparentFont IN Options THEN
      fBufferedDraw := true
   ELSE
      fBufferedDraw := (floBufferedDraw IN Options) AND NOT (csDesigning IN
         ComponentState);
   IF fBufferedDraw THEN
      TargetCanvas := Img.Canvas
   ELSE IF Assigned(ExternalCanvas) THEN
      TargetCanvas := ExternalCanvas
   ELSE
      TargetCanvas := Canvas;
   fNeedUpdateOnlyMainText := fNeedUpdateOnlyMainText AND NOT (fBufferedDraw)
      AND (NOT IsItAFilledBitmap(BackgroundBmp));
   IF NOT fRunOnce THEN
   BEGIN
      fNeedUpdateOnlyMainText := false;
      fRunOnce := true;
   END;
   TargetCanvas.Font := FreeFont;
   //...CALC POSITION
   GetTextExtentPoint32(TargetCanvas.handle, PChar(Caption),
      length(Caption), Size);
   WITH TextStyles, Colors DO
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
   x := 0;
   y := 0;
   Size.cx := Size.cx + 2 + trunc(Size.cx * 0.01);
   //  Size.cy:=Size.cy+trunc(Size.cy*0.1);
   Size.cy := Size.cy + 2;
   TextSize := Size;
   IF (CurrTextStyle = fstShadow) OR (CurrTextStyle = fstVolumetric) THEN
   BEGIN
      inc(Size.cy, Illumination.ShadowDepth);
      inc(Size.cx, Illumination.ShadowDepth);
   END;
   IF floDelineatedText IN Options THEN
   BEGIN
      inc(Size.cy, 2);
      inc(Size.cx, 2);
   END;

   IF (Align = alNone) AND AutoSize THEN
      CASE FDirection OF
         fldLeftRight, fldRightLeft:
            BEGIN
               width := Size.cx;
               height := Size.cy;
            END;
      ELSE                              {fldDownUp,fldUpDown:}
         BEGIN
            width := Size.cy;
            height := Size.cx;
         END;
      END;

   //  pt := CalcAlignedTextPosition( TargetCanvas.handle, Caption, Size );
   //  x := pt.x; y := pt.y;
   //CalcAlignedTextPosition( TargetCanvas.handle, Caption, Size );

   CASE FDirection OF
      fldLeftRight:
         BEGIN //if Align = alNone then begin width:=max(w,Size.cx);height:=max(h,Size.cy); end;
            CASE Alignment OF
               taCenter: x := (Width - Size.cx) DIV 2;
               taRightJustify: x := Width - Size.cx;
            END;
         END;
      fldRightLeft:
         BEGIN //if Align = alNone then begin width:=max(w,Size.cx);height:=max(h,Size.cy);x:=width;y:=height; end;
            CASE Alignment OF
               taCenter: x := (Width + Size.cx) DIV 2;
               taLeftJustify: x := Width - (Size.cx - TextSize.cx) - 2;
            ELSE
               x := TextSize.cx;
            END;
            y := TextSize.cy;
         END;
      fldDownUp:
         BEGIN //if Align = alNone then begin height:=max(h,Size.cx);width:=max(w,Size.cy);y:=height-2; end;
            CASE Alignment OF
               taCenter: y := (Height + TextSize.cx - (Size.cy - TextSize.cy))
                  DIV 2;
               taRightJustify: y := TextSize.cx - 4;
            ELSE
               y := Height - (Size.cy - TextSize.cy) - 2;
            END;
         END;
      fldUpDown:
         BEGIN //if Align = alNone then begin height:=max(h,Size.cx);width:=max(w,Size.cy);x:=width; end;
            CASE Alignment OF
               taCenter: y := (Height - Size.cx) DIV 2;
               taRightJustify: y := Height - Size.cx;
            ELSE
               y := 1;
            END;
            x := TextSize.cy;
         END;
   END;

   //...CALC POSITION end

   R := GetClientRect;
   IF TargetCanvas = Img.Canvas THEN
   BEGIN
      Img.Width := Width;
      Img.Height := Height;
   END;

   SetBkMode(TargetCanvas.handle, 1 {TRANSPARENT});
   IF NOT Transparent THEN
   BEGIN
      TargetCanvas.Brush.Style := bsSolid;
      IF fActiveNow THEN
         TargetCanvas.Brush.Color := Colors.BackgroundActive
      ELSE
         TargetCanvas.Brush.Color := Colors.Background;
      TargetCanvas.FillRect(R);
   END;

   TRY
      fUseBackgroundBmp := IsItAFilledBitmap(BackgroundBmp);
   EXCEPT
      //  raise;
      fUseBackgroundBmp := false;
      BackgroundBmp := NIL;
      FBackgroundImage := NIL;
   END;

   TRY
      fUseTextureBmp := IsItAFilledBitmap(TextureBmp);
   EXCEPT
      fUseTextureBmp := false;
      TextureBmp := NIL;
      FTextureImage := NIL;
   END;

   //  ShadowColor_ := Colors.Shadow;
   //  HighlightColor_ := Colors.Highlight;
   IF fUseBackgroundBmp THEN
   BEGIN                                //...FillBackground
      Tx := 0;
      Ty := 0;
      WHILE Tx < Width DO
      BEGIN
         WHILE Ty < Height DO
         BEGIN
            BitBlt(TargetCanvas.Handle, Tx, Ty,
               BackgroundBmp.Width, BackgroundBmp.Height,
               BackgroundBmp.Canvas.Handle, 0, 0, SRCCOPY);
            Inc(Ty, BackgroundBmp.Height);
         END;
         Inc(Tx, BackgroundBmp.Width);
         Ty := 0;
      END;
   END
   ELSE IF fBufferedDraw THEN
      WITH TargetCanvas DO
      BEGIN
         IF Transparent OR (floTransparentFont IN Options) THEN
         TRY
            Brush.Color := Parent.Brush.Color;
            Brush.Style := bsSolid;
            FillRect(R);
            Brush.Style := bsClear;
            GetParentImageRect(self, Bounds(Left, Top, Width, Height),
               TargetCanvas.Handle);
         EXCEPT
         END;
      END;

   OldGradientFActive := Gradient.FActive;
   //...Supress Gradient if needed
   WITH Colors DO
      IF (fActiveNow AND (TextActive <> Text)) OR NOT Enabled THEN
         Gradient.FActive := false;
   IF floDelineatedText IN Options THEN
   BEGIN
      x_ := 4;
      y_ := 4;
   END
   ELSE
   BEGIN
      x_ := 2;
      y_ := 2;
   END;

   IF CurrTextStyle = fstNone THEN
   BEGIN
      x_ := x_ DIV 2 - 1;
      y_ := y_ DIV 2 - 1;
   END;
   IF CurrTextStyle = fstShadow THEN
   BEGIN
      x_ := x_ DIV 2 - 1;
      y_ := y_ DIV 2 - 1;
   END;
   IF {fNeedRemakeTextureMask and}  fUseTextureBmp OR (floTransparentFont IN
      Options) THEN
   BEGIN
      IF NOT Assigned(TextureMask) THEN
         TextureMask := TBitmap.Create;
      WITH TextureMask DO
      BEGIN
         Width := Self.Width;
         Height := Self.Height;
         Canvas.Brush.Color := clBlack;
         Canvas.Brush.Style := bsSolid;
         Canvas.FillRect(GetClientRect);
         Canvas.Font := FreeFont;
         Canvas.Font.Color := clWhite;
         IF (CurrTextStyle = fstNone) OR (CurrTextStyle = fstShadow) THEN
            Canvas.TextOut(x + x_, y + y_, Caption)
         ELSE
            Canvas.TextOut(x + x_ DIV 2, y + y_ DIV 2, Caption);
         Tx := 0;
         Ty := 0;

         IF NOT Self.Transparent THEN
         BEGIN
            BitBlt(Canvas.Handle, Tx, Ty, Width, Height, TargetCanvas.Handle, 0,
               0, SRCAND);
            IF fActiveNow THEN
               ChangeBitmapColor(TextureMask, clBlack, Colors.BackgroundActive)
            ELSE
               ChangeBitmapColor(TextureMask, clBlack, Colors.Background);
            BitBlt(Self.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0,
               SRCCOPY);
            exit;
         END;

         IF floTransparentFont IN Options THEN
            BitBlt(Canvas.Handle, Tx, Ty, Width, Height, TargetCanvas.Handle, 0,
               0, SRCAND)
         ELSE IF fUseTextureBmp THEN    //...fill mask with texture
            WHILE Tx < Width DO
            BEGIN
               WHILE Ty < Height DO
               BEGIN
                  BitBlt(Canvas.Handle, Tx, Ty, TextureBmp.Width,
                     TextureBmp.Height, TextureBmp.canvas.Handle, 0, 0, SRCAND);
                  Inc(Ty, TextureBmp.Height);
               END;
               Inc(Tx, TextureBmp.Width);
               Ty := 0;
            END;
      END;
   END;

   IF IsItAFilledBitmap(TextureBmp) THEN
      FontColor := 0;
   ExtTextOutExt(TargetCanvas.Handle, x, y, GetClientRect, Caption,
      CurrTextStyle, floDelineatedText IN Options,
      fNeedUpdateOnlyMainText, FontColor, CurrDelinColor,
      Colors.Highlight, Colors.Shadow,
      Illumination, Gradient, FreeFont);

   //  SetBkMode( TargetCanvas.handle, iOldBkMode );
   fNeedUpdateOnlyMainText := false;
   Gradient.FActive := OldGradientFActive;

   IF (Assigned(TextureBmp) OR (floTransparentFont IN Options)) AND
      (CurrTextStyle <> fstPushed) THEN
      BitBlt(TargetCanvas.Handle, 0, 0, TextureMask.Width, TextureMask.Height,
         TextureMask.canvas.Handle, 0, 0, SRCPAINT);

   IF Img.Canvas = TargetCanvas THEN
      BitBlt(Canvas.Handle, 0, 0, Img.Width, Img.Height,
         TargetCanvas.Handle, 0, 0, SRCCOPY);

   //R:=Rect(left,top,left+width,top+height);
   //ValidateRect( Parent.Handle, @r );

END;

//______

PROCEDURE TJvgLabel.CreateLabelFont;
BEGIN
   IF NOT (bFirstCreate) THEN
      DeleteObject(FreeFont.Handle);
   FreeFont.Handle := CreateRotatedFont(Font, RadianEscapments[FDirection]);
   bFirstCreate := false;
END;
//______

PROCEDURE TJvgLabel.InvalidateLabel(UpdateBackgr: boolean);
VAR
   r                          : TRect;
BEGIN
   R := Bounds(Left, Top, Width, Height);
   InvalidateRect(Parent.Handle, @r, UpdateBackgr);
END;
//______

PROCEDURE TJvgLabel.OnGradientChanged(Sender: TObject);
BEGIN
   fNeedUpdateOnlyMainText := true;
   Paint;
   //InvalidateLabel(false);
END;
//______

PROCEDURE TJvgLabel.OnIlluminationChanged(Sender: TObject);
BEGIN
   CalcShadowAndHighlightColors((Parent AS TWinControl).Brush.Color, Colors);
   InvalidateLabel(true);
END;
//____________________________________________________Properties

PROCEDURE TJvgLabel.SetDirection(Value: TglLabelDir);
BEGIN
   FDirection := Value;
   CreateLabelFont;
   fNeedRemakeTextureMask := true;
   InvalidateLabel(true);
END;
//______________________________________________________________

PROCEDURE TJvgLabel.SetFontWeight(Value: TFontWeight);
BEGIN
   IF FFontWeight = Value THEN
      exit;
   FFontWeight := Value;
   uFontWeight := word(Value) * 100;
   CreateLabelFont;
   fNeedRemakeTextureMask := true;
   InvalidateLabel(true);
END;
//______________________________________________________________

PROCEDURE TJvgLabel.SetOptions(Value: TglLabelOptions);
BEGIN
   IF FOptions = Value THEN
      exit;
   FOptions := Value;
   ActiveWhileControlFocused := floActiveWhileControlFocused IN Options;
   IF floTransparentFont IN Options THEN
      Options := Options + [floBufferedDraw];
   CalcShadowAndHighlightColors((Parent AS TWinControl).Brush.Color, Colors);
   fNeedRemakeTextureMask := true;
   InvalidateLabel(true);
END;

PROCEDURE TJvgLabel.SetTexture(Value: TBitmap);
BEGIN
   IF Assigned(FTexture) THEN
      FTexture.Free;
   FTexture := NIL;
   IF (Value <> NIL) AND (Value.Handle <> 0) THEN
   BEGIN
      FTexture := TBitmap.Create;
      FTexture.Assign(Value);
      TextureBmp := FTexture;
   END
   ELSE IF Assigned(FTextureImage) THEN
      TextureBmp := FTextureImage.Picture.Bitmap
   ELSE
      TextureBmp := NIL;
   fNeedRemakeTextureMask := true;
   InvalidateLabel(true);
END;

PROCEDURE TJvgLabel.SetBackground(Value: TBitmap);
BEGIN
   IF Assigned(FBackground) THEN
      FBackground.Free;
   FBackground := NIL;
   IF (Value <> NIL) AND (Value.Handle <> 0) THEN
   BEGIN
      FBackground := TBitmap.Create;
      FBackground.Assign(Value);
      BackgroundBmp := FBackground;
   END
   ELSE IF (FBackgroundImage <> NIL) THEN
      BackgroundBmp := FBackgroundImage.Picture.Bitmap
   ELSE
      BackgroundBmp := NIL;
   InvalidateLabel(true);
END;

FUNCTION TJvgLabel.GetTexture: TBitmap;
BEGIN
   IF NOT Assigned(FTexture) THEN
      FTexture := TBitmap.Create;
   Result := FTexture;
END;

FUNCTION TJvgLabel.GetBackground: TBitmap;
BEGIN
   IF NOT Assigned(FBackground) THEN
      FBackground := TBitmap.Create;
   Result := FBackground;
END;

PROCEDURE TJvgLabel.SetTextureImage(Value: TImage);
BEGIN
   FTextureImage := Value;
   IF (NOT IsItAFilledBitmap(FTexture)) AND Assigned(Value) THEN
   BEGIN
      TextureBmp := FTextureImage.Picture.Bitmap;
      InvalidateLabel(true);
   END
   ELSE
      TextureBmp := NIL;
END;

PROCEDURE TJvgLabel.SetBackgroundImage(Value: TImage);
BEGIN
   FBackgroundImage := Value;
   IF (NOT IsItAFilledBitmap(FBackground)) AND Assigned(Value) THEN
   BEGIN
      BackgroundBmp := FBackgroundImage.Picture.Bitmap;
      InvalidateLabel(true);
   END
   ELSE
      BackgroundBmp := NIL;
END;

PROCEDURE TJvgLabel.SetAlignment(Value: TAlignment);
BEGIN
   FAlignment := Value;
   Invalidate;
END;

//________________________________________________________ TJvgStaticTextLabel _

CONSTRUCTOR TJvgStaticTextLabel.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);
   FActiveColor := clWhite;
   FAlignment := ftaBroadwise;
   FOptions := [ftoActiveWhileControlFocused];
   FWordWrap := true;
   Width := 100;
   Height := 16;
END;
//______
//______

PROCEDURE TJvgStaticTextLabel.CMMouseEnter(VAR Message: TMessage);
BEGIN
   IF (ftoIgnoreMouse IN Options) OR fShowAsActiveWhileControlFocused THEN
      exit;
   fActiveNow := true;
   paint;
   INHERITED;
END;
//______

PROCEDURE TJvgStaticTextLabel.CMMouseLeave(VAR Message: TMessage);
BEGIN
   IF (ftoIgnoreMouse IN Options) OR fShowAsActiveWhileControlFocused THEN
      exit;
   fActiveNow := false;
   IF ftoUnderlinedActive IN Options THEN
      Invalidate
   ELSE
      paint;
   INHERITED;
END;
//______

PROCEDURE TJvgStaticTextLabel.CallMouseEnter;
VAR
   EmptyMsg                   : TMessage;
BEGIN
   CMMouseEnter(EmptyMsg);
END;
//______

PROCEDURE TJvgStaticTextLabel.CallMouseLeave;
VAR
   EmptyMsg                   : TMessage;
BEGIN
   CMMouseLeave(EmptyMsg);
END;
//______

PROCEDURE TJvgStaticTextLabel.Paint;
CONST
   Alignments                    : ARRAY[TglAlignment] OF Word = (DT_LEFT,
      DT_RIGHT, DT_CENTER, 0);
   WordWraps                  : ARRAY[Boolean] OF Word = (0, DT_WORDBREAK);
VAR
   Alignment_                 : TglAlignment;
   TargetCanvas               : TCanvas;
   Rect                       : TRect;
BEGIN
   //inherited;
   IF length(Caption) = 0 THEN
      exit;

   IF Assigned(ExternalCanvas) THEN
      TargetCanvas := ExternalCanvas
   ELSE
      TargetCanvas := Canvas;
   TargetCanvas.Font.Assign(Font);
   Alignment_ := FAlignment;
   SetBkMode(TargetCanvas.Handle, integer(FTransparent));

   {  if fActiveNow and(ftoUnderlinedActive in Options) then
       TargetCanvas.Font.Style := Font.Style + [fsUnderline]
     else
       TargetCanvas.Font.Style := Font.Style - [fsUnderline];
   }
   IF fActiveNow THEN
      SetTextColor(TargetCanvas.Handle, ColorToRGB(ActiveColor))
   ELSE
      SetTextColor(TargetCanvas.Handle, ColorToRGB(Font.Color));

   //  TextOut( TargetCanvas.Handle, 0, 0, 'lpszString', 10);
   //  BitBlt( TargetCanvas.Handle, 0, 0, Width, Height, Image.TargetCanvas.Handle, Width, Height, SRCCOPY );
   IF (Alignment = ftaBroadwise) THEN
   BEGIN
      IF FWordWrap THEN
      BEGIN
         DrawTextBroadwise(TargetCanvas);
         exit;
      END
      ELSE
         Alignment_ := ftaLeftJustify;
   END;
   Rect := ClientRect;
   DrawText(TargetCanvas.Handle, PChar(Caption), Length(Caption), Rect,
      DT_EXPANDTABS OR WordWraps[FWordWrap] OR Alignments[Alignment_]);

END;
//______

PROCEDURE TJvgStaticTextLabel.DrawTextBroadwise(Canvas: TCanvas);
VAR
   DrawPos, Pos1, Pos2, LineWidth,
      LineNo, LexemCount, TextHeight: integer;
   Lexem                      : STRING;
   Size                       : TSIZE;
   fStop, fBroadwiseLine      : boolean;

   FUNCTION GetNextLexem(VAR Pos1, Pos2: integer; fTrimleft: boolean): STRING;
   VAR
      Pos                     : integer;
   BEGIN
      pos := pos1;
      IF Caption[Pos] = ' ' THEN
         REPEAT inc(Pos);
         UNTIL (Pos > length(Caption)) OR (Caption[Pos] <> ' ');
      Pos2 := Pos;
      IF fTrimleft AND (LineNo > 0) THEN
         Pos1 := Pos;
      REPEAT inc(Pos2);
      UNTIL (Pos2 > length(Caption)) OR (Caption[Pos2] = ' ');

      Result := copy(Caption, Pos1, Pos2 - Pos1);
   END;

   PROCEDURE DrawLine(AdditSpace: cardinal);
   VAR
      i, DrawPos1, DrawPos2   : integer;
      Lexem                   : STRING;
      Size                    : TSIZE;
      X, X_                   : single;
   BEGIN
      DrawPos1 := DrawPos;
      DrawPos2 := DrawPos;
      X := 0;
      X_ := 0;
      LineWidth := 0;
      FOR i := 1 TO LexemCount DO
      BEGIN
         Lexem := GetNextLexem(DrawPos1, DrawPos2, i = 1);
         //      if LexemCount=1 then Lexem:=Lexem+' ';
         GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), length(Lexem), Size);
         inc(LineWidth, trunc(X));
         X := X + Size.cx;
         IF (trunc(X) > Width) AND (LexemCount > 1) THEN
            exit;

         IF (LexemCount > 1) AND fBroadwiseLine THEN
            X := X + AdditSpace / (LexemCount - 1);
         TextOut(Canvas.Handle, trunc(X_), LineNo * TextHeight, PChar(Lexem),
            length(Lexem));
         X_ := X;
         DrawPos1 := DrawPos2;
      END;
   END;
BEGIN
   IF Text = '' THEN
      exit;
   LineWidth := 0;
   LineNo := 0;
   DrawPos := 1;
   Pos1 := 1;
   Pos2 := 1;
   LexemCount := 0;
   TextHeight := 0;
   fStop := false;
   fBroadwiseLine := true;
   REPEAT
      Lexem := GetNextLexem(Pos1, Pos2, LexemCount = 0);
      //    if LexemCount=0 then Lexem:=Lexem+' ';
      GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), length(Lexem), Size);
      inc(LineWidth, Size.cx);
      inc(LexemCount);
      IF TextHeight < Size.cy THEN
         TextHeight := Size.cy;
      IF (LineWidth > Width) OR (Pos2 >= length(Caption)) THEN
      BEGIN
         IF (LineWidth > Width) THEN
         BEGIN
            IF LexemCount = 1 THEN
               Pos1 := Pos2;
            IF LexemCount > 1 THEN
               dec(LexemCount);
            DrawLine(Width - (LineWidth - Size.cx));
            DrawPos := Pos1;
            inc(LineNo);
            LexemCount := 0;
            LineWidth := 0;
            fStop := Pos1 > length(Caption);
         END
         ELSE
         BEGIN
            fBroadwiseLine := ftoBroadwiseLastLine IN Options;
            DrawLine(Width - LineWidth);
            inc(LineNo);
            fStop := true;
         END;
      END
      ELSE
         Pos1 := Pos2;
   UNTIL fStop;
   IF FAutoSize THEN
      Height := max(12, LineNo * TextHeight);
END;
//______

PROCEDURE TJvgStaticTextLabel.AdjustBounds;
CONST
   WordWraps                  : ARRAY[Boolean] OF Word = (0, DT_WORDBREAK);
VAR
   DC                         : HDC;
   X                          : Integer;
   Rect                       : TRect;
BEGIN
   IF NOT (csReading IN ComponentState) AND FAutoSize THEN
   BEGIN
      Rect := ClientRect;
      DC := GetDC(0);
      Canvas.Handle := DC;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect,
         DT_EXPANDTABS OR DT_CALCRECT OR WordWraps[FWordWrap]);
      Canvas.Handle := 0;
      ReleaseDC(0, DC);
      X := Left;
      IF FAlignment = ftaRightJustify THEN
         Inc(X, Width - Rect.Right);
      SetBounds(X, Top, Rect.Right, Rect.Bottom);
   END;
END;
//______

PROCEDURE TJvgStaticTextLabel.SetAlignment(Value: TglAlignment);
BEGIN
   FAlignment := Value;
   Invalidate;
END;
//______

PROCEDURE TJvgStaticTextLabel.SetOptions(Value: TglStTextOptions);
BEGIN
   FOptions := Value;
   ActiveWhileControlFocused := ftoActiveWhileControlFocused IN Options;
   Invalidate;
END;
//______

PROCEDURE TJvgStaticTextLabel.SetWordWrap(Value: boolean);
BEGIN
   FWordWrap := Value;
   Invalidate;
END;
//______

PROCEDURE TJvgStaticTextLabel.SetAutoSize(Value: boolean);
BEGIN
   INHERITED AutoSize := Value;
   AdjustBounds;
END;
//______

FUNCTION TJvgStaticTextLabel.GetAutoSize: boolean;
BEGIN
   Result := INHERITED AutoSize;
END;

//________________________________________________________ TJvgGlyphLabel _

CONSTRUCTOR TJvgGlyphLabel.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);
   ControlStyle := [csCaptureMouse, csOpaque, csClickEvents, csSetCaption,
      csReplicatable];
END;

DESTRUCTOR TJvgGlyphLabel.Destroy;
BEGIN
   IF Assigned(FGlyphOn) THEN
      FGlyphOn.Free;
   IF Assigned(FGlyphOff) THEN
      FGlyphOff.Free;
   IF Assigned(FGlyphDisabled) THEN
      FGlyphDisabled.Free;
   INHERITED;
END;

FUNCTION TJvgGlyphLabel.IsCustomGlyph: Boolean;
BEGIN
   Result := FGlyphKind = fgkCustom;
END;

PROCEDURE TJvgGlyphLabel.SetGlyphOn(Value: TBitmap);
BEGIN
   IF Assigned(FGlyphOn) THEN
      FGlyphOn.Free;
   FGlyphOn := TBitmap.Create;
   FGlyphKind := fgkCustom;
   FGlyphOn.Assign(Value);
   Invalidate;
END;

FUNCTION TJvgGlyphLabel.GetGlyphOn: TBitmap;
BEGIN
   IF NOT Assigned(FGlyphOn) THEN
      FGlyphOn := TBitmap.Create;
   Result := FGlyphOn;
END;

PROCEDURE TJvgGlyphLabel.SetGlyphOff(Value: TBitmap);
BEGIN
   IF Assigned(FGlyphOff) THEN
      FGlyphOff.Free;
   FGlyphOff := TBitmap.Create;
   FGlyphKind := fgkCustom;
   FGlyphOff.Assign(Value);
   Invalidate;
END;

FUNCTION TJvgGlyphLabel.GetGlyphOff: TBitmap;
BEGIN
   IF NOT Assigned(FGlyphOff) THEN
      FGlyphOff := TBitmap.Create;
   Result := FGlyphOff;
END;

PROCEDURE TJvgGlyphLabel.SetGlyphDisabled(Value: TBitmap);
BEGIN
   IF Assigned(FGlyphDisabled) THEN
      FGlyphDisabled.Free;
   FGlyphDisabled := TBitmap.Create;
   FGlyphDisabled.Assign(Value);
   Invalidate;
END;

FUNCTION TJvgGlyphLabel.GetGlyphDisabled: TBitmap;
BEGIN
   IF NOT Assigned(FGlyphDisabled) THEN
      FGlyphDisabled := TBitmap.Create;
   Result := FGlyphDisabled;
END;

PROCEDURE TJvgGlyphLabel.SetGlyphKind(Value: TglGlyphKind);
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
   BEGIN
      FGlyphOn.LoadFromResourceName(hInstance, 'ON');
      FGlyphOff.LoadFromResourceName(hInstance, 'OFF');
      FGlyphDisabled := TBitmap.Create;
      FGlyphDisabled.LoadFromResourceName(hInstance, 'DISABLED');
   END;

END;

END.

