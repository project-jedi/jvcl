{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgGraphicButton.PAS, released on 2003-01-15.

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

UNIT JvgGraphicButton;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   JVComponent,
   Graphics,
   controls,
   JvgTypes,
   JvgUtils;

TYPE

   TGButtonState = (bsActive, bsPassive, bsPushed);

   TJvgGraphicButton = CLASS(TJvGraphicControl)
   PRIVATE
      FAutoSize: boolean;
      FGlyphActive: TBitmap;
      FGlyphPassive: TBitmap;
      FGlyphPushed: TBitmap;
      State: TGButtonState;
      FOnMouseEnter: TNotifyEvent;
      FOnMouseLeave: TNotifyEvent;
      PROCEDURE SetGlyphActive(Value: TBitmap);
      PROCEDURE SetGlyphPassive(Value: TBitmap);
      PROCEDURE SetGlyphPushed(Value: TBitmap);
   PROTECTED
      PROCEDURE MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
         Integer); OVERRIDE;
      PROCEDURE MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
         OVERRIDE;
      //    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;

   PUBLIC
      PROCEDURE Paint; OVERRIDE;
      PROPERTY Canvas;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY Enabled;
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

      PROPERTY GlyphActive: TBitmap READ FGlyphActive WRITE SetGlyphActive;
      PROPERTY GlyphPassive: TBitmap READ FGlyphPassive WRITE SetGlyphPassive;
      PROPERTY GlyphPushed: TBitmap READ FGlyphPushed WRITE SetGlyphPushed;

      PROPERTY OnMouseEnter: TNotifyEvent READ FOnMouseEnter WRITE
         FOnMouseEnter;
      PROPERTY OnMouseLeave: TNotifyEvent READ FOnMouseLeave WRITE
         FOnMouseLeave;
   END;

IMPLEMENTATION

//*****************************************_____________LowLevel METHODS
//________________________________________________________

CONSTRUCTOR TJvgGraphicButton.Create(AOwner: TComponent);
BEGIN

   INHERITED Create(AOwner);
   //  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
     {inherited } Width := 105;
   {inherited } Height := 105;

   FGlyphActive := TBitmap.create;
   FGlyphPassive := TBitmap.create;
   FGlyphPushed := TBitmap.create;
   //...defaults
   FAutoSize := false;
   State := bsPassive;
END;
//________________________________________________________

DESTRUCTOR TJvgGraphicButton.Destroy;
BEGIN
   FGlyphActive.Free;
   FGlyphPassive.Free;
   FGlyphPushed.Free;
   INHERITED;
END;
//________________________________________________________

PROCEDURE TJvgGraphicButton.Paint;
VAR
   Glyph                      : TBitmap;
BEGIN
   CASE State OF
      bsActive: IF Assigned(FGlyphActive) THEN
            Glyph := FGlyphActive
         ELSE
            Glyph := FGlyphPassive;
      bsPassive: Glyph := FGlyphPassive;
   ELSE                                 {bsPushed}
      BEGIN
         IF Assigned(FGlyphPushed) THEN
            Glyph := FGlyphPushed
         ELSE
            Glyph := FGlyphActive;
         IF NOT Assigned(Glyph) THEN
            Glyph := FGlyphPassive;
      END;
   END;
   IF Assigned(Glyph) THEN
      BitBlt(Canvas.Handle, 0, 0, Glyph.Width, Glyph.Height,
         Glyph.Canvas.Handle, 0, 0, SRCCOPY);
   IF (csDesigning IN ComponentState) AND (tag <> 9999) THEN
      WITH Canvas DO
      BEGIN
         Pen.Color := clBlack;
         Pen.Style := psDash;
         Brush.Style := bsClear;
         Rectangle(0, 0, width, height);
      END;

END;
//________________________________________________________

PROCEDURE TJvgGraphicButton.SetGlyphActive(Value: TBitmap);
BEGIN
   FGlyphActive.Assign(Value);
   Repaint;
END;

PROCEDURE TJvgGraphicButton.SetGlyphPassive(Value: TBitmap);
BEGIN
   FGlyphPassive.Assign(Value);
   Repaint;
END;

PROCEDURE TJvgGraphicButton.SetGlyphPushed(Value: TBitmap);
BEGIN
   FGlyphPushed.Assign(Value);
   Repaint;
END;

//________________________________________________________

PROCEDURE TJvgGraphicButton.CMMouseEnter(VAR Message: TMessage);
BEGIN
   INHERITED;
   State := bsActive;
   Paint;
   IF Assigned(FOnMouseEnter) THEN
      FOnMouseEnter(self);
END;

PROCEDURE TJvgGraphicButton.CMMouseLeave(VAR Message: TMessage);
BEGIN
   INHERITED;
   State := bsPassive;
   Paint;
   IF Assigned(FOnMouseLeave) THEN
      FOnMouseLeave(self);
END;

PROCEDURE TJvgGraphicButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
   X, Y: Integer);
BEGIN
   INHERITED MouseDown(Button, Shift, X, Y);
   IF (Button <> mbLeft) OR (NOT Enabled) OR (State = bsPassive) THEN
      exit;
   State := bsPushed;
   Paint;
END;

PROCEDURE TJvgGraphicButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
   Y: Integer);
BEGIN
   INHERITED MouseUp(Button, Shift, X, Y);
   IF (State = bsPushed) AND Assigned(OnClick) THEN
      OnClick(self);
   IF State = bsPushed THEN
      State := bsActive
   ELSE
      State := bsPassive;
   Paint;
END;

END.

