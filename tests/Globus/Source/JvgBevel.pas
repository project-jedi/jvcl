{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgBevel.PAS, released on 2003-01-15.

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

{ This unit implements the TJvgBevel component which is an  extended
 TBevel Delphi component with gradient filling and advanced  borders
 drawing.}

UNIT JvgBevel;

INTERFACE

USES
   Windows,
   Messages,
   Classes,
   Controls,
   JVComponent,
   Graphics,
   JvgTypes,
   JvgCommClasses,
   JvgUtils,
   ExtCtrls;
TYPE

   TJvgBevel = CLASS(TJvGraphicControl)
   PRIVATE
      FBevelInner: TPanelBevel;
      FBevelOuter: TPanelBevel;
      FBevelSides: TglSides;
      FBevelBold: boolean;
      FBevelPenStyle: TPenStyle;
      FBevelPenWidth: word;
      FInteriorOffset: word;
      FGradient: TJvgGradient;
      FVertLines: TJvgBevelLines;
      FHorLines: TJvgBevelLines;
      //    FMouseSentencive	  : boolean;
      FExternalCanvas: TCanvas;
      PROCEDURE OnSmthChanged(Sender: TObject);

      PROCEDURE SetBevelInner(Value: TPanelBevel);
      PROCEDURE SetBevelOuter(Value: TPanelBevel);
      PROCEDURE SetBevelSides(Value: TglSides);
      PROCEDURE SetBevelBold(Value: boolean);
      PROCEDURE SetBevelPenStyle(Value: TPenStyle);
      PROCEDURE SetBevelPenWidth(Value: word);
      PROCEDURE SetInteriorOffset(Value: word);

      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
   PROTECTED
   PUBLIC
      Ctrl3D: boolean;
      PROCEDURE Paint; OVERRIDE;
      PROPERTY ExternalCanvas: TCanvas READ FExternalCanvas WRITE
         FExternalCanvas;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Loaded; OVERRIDE;

   PUBLISHED
      {$IFDEF COMPILER5_UP}
      PROPERTY Anchors;
      {$ENDIF}
      PROPERTY Align;
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

      PROPERTY Canvas;
      PROPERTY BevelInner: TPanelBevel READ FBevelInner WRITE SetBevelInner
         DEFAULT bvLowered;
      PROPERTY BevelOuter: TPanelBevel READ FBevelOuter WRITE SetBevelOuter
         DEFAULT bvNone;
      PROPERTY BevelSides: TglSides READ FBevelSides WRITE SetBevelSides
         DEFAULT [fsdLeft, fsdTop, fsdRight, fsdBottom];
      PROPERTY BevelBold: boolean READ FBevelBold WRITE SetBevelBold
         DEFAULT false;
      PROPERTY BevelPenStyle: TPenStyle READ FBevelPenStyle WRITE
         SetBevelPenStyle
         DEFAULT psSolid;
      PROPERTY BevelPenWidth: word READ FBevelPenWidth WRITE SetBevelPenWidth
         DEFAULT 1;
      PROPERTY InteriorOffset: word READ FInteriorOffset WRITE SetInteriorOffset
         DEFAULT 0;
      PROPERTY Gradient: TJvgGradient READ FGradient WRITE FGradient;
      PROPERTY VertLines: TJvgBevelLines READ FVertLines WRITE FVertLines;
      PROPERTY HorLines: TJvgBevelLines READ FHorLines WRITE FHorLines;
   END;

PROCEDURE Register;

IMPLEMENTATION
{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN

END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

CONSTRUCTOR TJvgBevel.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FGradient := TJvgGradient.Create;
   FVertLines := TJvgBevelLines.Create;
   FHorLines := TJvgBevelLines.Create;
   //..defaults
   Width := 50;
   Height := 50;
   FBevelInner := bvLowered;
   //  FBevelOuter := bvNone;
   FBevelSides := [fsdLeft, fsdTop, fsdRight, fsdBottom];
   FBevelPenStyle := psSolid;
   FBevelPenWidth := 1;
   Ctrl3D := true;
   FGradient.OnChanged := OnSmthChanged;
   FVertLines.OnChanged := OnSmthChanged;
   FHorLines.OnChanged := OnSmthChanged;
END;

DESTRUCTOR TJvgBevel.Destroy;
BEGIN
   Gradient.Free;
   FVertLines.Free;
   FHorLines.Free;
   INHERITED;
END;

PROCEDURE TJvgBevel.Paint;
VAR
   r, r_                      : TRect;
   BoxSides                   : TglSides;
   TargetCanvas               : TCanvas;

   PROCEDURE DrawLines(r_: TRect; Direction: TglLinesDir; Lines:
      TJvgBevelLines);
   VAR
      i                       : integer;
   BEGIN
      IF Direction = fldVertical THEN
      BEGIN
         IF Ctrl3D THEN
            BoxSides := [fsdLeft, fsdRight]
         ELSE
            BoxSides := [fsdLeft];
         IF Lines.IgnoreBorder THEN
         BEGIN
            r_.top := r.top;
            r_.bottom := r.bottom;
         END;
      END
      ELSE
      BEGIN
         IF Ctrl3D THEN
            BoxSides := [fsdTop, fsdBottom]
         ELSE
            BoxSides := [fsdTop];
         IF Lines.IgnoreBorder THEN
         BEGIN
            r_.left := r.left;
            r_.right := r.right;
         END;
      END;

      FOR i := 1 TO Lines.Count DO
      BEGIN
         CASE Direction OF
            fldVertical:
               BEGIN
                  r_.left := MulDiv(i, Width, Lines.Count + 1);
                  r_.Right := r_.Left + Lines.Thickness + integer(Lines.Bold);
               END;
         ELSE                           {fldHorizontal:}
            BEGIN
               r_.Top := MulDiv(i, Height, Lines.Count + 1);
               //	  if i = 1 then dec( r_.Top, Lines.Thickness );
               r_.Bottom := r_.Top + Lines.Thickness + integer(Lines.Bold);
            END;
         END;
         {$IFDEF COMPILER5_UP}
         IF Lines.Style = bvSpace THEN
            BoxSides := [fsdLeft, fsdTop];
         {$ENDIF}

         DrawBoxEx(TargetCanvas.Handle, r_, BoxSides, Lines.Style, bvNone,
            Lines.Bold, 0, true);
      END;
   END;
BEGIN
   IF Assigned(ExternalCanvas) THEN
      TargetCanvas := ExternalCanvas
   ELSE
      TargetCanvas := Canvas;
   r := ClientRect;
   InflateRect(r, -FInteriorOffset, -FInteriorOffset);
   GradientBox(TargetCanvas.handle, r, Gradient,
      integer(FBevelPenStyle), FBevelPenWidth);

   r := ClientRect;
   dec(r.right);
   dec(r.bottom);
   TargetCanvas.Pen.Width := FBevelPenWidth;
   TargetCanvas.Pen.Style := FBevelPenStyle;
   r_ := DrawBoxEx(TargetCanvas.Handle, r, BevelSides, BevelInner, BevelOuter,
      FBevelBold, 0, true);

   DrawLines(r_, fldHorizontal, HorLines);
   DrawLines(r_, fldVertical, VertLines);
END;

PROCEDURE TJvgBevel.CMMouseEnter(VAR Message: TMessage);
BEGIN
   INHERITED;
END;

PROCEDURE TJvgBevel.CMMouseLeave(VAR Message: TMessage);
BEGIN
   INHERITED;
END;

PROCEDURE TJvgBevel.OnSmthChanged(Sender: TObject);
BEGIN
   Repaint;
END;
//...______________________________________________PROPERTIES METHODS

PROCEDURE TJvgBevel.SetBevelOuter(Value: TPanelBevel);
//var r: TRect;
BEGIN
   IF FBevelOuter = Value THEN
      exit;
   //  r:=ClientRect; InflateRect( r, -5, -5 );
   FBevelOuter := Value;
   Invalidate;                          // ValidateRect( canvas.handle, @r );
END;

PROCEDURE TJvgBevel.SetBevelInner(Value: TPanelBevel);
//var r: TRect;
BEGIN
   IF FBevelInner = Value THEN
      exit;
   //  r:=ClientRect; InflateRect( r, -5, -5 );
   FBevelInner := Value;
   Invalidate;                          //ValidateRect( canvas.handle, @r );
END;

PROCEDURE TJvgBevel.SetBevelSides(Value: TglSides);
BEGIN
   IF FBevelSides = Value THEN
      exit;
   FBevelSides := Value;
   Invalidate;
END;

PROCEDURE TJvgBevel.SetBevelBold(Value: boolean);
BEGIN
   IF FBevelBold = Value THEN
      exit;
   FBevelBold := Value;
   Invalidate;
END;

PROCEDURE TJvgBevel.SetBevelPenStyle(Value: TPenStyle);
BEGIN
   IF FBevelPenStyle = Value THEN
      exit;
   FBevelPenStyle := Value;
   Invalidate;
END;

PROCEDURE TJvgBevel.SetBevelPenWidth(Value: word);
BEGIN
   IF FBevelPenWidth = Value THEN
      exit;
   FBevelPenWidth := Value;
   Invalidate;
END;

PROCEDURE TJvgBevel.SetInteriorOffset(Value: word);
BEGIN
   IF FInteriorOffset = Value THEN
      exit;
   FInteriorOffset := Value;
   Invalidate;
END;

PROCEDURE TJvgBevel.Loaded;
BEGIN
   INHERITED;
   IF FGradient.Active THEN
      ControlStyle := ControlStyle + [csOpaque];
END;

END.

