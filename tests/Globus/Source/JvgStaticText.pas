{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgStaticText.PAS, released on 2003-01-15.

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

UNIT JvgStaticText;

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

TYPE

   TJvgStaticText = CLASS(TJvGraphicControl)
   PRIVATE
      FActiveColor: TColor;
      FAlignment: TglAlignment;
      FAutoSize: boolean;
      FTransparent: boolean;
      FWordWrap: boolean;
      FOnMouseEnter: TNotifyEvent;
      FOnMouseleave: TNotifyEvent;

      fActive: boolean;
      Image: TBitmap;
      PROCEDURE CreateImage;
      PROCEDURE DrawTextBroadwise;
      PROCEDURE AdjustBounds;

      PROCEDURE SetAlignment(Value: TglAlignment);
      PROCEDURE SetAutoSize(Value: boolean);
      PROCEDURE SetTransparent(Value: boolean);
      PROCEDURE SetWordWrap(Value: boolean);

   PROTECTED
      PROCEDURE CMFontChanged(VAR Message: TMessage);
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
      PROCEDURE Paint; OVERRIDE;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
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
      PROPERTY ActiveColor: TColor READ FActiveColor WRITE FActiveColor
         DEFAULT clWhite;
      PROPERTY Alignment: TglAlignment READ FAlignment WRITE SetAlignment
         DEFAULT ftaBroadwise;
      PROPERTY AutoSize: boolean READ FAutoSize WRITE SetAutoSize
         DEFAULT true;
      PROPERTY Transparent: boolean READ FTransparent WRITE SetTransparent
         DEFAULT true;
      PROPERTY WordWrap: boolean READ FWordWrap WRITE SetWordWrap
         DEFAULT true;
      PROPERTY OnMouseEnter: TNotifyEvent READ FOnMouseEnter WRITE
         FOnMouseEnter;
      PROPERTY OnMouseLeave: TNotifyEvent READ FOnMouseLeave WRITE
         FOnMouseLeave;

   END;

PROCEDURE Register;

IMPLEMENTATION

{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

CONSTRUCTOR TJvgStaticText.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);
   Width := 100;
   Height := 100;
   //  Image := TBitmap.Create;
   FTransparent := true;
   FActiveColor := clWhite;
   FAutoSize := true;
   FAlignment := ftaBroadwise;
   FWordWrap := true;
   IF NOT (csLoading IN ComponentState) THEN
      CreateImage;
END;
//______

DESTRUCTOR TJvgStaticText.Destroy;
BEGIN
   //  Image.Free;
   INHERITED Destroy;
END;
//______

PROCEDURE TJvgStaticText.CMFontChanged(VAR Message: TMessage);
BEGIN
   INHERITED;
END;
//______

PROCEDURE TJvgStaticText.Loaded;
BEGIN
   INHERITED;
   CreateImage;
END;
//______

PROCEDURE TJvgStaticText.CMMouseEnter(VAR Message: TMessage);
BEGIN
   fActive := true;
   paint;
   IF Assigned(FOnMouseEnter) THEN
      FOnMouseEnter(self);
END;

PROCEDURE TJvgStaticText.CMMouseLeave(VAR Message: TMessage);
BEGIN
   fActive := false;
   paint;
   IF Assigned(FOnMouseLeave) THEN
      FOnMouseLeave(self);
END;
//______

PROCEDURE TJvgStaticText.Paint;
CONST
   Alignments                    : ARRAY[TglAlignment] OF Word = (DT_LEFT,
      DT_RIGHT, DT_CENTER, 0);
   WordWraps                  : ARRAY[Boolean] OF Word = (0, DT_WORDBREAK);
VAR
   Alignment_                 : TglAlignment;
   Rect                       : TRect;
BEGIN
   IF length(Caption) = 0 THEN
      exit;
   Alignment_ := FAlignment;
   SetBkMode(Canvas.Handle, integer(FTransparent));
   IF fActive THEN
      SetTextColor(Canvas.Handle, ColorToRGB(ActiveColor))
   ELSE
      SetTextColor(Canvas.Handle, ColorToRGB(Font.Color));
   //  TextOut( Canvas.Handle, 0, 0, 'lpszString', 10);
   //  BitBlt( Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, Width, Height, SRCCOPY );
   IF (Alignment = ftaBroadwise) THEN
   BEGIN
      IF FWordWrap THEN
      BEGIN
         DrawTextBroadwise;
         exit;
      END
      ELSE
         Alignment_ := ftaLeftJustify;
   END;
   Rect := ClientRect;
   DrawText(Canvas.Handle, PChar(Caption), Length(Caption), Rect,
      DT_EXPANDTABS OR WordWraps[FWordWrap] OR Alignments[Alignment_]);

END;
//______

PROCEDURE TJvgStaticText.CreateImage;
BEGIN
   //  DrawTextBroadwise;
END;
//______

PROCEDURE TJvgStaticText.DrawTextBroadwise;
VAR
   i, DrawPos, Pos1, Pos2, LineWidth,
      LineNo, LexemCount, LexemCount_, TextHeight: cardinal;
   Lexem                      : STRING;
   Size                       : TSIZE;

   FUNCTION GetNextLexem(VAR Pos1, Pos2: cardinal; fTrimleft: boolean): STRING;
   VAR
      Pos                     : cardinal;
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
      i, DrawPos1, DrawPos2   : cardinal;
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
         IF (X > Width) AND (LexemCount > 1) THEN
            exit;
         IF LexemCount > 1 THEN
            X := X + AdditSpace / (LexemCount - 1);
         TextOut(Canvas.Handle, trunc(X_), LineNo * TextHeight, PChar(Lexem),
            length(Lexem));
         X_ := X;
         DrawPos1 := DrawPos2;
      END;
   END;
BEGIN
   LineWidth := 0;
   LineNo := 0;
   DrawPos := 1;
   Pos1 := 1;
   Pos2 := 1;
   LexemCount := 0;
   TextHeight := 0;
   REPEAT
      Lexem := GetNextLexem(Pos1, Pos2, LexemCount = 0);
      //    if LexemCount=0 then Lexem:=Lexem+' ';
      GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), length(Lexem), Size);
      inc(LineWidth, Size.cx);
      inc(LexemCount);
      IF TextHeight < Size.cy THEN
         TextHeight := Size.cy;
      IF (LineWidth > Width) OR (Pos2 > length(Caption)) THEN
      BEGIN
         IF LexemCount = 1 THEN
            Pos1 := Pos2;
         IF (Pos2 <= length(Caption)) THEN
         BEGIN
            IF LexemCount > 1 THEN
               dec(LexemCount);
            DrawLine(Width - (LineWidth - Size.cx));
         END
         ELSE
            DrawLine(Width - (LineWidth));

         DrawPos := Pos1;
         inc(LineNo);
         LexemCount := 0;
         LineWidth := 0;                //TextHeight := 0;
      END
      ELSE
         Pos1 := Pos2;
   UNTIL Pos2 > length(Caption);
   IF FAutoSize THEN
      Height := max(12, LineNo * TextHeight);
END;

PROCEDURE TJvgStaticText.AdjustBounds;
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

PROCEDURE TJvgStaticText.SetAlignment(Value: TglAlignment);
BEGIN
   FAlignment := Value;
   Invalidate;
END;

PROCEDURE TJvgStaticText.SetAutoSize(Value: boolean);
BEGIN
   FAutoSize := Value;
   AdjustBounds;
   Repaint;
END;

PROCEDURE TJvgStaticText.SetTransparent(Value: boolean);
BEGIN
   FTransparent := Value;
   Repaint;
END;

PROCEDURE TJvgStaticText.SetWordWrap(Value: boolean);
BEGIN
   FWordWrap := Value;
   Invalidate;
END;

END.

