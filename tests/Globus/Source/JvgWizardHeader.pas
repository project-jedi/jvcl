{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgWizardHeader.PAS, released on 2003-01-15.

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

UNIT JvgWizardHeader;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   JVComponent,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   comctrls,
   JvgCommClasses;

TYPE
   TJvgWizardHeader = CLASS(TJvGraphicControl)
   PRIVATE
      FComments: TStrings;
      FCaptions: TStrings;
      FPageControl: TPageControl;
      FPageNo: integer;
      FCommentFont: TFont;
      FCaptionFont: TFont;
      FSymbolFont: TFont;
      FSymbol: STRING;
      FGradient: TJvgGradient;
      FGlyph: TBitmap;
      FBufferedDraw: boolean;
      PROCEDURE SetCaptions(CONST Value: TStrings);
      PROCEDURE SetComments(CONST Value: TStrings);
      PROCEDURE SetPageNo(CONST Value: integer);
      PROCEDURE SetCaptionFont(CONST Value: TFont);
      PROCEDURE SetCommentFont(CONST Value: TFont);
      PROCEDURE SetSymbolFont(CONST Value: TFont);
      PROCEDURE SetSymbol(CONST Value: STRING);
      PROCEDURE SetGradient(CONST Value: TJvgGradient);
      PROCEDURE SetGlyph(CONST Value: TBitmap);
      FUNCTION GetGlyph: TBitmap;
      { Private declarations }
   PROTECTED
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Paint; OVERRIDE;
   PUBLISHED
      PROPERTY Align DEFAULT alTop;
      PROPERTY Anchors;
      PROPERTY CaptionFont: TFont READ FCaptionFont WRITE SetCaptionFont;
      PROPERTY CommentFont: TFont READ FCommentFont WRITE SetCommentFont;
      PROPERTY SymbolFont: TFont READ FSymbolFont WRITE SetSymbolFont;
      PROPERTY PageNo: integer READ FPageNo WRITE SetPageNo;
      PROPERTY Captions: TStrings READ FCaptions WRITE SetCaptions;
      PROPERTY Comments: TStrings READ FComments WRITE SetComments;
      PROPERTY Symbol: STRING READ FSymbol WRITE SetSymbol;
      PROPERTY Gradient: TJvgGradient READ FGradient WRITE SetGradient;
      PROPERTY Glyph: TBitmap READ GetGlyph WRITE SetGlyph;
      PROPERTY BufferedDraw: boolean READ FBufferedDraw WRITE FBufferedDraw;
      //    property PageControl: TPageControl read FPageControl write SetPageControl;
   END;

PROCEDURE Register;

IMPLEMENTATION
USES JvgTypes,
   JvgUtils;

PROCEDURE Register;
BEGIN
   //  RegisterComponents('Gl Controls', [TJvgWizardHeader]);
END;

{ TJvgWizardHeader }

CONSTRUCTOR TJvgWizardHeader.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   ControlStyle := ControlStyle + [csOpaque];
   Align := alTop;
   Height := 60;
   FCaptions := TStringList.Create;
   FComments := TStringList.Create;
   FGradient := TJvgGradient.Create;
   FGradient.Active := true;
   FGradient.FromColor := clHighlight;
   FGradient.ToColor := clWindow;
   FGradient.Orientation := fgdVertical;
   FCaptionFont := Font;
   FCaptionFont.Style := [fsBold];
   FCommentFont := TFont.Create;
   FSymbolFont := TFont.Create;
   FSymbolFont.Name := 'Wingdings';
   FSymbolFont.Size := 26;
   FSymbolFont.Color := clHighlightText;
   FSymbolFont.Style := [fsBold];
   //  FSymbol := '4';
END;

DESTRUCTOR TJvgWizardHeader.Destroy;
BEGIN
   FCaptions.Free;
   FComments.Free;
   FCommentFont.Free;
   FGradient.Free;
   INHERITED;
END;

FUNCTION TJvgWizardHeader.GetGlyph: TBitmap;
BEGIN
   IF NOT Assigned(FGlyph) THEN
      FGlyph := TBitmap.Create;
   Result := FGlyph;
END;

PROCEDURE TJvgWizardHeader.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
   IF (AComponent = FPageControl) AND (Operation = opRemove) THEN
   BEGIN
      FPageControl := NIL;
   END;
END;

PROCEDURE TJvgWizardHeader.Paint;
VAR
   R, SR                      : TRect;
   Offset                     : integer;
   Buffer                     : TBitmap;
   Caption, Comment           : STRING;
   TargetCanvas               : TCanvas;
BEGIN
   FSymbol := copy(FSymbol, 1, 1);

   IF BufferedDraw THEN
   BEGIN
      Buffer := TBitmap.Create;
      Buffer.Width := Width;
      Buffer.Height := Height;
      TargetCanvas := Buffer.Canvas;
   END
   ELSE
   BEGIN
      Buffer := NIL;
      TargetCanvas := Canvas;
   END;

   TRY

      IF FCaptions.Count = 0 THEN
         Caption := 'Caption'
      ELSE
         Caption := FCaptions[Min(FCaptions.Count - 1, PageNo)];
      IF FComments.Count = 0 THEN
         Comment := 'Some comment text'
      ELSE
         Comment := FComments[Min(FComments.Count - 1, PageNo)];

      R := ClientRect;

      TargetCanvas.Brush.Color := clWindow;
      TargetCanvas.FillRect(R);

      Inc(R.Left, 20);
      Dec(R.Right, 60);
      Inc(R.Top, 8);
      Dec(R.Bottom, 5);

      TargetCanvas.Font.Assign(CaptionFont);
      DrawText(TargetCanvas.Handle, PChar(Caption), length(Caption), R,
         DT_SINGLELINE);

      inc(R.Top, TargetCanvas.TextHeight('Hy'));
      inc(R.Left, 20);

      TargetCanvas.Font.Assign(CommentFont);
      SR := R;
      DrawText(TargetCanvas.Handle, PChar(Comment), length(Comment), SR,
         DT_WORDBREAK OR DT_CALCRECT);

      OffsetRect(SR, 0, (R.Bottom - SR.Bottom) DIV 2);
      DrawText(TargetCanvas.Handle, PChar(Comment), length(Comment), SR,
         DT_WORDBREAK);

      IF Assigned(FGlyph) AND (FGlyph.Width > 0) THEN
      BEGIN
         R := ClientRect;
         Offset := (Height - FGlyph.Height) DIV 2;

         //    BitBlt(TargetCanvas.Handle, R.Right-FGlyph.Width-Offset, R.Top+Offset, FGlyph.Width, FGlyph.Height, FGlyph.TargetCanvas.Handle, 0, 0, SRCCOPY);
         DrawBitmapExt(TargetCanvas.Handle, FGlyph, R, R.Right - FGlyph.Width -
            Offset, R.Top + Offset,
            fwoNone, fdsDefault, true, GetTransparentColor(FGlyph,
               ftcLeftBottomPixel), 0);
      END
      ELSE IF length(FSymbol) > 0 THEN
      BEGIN
         TargetCanvas.Brush.Color := clHighlight;
         R := ClientRect;
         SR := Rect(R.Right - 50, R.Top + 5, R.Right - 5, R.Bottom - 5);
         IF Assigned(Gradient) AND Gradient.Active THEN
            dec(SR.Bottom, 3);
         TargetCanvas.FillRect(SR);

         TargetCanvas.Font.Assign(SymbolFont);
         SetBkMode(TargetCanvas.Handle, TRANSPARENT);
         DrawText(TargetCanvas.Handle, PChar(Symbol), length(FSymbol), SR,
            DT_SINGLELINE OR DT_CENTER OR DT_VCENTER);
      END;

      R := ClientRect;
      DrawEdge(TargetCanvas.Handle, R, EDGE_ETCHED, BF_BOTTOM);

      IF Gradient.Active THEN
         GradientBox(TargetCanvas.Handle, Rect(R.Left, R.Bottom - 5, R.Right,
            R.Bottom - 1), Gradient, 1, 1);

      IF BufferedDraw THEN
         BitBlt(Canvas.Handle, 0, 0, Width, Height, TargetCanvas.Handle, 0, 0,
            SRCCOPY);

   FINALLY
      IF BufferedDraw THEN
         Buffer.Free;
   END;
END;

PROCEDURE TJvgWizardHeader.SetCaptionFont(CONST Value: TFont);
BEGIN
   FCaptionFont.Assign(Value);
   Invalidate;
END;

PROCEDURE TJvgWizardHeader.SetCaptions(CONST Value: TStrings);
BEGIN
   FCaptions.Assign(Value);
   Invalidate;
END;

PROCEDURE TJvgWizardHeader.SetCommentFont(CONST Value: TFont);
BEGIN
   FCommentFont.Assign(Value);
   Invalidate;
END;

PROCEDURE TJvgWizardHeader.SetComments(CONST Value: TStrings);
BEGIN
   FComments.Assign(Value);
   Invalidate;
END;

PROCEDURE TJvgWizardHeader.SetGlyph(CONST Value: TBitmap);
BEGIN
   IF NOT Assigned(FGlyph) THEN
      FGlyph := TBitmap.Create;
   FGlyph.Assign(Value);
END;

PROCEDURE TJvgWizardHeader.SetGradient(CONST Value: TJvgGradient);
BEGIN
   FGradient.Assign(Value);
END;

PROCEDURE TJvgWizardHeader.SetPageNo(CONST Value: integer);
BEGIN
   FPageNo := Value;
   Paint;
END;

PROCEDURE TJvgWizardHeader.SetSymbol(CONST Value: STRING);
BEGIN
   FSymbol := Value;
   Invalidate;
END;

PROCEDURE TJvgWizardHeader.SetSymbolFont(CONST Value: TFont);
BEGIN
   FSymbolFont.Assign(Value);
   Invalidate;
END;

END.

