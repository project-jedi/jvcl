{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgDigits.PAS, released on 2003-01-15.

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

UNIT JvgDigits;

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
   JVComponent,
   JvgUtils,
   JvgCommClasses;

TYPE
   TGraphDigitsElem = (_deT_, _deC_, _deB_, _deTL_, _deTR_, _deBL_, _deBR_,
      _deDOT_);
   TGraphDigitsElemSet = SET OF TGraphDigitsElem;
   TColorsPair = RECORD ActiveColor, PassiveColor: TColor;
   END;
   TSpSymbol = (_none_, _colon_, _slash_, _backslash_);

   TJvgDigits = CLASS(TJvGraphicControl)
   PRIVATE
      FValue: Double;
      FDSize: TJvgPointClass;
      FActiveColor: TColor;
      FPassiveColor: TColor;
      FBackgrColor: TColor;
      FInsertSpSymbolAt: integer;
      FPositions: word;
      FPenWidth: word;
      FGap: word;
      FInterspace: word;
      FTransparent: boolean;
      FAlignment: TAlignment;
      FInteriorOffset: word;
      FPenStyle: TPenStyle;
      FSpecialSymbol: TSpSymbol;
      FBevel: TJvgExtBevel;
      FGradient: TJvgGradient;
      FDigitCount: integer;

      OldStrWidth: integer;
      OldDotPos: integer;
      fNeedRepaint: boolean;
      ClientR: TRect;
      ColorsPair: TColorsPair;

      PROCEDURE WMSize(VAR Message: TWMSize); MESSAGE WM_SIZE;
      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
      PROCEDURE SetValue(NewValue: Double);
      PROCEDURE SetActiveColor(Value: TColor);
      PROCEDURE SetPassiveColor(Value: TColor);
      PROCEDURE SetBackgrColor(Value: TColor);
      PROCEDURE SetPositions(Value: Word);
      PROCEDURE SetPenWidth(Value: Word);
      PROCEDURE SetInterspace(Value: Word);
      PROCEDURE SetGap(Value: Word);
      PROCEDURE SetTransparent(Value: boolean);
      PROCEDURE SetAlignment(Value: TAlignment);
      PROCEDURE SetInteriorOffset(Value: word);
      PROCEDURE SetInsertSpSymbolAt(Value: integer);
      PROCEDURE SetPenStyle(Value: TPenStyle);
      PROCEDURE SetSpecialSymbol(Value: TSpSymbol);
      PROCEDURE SetDigitCount(Value: integer);

      PROCEDURE SmthChanged(Sender: TObject);
   PUBLIC
      PROCEDURE Paint; OVERRIDE;
      PROCEDURE PaintTo(Canvas: TCanvas);
      PROPERTY Canvas;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

   PUBLISHED
      PROPERTY OnClick;
      PROPERTY OnMouseDown;
      PROPERTY OnMouseMove;
      PROPERTY Value: Double READ FValue WRITE SetValue;
      PROPERTY DigitSize: TJvgPointClass READ FDSize WRITE FDSize;
      PROPERTY ActiveColor: TColor READ FActiveColor WRITE SetActiveColor
         DEFAULT clWhite;
      PROPERTY PassiveColor: TColor READ FPassiveColor WRITE SetPassiveColor
         DEFAULT $00202020;
      PROPERTY BackgrColor: TColor READ FBackgrColor WRITE SetBackgrColor
         DEFAULT clBlack;
      PROPERTY Positions: word READ FPositions WRITE SetPositions
         DEFAULT 0;
      PROPERTY PenWidth: word READ FPenWidth WRITE SetPenWidth
         DEFAULT 1;
      PROPERTY Gap: word READ FGap WRITE SetGap
         DEFAULT 1;
      PROPERTY Interspace: word READ FInterspace WRITE SetInterspace
         DEFAULT 3;
      PROPERTY Transparent: boolean READ FTransparent WRITE SetTransparent
         DEFAULT false;
      PROPERTY Alignment: TAlignment READ FAlignment WRITE SetAlignment
         DEFAULT taCenter;
      PROPERTY InteriorOffset: word READ FInteriorOffset WRITE SetInteriorOffset
         DEFAULT 0;
      PROPERTY InsertSpSymbolAt: integer READ FInsertSpSymbolAt WRITE
         SetInsertSpSymbolAt
         DEFAULT -1;
      PROPERTY PenStyle: TPenStyle READ FPenStyle WRITE SetPenStyle
         DEFAULT psSolid;
      PROPERTY SpecialSymbol: TSpSymbol
         READ FSpecialSymbol WRITE SetSpecialSymbol DEFAULT _none_;
      PROPERTY Bevel: TJvgExtBevel READ FBevel WRITE FBevel;
      PROPERTY Gradient: TJvgGradient READ FGradient WRITE FGradient;
      PROPERTY DigitCount: integer READ FDigitCount WRITE SetDigitCount
         DEFAULT -1;
   END;

CONST
   DigitsSet                  : ARRAY[0..11] OF TGraphDigitsElemSet
                              = ([_deT_, _deB_, _deTL_, _deTR_, _deBL_, _deBR_],  //...0
      [_deTR_, _deBR_],                 //...1
      [_deT_, _deC_, _deB_, _deTR_, _deBL_], //...2
      [_deT_, _deC_, _deB_, _deTR_, _deBR_], //...3
      [_deC_, _deTL_, _deTR_, _deBR_],  //...4
      [_deT_, _deC_, _deB_, _deTL_, _deBR_], //...5
      [_deT_, _deC_, _deB_, _deTL_, _deBL_, _deBR_], //...6
      [_deT_, _deTR_, _deBR_],          //...7
      [_deT_, _deC_, _deB_, _deTL_, _deTR_, _deBL_, _deBR_], //...8
      [_deT_, _deC_, _deB_, _deTL_, _deTR_, _deBR_], //...9
      [],                               //...' '
      [_deDOT_]                         //...','
      );

IMPLEMENTATION

//*****************************************_____________LowLevel METHODS

CONSTRUCTOR TJvgDigits.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FDSize := TJvgPointClass.Create;
   //...set defaults
   Width := 160;
   Height := 28;
   //  if csDesigning in ComponentState then FValue:=1.1234567890;
   FDSize.x := 10;
   FDSize.y := 21;
   FDSize.OnChanged := SmthChanged;
   FActiveColor := clWhite;
   FPassiveColor := $00202020;
   FBackgrColor := clBlack;
   FPositions := 0;
   FPenWidth := 1;
   FGap := 1;
   FInterspace := 3;
   FTransparent := false;
   FInteriorOffset := 0;
   FInsertSpSymbolAt := -1;
   FDigitCount := -1;
   FPenStyle := psSolid;
   fNeedRepaint := true;
   Color := FBackgrColor;
   FAlignment := taCenter;
   FSpecialSymbol := _none_;
   FBevel := TJvgExtBevel.Create;
   FBevel.OnChanged := SmthChanged;
   FGradient := TJvgGradient.Create;
   FGradient.OnChanged := SmthChanged;
END;

DESTRUCTOR TJvgDigits.Destroy;
BEGIN
   FDSize.Free;
   FBevel.Free;
   FGradient.Free;
   INHERITED;
END;

PROCEDURE TJvgDigits.Paint;
BEGIN
   TRY
      IF Canvas.handle <> 0 THEN
         PaintTo(Canvas);
   EXCEPT
   END;
END;

PROCEDURE TJvgDigits.PaintTo(Canvas: TCanvas);
VAR
   pt, OldPt                  : TPoint;
   xPos, yPos, D, CenterY, s, i, IWidth: integer;
   str, sChar                 : STRING;
   r                          : TRect;
   SPassive                   : boolean;

   PROCEDURE FillBackgr;                //**************************LOCAL PROC
   BEGIN
      IF FTransparent OR FGradient.Active THEN
         exit;
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := FBackgrColor;
      Canvas.FillRect(ClientR);
   END; //********************************************LOCAL PROC END

   FUNCTION DrawDigit(pt: TPoint; C: TColorsPair): integer; //****LOCAL PROC
   LABEL
      deC_L, deB_L, deTL_L, deTR_L, deBL_L, deBR_L, deEND_L;
   BEGIN
      WITH Canvas DO
      BEGIN
         IF FInsertSpSymbolAt = i THEN
         BEGIN

            CASE FSpecialSymbol OF
               _Colon_:
                  BEGIN
                     pt.x := pt.x + FInterspace;
                     Windows.SetPixel(Handle, pt.x, pt.y + FDsize.y DIV 3,
                        ColorToRGB(C.ActiveColor));
                     Windows.SetPixel(Handle, pt.x, pt.y + FDSize.y - FDsize.y
                        DIV 3, ColorToRGB(C.ActiveColor));
                     pt.x := pt.x + FInterspace * 2;
                  END;
               _slash_:
                  BEGIN
                     Windows.MoveToEx(Handle, pt.x + FDSize.x, pt.y + 1,
                        @OldPt);
                     Windows.LineTo(Handle, pt.x, pt.y + FDSize.y);
                     pt.x := pt.x + FDSize.x + FInterspace;
                  END;
               _backslash_:
                  BEGIN
                     Windows.MoveToEx(Handle, pt.x, pt.y + 1, @OldPt);
                     Windows.LineTo(Handle, pt.x + FDSize.x, pt.y + FDSize.y);
                     pt.x := pt.x + FDSize.x + FInterspace;
                  END;
            END;
         END;
         //OldColonpt.x:=pt.x;
         IF Pen.Width = 1 THEN
            s := 1
         ELSE
            s := 0;
         r := Rect(pt.x, pt.y, pt.x + FDSize.x, FDSize.y + pt.y);
         CenterY := r.top + (FDSize.y - Pen.Width) DIV 2;

         sChar := str[i];
         IF sChar = ' ' THEN
            d := 10
         ELSE IF (sChar = ',') OR (sChar = '.') THEN
            d := 11
         ELSE
            d := StrToInt(sChar);

         //...Draw Dot
         IF pt.x <= Width THEN
            IF _deDOT_ IN DigitsSet[d] THEN
            BEGIN
               OldDotPos := i;
               Windows.SetPixel(Handle, pt.x, r.bottom,
                  ColorToRGB(C.ActiveColor));
               pt.x := pt.x + FInterspace;
            END
            ELSE
            BEGIN                       ///...Draw Digit

               IF _deT_ IN DigitsSet[d] THEN
                  Pen.Color := C.ActiveColor
               ELSE IF SPassive THEN
                  Pen.Color := C.PassiveColor
               ELSE
                  GOTO deC_L;
               MoveTo(r.left + FGap, r.top);
               LineTo(r.right - FGap + s, r.top);
               deC_L:
               IF _deC_ IN DigitsSet[d] THEN
                  Pen.Color := C.ActiveColor
               ELSE IF SPassive THEN
                  Pen.Color := C.PassiveColor
               ELSE
                  GOTO deB_L;
               MoveTo(r.left + FGap, CenterY);
               LineTo(r.right - FGap + s, CenterY);
               deB_L:
               IF _deB_ IN DigitsSet[d] THEN
                  Pen.Color := C.ActiveColor
               ELSE IF SPassive THEN
                  Pen.Color := C.PassiveColor
               ELSE
                  GOTO deTL_L;
               MoveTo(r.left + FGap, r.bottom);
               LineTo(r.right - FGap + s, r.bottom);
               deTL_L:
               IF _deTL_ IN DigitsSet[d] THEN
                  Pen.Color := C.ActiveColor
               ELSE IF SPassive THEN
                  Pen.Color := C.PassiveColor
               ELSE
                  GOTO deTR_L;
               MoveTo(r.left, r.top + FGap);
               LineTo(r.left, CenterY - FGap + s);
               deTR_L:
               IF _deTR_ IN DigitsSet[d] THEN
                  Pen.Color := C.ActiveColor
               ELSE IF SPassive THEN
                  Pen.Color := C.PassiveColor
               ELSE
                  GOTO deBL_L;
               MoveTo(r.right, r.top + FGap);
               LineTo(r.right, CenterY - FGap + s);
               deBL_L:
               IF _deBL_ IN DigitsSet[d] THEN
                  Pen.Color := C.ActiveColor
               ELSE IF SPassive THEN
                  Pen.Color := C.PassiveColor
               ELSE
                  GOTO deBR_L;
               MoveTo(r.left, r.bottom - FGap);
               LineTo(r.left, CenterY + Pen.Width - s + FGap);
               deBR_L:
               IF _deBR_ IN DigitsSet[d] THEN
                  Pen.Color := C.ActiveColor
               ELSE IF SPassive THEN
                  Pen.Color := C.PassiveColor
               ELSE
                  GOTO deEND_L;
               MoveTo(r.right, r.bottom - FGap);
               LineTo(r.right, CenterY + Pen.Width - s + FGap);
               deEND_L:
               pt.x := pt.x + FDSize.x + FInterspace;
            END;
      END;
      Result := pt.x;
   END; //********************************************LOCAL PROC END

BEGIN //*********************************************MAIN PAINT PROC
   ClientR := GetClientRect;
   //--- gradient and Bevels
   IF FGradient.Active THEN
      WITH FBevel, FGradient DO
      BEGIN
         InflateRect(ClientR, -FInteriorOffset, -FInteriorOffset);
         GradientBox(Canvas.handle, ClientR, Gradient,
            integer(BevelPenStyle), BevelPenWidth);
      END;
   IF FBevel.Active THEN
      WITH FBevel DO
      BEGIN
         ClientR := ClientRect;
         dec(ClientR.right);
         dec(ClientR.bottom);
         Canvas.Pen.Width := BevelPenWidth;
         Canvas.Pen.Style := BevelPenStyle;
         ClientR := DrawBoxEx(Canvas.Handle, ClientR,
            Sides, Inner, Outer,
            Bold, 0, true);
         inc(ClientR.Right);
         inc(ClientR.Bottom);
      END;
   //---
 //  InflateRect(ClientR,-InteriorOffset,-InteriorOffset);
   r := ClientR;                        //dec( r.right ); dec( r.bottom );
   str := FloatToStr(FValue);
   IF (DigitCount <> -1) AND (DigitCount > Length(str)) THEN
      FOR i := 1 TO DigitCount - Length(str) DO
         str := str + '0';
   IF FPositions > 0 THEN
      str := Spaces(FPositions - Length(str)) + str;

   IWidth := 0;
   FOR i := 1 TO Length(str) DO
      IF str[i] <> ',' THEN
         inc(IWidth, FDSize.x + InterSpace)
      ELSE
         inc(IWidth, InterSpace);
   inc(IWidth, InterSpace);

   IF (FInsertSpSymbolAt > 0) AND (FInsertSpSymbolAt <= Length(str)) THEN
      IF FSpecialSymbol = _colon_ THEN
         inc(IWidth, InterSpace * 3)
      ELSE
         inc(IWidth, FDSize.x + InterSpace);
   //else inc( IWidth ,6 );
   CASE Alignment OF
      taLeftJustify: xPos := InterSpace;
      taCenter: xPos := (ClientR.right - ClientR.left - IWidth) DIV 2 +
         InterSpace;
   ELSE                                 {taRightJustify:}
      xPos := ClientR.right - ClientR.left - IWidth + InterSpace;
   END;
   yPos := (Height - FDSize.y) DIV 2;
   i := Pos(',', str);
   IF (i <> 0) AND (i <> OldDotPos) THEN
      fNeedRepaint := true;
   //if (FInsertSpSymbolAt>0)and(OldSpSymbolxPos<>xPos) then fNeedRepaint:=true;
   WITH Canvas DO
   BEGIN
      IF fNeedRepaint THEN
         FillBackgr;
      Pen.Color := FActiveColor;
      Pen.Style := PenStyle;
      Pen.Width := FPenWidth;

      pt.x := xPos;
      pt.y := yPos;
      ColorsPair.ActiveColor := FActiveColor;
      ColorsPair.PassiveColor := FPassiveColor;
      SPassive := NOT FGradient.Active;
      FOR i := 1 TO Length(str) DO
         pt.x := DrawDigit(pt, ColorsPair);
   END;
   fNeedRepaint := true;
END;

PROCEDURE TJvgDigits.WMSize(VAR Message: TWMSize);
BEGIN
   fNeedRepaint := true;
END;

PROCEDURE TJvgDigits.CMMouseEnter(VAR Message: TMessage);
BEGIN
   INHERITED;
END;

PROCEDURE TJvgDigits.CMMouseLeave(VAR Message: TMessage);
BEGIN
   INHERITED;
END;

PROCEDURE TJvgDigits.SmthChanged(Sender: TObject);
BEGIN
   Paint;
END;
//...______________________________________________PROPERTIES METHODS

PROCEDURE TJvgDigits.SetValue(NewValue: Double);
BEGIN
   TRY
      IF FValue = NewValue THEN
         exit;
      FValue := NewValue;
      IF OldStrWidth <> Length(FloatToStr(FValue)) THEN
      BEGIN
         OldStrWidth := Length(FloatToStr(FValue));
         fNeedRepaint := true;
         rePaint;
      END
      ELSE
      BEGIN
         fNeedRepaint := false;
         Paint;
      END;
   EXCEPT
   END;
END;

PROCEDURE TJvgDigits.SetActiveColor(Value: TColor);
BEGIN
   IF FActiveColor = Value THEN
      exit;
   FActiveColor := Value;
   fNeedRepaint := false;
   Paint;
END;

PROCEDURE TJvgDigits.SetPassiveColor(Value: TColor);
BEGIN
   IF FPassiveColor = Value THEN
      exit;
   FPassiveColor := Value;
   fNeedRepaint := false;
   Paint;
END;

PROCEDURE TJvgDigits.SetBackgrColor(Value: TColor);
BEGIN
   IF FBackgrColor = Value THEN
      exit;
   FBackgrColor := Value;
   fNeedRepaint := true;
   rePaint;
END;

PROCEDURE TJvgDigits.SetPositions(Value: Word);
BEGIN
   IF FPositions = Value THEN
      exit;
   FPositions := Value;
   fNeedRepaint := true;
   rePaint;
END;

PROCEDURE TJvgDigits.SetPenWidth(Value: Word);
BEGIN
   IF FPenWidth = Value THEN
      exit;
   FPenWidth := Value;
   fNeedRepaint := true;
   rePaint;
END;

PROCEDURE TJvgDigits.SetInterspace(Value: Word);
BEGIN
   IF FInterspace = Value THEN
      exit;
   FInterspace := Value;
   fNeedRepaint := true;
   rePaint;
END;

PROCEDURE TJvgDigits.SetGap(Value: Word);
BEGIN
   IF FGap = Value THEN
      exit;
   FGap := Value;
   fNeedRepaint := true;
   rePaint;
END;

PROCEDURE TJvgDigits.SetTransparent(Value: boolean);
BEGIN
   IF FTransparent = Value THEN
      exit;
   FTransparent := Value;
   fNeedRepaint := true;
   rePaint;
END;

PROCEDURE TJvgDigits.SetAlignment(Value: TAlignment);
BEGIN
   IF FAlignment = Value THEN
      exit;
   FAlignment := Value;
   fNeedRepaint := true;
   rePaint;
END;

PROCEDURE TJvgDigits.SetInteriorOffset(Value: word);
BEGIN
   IF FInteriorOffset = Value THEN
      exit;
   FInteriorOffset := Value;
   fNeedRepaint := true;
   rePaint;
END;

PROCEDURE TJvgDigits.SetInsertSpSymbolAt(Value: integer);
BEGIN
   IF FInsertSpSymbolAt = Value THEN
      exit;
   FInsertSpSymbolAt := Value;
   fNeedRepaint := true;
   Paint;
END;

PROCEDURE TJvgDigits.SetPenStyle(Value: TPenStyle);
BEGIN
   IF FPenStyle = Value THEN
      exit;
   FPenStyle := Value;
   fNeedRepaint := false;
   Paint;
END;

PROCEDURE TJvgDigits.SetSpecialSymbol(Value: TSpSymbol);
BEGIN
   IF FSpecialSymbol = Value THEN
      exit;
   FSpecialSymbol := Value;
   IF Value = _none_ THEN
      FInsertSpSymbolAt := -1;
   fNeedRepaint := true;
   rePaint;
END;

PROCEDURE TJvgDigits.SetDigitCount(Value: integer);
BEGIN
   IF FDigitCount = Value THEN
      exit;
   FDigitCount := Value;
   fNeedRepaint := true;
   rePaint;
END;

END.

