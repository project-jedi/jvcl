{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgUtils.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgUtils;

interface

uses
  Windows, Messages, Graphics, ExtCtrls, SysUtils, Classes, Controls, Forms,
  JvgTypes, JvgCommClasses, Jvg3DColors, MMSystem;

type
  TJvgPublicWinControl = class(TWinControl)
  public
    procedure PaintWindow(DC: HDC); override;
    procedure RecreateWnd;
    property Font;
    property OnEnter;
    property OnExit;
    property Color;
  end;

function IsEven(i: integer): boolean;
function InchesToPixels(DC: HDC; Value: Single; IsHorizontal: Boolean): Integer;
function CentimetersToPixels(DC: HDC; Value: Single; IsHorizontal: Boolean): Integer;

procedure SwapInt(var i1, i2: integer);
function Spaces(count: integer): string;
function DupStr(const str: string; Count: integer): string;
function DupChar(C: Char; Count: integer): string;
procedure Msg(msg: string);
function RectW(R: TRect): integer;
function RectH(R: TRect): integer;
function IncColor(lColor: LongInt; bOffset: byte): LongInt;
function DecColor(lColor: LongInt; bOffset: byte): LongInt;
function IsItAFilledBitmap(Bmp: TBitmap): boolean;
procedure DrawTextInRectWithAlign(DC: HDC; r: TRect; Text: string;
  HAlign: TglHorAlign; VAlign: TglVertAlign;
  Style: TglTextStyle; Fnt: TFont;
  FLAGS: UINT);

procedure DrawTextInRect(DC: HDC; r: TRect; Text: string;
  Style: TglTextStyle; Fnt: TFont; FLAGS: UINT);

procedure ExtTextOutExt(DC: HDC;
  x, y: integer;
  r: TRect;
  Text: string;
  Style: TglTextStyle;
  fDelineated, fSupress3D: boolean;
  FontColor, DelinColor, HighlightColor, ShadowColor: TColor;
  Illumination: TJvgIllumination;
  Gradient: TJvgGradient;
  Font: TFont);

procedure DrawBox(DC: HDC; var r: TRect; Style: TglBoxStyle;
  BackgrColor: LongInt; fTransparent: boolean);

function DrawBoxEx(DC: HDC; rect: TRect; Borders: TglSides;
  BevelInner, BevelOuter: TPanelBevel;
  Bold: boolean; BackgrColor: LongInt;
  fTransparent: boolean): TRect;

procedure GradientBox(DC: HDC; r: TRect; Gradient: TJvgGradient;
  PenStyle, PenWidth: integer);

procedure ChangeBitmapColor(Bitmap: TBitmap;
  FromColor, ToColor: TColor);

procedure DrawBitmapExt(DC: HDC; { DC - background & result}
  SourceBitmap: TBitmap;
  r: TRect;
  x, y: integer; //...x,y _in_ rect!
  BitmapOption: TglWallpaperOption;
  DrawState: TglDrawState;
  fTransparent: boolean;
  TransparentColor: TColor;
  DisabledMaskColor: TColor);

procedure CreateBitmapExt(DC: HDC; { DC - background & result}
  SourceBitmap: TBitmap;
  r: TRect;
  x, y: integer; //...x,y _in_ rect!
  BitmapOption: TglWallpaperOption;
  DrawState: TglDrawState;
  fTransparent: boolean;
  TransparentColor: TColor;
  DisabledMaskColor: TColor);

procedure BringParentWindowToTop(Wnd: TWinControl);
function GetParentForm(Control: TControl): TForm;
procedure GetWindowImageFrom(Control: TWinControl; X, Y: integer; fDrawSelf, fDrawChildWindows: boolean; DC: HDC);
procedure GetWindowImage(Control: TWinControl; fDrawSelf, fDrawChildWindows: boolean; DC: HDC);
procedure GetParentImageRect(Control: TControl; Rect: TRect; DC: HDC);
function CreateRotatedFont(F: TFont; Escapement: Integer): hFont;
function FindMainWindow(sWndClass, sWndTitle: string): HWND;
procedure CalcShadowAndHighlightColors(BaseColor: TColor; Colors: TJvgLabelColors);

function CalcMathString(sExpression: string): single;

function IIF(fExpression: boolean; IfTrue, IfFalse: variant): variant; overload;
function IIF(fExpression: boolean; const IfTrue, IfFalse: string): string; overload;

function GetTransparentColor(Bitmap: TBitmap; AutoTrColor: TglAutoTransparentColor): TColor;
procedure TypeStringOnKeyboard(S: string);
//function NextStringGridCell( Grid: TStringGrid ): boolean;
procedure DrawTextExtAligned(Canvas: TCanvas; const Text: string; R: TRect; Alignment: TglAlignment; WordWrap: boolean);
procedure LoadComponentFromTextFile(Component: TComponent; FileName: string);
procedure SaveComponentToTextFile(Component: TComponent; FileName: string);
function ComponentToString(Component: TComponent): string;
procedure StringToComponent(Component: TComponent; const Value: string);
function PlayWaveResource(sResName: string): boolean;
function UserName: string;
function ComputerName: string;
function CreateIniFileName: string;
function ExpandString(const str: string; len: integer): string;
function Transliterate(const Str: string; RusToLat: boolean): string;
function IsSmallFonts: boolean;
function SystemColorDepth: integer;
function GetFileType(const FileName: string): TglFileType;
function FindControlAtPt(Control: TWinControl; pt: TPoint; MinClass: TClass): TControl;
function StrPosExt(const Str1, Str2: PChar; Str2Len: DWORD): PChar; assembler;

{$IFDEF glDEBUG}
function DeleteObject(p1: HGDIOBJ): BOOL; stdcall;
{$ENDIF glDEBUG}

implementation

uses
  ShlObj, Math,
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvConsts;

{$IFNDEF USEJVCL}
resourcestring
  RsERightBracketsNotFound = 'Right brackets not found';
  RsERightBracketHavntALeftOnePosd = 'Right bracket havn''t a left one. Pos: %d';
  RsEDivideBy = 'Divide by 0';
  RsEDuplicateSignsAtPos = 'Duplicate signs at Pos: %d';
  RsEExpressionStringIsEmpty = 'Expression string is empty.';
  {$IFDEF glDEBUG}
  RsEObjectMemoryLeak = 'object memory leak';
  {$ENDIF glDEBUG}
{$ENDIF USEJVCL}

{ debug func }
{$IFDEF glDEBUG}
function DeleteObject(P1: HGDIOBJ): BOOL; stdcall;
begin
  Result := Windows.DeleteObject(P1);
  if not Result then
    raise Exception.CreateRes(@RsEObjectMemoryLeak);
end;
{$ENDIF glDEBUG}

procedure TJvgPublicWinControl.PaintWindow(DC: HDC);
begin
  inherited PaintWindow(DC);
end;

procedure TJvgPublicWinControl.RecreateWnd;
begin
  inherited RecreateWnd;
end;

//_________________________________________________________________\\

//{ Проверка целого числа на четность }
{ Checks if integer is even [translated] }

function IsEven(i: integer): boolean;
begin
  Result := i / 2 = i div 2;
end;

//{ Обмен значений }
{ Swaps the values [translated] }

procedure SwapInt(var i1, i2: integer);
var
  i_: integer;
begin
  i_ := i1;
  i1 := i2;
  i2 := i_;
end;

//{ Создание строки из заданого числа пробелов }
{ Makes string of given number of spaces [translated] }

function Spaces(Count: integer): string;
var
  i: word;
begin
  Result := ''; //if Count<=0 then exit;
  for i := 1 to Count do
    Result := Result + ' ';
end;

//{ Создание строки из заданого числа символов }
{ Makes string of given number of character [translated] }

function DupChar(C: Char; Count: integer): string;
var
  i: word;
begin
  Result := '';
  for i := 1 to Count do
    Result := Result + C;
end;

//{ Создание строки из заданого числа строк }
{ Makes string of given number of strings [translated] }

function DupStr(const str: string; Count: integer): string;
var
  i: word;
begin
  Result := '';
  for i := 1 to Count do
    Result := Result + str;
end;

//{ Модальное окно с иконкой информации и одной кнопкой ОК }
{ Modal window with (i) icon and single OK button [translated] }

procedure Msg(msg: string);
begin
  MessageBox(GetForegroundWindow, PChar(msg), '',
    MB_APPLMODAL or MB_ICONINFORMATION or MB_OK);
end;

//{ Проверка вхождения точки в примоугольник. границы не считаются }
{ Checks if point is inside rect. Rect's borders are not part of rect [translated] }
{ // (andreas) make Delphi 5 compiler happy
function IsPointInRect(P: TPoint; R: TRect): boolean;
begin
  Result := PtInRect(R,P);
//  Result := (P.x > R.Left) and (P.x < R.Right) and (P.y > R.Top) and (P.y < R.Bottom);
end;
}

//{ Длина примоугольника }
{ Rect's width [translated] }

function RectW(R: TRect): integer;
begin
  Result := R.Right - R.Left;
end;

//{ Высота примоугольника }
{ Rect's height [translated] }

function RectH(R: TRect): integer;
begin
  Result := R.Bottom - R.Top;
end;

//{ Увеличивает составляющие цвета на заданное смещение }
{ Increases components of the colour with given offset [translated] }

function IncColor(lColor: LongInt; bOffset: byte): LongInt;
var
  R, G, B: byte;
begin
  if lColor < 0 then
    lColor := GetSysColor(lColor and $FF);
  R := min(255, GetRValue(lColor) + bOffset);
  G := min(255, GetGValue(lColor) + bOffset);
  B := min(255, GetBValue(lColor) + bOffset);
  Result := RGB(R, G, B);
end;

//{ Уменьшает составляющие цвета на заданное смещение }
{ Decreases components of the colour with given offset [translated] }

function DecColor(lColor: LongInt; bOffset: byte): LongInt;
var
  R, G, B: byte;
begin
  if lColor < 0 then
    lColor := GetSysColor(lColor and $FF);
  R := Max(0, GetRValue(lColor) - bOffset);
  G := Max(0, GetGValue(lColor) - bOffset);
  B := Max(0, GetBValue(lColor) - bOffset);
  Result := RGB(R, G, B);
end;

//{ Переводит сантиметра в пиксели }
{ Centimeter -> Pixels [translated] }

function InchesToPixels(DC: HDC; Value: Single; IsHorizontal: Boolean): Integer;
const
  LogPixels: array [Boolean] of Integer = (LOGPIXELSY, LOGPIXELSX);
begin
  Result := Round(Value * GetDeviceCaps(DC, LogPixels[IsHorizontal]) * 1.541 / 10);
end;

function CentimetersToPixels(DC: HDC; Value: Single; IsHorizontal: Boolean): Integer;
const
  LogPixels: array [Boolean] of Integer = (LOGPIXELSY, LOGPIXELSX);
begin
  Result := Round(Value * GetDeviceCaps(DC, LogPixels[IsHorizontal]) * 1.541 * 2.54 / 10);
end;

//{ Проверяет создан ли объеки bitmap и имеет ли он размер }
{ Checks wheter bitmap object is created and is having size [translated] }

function IsItAFilledBitmap(Bmp: TBitmap): boolean;
begin
  with Bmp do
    Result := Assigned(bmp) and (Width <> 0) and (Height <> 0);
end;


// Выводит текст с выравниванием, с заданным стилем и заданым шрифтом.
//
// DC - handle of canvas
// HAlign, VAlign - Выравнивание по ширине и высоте
// Style - стиль (вдавленный, с тенью etc)
// FLAGS - доп. параметры для Windows.DrawText

{
  Renders text wth alignment, given style and given font

  DC             - Handle of canvas
  HAlign, VAlign - Alingment horizontal and vertical
  Style          - Style (embossed, with shadow, etc)
  FLAGS          - Extra parameters for Windows.DrawText
}

procedure DrawTextInRectWithAlign(DC: HDC; r: TRect; Text: string;
  HAlign: TglHorAlign; VAlign: TglVertAlign;
  Style: TglTextStyle; Fnt: TFont;
  FLAGS: UINT);
begin
  case HAlign of
    fhaLeft: FLAGS := FLAGS or DT_LEFT;
    fhaCenter: FLAGS := FLAGS or DT_CENTER;
    fhaRight: FLAGS := FLAGS or DT_RIGHT;
  end;
  case VAlign of
    fvaTop: FLAGS := FLAGS or DT_TOP;
    fvaCenter: FLAGS := FLAGS or DT_VCENTER;
    fvaBottom: FLAGS := FLAGS or DT_BOTTOM;
  end;

  DrawTextInRect(DC, r, Text, Style, Fnt, FLAGS);
end;


// Выводит текст с выравниванием, с заданным стилем и заданым шрифтом.
//
// DC - handle of canvas
// Style - стиль (вдавленный, с тенью etc)
// FLAGS - параметры для Windows.DrawText

{
  Renders text wth alignment, given style and given font

  DC             - Handle of canvas
  Style          - Style (embossed, with shadow, etc)
  FLAGS          - Extra parameters for Windows.DrawText
}

procedure DrawTextInRect(DC: HDC; r: TRect; Text: string; Style: TglTextStyle; Fnt: TFont; FLAGS: UINT);
var
  iOldBkMode: integer;
  OldFont: Windows.HFont;
  FontColor: TColor;
  ShadowColor_, HighlightColor_: TColor;
begin

  if not Assigned(Fnt) then
    exit;
  if FLAGS = 0 then
    FLAGS := DT_LEFT or DT_VCENTER or DT_SINGLELINE;
  iOldBkMode := SetBkMode(DC, integer(Transparent));
  FontColor := Fnt.Color;

  ShadowColor_ := clBtnShadow;
  HighlightColor_ := clBtnHighlight;

  OldFont := SelectObject(DC, Fnt.Handle);
  case Style of
    fstRaised:
      begin
        SetTextColor(DC, ColorToRGB(HighlightColor_));
        OffsetRect(r, -1, -1);
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
        SetTextColor(DC, ColorToRGB(ShadowColor_));
        OffsetRect(r, 2, 2);
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
        SetTextColor(DC, ColorToRGB(FontColor));
        OffsetRect(r, -1, -1);
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
      end;
    fstRecessed:
      begin
        SetTextColor(DC, ColorToRGB(ShadowColor_));
        OffsetRect(r, -1, -1);
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
        SetTextColor(DC, ColorToRGB(HighlightColor_));
        OffsetRect(r, 2, 2);
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
        SetTextColor(DC, ColorToRGB(FontColor));
        OffsetRect(r, -1, -1);
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
      end;
    fstPushed:
      begin
        SetTextColor(DC, ColorToRGB(HighlightColor_));
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
        SetTextColor(DC, ColorToRGB(ShadowColor_));
        OffsetRect(r, -1, -1);
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
      end;
    fstShadow:
      begin
        SetTextColor(DC, ColorToRGB(ShadowColor_));
        OffsetRect(r, 2, 2);
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
        SetTextColor(DC, ColorToRGB(FontColor));
        OffsetRect(r, -2, -2);
        DrawText(DC, PChar(Text), length(Text), r, FLAGS);
      end;
  else
    begin
      SetTextColor(DC, ColorToRGB(FontColor));
      DrawText(DC, PChar(Text), length(Text), r, FLAGS);
    end;
  end;
  SelectObject(DC, OldFont);
  SetBkMode(DC, iOldBkMode);
end;

// Выводит текст с заданным стилем, с опцией контура и заданием цветов отрисовки 3D эффектов.
//
// DC - handle of canvas
// Style - стиль (вдавленный, с тенью etc)
// fDelineated - контур цвета DelinColor
// FontColor, DelinColor, HighlightColor, ShadowColor - цвета шрифта и 3D эффектов
// Illumination - не используется
// Gradient - градиент для заливки букв текста

{
  Renders text wth given style, countouring option and given colours fo 3D effects

  DC             - Handle of canvas
  Style          - Style (embossed, with shadow, etc)
  fDelineated    - Contour of colour of DelinColour
  FontColor, DelinColor, HighlightColor, ShadowColor -
                   Colors of font and 3D effects
  Illumination   - Not used
  Gradient       - Gradient for filling letters of text
}

procedure ExtTextOutExt(DC: HDC;
  x, y: integer;
  r: TRect;
  Text: string;
  Style: TglTextStyle;
  fDelineated, fSupress3D: boolean;
  FontColor, DelinColor, HighlightColor, ShadowColor: TColor;
  Illumination: TJvgIllumination;
  Gradient: TJvgGradient;
  Font: TFont);
var
  iOldBkMode, x_, y_, i, ShadowDepth: integer;
  OldFont: Windows.HFont;
  //...local proc

  procedure DrawMain(fDelineated: boolean; s_: integer);
  begin
    if fDelineated then
    begin
      if not fSupress3D then
      begin
        SetTextColor(DC, ColorToRGB(DelinColor));
        ExtTextOut(DC, x + s_, y + s_, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
        ExtTextOut(DC, x + 2 + s_, y + 2 + s_, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
        ExtTextOut(DC, x + s_, y + s_ + 2, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
        ExtTextOut(DC, x + s_ + 2, y + s_, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
      end;
      SetTextColor(DC, ColorToRGB(FontColor));
      if Assigned(Gradient) then
        Gradient.TextOut(DC, Text, r, x + s_ + 1, y + s_ + 1)
      else
        ExtTextOut(DC, x + s_ + 1, y + s_ + 1, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
    end
    else
    begin
      SetTextColor(DC, ColorToRGB(FontColor));
      if Assigned(Gradient) then
        Gradient.TextOut(DC, Text, r, x + s_, y + s_)
      else
        ExtTextOut(DC, x + s_, y + s_, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
    end;
  end;
begin
  if (not Assigned(Font)) then
    exit;
  OldFont := SelectObject(DC, Font.Handle);
  iOldBkMode := SetBkMode(DC, TRANSPARENT);

  if fDelineated then
  begin
    x_ := 4;
    y_ := 4;
  end
  else
  begin
    x_ := 2;
    y_ := 2;
  end;
  if Style = fstNone then
  begin
    x_ := x_ div 2 - 1;
    y_ := y_ div 2 - 1;
  end;
  if Style = fstShadow then
  begin
    x_ := x_ div 2 - 1;
    y_ := y_ div 2 - 1;
  end;
  if Assigned(Illumination) then
    ShadowDepth := Illumination.ShadowDepth
  else
    ShadowDepth := 2;
  case Style of
    fstRaised:
      begin
        if not fSupress3D then
        begin
          SetTextColor(DC, ColorToRGB(HighlightColor));
          ExtTextOut(DC, x, y, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
          SetTextColor(DC, ColorToRGB(ShadowColor));
          ExtTextOut(DC, x + x_, y + y_, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
        end;
        DrawMain(fDelineated, 1);
      end;
    fstRecessed:
      begin
        if not fSupress3D then
        begin
          SetTextColor(DC, ColorToRGB(ShadowColor));
          ExtTextOut(DC, x, y, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
          SetTextColor(DC, ColorToRGB(HighlightColor));
          ExtTextOut(DC, x + x_, y + y_, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
        end;
        DrawMain(fDelineated, 1);
      end;
    fstPushed:
      begin
        SetTextColor(DC, ColorToRGB(HighlightColor));
        ExtTextOut(DC, x + 1, y + 1, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
        SetTextColor(DC, ColorToRGB(ShadowColor));
        ExtTextOut(DC, x, y, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
      end;
    fstShadow:
      begin
        if not fSupress3D then
        begin
          SetTextColor(DC, ColorToRGB(ShadowColor));
          ExtTextOut(DC, x + x_ + ShadowDepth, y + y_ + ShadowDepth, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
        end;
        DrawMain(fDelineated, 0);
      end;
    fstVolumetric:
      begin
        if not fSupress3D then
        begin
          SetTextColor(DC, ColorToRGB(ShadowColor));
          for i := 1 to ShadowDepth do
            ExtTextOut(DC, x + i, y + i, ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
        end;
        DrawMain(fDelineated, 0);
      end;
  else
    begin
      DrawMain(fDelineated, 0);
      //    SetTextColor( DC , ColorToRGB(FontColor) );
      //    ExtTextOut( DC, x, y,  ETO_CLIPPED, @r, PChar(Text), Length(Text), nil);
    end;
  end;
  SelectObject(DC, OldFont);
  SetBkMode(DC, iOldBkMode);
end;

// Рисует прямоугольник с заданым 3D стилем.
//
// DC - handle of canvas
// Style - стиль (fbsFlat, fbsCtl3D, fbsStatusControl, fbsRecessed, fbsRaised, fbsRaisedFrame, fbsRecessedFrame)
// BackgrColor- цвет фона, если fTransparent = false

{
   Draws rect with given 3D style

   DC          - Handle of canvas
   Style       - Style (fbsFlat, fbsCtl3D, fbsStatusControl, fbsRecessed, fbsRaised, fbsRaisedFrame, fbsRecessedFrame)
   BackgrColor - Background Color if FTransparen is false
}


procedure DrawBox(DC: HDC; var r: TRect; Style: TglBoxStyle;
  BackgrColor: LongInt; fTransparent: boolean);
const
  FBorderWidth = 1;
begin

  case Style of
    fbsFlat: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      begin
      end;
    fbsCtl3D: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      begin
        r.top := r.top + 2;
        r.left := r.left + 2;
        r.right := r.right - 2;
        r.bottom := r.bottom - 1;
        //  Frame3D(Canvas, r,clBtnShadow,clBtnHighlight,1);
      end;
    //    fbsStatusControl:
    fbsRaised: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      begin
        InflateRect(r, -2, -2);
        DrawEdge(DC, r, BDR_RAISEDOUTER, BF_BOTTOMRIGHT); // black
        Dec(r.Bottom);
        Dec(r.Right);
        DrawEdge(DC, r, BDR_RAISEDINNER, BF_TOPLEFT); // btnhilite
        Inc(r.Top);
        Inc(r.Left);
        DrawEdge(DC, r, BDR_RAISEDINNER, BF_BOTTOMRIGHT or BF_MIDDLE); // btnshadow
      end;
    fbsRecessed: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      begin
        r.bottom := r.bottom - 1;
        DrawEdge(DC, r, BDR_SUNKENINNER, BF_TOPLEFT); // black
        DrawEdge(DC, r, BDR_SUNKENOUTER, BF_BOTTOMRIGHT); // btnhilite
        Dec(r.Bottom);
        Dec(r.Right);
        Inc(r.Top);
        Inc(r.Left);
        DrawEdge(DC, r, BDR_SUNKENOUTER, BF_TOPLEFT or BF_MIDDLE); // btnshadow
        inc(r.top);
        inc(r.left);
      end;
    fbsRaisedFrame: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      begin
        DrawEdge(DC, r, BDR_RAISEDOUTER, BF_BOTTOMRIGHT); // black
        Dec(r.Bottom);
        Dec(r.Right);
        DrawEdge(DC, r, BDR_RAISEDINNER, BF_TOPLEFT); // btnhilite
        Inc(r.Top);
        Inc(r.Left);
        DrawEdge(DC, r, BDR_RAISEDINNER, BF_BOTTOMRIGHT or BF_MIDDLE); // btnshadow

        InflateRect(r, -FBorderWidth, -FBorderWidth);

        DrawEdge(DC, r, BDR_SUNKENINNER, BF_TOPLEFT); // black
        DrawEdge(DC, r, BDR_SUNKENOUTER, BF_BOTTOMRIGHT); // btnhilite
        Dec(r.Bottom);
        Dec(r.Right);
        Inc(r.Top);
        Inc(r.Left);
        DrawEdge(DC, r, BDR_SUNKENOUTER, BF_TOPLEFT or BF_MIDDLE); // btnshadow
        inc(r.top);
        inc(r.left);

      end;

    fbsRecessedFrame: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      begin
        DrawEdge(DC, r, BDR_SUNKENINNER, BF_TOPLEFT); // black
        DrawEdge(DC, r, BDR_SUNKENOUTER, BF_BOTTOMRIGHT); // btnhilite
        Dec(r.Bottom);
        Dec(r.Right);
        Inc(r.Top);
        Inc(r.Left);
        DrawEdge(DC, r, BDR_SUNKENOUTER, BF_TOPLEFT or BF_MIDDLE); // btnshadow
        inc(r.top);
        inc(r.left);

        InflateRect(r, -FBorderWidth, -FBorderWidth);

        DrawEdge(DC, r, BDR_RAISEDOUTER, BF_BOTTOMRIGHT); // black
        Dec(r.Bottom);
        Dec(r.Right);
        DrawEdge(DC, r, BDR_RAISEDINNER, BF_TOPLEFT); // btnhilite
        Inc(r.Top);
        Inc(r.Left);
        DrawEdge(DC, r, BDR_RAISEDINNER, BF_BOTTOMRIGHT or BF_MIDDLE); // btnshadow

      end;
  end;
end;


// Рисует прямоугольник с заданым 3D стилем и с указанием сторон.
//
// DC - handle of canvas
// Borders - стороны для отрисовки
// BevelInner, BevelOuter - стили сторон
// Bold - толстая рамка
// BackgrColor- цвет фона, если fTransparent = false

{
  Draws rect with given 3D style and specifing borders

  DC          - Handle of canvas
  Borders     - Borders for drawing
  BevelInner, BevelOuter - Borders' styles
  Bold        - Bold border(frame)
  BackgrColor - Background Color if fTransparent is false
}

function DrawBoxEx(DC: HDC; rect: TRect; Borders: TglSides;
  BevelInner, BevelOuter: TPanelBevel;
  Bold: boolean; BackgrColor: LongInt;
  fTransparent: boolean): TRect;
var
  i: word;
  BPen, LPen, SPen, OldPen: HPEN;
  hBackgrBrush, hOldBrush: HBRUSH;
  r, r_: TRect;
  BColor, HColor, SColor: LongInt;
  LOGOLDPEN: TLOGPEN;
  PenWidth: UINT;

  procedure SetDefColors;
  begin
    BColor := GetSysColor(COLOR_3DDKSHADOW);
    HColor := GetSysColor(COLOR_3DHILIGHT);
    SColor := GetSysColor(COLOR_3DSHADOW);
  end;

  procedure DrawBevel(Bevel: TPanelBevel); //____________LOCAL PROC_
  begin
    if fsdLeft in Borders then
    begin
      case Bevel of
        bvRaised:
          begin
            SelectObject(DC, LPen);
            MoveToEx(DC, r.left, r.top, nil);
            LineTo(DC, r.left, r.bottom + 1);
            inc(r_.left);
            //.if Bold then inc(r_.left);
          end;
        bvLowered:
          begin
            if Bold then
            begin
              SelectObject(DC, BPen);
              MoveToEx(DC, r.left, r.top, nil);
              LineTo(DC, r.left, r.bottom);
              inc(r_.left);
              SelectObject(DC, SPen);
              if fsdBottom in Borders then
                i := 0
              else
                i := 1;
              MoveToEx(DC, r.left + 1, r.top + 1, nil);
              LineTo(DC, r.left + 1, r.bottom + i);
              //SetPixel(DC, r.left, r.bottom-1, SColor);
              inc(r_.left);
            end
            else
            begin
              SelectObject(DC, SPen);
              MoveToEx(DC, r.left, r.top, nil);
              LineTo(DC, r.left, r.bottom);
              inc(r_.left);
            end;
          end;
        bvSpace:
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, r.left, r.top, nil);
            LineTo(DC, r.left, r.bottom);
            inc(r_.left);
          end;
      end; //........{ . END CASE . }
    end;
    if fsdTop in Borders then
    begin
      case Bevel of
        bvRaised:
          begin
            SelectObject(DC, LPen);
            MoveToEx(DC, r.left, r.top, nil);
            LineTo(DC, r.right, r.top);
            inc(r_.top);
            //.if Bold then inc(r_.top);
          end;
        bvLowered:
          begin
            if Bold then
            begin
              SelectObject(DC, BPen);
              MoveToEx(DC, r.left, r.top, nil);
              LineTo(DC, r.right, r.top);
              inc(r_.top);
              SelectObject(DC, SPen);
              MoveToEx(DC, r.left + 1, r.top + 1, nil);
              LineTo(DC, r.right, r.top + 1);
              //SetPixel(DC, r.right-1, r.top+1, SColor);
              inc(r_.top);
            end
            else
            begin
              SelectObject(DC, SPen);
              MoveToEx(DC, r.left, r.top, nil);
              LineTo(DC, r.right, r.top);
              inc(r_.top);
            end;
          end;
        bvSpace:
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, r.left, r.top, nil);
            LineTo(DC, r.right, r.top);
            inc(r_.top);
          end;
      end; //........{ . END CASE . }
    end;
    if fsdRight in Borders then
    begin
      case Bevel of
        bvRaised:
          begin
            if Bold then
            begin
              SelectObject(DC, BPen);
              MoveToEx(DC, r.right, r.top, nil);
              LineTo(DC, r.right, r.bottom + 1);
              dec(r_.right);
              SelectObject(DC, SPen);
              MoveToEx(DC, r.right - 1, r.top + 1, nil);
              LineTo(DC, r.right - 1, r.bottom + 1);
              //SetPixel(DC, r.right-1, r.bottom-1, SColor);
              dec(r_.right);
            end
            else
            begin
              SelectObject(DC, SPen);
              MoveToEx(DC, r.right, r.top, nil);
              LineTo(DC, r.right, r.bottom + 1);
              dec(r_.right);
            end;
          end;
        bvLowered:
          begin
            SelectObject(DC, LPen);
            MoveToEx(DC, r.right, r.top, nil);
            LineTo(DC, r.right, r.bottom);
            dec(r_.right);
            //. if Bold then dec(r_.right);
          end;
        bvSpace:
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, r.right, r.top, nil);
            LineTo(DC, r.right, r.bottom);
            dec(r_.right);
          end;
      end; //........{ . END CASE . }
    end;
    if fsdBottom in Borders then
    begin
      case Bevel of
        bvRaised:
          begin
            if Bold then
            begin
              SelectObject(DC, BPen);
              if fsdLeft in Borders then
                i := 1
              else
                i := 0;
              MoveToEx(DC, r.left {+1}, r.bottom, nil);
              LineTo(DC, r.right, r.bottom);
              dec(r_.bottom);
              SelectObject(DC, SPen);
              MoveToEx(DC, r.left + i {+i}, r.bottom - 1, nil);
              LineTo(DC, r.right, r.bottom - 1);
              //SetPixel(DC, r.right-1+i, r.bottom-1, SColor);
              dec(r_.bottom);
            end
            else
            begin
              SelectObject(DC, SPen);
              MoveToEx(DC, r.left, r.bottom, nil);
              LineTo(DC, r.right, r.bottom);
              dec(r_.bottom);
            end;
          end;
        bvLowered:
          begin
            SelectObject(DC, LPen);
            //    if Borders.Left then i:=1 else i:=0;
            MoveToEx(DC, r.left, r.bottom {-1}, nil);
            LineTo(DC, r.right + 1, r.bottom {-1});
            dec(r_.bottom);
            //. if Bold then dec(r_.bottom);
            //dec(r_.bottom);
          end;
        bvSpace:
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, r.left, r.bottom {-1}, nil);
            LineTo(DC, r.right + 1, r.bottom {-1});
            dec(r_.bottom);
          end;
      end; //........{ . END CASE . }
    end;
  end; //_______________________________________LOCAL END_
begin //_________________________________________MAIN_

  try
    if Assigned(glGlobalData.lp3DColors) then
      with TJvg3DColors(glGlobalData.lp3DColors) do
      begin
        BColor := ColorToRGB(DkShadow);
        HColor := ColorToRGB(Highlight);
        SColor := ColorToRGB(Shadow);
      end
    else
      SetDefColors;

  except
  end;

  LPen := CreatePen(PS_SOLID, 1, HColor); //.
  OldPen := SelectObject(DC, LPen); //..
  DeleteObject(SelectObject(DC, OldPen)); //...get OldPen

  FillChar(LOGOLDPEN, SizeOf(LOGOLDPEN), 0);
  GetObject(OldPen, SizeOf(LOGOLDPEN), @LOGOLDPEN);
  if LOGOLDPEN.lopnWidth.x = 0 then
    PenWidth := 1
  else
    PenWidth := LOGOLDPEN.lopnWidth.x;
  BPen := CreatePen(LOGOLDPEN.lopnStyle, PenWidth, BColor);
  LPen := CreatePen(LOGOLDPEN.lopnStyle, PenWidth, HColor);
  SPen := CreatePen(LOGOLDPEN.lopnStyle, PenWidth, SColor);
  SelectObject(DC, LPen);
  r_ := rect;
  r := rect;
  if BevelOuter <> bvNone then
    DrawBevel(BevelOuter);
  r := r_;
  //  if (BevelOuter = bvRaised)and(BevelInner = bvLowered)and Bold then
  //  begin dec(r.top); dec(r.left); end;

  if BevelInner <> bvNone then
    DrawBevel(BevelInner);

  SelectObject(DC, OldPen);
  DeleteObject(BPen);
  DeleteObject(LPen);
  DeleteObject(SPen);

  if not fTransparent then
  begin
    hBackgrBrush := CreateSolidBrush(ColorToRGB(BackgrColor));
    hOldBrush := SelectObject(DC, hBackgrBrush);
    r := r_; {dec(r.top);dec(r.left);}
    inc(r.right);
    inc(r.bottom);
    FillRect(DC, r, hBackgrBrush);
    DeleteObject(SelectObject(DC, hOldBrush));
  end;

  Result := r_;
end;

// Рисует градиент TJvgGradient
{ Draws TJvgGradient gradient [translated] }

procedure GradientBox(DC: HDC; r: TRect; Gradient: TJvgGradient; PenStyle, PenWidth: integer);
begin
  Gradient.Draw(DC, r, PenStyle, PenWidth);
end;

// Заменяет цвет у битмапа
{ Replaces bitmap's color [translated] }

procedure ChangeBitmapColor(Bitmap: TBitmap;
  FromColor, ToColor: TColor);
var
  IWidth, IHeight: Integer;
  DRect, SRect: TRect;
  MonoBMP, OldBMP: HBitmap;
  MonoDC: HDC;
begin
  if (Bitmap.Width or Bitmap.Height) = 0 then
    Exit;
  IWidth := Bitmap.Width;
  IHeight := Bitmap.Height;
  DRect := Rect(0, 0, IWidth, IHeight);
  SRect := DRect;

  MonoDC := CreateCompatibleDC(Bitmap.Canvas.handle);
  MonoBMP := CreateBitmap(IWidth, IHeight, 1, 1, nil);
  OldBMP := SelectObject(MonoDC, MonoBMP);

  try
    with Bitmap.Canvas do { Convert FromColor to ToColor }
    begin
      Bitmap.Canvas.Brush.Color := FromColor;
      {copy Bitmap to MonoBmp}
      BitBlt(MonoDC, 0, 0, IWidth, IHeight, Handle, 0, 0, cmSrcCopy);
      Brush.Color := ToColor;
      SetTextColor(Handle, clBlack);
      SetBkColor(Handle, clWhite);
      BitBlt(Handle, 0, 0, IWidth, IHeight,
        MonoDC, 0, 0, ROP_DSPDxax);
    end;
  finally
    DeleteObject(SelectObject(MonoDC, OldBMP));
    DeleteDC(MonoDC);
  end;
end;

// Выводит битмап. Прозрачный, disabled, размноженный etc
{ Paints bitmap. Transparent, disabled, multiplied, etc [translated] }

procedure DrawBitmapExt(DC: HDC; { DC - background & result}
  SourceBitmap: TBitmap;
  r: TRect;
  x, y: integer; //...x,y _in_ rect!
  BitmapOption: TglWallpaperOption;
  DrawState: TglDrawState;
  fTransparent: boolean;
  TransparentColor: TColor;
  DisabledMaskColor: TColor);
begin

  CreateBitmapExt(DC, SourceBitmap, r, x, y, BitmapOption,
    DrawState, fTransparent, TransparentColor,
    DisabledMaskColor);
end;

//..DrawBitmap algorithm borrow from Delphi2 VCL Sources
{ create bimap based on  SourceBitmap and write new bitmap to DC }

procedure CreateBitmapExt(DC: HDC; {target DC}
  SourceBitmap: TBitmap;
  r: TRect;
  x, y: integer; //...x,y _in_ rect!
  BitmapOption: TglWallpaperOption;
  DrawState: TglDrawState;
  fTransparent: boolean;
  TransparentColor: TColor;
  DisabledMaskColor: TColor);
var
  x_, y_, H, W: integer;
  D, D_: double;
  TmpImage, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  //  DestDC: HDC;
  BmpInfo: Windows.TBitmap;
  ptSize, ptOrg: TPoint;
  MemDC, ImageDC: HDC;
  OldBMP, OldMonoBMP, OldScreenImageBMP, OldMemBMP: HBitmap;
  Mono_BMP, ScreenImageBMP, MemBMP: HBitmap;
  MonoDC, ScreenImageDC: HDC;
  OldBkColor: COLORREF;
  SavedIHeight: integer;
const
  ROP_DSPDxax = $00E20746;

  procedure BitBltWorks; //*************************************END LOCAL
  begin
    if fTransparent then
    begin
      { create copy of drawing image }
      BitBlt(MemDC, 0, 0, IWidth, IHeight, ImageDC, 0, 0, SRCCOPY);
      if DrawState = fdsDisabled then
        TransparentColor := clBtnFace;
      OldBkColor := SetBkColor(MemDC, ColorToRGB(TransparentColor));
      { create monohrome mask: TransparentColor -> white, other color -> black }
      BitBlt(MonoDC, 0, 0, IWidth, IHeight, MemDC, 0, 0, SRCCOPY);
      SetBkColor(MemDC, OldBkColor);
      {create copy of screen image}
      BitBlt(ScreenImageDC, 0, 0, IWidth, IHeight, DC, x_, y_, SRCCOPY);
      { put monohrome mask }
      BitBlt(ScreenImageDC, 0, 0, IWidth, IHeight, MonoDC, 0, 0, SRCAND);
      BitBlt(MonoDC, 0, 0, IWidth, IHeight, MonoDC, 0, 0, NOTSRCCOPY);
      { put inverse monohrome mask }
      BitBlt(MemDC, 0, 0, IWidth, IHeight, MonoDC, 0, 0, SRCAND);
      { merge Screen screen image(MemDC) and Screen image(ScreenImageDC) }
      BitBlt(MemDC, 0, 0, IWidth, IHeight, ScreenImageDC, 0, 0, SRCPAINT);
      { to screen }
  //    DSTINVERT MERGEPAINT
      BitBlt(DC, x_, y_, IWidth, IHeight, MemDC, 0, 0, SRCCOPY);
    end
    else
      BitBlt(DC, x_, y_, IWidth, IHeight, ImageDC, 0, 0, SRCCOPY);

  end; //*******************************************END LOCAL PROC

begin
  if (SourceBitmap.Width or SourceBitmap.Height) = 0 then
    Exit;

  x := x + r.left;
  y := y + r.top;
  x_ := x;
  y_ := y;
  OldBMP := 0;
  OldMemBMP := 0;
  OldMonoBMP := 0;
  OldScreenImageBMP := 0;
  MemDC := 0;
  ImageDC := 0;
  // Mono_BMP := 0;
  // ScreenImageBMP := 0;
  // MemBMP := 0;
  MonoDC := 0;
  ScreenImageDC := 0;

  IWidth := SourceBitmap.Width; //min( SourceBitmap.Width, r.right-r.left );
  IHeight := SourceBitmap.Height; //min( SourceBitmap.Height, r.bottom-r.top );
  TmpImage := TBitmap.Create;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~/
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    ORect := Rect(0, 0, IWidth, IHeight);

    TmpImage.Canvas.Brush.Color := TransparentColor;
    TmpImage.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));

    case DrawState of
      fdsDefault: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        begin
          BitBlt(TmpImage.Canvas.Handle, 0, 0, IWidth, IHeight,
            SourceBitmap.canvas.Handle, 0, 0, SRCCOPY);
        end;
      fdsDelicate: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        begin
          with TmpImage.Canvas do
            BitBlt(Handle, 0, 0, IWidth, IHeight,
              SourceBitmap.canvas.Handle, 0, 0, cmSrcCopy);
          begin
            { Convert white to clBtnHighlight }
            ChangeBitmapColor(TmpImage, clWhite, clBtnHighlight);
            { Convert gray to clBtnShadow }
            ChangeBitmapColor(TmpImage, clGray, clBtnShadow);
            { Convert transparent color to clBtnFace }
         //     ChangeBitmapColor(TmpImage,ColorToRGB(}TransparentColor),clBtnFace);
          end;
        end;
      fdsDisabled: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        begin
          if DisabledMaskColor <> 0 then
            ChangeBitmapColor(TmpImage, DisabledMaskColor, clBlack);
          MonoBmp := Tbitmap.Create;
          try { Create a disabled version }
            with MonoBmp do
            begin
              Assign(SourceBitmap);
              Canvas.Brush.Color := 0;
              Width := IWidth;
              if Monochrome then
              begin
                Canvas.Font.Color := clWhite;
                Monochrome := False;
                Canvas.Brush.Color := clWhite;
              end;
              Monochrome := True;
            end;
            with TmpImage.Canvas do
            begin
              Brush.Color := clBtnFace;
              FillRect(IRect);
              Brush.Color := clBtnHighlight;
              SetTextColor(Handle, 0);
              SetBkColor(Handle, clWhite);
              BitBlt(Handle, 1, 1, IWidth, IHeight,
                MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              Brush.Color := clBtnShadow;
              SetTextColor(Handle, 0);
              SetBkColor(Handle, clWhite);
              BitBlt(Handle, 0, 0, IWidth, IHeight,
                MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end;
          finally
            MonoBmp.Free;
          end;
        end;
    end; {CASE}

    with TmpImage.Canvas do
      if (BitmapOption = fwoStretch) or (BitmapOption = fwoPropStretch) then
      begin
        MemDC := CreateCompatibleDC(DC);
        MemBMP := CreateCompatibleBitmap(TmpImage.Canvas.Handle, r.right - r.left, r.bottom - r.top);
        OldMemBMP := SelectObject(MemDC, MemBMP);
        W := r.right - r.left;
        H := r.bottom - r.top;
        if BitmapOption = fwoPropStretch then
        begin
          D_ := W / IWidth;
          D := H / IHeight;
          if D > D_ then
            D := D_; //...D == min
          W := trunc(IWidth * D);
          H := trunc(IHeight * D);
        end;
        StretchBlt(MemDC, 0, 0, W, H, Handle,
          0, 0, IWidth, IHeight, SRCCOPY);

        IWidth := W;
        IHeight := H;
        TmpImage.Width := W;
        TmpImage.Height := H;
        BitBlt(Handle, 0, 0, IWidth, IHeight, MemDC, 0, 0, SRCCOPY);

        DeleteObject(SelectObject(MemDC, OldMemBMP));
        DeleteDC(MemDC);
      end;

    ImageDC := CreateCompatibleDC(DC);

    if fTransparent then
    begin
      MemDC := CreateCompatibleDC(DC);
      ScreenImageDC := CreateCompatibleDC(DC);
      MonoDC := CreateCompatibleDC(DC);

      Mono_BMP := CreateBitmap(IWidth, IHeight, 1, 1, nil);
      ScreenImageBMP := CreateCompatibleBitmap(TmpImage.Canvas.Handle, IWidth, IHeight);
      MemBMP := CreateCompatibleBitmap(TmpImage.Canvas.Handle, IWidth, IHeight);

      OldMonoBMP := SelectObject(MonoDC, Mono_BMP);
      OldScreenImageBMP := SelectObject(ScreenImageDC, ScreenImageBMP);
      OldMemBMP := SelectObject(MemDC, MemBMP);
    end;
    OldBMP := SelectObject(ImageDC, TmpImage.Handle);

    if OldBMP <> 0 then
    begin
      SetMapMode(ImageDC, GetMapMode(DC));
      GetObject(TmpImage.Handle, SizeOf(Windows.TBitmap), @BmpInfo);
      ptSize.x := BmpInfo.bmWidth;
      ptOrg.x := 0;
      ptSize.y := BmpInfo.bmHeight;
      ptOrg.y := 0;
      if fTransparent then
      begin
        DPtoLP(DC, ptSize, 1);
        DPtoLP(MemDC, ptOrg.y, 1);
      end;
      if BitmapOption = fwoTile then
      begin
        //SavedIWidth:=IWidth;
        SavedIHeight := IHeight;
        while x_ < r.right do
        begin
          //IWidth:=SavedIWidth; //SavedIWidth:=IWidth;
          if x_ + IWidth > r.right then
            IWidth := r.right - x_;
          while y_ < r.bottom do
          begin
            IHeight := SavedIHeight; // SavedIHeight:=IHeight;
            if y_ + IHeight > r.bottom then
              IHeight := r.bottom - y_;
            BitBltWorks;
            Inc(y_, IHeight);
          end;
          Inc(x_, IWidth);
          y_ := y;
        end;
      end
      else
        BitBltWorks;
    end;
  finally
    DeleteObject(SelectObject(ImageDC, OldBMP));
    DeleteDC(ImageDC);
    if fTransparent then
    begin
      DeleteObject(SelectObject(MonoDC, OldMonoBMP));
      DeleteObject(SelectObject(ScreenImageDC, OldScreenImageBMP));
      DeleteObject(SelectObject(MemDC, OldMemBMP));
      DeleteDC(MonoDC);
      DeleteDC(ScreenImageDC);
      DeleteDC(MemDC);
    end;
    TmpImage.Free;
  end;

end;

//{ Выносит родительское окно на передний план }
{ Brings parent window forefront [translated] }

procedure BringParentWindowToTop(Wnd: TWinControl);
begin
  if Wnd is TForm then
    BringWindowToTop(Wnd.Handle)
  else
  if Wnd.Parent is TWinControl then
    BringParentWindowToTop(Wnd.Parent);
end;

//{ Возвращает родительское окно класса TForm }
{ Gives parent window of TForm class }

function GetParentForm(Control: TControl): TForm;
begin
  if Control is TForm then
  begin
    Result := TForm(Control);
    exit;
  end
  else
  if Control.Parent is TWinControl then
    Result := GetParentForm(Control.Parent)
  else
    Result := nil;
end;


// Отрисовывает TWinControl со всем содержимым на DC со смещением X, Y
// ...from rxLib... :( very sorry
{ Paints TWinControl with all its content onto DC with offset(shift) X,Y
  ...from rxLib... :( very sorry [translated] }

procedure GetWindowImageFrom(Control: TWinControl; X, Y: integer; fDrawSelf, fDrawChildWindows: boolean; DC: HDC);
var
  I, Count, SaveIndex: Integer;
begin
  if Control = nil then
    exit;
  Count := Control.ControlCount;

  { Copy self image }
  if fDrawSelf then
  begin
    SaveIndex := SaveDC(DC);
    SetViewportOrgEx(DC, X, Y, nil);
    TJvgPublicWinControl(Control).PaintWindow(DC);
    RestoreDC(DC, SaveIndex);
  end;
  { Copy images of graphic controls }
  for I := 0 to Count - 1 do
  begin
    if (Control.Controls[I] <> nil) then
    begin
      if Control.Controls[I] = Control then
        Break;
      if (Control.Controls[I] is TWinControl) and fDrawChildWindows then
        GetWindowImageFrom(TWinControl(Control.Controls[I]),
          TWinControl(Control.Controls[I]).Left,
          TWinControl(Control.Controls[I]).Top,
          true {fDrawSelf}, fDrawChildWindows, DC)
      else
        with Control.Controls[I] do
          if Visible then
          begin
            SaveIndex := SaveDC(DC);
            SetViewportOrgEx(DC, Left + X, Top + Y, nil);
            Perform(WM_PAINT, Longint(DC), 0);
            RestoreDC(DC, SaveIndex);
          end;
    end;
  end;
end;

// Отрисовывает TWinControl со всем содержимым на DC со смещением 0, 0
{ Paints(renders) TWinControl with all its content onto DC with offset (0,0) [translated] }

procedure GetWindowImage(Control: TWinControl; fDrawSelf, fDrawChildWindows: boolean; DC: HDC);
begin
  GetWindowImageFrom(Control, 0, 0, fDrawSelf, fDrawChildWindows, DC);
end;

// Отрисовывает родительский TWinControl со всем содержимым на DC с ограничением по Rect
{ Paints parent TWinControl with all its contents onto DC with limit of Rect [translated ] }

procedure GetParentImageRect(Control: TControl; Rect: TRect; DC: HDC);
var
  I, Count, X, Y, SaveIndex: Integer;
  R, SelfR, CtlR: TRect;
begin
  if Control.Parent = nil then
    exit;
  Count := Control.Parent.ControlCount;
  SelfR := Bounds(Control.Left, Control.Top, Control.Width, Control.Height);
  //  OffsetRect( Rect, Control.Left, Control.Top );
  IntersectRect(SelfR, SelfR, Rect);

  X := -Rect {Control}.Left;
  Y := -Rect {Control}.Top;
  { Copy parent control image }
  SaveIndex := SaveDC(DC);
  SetViewportOrgEx(DC, X, Y, nil);
  IntersectClipRect(DC, 0, 0, {Control.Parent.ClientWidth} Rect.Right,
    {Control.Parent.ClientHeight} Rect.Bottom);
  TJvgPublicWinControl(Control.Parent).PaintWindow(DC);
  RestoreDC(DC, SaveIndex);
  { Copy images of graphic controls }
  for I := 0 to Count - 1 do
  begin
    if (Control.Parent.Controls[I] <> nil) and
      not (Control.Parent.Controls[I] is TWinControl) then
    begin
      if Control.Parent.Controls[I] = Control then
        Break;
      with Control.Parent.Controls[I] do
      begin
        CtlR := Bounds(Left, Top, Width, Height);
        if IntersectRect(R, SelfR, CtlR) and Visible then
        begin
          SaveIndex := SaveDC(DC);
          SetViewportOrgEx(DC, Left + X, Top + Y, nil);
          IntersectClipRect(DC, 0, 0, Width, Height);
          Perform(WM_PAINT, Longint(DC), 0);
          RestoreDC(DC, SaveIndex);
        end;
      end;
    end;
  end;
end;

// Создает шрифт с заданным углом наклона
{ Makes font inclined with angle [translated] }

function CreateRotatedFont(F: TFont; Escapement: Integer): hFont;
{-create a rotated font based on the font object F}
var
  LF: TLogFont;
begin
  FillChar(LF, SizeOf(LF), #0);
  with LF do
  begin
    lfHeight := F.Height;
    //    lfWidth        := 8;//FHeight div 4;
    lfEscapement := Escapement;
    lfOrientation := 0;
    if fsBold in F.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    //    if FFontWeight     <> fwDONTCARE then lfWeight:=uFontWeight;
    lfItalic := Byte(fsItalic in F.Style);
    lfUnderline := Byte(fsUnderline in F.Style);
    lfStrikeOut := Byte(fsStrikeOut in F.Style);
    lfCharSet := F.CHARSET;
    StrPCopy(lfFaceName, F.Name);
    lfQuality := DEFAULT_QUALITY;
    {everything else as default}
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case F.Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Result := CreateFontIndirect(LF);
end;

// Возвращает главное окно приложения
{ Returns main window of application [translated] }

function FindMainWindow(sWndClass, sWndTitle: string): HWND;
begin
  Result := 0;
  if (sWndClass = '') and (sWndTitle = '') then
    exit;
  Result := FindWindow(PChar(sWndClass), PChar(sWndTitle));
end;

// Для заданного цвета BaseColor считает цвет тени и подсветки
{ Calculates colors of shadow and lighted border for given base color. [translated] }

procedure CalcShadowAndHighlightColors(BaseColor: TColor; Colors: TJvgLabelColors);
var
  r, g, b: byte;
begin
  with Colors do
  begin
    if (BaseColor and $80000000) <> 0 then
      BaseColor := GetSysColor(BaseColor and $FF);
    b := (BaseColor and $00FF0000) shr 16;
    g := (BaseColor and $0000FF00) shr 8;
    r := BaseColor and $000000FF;
    if AutoShadow then
    begin
      {if r<g then limit:=r else limit:=g; if b<limit then limit:=b;//...min
      if limit<FColorShadowShift then FColorShadowShift:=limit;
      FShadow := RGB(r-FColorShadowShift,g-FColorShadowShift,b-FColorShadowShift);}
      Shadow := RGB(Max(r - ColorShadowShift, 0), max(g - ColorShadowShift, 0), max(b - ColorShadowShift, 0));
    end;
    if AutoHighlight then
    begin
      {if r>g then limit:=r else limit:=g; if b>limit then limit:=b;//...max
      if (255-limit)<FColorHighlightShift then FColorHighlightShift:=255-limit;
      FHighlight := RGB(r+FColorHighlightShift,g+FColorHighlightShift,b+FColorHighlightShift);}
      Highlight := RGB(min(r + ColorHighlightShift, 255), min(g + ColorHighlightShift, 255), min(b +
        ColorHighlightShift, 255));
    end;
  end;
end;

// Считает арифметическое выражение из строки
{ Calculates arithmetic expression, given in string [translated] }

function CalcMathString(sExpression: string): single;
var
  ExpressionPtr, ExpressionLength, BracketsCount: integer;
  fCalcResult: boolean;
  cCurrChar: char;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~LOCAL PROCS
  function Expression: single; forward;

  procedure NextChar;
  begin
    inc(ExpressionPtr);
    if ExpressionPtr <= ExpressionLength then
      cCurrChar := sExpression[ExpressionPtr]
    else
      cCurrChar := #0;
    if cCurrChar = ' ' then
      NextChar;
    if cCurrChar = #0 then
      exit;
    if not (cCurrChar in ['0'..'9', ',', '.', '-', '+', '/', '*', '(', ')']) then
      NextChar;
  end;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  function DigitsToValue: single;
  var
    PointDepth: integer;
    fPoint: boolean;
  begin
    Result := 0;
    fPoint := false;
    PointDepth := 0;
    while cCurrChar = ' ' do
      NextChar;

    if (cCurrChar <= '9') and (cCurrChar >= '0') then //.....digit_
    begin
      while (cCurrChar <= '9') and (cCurrChar >= '0') do
      begin
        Result := Result * 10 + ord(cCurrChar) - ord('0');
        NextChar;
        if fPoint then
          inc(PointDepth);
        if (cCurrChar = '.') or (cCurrChar = ',') then
        begin
          NextChar;
          fPoint := true;
        end;
      end; //While
      if PointDepth <> 0 then
        Result := Result / (10 * PointDepth);
    end
    else //...............................................sign or braket_
    begin
      case cCurrChar of
        '-':
          begin
            NextChar;
            Result := -1 * Result;
          end;
        '(':
          begin
            inc(BracketsCount);
            NextChar;
            Result := Expression;
            while cCurrChar = ' ' do
              NextChar;
            if cCurrChar <> ')' then
            begin
              raise Exception.CreateRes(@RsERightBracketsNotFound);
              fCalcResult := false;
              Result := 0;
            end
            else
              NextChar;
          end;
        // '.': fPoint := true;
        // ',': fPoint := true;
      end; //CASE
    end;
    if cCurrChar = ')' then
    begin
      dec(BracketsCount);
      if BracketsCount < 0 then
        raise Exception.CreateResFmt(@RsERightBracketHavntALeftOnePosd, [ExpressionPtr - 1]);
    end;
  end;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  function TestFor_MulDiv: single;
  var
    Denominator: single;
  begin
    Result := DigitsToValue; // . . .test For digits, signs And brackets
    while true do
    begin
      case cCurrChar of
        //  Case "-":    NextChar
        '*':
          begin
            NextChar;
            Result := Result * DigitsToValue;
          end;
        '/':
          begin
            NextChar;
            Denominator := DigitsToValue;
            if Denominator <> 0 then
              Result := Result / Denominator
            else
            begin
              fCalcResult := false;
              raise Exception.CreateRes(@RsEDivideBy);
            end;
          end;
      else //case else
        break;
      end; //END CASE
    end;

  end;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  function Expression: single;
  begin
    Result := TestFor_MulDiv; //...test for '*' and '/'
    while true do
      case cCurrChar of //...TestFor_AddSub
        ' ': NextChar;
        '+':
          begin
            NextChar;
            if cCurrChar in ['+', '-', '/', '*'] then
              raise Exception.CreateResFmt(@RsEDuplicateSignsAtPos , [ExpressionPtr - 1]);
            Result := Result + TestFor_MulDiv;
          end;
        '-':
          begin
            NextChar;
            if cCurrChar in ['+', '-', '/', '*'] then
              raise Exception.CreateResFmt(@RsEDuplicateSignsAtPos, [ExpressionPtr - 1]);
            Result := Result - TestFor_MulDiv;
          end;
      else
        break;
      end;
  end;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
begin //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~MAIN PROC

  ExpressionPtr := 0;
  BracketsCount := 0;
  //  Calculate = FALSE;
  sExpression := trim(sExpression);

  ExpressionLength := Length(sExpression);
  if ExpressionLength = 0 then
    raise Exception.CreateRes(@RsEExpressionStringIsEmpty);
  fCalcResult := true;
  NextChar;
  Result := Expression;
  //  CalcResult = Value;
  //  fCalculate = fCalcResult

end;

// Тренарный оператор   x ? y : z
{ Ternary operator: x? y : z [translated] }

function IIF(fExpression: boolean; IfTrue, IfFalse: variant): variant; overload;
begin
  if fExpression then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function IIF(fExpression: boolean; const IfTrue, IfFalse: string): string; overload;
begin
  if fExpression then
    Result := IfTrue
  else
    Result := IfFalse;
end;

// Возвращает цвет левой/правой нижней/верхней точки битмапа
{ Returns colour of Leftmost/Rightmost Top/Bottom pixel of bitmap [translated] }

function GetTransparentColor(Bitmap: TBitmap; AutoTrColor: TglAutoTransparentColor): TColor;
var
  x, y: integer;
begin
  if (AutoTrColor = ftcUser) or (not IsItAFilledBitmap(Bitmap)) then
    Result := 0
  else
  begin
    case AutoTrColor of
      ftcLeftTopPixel:
        begin
          x := 0;
          y := 0;
        end;
      ftcLeftBottomPixel:
        begin
          x := 0;
          y := Bitmap.Height - 1;
        end;
      ftcRightTopPixel:
        begin
          x := Bitmap.Width - 1;
          y := 0;
        end;
    else {ftcRightBottomPixel}
      begin
        x := Bitmap.Width - 1;
        y := Bitmap.Height - 1;
      end;
    end;
    Result := GetPixel(Bitmap.Canvas.Handle, x, y);
  end;
end;

procedure TypeStringOnKeyboard(S: string);
var
  i: integer;
  vk: byte;
begin
  for i := 1 to length(S) do
  begin
    if ord(S[i]) > 32 then
      vk := ord(S[i]) - 32
    else
      vk := ord(S[i]);
    keybd_event(vk, 0, 0, 0);
    keybd_event(vk, 0, KEYEVENTF_KEYUP, 0);
  end;
end;

{function NextStringGridCell( Grid: TStringGrid ): boolean;
var
  R: TRect;
  i: integer;
begin
  with Grid do
  begin
    if Cols[Selection.Left][Selection.Top]='' then
    begin Result := true; exit; end;
    Result := not ((Grid.Selection.Top = RowCount-1)and(Grid.Selection.Left =
    if Result then
    if Selection.Top = RowCount-1 then
    begin
      Perform( wM_KEYDOWN, VK_TAB, 1);
      for i:=1 to RowCount-FixedRows-1 do Perform( wM_KEYDOWN, VK_UP, 1);
    end
    else
    begin Perform( wM_KEYDOWN, VK_DOWN, 1); end;
//    Grid.SetFocus;
     Grid.EditorMode:=false;
     Grid.EditorMode:=true;
  end;
end;
}

procedure DrawTextExtAligned(Canvas: TCanvas; const Text: string; R: TRect; Alignment: TglAlignment; WordWrap: boolean);
const
  Alignments: array[TglAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER, 0);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DrawPos, Pos1, Pos2, LineWidth,
    LineNo, LexemCount, TextHeight: integer;
  Width: integer;
  Lexem: string;
  Size: TSIZE;
  fStop, fBroadwiseLine: boolean;

  function GetNextLexem(var Pos1, Pos2: integer; fTrimleft: boolean): string;
  var
    Pos: integer;
  begin
    pos := pos1;
    if Text[Pos] = ' ' then
      repeat inc(Pos);
      until (Pos > length(Text)) or (Text[Pos] <> ' ');
    Pos2 := Pos;
    if fTrimleft and (LineNo > 0) then
      Pos1 := Pos;
    repeat inc(Pos2);
    until (Pos2 > length(Text)) or (Text[Pos2] = ' ');

    Result := copy(Text, Pos1, Pos2 - Pos1);
  end;

  procedure DrawLine(AdditSpace: cardinal);
  var
    i, DrawPos1, DrawPos2: integer;
    Lexem: string;
    Size: TSIZE;
    X, X_: single;
  begin
    DrawPos1 := DrawPos;
    DrawPos2 := DrawPos;
    X := 0;
    X_ := 0;
    LineWidth := 0;
    for i := 1 to LexemCount do
    begin
      Lexem := GetNextLexem(DrawPos1, DrawPos2, i = 1);
      //      if LexemCount=1 then Lexem:=Lexem+' ';
      GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), length(Lexem), Size);
      inc(LineWidth, trunc(X));
      X := X + Size.cx;
      if (trunc(X) > Width) and (LexemCount > 1) then
        exit;

      if (LexemCount > 1) and fBroadwiseLine then
        X := X + AdditSpace / (LexemCount - 1);
      TextOut(Canvas.Handle, R.Left + trunc(X_), R.Top + LineNo * TextHeight, PChar(Lexem), length(Lexem));
      X_ := X;
      DrawPos1 := DrawPos2;
    end;
  end;
begin
  if Text = '' then
    exit;
  if (Alignment <> ftaBroadwise) then
  begin
    DrawText(Canvas.Handle, PChar(Text), Length(Text), R,
      DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment]);
    exit;
  end;
  Width := R.Right - R.Left; {Height := R.Bottom - R.Top;}
  LineWidth := 0;
  LineNo := 0;
  DrawPos := 1;
  Pos1 := 1;
  Pos2 := 1;
  LexemCount := 0;
  TextHeight := 0;
  fStop := false;
  fBroadwiseLine := true;
  repeat
    Lexem := GetNextLexem(Pos1, Pos2, LexemCount = 0);
    //    if LexemCount=0 then Lexem:=Lexem+' ';
    GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), length(Lexem), Size);
    inc(LineWidth, Size.cx);
    inc(LexemCount);
    if TextHeight < Size.cy then
      TextHeight := Size.cy;
    if (LineWidth > Width) or (Pos2 >= length(Text)) then
    begin
      if (LineWidth > Width) then
      begin
        if LexemCount = 1 then
          Pos1 := Pos2;
        if LexemCount > 1 then
          dec(LexemCount);
        DrawLine(Width - (LineWidth - Size.cx));
        DrawPos := Pos1;
        inc(LineNo);
        LexemCount := 0;
        LineWidth := 0;
        fStop := Pos1 > length(Text);
      end
      else
      begin
        fBroadwiseLine := false; //ftoBroadwiseLastLine;
        DrawLine(Width - LineWidth);
        inc(LineNo);
        fStop := true;
      end;
    end
    else
      Pos1 := Pos2;
  until fStop;
  //  if FAutoSize then Height := max( 12, LineNo*TextHeight );
end;

// Загружает компонент из текстового файла - десериализация
{ Deserialization: loading component from text file [translated] }

procedure LoadComponentFromTextFile(Component: TComponent; FileName: string);
var
  ms: TMemoryStream;
  fs: TFileStream;
begin
  ms := TMemoryStream.Create;
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    ObjectTextToBinary(fs, ms);
    ms.position := 0;
    ms.ReadComponent(Component);
  finally
    ms.Free;
    fs.free;
  end;
end;

// сериализует компонент в строку
{ Serializing component to string [translated] }

function ComponentToString(Component: TComponent): string;
var
  ms: TMemoryStream;
  ss: TStringStream;
begin
  ss := TStringStream.Create(' ');
  ms := TMemoryStream.Create;
  try
    ms.WriteComponent(Component);
    ms.position := 0;
    ObjectBinaryToText(ms, ss);
    ss.position := 0;
    Result := ss.DataString;
  finally
    ms.Free;
    ss.free;
  end;
end;

// Сохраняет компонент в текстовый файл - сериализация
{ Serialization: writing component to text file [translated] }

procedure SaveComponentToTextFile(Component: TComponent; FileName: string);
var
  ms: TMemoryStream;
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
  ms := TMemoryStream.Create;
  try
    ms.WriteComponent(Component);
    ms.position := 0;
    ObjectBinaryToText(ms, fs);
  finally
    ms.Free;
    fs.free;
  end;
end;

// десериализует строку в компонент
{ Deserializing component from string [translated] }

procedure StringToComponent(Component: TComponent; const Value: string);
var
  StrStream: TStringStream;
  ms: TMemoryStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    ms := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, ms);

      ms.position := 0;
      ms.ReadComponent(Component);

      //      Result := BinStream.ReadComponent(nil);
    finally
      ms.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

// Воспроизводит ресурс WAV из ресурсов
{ Plays WAV resource [translated] }

function PlayWaveResource(sResName: string): boolean;
var
  WaveHandle: THandle;
  WavePointer: pointer;
begin
  Result := false;
  WaveHandle := FindResource(hInstance, PChar(sResName), RT_RCDATA);
  if WaveHandle <> 0 then
  begin
    WaveHandle := LoadResource(hInstance, WaveHandle);
    if WaveHandle <> 0 then
    begin
      WavePointer := LockResource(WaveHandle);
      Result := sndPlaySound(WavePointer, snd_Memory or SND_ASYNC);
      UnlockResource(WaveHandle);
      FreeResource(WaveHandle);
    end;
  end;
end;

// Имя пользователя текущего потока
{ User name for current thread [translated] }

function UserName: string;
var
  un: array[0..32] of char;
  ul: DWORD;
begin
  ul := 32;
  GetUserName(un, ul);
  Result := un;
end;

// Имя компьютера
{ PC name [translated] }

function ComputerName: string;
var
  un: array[0..32] of char;
  ul: DWORD;
begin
  ul := 32;
  GetComputerName(un, ul);
  Result := un;
end;

// Создает ini файл с именем файла проекта - пользуйтесь ChangeFileExt
{ Creates ini-file with the same name to project's file - use ChangeFileExt [translated] }

function CreateIniFileName: string;
begin
  Result := ParamStr(0);
  SetLength(Result, length(Result) - length(ExtractFileExt(Result)));
  Result := Result + '.ini';
end;

// Расширяет строку пробелами до заданной длины
{ Expands string with spaces up to given length [translated] }

function ExpandString(const str: string; len: integer): string;
var
  i: integer;
begin
  Result := str;
  if Length(Result) >= len then
    exit;
  SetLength(Result, len);

  for i := 1 to length(Result) do
    if i <= Length(str) then
      Result[i] := Str[i]
    else
      Result[i] := ' ';
end;

// Транслитерация строки RusToLat и обратно
{ Translitarating string Rus<->Lat [translated] }

function Transliterate(const Str: string; RusToLat: boolean): string;
const
  LAT: string = 'ABVGDEGZIIKLMNOPRSTUFHC___"Y''EUYabvgdegziiklmnoprstufhc___"y''euy+';
  RUS: string = 'АБВГДЕЖЗИЙКЛМНОПРСТУФХЦШЩЪЫЬЭЮЯабвгдежзийклмнопрстуфхцшщъыьэюя+';
  LATRUS: array[1..52, 1..2] of char =
  (
    ('A', 'А'), ('B', 'Б'), ('C', 'Ц'), ('D', 'Д'), ('E', 'Е'),
    ('F', 'Ф'), ('G', 'Ж'), ('H', 'Х'), ('I', 'И'), ('J', 'Й'),
    ('K', 'К'), ('L', 'Л'), ('M', 'М'), ('N', 'Н'), ('O', 'О'),
    ('P', 'П'), ('Q', #0), ('R', 'Р'), ('S', 'С'), ('T', 'Т'),
    ('U', 'У'), ('V', 'В'), ('W', #0), ('X', #0), ('Y', 'Ы'), ('Z', 'З'),
    ('a', 'а'), ('b', 'б'), ('c', 'ц'), ('d', 'д'), ('e', 'е'),
    ('f', 'ф'), ('g', 'ж'), ('h', 'х'), ('i', 'и'), ('j', 'й'),
    ('k', 'к'), ('l', 'л'), ('m', 'м'), ('n', 'н'), ('o', 'о'),
    ('p', 'п'), ('q', #0), ('r', 'р'), ('s', 'с'), ('t', 'т'),
    ('u', 'у'), ('v', 'в'), ('w', #0), ('x', #0), ('y', 'ы'), ('z', 'з')
    );

  TRANS_PAIRCOUNT = 14;
  aTRANS_PAIR: array[1..TRANS_PAIRCOUNT, boolean] of string = (('х', 'kh'), ('ц', 'ts'), ('ч', 'ch'), ('ш', 'sh'), ('щ',
    'shch'), ('ю', 'iu'), ('я', 'ia'),
    ('Х', 'Kh'), ('Ц', 'Ts'), ('Ч', 'Сh'), ('Ш', 'Sh'), ('Щ', 'Shch'), ('Ю', 'Iu'), ('Я', 'Ia'));
var
  i, j: integer;
begin
  Result := Str;
  for i := 1 to TRANS_PAIRCOUNT do
    Result := StringReplace(Result, aTRANS_PAIR[i, not RusToLat], aTRANS_PAIR[i, RusToLat], [rfReplaceAll]);

  if RusToLat then
  begin
    for i := 1 to length(Result) do
      if Result[i] in ['А'..'я'] then
        Result[i] := LAT[ord(Result[i]) - ord('А') + 1]; // else
  end
  else
    for i := 1 to length(Result) do
      if Result[i] in ['A'..'z'] then
        for j := 1 to 52 do
          if Result[i] = LATRUS[j, 1] then
          begin
            Result[i] := LATRUS[j, 2];
            break;
          end;
end;

// Значение функции TRUE если мелкий шрифт
{ Function returns true, if font is small [translated] }

function IsSmallFonts: boolean;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := (GetDeviceCaps(DC, LOGPIXELSX) = 96);
  //{ В случае крупного шрифта будет 120}
  { For large font it would be 120 [translated] }
  ReleaseDC(0, DC);
end;

//{ Глуюина цвета в системе, бит (8, 16 или 32) }
{ Color depth in system: 8, 16 or 32 bits [translated] }

function SystemColorDepth: integer;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetDeviceCaps(DC, BITSPIXEL);
  ReleaseDC(0, DC);
end;

function GetFileType(const FileName: string): TglFileType;
var
  Ext: string;
  i: integer;
const
  aExt: array[0..3] of string = ('.gif', '.jpeg', '.jpg', '.bmp');
  aType: array[0..4] of TglFileType = (fftGif, fftJpeg, fftJpeg, fftBmp, fftUndefined);
begin
  Result := fftUndefined;
  Ext := ExtractFileExt(FileName);
  for i := 0 to 3 do
    if CompareText(Ext, aExt[i]) = 0 then
    begin
      Result := aType[i];
      break;
    end;
end;

//{ Ищет верхний контрол в заданной точке }
{ Looks for upper(topmost) control at given point [translated] }

function FindControlAtPt(Control: TWinControl; pt: TPoint; MinClass: TClass): TControl;
var
  i: integer;
begin
  Result := nil;
  for i := Control.ControlCount - 1 downto 0 do
    if (Control.Controls[i] is MinClass) and PtInRect(Control.Controls[i].BoundsRect, pt) then
    begin
      Result := Control.Controls[i];
      break;
    end;
end;

//  StrPosExt - ищет позицию одной строки в другой с заданной длиной.
//  На длинных строках превосходит StrPos на 1-2 порядка.
{ StrPosExt - Looks for position of one string inside another with given length
  Outperforms StrPos on long strings in 10-100 times (1-2 orders) [translated] }

function StrPosExt(const Str1, Str2: PChar; Str2Len: DWORD): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        OR      EAX,EAX         // Str1
        JE      @@2             // если строка Str1 пуста - на выход;
                                // If Str1 is empty - get out [translated]
        OR      EDX,EDX         // Str2
        JE      @@2             // если строка Str2 пуста - на выход;
                                // If Str2 is empty - get out [translated]
        MOV     EBX,EAX
        MOV     EDI,EDX         // установим смещение для SCASB - подстрока Str2;
                                // Setting offset for SCASB - substring Str2 [translated]
        XOR     AL,AL           // обнулим AL; Zero AL [translated]

        push ECX                // длина строки; String length [translated]

        MOV     ECX,0FFFFFFFFH  // счетчик с запасом; Counter value, large enough
                                // to be assured it will never underflow [translated]
        REPNE   SCASB           // ищем конец подстроки Str2;
                                // Searching for end of Str2 substring [translated]
        NOT     ECX             // инвертируем ECX - получаем длину строки+1;
                                // Inverting ECX - getting string length +1 [translated]
        DEC     ECX             // в ECX - длина искомой подстроки Str2;
                                // And here is exact length [translated]

        JE      @@2             // при нулевой длине - все на выход; length = 0? get out! [translated]
        MOV     ESI,ECX         // сохраняем длину подстроки в ESI;
                                // Saving substring length in ESI [translated]

        pop ECX

        SUB     ECX,ESI         // ECX == разница длин строк : Str1 - Str2;
                                // ECX := length(Str1) - length(Str2) [translated]
        JBE     @@2             // если длина подсроки больше длине строки - выход;
                                // length(substring) > length(containing string) ? get out! [translated]
        MOV     EDI,EBX         // EDI  - начало строки Str1; EDI points to the beginning od Str1 [translated]
        LEA     EBX,[ESI-1]     // EBX - длина сравнения строк; EBX - length of comparision of strings  [translated]
@@1:    MOV     ESI,EDX         // ESI - смещение строки Str2; ESI - offset of Str2 string [translated]
        LODSB                   // загужаем первый символ подстроки в AL;
                                // Loading 1st byte of substring into AL [translated]
        REPNE   SCASB           // ищем этот символ в строке EDI;
                                // Searching that very char in EDI string [translated]
        JNE     @@2             // если символ не обнаружен - на выход;
                                // Char not found? get out! [translated]
        MOV     EAX,ECX         // сохраним разницу длин строк;
                                // Saving difference of lengths of strings [translated]
        PUSH    EDI             // запомним текущее смещение поиска;
                                // Saving current offset of search [translated]
        MOV     ECX,EBX
        REPE    CMPSB           // побайтно сравниваем строки;
                                // per-byte comparision of strings [translated]
        POP     EDI
        MOV     ECX,EAX
        JNE     @@1             // если строки различны - ищем следующее совпадение первого символа;
                                // If strings do not match - searching for 1st substring's char again [translated]
        LEA     EAX,[EDI-1]
        JMP     @@3
@@2:    XOR     EAX,EAX
@@3:    POP     EBX
        POP     ESI
        POP     EDI
end;

end.

