{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgUtils.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
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

unit JvgUtils;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Graphics, ExtCtrls,
  SysUtils, Classes, Controls, Forms, MMSystem,
  JvgTypes, JvgCommClasses, Jvg3DColors;

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

function IsEven(I: Integer): Boolean;
function InchesToPixels(DC: HDC; Value: Single; IsHorizontal: Boolean): Integer;
function CentimetersToPixels(DC: HDC; Value: Single; IsHorizontal: Boolean): Integer;

procedure SwapInt(var I1, I2: Integer);
function Spaces(Count: Integer): string;
function DupStr(const Str: string; Count: Integer): string;
function DupChar(C: Char; Count: Integer): string;
procedure Msg(AMsg: string);
function RectW(R: TRect): Integer;
function RectH(R: TRect): Integer;
function IncColor(AColor: Longint; AOffset: Byte): Longint;
function DecColor(AColor: Longint; AOffset: Byte): Longint;
function IsItAFilledBitmap(Bmp: TBitmap): Boolean;
procedure DrawTextInRectWithAlign(DC: HDC; R: TRect; Text: string;
  HAlign: TglHorAlign; VAlign: TglVertAlign;
  Style: TglTextStyle; Fnt: TFont; Flags: UINT);

procedure DrawTextInRect(DC: HDC; R: TRect; Text: string;
  Style: TglTextStyle; Fnt: TFont; Flags: UINT);

procedure ExtTextOutExt(DC: HDC; X, Y: Integer; R: TRect; Text: string;
  Style: TglTextStyle; ADelineated, ASupress3D: Boolean;
  FontColor, DelinColor, HighlightColor, ShadowColor: TColor;
  Illumination: TJvgIllumination; Gradient: TJvgGradient; Font: TFont);

procedure DrawBox(DC: HDC; var R: TRect; Style: TglBoxStyle;
  BackgrColor: Longint; ATransparent: Boolean);

function DrawBoxEx(DC: HDC; ARect: TRect; Borders: TglSides;
  BevelInner, BevelOuter: TPanelBevel; Bold: Boolean; BackgrColor: Longint;
  ATransparent: Boolean): TRect;

procedure GradientBox(DC: HDC; R: TRect; Gradient: TJvgGradient;
  PenStyle, PenWidth: Integer);

procedure ChangeBitmapColor(Bitmap: TBitmap; FromColor, ToColor: TColor);

procedure DrawBitmapExt(DC: HDC; { DC - background & result}
  SourceBitmap: TBitmap; R: TRect;
  X, Y: Integer; //...X,Y _in_ rect!
  BitmapOption: TglWallpaperOption; DrawState: TglDrawState;
  ATransparent: Boolean; TransparentColor: TColor; DisabledMaskColor: TColor);

procedure CreateBitmapExt(DC: HDC; { DC - background & result}
  SourceBitmap: TBitmap; R: TRect;
  X, Y: Integer; //...X,Y _in_ rect!
  BitmapOption: TglWallpaperOption; DrawState: TglDrawState;
  ATransparent: Boolean; TransparentColor: TColor; DisabledMaskColor: TColor);

procedure BringParentWindowToTop(Wnd: TWinControl);
function GetParentForm(Control: TControl): TForm;
procedure GetWindowImageFrom(Control: TWinControl; X, Y: Integer; ADrawSelf, ADrawChildWindows: Boolean; DC: HDC);
procedure GetWindowImage(Control: TWinControl; ADrawSelf, ADrawChildWindows: Boolean; DC: HDC);
procedure GetParentImageRect(Control: TControl; Rect: TRect; DC: HDC);
function CreateRotatedFont(F: TFont; Escapement: Integer): HFONT;
function FindMainWindow(AWndClass, AWndTitle: string): HWND;
procedure CalcShadowAndHighlightColors(BaseColor: TColor; Colors: TJvgLabelColors);

function CalcMathString(AExpression: string): Single;

function IIF(AExpression: Boolean; IfTrue, IfFalse: Variant): Variant; overload;
function IIF(AExpression: Boolean; const IfTrue, IfFalse: string): string; overload;

function GetTransparentColor(Bitmap: TBitmap; AutoTrColor: TglAutoTransparentColor): TColor;
procedure TypeStringOnKeyboard(S: string);
//function NextStringGridCell( Grid: TStringGrid ): Boolean;
procedure DrawTextExtAligned(Canvas: TCanvas; const Text: string; R: TRect; Alignment: TglAlignment; WordWrap: Boolean);
procedure LoadComponentFromTextFile(Component: TComponent; FileName: string);
procedure SaveComponentToTextFile(Component: TComponent; FileName: string);
function ComponentToString(Component: TComponent): string;
procedure StringToComponent(Component: TComponent; const Value: string);
function PlayWaveResource(ResName: string): Boolean;
function UserName: string;
function ComputerName: string;
function CreateIniFileName: string;
function ExpandString(const Str: string; Len: Integer): string;
function Transliterate(const Str: string; RusToLat: Boolean): string;
function IsSmallFonts: Boolean;
function SystemColorDepth: Integer;
function GetFileType(const FileName: string): TglFileType;
function FindControlAtPt(Control: TWinControl; Pt: TPoint; MinClass: TClass): TControl;
function StrPosExt(const Str1, Str2: PChar; Str2Len: DWORD): PChar; assembler;

{$IFDEF glDEBUG}
function DeleteObject(P1: HGDIOBJ): BOOL; stdcall;
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

function IsEven(I: Integer): Boolean;
begin
  Result := not Odd(I);
end;

procedure SwapInt(var I1, I2: Integer);
var
  Tmp: Integer;
begin
  Tmp := I1;
  I1 := I2;
  I2 := Tmp;
end;

function Spaces(Count: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Count do
    Result := Result + ' ';
end;

function DupChar(C: Char; Count: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Count do
    Result := Result + C;
end;

function DupStr(const Str: string; Count: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Count do
    Result := Result + Str;
end;

{ Modal window with (i) icon and single OK button }

procedure Msg(AMsg: string);
begin
  MessageBox(GetForegroundWindow, PChar(AMsg), '',
    MB_APPLMODAL or MB_ICONINFORMATION or MB_OK);
end;

{ Checks if point is inside rect. Rect's borders are not part of rect }
{ // (andreas) make Delphi 5 compiler happy
function IsPointInRect(P: TPoint; R: TRect): Boolean;
begin
  Result := PtInRect(R, P);
//  Result := (P.X > R.Left) and (P.X < R.Right) and (P.Y > R.Top) and (P.Y < R.Bottom);
end;
}

{ Rect's width }

function RectW(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

{ Rect's height }

function RectH(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

{ Increases components of the colour with given offset }

function IncColor(AColor: Longint; AOffset: Byte): Longint;
var
  R, G, B: Byte;
begin
  if AColor < 0 then
    AColor := GetSysColor(AColor and $FF);
  R := Min(255, GetRValue(AColor) + AOffset);
  G := Min(255, GetGValue(AColor) + AOffset);
  B := Min(255, GetBValue(AColor) + AOffset);
  Result := RGB(R, G, B);
end;

{ Decreases components of the colour with given offset }

function DecColor(AColor: Longint; AOffset: Byte): Longint;
var
  R, G, B: Byte;
begin
  if AColor < 0 then
    AColor := GetSysColor(AColor and $FF);
  R := Max(0, GetRValue(AColor) - AOffset);
  G := Max(0, GetGValue(AColor) - AOffset);
  B := Max(0, GetBValue(AColor) - AOffset);
  Result := RGB(R, G, B);
end;

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

{ Checks wheter bitmap object is created and is having size }

function IsItAFilledBitmap(Bmp: TBitmap): Boolean;
begin
  with Bmp do
    Result := Assigned(Bmp) and (Width <> 0) and (Height <> 0);
end;


{
  Renders text wth alignment, given style and given font

  DC             - Handle of canvas
  HAlign, VAlign - Alingment horizontal and vertical
  Style          - Style (embossed, with shadow, etc)
  Flags          - Extra parameters for Windows.DrawText
}

procedure DrawTextInRectWithAlign(DC: HDC; R: TRect; Text: string;
  HAlign: TglHorAlign; VAlign: TglVertAlign;
  Style: TglTextStyle; Fnt: TFont; Flags: UINT);
begin
  case HAlign of
    fhaLeft:
      Flags := Flags or DT_LEFT;
    fhaCenter:
      Flags := Flags or DT_CENTER;
    fhaRight:
      Flags := Flags or DT_RIGHT;
  end;
  case VAlign of
    fvaTop:
      Flags := Flags or DT_TOP;
    fvaCenter:
      Flags := Flags or DT_VCENTER;
    fvaBottom:
      Flags := Flags or DT_BOTTOM;
  end;

  DrawTextInRect(DC, R, Text, Style, Fnt, Flags);
end;


{
  Renders text with alignment, given style and given font

  DC             - Handle of canvas
  Style          - Style (embossed, with shadow, etc)
  Flags          - Extra parameters for Windows.DrawText
}

procedure DrawTextInRect(DC: HDC; R: TRect; Text: string; Style: TglTextStyle; Fnt: TFont; Flags: UINT);
var
  OldBkMode: Integer;
  OldFont: Windows.HFONT;
  FontColor: TColor;
  ShadowColor, HighlightColor: TColor;
begin
  if not Assigned(Fnt) then
    Exit;
  if Flags = 0 then
    Flags := DT_LEFT or DT_VCENTER or DT_SINGLELINE;
  OldBkMode := SetBkMode(DC, Ord(Transparent));
  FontColor := Fnt.Color;

  ShadowColor := clBtnShadow;
  HighlightColor := clBtnHighlight;

  OldFont := SelectObject(DC, Fnt.Handle);
  case Style of
    fstRaised:
      begin
        SetTextColor(DC, ColorToRGB(HighlightColor));
        OffsetRect(R, -1, -1);
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
        SetTextColor(DC, ColorToRGB(ShadowColor));
        OffsetRect(R, 2, 2);
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
        SetTextColor(DC, ColorToRGB(FontColor));
        OffsetRect(R, -1, -1);
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
      end;
    fstRecessed:
      begin
        SetTextColor(DC, ColorToRGB(ShadowColor));
        OffsetRect(R, -1, -1);
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
        SetTextColor(DC, ColorToRGB(HighlightColor));
        OffsetRect(R, 2, 2);
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
        SetTextColor(DC, ColorToRGB(FontColor));
        OffsetRect(R, -1, -1);
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
      end;
    fstPushed:
      begin
        SetTextColor(DC, ColorToRGB(HighlightColor));
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
        SetTextColor(DC, ColorToRGB(ShadowColor));
        OffsetRect(R, -1, -1);
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
      end;
    fstShadow:
      begin
        SetTextColor(DC, ColorToRGB(ShadowColor));
        OffsetRect(R, 2, 2);
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
        SetTextColor(DC, ColorToRGB(FontColor));
        OffsetRect(R, -2, -2);
        DrawText(DC, PChar(Text), Length(Text), R, Flags);
      end;
  else
    begin
      SetTextColor(DC, ColorToRGB(FontColor));
      DrawText(DC, PChar(Text), Length(Text), R, Flags);
    end;
  end;
  SelectObject(DC, OldFont);
  SetBkMode(DC, OldBkMode);
end;

{
  Renders text wth given style, countouring option and given colours fo 3D effects

  DC             - Handle of canvas
  Style          - Style (embossed, with shadow, etc)
  ADelineated    - Contour of color of DelinColor
  FontColor, DelinColor, HighlightColor, ShadowColor -
                   Colors of font and 3D effects
  Illumination   - Not used
  Gradient       - Gradient for filling letters of text
}

procedure ExtTextOutExt(DC: HDC; X, Y: Integer; R: TRect; Text: string;
  Style: TglTextStyle; ADelineated, ASupress3D: Boolean;
  FontColor, DelinColor, HighlightColor, ShadowColor: TColor;
  Illumination: TJvgIllumination; Gradient: TJvgGradient; Font: TFont);
var
  OldBkMode, X1, Y1, I, ShadowDepth: Integer;
  OldFont: Windows.HFONT;

  procedure DrawMain(ADelineated: Boolean; S: Integer);
  begin
    if ADelineated then
    begin
      if not ASupress3D then
      begin
        SetTextColor(DC, ColorToRGB(DelinColor));
        ExtTextOut(DC, X + S, Y + S, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
        ExtTextOut(DC, X + 2 + S, Y + 2 + S, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
        ExtTextOut(DC, X + S, Y + S + 2, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
        ExtTextOut(DC, X + S + 2, Y + S, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
      end;
      SetTextColor(DC, ColorToRGB(FontColor));
      if Assigned(Gradient) then
        Gradient.TextOut(DC, Text, R, X + S + 1, Y + S + 1)
      else
        ExtTextOut(DC, X + S + 1, Y + S + 1, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
    end
    else
    begin
      SetTextColor(DC, ColorToRGB(FontColor));
      if Assigned(Gradient) then
        Gradient.TextOut(DC, Text, R, X + S, Y + S)
      else
        ExtTextOut(DC, X + S, Y + S, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
    end;
  end;
begin
  if not Assigned(Font) then
    Exit;
  OldFont := SelectObject(DC, Font.Handle);
  OldBkMode := SetBkMode(DC, TRANSPARENT);

  if ADelineated then
  begin
    X1 := 4;
    Y1 := 4;
  end
  else
  begin
    X1 := 2;
    Y1 := 2;
  end;
  if Style = fstNone then
  begin
    X1 := X1 div 2 - 1;
    Y1 := Y1 div 2 - 1;
  end;
  if Style = fstShadow then
  begin
    X1 := X1 div 2 - 1;
    Y1 := Y1 div 2 - 1;
  end;
  if Assigned(Illumination) then
    ShadowDepth := Illumination.ShadowDepth
  else
    ShadowDepth := 2;
  case Style of
    fstRaised:
      begin
        if not ASupress3D then
        begin
          SetTextColor(DC, ColorToRGB(HighlightColor));
          ExtTextOut(DC, X, Y, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
          SetTextColor(DC, ColorToRGB(ShadowColor));
          ExtTextOut(DC, X + X1, Y + Y1, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
        end;
        DrawMain(ADelineated, 1);
      end;
    fstRecessed:
      begin
        if not ASupress3D then
        begin
          SetTextColor(DC, ColorToRGB(ShadowColor));
          ExtTextOut(DC, X, Y, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
          SetTextColor(DC, ColorToRGB(HighlightColor));
          ExtTextOut(DC, X + X1, Y + Y1, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
        end;
        DrawMain(ADelineated, 1);
      end;
    fstPushed:
      begin
        SetTextColor(DC, ColorToRGB(HighlightColor));
        ExtTextOut(DC, X + 1, Y + 1, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
        SetTextColor(DC, ColorToRGB(ShadowColor));
        ExtTextOut(DC, X, Y, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
      end;
    fstShadow:
      begin
        if not ASupress3D then
        begin
          SetTextColor(DC, ColorToRGB(ShadowColor));
          ExtTextOut(DC, X + X1 + ShadowDepth, Y + Y1 + ShadowDepth, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
        end;
        DrawMain(ADelineated, 0);
      end;
    fstVolumetric:
      begin
        if not ASupress3D then
        begin
          SetTextColor(DC, ColorToRGB(ShadowColor));
          for I := 1 to ShadowDepth do
            ExtTextOut(DC, X + I, Y + I, ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
        end;
        DrawMain(ADelineated, 0);
      end;
  else
    DrawMain(ADelineated, 0);
    //    SetTextColor( DC , ColorToRGB(FontColor) );
    //    ExtTextOut( DC, X, Y,  ETO_CLIPPED, @R, PChar(Text), Length(Text), nil);
  end;
  SelectObject(DC, OldFont);
  SetBkMode(DC, OldBkMode);
end;

{
   Draws rect with given 3D style

   DC          - Handle of canvas
   Style       - Style (fbsFlat, fbsCtl3D, fbsStatusControl, fbsRecessed, fbsRaised, fbsRaisedFrame, fbsRecessedFrame)
   BackgrColor - Background Color if FTransparen is False
}


procedure DrawBox(DC: HDC; var R: TRect; Style: TglBoxStyle;
  BackgrColor: Longint; ATransparent: Boolean);
const
  FBorderWidth = 1;
begin

  case Style of
    fbsFlat:
      begin
      end;
    fbsCtl3D:
      begin
        R.Top := R.Top + 2;
        R.Left := R.Left + 2;
        R.Right := R.Right - 2;
        R.Bottom := R.Bottom - 1;
        //  Frame3D(Canvas, R,clBtnShadow,clBtnHighlight,1);
      end;
    //    fbsStatusControl:
    fbsRaised:
      begin
        InflateRect(R, -2, -2);
        DrawEdge(DC, R, BDR_RAISEDOUTER, BF_BOTTOMRIGHT); // black
        Dec(R.Bottom);
        Dec(R.Right);
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_TOPLEFT); // btnhilite
        Inc(R.Top);
        Inc(R.Left);
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_BOTTOMRIGHT or BF_MIDDLE); // btnshadow
      end;
    fbsRecessed:
      begin
        R.Bottom := R.Bottom - 1;
        DrawEdge(DC, R, BDR_SUNKENINNER, BF_TOPLEFT); // black
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_BOTTOMRIGHT); // btnhilite
        Dec(R.Bottom);
        Dec(R.Right);
        Inc(R.Top);
        Inc(R.Left);
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_TOPLEFT or BF_MIDDLE); // btnshadow
        Inc(R.Top);
        Inc(R.Left);
      end;
    fbsRaisedFrame:
      begin
        DrawEdge(DC, R, BDR_RAISEDOUTER, BF_BOTTOMRIGHT); // black
        Dec(R.Bottom);
        Dec(R.Right);
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_TOPLEFT); // btnhilite
        Inc(R.Top);
        Inc(R.Left);
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_BOTTOMRIGHT or BF_MIDDLE); // btnshadow

        InflateRect(R, -FBorderWidth, -FBorderWidth);

        DrawEdge(DC, R, BDR_SUNKENINNER, BF_TOPLEFT); // black
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_BOTTOMRIGHT); // btnhilite
        Dec(R.Bottom);
        Dec(R.Right);
        Inc(R.Top);
        Inc(R.Left);
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_TOPLEFT or BF_MIDDLE); // btnshadow
        Inc(R.Top);
        Inc(R.Left);
      end;
    fbsRecessedFrame:
      begin
        DrawEdge(DC, R, BDR_SUNKENINNER, BF_TOPLEFT); // black
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_BOTTOMRIGHT); // btnhilite
        Dec(R.Bottom);
        Dec(R.Right);
        Inc(R.Top);
        Inc(R.Left);
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_TOPLEFT or BF_MIDDLE); // btnshadow
        Inc(R.Top);
        Inc(R.Left);

        InflateRect(R, -FBorderWidth, -FBorderWidth);

        DrawEdge(DC, R, BDR_RAISEDOUTER, BF_BOTTOMRIGHT); // black
        Dec(R.Bottom);
        Dec(R.Right);
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_TOPLEFT); // btnhilite
        Inc(R.Top);
        Inc(R.Left);
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_BOTTOMRIGHT or BF_MIDDLE); // btnshadow
      end;
  end;
end;

{
  Draws rect with given 3D style and specifing borders

  DC          - Handle of canvas
  Borders     - Borders for drawing
  BevelInner, BevelOuter - Borders' styles
  Bold        - Bold border(frame)
  BackgrColor - Background Color if ATransparent is False
}

function DrawBoxEx(DC: HDC; ARect: TRect; Borders: TglSides;
  BevelInner, BevelOuter: TPanelBevel; Bold: Boolean; BackgrColor: Longint;
  ATransparent: Boolean): TRect;
var
  I: Word;
  BPen, LPen, SPen, OldPen: HPEN;
  HBackgrBrush, HOldBrush: HBRUSH;
  R, R1: TRect;
  BColor, HColor, SColor: Longint;
  LogOldPen: TLOGPEN;
  PenWidth: UINT;

  procedure SetDefColors;
  begin
    BColor := GetSysColor(COLOR_3DDKSHADOW);
    HColor := GetSysColor(COLOR_3DHILIGHT);
    SColor := GetSysColor(COLOR_3DSHADOW);
  end;

  procedure DrawBevel(Bevel: TPanelBevel);
  begin
    if fsdLeft in Borders then
    begin
      case Bevel of
        bvRaised:
          begin
            SelectObject(DC, LPen);
            MoveToEx(DC, R.Left, R.Top, nil);
            LineTo(DC, R.Left, R.Bottom + 1);
            Inc(R1.Left);
            //.if Bold then Inc(R1.Left);
          end;
        bvLowered:
          if Bold then
          begin
            SelectObject(DC, BPen);
            MoveToEx(DC, R.Left, R.Top, nil);
            LineTo(DC, R.Left, R.Bottom);
            Inc(R1.Left);
            SelectObject(DC, SPen);
            if fsdBottom in Borders then
              I := 0
            else
              I := 1;
            MoveToEx(DC, R.Left + 1, R.Top + 1, nil);
            LineTo(DC, R.Left + 1, R.Bottom + I);
            //SetPixel(DC, R.Left, R.Bottom-1, SColor);
            Inc(R1.Left);
          end
          else
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Left, R.Top, nil);
            LineTo(DC, R.Left, R.Bottom);
            Inc(R1.Left);
          end;
        bvSpace:
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Left, R.Top, nil);
            LineTo(DC, R.Left, R.Bottom);
            Inc(R1.Left);
          end;
      end;
    end;
    if fsdTop in Borders then
    begin
      case Bevel of
        bvRaised:
          begin
            SelectObject(DC, LPen);
            MoveToEx(DC, R.Left, R.Top, nil);
            LineTo(DC, R.Right, R.Top);
            Inc(R1.Top);
            //.if Bold then Inc(R1.Top);
          end;
        bvLowered:
          if Bold then
          begin
            SelectObject(DC, BPen);
            MoveToEx(DC, R.Left, R.Top, nil);
            LineTo(DC, R.Right, R.Top);
            Inc(R1.Top);
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Left + 1, R.Top + 1, nil);
            LineTo(DC, R.Right, R.Top + 1);
            //SetPixel(DC, R.Right-1, R.Top+1, SColor);
            Inc(R1.Top);
          end
          else
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Left, R.Top, nil);
            LineTo(DC, R.Right, R.Top);
            Inc(R1.Top);
          end;
        bvSpace:
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Left, R.Top, nil);
            LineTo(DC, R.Right, R.Top);
            Inc(R1.Top);
          end;
      end;
    end;
    if fsdRight in Borders then
    begin
      case Bevel of
        bvRaised:
          if Bold then
          begin
            SelectObject(DC, BPen);
            MoveToEx(DC, R.Right, R.Top, nil);
            LineTo(DC, R.Right, R.Bottom + 1);
            Dec(R1.Right);
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Right - 1, R.Top + 1, nil);
            LineTo(DC, R.Right - 1, R.Bottom + 1);
            //SetPixel(DC, R.Right-1, R.Bottom-1, SColor);
            Dec(R1.Right);
          end
          else
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Right, R.Top, nil);
            LineTo(DC, R.Right, R.Bottom + 1);
            Dec(R1.Right);
          end;
        bvLowered:
          begin
            SelectObject(DC, LPen);
            MoveToEx(DC, R.Right, R.Top, nil);
            LineTo(DC, R.Right, R.Bottom);
            Dec(R1.Right);
            //. if Bold then Dec(R1.Right);
          end;
        bvSpace:
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Right, R.Top, nil);
            LineTo(DC, R.Right, R.Bottom);
            Dec(R1.Right);
          end;
      end;
    end;
    if fsdBottom in Borders then
    begin
      case Bevel of
        bvRaised:
          if Bold then
          begin
            SelectObject(DC, BPen);
            if fsdLeft in Borders then
              I := 1
            else
              I := 0;
            MoveToEx(DC, R.Left {+1}, R.Bottom, nil);
            LineTo(DC, R.Right, R.Bottom);
            Dec(R1.Bottom);
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Left + I {+I}, R.Bottom - 1, nil);
            LineTo(DC, R.Right, R.Bottom - 1);
            //SetPixel(DC, R.Right-1+I, R.Bottom-1, SColor);
            Dec(R1.Bottom);
          end
          else
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Left, R.Bottom, nil);
            LineTo(DC, R.Right, R.Bottom);
            Dec(R1.Bottom);
          end;
        bvLowered:
          begin
            SelectObject(DC, LPen);
            //    if Borders.Left then I:=1 else I:=0;
            MoveToEx(DC, R.Left, R.Bottom {-1}, nil);
            LineTo(DC, R.Right + 1, R.Bottom {-1});
            Dec(R1.Bottom);
            //. if Bold then Dec(R1.Bottom);
            //Dec(R1.Bottom);
          end;
        bvSpace:
          begin
            SelectObject(DC, SPen);
            MoveToEx(DC, R.Left, R.Bottom {-1}, nil);
            LineTo(DC, R.Right + 1, R.Bottom {-1});
            Dec(R1.Bottom);
          end;
      end;
    end;
  end;

begin
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

  LPen := CreatePen(PS_SOLID, 1, HColor);
  OldPen := SelectObject(DC, LPen);
  DeleteObject(SelectObject(DC, OldPen));

  FillChar(LogOldPen, SizeOf(LogOldPen), 0);
  GetObject(OldPen, SizeOf(LogOldPen), @LogOldPen);
  if LogOldPen.lopnWidth.X = 0 then
    PenWidth := 1
  else
    PenWidth := LogOldPen.lopnWidth.X;
  BPen := CreatePen(LogOldPen.lopnStyle, PenWidth, BColor);
  LPen := CreatePen(LogOldPen.lopnStyle, PenWidth, HColor);
  SPen := CreatePen(LogOldPen.lopnStyle, PenWidth, SColor);
  SelectObject(DC, LPen);
  R1 := ARect;
  R := ARect;
  if BevelOuter <> bvNone then
    DrawBevel(BevelOuter);
  R := R1;
  //  if (BevelOuter = bvRaised)and(BevelInner = bvLowered)and Bold then
  //  begin Dec(R.Top); Dec(R.Left); end;

  if BevelInner <> bvNone then
    DrawBevel(BevelInner);

  SelectObject(DC, OldPen);
  DeleteObject(BPen);
  DeleteObject(LPen);
  DeleteObject(SPen);

  if not ATransparent then
  begin
    HBackgrBrush := CreateSolidBrush(ColorToRGB(BackgrColor));
    HOldBrush := SelectObject(DC, HBackgrBrush);
    R := R1; {Dec(R.Top);Dec(R.Left);}
    Inc(R.Right);
    Inc(R.Bottom);
    FillRect(DC, R, HBackgrBrush);
    DeleteObject(SelectObject(DC, HOldBrush));
  end;

  Result := R1;
end;

{ Draws TJvgGradient gradient }

procedure GradientBox(DC: HDC; R: TRect; Gradient: TJvgGradient; PenStyle, PenWidth: Integer);
begin
  Gradient.Draw(DC, R, PenStyle, PenWidth);
end;

{ Replaces bitmap's color }

procedure ChangeBitmapColor(Bitmap: TBitmap; FromColor, ToColor: TColor);
var
  IWidth, IHeight: Integer;
  DRect, SRect: TRect;
  MonoBMP, OldBMP: HBITMAP;
  MonoDC: HDC;
begin
  if (Bitmap.Width or Bitmap.Height) = 0 then
    Exit;
  IWidth := Bitmap.Width;
  IHeight := Bitmap.Height;
  DRect := Rect(0, 0, IWidth, IHeight);
  SRect := DRect;

  MonoDC := CreateCompatibleDC(Bitmap.Canvas.Handle);
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
      BitBlt(Handle, 0, 0, IWidth, IHeight, MonoDC, 0, 0, ROP_DSPDxax);
    end;
  finally
    DeleteObject(SelectObject(MonoDC, OldBMP));
    DeleteDC(MonoDC);
  end;
end;

{ Paints bitmap. Transparent, disabled, multiplied, etc }

procedure DrawBitmapExt(DC: HDC; { DC - background & result}
  SourceBitmap: TBitmap; R: TRect;
  X, Y: Integer; //...X,Y _in_ rect!
  BitmapOption: TglWallpaperOption; DrawState: TglDrawState;
  ATransparent: Boolean; TransparentColor: TColor; DisabledMaskColor: TColor);
begin
  CreateBitmapExt(DC, SourceBitmap, R, X, Y, BitmapOption,
    DrawState, ATransparent, TransparentColor, DisabledMaskColor);
end;

//..DrawBitmap algorithm borrow from Delphi2 VCL Sources
{ create bimap based on SourceBitmap and write new bitmap to DC }

procedure CreateBitmapExt(DC: HDC; {target DC}
  SourceBitmap: TBitmap; R: TRect;
  X, Y: Integer; //...X,Y _in_ rect!
  BitmapOption: TglWallpaperOption; DrawState: TglDrawState;
  ATransparent: Boolean; TransparentColor: TColor; DisabledMaskColor: TColor);
const
  ROP_DSPDxax = $00E20746;
var
  X1, Y1, H, W: Integer;
  D, D1: Double;
  TmpImage, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  //  DestDC: HDC;
  BmpInfo: Windows.TBitmap;
  PtSize, PtOrg: TPoint;
  MemDC, ImageDC: HDC;
  OldBMP, OldMonoBMP, OldScreenImageBMP, OldMemBMP: HBITMAP;
  HMonoBMP, ScreenImageBMP, MemBMP: HBITMAP;
  MonoDC, ScreenImageDC: HDC;
  OldBkColor: COLORREF;
  SavedIHeight: Integer;

  procedure BitBltWorks;
  begin
    if ATransparent then
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
      BitBlt(ScreenImageDC, 0, 0, IWidth, IHeight, DC, X1, Y1, SRCCOPY);
      { put monochrome mask }
      BitBlt(ScreenImageDC, 0, 0, IWidth, IHeight, MonoDC, 0, 0, SRCAND);
      BitBlt(MonoDC, 0, 0, IWidth, IHeight, MonoDC, 0, 0, NOTSRCCOPY);
      { put inverse monochrome mask }
      BitBlt(MemDC, 0, 0, IWidth, IHeight, MonoDC, 0, 0, SRCAND);
      { merge Screen screen image(MemDC) and Screen image(ScreenImageDC) }
      BitBlt(MemDC, 0, 0, IWidth, IHeight, ScreenImageDC, 0, 0, SRCPAINT);
      { to screen }
      //    DSTINVERT MERGEPAINT
      BitBlt(DC, X1, Y1, IWidth, IHeight, MemDC, 0, 0, SRCCOPY);
    end
    else
      BitBlt(DC, X1, Y1, IWidth, IHeight, ImageDC, 0, 0, SRCCOPY);
  end;

begin
  if (SourceBitmap.Width = 0) or (SourceBitmap.Height = 0) then
    Exit;

  X := X + R.Left;
  Y := Y + R.Top;
  X1 := X;
  Y1 := Y;
  OldBMP := 0;
  OldMemBMP := 0;
  OldMonoBMP := 0;
  OldScreenImageBMP := 0;
  MemDC := 0;
  ImageDC := 0;
  // MonoBMP := 0;
  // ScreenImageBMP := 0;
  // MemBMP := 0;
  MonoDC := 0;
  ScreenImageDC := 0;

  IWidth := SourceBitmap.Width; //Min( SourceBitmap.Width, R.Right-R.Left );
  IHeight := SourceBitmap.Height; //Min( SourceBitmap.Height, R.Bottom-R.Top );
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    ORect := Rect(0, 0, IWidth, IHeight);

    TmpImage.Canvas.Brush.Color := TransparentColor;
    TmpImage.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));

    case DrawState of
      fdsDefault:
        BitBlt(TmpImage.Canvas.Handle, 0, 0, IWidth, IHeight,
          SourceBitmap.Canvas.Handle, 0, 0, SRCCOPY);
      fdsDelicate:
        begin
          with TmpImage.Canvas do
            BitBlt(Handle, 0, 0, IWidth, IHeight,
              SourceBitmap.Canvas.Handle, 0, 0, SRCCOPY);
          { Convert white to clBtnHighlight }
          ChangeBitmapColor(TmpImage, clWhite, clBtnHighlight);
          { Convert gray to clBtnShadow }
          ChangeBitmapColor(TmpImage, clGray, clBtnShadow);
          { Convert transparent color to clBtnFace }
          //     ChangeBitmapColor(TmpImage,ColorToRGB(}TransparentColor),clBtnFace);
        end;
      fdsDisabled:
        begin
          if DisabledMaskColor <> 0 then
            ChangeBitmapColor(TmpImage, DisabledMaskColor, clBlack);
          MonoBmp := TBitmap.Create;
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
    end;

    with TmpImage.Canvas do
      if (BitmapOption = fwoStretch) or (BitmapOption = fwoPropStretch) then
      begin
        MemDC := CreateCompatibleDC(DC);
        MemBMP := CreateCompatibleBitmap(TmpImage.Canvas.Handle, R.Right - R.Left, R.Bottom - R.Top);
        OldMemBMP := SelectObject(MemDC, MemBMP);
        W := R.Right - R.Left;
        H := R.Bottom - R.Top;
        if BitmapOption = fwoPropStretch then
        begin
          D1 := W / IWidth;
          D := H / IHeight;
          if D > D1 then
            D := D1; //...D == Min
          W := Trunc(IWidth * D);
          H := Trunc(IHeight * D);
        end;
        StretchBlt(MemDC, 0, 0, W, H, Handle, 0, 0, IWidth, IHeight, SRCCOPY);

        IWidth := W;
        IHeight := H;
        TmpImage.Width := W;
        TmpImage.Height := H;
        BitBlt(Handle, 0, 0, IWidth, IHeight, MemDC, 0, 0, SRCCOPY);

        DeleteObject(SelectObject(MemDC, OldMemBMP));
        DeleteDC(MemDC);
      end;

    ImageDC := CreateCompatibleDC(DC);

    if ATransparent then
    begin
      MemDC := CreateCompatibleDC(DC);
      ScreenImageDC := CreateCompatibleDC(DC);
      MonoDC := CreateCompatibleDC(DC);

      HMonoBMP := CreateBitmap(IWidth, IHeight, 1, 1, nil);
      ScreenImageBMP := CreateCompatibleBitmap(TmpImage.Canvas.Handle, IWidth, IHeight);
      MemBMP := CreateCompatibleBitmap(TmpImage.Canvas.Handle, IWidth, IHeight);

      OldMonoBMP := SelectObject(MonoDC, HMonoBMP);
      OldScreenImageBMP := SelectObject(ScreenImageDC, ScreenImageBMP);
      OldMemBMP := SelectObject(MemDC, MemBMP);
    end;
    OldBMP := SelectObject(ImageDC, TmpImage.Handle);

    if OldBMP <> 0 then
    begin
      SetMapMode(ImageDC, GetMapMode(DC));
      GetObject(TmpImage.Handle, SizeOf(Windows.TBitmap), @BmpInfo);
      PtSize.X := BmpInfo.bmWidth;
      PtOrg.X := 0;
      PtSize.Y := BmpInfo.bmHeight;
      PtOrg.Y := 0;
      if ATransparent then
      begin
        DPtoLP(DC, PtSize, 1);
        DPtoLP(MemDC, PtOrg.Y, 1);
      end;
      if BitmapOption = fwoTile then
      begin
        //SavedIWidth:=IWidth;
        SavedIHeight := IHeight;
        while X1 < R.Right do
        begin
          //IWidth:=SavedIWidth; //SavedIWidth:=IWidth;
          if X1 + IWidth > R.Right then
            IWidth := R.Right - X1;
          while Y1 < R.Bottom do
          begin
            IHeight := SavedIHeight; // SavedIHeight:=IHeight;
            if Y1 + IHeight > R.Bottom then
              IHeight := R.Bottom - Y1;
            BitBltWorks;
            Inc(Y1, IHeight);
          end;
          Inc(X1, IWidth);
          Y1 := Y;
        end;
      end
      else
        BitBltWorks;
    end;
  finally
    DeleteObject(SelectObject(ImageDC, OldBMP));
    DeleteDC(ImageDC);
    if ATransparent then
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

{ Brings parent window to front }

procedure BringParentWindowToTop(Wnd: TWinControl);
begin
  if Wnd is TForm then
    BringWindowToTop(Wnd.Handle)
  else
  if Wnd.Parent is TWinControl then
    BringParentWindowToTop(Wnd.Parent);
end;

{ Gives parent window of TForm class }

function GetParentForm(Control: TControl): TForm;
begin
  if Control is TForm then
    Result := TForm(Control)
  else
  if Control.Parent is TWinControl then
    Result := GetParentForm(Control.Parent)
  else
    Result := nil;
end;


{ Paints TWinControl with all its content onto DC with offset(shift) X,Y
  ...from rxLib... :( very sorry }

procedure GetWindowImageFrom(Control: TWinControl; X, Y: Integer; ADrawSelf, ADrawChildWindows: Boolean; DC: HDC);
var
  I, Count, SaveIndex: Integer;
begin
  if Control = nil then
    Exit;
  Count := Control.ControlCount;

  { Copy self image }
  if ADrawSelf then
  begin
    SaveIndex := SaveDC(DC);
    SetViewportOrgEx(DC, X, Y, nil);
    TJvgPublicWinControl(Control).PaintWindow(DC);
    RestoreDC(DC, SaveIndex);
  end;
  { Copy images of graphic controls }
  for I := 0 to Count - 1 do
  begin
    if Control.Controls[I] <> nil then
    begin
      if Control.Controls[I] = Control then
        Break;
      if (Control.Controls[I] is TWinControl) and ADrawChildWindows then
        GetWindowImageFrom(TWinControl(Control.Controls[I]),
          TWinControl(Control.Controls[I]).Left,
          TWinControl(Control.Controls[I]).Top,
          True {ADrawSelf}, ADrawChildWindows, DC)
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

{ Paints(renders) TWinControl with all its content onto DC with offset (0,0) }

procedure GetWindowImage(Control: TWinControl; ADrawSelf, ADrawChildWindows: Boolean; DC: HDC);
begin
  GetWindowImageFrom(Control, 0, 0, ADrawSelf, ADrawChildWindows, DC);
end;

{ Paints parent TWinControl with all its contents onto DC with limit of Rect }

procedure GetParentImageRect(Control: TControl; Rect: TRect; DC: HDC);
var
  I, Count, X, Y, SaveIndex: Integer;
  R, SelfR, CtlR: TRect;
begin
  if Control.Parent = nil then
    Exit;
  Count := Control.Parent.ControlCount;
  SelfR := Bounds(Control.Left, Control.Top, Control.Width, Control.Height);
  //  OffsetRect( Rect, Control.Left, Control.Top );
  IntersectRect(SelfR, SelfR, Rect);

  X := -Rect.Left;
  Y := -Rect.Top;
  { Copy parent control image }
  SaveIndex := SaveDC(DC);
  SetViewportOrgEx(DC, X, Y, nil);
  IntersectClipRect(DC, 0, 0, Rect.Right, Rect.Bottom);
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

{-create a rotated font based on the font object F}

function CreateRotatedFont(F: TFont; Escapement: Integer): HFONT;
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
    lfItalic := Ord(fsItalic in F.Style);
    lfUnderline := Ord(fsUnderline in F.Style);
    lfStrikeOut := Ord(fsStrikeOut in F.Style);
    lfCharSet := F.CHARSET;
    StrPCopy(lfFaceName, F.Name);
    lfQuality := DEFAULT_QUALITY;
    {everything else as default}
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case F.Pitch of
      fpVariable:
        lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed:
        lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Result := CreateFontIndirect(LF);
end;

{ Returns main window of application }

function FindMainWindow(AWndClass, AWndTitle: string): HWND;
begin
  Result := 0;
  if (AWndClass = '') and (AWndTitle = '') then
    Exit;
  Result := FindWindow(PChar(AWndClass), PChar(AWndTitle));
end;

{ Calculates colors of shadow and lighted border for given base color. }

procedure CalcShadowAndHighlightColors(BaseColor: TColor; Colors: TJvgLabelColors);
var
  R, G, B: Byte;
begin
  with Colors do
  begin
    if (BaseColor and $80000000) <> 0 then
      BaseColor := GetSysColor(BaseColor and $FF);
    B := (BaseColor and $00FF0000) shr 16;
    G := (BaseColor and $0000FF00) shr 8;
    R := BaseColor and $000000FF;
    if AutoShadow then
    begin
      {if R<G then limit:=R else limit:=G; if B<limit then limit:=B;//...Min
      if limit<FColorShadowShift then FColorShadowShift:=limit;
      FShadow := RGB(R-FColorShadowShift,G-FColorShadowShift,B-FColorShadowShift);}
      Shadow := RGB(Max(R - ColorShadowShift, 0), Max(G - ColorShadowShift, 0), Max(B - ColorShadowShift, 0));
    end;
    if AutoHighlight then
    begin
      {if R>G then limit:=R else limit:=G; if B>limit then limit:=B;//...Max
      if (255-limit)<FColorHighlightShift then FColorHighlightShift:=255-limit;
      FHighlight := RGB(R+FColorHighlightShift,G+FColorHighlightShift,B+FColorHighlightShift);}
      Highlight := RGB(Min(R + ColorHighlightShift, 255), Min(G + ColorHighlightShift, 255), Min(B +
        ColorHighlightShift, 255));
    end;
  end;
end;

{ Calculates arithmetic expression, given in string }

function CalcMathString(AExpression: string): Single;
var
  ExpressionPtr, ExpressionLength, BracketsCount: Integer;
  CalcResult: Boolean;
  CurrChar: Char;

  function Expression: Single; forward;

  procedure NextChar;
  begin
    Inc(ExpressionPtr);
    if ExpressionPtr <= ExpressionLength then
      CurrChar := AExpression[ExpressionPtr]
    else
      CurrChar := #0;
    if CurrChar = ' ' then
      NextChar;
    if CurrChar = #0 then
      Exit;
    if not (CurrChar in ['0'..'9', ',', '.', '-', '+', '/', '*', '(', ')']) then
      NextChar;
  end;

  function DigitsToValue: Single;
  var
    PointDepth: Integer;
    Point: Boolean;
  begin
    Result := 0;
    Point := False;
    PointDepth := 0;
    while CurrChar = ' ' do
      NextChar;

    if (CurrChar >= '0') and (CurrChar <= '9') then
    begin
      while (CurrChar >= '0') and (CurrChar <= '9') do
      begin
        Result := Result * 10 + Ord(CurrChar) - Ord('0');
        NextChar;
        if Point then
          Inc(PointDepth);
        if (CurrChar = '.') or (CurrChar = ',') then
        begin
          NextChar;
          Point := True;
        end;
      end;
      if PointDepth <> 0 then
        Result := Result / (10.0 * PointDepth);
    end
    else
    begin
      case CurrChar of
        '-':
          begin
            NextChar;
            Result := -1.0 * Result;
          end;
        '(':
          begin
            Inc(BracketsCount);
            NextChar;
            Result := Expression;
            while CurrChar = ' ' do
              NextChar;
            if CurrChar <> ')' then
            begin
              raise Exception.CreateRes(@RsERightBracketsNotFound);
              CalcResult := False;
              Result := 0;
            end
            else
              NextChar;
          end;
        // '.': Point := True;
        // ',': Point := True;
      end;
    end;
    if CurrChar = ')' then
    begin
      Dec(BracketsCount);
      if BracketsCount < 0 then
        raise Exception.CreateResFmt(@RsERightBracketHavntALeftOnePosd, [ExpressionPtr - 1]);
    end;
  end;

  function TestForMulDiv: Single;
  var
    Denominator: Single;
  begin
    Result := DigitsToValue; // . . .test For digits, signs And brackets
    while True do
    begin
      case CurrChar of
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
              CalcResult := False;
              raise Exception.CreateRes(@RsEDivideBy);
            end;
          end;
      else
        Break;
      end;
    end;
  end;

  function Expression: Single;
  begin
    Result := TestForMulDiv; //...test for '*' and '/'
    while True do
      case CurrChar of //...TestFor_AddSub
        ' ':
          NextChar;
        '+':
          begin
            NextChar;
            if CurrChar in ['+', '-', '/', '*'] then
              raise Exception.CreateResFmt(@RsEDuplicateSignsAtPos , [ExpressionPtr - 1]);
            Result := Result + TestForMulDiv;
          end;
        '-':
          begin
            NextChar;
            if CurrChar in ['+', '-', '/', '*'] then
              raise Exception.CreateResFmt(@RsEDuplicateSignsAtPos, [ExpressionPtr - 1]);
            Result := Result - TestForMulDiv;
          end;
      else
        Break;
      end;
  end;

begin
  ExpressionPtr := 0;
  BracketsCount := 0;
  AExpression := Trim(AExpression);

  ExpressionLength := Length(AExpression);
  if ExpressionLength = 0 then
    raise Exception.CreateRes(@RsEExpressionStringIsEmpty);
  CalcResult := True;
  NextChar;
  Result := Expression;
end;

{ Ternary operator: X ? Y : Z }

function IIF(AExpression: Boolean; IfTrue, IfFalse: Variant): Variant; overload;
begin
  if AExpression then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function IIF(AExpression: Boolean; const IfTrue, IfFalse: string): string; overload;
begin
  if AExpression then
    Result := IfTrue
  else
    Result := IfFalse;
end;

{ Returns colour of Leftmost/Rightmost Top/Bottom pixel of bitmap  }

function GetTransparentColor(Bitmap: TBitmap; AutoTrColor: TglAutoTransparentColor): TColor;
var
  X, Y: Integer;
begin
  if (AutoTrColor = ftcUser) or not IsItAFilledBitmap(Bitmap) then
    Result := 0
  else
  begin
    case AutoTrColor of
      ftcLeftTopPixel:
        begin
          X := 0;
          Y := 0;
        end;
      ftcLeftBottomPixel:
        begin
          X := 0;
          Y := Bitmap.Height - 1;
        end;
      ftcRightTopPixel:
        begin
          X := Bitmap.Width - 1;
          Y := 0;
        end;
    else {ftcRightBottomPixel}
      begin
        X := Bitmap.Width - 1;
        Y := Bitmap.Height - 1;
      end;
    end;
    Result := GetPixel(Bitmap.Canvas.Handle, X, Y);
  end;
end;

procedure TypeStringOnKeyboard(S: string);
var
  I: Integer;
  VK: Byte;
begin
  for I := 1 to Length(S) do
  begin
    if Ord(S[I]) > 32 then
      VK := Ord(S[I]) - 32
    else
      VK := Ord(S[I]);
    keybd_event(VK, 0, 0, 0);
    keybd_event(VK, 0, KEYEVENTF_KEYUP, 0);
  end;
end;

{function NextStringGridCell( Grid: TStringGrid ): Boolean;
var
  R: TRect;
  I: Integer;
begin
  with Grid do
  begin
    if Cols[Selection.Left][Selection.Top]='' then
    begin Result := True; Exit; end;
    Result := not ((Grid.Selection.Top = RowCount-1)and(Grid.Selection.Left =
    if Result then
    if Selection.Top = RowCount-1 then
    begin
      Perform( wM_KEYDOWN, VK_TAB, 1);
      for I:=1 to RowCount-FixedRows-1 do Perform( wM_KEYDOWN, VK_UP, 1);
    end
    else
    begin Perform( wM_KEYDOWN, VK_DOWN, 1); end;
//    Grid.SetFocus;
     Grid.EditorMode:=False;
     Grid.EditorMode:=True;
  end;
end;
}

procedure DrawTextExtAligned(Canvas: TCanvas; const Text: string; R: TRect; Alignment: TglAlignment; WordWrap: Boolean);
const
  Alignments: array [TglAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER, 0);
  WordWraps: array [Boolean] of Word = (0, DT_WORDBREAK);
var
  DrawPos, Pos1, Pos2, LineWidth, LineNo, LexemCount, TextHeight: Integer;
  Width: Integer;
  Lexem: string;
  Size: TSize;
  Stop, BroadwiseLine: Boolean;

  function GetNextLexem(var Pos1, Pos2: Integer; TrimLeft: Boolean): string;
  var
    Pos: Integer;
  begin
    Pos := Pos1;
    if Text[Pos] = ' ' then
      repeat
        Inc(Pos);
      until (Pos > Length(Text)) or (Text[Pos] <> ' ');
    Pos2 := Pos;
    if TrimLeft and (LineNo > 0) then
      Pos1 := Pos;
    repeat
      Inc(Pos2);
    until (Pos2 > Length(Text)) or (Text[Pos2] = ' ');

    Result := Copy(Text, Pos1, Pos2 - Pos1);
  end;

  procedure DrawLine(AdditSpace: Cardinal);
  var
    I, DrawPos1, DrawPos2: Integer;
    Lexem: string;
    Size: TSize;
    X, X1: Single;
  begin
    DrawPos1 := DrawPos;
    DrawPos2 := DrawPos;
    X := 0;
    X1 := 0;
    LineWidth := 0;
    for I := 1 to LexemCount do
    begin
      Lexem := GetNextLexem(DrawPos1, DrawPos2, I = 1);
      //      if LexemCount=1 then Lexem:=Lexem+' ';
      GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), Length(Lexem), Size);
      Inc(LineWidth, Trunc(X));
      X := X + Size.cx;
      if (Trunc(X) > Width) and (LexemCount > 1) then
        Exit;

      if (LexemCount > 1) and BroadwiseLine then
        X := X + AdditSpace / (LexemCount - 1);
      TextOut(Canvas.Handle, R.Left + Trunc(X1), R.Top + LineNo * TextHeight, PChar(Lexem), Length(Lexem));
      X1 := X;
      DrawPos1 := DrawPos2;
    end;
  end;

begin
  if Text = '' then
    Exit;
  if Alignment <> ftaBroadwise then
  begin
    DrawText(Canvas.Handle, PChar(Text), Length(Text), R,
      DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment]);
    Exit;
  end;
  Width := R.Right - R.Left; {Height := R.Bottom - R.Top;}
  LineWidth := 0;
  LineNo := 0;
  DrawPos := 1;
  Pos1 := 1;
  Pos2 := 1;
  LexemCount := 0;
  TextHeight := 0;
  Stop := False;
  BroadwiseLine := True;
  repeat
    Lexem := GetNextLexem(Pos1, Pos2, LexemCount = 0);
    //    if LexemCount=0 then Lexem:=Lexem+' ';
    GetTextExtentPoint32(Canvas.Handle, PChar(Lexem), Length(Lexem), Size);
    Inc(LineWidth, Size.cx);
    Inc(LexemCount);
    if TextHeight < Size.cy then
      TextHeight := Size.cy;
    if (LineWidth > Width) or (Pos2 >= Length(Text)) then
    begin
      if LineWidth > Width then
      begin
        if LexemCount = 1 then
          Pos1 := Pos2;
        if LexemCount > 1 then
          Dec(LexemCount);
        DrawLine(Width - (LineWidth - Size.cx));
        DrawPos := Pos1;
        Inc(LineNo);
        LexemCount := 0;
        LineWidth := 0;
        Stop := Pos1 > Length(Text);
      end
      else
      begin
        BroadwiseLine := False; //ftoBroadwiseLastLine;
        DrawLine(Width - LineWidth);
        Inc(LineNo);
        Stop := True;
      end;
    end
    else
      Pos1 := Pos2;
  until Stop;
  //  if FAutoSize then Height := Max( 12, LineNo*TextHeight );
end;

{ Deserialization: loading component from text file }

procedure LoadComponentFromTextFile(Component: TComponent; FileName: string);
var
  MemStream: TMemoryStream;
  FileStream: TFileStream;
begin
  MemStream := TMemoryStream.Create;
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    ObjectTextToBinary(FileStream, MemStream);
    MemStream.Position := 0;
    MemStream.ReadComponent(Component);
  finally
    MemStream.Free;
    FileStream.Free;
  end;
end;

{ Serializing component to string }

function ComponentToString(Component: TComponent): string;
var
  MemStream: TMemoryStream;
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(' ');
  MemStream := TMemoryStream.Create;
  try
    MemStream.WriteComponent(Component);
    MemStream.Position := 0;
    ObjectBinaryToText(MemStream, StringStream);
    StringStream.Position := 0;
    Result := StringStream.DataString;
  finally
    MemStream.Free;
    StringStream.Free;
  end;
end;

{ Serialization: writing component to text file }

procedure SaveComponentToTextFile(Component: TComponent; FileName: string);
var
  MemStream: TMemoryStream;
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
  try
    MemStream := TMemoryStream.Create;
    try
      MemStream.WriteComponent(Component);
      MemStream.Position := 0;
      ObjectBinaryToText(MemStream, FileStream);
    finally
      MemStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

{ Deserializing component from string }

procedure StringToComponent(Component: TComponent; const Value: string);
var
  StrStream: TStringStream;
  MemStream: TMemoryStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    MemStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, MemStream);

      MemStream.Position := 0;
      MemStream.ReadComponent(Component);
      //      Result := BinStream.ReadComponent(nil);
    finally
      MemStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

{ Plays WAV resource }

function PlayWaveResource(ResName: string): Boolean;
var
  WaveHandle: THandle;
  WavePointer: Pointer;
begin
  Result := False;
  WaveHandle := FindResource(HInstance, PChar(ResName), RT_RCDATA);
  if WaveHandle <> 0 then
  begin
    WaveHandle := LoadResource(HInstance, WaveHandle);
    if WaveHandle <> 0 then
    begin
      WavePointer := LockResource(WaveHandle);
      Result := sndPlaySound(WavePointer, SND_MEMORY or SND_ASYNC);
      UnlockResource(WaveHandle);
      FreeResource(WaveHandle);
    end;
  end;
end;

{ User name for current thread }

function UserName: string;
var
  Name: array [0..127] of Char;
  Len: DWORD;
begin
  Len := SizeOf(Name);
  GetUserName(Name, Len);
  Result := Name;
end;

{ PC name }

function ComputerName: string;
var
  Name: array [0..127] of Char;
  Len: DWORD;
begin
  Len := SizeOf(Name);
  GetComputerName(Name, Len);
  Result := Name;
end;

{ Creates ini-file with the same name to project's file - use ChangeFileExt }

function CreateIniFileName: string;
begin
  Result := ParamStr(0);
  SetLength(Result, Length(Result) - Length(ExtractFileExt(Result)));
  Result := Result + '.ini';
end;

{ Expands string with spaces up to given Length }

function ExpandString(const Str: string; Len: Integer): string;
var
  I: Integer;
begin
  Result := Str;
  if Length(Result) >= Len then
    Exit;
  SetLength(Result, Len);

  for I := 1 to Length(Result) do
    if I <= Length(Str) then
      Result[I] := Str[I]
    else
      Result[I] := ' ';
end;

{ Transliterating string Rus <-> Lat }

function Transliterate(const Str: string; RusToLat: Boolean): string;
const
  LAT: string = 'ABVGDEGZIIKLMNOPRSTUFHC___"Y''EUYabvgdegziiklmnoprstufhc___"y''euy+';
  RUS: string = 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ+';
  LATRUS: array [1..52, 1..2] of Char =
  (
    ('A', 'À'), ('B', 'Á'), ('C', 'Ö'), ('D', 'Ä'), ('E', 'Å'),
    ('F', 'Ô'), ('G', 'Æ'), ('H', 'Õ'), ('I', 'È'), ('J', 'É'),
    ('K', 'Ê'), ('L', 'Ë'), ('M', 'Ì'), ('N', 'Í'), ('O', 'Î'),
    ('P', 'Ï'), ('Q', #0), ('R', 'Ð'), ('S', 'Ñ'), ('T', 'Ò'),
    ('U', 'Ó'), ('V', 'Â'), ('W', #0), ('X', #0), ('Y', 'Û'), ('Z', 'Ç'),
    ('a', 'à'), ('b', 'á'), ('c', 'ö'), ('d', 'ä'), ('e', 'å'),
    ('f', 'ô'), ('g', 'æ'), ('h', 'õ'), ('i', 'è'), ('j', 'é'),
    ('k', 'ê'), ('l', 'ë'), ('m', 'ì'), ('n', 'í'), ('o', 'î'),
    ('p', 'ï'), ('q', #0), ('r', 'ð'), ('s', 'ñ'), ('t', 'ò'),
    ('u', 'ó'), ('v', 'â'), ('w', #0), ('x', #0), ('y', 'û'), ('z', 'ç')
    );

  TRANS_PAIRCOUNT = 14;
  TRANS_PAIR: array [1..TRANS_PAIRCOUNT, Boolean] of string =
   (('õ', 'kh'), ('ö', 'ts'), ('÷', 'ch'), ('ø', 'sh'), ('ù', 'shch'), ('þ', 'iu'), ('ÿ', 'ia'),
    ('Õ', 'Kh'), ('Ö', 'Ts'), ('×', 'Ñh'), ('Ø', 'Sh'), ('Ù', 'Shch'), ('Þ', 'Iu'), ('ß', 'Ia'));
var
  I, J: Integer;
begin
  Result := Str;
  for I := 1 to TRANS_PAIRCOUNT do
    Result := StringReplace(Result, TRANS_PAIR[I, not RusToLat], TRANS_PAIR[I, RusToLat], [rfReplaceAll]);

  if RusToLat then
  begin
    for I := 1 to Length(Result) do
      if Result[I] in ['À'..'ÿ'] then
        Result[I] := LAT[Ord(Result[I]) - Ord('À') + 1];
  end
  else
    for I := 1 to Length(Result) do
      if Result[I] in ['A'..'z'] then
        for J := 1 to 52 do
          if Result[I] = LATRUS[J, 1] then
          begin
            Result[I] := LATRUS[J, 2];
            Break;
          end;
end;

{ Function returns True, if font is small }

function IsSmallFonts: Boolean;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := (GetDeviceCaps(DC, LOGPIXELSX) = 96);
  { For large font it would be 120 }
  ReleaseDC(0, DC);
end;

{ Color depth in system: 8, 16 or 32 bits }

function SystemColorDepth: Integer;
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
  I: Integer;
const
  Extensions: array [0..3] of string = ('.gif', '.jpeg', '.jpg', '.bmp');
  Types: array [0..4] of TglFileType = (fftGif, fftJpeg, fftJpeg, fftBmp, fftUndefined);
begin
  Result := fftUndefined;
  Ext := ExtractFileExt(FileName);
  for I := 0 to 3 do
    if CompareText(Ext, Extensions[I]) = 0 then
    begin
      Result := Types[I];
      Break;
    end;
end;

{ Looks for upper(topmost) control at given point }

function FindControlAtPt(Control: TWinControl; Pt: TPoint; MinClass: TClass): TControl;
var
  I: Integer;
begin
  Result := nil;
  for I := Control.ControlCount - 1 downto 0 do
    if (Control.Controls[I] is MinClass) and PtInRect(Control.Controls[I].BoundsRect, Pt) then
    begin
      Result := Control.Controls[I];
      Break;
    end;
end;

{ StrPosExt - Looks for position of one string inside another with given length
  Outperforms StrPos on long strings in 10-100 times (1-2 orders) }

function StrPosExt(const Str1, Str2: PChar; Str2Len: DWORD): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        OR      EAX,EAX         // Str1
        JE      @@2             // If Str1 is empty - get out
        OR      EDX,EDX         // Str2
        JE      @@2             // If Str2 is empty - get out
        MOV     EBX,EAX
        MOV     EDI,EDX         // Setting offset for SCASB - substring Str2
        XOR     AL,AL           // Zero AL

        push ECX                // String length

        MOV     ECX,0FFFFFFFFH  // to be assured it will never underflow
        REPNE   SCASB           // Searching for end of Str2 substring
        NOT     ECX             // Inverting ECX - getting string length +1
        DEC     ECX             // And here is exact length

        JE      @@2             // length = 0? get out!
        MOV     ESI,ECX         // Saving substring length in ESI

        pop ECX

        SUB     ECX,ESI         // ECX := Length(Str1) - Length(Str2)
        JBE     @@2             // Length(substring) > Length(containing string) ? get out!
        MOV     EDI,EBX         // EDI points to the beginning od Str1
        LEA     EBX,[ESI-1]     // EBX - length of comparision of strings 
@@1:    MOV     ESI,EDX         // ESI - offset of Str2 string
        LODSB                   // Loading 1st byte of substring into AL
        REPNE   SCASB           // Searching that very char in EDI string
        JNE     @@2             // Char not found? get out!
        MOV     EAX,ECX         // Saving difference of lengths of strings
        PUSH    EDI             // Saving current offset of search
        MOV     ECX,EBX
        REPE    CMPSB           // per-byte comparision of strings
        POP     EDI
        MOV     ECX,EAX
        JNE     @@1             // If strings do not match - searching for 1st substring's char again
        LEA     EAX,[EDI-1]
        JMP     @@3
@@2:    XOR     EAX,EAX
@@3:    POP     EBX
        POP     ESI
        POP     EDI
end;

end.

