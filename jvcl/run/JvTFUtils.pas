{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFUtils.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvTFUtils;

interface

uses
  {$IFDEF VCL}
  Windows, Graphics, Controls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, Types, QWindows,
  {$ENDIF VisualCLX}
  Classes, SysUtils;

{$HPPEMIT '#ifndef TDate'}
{$IFDEF VCL}
{$HPPEMIT '#define TDate Controls::TDate'}
{$HPPEMIT '#define TTime Controls::TTime'}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$HPPEMIT '#define TDate TDateTime'}
{$HPPEMIT '#define TTime TDateTime'}
{$ENDIF VisualCLX}
{$HPPEMIT '#endif'}
  
type
  TJvTFVisibleScrollBars = set of (vsbHorz, vsbVert);
  EJvTFDateError = class(Exception);

  TTFDayOfWeek = (dowSunday, dowMonday, dowTuesday, dowWednesday,
    dowThursday, dowFriday, dowSaturday);
  TTFDaysOfWeek = set of TTFDayOfWeek;

  TJvTFVAlignment = (vaTop, vaCenter, vaBottom);

  TJvTFDirection = (dirUp, dirDown, dirLeft, dirRight);

const
  DOW_WEEK: TTFDaysOfWeek = [dowSunday..dowSaturday];
  DOW_WEEKEND: TTFDaysOfWeek = [dowSunday, dowSaturday];
  DOW_WORKWEEK: TTFDaysOfWeek = [dowMonday..dowFriday];

  ONE_HOUR = 1 / 24;
  ONE_MINUTE = ONE_HOUR / 60;
  ONE_SECOND = ONE_MINUTE / 60;
  ONE_MILLISECOND = ONE_SECOND / 1000;

function ExtractYear(aDate: TDateTime): Word;
function ExtractMonth(aDate: TDateTime): Word;
function ExtractDay(aDate: TDateTime): Word;
function ExtractHours(aTime: TDateTime): Word;
function ExtractMins(aTime: TDateTime): Word;
function ExtractSecs(aTime: TDateTime): Word;
function ExtractMSecs(aTime: TDateTime): Word;
function FirstOfMonth(aDate: TDateTime): TDateTime;
function GetDayOfNthDOW(Year, Month, DOW, N: Word): Word;
function GetWeeksInMonth(Year, Month: Word; StartOfWeek: Integer): Word;

procedure IncBorlDOW(var BorlDOW: Integer; N: Integer = 1);
procedure IncDOW(var DOW: TTFDayOfWeek; N: Integer = 1);
procedure IncDays(var aDate: TDateTime; N: Integer = 1);
procedure IncWeeks(var aDate: TDateTime; N: Integer = 1);
procedure IncMonths(var aDate: TDateTime; N: Integer = 1);
procedure IncYears(var aDate: TDateTime; N: Integer = 1);

function EndOfMonth(aDate: TDateTime): TDateTime;
function IsFirstOfMonth(aDate: TDateTime): Boolean;
function IsEndOfMonth(aDate: TDateTime): Boolean;
procedure EnsureMonth(Month: Word);
procedure EnsureDOW(DOW: Word);
function EqualDates(D1, D2: TDateTime): Boolean;
function Lesser(N1, N2: Integer): Integer;
function Greater(N1, N2: Integer): Integer;
function GetDivLength(TotalLength, DivCount, DivNum: Integer): Integer;
function GetDivNum(TotalLength, DivCount, X: Integer): Integer;
function GetDivStart(TotalLength, DivCount, DivNum: Integer): Integer;
function DOWToBorl(aDOW: TTFDayOfWeek): Integer;
function BorlToDOW(BorlDOW: Integer): TTFDayOfWeek;
function DateToDOW(aDate: TDateTime): TTFDayOfWeek;

procedure CalcTextPos(HostRect: TRect; var TextLeft, TextTop: Integer;
  var TextBounds: TRect; aFont: TFont; aAngle: Integer;
  HAlign: TAlignment; VAlign: TJvTFVAlignment; aTxt: string);

procedure DrawAngleText(aCanvas: TCanvas; HostRect: TRect;
  var TextBounds: TRect; aAngle: Integer; HAlign: TAlignment;
  VAlign: TJvTFVAlignment; aTxt: string);

function RectWidth(ARect: TRect): Integer;
function RectHeight(ARect: TRect): Integer;
function EmptyRect: TRect;
function IsClassByName(Obj: TObject; ClassName: ShortString): Boolean;

implementation

{$IFDEF USEJVCL}
uses
  JvResources;
{$ENDIF USEJVCL}

{$IFNDEF USEJVCL}
resourcestring
  RsEResultDoesNotFallInMonth = 'Result does not fall in given month';
  RsEInvalidMonthValue = 'Invalid Month Value (%d)';
  RsEInvalidDayOfWeekValue = 'Invalid value for day of week (%d)';
{$ENDIF !USEJVCL}

function ExtractYear(aDate: TDateTime): Word;
var
  M, D: Word;
begin
  DecodeDate(aDate, Result, M, D);
end;

function ExtractMonth(aDate: TDateTime): Word;
var
  Y, D: Word;
begin
  DecodeDate(aDate, Y, Result, D);
end;

function ExtractDay(aDate: TDateTime): Word;
var
  Y, M: Word;
begin
  DecodeDate(aDate, Y, M, Result);
end;

function FirstOfMonth(aDate: TDateTime): TDateTime;
var
  Y, M, D: Word;
begin
  DecodeDate(aDate, Y, M, D);
  Result := EncodeDate(Y, M, 1);
end;

function GetDayOfNthDOW(Year, Month, DOW, N: Word): Word;
var
  FirstDayDOW: Word;
  WorkDate: TDateTime;
begin
  WorkDate := EncodeDate(Year, Month, 1);
  FirstDayDOW := DayOfWeek(WorkDate);
  WorkDate := WorkDate + (DOW - FirstDayDOW);
  if DOW < FirstDayDOW then
    WorkDate := WorkDate + 7;

  // WorkDate is now at the first DOW
  // Now adjust for N
  WorkDate := WorkDate + (7 * (N - 1));

  Result := ExtractDay(WorkDate);
  // Finally, check to make sure WorkDate is in the given month
  if Trunc(EncodeDate(Year, Month, 1)) <> Trunc(FirstOfMonth(WorkDate)) then
    raise EJvTFDateError.Create(RsEResultDoesNotFallInMonth);
end;

function GetWeeksInMonth(Year, Month: Word; StartOfWeek: Integer): Word;
var
  DOW,
    EndOfWeek: Integer;
  EOM,
    WorkDate: TDateTime;
begin
  // Get the end of the week
  EndOfWeek := StartOfWeek;
  IncBorlDOW(EndOfWeek, -1);

  // Start working at the first of the month
  WorkDate := EncodeDate(Year, Month, 1);

  // Get the end of the month
  EOM := EndOfMonth(WorkDate);

  // Get the day the first falls on
  DOW := DayOfWeek(WorkDate);

  // Advance WorkDate to the end of the week
  while DOW <> EndOfWeek do
  begin
    IncBorlDOW(DOW, 1);
    WorkDate := WorkDate + 1;
  end;

  // We're now on week 1
  Result := 1;
  // Now roll through the rest of the month
  while Trunc(WorkDate) < Trunc(EOM) do
  begin
    Inc(Result);
    IncWeeks(WorkDate, 1);
  end;
end;

procedure IncBorlDOW(var BorlDOW: Integer; N: Integer); // N defaults to 1
begin
  BorlDOW := (BorlDOW + (N mod 7)) mod 7;
  if BorlDOW = 0 then
    BorlDOW := 7;
  BorlDOW := Abs(BorlDOW);
end;

procedure IncDOW(var DOW: TTFDayOfWeek; N: Integer);
                                  // N defaults to 1
var
  BorlDOW: Integer;
begin
  BorlDOW := DOWToBorl(DOW);
  IncBorlDOW(BorlDOW, N);
  DOW := BorlToDOW(BorlDOW);
end;

procedure IncDays(var aDate: TDateTime; N: Integer);
                                     // N defaults to 1
begin
  aDate := aDate + N;
end;

procedure IncWeeks(var aDate: TDateTime; N: Integer);
                                     // N defaults to 1
begin
  aDate := aDate + N * 7;
end;

procedure IncMonths(var aDate: TDateTime; N: Integer);
                                      // N defaults to 1
var
  Y, M, D, EOMD: Word;
begin
  DecodeDate(aDate, Y, M, D);
  Inc(Y, N div 12);
  Inc(M, N mod 12);

  // Be careful not to get invalid date in Feb.
  if M = 2 then
  begin
    EOMD := ExtractDay(EndOfMonth(EncodeDate(Y, M, 1)));
    if D > EOMD then
      D := EOMD;
  end;

  aDate := EncodeDate(Y, M, D);
end;

procedure IncYears(var aDate: TDateTime; N: Integer);
                                     // N defaults to 1
var
  Y, M, D, EOMD: Word;
begin
  DecodeDate(aDate, Y, M, D);
  Inc(Y, N);

  // Be careful not to get invalid date in Feb.
  if M = 2 then
  begin
    EOMD := ExtractDay(EndOfMonth(EncodeDate(Y, M, 1)));
    if D > EOMD then
      D := EOMD;
  end;

  aDate := EncodeDate(Y, M, D);
end;

function EndOfMonth(aDate: TDateTime): TDateTime;
var
  Y, M, D: Word;
begin
  DecodeDate(aDate, Y, M, D);
  Inc(M);
  if M > 12 then
  begin
    M := 1;
    Inc(Y);
  end;
  Result := EncodeDate(Y, M, 1) - 1;
end;

function IsFirstOfMonth(aDate: TDateTime): Boolean;
var
  Y, M, D: Word;
begin
  DecodeDate(aDate, Y, M, D);
  Result := D = 1;
end;

function IsEndOfMonth(aDate: TDateTime): Boolean;
begin
  Result := EqualDates(aDate, EndOfMonth(aDate));
end;

procedure EnsureMonth(Month: Word);
begin
  if (Month < 1) or (Month > 12) then
    raise EJvTFDateError.CreateFmt(RsEInvalidMonthValue, [Month]);
end;

procedure EnsureDOW(DOW: Word);
begin
  if (DOW < 1) or (DOW > 7) then
    raise EJvTFDateError.CreateFmt(RsEInvalidDayOfWeekValue, [DOW]);
end;

function EqualDates(D1, D2: TDateTime): Boolean;
begin
  Result := Trunc(D1) = Trunc(D2);
end;

function ExtractHours(aTime: TDateTime): Word;
var
  M, S, MS: Word;
begin
  DecodeTime(aTime, Result, M, S, MS);
end;

function ExtractMins(aTime: TDateTime): Word;
var
  H, S, MS: Word;
begin
  DecodeTime(aTime, H, Result, S, MS);
end;

function ExtractSecs(aTime: TDateTime): Word;
var
  H, M, MS: Word;
begin
  DecodeTime(aTime, H, M, Result, MS);
end;

function ExtractMSecs(aTime: TDateTime): Word;
var
  H, M, S: Word;
begin
  DecodeTime(aTime, H, M, S, Result);
end;

function Lesser(N1, N2: Integer): Integer;
begin
  if N1 < N2 then
    Result := N1
  else
    Result := N2;
end;

function Greater(N1, N2: Integer): Integer;
begin
  if N1 > N2 then
    Result := N1
  else
    Result := N2;
end;

function GetDivLength(TotalLength, DivCount, DivNum: Integer): Integer;
begin
  if (DivNum < 0) or (DivNum >= DivCount) then
    Result := -1
  else
  begin
    Result := TotalLength div DivCount;
    if DivNum < TotalLength mod DivCount then
      Inc(Result);
  end;
end;

function GetDivNum(TotalLength, DivCount, X: Integer): Integer;
var
  Base,
    MakeUp,
    MakeUpWidth: Integer;
begin
  if (X < 0) or (X >= TotalLength) then
    Result := -1
  else
  begin
    Base := TotalLength div DivCount;
    MakeUp := TotalLength mod DivCount;
    MakeUpWidth := MakeUp * (Base + 1);

    if X < MakeUpWidth then
      Result := X div (Base + 1)
    else
      Result := (X - MakeUpWidth) div Base + MakeUp;
  end;
end;

function GetDivStart(TotalLength, DivCount, DivNum: Integer): Integer;
var
  Base,
    MakeUp,
    MakeUpWidth: Integer;
begin
  if (DivNum < 0) or (DivNum >= DivCount) then
    Result := -1
  else
  begin
    Base := TotalLength div DivCount;
    MakeUp := TotalLength mod DivCount;
    MakeUpWidth := MakeUp * (Base + 1);

    if DivNum <= MakeUp then
      Result := DivNum * (Base + 1)
    else
      Result := (DivNum - MakeUp) * Base + MakeUpWidth;
  end;
end;

function DOWToBorl(aDOW: TTFDayOfWeek): Integer;
begin
  Result := Ord(aDOW) + 1;
end;

function BorlToDOW(BorlDOW: Integer): TTFDayOfWeek;
begin
  Result := TTFDayOfWeek(BorlDOW - 1);
end;

function DateToDOW(aDate: TDateTime): TTFDayOfWeek;
var
  BorlDOW: Integer;
begin
  BorlDOW := DayOfWeek(aDate);
  Result := BorlToDOW(BorlDOW);
end;

//////////////////////////////////////////////////////////////////
// Credit for the CalcTextPos routine goes to Joerg Lingner.    //
// It comes from his JLLabel component (freeware - Torry's).    //
// It is used here with his permission.  Thanks Joerg!          //
// He can be reached at jlingner@t-online.de                    //
//////////////////////////////////////////////////////////////////

procedure CalcTextPos(HostRect: TRect; var TextLeft, TextTop: Integer;
  var TextBounds: TRect; aFont: TFont; aAngle: Integer;
  HAlign: TAlignment; VAlign: TJvTFVAlignment; aTxt: string);
{==========================================================================}
{ Calculate text pos. depend. on: Font, Escapement, Alignment and length   }
{--------------------------------------------------------------------------}
var
  DC: HDC;
  hSavFont: HFont;
  Size: TSize;
  x, y: Integer;
    //cStr   : array[0..255] of Char;
  PTxt: PChar;
  a, b, c, d: Integer;
  lb, lt, rb, rt: TPoint;
begin
  aAngle := aAngle div 10;

  PTxt := StrAlloc((Length(aTxt) + 4) * SizeOf(Char));
  StrPCopy(PTxt, aTxt);

  //StrPCopy(cStr, aTxt);
  DC := GetDC(0);
  hSavFont := SelectObject(DC, aFont.Handle);
  //GetTextExtentPoint32(DC, cStr, Length(aTxt), Size);
  {$IFDEF VCL}
  Windows.GetTextExtentPoint32(DC, PTxt, StrLen(PTxt), Size);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  GetTextExtentPoint32(DC, PTxt, StrLen(PTxt), Size);
  {$ENDIF VisualCLX}
  StrDispose(PTxt);
  SelectObject(DC, hSavFont);
  ReleaseDC(0, DC);

  x := 0;
  y := 0;

  if aAngle <= 90 then
  begin { 1.Quadrant }
    x := 0;
    y := Trunc(Size.cx * sin(aAngle * Pi / 180));
  end
  else
  if aAngle <= 180 then
  begin { 2.Quadrant }
    x := Trunc(Size.cx * -cos(aAngle * Pi / 180));
    y := Trunc(Size.cx * sin(aAngle * Pi / 180) + Size.cy * cos((180 - aAngle) * Pi / 180));
  end
  else
  if aAngle <= 270 then
  begin { 3.Quadrant }
    x := Trunc(Size.cx * -cos(aAngle * Pi / 180) + Size.cy * sin((aAngle - 180) * Pi / 180));
    y := Trunc(Size.cy * sin((270 - aAngle) * Pi / 180));
  end
  else
  if aAngle <= 360 then
  begin { 4.Quadrant }
    x := Trunc(Size.cy * sin((360 - aAngle) * Pi / 180));
    y := 0;
  end;

  TextLeft := HostRect.Left + x;
  TextTop := HostRect.Top + y;
  //ARect.Top := ARect.Top + y;
  //ARect.Left := ARect.Left + x;

  x := Abs(Trunc(Size.cx * cos(aAngle * Pi / 180))) + Abs(Trunc(Size.cy * sin(aAngle * Pi / 180)));
  y := Abs(Trunc(Size.cx * sin(aAngle * Pi / 180))) + Abs(Trunc(Size.cy * cos(aAngle * Pi / 180)));

  case HAlign of
    taCenter:
      //ARect.Left := ARect.Left + ((RectWidth(SaveRect) - X) div 2);
      TextLeft := TextLeft + ((RectWidth(HostRect) - x) div 2);
    taRightJustify:
      //ARect.Left := ARect.Left + RectWidth(SaveRect) - X;
      TextLeft := TextLeft + RectWidth(HostRect) - x;
  end;

  case VAlign of
    vaCenter:
      //ARect.Top := ARect.Top + ((RectHeight(SaveRect) - Y) div 2);
      TextTop := TextTop + ((RectHeight(HostRect) - y) div 2);
    vaBottom:
      //ARect.Top := ARect.Top + RectHeight(SaveRect) - Y;
      TextTop := TextTop + RectHeight(HostRect) - y;
  end;

  //ARect.Right := ARect.Left + X;
  //ARect.Bottom := ARect.Top + Y;
//********************************************
//  calculate the border areas

  a := Trunc(Size.cy * sin(aAngle * Pi / 180));
  b := Trunc(Size.cy * cos(aAngle * Pi / 180));
  c := Trunc(Size.cx * cos(aAngle * Pi / 180));
  d := Trunc(Size.cx * sin(aAngle * Pi / 180));

  //lt := ARect.TopLeft;
  lt := Point(TextLeft, TextTop);
  lb := lt;
  lb.x := lb.x + a;
  lb.y := lb.y + b;
  rb := lb;
  rb.x := rb.x + c;
  rb.y := rb.y - d;
  rt := rb;
  rt.x := rt.x - a;
  rt.y := rt.y - b;

  TextBounds.Left := Lesser(Lesser(lt.x, lb.x), Lesser(rb.x, rt.x));
  TextBounds.Right := Greater(Greater(lt.x, lb.x), Greater(rb.x, rt.x));
  TextBounds.Top := Lesser(Lesser(lt.y, lb.y), Lesser(rb.y, rt.y));
  TextBounds.Bottom := Greater(Greater(lt.y, lb.y), Greater(rb.y, rt.y));
//*********************************************************************************************
end;

procedure DrawAngleText(aCanvas: TCanvas; HostRect: TRect;
  var TextBounds: TRect; aAngle: Integer; HAlign: TAlignment;
  VAlign: TJvTFVAlignment; aTxt: string);
var
  {$IFDEF VCL}
  LogFont: TLogFont;
  {$ENDIF VCL}
  TxtRect: TRect;
  Flags: UINT;
  PTxt: PChar;
  ClipRgn: HRgn;
  TextLeft,
    TextTop: Integer;
begin
  //TxtRect := ARect;
  //CalcTextPos(TxtRect, aCanvas.Font, aAngle, HAlign, VAlign, aTxt);
  CalcTextPos(HostRect, TextLeft, TextTop, TextBounds, aCanvas.Font, aAngle,
    HAlign, VAlign, aTxt);
  {$IFDEF VCL}
  Windows.GetObject(aCanvas.Font.Handle, SizeOf(LogFont), @LogFont);
  LogFont.lfEscapement := aAngle;
  LogFont.lfOrientation := LogFont.lfEscapement;
  aCanvas.Font.Handle := CreateFontIndirect(LogFont);
  {$ENDIF VCL}
  Flags := DT_NOPREFIX or DT_LEFT or DT_TOP or DT_NOCLIP or DT_SINGLELINE;

  PTxt := StrAlloc((Length(aTxt) + 4) * SizeOf(Char));
  StrPCopy(PTxt, aTxt);
  {$IFDEF VCL}
  //ClipRgn := Windows.CreateRectRgn(ARect.Left, ARect.Top,
    //                               ARect.Right, ARect.Bottom);
  ClipRgn := Windows.CreateRectRgn(HostRect.Left, HostRect.Top,
    HostRect.Right, HostRect.Bottom);
  Windows.SelectClipRgn(aCanvas.Handle, ClipRgn);

  //Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);
  TxtRect := Rect(TextLeft, TextTop, TextLeft + 1, TextTop + 1);
  Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);

  Windows.SelectClipRgn(aCanvas.Handle, 0);
  Windows.DeleteObject(ClipRgn);
  StrDispose(PTxt);
  aCanvas.Font.Handle := 0;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  ClipRgn := CreateRectRgn(HostRect.Left, HostRect.Top,
    HostRect.Right, HostRect.Bottom);
  SelectClipRgn(aCanvas.Handle, ClipRgn);

  //Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);
  TxtRect := Rect(TextLeft, TextTop, TextLeft + 1, TextTop + 1);
  DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);

  SelectClipRgn(aCanvas.Handle, 0);
  DeleteObject(ClipRgn);
  StrDispose(PTxt);
  aCanvas.Font.Handle := nil;
  {$ENDIF VisualCLX}

  //ARect := TxtRect;
end;

function RectWidth(ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

function RectHeight(ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

function EmptyRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function IsClassByName(Obj: TObject; ClassName: ShortString): Boolean;
var
  ClassRef: TClass;
begin
  Result := False;
  ClassRef := Obj.ClassType;
  while (ClassRef <> nil) and not Result do
    if ClassRef.ClassName = ClassName then
      Result := True
    else
      ClassRef := ClassRef.ClassParent;
end;

end.

