{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComponentFunctions.pas, released November 1999.

The Initial Developer of the Original Code is Anthony Steele [asteele@iafrica.com]
Portions created by Anthony Steele are Copyright (C) 1999-2001 Anthony Steele.
All Rights Reserved.

Contributor(s):

Last Modified: 20002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvComponentFunctions;

{-----------------------------------------------------------------------------
Comments:
  Functions pulled out of MemoEx, used in MemoEx.pas and TypedEdit.pas

  This unit has low internal cohesion (ie it contains routines that do all kinds of stuff)
  Some are very good candidates for wider reuse
  some are quite specific to the controls
  and in a larger library this unit would be broken up

  I have tried to group related functions together
}

interface

uses
  Windows, Graphics, ComCtrls, Controls;

function CharIsMoney(const ch: char): boolean;

{ there is a STrToIntDef provided by Delphi, but no "safe" versions of
  StrToFloat or StrToCurr }
function StrToFloatDef(const str: string; def: extended): extended;
function StrToCurrDef(const str: string; cDef: currency): currency;

{ GetChangedText works out the new text given the current cursor pos & the key pressed
  It is not very useful in other contexts,
  but it is in this unit as it is needed in both MemoEx and TypedEdit }
function GetChangedText(const Text: string; SelStart, SelLength: integer; Key: char): string;

function MakeYear4Digit(Year, Pivot: integer): integer;

function StrIsInteger(const S: AnsiString): boolean;
function StrIsFloatMoney(const ps: string): boolean;
function StrIsDateTime(const ps: string): boolean;

function PreformatDateString(ps: string): string;

function BooleanToInteger(const pb: boolean): integer;
function StringToBoolean(const ps: AnsiString): boolean;

function SafeStrToDateTime(const ps: string): TDateTime;
function SafeStrToDate(const ps: string): TDateTime;
function SafeStrToTime(const ps: string): TDateTime;

function StrDelete(const psSub, psMain: string): string;

{ listview functions }
function ConvertStates(const State: integer): TItemStates;

function ChangeHasDeselect(const peOld, peNew: TItemStates): boolean;
function ChangeHasSelect(const peOld, peNew: TItemStates): boolean;

function ChangeHasDefocus(const peOld, peNew: TItemStates): boolean;
function ChangeHasFocus(const peOld, peNew: TItemStates): boolean;

function GetListItemColumn(const pcItem: TListItem; piIndex: integer): string;

function ToRightOf(const pc: TControl; piSpace: integer = 0): integer;
procedure CenterHeight(const pc, pcParent: TControl);

function TimeOnly(pcValue: TDateTime): TTime;
function DateOnly(pcValue: TDateTime): TDate;

type
  TdtKind = (dtkDateOnly, dtkTimeOnly, dtkDateTime);
{ TDateTime value used to signify Null value}
const
  NullEquivalentDate: TDateTime = 0.0;

function DateIsNull(const pdtValue: TDateTime; const pdtKind: TdtKind): Boolean;
// replacement for Win32Check
function OSCheck(RetVal: boolean): boolean;

function MinimizeName(const Filename: string; Canvas: TCanvas; MaxLen: Integer): string;

implementation

uses
  {delphi } Classes, SysUtils, CommCtrl,
  { jcl } JCLStrings;

{-------------------------------------------------------------------------------
  internals }

function StrPosNoCase(const psSub, psMain: AnsiString): integer;
begin
  Result := Pos(AnsiUpperCase(psSub), AnsiUpperCase(psMain));
end;

function StrRestOf(const ps: AnsiString; const n: integer): AnsiString;
begin
  Result := Copy(ps, n, (Length(ps) - n + 1));
end;

{!!!!!!!! use these cos the JCL one is badly broken }

{ Am using this one purely as an itnernal for StrReplace

 Replace part of a AnsiString with new text. iUpdatePos is the last update position
 i.e. the position the substr was found + the length of the replacement AnsiString + 1.
 Use 0 first time in }

function StrReplaceInstance(const psSource, psSearch, psReplace: AnsiString;
  var piUpdatePos: integer; const pbCaseSens: boolean): AnsiString;
var
  liIndex: integer;
  lsCopy: AnsiString;
begin
  Result := psSource;
  if piUpdatePos >= Length(psSource) then
    exit;
  if psSearch = '' then
    exit;

  Result := StrLeft(psSource, piUpdatePos - 1);
  lsCopy := StrRestOf(psSource, piUpdatePos);

  if pbCaseSens then
    liIndex := Pos(psSearch, lsCopy)
  else
    liIndex := StrPosNoCase(psSearch, lsCopy);
  if liIndex = 0 then
    begin
      Result := psSource;
      piUpdatePos := Length(psSource) + 1;
      exit;
    end;

  Result := Result + StrLeft(lsCopy, liIndex - 1);
  Result := Result + psReplace;
  piUpdatePos := Length(Result) + 1;
  Result := Result + StrRestOf(lsCopy, liIndex + Length(psSearch));
end;

function LStrReplace(const psSource, psSearch, psReplace: AnsiString;
  const pbCaseSens: boolean): AnsiString;
var
  liUpdatePos: integer;
begin
  liUpdatePos := 0;
  Result := psSource;
  while liUpdatePos < Length(Result) do
    Result := StrReplaceInstance(Result, psSearch, psReplace, liUpdatePos, pbCaseSens);
end;

{-------------------------------------------------------------------------------
  exported }

{ if it's not a decimal point then it must be a digit, space or currency symbol
  also always use $ for money }

function CharIsMoney(const ch: char): boolean;
begin
  Result := CharIsDigit(ch) or (ch = AnsiSpace) or (ch = '$') or (ch = '-') or
    (Pos(ch, CurrencyString) > 0);
end;

function StrToCurrDef(const str: string; cDef: currency): currency;
var
  lStr: string;
begin
  try
    lStr := StrStripNonNumberChars(str);

    if lStr = '' then
      Result := cDef
    else
      Result := StrToCurr(lstr);
  except
    Result := cDef;
  end;
end;

function StrToFloatDef(const str: string; def: extended): extended;
var
  lStr: string;
begin
  lStr := StrStripNonNumberChars(str);

  if lStr = '' then
    Result := Def
  else
    try
    { the string '-' fails StrToFloat, but it can be interpreted as 0  }
      if StrRight(lStr, 1) = '-' then
        lStr := lStr + '0';

    { a string that ends in a '.' such as '12.' fails StrToFloat,
     but as far as I am concerned, it may as well be interpreted as 12.0 }
      if StrRight(lStr, 1) = '.' then
        lStr := lStr + '0';

      Result := StrToFloat(lStr);
    except
      Result := Def;
    end;
end;

function GetChangedText(const Text: string; SelStart, SelLength: integer; Key: char): string;
begin
  { take the original text, replace what will be overwritten with new value }
  Result := Text;

  if SelLength > 0 then
    Delete(Result, SelStart + 1, SelLength);
  if Key <> #0 then
    Insert(Key, Result, SelStart + 1);
end;

{ "window" technique for years to translate 2 digits to 4 digits.
   The window is 100 years wide
   The windowsill year is the lower edge of the window
  A windowsill year of 1900 is equivalent to putting 1900 before every 2-digit year
 if piWindowsillYear is 1940, then 40 is interpreted as 1940, 00 as 2000 and 39 as 2039
 The system default is 1950
}
{ "window" technique for years to translate 2 digits to 4 digits.
   The window is 100 years wide
   The pivot year is the lower edge of the window
  A pivot year of 1900 is equivalent to putting 1900 before every 2-digit year
 if pivot is 1940, then 40 is interpreted as 1940, 00 as 2000 and 39 as 2039
 The system default is 1950

 Why the reimplementation?
 JclDatetime.Make4DigitYear will fail after 2100, this won't
 note that in this implementation pivot is a 4-digit year
 I have made it accept JclDatetime.Make4DigitYear's 2 digit pivot years.
 They are expanded by adding 1900.

 It is also better in that a valid 4-digit year will pass through unchanged,
 not fail an assertion.
}

function MakeYear4Digit(Year, Pivot: integer): integer;
var
  Century: integer;
begin
  Assert(Pivot >= 0);

  { map 100 to zero }
  if Year = 100 then
    Year := 0;
  if Pivot = 100 then
    Pivot := 0;

  // turn 2 digit pivot to 4 digit
  if Pivot < 100 then
    Pivot := Pivot + 1900;

  { turn 2 digit years to 4 digits }
  if (Year >= 0) and (Year < 100) then
    begin
      Century := (Pivot div 100) * 100;

      Result := Year + Century; // give the result the same century as the pivot
      if Result < Pivot then
//  cannot be lower than the Pivot
        Result := Result + 100;
    end
  else
    Result := Year;
end;

function StrIsInteger(const S: AnsiString): boolean;
var
  I: integer;
  ch: char;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
    begin
      ch := S[I];
      if (not CharIsNumber(ch)) or (ch = DecimalSeparator) then //Az
        begin
          Result := False;
          Exit;
        end;
    end;
end;

function StrIsFloatMoney(const ps: string): boolean;
var
  liLoop, liDots: integer;
  ch: char;
begin
  Result := True;
  liDots := 0;

  for liLoop := 1 to Length(ps) do
    begin
    { allow digits, space, currency symbol and one decimal dot }
      ch := ps[liLoop];

      if (ch = DecimalSeparator) then
        begin
          inc(liDots);
          if liDots > 1 then
            begin
              Result := False;
              break;
            end;
        end
      else
        if not CharIsMoney(ch) then
          begin
            Result := False;
            break;
          end;
    end;
end;

function StrIsDateTime(const ps: string): boolean;
const
  MIN_DATE_TIME_LEN = 6; {2Jan02 }
  MAX_DATE_TIME_LEN = 30; { 30 chars or so in '12 December 1999 12:23:23:00' }
var
  liLoop: integer;
  ch: char;
  liColons, liSlashes, liSpaces, liDigits, liAlpha: integer;
  lbDisqualify: boolean;
begin
  if Length(ps) < MIN_DATE_TIME_LEN then
    begin
      Result := False;
      exit;
    end;

  if Length(ps) > MAX_DATE_TIME_LEN then
    begin
      Result := False;
      exit;
    end;

  lbDisqualify := False;
  liColons := 0;
  liSlashes := 0;
  liSpaces := 0;
  liDigits := 0;
  liAlpha := 0;

  for liLoop := 1 to Length(ps) do
    begin
      ch := ps[liLoop];

      if (ch = ':') then
        inc(liColons)
      else
        if (ch = AnsiForwardSlash) then
          inc(liSlashes)
        else
          if (ch = AnsiSpace) then
            inc(liSpaces)
          else
            if CharIsDigit(ch) then
              inc(liDigits)
            else
              if CharIsAlpha(ch) then
                inc(liAlpha)
              else
                begin
      // no weird punctuation in dates!
                  lbDisqualify := True;
                  break;
                end;
    end;

  Result := False;
  if not lbDisqualify then
    { a date must have colons and slashes and spaces, but not to many of each }
    if (liColons > 0) or (liSlashes > 0) or (liSpaces > 0) then
      { only 2 slashes in "dd/mm/yy" or 3 colons in "hh:mm:ss:ms" or 6 spaces "yy mm dd hh mm ss ms" }
      if (liSlashes <= 2) and (liColons <= 3) and (liSpaces <= 6) then
        { must have some digits (min 3 digits, eg in "2 jan 02", max 16 dgits in "01/10/2000 10:10:10:10"
        longest month name is 8 chars }
        if (liDigits >= 3) and (liDigits <= 16) and (liAlpha <= 10) then
          Result := True;

  { define in terms of results - if I can interpret it as a date, then I can }
  if Result then
    Result := (SafeStrToDateTime(PreformatDateString(ps)) <> 0);
end;

function PreformatDateString(ps: string): string;
var
  liLoop: integer;
begin
  { turn any month names to numbers }

  { use the StrReplace in stringfunctions -
  the one in JclStrings is badly broken and brings down the app }

  for liLoop := Low(LongMonthNames) to High(LongMonthNames) do
    ps := LStrReplace(ps, LongMonthNames[liLoop], IntToStr(liLoop), False);

  { now that 'January' is gone, catch 'Jan' }
  for liLoop := Low(ShortMonthNames) to High(ShortMonthNames) do
    ps := LStrReplace(ps, ShortMonthNames[liLoop], IntToStr(liLoop), False);

  { remove redundant spaces }
  ps := LStrReplace(ps, AnsiSpace + AnsiSpace, AnsiSpace, False);

  Result := ps;
end;

function BooleanToInteger(const pb: boolean): integer;
begin
  if pb then
    Result := 1
  else
    Result := 0;
end;

{ from my ConvertFunctions unit }

function StringToBoolean(const ps: AnsiString): boolean;
const
  TRUE_STRINGS: array[1..5] of string = ('true', 't', 'y', 'yes', '1');
var
  liLoop: integer;
begin
  Result := False;

  for liLoop := Low(TRUE_STRINGS) to High(TRUE_STRINGS) do
    if AnsiSameText(ps, TRUE_STRINGS[liLoop]) then
      begin
        Result := True;
        break;
      end;
end;

function SafeStrToDateTime(const ps: string): TDateTime;
begin
  try
    Result := StrToDateTime(PreformatDateString(ps));
  except
    on E: EConvertError do
      Result := 0.0
    else
      raise;
  end;
end;

function SafeStrToDate(const ps: string): TDateTime;
begin
  try
    Result := StrToDate(PreformatDateString(ps));
  except
    on E: EConvertError do
      Result := 0.0
    else
      raise;
  end;
end;

function SafeStrToTime(const ps: string): TDateTime;
begin
  try
    Result := StrToTime(ps)
  except
    on E: EConvertError do
      Result := 0.0
    else
      raise;
  end;
end;

{ imported from VCLFunctions }

procedure CenterHeight(const pc, pcParent: TControl);
begin
  pc.Top := //pcParent.Top +
    ((pcParent.Height - pc.Height) div 2);
end;

function ToRightOf(const pc: TControl; piSpace: integer): integer;
begin
  if pc <> nil then
    Result := pc.Left + pc.Width + piSpace
  else
    Result := piSpace;
end;

{ have to do this as it depends what the datekind of the control is}

function DateIsNull(const pdtValue: TDateTime; const pdtKind: TdtKind): Boolean;
begin
  Result := false;
  case pdtKind of
    dtkDateOnly: Result := pdtValue < 1; //if date only then anything less than 1 is considered null
    dtkTimeOnly: Result := frac(pdtValue) = NullEquivalentDate; //if time only then anything without a remainder is null
    dtkDateTime: Result := pdtValue = NullEquivalentDate;
  end;
end;

function OSCheck(RetVal: boolean): boolean;
begin
  if not RetVal then RaiseLastOSError;
  Result := RetVal;
end;

function MinimizeName(const Filename: string; Canvas: TCanvas; MaxLen: Integer): string;
var b: array[0..MAX_PATH] of char; R: TRect;
begin
  StrCopy(b, PChar(Filename));
  R := Rect(0, 0, MaxLen, Canvas.TextHeight('Wq'));
  if DrawText(Canvas.Handle, b, Length(Filename), R,
    DT_SINGLELINE or DT_MODIFYSTRING or DT_PATH_ELLIPSIS or DT_CALCRECT or DT_NOPREFIX) > 0 then
    Result := b
  else
    Result := Filename;
end;

function TimeOnly(pcValue: TDateTime): TTime;
begin
  Result := frac(pcValue);
end;

function DateOnly(pcValue: TDateTime): TDate;
begin
  Result := trunc(pcValue);
end;

{-------------------------------------------------------------------------------
  internals used below }

function HasFlag(a, b: integer): boolean;
begin
  Result := (a and b) <> 0;
end;

{-------------------------------------------------------------------------------
  listview specific stuff }

{ compied from ComCtrls.pas's implmentation section }

function ConvertStates(const State: integer): TItemStates;
begin
  Result := [];
  if HasFlag(State, LVIS_ACTIVATING) then
    Include(Result, isActivating);
  if HasFlag(State, LVIS_CUT) then
    Include(Result, isCut);
  if HasFlag(State, LVIS_DROPHILITED) then
    Include(Result, isDropHilited);
  if HasFlag(State, LVIS_FOCUSED) then
    Include(Result, isFocused);
  if HasFlag(State, LVIS_SELECTED) then
    Include(Result, isSelected);
end;

function ChangeHasSelect(const peOld, peNew: TItemStates): boolean;
begin
  Result := (not (isSelected in peOld)) and (isSelected in peNew);
end;

function ChangeHasDeselect(const peOld, peNew: TItemStates): boolean;
begin
  Result := (isSelected in peOld) and (not (isSelected in peNew));
end;

function ChangeHasFocus(const peOld, peNew: TItemStates): boolean;
begin
  Result := (not (isFocused in peOld)) and (isFocused in peNew);
end;

function ChangeHasDefocus(const peOld, peNew: TItemStates): boolean;
begin
  Result := (isFocused in peOld) and (not (isFocused in peNew));
end;

function GetListItemColumn(const pcItem: TListItem; piIndex: integer): string;
begin
  if pcItem = nil then
    begin
      Result := '';
      exit;
    end;

  if (piIndex < 0) or (piIndex > pcItem.SubItems.Count) then
    begin
      Result := '';
      exit;
    end;

  if piIndex = 0 then
    Result := pcItem.Caption
  else
    Result := pcItem.SubItems[piIndex - 1];
end;

{!! from strFunctions }

function StrDeleteChars(const ps: string; const piPos: integer; const piCount: integer): string;
begin
  Result := StrLeft(ps, piPos - 1) + StrRestOf(ps, piPos + piCount);
end;

function StrDelete(const psSub, psMain: string): string;
var
  liPos: integer;
begin
  Result := psMain;
  if psSub = '' then
    exit;

  liPos := StrIPos(psSub, psMain);

  while liPos > 0 do
    begin
      Result := StrDeleteChars(Result, liPos, Length(psSub));
      liPos := StrIPos(psSub, Result);
    end;
end;

end.

