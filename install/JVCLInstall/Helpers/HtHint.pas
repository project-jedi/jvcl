{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: HtHint.pas, released on 2004-02-06.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit HtHint;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  JvConsts;

type
  THtHintWindow = class(THintWindow)
  protected
    procedure Paint; override;
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
  end;

function WordWrapString(const S: string; Width: Integer = 75): string;

implementation

function WordWrapString(const S: string; Width: Integer = 75): string;
var
  i, cnt, Len, LastWordStart, BreakStrLen: Integer;
begin
  Result := S;
  BreakStrLen := Length(sLineBreak);
  if (Width <= 0) or (S = '') then
    Exit;

  Len := Length(Result);
  i := 1;
  while i <= Len do
  begin
    cnt := 0;
    LastWordStart := 0;
    while (i <= Len) and ((LastWordStart = 0) or (cnt <= Width)) do
    begin
      if Result[i] = ' ' then
        LastWordStart := i;
      Inc(cnt);
      Inc(i);
    end;
    if i <= Len then
    begin
      if LastWordStart > 0 then
      begin
        Delete(Result, LastWordStart, 1);
        Dec(Len, 1);
        i := LastWordStart;
      end;
      Insert(sLineBreak, Result, i);
      Inc(Len, BreakStrLen);
      Inc(i, BreakStrLen);
    end;
  end;
end;

function SubStr(const S: string; const Index: Integer; const Separator: string): string;
{ Returns a substring. Substrings are divided by Sep character [translated] }
var
  I: Integer;
  pB, pE: PChar;
begin
  Result := '';
  if ((Index < 0) or ((Index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then
    Exit;
  pB := PChar(S);
  for I := 1 to Index do
  begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then
      Exit;
    pB := pB + Length(Separator);
    if pB[0] = #0 then
      Exit;
  end;
  pE := StrPos(pB + 1, PChar(Separator));
  if pE = nil then
    pE := PChar(S) + Length(S);
  if not (AnsiStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, pB, pE - pB);
end;

procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
var
  CL: string;
  I: Integer;
  M1: string;
  OriRect: TRect; // it's added
  LastFontStyle: TFontStyles;
  LastFontColor: TColor;

  function Cmp(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
  end;

  function Cmp1(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
    if Result then
      Inc(I, Length(M1));
  end;

  function CmpL(M1: string): Boolean;
  begin
    Result := Cmp(M1 + '>');
  end;

  function CmpL1(M1: string): Boolean;
  begin
    Result := Cmp1(M1 + '>');
  end;

  procedure Draw(const M: string);
  begin
    if not Assigned(Canvas) then
      Exit;
    if not CalcWidth then
      Canvas.TextOut(Rect.Left, Rect.Top, M);
    Rect.Left := Rect.Left + Canvas.TextWidth(M);
  end;

  procedure Style(const Style: TFontStyle; const Include: Boolean);
  begin
    if not Assigned(Canvas) then
      Exit;
    if Include then
      Canvas.Font.Style := Canvas.Font.Style + [Style]
    else
      Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;

begin
  PlainItem := '';
  LastFontColor := 0; { satisfy compiler }
  if Canvas <> nil then
  begin
    LastFontStyle := Canvas.Font.Style;
    LastFontColor := Canvas.Font.Color;
  end;
  try
    if HideSelColor and Assigned(Canvas) then
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.Font.Color := clWindowText;
    end;
    if Assigned(Canvas) then
      Canvas.FillRect(Rect);

    Width := Rect.Left;
    Rect.Left := Rect.Left + 2;

    OriRect := Rect; //save origin rectangle

    M1 := '';
    I := 1;
    while I <= Length(Text) do
    begin
      if (Text[I] = '<') and
        (CmpL('b') or CmpL('/b') or
        CmpL('i') or CmpL('/i') or
        CmpL('u') or CmpL('/u') or
        Cmp('c:')) then
      begin
        Draw(M1);
        PlainItem := PlainItem + M1;

        if CmpL1('b') then
          Style(fsBold, True)
        else
        if CmpL1('/b') then
          Style(fsBold, False)
        else
        if CmpL1('i') then
          Style(fsItalic, True)
        else
        if CmpL1('/i') then
          Style(fsItalic, False)
        else
        if CmpL1('u') then
          Style(fsUnderline, True)
        else
        if CmpL1('/u') then
          Style(fsUnderline, False)
        else
        if Cmp1('c:') then
        begin
          CL := SubStr(PChar(Text) + I, 0, '>');
          if (HideSelColor or not (odSelected in State)) and Assigned(Canvas) then
          try
            if (Length(CL) > 0) and (CL[1] <> '$') then
              Canvas.Font.Color := StringToColor('cl' + CL)
            else
              Canvas.Font.Color := StringToColor(CL);
          except
          end;
          Inc(I, Length(CL) + 1 {'>'});
        end;
        Inc(I);
        if (Text[I] = Chr(13)) and Cmp1(string(Chr(10))) then
        begin
          Rect.Left := OriRect.Left;
          Rect.Top := Rect.Top + Canvas.TextHeight(M1 + 'W');
          Inc(I);
        end;
        Dec(I);
        M1 := '';
      end
      else
      // next lines were added
      if (Text[I] = Chr(13)) and Cmp1(string(Chr(10))) then
      begin
        // new line
        Draw(M1);
        PlainItem := PlainItem + M1;
        Rect.Left := OriRect.Left;
        Rect.Top := Rect.Top + Canvas.TextHeight(M1 + 'W');
        M1 := '';
      end
      else
        M1 := M1 + Text[I]; // add text
      Inc(I);
    end; { for }
    Draw(M1);
    PlainItem := PlainItem + M1;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := LastFontStyle;
      Canvas.Font.Color := LastFontColor;
    end;
  end;
  Width := Rect.Left - Width + 2;
end;

function ItemHtDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, False);
end;

function THtHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: Pointer): TRect;
var
  S: string;
  R: TRect;
  W: Integer;
  i: Integer;
  Lines: TStrings;
begin
  Result := inherited CalcHintRect(MaxWidth, AHint, AData);

  R := Rect(2, 2, MaxWidth - 2, MaxInt);
  Lines := TStringList.Create;
  try
    Lines.Text := AHint;
    W := Result.Right;
    Result.Right := 0;
    for i := 0 to Lines.Count - 1 do
    begin
      ItemHTDrawEx(Canvas, R, [odDefault], Lines[i], False, S, W, True);
      if W > Result.Right then
        Result.Right := W;
    end;
  finally
    Lines.Free;
  end;
  Inc(Result.Right, 6);
end;

procedure THtHintWindow.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Canvas.Font.Color := Screen.HintFont.Color;

  ItemHtDraw(Canvas, R, [odDefault], Text, False);
end;

end.
