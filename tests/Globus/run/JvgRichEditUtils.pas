{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgRichEditUtils.PAS, released on 2003-01-15.

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

unit JvgRichEditUtils; //  TRichEdit Wrapping functions

interface
uses Windows, Messages, graphics, comctrls, classes;

procedure AddNl(RE: TRichEdit);
procedure AddText(RE: TRichEdit; Str: string; TxtSize: integer; TxtStyle: TFontStyles; TxtColor: TColor);
procedure AddTextNl(RE: TRichEdit; Str: string; TxtSize: integer; TxtStyle: TFontStyles; TxtColor: TColor);
procedure doSintaxHighligh(memo: TRichEdit);

implementation

procedure AddNl(RE: TRichEdit);
begin
  RE.Lines.Add('');
end;

procedure AddText(RE: TRichEdit; Str: string; TxtSize: integer; TxtStyle: TFontStyles; TxtColor: TColor);
var
  Pos1, Pos2: integer;
begin
  with RE, RE.Lines do
  begin
    Pos1 := length(Text);
    if Pos1 = 0 then
      Text := str
    else
      Lines[Lines.Count - 1] := Lines[Lines.Count - 1] + Str;
    Pos2 := length(Text);
    SelStart := Pos1 - 2;
    SelLength := Pos2 - Pos1 + 1;

    SelAttributes.Size := TxtSize;
    SelAttributes.Style := TxtStyle;
    SelAttributes.Color := TxtColor;
  end;
end;

procedure AddTextNl(RE: TRichEdit; Str: string; TxtSize: integer; TxtStyle: TFontStyles; TxtColor: TColor);
begin
  RE.Lines.Add('');
  AddText(RE, Str, TxtSize, TxtStyle, TxtColor);
end;

procedure doSintaxHighligh(memo: TRichEdit);
type
  THData = record
    Word: string;
    Color: TColor;
    Bold: boolean;
  end;

  TLexemType = (ltTag, ltComma);

  PLexem = ^TLexem;
  TLexem = record
    _start, _end: integer;
    _type: TLexemType;
  end;

  TLastLexem = record
    Tag, Comma: TLexem;
  end;

var
  Pos1, Pos2: integer;
  i: integer;
  fInQuote, doBold: boolean;
  Lexem, memoText: string;
  Lexems: TList;
  LastLexem: TLastLexem;
  PL: PLexem;

const
  WORDSCOUNT = 6;
  aWord: array[1..WORDSCOUNT] of string = ('<html>', '<body>', 'function', 'var', 'script', '<Table');
  aColor: array[1..WORDSCOUNT] of TColor = ($500000, $500000, $500000, $500000, $000060, $606000);
  aBold: array[1..WORDSCOUNT] of boolean = (false, false, true, true, false, false);

  function GetNextLexem(var Pos1, Pos2: integer; fTrimleft: boolean): string;
  var
    Pos: integer;
  begin
    pos := pos1;
    if memoText[Pos] = ' ' then
      repeat inc(Pos);
      until (Pos > length(memoText)) or (memoText[Pos] <> ' ');
    Pos2 := Pos;
    if fTrimleft then Pos1 := Pos;
    repeat inc(Pos2);
    until (Pos2 > length(memoText)) or (memoText[Pos2] = ' ') or (memoText[Pos2] = '''') or (memoText[Pos2] = '"') or (memoText[Pos2] = chr($0D));

    Result := copy(memoText, Pos1, Pos2 - Pos1);
  end;
begin
  Lexems := TList.Create;
  try

    memoText := memo.Text;

    memo.Lines.BeginUpdate;
    SendMessage(memo.Handle, WM_SETREDRAW, 0, 0);
    Pos1 := 1;
    Pos2 := 1;
    fInQuote := false;

    memo.SelStart := 0;
    memo.SelLength := length(memoText);
    memo.SelAttributes.Color := clBlack;
    memo.SelAttributes.Style := [];

    LastLexem.Tag._start := -1;
    LastLexem.Comma._start := -1;

    while Pos1 < length(memoText) do
    begin
      if (memoText[Pos1] = '>') and (LastLexem.Tag._start <> -1) then
      begin
        New(PL);
        Lexems.Add(PL);
        PL^._type := ltTag;
        PL^._start := LastLexem.Tag._start;
        PL^._end := Pos1;
        LastLexem.Tag._start := -1;
      end
      else if ((memoText[Pos1] = '''') or (memoText[Pos1] = '"')) and (LastLexem.Comma._start <> -1) then
      begin
        New(PL);
        Lexems.Add(PL);
        PL^._type := ltComma;
        PL^._start := LastLexem.Comma._start;
        PL^._end := Pos1;
        LastLexem.Comma._start := -1;
      end
      else if (memoText[Pos1] = '<') then
        LastLexem.Tag._start := pos1 - 1
      else if (memoText[Pos1] = '''') or (memoText[Pos1] = '"') then
        LastLexem.Comma._start := pos1 - 1;
      inc(pos1);
      if (memoText[Pos1 - 1] = '<') and (memoText[Pos1] = '#') then
        doBold := true;
    end;

    for i := 0 to Lexems.Count - 1 do
      with PLexem(Lexems[i])^ do
        if _type = ltTag then
        begin
          memo.SelStart := _start;
          memo.SelLength := _end - _start;
          memo.SelAttributes.Color := clBlue;
          if doBold then
            memo.SelAttributes.Style := [fsBold]
          else
            memo.SelAttributes.Style := [];
        end;

    for i := 0 to Lexems.Count - 1 do
      with PLexem(Lexems[i])^ do
        if _type = ltComma then
        begin
          memo.SelStart := _start;
          memo.SelLength := _end - _start;
          memo.SelAttributes.Color := clTeal;
        end;

    memo.Lines.EndUpdate;
    SendMessage(memo.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(memo.Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN);
    memo.SelStart := 0;
    memo.SelLength := 0;

  finally
    for i := 0 to Lexems.Count - 1 do
      Dispose(Lexems[i]);
    Lexems.Free;
  end;
end;

end.
