{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgRichEditUtils.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  TRichEdit Wrapping functions

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgRichEditUtils;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Graphics, ComCtrls, Classes;

procedure AddNl(RE: TRichEdit);
procedure AddText(RE: TRichEdit; const Str: string; TxtSize: Integer;
  TxtStyle: TFontStyles; TxtColor: TColor);
procedure AddTextNl(RE: TRichEdit; const Str: string; TxtSize: Integer;
  TxtStyle: TFontStyles; TxtColor: TColor);
procedure DoSyntaxHighlight(Memo: TRichEdit);

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

procedure AddNl(RE: TRichEdit);
begin
  RE.Lines.Add('');
end;

procedure AddText(RE: TRichEdit; const Str: string; TxtSize: Integer;
  TxtStyle: TFontStyles; TxtColor: TColor);
var
  Pos1, Pos2: Integer;
begin
  with RE, RE.Lines do
  begin
    Pos1 := Length(Text);
    if Pos1 = 0 then
      Text := Str
    else
      Lines[Lines.Count - 1] := Lines[Lines.Count - 1] + Str;
    Pos2 := Length(Text);
    SelStart := Pos1 - 2;
    SelLength := Pos2 - Pos1 + 1;

    SelAttributes.Size := TxtSize;
    SelAttributes.Style := TxtStyle;
    SelAttributes.Color := TxtColor;
  end;
end;

procedure AddTextNl(RE: TRichEdit; const Str: string; TxtSize: Integer;
  TxtStyle: TFontStyles; TxtColor: TColor);
begin
  RE.Lines.Add('');
  AddText(RE, Str, TxtSize, TxtStyle, TxtColor);
end;

procedure DoSyntaxHighlight(Memo: TRichEdit);
type
  THData = record
    Word: string;
    Color: TColor;
    Bold: Boolean;
  end;

  TLexemKind = (ltTag, ltComma);

  PLexem = ^TLexem;
  TLexem = record
    StartPos, EndPos: Integer;
    Kind: TLexemKind;
  end;

  TLastLexem = record
    Tag, Comma: TLexem;
  end;

{ (rom) disabled  not used
const
  cWordsCount = 6;
  cWord: array [1..cWordsCount] of PChar =
    ('<html>', '<body>', 'function', 'var', 'script', '<Table');
  cColor: array [1..cWordsCount] of TColor =
    ($500000, $500000, $500000, $500000, $000060, $606000);
  cBold: array [1..cWordsCount] of Boolean =
    (False, False, True, True, False, False);
}
var
  Pos1: Integer;
  i: Integer;
  doBold: Boolean;
  MemoText: string;
  Lexems: TList;
  LastLexem: TLastLexem;
  PL: PLexem;

  function GetNextLexem(var Pos1, Pos2: Integer; Trimleft: Boolean): string;
  var
    Pos: Integer;
  begin
    Pos := Pos1;
    if MemoText[Pos] = ' ' then
      repeat
        Inc(Pos);
      until (Pos > Length(MemoText)) or (MemoText[Pos] <> ' ');
    Pos2 := Pos;
    if Trimleft then
      Pos1 := Pos;
    repeat
      Inc(Pos2);
    until (Pos2 > Length(MemoText)) or (MemoText[Pos2] = ' ') or
      (MemoText[Pos2] = '''') or (MemoText[Pos2] = '"') or
      (MemoText[Pos2] = Chr($0D));

    Result := Copy(MemoText, Pos1, Pos2 - Pos1);
  end;

begin
  doBold := False;
  Lexems := TList.Create;
  try
    MemoText := Memo.Text;

    Memo.Lines.BeginUpdate;
    SendMessage(Memo.Handle, WM_SETREDRAW, 0, 0);
    Pos1 := 1;

    Memo.SelStart := 0;
    Memo.SelLength := Length(MemoText);
    Memo.SelAttributes.Color := clBlack;
    Memo.SelAttributes.Style := [];

    LastLexem.Tag.StartPos := -1;
    LastLexem.Comma.StartPos := -1;

    while Pos1 < Length(MemoText) do
    begin
      if (MemoText[Pos1] = '>') and (LastLexem.Tag.StartPos <> -1) then
      begin
        New(PL);
        Lexems.Add(PL);
        PL^.Kind := ltTag;
        PL^.StartPos := LastLexem.Tag.StartPos;
        PL^.EndPos := Pos1;
        LastLexem.Tag.StartPos := -1;
      end
      else
      if ((MemoText[Pos1] = '''') or (MemoText[Pos1] = '"')) and
        (LastLexem.Comma.StartPos <> -1) then
      begin
        New(PL);
        Lexems.Add(PL);
        PL^.Kind := ltComma;
        PL^.StartPos := LastLexem.Comma.StartPos;
        PL^.EndPos := Pos1;
        LastLexem.Comma.StartPos := -1;
      end
      else
      if MemoText[Pos1] = '<' then
        LastLexem.Tag.StartPos := Pos1 - 1
      else
      if (MemoText[Pos1] = '''') or (MemoText[Pos1] = '"') then
        LastLexem.Comma.StartPos := Pos1 - 1;
      Inc(Pos1);
      if (MemoText[Pos1 - 1] = '<') and (MemoText[Pos1] = '#') then
        doBold := True;
    end;

    for i := 0 to Lexems.Count - 1 do
      with PLexem(Lexems[i])^ do
        if Kind = ltTag then
        begin
          Memo.SelStart := StartPos;
          Memo.SelLength := EndPos - StartPos;
          Memo.SelAttributes.Color := clBlue;
          if doBold then
            Memo.SelAttributes.Style := [fsBold]
          else
            Memo.SelAttributes.Style := [];
        end;

    for i := 0 to Lexems.Count - 1 do
      with PLexem(Lexems[i])^ do
        if Kind = ltComma then
        begin
          Memo.SelStart := StartPos;
          Memo.SelLength := EndPos - StartPos;
          Memo.SelAttributes.Color := clTeal;
        end;

    Memo.Lines.EndUpdate;
    SendMessage(Memo.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Memo.Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN);
    Memo.SelStart := 0;
    Memo.SelLength := 0;

  finally
    for i := 0 to Lexems.Count - 1 do
      Dispose(Lexems[i]);
    Lexems.Free;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

