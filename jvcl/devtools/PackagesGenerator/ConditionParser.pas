{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ConditionParser.pas, released on 2004-11-05.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausdaden att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s):
  Olivier Sannier [obones att altern dott org]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ConditionParser;

interface

type
  (*
    Conditional (boolean) expression parser for Package Generator

    Syntax:
      <b-exp>       ::= <b-term> ( [ "|" | "||" | "^" ] <b-term> )*
      <b-term>      ::= <b-notfactor> ( [ "&" | "&&" ] <b-notfactor> )*
      <b-notfactor> ::= <b-factor> | "!" <b-factor>
      <b-factor>    ::= "(" <b-exp> ")" | <ident>

    Symbols:
      !    : not
      &, &&: and
      |, ||: or
      ^    : xor

    Examples:
      "!A && (B || C) || (D && E) && F || G || H"
  *)

  TConditionParser = class(TObject)
  private
    FText: string;
    FIndex: Integer;
    Look: string;
    procedure Next;

    function GetToken: string;
    function BoolFactor: Boolean;
    function BoolNotFactor: Boolean;
    function BoolTerm: Boolean;
    function BoolExp: Boolean;
  protected
    procedure MissingRightParenthesis; virtual;
    // override GetIdentValue to allow TConditionParser to use your identifiers.
    function GetIdentValue(const Ident: string): Boolean; virtual; abstract;
  public
    // Parse the given text as a condition
    function Parse(const AText: string): Boolean;
    
    // Eval parses the given ACondition and returns the evaluated boolean value.
    class function Eval(const ACondition: string): Boolean;
  end;

implementation

{ TConditionParser }

function TConditionParser.GetToken: string;
{ A small lexer that has one and two char long symbols and everything else is
  an identifier. }
const
  SymbolOne = ['!', '^', '(', ')'];
  SymbolTwo = ['&', '|']; // => '&', '|' and '&&', '||' are allowed
var
  Idx, Len: Integer;
  Text: string;
begin
  // local variables are a lot faster
  Len := Length(FText);
  Idx := FIndex;
  Text := FText;

  Result := '';
  // skip white spaces
  while (Idx <= Len) and (Text[Idx] <= #32) do
    Inc(Idx);
  if Idx > Len then // text end reached
    Exit;

  FIndex := Idx; // save start index

  if Text[Idx] in SymbolOne then
  begin
    // one char
    Inc(Idx);
  end
  else
  if Text[Idx] in SymbolTwo then
  begin
    // one or two chars
    Inc(Idx);
    if Idx <= Len then // &&, ||
    begin
      if Text[Idx] = Text[Idx - 1] then
        Inc(Idx);
    end;
  end
  else
  begin
    // identifier
    Inc(Idx);
    while (Idx <= Len) and (Text[Idx] > #32) do
    begin
      if Text[Idx] in (SymbolOne + SymbolTwo) then
        Break;
      Inc(Idx);
    end;
  end;

  Result := Copy(Text, FIndex, Idx - FIndex);
  FIndex := Idx;
end;

procedure TConditionParser.Next;
begin
  Look := GetToken;
end;

function TConditionParser.BoolFactor: Boolean; // = b-factor
begin
  if Look = '(' then
  begin
    Next;
    Result := BoolExp;
    if Look <> ')' then
      MissingRightParenthesis;
  end
  else
    Result := GetIdentValue(Look);
  Next();
end;

function TConditionParser.BoolNotFactor: Boolean; // = b-notfactor
begin
  if Look = '!' then
  begin
    Next;
    Result := not BoolFactor
  end
  else
    Result := BoolFactor;
end;

function TConditionParser.BoolTerm: Boolean; // = b-term
var
  //Op: string;
  b: Boolean;
begin
  Result := BoolNotFactor;
  while (Look = '&') or (Look = '&&') do
  begin
    //Op := Look;
    Next;

    b := BoolNotFactor;
    //if (Op = '&') or (Op = '&&') then
      Result := Result and b
  end;
end;

function TConditionParser.BoolExp: Boolean; // = b-exp
var
  Op: string;
  b: Boolean;
begin
  Result := BoolTerm;
  while (Look = '|') or (Look = '||') or (Look = '^') do
  begin
    Op := Look;
    Next;

    b := BoolTerm;
    if (Op = '|') or (Op = '||') then
      Result := Result or b
    else
    if Look = '^' then
      Result := Result xor b;
  end;
end;

function TConditionParser.Parse(const AText: string): Boolean;
begin
  FText := AText;
  FIndex := 1;
  Next; // get first token into "Look"
  Result := BoolExp;
end;

procedure TConditionParser.MissingRightParenthesis;
begin
  // ignore error
end;

class function TConditionParser.Eval(const ACondition: string): Boolean;
var
  Parser: TConditionParser;
begin
  Parser := Self.Create;
  try
    Result := Parser.Parse(ACondition);
  finally
    Parser.Free;
  end;
end;

end.
