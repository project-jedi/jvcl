{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreterParser.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s): Peter Schraut (http://www.console-de.de)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description : Parser for JVCL Interpreter version 2

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{ history (JVCL Library versions):

Upcoming JVCL 3.00
    - peter schraut added shl, shr and xor support
}

unit JvInterpreterParser;

{$I jvcl.inc}

interface

uses
  SysUtils;

type
  TTokenKind = type Integer;

  TJvInterpreterParser = class(TObject)
  private
    FSource: string;
    FPCPos: PChar; { current parse position }
    procedure SetSource(const Value: string);
    function GetPos: Integer;
    procedure SetPos(Value: Integer);
  public
    { Token - returns next token }
    function Token: string;
    procedure Init;
    property Source: string read FSource write SetSource;
    property PCPos: PChar read FPCPos write FPCPos;
    property Pos: Integer read GetPos write SetPos;
  end;

  //JvInterpreterError = class(Exception)
  //
  //end;

  TPriorLevel = 0..8;

{ tokenizer }

function TokenTyp(const Token: string): TTokenKind;
{ return operation priority }
function Prior(const TTyp: TTokenKind): TPriorLevel;
function TypToken(const TTyp: TTokenKind): string;

{ Token types }
const
  ttUnknown = -1; { unknown error - internal error in most cases - for debugging }
  ttEmpty = 0; { end of file - eof }
  ttIdentifier = 10; { Identifier }
  ttInteger = 11; { Integer constant }
  ttDouble = 12; { double constant }
  ttString = 13; { string constant }
  ttBoolean = 14; { boolean - variable type }

  ttLB = 40; { ( }
  ttRB = 41; { ) }
  ttCol = 42; { , }
  ttPoint = 43; { . }
  ttColon = 44; { : }
  ttSemicolon = 45; { ; }
  ttLS = 46; { [ }
  ttRS = 47; { ] }
  ttDoublePoint = 48; {..}
  ttDoubleQuote = 49; {"}

  ttFalse = 63; { false }
  ttTrue = 65; { true }

  ttBegin = 66; { begin }
  ttEnd = 67; { end }
  ttIf = 68; { if }
  ttThen = 69; { then }
  ttElse = 70; { else }
  ttWhile = 71; { while }
  ttDo = 72; { do }
  ttRepeat = 73; { repeat }
  ttUntil = 74; { until }
  ttProcedure = 75; { procedure }
  ttFunction = 76; { function }
  ttFor = 77; { for }
  ttTo = 78; { to }
  ttBreak = 79; { break }
  ttContinue = 80; { continue }
  ttVar = 81; { var }
  ttTry = 82; { try }
  ttFinally = 83; { finally }
  ttExcept = 84; { except }
  ttOn = 85; { on }
  ttRaise = 86; { raise }
  ttExternal = 87; { external }
  ttUnit = 88; { unit }
  ttUses = 89; { uses }
  ttConst = 90; { Const }
  ttPublic = 91; { Public }
  ttPrivate = 92; { Private }
  ttProtected = 93; { Protected }
  ttPublished = 94; { Published }
  ttProperty = 95; { Property }
  ttClass = 96; { Class }
  ttType = 97; { Type }
  ttInterface = 98; { Interface }
  ttImplementation = 99; { Implementation }
  ttExit = 100; { Exit }
  ttArray = 101; { Array }
  ttOf = 102; { Of }
  ttCase = 103; { Case }
  ttProgram = 104; { Program }
  ttIn = 105; { In }
  ttRecord = 106; { Record }
  ttDownTo = 107; { DownTo }


  { priority 8 - highest }
  ttNot = 21; { not }

  { priority 6 }
  ttMul = 22; { * }
  ttDiv = 23; { / }
  ttIntDiv = 24; { div }
  ttMod = 25; { mod }

  { priority 5 }
  ttAnd = 26; { and }

  { priority 4 }
  ttPlus = 27; { + }
  ttMinus = 28; { - }
  ttOr = 29; { or }

  { priority 3 }
  ttEqu = 30; { = }
  ttGreater = 31; { > }
  ttLess = 32; { < }
  ttNotEqu = 33; { <> }

  { priority 2 }
  ttEquGreater = 34; { >= }
  ttEquLess = 35; { <= }

  { priority 6 }
  ttShl = 36; { shl } // [peter schraut: added on 2005/08/14]
  ttShr = 37; { shr } // [peter schraut: added on 2005/08/14]

  { priority 3 }
  ttXor = 38; { xor } // [peter schraut: added on 2005/08/14]

  { priority 1 - lowest }
  { nothing }

  priorNot = 8;
  priorMul = 6;
  priorDiv = 6;
  priorIntDiv = 6;
  priorMod = 6;
  priorAnd = 5;
  priorPlus = 4;
  priorMinus = 4;
  priorOr = 4;
  priorEqu = 3;
  priorGreater = 3;
  priorLess = 3;
  priorNotEqu = 3;
  priorEquGreater = 2;
  priorEquLess = 2;
  priorShl = 6; // [peter schraut: added on 2005/08/14]
  priorShr = 6; // [peter schraut: added on 2005/08/14]
  priorXor = 3; // [peter schraut: added on 2005/08/14]

  ttFirstExpression = 10; { tokens for expression }
  ttLastExpression = 59; {                       }

  { keywords }
  kwTRUE = 'true';
  kwFALSE = 'false';
  kwOR = 'or';
  kwAND = 'and';
  kwNOT = 'not';
  kwDIV = 'div';
  kwMOD = 'mod';
  kwBEGIN = 'begin';
  kwEND = 'end';
  kwIF = 'if';
  kwTHEN = 'then';
  kwELSE = 'else';
  kwWHILE = 'while';
  kwDO = 'do';
  kwREPEAT = 'repeat';
  kwUNTIL = 'until';
  kwPROCEDURE = 'procedure';
  kwFUNCTION = 'function';
  kwFOR = 'for';
  kwTO = 'to';
  kwBREAK = 'break';
  kwCONTINUE = 'continue';
  kwVAR = 'var';
  kwTRY = 'try';
  kwFINALLY = 'finally';
  kwEXCEPT = 'except';
  kwON = 'on';
  kwRAISE = 'raise';
  kwEXTERNAL = 'external';
  kwUNIT = 'unit';
  kwUSES = 'uses';
  kwCONST = 'const';
  kwPUBLIC = 'public';
  kwPRIVATE = 'private';
  kwPROTECTED = 'protected';
  kwPUBLISHED = 'published';
  kwPROPERTY = 'property';
  kwCLASS = 'class';
  kwTYPE = 'type';
  kwINTERFACE = 'interface';
  kwIMPLEMENTATION = 'implementation';
  kwEXIT = 'exit';
  kwARRAY = 'array';
  kwOF = 'of';
  kwCASE = 'case';
  kwPROGRAM = 'program';
  kwIN = 'in';
  kwRECORD = 'record';
  kwDOWNTO = 'downto';
  kwNIL = 'nil';
  kwSHL = 'shl'; // [peter schraut: added on 2005/08/14]
  kwSHR = 'shr'; // [peter schraut: added on 2005/08/14]
  kwXOR = 'xor'; // [peter schraut: added on 2005/08/14]

  { directives }
  drNAME = 'name';
  drINDEX = 'index';

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JvInterpreter, JvInterpreterConst, JvConsts, Windows;

const
  K = '''';

{*********************** tokenizer ***********************}
{ modified algorithm from mozilla source }

type
  TTokenTag = record
    // (rom) changed to PChar to get rid of hidden initialization section
    Token: PChar;
    TTyp: TTokenKind;
  end;

const
  P_UNKNOWN = -1;
  MIN_WORD_LENGTH = 2;
  MAX_WORD_LENGTH = 14; { = length('implementation') }


  // [peter schraut: added on 2005/08/14]
  // Created new HashTable to avoid collisions
  // with added keywords such as shl, shr and xor
  // Mantis 3333 (ivan_ra): optimized version
   AssoIndices: array [0..31] of Integer = (
        { 0   1   2   3   4   5   6   7   8   9 }
    {00} 50, 80, 25, 13, 92, 71, 87, 61, 91, 99,
    {10} 73, 95, 27,  7, 16,  1, 96, 41, 91, 99,
    {20} 19, 15, 72,  1, 50, 30,  9,  6, 45, 27,
    {30} 79, 61);

   AssoValues: array [0..255] of Integer = (
        { 0   1   2   3   4   5   6   7   8   9 }
    {00} -1, -1, -1, -1, -1, -1, 44, 10, -1, -1,
    {10} 37, -1, -1, -1, -1,  7, -1, -1, -1, -1,
    {20} -1, -1, -1, 27, -1, -1, -1, -1, -1, -1,
    {30} -1, 41, 26, -1, -1, 20, -1, -1, -1, 28,
    {40} -1, 30, 39, -1, -1, -1, -1, 13, -1, -1,
    {50} -1, -1, -1, -1, -1, -1, -1,  1, -1, -1,
    {60} -1, -1, -1, -1, -1, 12, -1, -1, -1, -1,
    {70} -1, -1,  6, -1, -1, -1, -1, -1, -1, -1,
    {80} 34, -1, -1, -1, -1, -1,  3, -1, -1, 49,
    {90} -1, -1, 45, -1, -1, -1, -1, -1, -1, -1,
    {100} 2, -1, 51, -1, -1, -1, -1, 46, -1, -1,
    {110}-1, -1, 17, -1, -1, -1, 36, -1, 11, -1,
    {120}-1, -1, 35, 48, -1, -1, -1, -1,  8, -1,
    {130}-1, 32, -1, 19, -1, -1, -1,  5, -1, -1,
    {140}40, -1, -1, -1, -1, -1, -1, -1, 21, -1,
    {150}22, -1, 31, -1, -1, -1, -1, -1, -1, 16,
    {160}43, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    {170}-1, -1, 18, -1, -1, -1, -1, 47, -1, -1,
    {180}-1, -1, -1, -1, -1, -1, -1, 42, -1, -1,
    {190}-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    {200}-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    {210}-1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    {220}29, -1, -1, 25,  4, 15, 24, -1, -1, -1,
    {230}-1, -1, 33, -1, -1,  9, -1, 50, -1, 14,
    {240}-1, -1, -1, 23, -1, -1, 38, -1, -1, -1,
    {250}-1, -1, -1, -1, -1,  0);

  WordList: array [0..51] of TTokenTag = (
    (Token: kwTRUE; TTyp: ttTrue),
    (Token: kwFALSE; TTyp: ttFalse),
    (Token: kwOR; TTyp: ttOr),
    (Token: kwAND; TTyp: ttAnd),
    (Token: kwNOT; TTyp: ttNot),
    (Token: kwDIV; TTyp: ttIntDiv),
    (Token: kwMOD; TTyp: ttMod),
    (Token: kwBEGIN; TTyp: ttBegin),
    (Token: kwEND; TTyp: ttEnd),
    (Token: kwIF; TTyp: ttIf),
    (Token: kwTHEN; TTyp: ttThen),
    (Token: kwELSE; TTyp: ttElse),
    (Token: kwWHILE; TTyp: ttWhile),
    (Token: kwDO; TTyp: ttDo),
    (Token: kwREPEAT; TTyp: ttRepeat),
    (Token: kwUNTIL; TTyp: ttUntil),
    (Token: kwPROCEDURE; TTyp: ttProcedure),
    (Token: kwFUNCTION; TTyp: ttFunction),
    (Token: kwFOR; TTyp: ttFor),
    (Token: kwTO; TTyp: ttTo),
    (Token: kwBREAK; TTyp: ttBreak),
    (Token: kwCONTINUE; TTyp: ttContinue),
    (Token: kwVAR; TTyp: ttVar),
    (Token: kwTRY; TTyp: ttTry),
    (Token: kwFINALLY; TTyp: ttFinally),
    (Token: kwEXCEPT; TTyp: ttExcept),
    (Token: kwON; TTyp: ttOn),
    (Token: kwRAISE; TTyp: ttRaise),
    (Token: kwEXTERNAL; TTyp: ttExternal),
    (Token: kwUNIT; TTyp: ttUnit),
    (Token: kwUSES; TTyp: ttUses),
    (Token: kwCONST; TTyp: ttConst),
    (Token: kwPUBLIC; TTyp: ttPublic),
    (Token: kwPRIVATE; TTyp: ttPrivate),
    (Token: kwPROTECTED; TTyp: ttProtected),
    (Token: kwPUBLISHED; TTyp: ttPublished),
    (Token: kwPROPERTY; TTyp: ttProperty),
    (Token: kwCLASS; TTyp: ttClass),
    (Token: kwTYPE; TTyp: ttType),
    (Token: kwINTERFACE; TTyp: ttInterface),
    (Token: kwIMPLEMENTATION; TTyp: ttImplementation),
    (Token: kwEXIT; TTyp: ttExit),
    (Token: kwARRAY; TTyp: ttArray),
    (Token: kwOF; TTyp: ttOf),
    (Token: kwCASE; TTyp: ttCase),
    (Token: kwPROGRAM; TTyp: ttProgram),
    (Token: kwIN; TTyp: ttIn),
    (Token: kwRECORD; TTyp: ttRecord),
    (Token: kwDOWNTO; TTyp: ttDownTo),
    (Token: kwSHL; TTyp: ttShl), // [peter schraut: added on 2005/08/14]
    (Token: kwSHR; TTyp: ttShr), // [peter schraut: added on 2005/08/14]
    (Token: kwXOR; TTyp: ttXor)  // [peter schraut: added on 2005/08/14]
    );

{ convert string into token number using hash tables }
// [peter schraut: added on 2005/08/14]
//  Made a few changes to PaTokenizeTag to work with new hashtable.
// Mantis 3333 (ivan_ra): optimized version
function PaTokenizeTag(const TokenStr: string): TTokenKind;
var
  Len, I: Integer;
  HVal: Integer;
begin
  Result := P_UNKNOWN;
  HVal := -1;
  Len := Length(TokenStr);

  if (MIN_WORD_LENGTH <= Len) and (Len <= MAX_WORD_LENGTH) then
  begin
    HVal := Len;
    for I:=1 to Len do
    begin
      HVal := HVal + AssoIndices[(Byte(TokenStr[I]) - Byte('a')) and $1F];
      if I = 3 then
        Break;
    end;
    HVal := HVal + AssoIndices[(Byte(TokenStr[Len]) - Byte('a')) and $1F];
    HVal := HVal and 255; {High(AssoValues)}
    HVal := AssoValues[HVal];
  end;

  if HVal <> -1 then
    if Cmp(WordList[HVal].Token, TokenStr) then
      Result := WordList[HVal].TTyp;
end;

const
  { !"#$%&'()*+,-./0123456789:;<=>? }
  Asso1Values: array [' '..'?'] of Integer =
    (-1, -1, -1, -1, -1, -1, -1, -1,
     ttLB, ttRB, ttMul, ttPlus, ttCol, ttMinus, ttPoint, ttDiv,
     ttInteger, ttInteger, ttInteger, ttInteger, ttInteger,
     ttInteger, ttInteger, ttInteger, ttInteger, ttInteger,
     ttColon, ttSemicolon, ttLess, ttEqu, ttGreater, -1);

{######################## tokenizer ########################}

function TokenTyp(const Token: string): TTokenKind;
var
  I: Integer;
  L1: Integer;
  T1: Char;
  Ci: Char;
  Point: Boolean;
label { Sorry about labels and gotos - for speed-ups only }
  Any, NotNumber;
begin
  L1 := Length(Token);
  if L1 = 0 then
  begin
    Result := ttEmpty;
    Exit;
  end;
  T1 := Token[1];
  if L1 = 1 then
  begin
    { Result := pa_tokenize_1tag(Token[1]);
    if Result = -1 then goto Any; }
    if CharInSet(T1, ['('..'>']) then { #40..#62 }
      Result := Asso1Values[T1]
    else
    if T1 = '[' then
      Result := ttLS
    else
    if T1 = ']' then
      Result := ttRS
    else
    if T1 = '"' then
      Result := ttDoubleQuote
    else
      goto Any;
  end
  else
    case T1 of
      '.':
        { may be '..' }
        begin
          if Token[2] = '.' then
            Result := ttDoublePoint
          else
            goto Any;
        end;
      '$':
        { may be hex constant }
        begin
          for I := 2 to L1 do
            if not CharInSet(Token[I], StConstSymbols) then
              goto Any;
          Result := ttInteger;
        end;
      '<':
        if L1 = 2 then
          case Token[2] of
            '=': Result := ttEquLess;
            '>': Result := ttNotEqu;
          else
            goto Any;
          end
        else
          goto Any;
      '>':
        if (L1 = 2) and (Token[2] = '=') then
          Result := ttEquGreater
        else
          goto Any;
    else
      begin
        Any: { !!LABEL!! }

        Point := False;
        for I := 1 to L1 do
        begin
          Ci := Token[I];
          if Ci = '.' then
            if Point then
              goto NotNumber {two Points in lexem}
            else
              Point := True
          else
          if not CharInSet(Ci, StConstSymbols10) then
            goto NotNumber { not number }
        end;
        if Point then
          Result := ttDouble
        else
          Result := ttInteger;
        Exit;

        NotNumber: { !!LABEL!! }

        if (L1 >= 2) and (Token[1] = '''') and (Token[L1] = '''') then
          Result := ttString
        else
        begin
          { keywords }
          Result := PaTokenizeTag(Token);

          if Result <> -1 then
          begin
          end
          else
            { may be Identifier }               // National symbols for OLE automation
            if not (CharInSet(T1, StIdFirstSymbols) or IsCharAlpha(T1)) then
              Result := ttUnknown
            else
            begin
              for I := 2 to L1 do
                if not (CharInSet(Token[I], StIdSymbols) or IsCharAlpha(Token[I])) then
                begin
                  Result := ttUnknown;
                  Exit;
                end;
              Result := ttIdentifier;
            end;
          end;
        end;
    end;
end;

function TypToken(const TTyp: TTokenKind): string;
begin
  Result := '?? not implemented !!'; { DEBUG !! }
end;

function Prior(const TTyp: TTokenKind): TPriorLevel;
const
  Priors: array [ttNot..ttXor] of TPriorLevel =
    (priorNot, priorMul, priorDiv, priorIntDiv, priorMod, priorAnd, priorPlus,
     priorMinus, priorOr, priorEqu, priorGreater, priorLess,
     priorNotEqu, priorEquGreater, priorEquLess,
     priorShl, priorShr, priorXor); // [peter schraut: added priorShl, priorShr, priorXor on 2005/08/14]
begin
  //if TTyp in [ttNot..ttEquLess] then
  if TTyp in [ttNot..ttXor] then  // [peter schraut: expanded to ttXor on 2005/08/14]
    Result := Priors[TTyp]
  else
    Result := 0;
end;

//=== { TJvInterpreterParser } ===============================================

procedure TJvInterpreterParser.SetSource(const Value: string);
begin
  FSource := Value;
  Init;
end;

procedure TJvInterpreterParser.Init;
begin
  FPCPos := PChar(FSource);
end;

function TJvInterpreterParser.Token: string;
var
  P, F: PChar;
  F1: PChar;
  I: Integer;
  PrevPoint:boolean;
//  PointCount: Integer;

  procedure Skip;
  begin
    case P[0] of
      '{':
        begin
          F := StrScan(P + 1, '}');
          if F = nil then
            JvInterpreterError(ieBadRemark, P - PChar(FSource));
          P := F + 1;
        end;
      '(':
        if P[1] = '*' then
        begin
          F := P + 2;
          while True do
          begin
            F := StrScan(F, '*');
            if F = nil then
              JvInterpreterError(ieBadRemark, P - PChar(FSource));
            if F[1] = ')' then
            begin
              Inc(F);
              Break;
            end;
            Inc(F);
          end;
          P := F + 1;
        end;
      '}':
        JvInterpreterError(ieBadRemark, P - PChar(FSource));
      '*':
        if (P[1] = ')') then
          JvInterpreterError(ieBadRemark, P - PChar(FSource));
      '/':
        if (P[1] = '/') then
          while not CharInSet(P[0], [Lf, Cr, #0]) do
            Inc(P);
    end;
    while CharInSet(P[0], [' ', Lf, Cr, Tab]) do
      Inc(P);
  end;

begin
  { New Token }
  F := FPCPos;
  P := FPCPos;
  PrevPoint:=false;
  if (P > PChar(FSource))
  and (P[-1] = '.')
  then
    PrevPoint := true;

  { Firstly skip spaces and remarks }
  repeat
    F1 := P;
    Skip;
  until F1 = P;
  F := P;                          // National symbols for OLE automation
  if CharInSet(P[0], StIdFirstSymbols) or PrevPoint and IsCharAlpha(P[0]) then
  { token }
  begin
    while CharInSet(P[0], StIdSymbols) or PrevPoint and IsCharAlpha(P[0]) do
      Inc(P);
    SetString(Result, F, P - F);
  end
  else
  if CharInSet(P[0], StConstSymbols10) then
  { number }
  begin
    while CharInSet(P[0], StConstSymbols10) or (P[0] = '.') do
    begin
      if (P[0] = '.') and (P[1] = '.') then
        Break;
      Inc(P);
    end;
    SetString(Result, F, P - F);
  end
  else
  if ((P[0] = '$') and
    CharInSet(P[1], StConstSymbols)) then
  { hex number }
  begin
    Inc(P);
    while CharInSet(P[0], StConstSymbols) do
      Inc(P);
    SetString(Result, F, P - F);
  end
  else
  if P[0] = '''' then
  { string constant }
  begin
    Inc(P);
    while not CharInSet(P[0], [Lf, Cr, #0]) do
    begin
      if P[0] = '''' then
        if P[1] = '''' then
          Inc(P)
        else
          Break;
      Inc(P);
    end;
    Inc(P);
    SetString(Result, F, P - F);
    I := 2;
    while I < Length(Result) - 1 do
    begin
      if Result[I] = '''' then
        Delete(Result, I, 1);
      Inc(I);
    end;
  end
  else
  if ((P[0] = '#') and
    CharInSet(P[1], StConstSymbols10)) then
  { Char constant }
  begin
    Inc(P);
    while CharInSet(P[0], StConstSymbols10) do
      Inc(P);
    SetString(Result, F + 1, P - F - 1);
    Result := '''' + Chr(StrToInt(Result)) + '''';
  end
  else
  if CharInSet(P[0], ['>', '=', '<', '.']) then
  begin
    if (P[0] = '.') and (P[1] = '.') then
    begin
      Result := '..';
      Inc(P, 2);
    end
    else
    if (P[0] = '>') and (P[1] = '=') then
    begin
      Result := '>=';
      Inc(P, 2);
    end
    else
    if (P[0] = '<') and (P[1] = '=') then
    begin
      Result := '<=';
      Inc(P, 2);
    end
    else
    if (P[0] = '<') and (P[1] = '>') then
    begin
      Result := '<>';
      Inc(P, 2);
    end
    else
    begin
      Result := P[0];
      Inc(P);
    end;
  end
  else
  if P[0] = #0 then
    Result := ''
  else
  begin
    Result := P[0];
    Inc(P);
  end;

  FPCPos := P;
end;

function TJvInterpreterParser.GetPos: Integer;
begin
  Result := FPCPos - PChar(FSource);
end;

procedure TJvInterpreterParser.SetPos(Value: Integer);
begin
  FPCPos := PChar(FSource) + Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
