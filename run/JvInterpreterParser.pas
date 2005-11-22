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
located at http://jvcl.sourceforge.net

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
  JvInterpreter, JvInterpreterConst, JvConsts;

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
  AssoIndices: array [0..511] of Word =
  (
    392,128,311,126,307,348,353,261,
     80,170, 97,336,264,294, 72, 73,
    220,146,360,  0, 33,288,343,282,
    162, 25,474,154, 47,276,329,302,
      0,377,360,198,344,132,352,  3,
    394, 32,252,277,473,203,123,363,
    316,262,139,447,312,383,131,393,
    184,  2,346,117,291,224,180,348,
    421, 97,286,277, 14, 74, 91,217,
    406, 82,355,  0,303,250, 56,498,
    335,495,494,237,127,239,145,250,
    495,221,401,380,459,431,481,428,
    433,362,403,116, 22,382, 53,  0,
     42,146,479, 59,451,116,204,343,
     44,361,446,160,358,445,263,423,
    386,325,294,405,303,330,337,256,
     40,382,188,290,174,274,259,146,
    208,436,111, 74,468,237,195,209,
    434, 10,178,323,485, 94,400,179,
    125,339, 94,225,422,113,143,490,
    472,496,282,191,376,473,269,143,
    285, 43,153,240,255,309,440,353,
    226,223, 38,157, 92,291, 12,480,
    457,174,284,442,268,228, 81,235,
    187,178,491, 35,138,212,183,176,
    130,444,419,318, 65,286,143,477,
    363,416, 41,410,365,148, 91,481,
    102,120,261,231,147,383,125, 39,
    369, 34,284,356, 70,231,138,124,
     77,496,138,143,  3,377,296,226,
     19,137,360,153, 88,282,170, 35,
    258,354,337,334,212, 43, 35,398,
    427,305,294,  8,385,278,313, 62,
    438,440,454,119, 26, 51,179,194,
     92, 66,338,275,151,322,322,189,
     98,432,236, 66,471,430,363,251,
    499,361,319,168,396,109,424,259,
    499,402,424,219,440, 36,363,418,
    207, 16,479,487, 42,401,158,186,
    274,294, 98,311,284, 24,447,482,
    448,207,321,330, 18,488,226,181,
     25,387, 20,488,406,315,207,381,
    216,434,452,319,353,163,328,464,
    413,264,477, 23,359, 64,416,424,
    364,482,425,311,269,186, 14,488,
    255,285,126, 40,434,130,359, 11,
    197,398, 88,123,276,257, 21,155,
    117, 48,339,227,259, 45,188,351,
     64,455,421,247,266,482,121, 37,
    407,368, 43, 61,387,482,256,181,
    455,122,385, 70,105,342,465, 90,
    468,235, 67, 19,396,222,470,485,
     91, 31,263, 94,229,163,421,378,
    175,292,484,305,479,128,125,211,
     21,449,  4,151,236, 84,427,290,
     23,296,456, 63, 36,107,257,492,
    214,244, 69,128,481, 24,325,258,
    227, 94, 76, 99,275, 84,480,364,
    154,356,270,179,167,156,214,476,
    314, 86,215, 96,296,468,334,484,
    431,264,120,448,443,368,285,119,
    212,196,362,338,192,123,126,350,
    338,455,171,286,495,240,171, 62,
    351,342,469,424,142,333,275, 18
   );

  // [peter schraut: added on 2005/08/14]
  // Created new LookupTable to work with
  // new Hashtable above. This array is
  // basically only a lookup table for
  // elements inside the WordList array below
  AssoValues: array [0..511] of SmallInt =
  (
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, 31, -1, -1, -1, -1, -1,
     -1, -1, 28, -1, -1, -1, -1, -1,
     -1, -1, 38, -1, -1, -1, -1, -1,
     30, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, 40, 20, -1, -1, -1, -1,
     -1, 18, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, 19, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, 34,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1,  7,
     32, -1, -1, 35, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, 41, -1, -1, -1, -1, -1, 36,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     13, -1, -1, -1, -1, -1, -1, 14,
     -1, -1, -1, -1,  1, -1, -1, -1,
     -1, -1,  8, 42,  2, -1, -1, -1,
     -1, -1, -1, -1, 37, -1, -1, -1,
     -1, -1, -1, -1, 15, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, 21, -1, -1,
     -1, -1, 50, -1, -1, -1, 25, -1,
     -1, -1, 27, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, 45, -1, -1,
     -1, -1, 11, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1,  3,
     -1, -1, -1, -1, 12, -1, -1, 23,
     -1, -1, -1, -1, -1, 22, -1, -1,
     -1, -1, -1, -1, 39, -1, -1, -1,
     -1, 48, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, 44, -1,
     -1, -1, 10, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     26,  4, -1, -1, -1, -1, -1, -1,
     46, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, 16, -1,
     -1, -1, -1, 29, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, 43, -1,
     -1, -1, -1, -1, -1, -1,  9, -1,
     -1, 24, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     49, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1,
     -1,  6, -1, -1, -1, -1, -1, -1,
     -1, -1, 33, -1, 47, -1, -1, -1,
     -1, -1, 17, -1, -1, -1, -1, -1,
     -1, -1,  0, -1, -1, -1, -1, -1,
     -1,  5, -1, -1, -1, -1, -1, 51,
     -1, -1, -1, -1, -1, -1, -1, -1
   );

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
function PaTokenizeTag(const TokenStr: string): TTokenKind;
var
  Len: Integer;
  HVal: Integer;
begin
  Result := P_UNKNOWN;
  HVal := -1;
  Len := Length(TokenStr);
  
  if (MIN_WORD_LENGTH <= Len) and (Len <= MAX_WORD_LENGTH) then
  begin
    HVal := Len;

    case HVal of
      1:
        HVal := HVal + AssoIndices[(Byte(TokenStr[1]) - Byte('a')) and 63];
      2:
        begin
          HVal := HVal + AssoIndices[(Byte(TokenStr[1]) - Byte('a')) and 63];
          HVal := HVal + AssoIndices[(Byte(TokenStr[2]) - Byte('a')) and 63];
        end;
      3:
        begin
          HVal := HVal + AssoIndices[(Byte(TokenStr[1]) - Byte('a')) and 63];
          HVal := HVal + AssoIndices[(Byte(TokenStr[2]) - Byte('a')) and 63];
          HVal := HVal + AssoIndices[(Byte(TokenStr[3]) - Byte('a')) and 63];
        end;

      4:
        begin
          HVal := HVal + AssoIndices[(Byte(TokenStr[1]) - Byte('a')) and 63];
          HVal := HVal + AssoIndices[(Byte(TokenStr[2]) - Byte('a')) and 63];
          HVal := HVal + AssoIndices[(Byte(TokenStr[3]) - Byte('a')) and 63];
          HVal := HVal + AssoIndices[(Byte(TokenStr[4]) - Byte('a')) and 63];
        end;
    else
      begin
        HVal := HVal + AssoIndices[(Byte(TokenStr[1]) - Byte('a')) and 63];
        HVal := HVal + AssoIndices[(Byte(TokenStr[2]) - Byte('a')) and 63];
        HVal := HVal + AssoIndices[(Byte(TokenStr[3]) - Byte('a')) and 63];
        HVal := HVal + AssoIndices[(Byte(TokenStr[4]) - Byte('a')) and 63];
        HVal := HVal + AssoIndices[(Byte(TokenStr[5]) - Byte('a')) and 63];
      end;
    end;

    HVal := HVal and 511;
    HVal := AssoValues[HVal];
  end;

  if HVal <> -1 then
  begin
    if Cmp(WordList[HVal].Token, TokenStr) then
      Result := WordList[HVal].TTyp;
  end;
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
    if T1 in ['('..'>'] then { #40..#62 }
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
            if not (Token[I] in StConstSymbols) then
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
          if not (Ci in StConstSymbols10) then
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
            { may be Identifier }
            if not (T1 in StIdFirstSymbols) then
              Result := ttUnknown
            else
            begin
              for I := 2 to L1 do
                if not (Token[I] in StIdSymbols) then
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
          while not (P[0] in [Lf, Cr, #0]) do
            Inc(P);
    end;
    while (P[0] in [' ', Lf, Cr, Tab]) do
      Inc(P);
  end;

begin
  { New Token }
  F := FPCPos;
  P := FPCPos;
  { Firstly skip spaces and remarks }
  repeat
    F1 := P;
    Skip;
  until F1 = P;
  F := P;
  if P[0] in StIdFirstSymbols then
  { token }
  begin
    while P[0] in StIdSymbols do
      Inc(P);
    SetString(Result, F, P - F);
  end
  else
  if P[0] in StConstSymbols10 then
  { number }
  begin
    while (P[0] in StConstSymbols10) or (P[0] = '.') do
    begin
      if (P[0] = '.') and (P[1] = '.') then
        Break;
      Inc(P);
    end;
    SetString(Result, F, P - F);
  end
  else
  if ((P[0] = '$') and
    (P[1] in StConstSymbols)) then
  { hex number }
  begin
    Inc(P);
    while P[0] in StConstSymbols do
      Inc(P);
    SetString(Result, F, P - F);
  end
  else
  if P[0] = '''' then
  { string constant }
  begin
    Inc(P);
    while P[0] <> #0 do
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
    (P[1] in StConstSymbols10)) then
  { Char constant }
  begin
    Inc(P);
    while P[0] in StConstSymbols10 do
      Inc(P);
    SetString(Result, F + 1, P - F - 1);
    Result := '''' + Chr(StrToInt(Result)) + '''';
  end
  else
  if P[0] in ['>', '=', '<', '.'] then
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

