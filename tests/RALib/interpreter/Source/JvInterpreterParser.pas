{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Parser for JVCL Interpreter version 2

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvInterpreterParser;

interface

uses SysUtils;

type

  TTokenTyp = type integer;

  TJvInterpreterParser = class
  private
    FSource : string;
    FPCPos : PChar; { current parse position }
    procedure SetSource(Value: string);
    function GetPos: integer;
    procedure SetPos(Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
   { Token - returns next token }
    function Token : string;
    procedure Init;
    property Source: string read FSource write SetSource;
    property PCPos: PChar read FPCPos write FPCPos;
    property Pos: integer read GetPos write SetPos;
  end;

  //JvInterpreterError = class(Exception)
  // 
  //end;
  
  TPriorLevel = 0..8;

 { tokenizer }

  function TokenTyp(const Token : string): TTokenTyp;
 { return operation priority }
  function Prior(const TTyp : TTokenTyp) : TPriorLevel;
  function TypToken(const TTyp : TTokenTyp): string;


 { Token types }
const
  ttUnknown      = -1; { unknown error - internal error in most cases - for debugging }
  ttEmpty        =  0; { end of file - eof }
  ttIdentifer    = 10; { identifer }
  ttInteger      = 11; { integer constant }
  ttDouble       = 12; { double constant }
  ttString       = 13; { string constant }
  ttBoolean      = 14; { boolean - variable type }

  ttLB           = 40; { ( }
  ttRB           = 41; { ) }
  ttCol          = 42; { , }
  ttPoint        = 43; { . }
  ttColon        = 44; { : }
  ttSemicolon    = 45; { ; }
  ttLS           = 46; { [ }
  ttRS           = 47; { ] }

  ttFalse        = 63; { false }
  ttTrue         = 65; { true }

  ttBegin        = 66; { begin }
  ttEnd          = 67; { end }
  ttIf           = 68; { if }
  ttThen         = 69; { then }
  ttElse         = 70; { else }
  ttWhile        = 71; { while }
  ttDo           = 72; { do }
  ttRepeat       = 73; { repeat }
  ttUntil        = 74; { until }
  ttProcedure    = 75; { procedure }
  ttFunction     = 76; { function }
  ttFor          = 77; { for }
  ttTo           = 78; { to }
  ttBreak        = 79; { break }
  ttContinue     = 80; { continue }
  ttVar          = 81; { var }
  ttTry          = 82; { try }
  ttFinally      = 83; { finally }
  ttExcept       = 84; { except }
  ttOn           = 85; { on }
  ttRaise        = 86; { raise }
  ttExternal     = 87; { external }
  ttUnit         = 88; { unit }
  ttUses         = 89; { uses }
  ttConst          = 90; { Const }
  ttPublic         = 91; { Public }
  ttPrivate        = 92; { Private }
  ttProtected      = 93; { Protected }
  ttPublished      = 94; { Published }
  ttProperty       = 95; { Property }
  ttClass          = 96; { Class }
  ttType           = 97; { Type }
  ttInterface      = 98; { Interface }
  ttImplementation = 99; { Implementation }
  ttExit           = 100; { Exit }
  ttArray          = 101; { Array }
  ttOf             = 102; { Of }
  ttCase           = 103; { Case }
  ttProgram        = 104; { Program }
  ttIn             = 105; { In }

 { priority 8 - highest }
  ttNot          = 21; { not }
 { priority 6 }
  ttMul          = 22; { * }
  ttDiv          = 23; { / }
  ttIntDiv       = 24; { div }
  ttMod          = 25; { mod }
 { priority 5 }
  ttAnd          = 26; { and }
 { priority 4 }
  ttPlus         = 27; { + }
  ttMinus        = 28; { - }
  ttOr           = 29; { or }
 { priority 3 }
  ttEqu          = 30; { = }
  ttGreater      = 31; { > }
  ttLess         = 32; { < }
  ttNotEqu       = 33; { <> }
 { priority 2 }
  ttEquGreater   = 34; { >= }
  ttEquLess      = 35; { <= }
 { priority 1 - lowest }
  { nothing }

  priorNot        = 8;
  priorMul        = 6;
  priorDiv        = 6;
  priorIntDiv     = 6;
  priorMod        = 6;
  priorAnd        = 5;
  priorPlus       = 4;
  priorMinus      = 4;
  priorOr         = 4;
  priorEqu        = 3;
  priorGreater    = 3;
  priorLess       = 3;
  priorNotEqu     = 3;
  priorEquGreater = 2;
  priorEquLess    = 2;

  ttFirstExpression = 10; { tokens for expression }
  ttLastExpression  = 59; {                       }

 { keywords }
  kwTRUE           =  'true'          ;
  kwFALSE          =  'false'         ;
  kwOR             =  'or'            ;
  kwAND            =  'and'           ;
  kwNOT            =  'not'           ;
  kwDIV            =  'div'           ;
  kwMOD            =  'mod'           ;
  kwBEGIN          =  'begin'         ;
  kwEND            =  'end'           ;
  kwIF             =  'if'            ;
  kwTHEN           =  'then'          ;
  kwELSE           =  'else'          ;
  kwWHILE          =  'while'         ;
  kwDO             =  'do'            ;
  kwREPEAT         =  'repeat'        ;
  kwUNTIL          =  'until'         ;
  kwPROCEDURE      =  'procedure'     ;
  kwFUNCTION       =  'function'      ;
  kwFOR            =  'for'           ;
  kwTO             =  'to'            ;
  kwBREAK          =  'break'         ;
  kwCONTINUE       =  'continue'      ;
  kwVAR            =  'var'           ;
  kwTRY            =  'try'           ;
  kwFINALLY        =  'finally'       ;
  kwEXCEPT         =  'except'        ;
  kwON             =  'on'            ;
  kwRAISE          =  'raise'         ;
  kwEXTERNAL       =  'external'      ;
  kwUNIT           =  'unit'          ;
  kwUSES           =  'uses'          ;
  kwCONST          =  'const'         ;
  kwPUBLIC         =  'public'        ;
  kwPRIVATE        =  'private'       ;
  kwPROTECTED      =  'protected'     ;
  kwPUBLISHED      =  'published'     ;
  kwPROPERTY       =  'property'      ;
  kwCLASS          =  'class'         ;
  kwTYPE           =  'type'          ;
  kwINTERFACE      =  'interface'     ;
  kwIMPLEMENTATION =  'implementation';
  kwEXIT           =  'exit'          ;
  kwARRAY          =  'array'         ;
  kwOF             =  'of'            ;
  kwCASE           =  'case'          ;
  kwPROGRAM        =  'program'       ;
  kwIN             =  'in'            ;


implementation

uses JvInterpreter, JvInterpreterConst;  

const
  K = '''';

{*********************** tokenizer ***********************}
 { modified algorithm from mozilla source }

type
  TTokentag = record
    Token : string;
    TTyp : TTokenTyp;
  end;

const
  P_UNKNOWN = -1;
  MIN_WORD_LENGTH = 2;
  MAX_WORD_LENGTH = 14; { = length('implementation') }

  asso_indexes : array [0..175] of integer = (
        { 0   1   2   3   4   5   6   7   8   9 }
  	{00}   35,  28,  4,  32,  19,  6,  7,  25,  28,  7, 
  	{10}   3,  32,  20,  25,  5,  36,  10,  4,  44,  9, 
  	{20}   39,  37,  37,  40,  2,  34,  19,  19,  40,  0, 
  	{30}   20,  2,  26,  14,  40,  28,  44,  14,  28,  1, 
  	{40}   21,  32,  20,  0,  9,  40,  44,  32,  31,  3, 
  	{50}   27,  20,  21,  39,  41,  13,  11,  36,  26,  31,
  	{60}   24,  14,  33,  2,  43,  44,  39,  4,  34,  18, 
  	{70}   9,  22,  40,  30,  2,  41,  39,  22,  12,  20, 
  	{80}   26,  18,  4,  15,  9,  19,  3,  12,  10,  28, 
  	{90}   29,  32,  8,  33,  22,  42,  27,  14,  3,  36, 
  	{100}   11,  0,  32,  26,  30,  26,  15,  7,  32,  14,
  	{110}   16,  24,  12,  16,  29,  16,  28,  28,  31,  4,
  	{120}   14,  4,  0,  34,  2,  19,  20,  32,  4,  31,
  	{130}   18,  28,  25,  10,  27,  33,  6,  9,  4,  1,
  	{140}   13,  10,  35,  38,  4,  43,  15,  11,  43,  3,
  	{150}   33,  43,  11,  19,  15,  33,  19,  27,  17,  30,
  	{160}   44,  12,  26,  24,  25,  31,  38,  15,  0,  27,
  	{170}   19,  22,  14,  10,  4,  30);

  	
	asso_values : array [0..176] of integer = (
        	{ 0   1   2   3   4   5   6   7   8   9 }
  	{00}   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  	{10}   -1, -1, -1, -1, -1, 2,  -1, -1, -1, 43, 
  	{20}   23,  19,  18,  -1, -1, -1, -1, -1, -1, -1,
  	{30}   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  	{40}   -1, -1, 9,  -1, 13,  -1, -1, -1, 31,  -1,
  	{50}   42,  4,  -1, -1, -1, 36,  -1, 26,  -1, 20, 
  	{60}   -1, 21,  -1, -1, -1, -1, -1, -1, 24,  -1,
  	{70}   38,  -1, 45,  16,  14,  0,  -1, -1, 25,  -1,
  	{80}   46,  -1, 10,  22,  7,  -1, 34,  -1, -1, -1,
  	{90}   39,  27,  6,  -1, 33,  -1, -1, 1,  -1, -1,
  	{100}   41,  -1, -1, 17,  -1, 29,  44,  -1, 28,  -1,
  	{110}   15,  8,  -1, 32,  12,  -1, -1, -1, 11,  -1,
  	{120}   37,  -1, -1, 40,  -1, -1, -1, 3,  -1, -1,
  	{130}   -1, -1, -1, -1, -1, -1, -1, 5,  -1, -1,
  	{140}   -1, -1, -1, -1, 35,  -1, -1, -1, -1, -1,
  	{150}   30,  -1, -1, -1, -1, -1, -1, -1, -1, -1,
  	{160}   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  	{170}   -1, -1, -1, -1, -1, -1, -1);


	word_list : array [0..46] of TTokentag = (
     	(Token: kwTRUE          ; TTyp: ttTrue            ),
     	(Token: kwFALSE         ; TTyp: ttFalse           ),
     	(Token: kwOR            ; TTyp: ttOr              ),
     	(Token: kwAND           ; TTyp: ttAnd             ),
     	(Token: kwNOT           ; TTyp: ttNot             ),
     	(Token: kwDIV           ; TTyp: ttDiv             ),
     	(Token: kwMOD           ; TTyp: ttMod             ),
     	(Token: kwBEGIN         ; TTyp: ttBegin           ),
     	(Token: kwEND           ; TTyp: ttEnd             ),
     	(Token: kwIF            ; TTyp: ttIf              ),
     	(Token: kwTHEN          ; TTyp: ttThen            ),
     	(Token: kwELSE          ; TTyp: ttElse            ),
     	(Token: kwWHILE         ; TTyp: ttWhile           ),
     	(Token: kwDO            ; TTyp: ttDo              ),
     	(Token: kwREPEAT        ; TTyp: ttRepeat          ),
     	(Token: kwUNTIL         ; TTyp: ttUntil           ),
     	(Token: kwPROCEDURE     ; TTyp: ttProcedure       ),
     	(Token: kwFUNCTION      ; TTyp: ttFunction        ),
     	(Token: kwFOR           ; TTyp: ttFor             ),
     	(Token: kwTO            ; TTyp: ttTo              ),
     	(Token: kwBREAK         ; TTyp: ttBreak           ),
     	(Token: kwCONTINUE      ; TTyp: ttContinue        ),
     	(Token: kwVAR           ; TTyp: ttVar             ),
     	(Token: kwTRY           ; TTyp: ttTry             ),
     	(Token: kwFINALLY       ; TTyp: ttFinally         ),
     	(Token: kwEXCEPT        ; TTyp: ttExcept          ),
     	(Token: kwON            ; TTyp: ttOn              ),
     	(Token: kwRAISE         ; TTyp: ttRaise           ),
     	(Token: kwEXTERNAL      ; TTyp: ttExternal        ),
     	(Token: kwUNIT          ; TTyp: ttUnit            ),
     	(Token: kwUSES          ; TTyp: ttUses            ),
     	(Token: kwCONST         ; TTyp: ttConst           ),
     	(Token: kwPUBLIC        ; TTyp: ttPublic          ),
     	(Token: kwPRIVATE       ; TTyp: ttPrivate         ),
     	(Token: kwPROTECTED     ; TTyp: ttProtected       ),
     	(Token: kwPUBLISHED     ; TTyp: ttPublished       ),
     	(Token: kwPROPERTY      ; TTyp: ttProperty        ),
     	(Token: kwCLASS         ; TTyp: ttClass           ),
     	(Token: kwTYPE          ; TTyp: ttType            ),
     	(Token: kwINTERFACE     ; TTyp: ttInterface       ),
     	(Token: kwIMPLEMENTATION; TTyp: ttImplementation  ),
     	(Token: kwEXIT          ; TTyp: ttExit            ),
     	(Token: kwARRAY         ; TTyp: ttArray           ),
     	(Token: kwOF            ; TTyp: ttOf              ),
     	(Token: kwCASE          ; TTyp: ttCase            ),
     	(Token: kwPROGRAM       ; TTyp: ttProgram         ),
     	(Token: kwIN            ; TTyp: ttIn              )
 );


{ convert string into token number using hash tables }

function pa_tokenize_tag(const TokenStr : string) : TTokenTyp;
var
  len: Integer;
  hval: Integer;
begin
  Result := P_UNKNOWN;
  hval := -1;
  len := Length(TokenStr);
  if (MIN_WORD_LENGTH <= len) and (len <= MAX_WORD_LENGTH) then
  begin
    hval := len;
    case hval of
      1 : hval := hval + asso_indexes[(byte(TokenStr[1]) - byte('a')) and $1f];
      2 :
        begin
          hval := hval + asso_indexes[(byte(TokenStr[1]) - byte('a')) and $1f];
          hval := hval + asso_indexes[(byte(TokenStr[2]) - byte('a')) and $1f];
        end;
      else
        begin
          hval := hval + asso_indexes[(byte(TokenStr[1]) - byte('a')) and $1f];
          hval := hval + asso_indexes[(byte(TokenStr[2]) - byte('a')) and $1f];
          hval := hval + asso_indexes[(byte(TokenStr[3]) - byte('a')) and $1f];
        end;
    end;
    hval := hval + asso_indexes[(byte(TokenStr[len]) - byte('a')) and $1f];
    hval := asso_values[hval];
  end;
  if hval <> -1 then
  begin
    if Cmp(word_list[hval].Token, TokenStr) then
      Result := word_list[hval].TTyp;
  end;
end;

const
  { !"#$%&'()*+,-./0123456789:;<=>? }
  asso_1values : array [' '..'?'] of integer = (
    -1, -1, -1, -1, -1, -1, -1, -1,
    ttLB, ttRB, ttMul, ttPlus, ttCol, ttMinus, ttPoint, ttDiv,
    ttInteger, ttInteger, ttInteger, ttInteger, ttInteger,
    ttInteger, ttInteger, ttInteger, ttInteger, ttInteger,
    ttColon, ttSemicolon, ttLess, ttEqu, ttGreater, -1
  );
{######################## tokenizer ########################}

function HasChar(const Ch : Char; const S : string) : boolean;
begin
  Result := Pos(Ch, S) > 0;
end;

function TokenTyp(const Token : string): TTokenTyp;
var
	i : Integer;
  L1 : integer;
  T1: char;
	Ci: char;
	Point : boolean;
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
      Result := asso_1values[T1]
    else if T1 = '[' then
      Result := ttLS
    else if T1 = ']' then
      Result := ttRS
    else goto Any;
  end
  else
		case T1 of    { }
      '$' :
         { may be hex constant }
        begin
          for i := 2 to L1 do
           {$IFDEF Delphi}
            if not (Token[i] in StConstSymbols) then
           {$ELSE}
            if not HasChar(Token[i], StConstSymbols) then
           {$ENDIF}
              goto Any; { !!GOTO!! }
          Result := ttInteger;
        end;
			'<' :
        if L1 = 2 then
          case Token[2] of    { }
            '=' : Result := ttEquLess;
            '>' : Result := ttNotEqu;
            else goto Any; { !!GOTO!! }
          end    { case }
        else goto Any; { !!GOTO!! }
			'>' :
        if (L1 = 2) and (Token[2] = '=') then
          Result := ttEquGreater
        else goto Any; { !!GOTO!! }
			else
        begin
          Any : { !!LABEL!! }

      		Point := false;
      		for i := 1 to L1 do
      		begin
      			Ci := Token[i];
      			if Ci = '.' then
      				if Point then goto NotNumber {two Points in lexem} { !!GOTO!! }
              else Point := true
      			else if (Ci < '0') or (Ci > '9') then
              goto NotNumber { not number } { !!GOTO!! };
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
            Result := pa_tokenize_tag(Token);

            if Result <> -1 then
  					else

  				 { may be identifer }
           {$IFDEF Delphi}
  					if not (T1 in StIdFirstSymbols) then
           {$ELSE}
  					if not HasChar(T1, StIdFirstSymbols) then
           {$ENDIF}
  						Result := ttUnknown
  					else
  					begin
  						for i := 2 to L1 do
               {$IFDEF Delphi}
                if not (Token[i] in StIdSymbols) then
               {$ELSE}
                if not HasChar(Token[i], StIdSymbols) then
               {$ENDIF}
  							begin
  								Result := ttUnknown;
  								Exit;
  							end;
  						Result := ttIdentifer;
  					end;
  				end;
      end;     { else }
    end;    { case Token[1] }
end;    { GetTokenTyp }

function TypToken(const TTyp : TTokenTyp): string;
begin
  Result := '?? not implemented !!'; { DEBUG !! }
end;    { TypToken }

function Prior(const TTyp : TTokenTyp) : TPriorLevel;
const
  Priors: array[ttNot..ttEquLess] of TPriorLevel = (
    priorNot, priorMul, priorDiv, priorIntDiv, priorMod, priorAnd, priorPlus,
    priorMinus, priorOr, priorEqu, priorGreater, priorLess,
    priorNotEqu, priorEquGreater, priorEquLess);
begin
	if TTyp in [ttNot..ttEquLess] then
		Result := Priors[TTyp]
	else
		Result := 0;
end;    { Prior }


{*************************** TJvInterpreterParser ****************************}
constructor TJvInterpreterParser.Create;
begin
  inherited Create;
end;

destructor TJvInterpreterParser.Destroy;
begin
  inherited Destroy;
end;

procedure TJvInterpreterParser.SetSource(Value: string);
begin
  FSource := Value;
  Init;
end;

procedure TJvInterpreterParser.Init;
begin
  FPCPos := PChar(FSource);
end;

function TJvInterpreterParser.Token : string;
var
  P, F : PChar;

  procedure Skip;
  begin
    case P[0] of
      '{' :
        begin
          F := StrScan(P+1, '}');
          if F = nil then JvInterpreterError(ieBadRemark, P - PChar(FSource));
          P := F+1;
        end;
      '(' :
        if (P[1] = '*') then
        begin
          F := P+2;
          while true do
          begin
            F := StrScan(F, '*');
            if F = nil then JvInterpreterError(ieBadRemark,  P - PChar(FSource));
            if F[1] = ')' then begin inc(F); break; end;
            inc(F);
          end;
          P := F+1;
        end;
      '}' : JvInterpreterError(ieBadRemark,  P - PChar(FSource));
      '*' : if (P[1] = ')') then JvInterpreterError(ieBadRemark,  P - PChar(FSource));
      '/' :
        if (P[1] = '/') then
          while not (P[0] in [#10, #13, #00]) do inc(P);
    end;
    while (P[0] in [' ', #10, #13, #9]) do inc(P);
  end;

var
  F1 : PChar;
  i : integer;
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
 {$IFDEF Delphi}
  if P[0] in StIdFirstSymbols then
 {$ELSE}
  if HasChar(P[0], StIdFirstSymbols) then
 {$ENDIF}
   { token }
  begin
   {$IFDEF Delphi}
    while P[0] in StIdSymbols do inc(P);
   {$ELSE}
    while HasChar(P[0], StIdSymbols) do inc(P);
   {$ENDIF}
    SetString(Result, F, P-F);
  end else
 {$IFDEF Delphi}
  if P[0] in StConstSymbols10 then
 {$ELSE}
  if HasChar(P[0], StConstSymbols10) then
 {$ENDIF}
   { number }
  begin
   {$IFDEF Delphi}
    while (P[0] in StConstSymbols10) or (P[0] = '.') do inc(P);
   {$ELSE}
    while HasChar(P[0], StConstSymbols10) or (P[0] = '.') do inc(P);
   {$ENDIF}
    SetString(Result, F, P-F);
  end else
  if ((P[0] = '$') and
   {$IFDEF Delphi}
    (P[1] in StConstSymbols)) then
   {$ELSE}
    HasChar(P[1], StConstSymbols)) then
   {$ENDIF}
   { hex number }
  begin
    inc(P);
   {$IFDEF Delphi}
    while P[0] in StConstSymbols do inc(P);
   {$ELSE}
    while HasChar(P[0], StConstSymbols) do inc(P);
   {$ENDIF}
    SetString(Result, F, P-F);
  end else
  if P[0] = '''' then
   { string constant }
  begin
    inc(P);
    while P[0] <> #0 do
    begin
      if (P[0] = '''') then
        if (P[1] = '''') then inc(P) else break;
      inc(P);
    end;
    inc(P);
    SetString(Result, F, P-F);
    i := 2;
    while i < Length(Result) -1 do
    begin
      if Result[i] = '''' then
        Delete(Result, i, 1);
      inc(i);
    end;
  end else
  if ((P[0] = '#') and
   {$IFDEF Delphi}
    (P[1] in StConstSymbols10)) then
   {$ELSE}
    HasChar(P[1], StConstSymbols10)) then
   {$ENDIF}
   { char constant }
  begin
    inc(P);
   {$IFDEF Delphi}
    while P[0] in StConstSymbols10 do inc(P);
   {$ELSE}
    while HasChar(P[0], StConstSymbols10) do inc(P);
   {$ENDIF}
    SetString(Result, F+1, P-F-1);
    Result := '''' + Chr(StrToInt(Result)) + '''';
  end else
  if P[0] in ['>', '=', '<'] then
  begin
    if (P[0] = '>') and (P[1] = '=') then
    begin
      Result := '>=';
      inc(P, 2);
    end else
    if (P[0] = '<') and (P[1] = '=') then
    begin
      Result := '<=';
      inc(P, 2);
    end else
    if (P[0] = '<') and (P[1] = '>') then
    begin
      Result := '<>';
      inc(P, 2);
    end else
    begin
      Result := P[0];
      inc(P);
    end;
  end else
  if P[0] = #0 then
    Result := ''
  else
  begin
    Result := P[0];
    inc(P);
  end;

  FPCPos := P;
end;

function TJvInterpreterParser.GetPos: integer;
begin
  Result := FPCPos - PChar(FSource);
end;

procedure TJvInterpreterParser.SetPos(Value: integer);
begin
  FPCPos := PChar(FSource) + Value;
end;
{########################### TJvIParser ###########################}


end.

