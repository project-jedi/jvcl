{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

This file is derived from ExprParser.pas of the MP3BookHelper project
http://mp3bookhelper.sourceforge.net and re-licensed under MPL by permission from
the original author Vlad Skarzhevskyy.

The Original Code is: ExprParser.pas, released on 2008-10-24

The Initial Developers of the Original Code are: Vlad Skarzhevskyy, Christian Schiffler
Copyright (c) 2002 Vlad Skarzhevskyy
Copyright (c) 2008 Christian Schiffler
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Equality Check is case insensitive due to usage of TMask class in unit Masks.

This unit is used as a helper for JvMemoryDataSet.pas.

}

unit JvExprParser;

{DEFINE TESTING_PARSER}

interface

uses
  Contnrs;

type
  TOnGetVariableValue = function(Sender: TObject; Varname: WideString; var Value: Variant): boolean of object;
  TOnExecuteFunction = function(Sender: TObject; FuncName: WideString; Args: Variant; var ResVal: Variant): boolean of
    object;

  TExprParser = class
  private
    FValue: Variant;
    FParser: TObject;
    FScan: TObject;
    FExpression: Widestring;
    FOnGetVariable: TOnGetVariableValue;
    FOnExecuteFunction: TOnExecuteFunction;
    FEnableWildcardMatching: boolean;
    procedure SetExpression(const Value: Widestring);
    function DoGetVariable(Varname: WideString; var Value: Variant): boolean;
    function DoExecuteFunction(FuncName: WideString; Args: Variant; var ResVal: Variant): boolean;
  public
    ErrorMessage: WideString;
    constructor Create();
    destructor Destroy; override;
    function eval: boolean; overload;
    function eval(Expression: Widestring): boolean; overload;
  published
    property Expression: Widestring read FExpression write SetExpression;
    property OnGetVariable: TOnGetVariableValue read FOnGetVariable write FOnGetVariable;
    property OnExecuteFunction: TOnExecuteFunction read FOnExecuteFunction write FOnExecuteFunction;
    property Value: Variant read FValue;
    property EnableWildcardMatching: boolean read FEnableWildcardMatching write FEnableWildcardMatching;
  end;

  {$IFDEF TESTING_PARSER}
var
  debugText                             : WideString;
  {$ENDIF}

implementation

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  Masks,
  SysUtils;

const
  NUMBERS                               = [
    WideChar('0')..WideChar('9')];

  LETTERS                               = [
    WideChar('a')..WideChar('z'),
    WideChar('A')..WideChar('Z'),
    WideChar('_')];

  LETTERS_NUMBERS                       = [
    WideChar('0')..WideChar('9'),
    WideChar('a')..WideChar('z'),
    WideChar('A')..WideChar('Z'),
    WideChar('_')];

  OPERATORS                             = [
    WideChar('+'), WideChar('-'),
    WideChar('/'), WideChar('*'),
    WideChar('='),
    WideChar('<'),
    WideChar('>'),
    WideChar('&'),
    WideChar('|'),
    WideChar('!')];

type

  EToken = (tkNA, tkEOF, tkError,
    tkLParen, tkRParen, tkComa,
    tkOperator, tkIdentifier,
    tkNumber, tkInteger, tkString);

  ELex = class
  public
    token: EToken;
    chr: Char;
    str: WideString;
    pos: integer;
    constructor Create(token: EToken; pos: integer); overload;
    constructor Create(token: EToken; str: WideString; pos: integer); overload;
    constructor Create(token: EToken; chr: WideChar; pos: integer); overload;
    function debug(): WideString;
  end;

  EScan = class(TObjectList)
  private
    function GetItem(Index: Integer): ELex;
  public
    ErrorMessage: WideString;
    constructor Create();
    destructor Destroy; override;
    property Items[Index: Integer]: ELex read GetItem; default;
    function pars(str: WideString): boolean;
    {$IFDEF TESTING_PARSER}
    procedure debugPrint();
    {$ENDIF}
  end;

  EVariant = variant;

  EParser = class;

  ENode = class
    FParser: EParser;
    constructor Create(Parser: EParser); virtual;
    function eval(): EVariant; virtual; abstract;
  end;

  ParserException = class(Exception)
  public
    constructor Create(const Msg: string; lex: ELex); overload;
    constructor Create(const E: Exception); overload;
  end;

  ENodeCValue = class(ENode)
  public
    cvalue: ELex;
    constructor Create(Parser: EParser; cvalue: ELex); reintroduce;
    function eval(): EVariant; override;
  end;

  ENodeVariable = class(ENode)
  public
    lex: ELex;
    constructor Create(Parser: EParser; lex: ELex); reintroduce;
    function eval(): EVariant; override;
  end;

  ENodeUnary = class(ENode)
  public
    operator: ELex;
    rightNode: ENode;
    constructor Create(Parser: EParser; operator: ELex; rightNode: ENode); reintroduce;
    destructor Destroy; override;
    function eval(): EVariant; override;
  end;

  ENodeBin = class(ENode)
  public
    operator: ELex;
    lefNode, rightNode: ENode;
    constructor Create(Parser: EParser; operator: ELex; lefNode, rightNode: ENode); reintroduce;
    destructor Destroy; override;
    function eval(): EVariant; override;
  end;

  ENodeFunction = class(ENode)
  public
    func: ELex;
    args: TObjectList;
    constructor Create(Parser: EParser; func: ELex); reintroduce;
    destructor Destroy; override;
    procedure arg(Node: ENode);
    function eval(): EVariant; override;
  end;

  EParser = class
  public
    Parent: TExprParser;
    scan: EScan;
    scan_idx: integer;
    root: ENode;
    ErrorMessage: WideString;
    value: EVariant;

    constructor Create();
    destructor Destroy; override;

    function pars(): boolean;
    function execute(): boolean;

    function Expr(): ENode;
    function Term(): ENode;
    function Factor(): ENode;

    function LexC(): ELex;
    function LexLook(look_ahead: integer = 1): ELex;
    procedure LexAcept();
  end;

var
  ELexEOF                               : ELex;

  {$IFDEF TESTING_PARSER}

procedure DebugMessage(msg: WideString);
begin
  debugText := debugText + msg + sLineBreak;
end;
{$ENDIF}

{ ELex }

constructor ELex.Create(token: EToken; pos: integer);
begin
  self.token := token;
  self.pos := pos;
end;

constructor ELex.Create(token: EToken; str: WideString; pos: integer);
begin
  self.token := token;
  self.str := str;
  self.pos := pos;
end;

constructor ELex.Create(token: EToken; chr: WideChar; pos: integer);
begin
  self.token := token;
  self.chr := Char(chr);
  self.pos := pos;
end;

function ELex.debug: WideString;
const
  tokenStr                              : array[EToken] of string =
    ('N/A', 'End of expression', 'Error',
    '(', ')', ',',
    'Operator', 'Identifier',
    'Number', 'Integer', 'String');
begin
  Result := tokenStr[token];
  case token of
    tkOperator: Result := Result + ': ' + chr;
    tkIdentifier,
      tkNumber,
      tkInteger,
      tkString:
      Result := Result + ': ' + str;
  end;
  Result := Result + ' at pos: ' + IntToStr(pos);
end;

{ EScan }

constructor EScan.Create;
begin
  inherited;
  OwnsObjects := true;
  ErrorMessage := '';
end;

destructor EScan.Destroy;
begin
  inherited;
end;

function EScan.GetItem(Index: Integer): ELex;
begin
  result := inherited Items[Index] as ELex;
end;

function EScan.pars(str: WideString): boolean;
var
  idx, start_idx, len                   : integer;
  c                                     : WideChar;
  s                                     : WideString;
  ctoken                                : EToken;
begin
  len := Length(str);
  idx := 1;
  s := '';
  ctoken := tkNA;

  while (idx <= len) do
  begin
    c := str[idx];
    start_idx := idx;
    Inc(idx);
    ctoken := tkNA;

    case c of
      '(': ctoken := tkLParen;
      ')': ctoken := tkRParen;
      ',': ctoken := tkComa;
      ' ', #09: ;
      else
        if (c in OPERATORS) then
        begin
          ctoken := tkOperator;
        end
        else
          if (c = '"') or (c = '''') then
          begin
            ctoken := tkString;
            while (idx <= len) and (str[idx] <> c) do
            begin
              s := s + str[idx];
              Inc(idx);
            end;
            if (idx <= len) and (str[idx] = c) then
              Inc(idx)
            else
            begin
              ctoken := tkError;
              ErrorMessage := 'No end of string found';
            end

          end
          else
            if (c in NUMBERS) then
            begin
              ctoken := tkInteger;
              s := s + c;
              while (idx <= len) and (str[idx] in NUMBERS) do
              begin
                s := s + str[idx];
                Inc(idx);
              end;
              if ((idx <= len) and (str[idx] = '.')) then
              begin
                ctoken := tkNumber;
                Inc(idx);
                s := s + '.';
                while (idx <= len) and (str[idx] in NUMBERS) do
                begin
                  s := s + str[idx];
                  Inc(idx);
                end;
              end;
            end
            else
              if (c = '.') then         // .55
              begin
                ctoken := tkNumber;
                s := s + c;
                while (idx <= len) and (str[idx] in NUMBERS) do
                begin
                  s := s + str[idx];
                  Inc(idx);
                end;
              end
              else
                if (c in LETTERS) then
                begin
                  ctoken := tkIdentifier;
                  s := s + c;
                  while (idx <= len) and (str[idx] in LETTERS_NUMBERS) do
                  begin
                    s := s + str[idx];
                    Inc(idx);
                  end;
                end
                else
                begin
                  ctoken := tkError;
                  ErrorMessage := 'Bad character ''' + c + '''';
                end;
    end;

    case ctoken of
      tkError: break;
      tkNA: ;                           // continue
      tkOperator: Add(ELex.Create(tkOperator, c, start_idx));
      tkIdentifier,
        tkNumber,
        tkInteger,
        tkString:
        begin
          if Lowercase(s) = 'and' then
            Add(ELex.Create(tkOperator, WideChar('&'), start_idx))
          else
            if Lowercase(s) = 'or' then
              Add(ELex.Create(tkOperator, WideChar('|'), start_idx))
            else
              Add(ELex.Create(ctoken, s, start_idx));
          s := '';
        end
      else
        Add(ELex.Create(ctoken, start_idx));
    end;
  end;
  Result := (ctoken <> tkError);
  ELexEOF := ELex.Create(tkEOF, idx);
  Add(ELexEOF);
end;

{$IFDEF TESTING_PARSER}

procedure EScan.debugPrint;
var
  i                                     : integer;
begin
  for i := 0 to Count - 1 do
    DebugMessage(Items[i].debug);
end;
{$ENDIF}

{ EParser }

constructor EParser.Create;
begin
  scan_idx := 0;
  ErrorMessage := '';
end;

destructor EParser.Destroy;
begin
  root.Free;
  inherited;
end;

function EParser.pars: boolean;
begin
  if (root <> nil) then
    FreeAndNil(root);
  try
    root := Expr();
    if (scan_idx < scan.Count - 1) then
    begin
      FreeAndNil(root);
      raise ParserException.Create('Unexpected ', LexC());
    end;
  except
    on E: Exception do
    begin
      ErrorMessage := E.Message;
    end
  end;
  Result := (root <> nil);
end;

function EParser.execute: boolean;
begin
  Result := false;
  if (root <> nil) then
  begin
    try
      value := root.eval();
      Result := true;
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
      end
    end;
  end;
end;

procedure EParser.LexAcept;
begin
  Inc(scan_idx);
end;

function EParser.LexC: ELex;
begin
  Result := LexLook(0);
end;

function EParser.LexLook(look_ahead: integer): ELex;
begin
  if ((scan_idx + look_ahead) < scan.Count) then
    Result := scan[scan_idx + look_ahead]
  else
    Result := ELexEOF;
end;

function EParser.Expr: ENode;
var
  cNode, rightNode                      : ENode;
  lex                                   : ELex;
begin
  cNode := nil;
  try
    cNode := Term();
    lex := LexC();

    if (lex.token = tkOperator) then
    begin
      if (lex.chr in ['+', '-']) then
      begin
        LexAcept();
        rightNode := Expr();
        if (rightNode = nil) then
          raise ParserException.Create('Expression expected after', lex);
        cNode := ENodeBin.Create(Self, lex, cNode, rightNode);
      end;
    end;
  except
    on PE: ParserException do
    begin
      FreeAndNil(cNode);
      raise;
    end;
    on E: Exception do
    begin
      FreeAndNil(cNode);
      raise ParserException.Create(E.Message);
    end;
  end;
  Result := cNode;
end;

function EParser.Term: ENode;
var
  cNode, rightNode                      : ENode;
  lex                                   : ELex;
begin
  cNode := nil;
  try
    cNode := Factor();
    lex := LexC();

    if (lex.token = tkOperator) then
    begin
      if (lex.chr in ['*', '/', '=', '&', '|', '<', '>']) then
      begin
        LexAcept();
        rightNode := Expr();
        if (rightNode = nil) then
          raise ParserException.Create('Expression expected after', lex);
        cNode := ENodeBin.Create(Self, lex, cNode, rightNode);
      end;
    end;
  except
    on PE: ParserException do
    begin
      FreeAndNil(cNode);
      raise;
    end;
    on E: Exception do
    begin
      FreeAndNil(cNode);
      raise ParserException.Create(E.Message);
    end;
  end;
  Result := cNode;
end;

function EParser.Factor: ENode;
var
  cNode                                 : ENode;
  fNode                                 : ENodeFunction;
  lex                                   : ELex;
begin
  cNode := nil;
  try
    lex := LexC();
    case lex.token of
      tkLParen:
        begin
          LexAcept();
          cNode := Expr();
          if ((LexC().token = tkRParen)) then
          begin
            LexAcept();
          end
          else
            raise ParserException.Create('Expected closing parenthesis instead of', LexC());
        end;
      tkOperator:                       // unary minus
        begin
          if (lex.chr in ['+', '-', '!']) then
          begin
            LexAcept();
            cNode := ENodeUnary.Create(Self, lex, Factor());
          end
          else
            raise ParserException.Create('Unexpected ', lex);
        end;
      tkNumber, tkInteger, tkString:
        begin
          cNode := ENodeCValue.Create(Self, lex);
          LexAcept();
        end;
      tkIdentifier:
        begin
          if (LexLook().token = tkLParen) then
          begin
            // function call
            LexAcept();
            fNode := ENodeFunction.Create(Self, lex);
            LexAcept();
            cNode := fNode;
            if ((LexC().token <> tkRParen)) then
            begin
              fNode.arg(Expr());
              while (LexC().token = tkComa) do
              begin
                LexAcept();
                fNode.arg(Expr());
              end;
            end;

            if ((LexC().token = tkRParen)) then
            begin
              LexAcept();
            end
            else
              raise ParserException.Create('Expected closing parenthesis instead of', LexC());
          end
          else
          begin
            cNode := ENodeVariable.Create(Self, lex);
            LexAcept();
          end;
        end;
      else
        raise ParserException.Create('Unexpected ', lex);
    end;
  except
    on PE: ParserException do
    begin
      FreeAndNil(cNode);
      raise;
    end;
    on E: Exception do
    begin
      FreeAndNil(cNode);
      raise ParserException.Create(E.Message);
    end;
  end;
  Result := cNode;
end;

{ ENode }

constructor ENode.Create(Parser: EParser);
begin
  FParser := Parser;
end;

{ ENodeBin }

constructor ENodeBin.Create(Parser: EParser; operator: ELex; lefNode, rightNode: ENode);
begin
  inherited Create(Parser);
  self.operator := operator;
  self.lefNode := lefNode;
  self.rightNode := rightNode;
end;

destructor ENodeBin.Destroy;
begin
  lefNode.Free;
  rightNode.Free;
  inherited;
end;

function ENodeBin.eval: EVariant;
var
  tmp, tmp2                             : variant;
  tmp_s, tmp_s2                         : WideString;

  function EvalEquality: boolean;
  var
    wildcard1, wildcard2                : boolean;
  begin
    // Determine values to have them handy.
    tmp := lefNode.eval;
    tmp2 := rightNode.eval;
    // Special case, at least one of both is null:
    if (tmp = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL) or (tmp2 = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL) then
    begin
      Result := (tmp = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL) and (tmp2 = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL);
    end
    else
    begin
      // Possiblilities:
      // Left hand contains wildcards -> Match right hand against left hand.
      // Right hand contains wildcards -> Match left hand against right hand.
      // Both hands contain wildcards -> Match for string equality as if no wildcards are supported.
      if FParser.Parent.FEnableWildcardMatching then
      begin
        tmp_s := tmp;
        tmp_s2 := tmp2;
        wildcard1 := (Pos('*', tmp_s) > 0) or (Pos('?', tmp_s) > 0);
        wildcard2 := (Pos('*', tmp_s2) > 0) or (Pos('?', tmp_s2) > 0);
        if wildcard1 and (not wildcard2) then
          Result := MatchesMask(tmp_s2, tmp_s)
        else
          if wildcard2 then
            Result := MatchesMask(tmp_s, tmp_s2)
          else
            Result := (tmp = tmp2);
      end
      else
        Result := (tmp = tmp2);
    end;
  end;
  function EvalLT: boolean;
  begin
    // Determine values to have them handy.
    tmp := lefNode.eval;
    tmp2 := rightNode.eval;
    // Special case, at least one of both is null:
    if (tmp = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL) or (tmp2 = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL) then
    begin
      // null is considered to be smaller than any value.
      Result := (tmp = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL);
    end
    else
    begin
      Result := (tmp < tmp2);
    end;
  end;
  function EvalGT: boolean;
  begin
    // Determine values to have them handy.
    tmp := lefNode.eval;
    tmp2 := rightNode.eval;
    // Special case, at least one of both is null:
    if (tmp = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL) or (tmp2 = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL) then
    begin
      // null is considered to be smaller than any value.
      Result := (tmp2 = {$IFDEF HAS_UNIT_VARIANTS}Variants.{$ENDIF HAS_UNIT_VARIANTS}NULL);
    end
    else
    begin
      Result := (tmp > tmp2);
    end;
  end;
begin
  case operator.chr of
    '+':
      begin
        tmp := lefNode.eval;
        // force string concatenation
        if (TVarData(tmp).VType = varString) or
          (TVarData(tmp).VType = varOleStr) then
        begin
          tmp_s := tmp;
          tmp_s2 := rightNode.eval;
          tmp_s := tmp_s + tmp_s2;
          Result := tmp_s;
        end
        else
        begin
          Result := tmp + rightNode.eval;
        end;
      end;
    '-': Result := lefNode.eval - rightNode.eval;
    '*': Result := lefNode.eval * rightNode.eval;
    '/': Result := lefNode.eval / rightNode.eval;
    '=': Result := EvalEquality();
    '<': Result := EvalLT();
    '>': Result := EvalGT();
    '&': Result := lefNode.eval and rightNode.eval;
    '|': Result := lefNode.eval or rightNode.eval;
  end;
end;

{ ENodeUnary }

constructor ENodeUnary.Create(Parser: EParser; operator: ELex; rightNode: ENode);
begin
  inherited Create(Parser);
  self.operator := operator;
  self.rightNode := rightNode;
end;

destructor ENodeUnary.Destroy;
begin
  rightNode.Free;
  inherited;
end;

function ENodeUnary.eval: EVariant;
begin
  Result := rightNode.eval();
  if operator.chr = '-' then
    Result := -Result;
  if operator.chr = '!' then
    Result := not Result;
end;

{ ENodeCValue }

constructor ENodeCValue.Create(Parser: EParser; cvalue: ELex);
begin
  inherited Create(Parser);
  self.cvalue := cvalue;
end;

function ENodeCValue.eval: EVariant;
begin
  case cvalue.token of
    tkNumber:
      Result := StrToFloat(cvalue.str);
    tkInteger:
      Result := StrToInt(cvalue.str);
    tkString:
      Result := cvalue.str;
  end;
end;

{ ENodeFunction }

constructor ENodeFunction.Create(Parser: EParser; func: ELex);
begin
  inherited Create(Parser);
  args := TObjectList.Create();
  args.OwnsObjects := true;
  self.func := func;
end;

destructor ENodeFunction.Destroy;
begin
  args.Free;
  inherited;
end;

procedure ENodeFunction.arg(Node: ENode);
begin
  args.Add(Node);
end;

function ENodeFunction.eval: EVariant;
var
  Value                                 : EVariant;
  vargs                                 : Variant;
  i                                     : integer;
begin
  vargs := VarArrayCreate([0, args.Count - 1], varVariant);
  for i := 0 to args.Count - 1 do
  begin
    vargs[i] := ENode(args[i]).eval;
  end;
  Value := NULL;
  if FParser.Parent.DoExecuteFunction(func.str, vargs, Value) then
    Result := Value
  else
    raise ParserException.Create('Function ' + func.str + ' could not be executed.');
end;

{ ENodeVariable }

constructor ENodeVariable.Create(Parser: EParser; lex: ELex);
begin
  inherited Create(Parser);
  self.lex := lex;
end;

function ENodeVariable.eval: EVariant;
var
  Value                                 : EVariant;
begin
  Value := NULL;
  if FParser.Parent.DoGetVariable(lex.str, Value) then
    Result := Value
  else
    raise ParserException.Create('Variable ' + lex.str + ' could not be fetched.');
end;

{ ParserException }

constructor ParserException.Create(const Msg: string; lex: ELex);
begin
  inherited Create(Msg + ' ' + lex.debug);
end;

constructor ParserException.Create(const E: Exception);
begin
  inherited Create(E.Message);
end;

{ TExprParser }

constructor TExprParser.Create;
begin
  ErrorMessage := '';
end;

destructor TExprParser.Destroy;
begin
  FParser.Free;
  FScan.Free;
  inherited;
end;

function TExprParser.eval(): boolean;
var
  parser                                : EParser;
  {$IFDEF TESTING_PARSER}
  scan                                  : EScan;
  {$ENDIF}
begin
  ErrorMessage := '';
  {$IFDEF TESTING_PARSER}
  debugText := '';
  scan := EScan(FScan);
  scan.debugPrint();
  {$ENDIF}
  parser := EParser(FParser);
  if (parser.execute()) then
  begin
    FValue := parser.value;
    Result := true;
  end
  else
  begin
    ErrorMessage := parser.ErrorMessage;
    Result := false;
  end
end;

function TExprParser.eval(Expression: Widestring): boolean;
begin
  Self.Expression := Expression;
  Result := eval();
end;

procedure TExprParser.SetExpression(const Value: Widestring);
begin
  FExpression := Value;
  if Assigned(FParser) then
    FParser.Free;
  if Assigned(FScan) then
    FScan.Free;
  FParser := EParser.Create;
  EParser(FParser).Parent := Self;
  FScan := EScan.Create;
  if (not EScan(FScan).pars(FExpression)) then
  begin
    ErrorMessage := EScan(FScan).ErrorMessage;
  end
  else
  begin
    EParser(FParser).scan := EScan(FScan);
    EParser(FParser).pars();
  end;
end;

function TExprParser.DoGetVariable(Varname: WideString; var Value: Variant): boolean;
begin
  if Assigned(FOnGetVariable) then
    Result := FOnGetVariable(Self, Varname, Value)
  else
    Result := false;
end;

function TExprParser.DoExecuteFunction(FuncName: WideString; Args: Variant; var ResVal: Variant): boolean;
begin
  if Assigned(FOnExecuteFunction) then
    Result := FOnExecuteFunction(Self, FuncName, Args, ResVal)
  else
    Result := false;
end;

end.

