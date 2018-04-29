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
located at http://jvcl.delphi-jedi.org

Known Issues:
  Equality Check is case insensitive due to usage of TMask class in unit Masks.

This unit is used as a helper for JvMemoryDataSet.pas.

}

unit JvExprParser;

{$I jvcl.inc}

{DEFINE TESTING_PARSER}

{$I jvcl.inc}

interface

uses
  SysUtils, Contnrs;

type
  TOnGetVariableValue = function(Sender: TObject; const VarName: string;
    var Value: Variant): Boolean of object;
  TOnExecuteFunction = function(Sender: TObject; const FuncName: string;
    const Args: Variant; var ResVal: Variant): Boolean of object;

  TExprParser = class
  private
    FValue: Variant;
    FParser: TObject; // TParser
    FScan: TObject; // TScan
    FExpression: string;
    FOnGetVariable: TOnGetVariableValue;
    FOnExecuteFunction: TOnExecuteFunction;
    FEnableWildcardMatching: Boolean;
    FErrorMessage: string;
    FCaseInsensitive: Boolean;
    procedure SetExpression(const Value: string);
    function DoGetVariable(const VarName: string; var Value: Variant): Boolean;
    function DoExecuteFunction(const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;
    procedure SetCaseInsensitive(const Value: Boolean);
  public
    constructor Create();
    destructor Destroy; override;
    function Eval: Boolean; overload;
    function Eval(const AExpression: string): Boolean; overload;

    property ErrorMessage: string read FErrorMessage;

  {published} // ahuser: not a TPersistent derived class
    property Expression: string read FExpression write SetExpression;
    property OnGetVariable: TOnGetVariableValue read FOnGetVariable write FOnGetVariable;
    property OnExecuteFunction: TOnExecuteFunction read FOnExecuteFunction write FOnExecuteFunction;
    property Value: Variant read FValue;
    property EnableWildcardMatching: Boolean read FEnableWildcardMatching write FEnableWildcardMatching;
    property CaseInsensitive: Boolean read FCaseInsensitive write SetCaseInsensitive;
  end;

  EExprParserError = class(Exception);

{$IFDEF TESTING_PARSER}
var
  DebugText: string;
{$ENDIF TESTING_PARSER}

implementation

uses
  Classes, Variants, Masks;

{$IFDEF COMPILER12_UP}
  // Our charsets do not contain any char > 127 what makes it safe because the
  // compiler generates correct code.
  {$WARN WIDECHAR_REDUCED OFF}
{$ENDIF COMPILER12_UP}

const
  cNumbers = ['0'..'9'];
  cLetters = ['a'..'z', 'A'..'Z', '_'];
  cLettersAndNumbers = cLetters + cNumbers;
  cOperators = [
    '+', '-',
    '/', '*',
    '=',
    '<',
    '>',
    '&',
    '|',
    '!',
    '~'];

type
  TToken = (tkNA, tkEOF, tkError,
    tkLParen, tkRParen, tkComa,
    tkOperator, tkIdentifier,
    tkNumber, tkInteger, tkString);

  TLex = class
  private
    FToken: TToken;
    FChr: Char;
    FStr: string;
    FPos: Integer;
  public
    constructor Create(AToken: TToken; APos: Integer); overload;
    constructor Create(AToken: TToken; const AStr: string; APos: Integer); overload;
    constructor Create(AToken: TToken; AChr: Char; APos: Integer); overload;
    function Debug(): string;

    property Token: TToken read FToken;
    property Chr: Char read FChr;
    property Str: string read FStr;
    property Pos: Integer read FPos;
  end;

  TScan = class(TObjectList)
  private
    FErrorMessage: string;
    function GetItem(Index: Integer): TLex;
  public
    constructor Create();
    property Items[Index: Integer]: TLex read GetItem; default;
    function Parse(const Str: string): Boolean;
    {$IFDEF TESTING_PARSER}
    procedure DebugPrint();
    {$ENDIF TESTING_PARSER}
    property ErrorMessage: string read FErrorMessage;
  end;

  TParser = class;

  TNode = class
  private
    FParser: TParser;
  public
    constructor Create(Parser: TParser); virtual;

    // Delphi 5 compiler shows hints about a not exported or used symbol
    // TNode.Eval. This is a compiler bug that is caused by the "abstract" keyword.
    function Eval(): Variant; virtual; abstract;
  end;

  EParserError = class(EExprParserError)
  public
    constructor Create(const Msg: string; Lex: TLex); overload;
  end;

  TNodeCValue = class(TNode)
  private
    FCValue: TLex;
  public
    constructor Create(AParser: TParser; ACValue: TLex); reintroduce;
    function Eval(): Variant; override;
  end;

  TNodeVariable = class(TNode)
  private
    FLex: TLex;
  public
    constructor Create(AParser: TParser; ALex: TLex); reintroduce;
    function Eval(): Variant; override;
  end;

  TNodeUnary = class(TNode)
  private
    FOperator: TLex;
    FRightNode: TNode;
  public
    constructor Create(AParser: TParser; AOperator: TLex; ARightNode: TNode); reintroduce;
    destructor Destroy; override;
    function Eval(): Variant; override;
  end;

  TNodeBin = class(TNode)
  private
    FOperator: TLex;
    FLeftNode, FRightNode: TNode;
  public
    constructor Create(AParser: TParser; AOperator: TLex; ALeftNode, ARightNode: TNode); reintroduce;
    destructor Destroy; override;
    function Eval(): Variant; override;
  end;

  TNodeFunction = class(TNode)
  private
    FFunc: TLex;
    FArgs: TObjectList;
  public
    constructor Create(AParser: TParser; AFunc: TLex); reintroduce;
    destructor Destroy; override;
    procedure AddArg(Node: TNode);
    function Eval(): Variant; override;
  end;

  TParser = class
  private
    FParent: TExprParser;
    FScan: TScan;
    FScanIdx: Integer;
    FRoot: TNode;
    FErrorMessage: string;
    FValue: Variant;
  public
    destructor Destroy; override;

    function Parse(): Boolean;
    function Execute(): Boolean;

    function Expr(): TNode;
    function Term(): TNode;
    function Factor(): TNode;

    function LexC(): TLex;
    function LexLook(LookAhead: Integer = 1): TLex;
    procedure LexAccept();

    property Parent: TExprParser read FParent write FParent;
    property Value: Variant read FValue;
    property ErrorMessage: string read FErrorMessage;
    property Scan: TScan read FScan write FScan;
  end;

var
  ELexEOF: TLex; // ahuser: what the...

{$IFDEF TESTING_PARSER}
procedure DebugMessage(const msg: string);
begin
  DebugText := DebugText + msg + sLineBreak;
end;
{$ENDIF TESTING_PARSER}

{ TLex }

constructor TLex.Create(AToken: TToken; APos: Integer);
begin
  FToken := AToken;
  FPos := APos;
end;

constructor TLex.Create(AToken: TToken; const AStr: string; APos: Integer);
begin
  inherited Create;
  FToken := AToken;
  FStr := AStr;
  FPos := APos;
end;

constructor TLex.Create(AToken: TToken; AChr: Char; APos: Integer);
begin
  FToken := AToken;
  FChr := Char(AChr);
  FPos := APos;
end;

function TLex.debug: string;
const
  TokenStr: array[TToken] of string =
    ('N/A', 'End of expression', 'Error',
    '(', ')', ',',
    'Operator', 'Identifier',
    'Number', 'Integer', 'String');
begin
  Result := TokenStr[Token];
  case Token of
    tkOperator:
      Result := Result + ': ' + Chr;
    tkIdentifier, tkNumber, tkInteger, tkString:
      Result := Result + ': ' + Str;
  end;
  Result := Result + ' at pos: ' + IntToStr(Pos);
end;

{ TScan }

constructor TScan.Create;
begin
  inherited Create;
  OwnsObjects := True;
  FErrorMessage := '';
end;

function TScan.GetItem(Index: Integer): TLex;
begin
  Result := inherited Items[Index] as TLex;
end;

function TScan.Parse(const Str: string): Boolean;
var
  Idx, StartIdx, Len: Integer;
  C: Char;
  S: string;
  CToken: TToken;
begin
  Len := Length(Str);
  Idx := 1;
  S := '';
  CToken := tkNA;

  while Idx <= Len do
  begin
    C := Str[Idx];
    StartIdx := Idx;
    Inc(Idx);
    CToken := tkNA;

    case C of
      '(': CToken := tkLParen;
      ')': CToken := tkRParen;
      ',': CToken := tkComa;
      ' ', #09: ;
    else
      if C in cOperators then
        CToken := tkOperator
      else
        if (C = '"') or (C = '''') then
        begin
          CToken := tkString;
          while (Idx <= Len) and (Str[Idx] <> C) do
          begin
            S := S + Str[Idx]; // ahuser: performance suicide
            Inc(Idx);
          end;
          if (Idx <= Len) and (Str[Idx] = C) then
            Inc(Idx)
          else
          begin
            CToken := tkError;
            FErrorMessage := 'No end of string found';
          end
        end
        else
        if C in cNumbers then
        begin
          CToken := tkInteger;
          S := S + C;
          while (Idx <= Len) and (Str[Idx] in cNumbers) do
          begin
            S := S + Str[Idx]; // ahuser: performance suicide
            Inc(Idx);
          end;
          if ((Idx <= Len) and (Str[Idx] = '.')) then
          begin
            CToken := tkNumber;
            Inc(Idx);
            S := S + '.';
            while (Idx <= Len) and (Str[Idx] in cNumbers) do
            begin
              S := S + Str[Idx]; // ahuser: performance suicide
              Inc(Idx);
            end;
          end;
        end
        else
        if C = '.' then         // .55
        begin
          CToken := tkNumber;
          S := S + C;
          while (Idx <= Len) and (Str[Idx] in cNumbers) do
          begin
            S := S + Str[Idx]; // ahuser: performance suicide
            Inc(Idx);
          end;
        end
        else
        if C in cLetters then
        begin
          CToken := tkIdentifier;
          S := S + C;
          while (Idx <= Len) and (Str[Idx] in cLettersAndNumbers) do
          begin
            S := S + Str[Idx]; // ahuser: performance suicide
            Inc(Idx);
          end;
        end
        else
        begin
          CToken := tkError;
          FErrorMessage := Format('Bad character ''%s''', [string(C)]);
        end;
    end;

    case CToken of
      tkError: break;
      tkNA: ;                           // continue
      tkOperator: Add(TLex.Create(tkOperator, C, StartIdx));
      tkIdentifier,
        tkNumber,
        tkInteger,
        tkString:
        begin
          if CompareText(S, 'and') = 0 then
            Add(TLex.Create(tkOperator, '&', StartIdx))
          else
            if CompareText(S, 'or') = 0 then
              Add(TLex.Create(tkOperator, '|', StartIdx))
            else
            begin
              if CompareText(S, 'like')=0 then
                Add(TLex.Create(tkOperator, '~', StartIdx))
              else
                Add(TLex.Create(CToken, S, StartIdx));
            end;
          S := '';
        end
      else
        Add(TLex.Create(CToken, StartIdx));
    end;
  end;
  Result := CToken <> tkError;
  ELexEOF := TLex.Create(tkEOF, Idx);
  Add(ELexEOF);
end;

{$IFDEF TESTING_PARSER}
procedure TScan.DebugPrint;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    DebugMessage(Items[I].Debug);
end;
{$ENDIF TESTING_PARSER}

{ TParser }

destructor TParser.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

function TParser.Parse: Boolean;
begin
  FreeAndNil(FRoot);
  try
    FRoot := Expr();
    if FScanIdx < FScan.Count - 1 then
    begin
      FreeAndNil(FRoot);
      raise EParserError.Create('Unexpected ', LexC());
    end;
  except
    on E: Exception do
      FErrorMessage := E.Message;
  end;
  Result := FRoot <> nil;
end;

function TParser.Execute: Boolean;
begin
  Result := False;
  if FRoot <> nil then
  begin
    try
      FValue := FRoot.Eval();
      Result := True;
    except
      on E: Exception do
      begin
        FErrorMessage := E.Message;
//        raise;
      end;
    end;
  end;
end;

procedure TParser.LexAccept;
begin
  Inc(FScanIdx);
end;

function TParser.LexC: TLex;
begin
  Result := LexLook(0);
end;

function TParser.LexLook(LookAhead: Integer): TLex;
begin
  if (FScanIdx + LookAhead) < FScan.Count then
    Result := FScan[FScanIdx + LookAhead]
  else
    Result := ELexEOF;
end;

function TParser.Expr: TNode;
var
  CNode, RightNode: TNode;
  Lex: TLex;
begin
  CNode := nil;
  try
    CNode := Term();
    Lex := LexC();

    if Lex.Token = tkOperator then
    begin
      if Lex.Chr in ['+', '-'] then
      begin
        LexAccept();
        RightNode := Expr();
        if RightNode = nil then
          raise EParserError.Create('Expression expected after', Lex);
        CNode := TNodeBin.Create(Self, Lex, CNode, RightNode);
      end;
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(CNode);
      if E is EParserError then
        raise
      else
        raise EParserError.Create(E.Message);
    end;
  end;
  Result := CNode;
end;

function TParser.Term: TNode;
var
  CNode, RightNode: TNode;
  Lex: TLex;
begin
  CNode := nil;
  try
    CNode := Factor();
    Lex := LexC();

    if Lex.Token = tkOperator then
    begin
      if Lex.Chr in ['*', '/', '=', '&', '|', '<', '>', '~'] then
      begin
        LexAccept();
        RightNode := Expr();
        if RightNode = nil then
          raise EParserError.Create('Expression expected after', Lex);
        CNode := TNodeBin.Create(Self, Lex, CNode, RightNode);
      end;
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(CNode);
      if E is EParserError then
        raise
      else
        raise EParserError.Create(E.Message);
    end;
  end;
  Result := CNode;
end;

function TParser.Factor: TNode;
var
  CNode: TNode;
  fNode: TNodeFunction;
  Lex: TLex;
begin
  CNode := nil;
  try
    Lex := LexC();
    case Lex.token of
      tkLParen:
        begin
          LexAccept();
          CNode := Expr();
          if (LexC().Token = tkRParen) then
            LexAccept()
          else
            raise EParserError.Create('Expected closing parenthesis instead of', LexC());
        end;
      tkOperator:                       // unary minus
        begin
          if Lex.Chr in ['+', '-', '!'] then
          begin
            LexAccept();
            CNode := TNodeUnary.Create(Self, Lex, Factor());
          end
          else
            raise EParserError.Create('Unexpected ', Lex);
        end;
      tkNumber, tkInteger, tkString:
        begin
          CNode := TNodeCValue.Create(Self, Lex);
          LexAccept();
        end;
      tkIdentifier:
        begin
          if LexLook().Token = tkLParen then
          begin
            // function call
            LexAccept();
            fNode := TNodeFunction.Create(Self, Lex);
            LexAccept();
            CNode := fNode;
            if (LexC().token <> tkRParen) then
            begin
              fNode.AddArg(Expr());
              while LexC().Token = tkComa do
              begin
                LexAccept();
                fNode.AddArg(Expr());
              end;
            end;

            if (LexC().token = tkRParen) then
              LexAccept()
            else
              raise EParserError.Create('Expected closing parenthesis instead of', LexC());
          end
          else
          begin
            CNode := TNodeVariable.Create(Self, Lex);
            LexAccept();
          end;
        end;
      else
        raise EParserError.Create('Unexpected ', Lex);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(CNode);
      if E is EParserError then
        raise
      else
        raise EParserError.Create(E.Message);
    end;
  end;
  Result := CNode;
end;

{ TNode }

constructor TNode.Create(Parser: TParser);
begin
  inherited Create;
  FParser := Parser;
end;

{ TNodeBin }

constructor TNodeBin.Create(AParser: TParser; AOperator: TLex; ALeftNode, ARightNode: TNode);
begin
  inherited Create(AParser);
  FOperator := AOperator;
  FLeftNode := ALeftNode;
  FRightNode := ARightNode;
end;

destructor TNodeBin.Destroy;
begin
  FLeftNode.Free;
  FRightNode.Free;
  inherited Destroy;
end;

function TNodeBin.Eval: Variant;
var
  LeftValue, RightValue: Variant;

  function FixupBoolean(var AVal1: Variant; var AVal2: Variant): Boolean;
  begin
    Result := (TVarData(AVal1).VType = varBoolean) or (TVarData(AVal2).VType = varBoolean);
    if Result then
    begin
      if UpperCase(AVal1) = 'TRUE' then
        AVal1 := 1
      else
        AVal1 := 0;

      if UpperCase(AVal2) = 'TRUE' then
        AVal2 := 1
      else
        AVal2 := 0;
    end;
  end;

  function FixupDateTime(var AVal1: Variant; var AVal2: Variant): Boolean;
  begin
    Result := TVarData(AVal1).VType = varDate;
    if Result then
    begin
      if TVarData(AVal2).VType = varString then
        AVal2 := StrToDateTime(AVal2); //convert;
    end;
  end;

  function FixupString(var aVal: Variant): Boolean;
  begin
    Result:=((TVarData(aVal).VType = varString) {$IFDEF UNICODE}or (TVarData(aVal).VType = varUString){$ENDIF UNICODE}) and FParser.Parent.FCaseInsensitive;
    if Result then
      aVal := AnsiUpperCase(aVal);
  end;

  //returns 'True' if a conversion was necessary.
  function FixupValues(var AVal1: Variant; var AVal2: Variant): Boolean;
  var
    bChanged: Boolean;
  begin
    Result := FixupDateTime(AVal1, AVal2);
    if not Result then
      Result := FixupDateTime(AVal2, AVal1);
    if not Result then
      Result := FixupBoolean(AVal1, AVal2);
    if not Result then //ensure that the 'String' case is the last one
    begin
      Result := FixupString(AVal1);
      bChanged := FixupString(AVal2);
      Result := Result or bChanged; //ensure that both Fixups are executed regardless of optimisations
    end;
  end;

  function EvalLike: Boolean;
  var
    Wildcard1, Wildcard2: Boolean;
    LeftStr, RightStr: string;
  begin
    if (LeftValue = Null) or (RightValue = Null) then
      Result := (LeftValue = Null) and (RightValue = Null)
    else
    begin
      // Possiblilities:
      // Left hand contains wildcards -> Match right hand against left hand.
      // Right hand contains wildcards -> Match left hand against right hand.
      // Both hands contain wildcards -> Match for string equality as if no wildcards are supported.

      LeftStr := LeftValue;
      RightStr := RightValue;
      Wildcard1 := (Pos('*', LeftStr) > 0) or (Pos('?', LeftStr) > 0);
      Wildcard2 := (Pos('*', RightStr) > 0) or (Pos('?', RightStr) > 0);
      if Wildcard1 and not Wildcard2 then
        Result := MatchesMask(RightStr, LeftStr)
      else
      if Wildcard2 then
        Result := MatchesMask(LeftStr, RightStr)
      else
        Result := LeftValue = RightValue;
    end;
  end;

  function EvalEquality: Boolean;
  begin
    // Special case, at least one of both is null:
    if (LeftValue = Null) or (RightValue = Null) then
      Result := (LeftValue = Null) and (RightValue = Null)
    else
    begin
      if FParser.Parent.FEnableWildcardMatching and (TVarData(LeftValue).VType<>varDate) then
      begin
        Result := EvalLike;
      end
      else
        Result := LeftValue = RightValue;
    end;
  end;

  function EvalLT: Boolean;
  begin
    // Special case, at least one of both is Null:
    if (LeftValue = Null) or (RightValue = Null) then
      // Null is considered to be smaller than any value.
      Result := LeftValue = Null
    else
      Result := LeftValue < RightValue;
  end;

  function EvalGT: Boolean;
  begin
    // Special case, at least one of both is Null:
    if (LeftValue = Null) or (RightValue = Null) then
      // Null is considered to be smaller than any value.
      Result := RightValue = Null
    else
      Result := LeftValue > RightValue;
  end;

var
  LeftStr, RightStr: string;
begin
  // Determine values to have them handy.
  LeftValue := FLeftNode.Eval;
  RightValue := FRightNode.Eval;
  FixupValues(LeftValue, RightValue);

  case FOperator.Chr of
    '+':
      begin
        // force string concatenation
        if (TVarData(LeftValue).VType = varString) or
          (TVarData(LeftValue).VType = varOleStr) then
        begin
          LeftStr := LeftValue;
          RightStr := RightValue;
          LeftStr := LeftStr + RightStr;
          Result := LeftStr;
        end
        else
          Result := LeftValue + RightValue;
      end;
    '-': Result := LeftValue - RightValue;
    '*': Result := FLeftNode.Eval * FRightNode.Eval;
    '/': Result := FLeftNode.Eval / FRightNode.Eval;
    '=': Result := EvalEquality();
    '<': Result := EvalLT();
    '>': Result := EvalGT();
    '&': Result := FLeftNode.Eval and FRightNode.Eval;
    '|': Result := FLeftNode.Eval or FRightNode.Eval;
    '~': Result := EvalLike;
  else
    Result := Null;
  end;
end;

{ TNodeUnary }

constructor TNodeUnary.Create(AParser: TParser; AOperator: TLex; ARightNode: TNode);
begin
  inherited Create(AParser);
  FOperator := AOperator;
  FRightNode := ARightNode;
end;

destructor TNodeUnary.Destroy;
begin
  FRightNode.Free;
  inherited Destroy;
end;

function TNodeUnary.Eval: Variant;
begin
  Result := FRightNode.Eval();
  if FOperator.Chr = '-' then
    Result := -Result;
  if FOperator.Chr = '!' then
    Result := not Result;
end;

{ TNodeCValue }

constructor TNodeCValue.Create(AParser: TParser; ACValue: TLex);
begin
  inherited Create(AParser);
  FCValue := ACValue;
end;

function TNodeCValue.Eval: Variant;
begin
  case FCValue.Token of
    tkNumber:
      Result := StrToFloat(FCValue.Str);
    tkInteger:
      Result := StrToInt(FCValue.Str);
    tkString:
      Result := FCValue.Str;
  else
    Result := Null;
  end;
end;

{ TNodeFunction }

constructor TNodeFunction.Create(AParser: TParser; AFunc: TLex);
begin
  inherited Create(AParser);
  FArgs := TObjectList.Create(True);
  FFunc := AFunc;
end;

destructor TNodeFunction.Destroy;
begin
  FArgs.Free;
  inherited Destroy;
end;

procedure TNodeFunction.AddArg(Node: TNode);
begin
  FArgs.Add(Node);
end;

function TNodeFunction.Eval: Variant;
var
  Value: Variant;
  VArgs: Variant;
  I: Integer;
begin

  VArgs := VarArrayCreate([0, FArgs.Count - 1], varVariant);
  for I := 0 to FArgs.Count - 1 do
    VArgs[I] := TNode(FArgs[I]).Eval();
  Value := Null;
  if FParser.Parent.DoExecuteFunction(FFunc.Str, VArgs, Value) then
    Result := Value
  else
    raise EParserError.CreateFmt('Function %s could not be executed.', [FFunc.Str]);
end;

{ TNodeVariable }

constructor TNodeVariable.Create(AParser: TParser; ALex: TLex);
begin
  inherited Create(AParser);
  FLex := ALex;
end;

function TNodeVariable.Eval: Variant;
var
  Value: Variant;
begin
  Value := Null;
  if FParser.Parent.DoGetVariable(FLex.Str, Value) then
    Result := Value
  else
    raise EParserError.Create('Variable ' + FLex.Str + ' could not be fetched.');
end;

{ EParserError }

constructor EParserError.Create(const Msg: string; Lex: TLex);
begin
  inherited CreateFmt('%s %s', [Msg, Lex.Debug]);
end;

{ TExprParser }

constructor TExprParser.Create;
begin
  inherited Create;
  FErrorMessage := '';
end;

destructor TExprParser.Destroy;
begin
  FParser.Free;
  FScan.Free;
  inherited Destroy;
end;

function TExprParser.Eval(): Boolean;
var
  Parser: TParser;
  {$IFDEF TESTING_PARSER}
  Scan: TScan;
  {$ENDIF TESTING_PARSER}
begin
  FErrorMessage := '';
  {$IFDEF TESTING_PARSER}
  DebugText := '';
  Scan := TScan(FScan);
  Scan.DebugPrint();
  {$ENDIF TESTING_PARSER}
  Parser := TParser(FParser);
  if Parser.Execute() then
  begin
    FValue := Parser.Value;
    Result := True;
  end
  else
  begin
    FErrorMessage := Parser.ErrorMessage;
    Result := False;
  end
end;

function TExprParser.Eval(const AExpression: string): Boolean;
begin
  SetExpression(AExpression);
  Result := Eval();
end;

procedure TExprParser.SetCaseInsensitive(const Value: Boolean);
begin
  FCaseInsensitive := Value;
end;

procedure TExprParser.SetExpression(const Value: string);
begin
  if Value <> FExpression then
  begin
    FExpression := Value;
    FParser.Free;
    FScan.Free;
    FParser := TParser.Create;
    TParser(FParser).Parent := Self;
    FScan := TScan.Create;
    if not TScan(FScan).Parse(FExpression) then
      FErrorMessage := TScan(FScan).ErrorMessage
    else
    begin
      TParser(FParser).Scan := TScan(FScan);
      TParser(FParser).Parse();
    end;
  end;
end;

function TExprParser.DoGetVariable(const VarName: string; var Value: Variant): Boolean;
begin
  Result := False;
  if Assigned(FOnGetVariable) then
    Result := FOnGetVariable(Self, VarName, Value);
end;

function TExprParser.DoExecuteFunction(const FuncName: string; const Args: Variant; var ResVal: Variant): Boolean;
begin
  Result := False;
  if Assigned(FOnExecuteFunction) then
    Result := FOnExecuteFunction(Self, FuncName, Args, ResVal);
end;

end.
