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

class       : TJvIParser
description : text parser

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvHLParser;

interface

uses SysUtils, Classes;

const
  ieBadRemark = 1;

type

  TIParserStyle = (psNone, psPascal, psCpp, psPython, psVB, psHtml, psPerl, psCocoR, psPhp);

  TJvIParser = class
  protected
    FpcProgram: PChar;
    FpcPos: PChar; // текущая позиция
    FHistory: TStringList;
    FHistorySize: integer;
    FHistoryPtr: integer;
    FStyle: TIParserStyle;
    FReturnComments: Boolean;

    function HistoryInd(index: integer): integer;
    function GetHistory(index: integer): string;
    function GetPosBeg(index: integer): integer;
    function GetPosEnd(index: integer): integer;
    procedure SetHistorySize(Size: integer);
    function GetPos: integer;
  public
    constructor Create;
    destructor Destroy; override;
    {возвращает следующую лексему сдвигает текущую позицию}
    function Token: string;
    {возвращает следующую лексему слева от текущей позиции сдвигает текущую позицию влево}
    // function TokenL : string; - чертовски трудно это сделать #;-(
    {откат назад на указанное количество лексем}
    procedure RollBack(index: integer);
    property History[index: integer]: string read GetHistory;
    property PosBeg[index: integer]: integer read GetPosBeg;
    property PosEnd[index: integer]: integer read GetPosEnd;
    property HistorySize: integer read FHistorySize write SetHistorySize;
    property Pos: integer read GetPos;
    property pcPos: PChar read FpcPos write FpcPos;
    property pcProgram: PChar read FpcProgram write FpcProgram;
    property Style: TIParserStyle read FStyle write FStyle;
    property ReturnComments: Boolean read FReturnComments write FReturnComments;
  end;

  EJvIParserError  = class(Exception)
  public
    ErrCode: integer;
    Pos: integer;
    constructor Create(AErrCode: integer; APos: integer);
  end;

  function IsStringConstant(const St: string): boolean;
  function IsIntConstant(const St: string): boolean;
  function IsRealConstant(const St: string): boolean;
  function IsIdentifer(const ID: string): boolean;
  function GetStringValue(const St: string): string;
  procedure ParseString(const S: string; Ss: TStrings);

implementation

uses JvCtlConst;

{$IFDEF Delphi}
type
  TSetOfChar = set of char;
{$ENDIF Delphi}
{$IFDEF BCB}
type
  TSetOfChar = string;
{$ENDIF BCB}

function CharInSet(const Ch : Char; const SetOfChar : TSetOfChar) : boolean;
begin
{$IFDEF Delphi}
  Result := Ch in SetOfChar;
{$ENDIF Delphi}
{$IFDEF BCB}
  Result := Pos(Ch, SetOfChar) > 0;
{$ENDIF BCB}
end;

constructor EJvIParserError .Create(AErrCode: integer; APos: integer);
begin
  ErrCode := AErrCode;
  Pos := APos;
end;


{*************************** TJvIParser ****************************}

constructor TJvIParser.Create;
begin
  inherited Create;
  FHistory := TStringList.Create;
  HistorySize := 10;
  Style := psPascal;
end;

destructor TJvIParser.Destroy;
begin
  FHistory.Free;
  inherited Destroy;
end;

function TJvIParser.Token: string;
var
  P, F: PChar;
const
  {$IFDEF Delphi}
  StSkip = [' ', #10, #13];
  {$ENDIF Delphi}
  {$IFDEF BCB}
  StSkip = ' '#10#13;
  {$ENDIF BCB}

  Function SkipComments:Boolean;
  begin
    SkipComments:=True;
    case P[0] of
      '{':
        if FStyle = psPascal then
        begin
          F := StrScan(P + 1, '}');
          if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
             Exit;
          P := F + 1;
        end;
      '}':
        if FStyle = psPascal then //IParserError(ieBadRemark, P - FpcProgram);
           Exit;
      '(':
        if (FStyle in [psPascal, psCocoR]) and (P[1] = '*') then
        begin
          F := P + 2;
          while true do
          begin
            F := StrScan(F, '*');
            if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
               Exit;
            if F[1] = ')' then
            begin
              inc(F);
              break;
            end;
            inc(F);
          end;
          P := F + 1;
        end;
      '*':
        if FStyle in [psPascal, psCocoR] then
        begin
          if (P[1] = ')') then
            //IParserError(ieBadRemark, P - FpcProgram)
            Exit;
        end
        else if FStyle in [psCpp, psPhp] then
          if P[1]='/' then //IParserError(ieBadRemark, P - FpcProgram);
             Exit;
      '/':
        if (FStyle in [psPascal, psCpp, psCocoR, psPhp]) and (P[1] = '/') then
        begin
          F := StrScan(P + 1, #13);
          if F = nil then F := StrEnd(P + 1);
          P := F;
        end
        else if (FStyle in [psCpp, psCocoR, psPhp]) and (P[1] = '*') then
        begin
          F := P + 2;
          while true do
          begin
            F := StrScan(F, '*');
            if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
               Exit;
            if F[1] = '/' then
            begin
              inc(F);
              break;
            end;
            inc(F);
          end;
          P := F + 1;
        end;
      '#':
        if (FStyle in [psPython, psPerl]) { and
           ((P = FpcProgram) or (P[-1] in [#10, #13])) } then
        begin
          F := StrScan(P + 1, #13);
          if F = nil then F := StrEnd(P + 1);
          P := F;
        end;
      '''':
        if FStyle = psVB then
        begin
          F := StrScan(P + 1, #13);
          if F = nil then F := StrEnd(P + 1);
          P := F;
        end;
    end;
    SkipComments:=False;
  end;

  procedure Return;
  begin
    FpcPos := P;
    FHistory[FHistoryPtr] := Result;
    FHistory.Objects[FHistoryPtr] := TObject(Pos - 1);
    inc(FHistoryPtr);
    if FHistoryPtr > FHistorySize - 1 then FHistoryPtr := 0;
  end; { Return }

var
  F1: PChar;
  i: integer;
begin
  { New Token - начать чтение новой лексемы }
  F := FpcPos;
  P := FpcPos;
  { Firstly skip spaces and remarks }
  repeat
    while CharInSet(P[0], StSkip) do
      inc(P);
    F1 := P;
    try
      If SkipComments Then
         P:=StrEnd(F1);
    except
      on E: EJvIParserError  do
        if (E.ErrCode = ieBadRemark) and ReturnComments then
          P := StrEnd(F1)
        else
          raise;
    end;
    if ReturnComments and (P > F1) then
    begin
      SetString(Result, F1, P - F1);
      Return;
      Exit;
    end;
    while CharInSet(P[0], StSkip) do
      inc(P);
  until F1 = P;

  F := P;
  if FStyle <> psHtml then
  begin
    if CharInSet(P[0], StIdFirstSymbols) then
      { token }
    begin
      while CharInSet(P[0], StIdSymbols) do
        inc(P);
      SetString(Result, F, P - F);
    end
    else if CharInSet(P[0], StConstSymbols10) then
      { number }
    begin
      while CharInSet(P[0], StConstSymbols10) or (P[0] = '.') do
        inc(P);
      SetString(Result, F, P - F);
    end
    else if (Style = psPascal) and (P[0] = '$') and
      CharInSet(P[1], StConstSymbols) then
      { pascal hex number }
    begin
      inc(P);
      while CharInSet(P[0], StConstSymbols) do
        inc(P);
      SetString(Result, F, P - F);
    end
    else if (Style = psPerl) and (P[0] in ['$', '@', '%', '&']) then
      { perl identifer }
    begin
      inc(P);
      while CharInSet(P[0], StIdSymbols) do
        inc(P);
      SetString(Result, F, P - F);
    end
    else if P[0] = '''' then
      { pascal string constant }
    begin
      inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '''' then
          if P[1] = '''' then
            inc(P)
          else
            break;
        inc(P);
      end;
      if P[0] <> #0 then inc(P);
      SetString(Result, F, P - F);
      i := 2;
      while i < Length(Result) - 1 do
      begin
        if Result[i] = '''' then
          Delete(Result, i, 1);
        inc(i);
      end;
    end
    else if (FStyle in [psCpp, psCocoR]) and (P[0] = '"') then
      { C++ string constant }
    begin
      inc(P);
      while P[0] <> #0 do
      begin
        if (P[0] = '"') and (P[-1] <> '\') then
          break;
        inc(P);
      end;
      if P[0] <> #0 then inc(P);
      SetString(Result, F, P - F);
    end
    else if ((FStyle in [psPython, psVB, psHtml]) and (P[0] = '"')) or
            ((FStyle in [psPerl, psPhp]) and (P[0] = '"') and ((P = FpcPos) or (P[-1] <> '/'))) then
      { Python, VB, Html, Perl string constant }
    begin
      inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '"' then
          break;
        inc(P);
      end;
      if P[0] <> #0 then inc(P);
      SetString(Result, F, P - F);
    end
    else if P[0] = #0 then
      Result := ''
    else
    begin
      Result := P[0];
      inc(P);
    end;
  end
  else { html }
  begin
    if (P[0] in ['=', '<', '>']) or
       ((P <> pcProgram) and (P[0] = '/') and (P[-1] = '<')) then
    begin
      Result := P[0];
      inc(P);
    end
    else if P[0] = '"' then
      { Html string constant }
    begin
      inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '"' then
          break;
        inc(P);
      end;
      if P[0] <> #0 then inc(P);
      SetString(Result, F, P - F);
    end
    else
    begin
      while not (P[0] in [#0, ' ', '=', '<', '>']) do
        inc(P);
      SetString(Result, F, P - F);
    end;
  end;
  Return;
end;

function TJvIParser.HistoryInd(index: integer): integer;
begin
  Result := FHistoryPtr - 1 - index;
  if Result < 0 then Result := Result + FHistorySize;
end;

function TJvIParser.GetHistory(index: integer): string;
begin
  Result := FHistory[HistoryInd(index)];
end;

function TJvIParser.GetPosEnd(index: integer): integer;
begin
  Result := integer(FHistory.Objects[HistoryInd(index)]) + 1;
end;

function TJvIParser.GetPosBeg(index: integer): integer;
var
  i: integer;
  S: string;
begin
  i := HistoryInd(index);
  S := FHistory[i];
  Result := integer(FHistory.Objects[i]) - Length(S) + 1;
  case FStyle of
    psPascal:
      if S[1] = '''' then
        for i := 2 to Length(S) - 1 do
          if S[i] = '''' then
            dec(Result);
  end;
end;

procedure TJvIParser.SetHistorySize(Size: integer);
{$IFDEF DEBUG}
var
  i: integer;
  {$ENDIF}
begin
  while Size > FHistorySize do
  begin
    FHistory.Add('');
    inc(FHistorySize);
  end;
  while Size < FHistorySize do
  begin
    FHistory.Delete(0);
    dec(FHistorySize);
  end;
  {$IFDEF DEBUG}
  for i := 0 to FHistorySize - 1 do
    FHistory[i] := '';
  {$ENDIF}
  FHistoryPtr := 0;
end;

function TJvIParser.GetPos: integer;
begin
  Result := pcPos - FpcProgram;
end;

procedure TJvIParser.RollBack(index: integer);
begin
  FpcPos := PosEnd[index] + FpcProgram;
  dec(FHistoryPtr, index);
  if FHistoryPtr < 0 then
    FHistoryPtr := FHistorySize + FHistoryPtr;
end;

{########################### TJvIParser ###########################}

procedure ParseString(const S: string; Ss: TStrings);
var
  Parser: TJvIParser;
  Token: string;
begin
  Ss.Clear;
  Parser := TJvIParser.Create;
  try
    Parser.pcProgram := PChar(S);
    Parser.pcPos := Parser.pcProgram;
    Token := Parser.Token;
    while Token <> '' do
    begin
      Ss.Add(Token);
      Token := Parser.Token;
    end;
  finally
    Parser.Free;
  end;
end;


function IsStringConstant(const St: string): boolean;
var
  LS: integer;
begin
  LS := Length(St);
  if (LS >= 2) and (((St[1] = '''') and (St[LS] = '''')) or
    ((St[1] = '"') and (St[LS] = '"'))) then
    Result := true
  else
    Result := false
end;

function IsRealConstant(const St: string): boolean;
var
  i, j: integer;
  Point: boolean;
begin
  Result := false;
  if (St = '.') or (St = '') then exit;
  if St[1] = '-' then
    if Length(St) = 1 then
      exit
    else
      j := 2
  else
    j := 1;
  Point := false;
  for i := j to Length(St) do
    if St[i] = '.' then
      if Point then
        exit
      else
        Point := true
    else if (St[i] < '0') or (St[i] > '9') then
      exit;
  Result := true;
end;

function IsIntConstant(const St: string): boolean;
var
  i, j: integer;
  Sym: TSetOfChar;
begin
  Result := false;
  if (Length(St) = 0) or ((Length(St) = 1) and (St[1] = '$')) then exit;
  Sym := StConstSymbols10;
  if (St[1] = '-') or (St[1] = '$') then
  begin
    if Length(St) = 1 then
      exit
    else
      j := 2;
    if St[1] = '$' then Sym := StConstSymbols;
  end
  else
    j := 1;
  for i := j to Length(St) do
    if not CharInSet(St[i], Sym) then exit;
  Result := true;
end;

function IsIdentifer(const ID: string): boolean;
var
  i, L: integer;
begin
  Result := false;
  L := Length(ID);
  if L = 0 then exit;
  if not CharInSet(ID[1], StIdFirstSymbols) then exit;
  for i := 1 to L do
  begin
    if not CharInSet(ID[1], StIdSymbols) then exit;
  end;
  Result := true;
end;

function GetStringValue(const St: string): string;
begin
  if IsStringConstant(St) then
    Result := Copy(St, 2, Length(St) - 2)
  else
    Result := St;
end;

end.

