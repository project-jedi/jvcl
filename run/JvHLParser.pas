{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHLParser.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

class       : TJvIParser
description : text parser

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]

-----------------------------------------------------------------------------}
// $Id$

{history:
3.0:
  2003-09-20: (changes by Andreas Hausladen)
    - added a TJvIParserW parser for unicode text
    - added unicode versions of the functions
}

unit JvHLParser;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Dialogs,
  JclWideStrings,
  JvTypes, JvJCLUtils;

const
  ieBadRemark = 1;

type
  TIParserStyle = (psNone, psPascal, psCpp, psPython, psVB, psHtml, psPerl, psCocoR, psPhp);

  TJvIParser = class(TObject)
  protected
    FpcProgram: PChar;
    FpcPos: PChar; // Current position [translated]
    FHistory: TStringList;
    FHistorySize: Integer;
    FHistoryPtr: Integer;
    FStyle: TIParserStyle;
    FReturnComments: Boolean;
    function HistoryInd(Index: Integer): Integer;
    function GetHistory(Index: Integer): string;
    function GetPosBeg(Index: Integer): Integer;
    function GetPosEnd(Index: Integer): Integer;
    procedure SetHistorySize(Size: Integer);
    function GetPos: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    { Returns the following token; shifts a current position [translated] }
    function Token: string;
    { Returns the following token to the left of a current position shifts
      a current position to the left [translated]
    function TokenL : string; - It is devilishly difficult to make it *;-( [translated] }
    { Rollback back on the indicated quantity of tokens [translated] }
    procedure RollBack(Index: Integer);
    property History[Index: Integer]: string read GetHistory;
    property PosBeg[Index: Integer]: Integer read GetPosBeg;
    property PosEnd[Index: Integer]: Integer read GetPosEnd;
    property HistorySize: Integer read FHistorySize write SetHistorySize;
    property Pos: Integer read GetPos;
    // (rom) name change needed
    property pcPos: PChar read FpcPos write FpcPos;
    property pcProgram: PChar read FpcProgram write FpcProgram;
    property Style: TIParserStyle read FStyle write FStyle;
    property ReturnComments: Boolean read FReturnComments write FReturnComments;
  end;

  TJvIParserW = class(TObject)
  protected
    FpcProgram: PWideChar;
    FpcPos: PWideChar; // Current position [translated]
    FHistory: TWStrings;
    FHistorySize: Integer;
    FHistoryPtr: Integer;
    FStyle: TIParserStyle;
    FReturnComments: Boolean;
    function HistoryInd(Index: Integer): Integer;
    function GetHistory(Index: Integer): WideString;
    function GetPosBeg(Index: Integer): Integer;
    function GetPosEnd(Index: Integer): Integer;
    procedure SetHistorySize(Size: Integer);
    function GetPos: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    { Returns the following token; shifts a current position [translated] }
    function Token: WideString;
    { Returns the following token to the left of a current position shifts
      a current position to the left [translated]
    function TokenL : string; - It is devilishly difficult to make it *;-( [translated] }
    { Rollback back on the indicated quantity of tokens [translated] }
    procedure RollBack(Index: Integer);
    property History[Index: Integer]: WideString read GetHistory;
    property PosBeg[Index: Integer]: Integer read GetPosBeg;
    property PosEnd[Index: Integer]: Integer read GetPosEnd;
    property HistorySize: Integer read FHistorySize write SetHistorySize;
    property Pos: Integer read GetPos;
    // (rom) name change needed
    property pcPos: PWideChar read FpcPos write FpcPos;
    property pcProgram: PWideChar read FpcProgram write FpcProgram;
    property Style: TIParserStyle read FStyle write FStyle;
    property ReturnComments: Boolean read FReturnComments write FReturnComments;
  end;

  EJvIParserError = class(Exception)
  private
    FErrCode: Integer;
    FPosition: Cardinal;
  public
    constructor Create(AErrCode: Integer; APosition: Cardinal; Dummy: Integer = 0);
    property ErrCode: Integer read FErrCode;
    property Position: Cardinal read FPosition;
  end;

function IsStringConstant(const St: string): Boolean;
function IsIntConstant(const St: string): Boolean;
function IsRealConstant(const St: string): Boolean;
function IsIdentifier(const ID: string): Boolean;
function GetStringValue(const St: string): string;
procedure ParseString(const S: string; Ss: TStrings);

function IsStringConstantW(const St: WideString): Boolean;
function IsIntConstantW(const St: WideString): Boolean;
function IsRealConstantW(const St: WideString): Boolean;
function IsIdentifierW(const ID: WideString): Boolean;
function GetStringValueW(const St: WideString): WideString;
procedure ParseStringW(const S: WideString; Ss: TWStrings);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvConsts;

//=== { EJvIParserError } ====================================================

constructor EJvIParserError.Create(AErrCode: Integer; APosition: Cardinal; Dummy: Integer = 0);
begin
  inherited Create('');
  FErrCode := AErrCode;
  FPosition := APosition;
end;

//=== { TJvIParser } =========================================================

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
const
  StSkip = [' ', Lf, Cr];
var
  P, F: PChar;
  F1: PChar;
  I: Integer;

  function SkipComments: Boolean;
  begin
    SkipComments := True;
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
          if P[2] = #0 then
            Exit; // line end
          F := P + 2;
          while True do
          begin
            F := StrScan(F, '*');
            if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
              Exit;
            if F[1] = ')' then
            begin
              Inc(F);
              Break;
            end;
            Inc(F);
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
        else
        if FStyle in [psCpp, psPhp] then
          if P[1] = '/' then //IParserError(ieBadRemark, P - FpcProgram);
            Exit;
      '/':
        if (FStyle in [psPascal, psCpp, psCocoR, psPhp]) and (P[1] = '/') then
        begin
          F := StrScan(P + 1, Cr);
          if F = nil then
            F := StrEnd(P + 1);
          P := F;
        end
        else
        if (FStyle in [psCpp, psCocoR, psPhp]) and (P[1] = '*') then
        begin
          if P[2] = #0 then
            Exit; // line end
          F := P + 2;
          while True do
          begin
            F := StrScan(F, '*');
            if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
              Exit;
            if F[1] = '/' then
            begin
              Inc(F);
              Break;
            end;
            Inc(F);
          end;
          P := F + 1;
        end;
      '#':
        if (FStyle in [psPython, psPerl]) { and
           ((P = FpcProgram) or (P[-1] in [Lf, Cr])) }then
        begin
          F := StrScan(P + 1, Cr);
          if F = nil then
            F := StrEnd(P + 1);
          P := F;
        end;
      '''':
        if FStyle = psVB then
        begin
          F := StrScan(P + 1, Cr);
          if F = nil then
            F := StrEnd(P + 1);
          P := F;
        end;
    end;
    SkipComments := False;
  end;

  procedure Return;
  begin
    FpcPos := P;
    FHistory[FHistoryPtr] := Result;
    FHistory.Objects[FHistoryPtr] := TObject(Pos - 1);
    Inc(FHistoryPtr);
    if FHistoryPtr > FHistorySize - 1 then
      FHistoryPtr := 0;
  end;

begin
  { New Token - To begin reading a new token [translated] }
  F := FpcPos;
  P := FpcPos;
  { Firstly skip spaces and remarks }
  repeat
    while P[0] in StSkip do
      Inc(P);
    F1 := P;
    try
      if SkipComments then
        P := StrEnd(F1);
    except
      on E: EJvIParserError do
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
    while P[0] in StSkip do
      Inc(P);
  until F1 = P;

  F := P;
  if FStyle <> psHtml then
  begin
    if P[0] in IdentifierFirstSymbols then
    { token }
    begin
      while P[0] in IdentifierSymbols do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if P[0] in DigitSymbols then
    { number }
    begin
      while (P[0] in DigitSymbols) or (P[0] = '.') do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if (Style = psPascal) and (P[0] = '$') and
      (P[1] in HexadecimalSymbols) then
    { pascal hex number }
    begin
      Inc(P);
      while (P[0] in HexadecimalSymbols) do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if (Style = psPerl) and (P[0] in ['$', '@', '%', '&']) then
    { perl identifier }
    begin
      Inc(P);
      while (P[0] in IdentifierSymbols) do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if P[0] = '''' then
    { pascal string constant }
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
      if P[0] <> #0 then
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
    if (FStyle in [psCpp, psCocoR]) and (P[0] = '"') then
    { C++ string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if (P[0] = '"') and (P[-1] <> '\') then
          Break;
        if (P[0] = '"') and (P[-1] = '\') then
        begin
         // count the backslashes, on even backslahses it is a string end
          I := 1;
          while (P - 1 - I > F) and (P[-1 - I] = '\') do
            Inc(I);
          if I and $01 = 0 then
            Break;  { same but faster than: if I mod 2 = 0 then Break; }
        end;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if ((FStyle in [psPython, psVB, psHtml]) and (P[0] = '"')) or
      ((FStyle in [psPerl, psPhp]) and (P[0] = '"') and ((P = FpcPos) or (P[-1] <> '/'))) then
    { Python, VB, Html, Perl string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '"' then
          Break;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if P[0] = #0 then
      Result := ''
    else
    begin
      Result := P[0];
      Inc(P);
    end;
  end
  else { html }
  begin
    if (P[0] in ['=', '<', '>']) or
      ((P <> pcProgram) and (P[0] = '/') and (P[-1] = '<')) then
    begin
      Result := P[0];
      Inc(P);
    end
    else
    if P[0] = '"' then
    { Html string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '"' then
          Break;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    begin
      while not (P[0] in [#0, ' ', '=', '<', '>']) do
        Inc(P);
      SetString(Result, F, P - F);
    end;
  end;
  Return;
end;

function TJvIParser.HistoryInd(Index: Integer): Integer;
begin
  Result := FHistoryPtr - 1 - Index;
  if Result < 0 then
    Result := Result + FHistorySize;
end;

function TJvIParser.GetHistory(Index: Integer): string;
begin
  Result := FHistory[HistoryInd(Index)];
end;

function TJvIParser.GetPosEnd(Index: Integer): Integer;
begin
  Result := Integer(FHistory.Objects[HistoryInd(Index)]) + 1;
end;

function TJvIParser.GetPosBeg(Index: Integer): Integer;
var
  I: Integer;
  S: string;
begin
  I := HistoryInd(Index);
  S := FHistory[I];
  Result := Integer(FHistory.Objects[I]) - Length(S) + 1;
  case FStyle of
    psPascal:
      if S[1] = '''' then
        for I := 2 to Length(S) - 1 do
          if S[I] = '''' then
            Dec(Result);
  end;
end;

procedure TJvIParser.SetHistorySize(Size: Integer);
begin
  while Size > FHistorySize do
  begin
    FHistory.Add('');
    Inc(FHistorySize);
  end;
  while Size < FHistorySize do
  begin
    FHistory.Delete(0);
    Dec(FHistorySize);
  end;
  FHistoryPtr := 0;
end;

function TJvIParser.GetPos: Integer;
begin
  Result := pcPos - FpcProgram;
end;

procedure TJvIParser.RollBack(Index: Integer);
begin
  FpcPos := PosEnd[Index] + FpcProgram;
  Dec(FHistoryPtr, Index);
  if FHistoryPtr < 0 then
    FHistoryPtr := FHistorySize + FHistoryPtr;
end;

//=== { TJvIParserW } ========================================================

constructor TJvIParserW.Create;
begin
  inherited Create;
  FHistory := TWStringList.Create;
  HistorySize := 10;
  Style := psPascal;
end;

destructor TJvIParserW.Destroy;
begin
  FHistory.Free;
  inherited Destroy;
end;

function TJvIParserW.Token: WideString;
const
  StSkip = [' ', Lf, Cr];
var
  P, F: PWideChar;
  F1: PWideChar;
  I: Integer;

  function SkipComments: Boolean;
  begin
    SkipComments := True;
    case P[0] of
      '{':
        if FStyle = psPascal then
        begin
          F := StrScanW(P + 1, WideChar('}'));
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
          if P[2] = #0 then
            Exit; // line end
          F := P + 2;
          while True do
          begin
            F := StrScanW(F, WideChar('*'));
            if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
              Exit;
            if F[1] = ')' then
            begin
              Inc(F);
              Break;
            end;
            Inc(F);
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
        else
        if FStyle in [psCpp, psPhp] then
          if P[1] = '/' then //IParserError(ieBadRemark, P - FpcProgram);
            Exit;
      '/':
        if (FStyle in [psPascal, psCpp, psCocoR, psPhp]) and (P[1] = '/') then
        begin
          F := StrScanW(P + 1, WideChar(Cr));
          if F = nil then
            F := StrEndW(P + 1);
          P := F;
        end
        else
        if (FStyle in [psCpp, psCocoR, psPhp]) and (P[1] = '*') then
        begin
          if P[2] = #0 then
            Exit; // line end
          F := P + 2;
          while True do
          begin
            F := StrScanW(F, WideChar('*'));
            if F = nil then //IParserError(ieBadRemark, P - FpcProgram);
              Exit;
            if F[1] = '/' then
            begin
              Inc(F);
              Break;
            end;
            Inc(F);
          end;
          P := F + 1;
        end;
      '#':
        if (FStyle in [psPython, psPerl]) { and
           ((P = FpcProgram) or (P[-1] in [Lf, Cr])) }then
        begin
          F := StrScanW(P + 1, WideChar(Cr));
          if F = nil then
            F := StrEndW(P + 1);
          P := F;
        end;
      '''':
        if FStyle = psVB then
        begin
          F := StrScanW(P + 1, WideChar(Cr));
          if F = nil then
            F := StrEndW(P + 1);
          P := F;
        end;
    end;
    SkipComments := False;
  end;

  procedure Return;
  begin
    FpcPos := P;
    FHistory.PStrings[FHistoryPtr]^ := Result;
    FHistory.Objects[FHistoryPtr] := TObject(Pos - 1);
    Inc(FHistoryPtr);
    if FHistoryPtr > FHistorySize - 1 then
      FHistoryPtr := 0;
  end;

begin
  { New Token - To begin reading a new token [translated] }
  F := FpcPos;
  P := FpcPos;
  { Firstly skip spaces and remarks }
  repeat
    while CharInSetW(P[0], StSkip) do
      Inc(P);
    F1 := P;
    try
      if SkipComments then
        P := StrEndW(F1);
    except
      on E: EJvIParserError do
        if (E.ErrCode = ieBadRemark) and ReturnComments then
          P := StrEndW(F1)
        else
          raise;
    end;
    if ReturnComments and (P > F1) then
    begin
      SetString(Result, F1, P - F1);
      Return;
      Exit;
    end;
    while CharInSetW(P[0], StSkip) do
      Inc(P);
  until F1 = P;

  F := P;
  if FStyle <> psHtml then
  begin
    if CharInSetW(P[0], IdentifierFirstSymbols) then
    { token }
    begin
      while CharInSetW(P[0], IdentifierSymbols) do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if CharInSetW(P[0], DigitSymbols) then
    { number }
    begin
      while CharInSetW(P[0], DigitSymbols) or (P[0] = '.') do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if (Style = psPascal) and (P[0] = '$') and
      CharInSetW(P[1], HexadecimalSymbols) then
    { pascal hex number }
    begin
      Inc(P);
      while CharInSetW(P[0], HexadecimalSymbols) do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if (Style = psPerl) and CharInSetW(P[0], ['$', '@', '%', '&']) then
    { perl identifier }
    begin
      Inc(P);
      while CharInSetW(P[0], IdentifierSymbols) do
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if P[0] = '''' then
    { pascal string constant }
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
      if P[0] <> #0 then
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
    if (FStyle in [psCpp, psCocoR]) and (P[0] = '"') then
    { C++ string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if (P[0] = '"') and (P[-1] <> '\') then
          Break;
        if (P[0] = '"') and (P[-1] = '\') then
        begin
         // count the backslashes, on even backslahses it is a string end
          I := 1;
          while (P - 1 - I > F) and (P[-1 - I] = '\') do
            Inc(I);
          if I and $01 = 0 then
            Break;  { same but faster than: if I mod 2 = 0 then Break; }
        end;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if ((FStyle in [psPython, psVB, psHtml]) and (P[0] = '"')) or
      ((FStyle in [psPerl, psPhp]) and (P[0] = '"') and ((P = FpcPos) or (P[-1] <> '/'))) then
    { Python, VB, Html, Perl string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '"' then
          Break;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    if P[0] = #0 then
      Result := ''
    else
    begin
      Result := P[0];
      Inc(P);
    end;
  end
  else { html }
  begin
    if CharInSetW(P[0], ['=', '<', '>']) or
      ((P <> pcProgram) and (P[0] = '/') and (P[-1] = '<')) then
    begin
      Result := P[0];
      Inc(P);
    end
    else
    if P[0] = '"' then
    { Html string constant }
    begin
      Inc(P);
      while P[0] <> #0 do
      begin
        if P[0] = '"' then
          Break;
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
      SetString(Result, F, P - F);
    end
    else
    begin
      while not CharInSetW(P[0], [#0, ' ', '=', '<', '>']) do
        Inc(P);
      SetString(Result, F, P - F);
    end;
  end;
  Return;
end;

function TJvIParserW.HistoryInd(Index: Integer): Integer;
begin
  Result := FHistoryPtr - 1 - Index;
  if Result < 0 then
    Result := Result + FHistorySize;
end;

function TJvIParserW.GetHistory(Index: Integer): WideString;
begin
  Result := FHistory[HistoryInd(Index)];
end;

function TJvIParserW.GetPosEnd(Index: Integer): Integer;
begin
  Result := Integer(FHistory.Objects[HistoryInd(Index)]) + 1;
end;

function TJvIParserW.GetPosBeg(Index: Integer): Integer;
var
  I: Integer;
  S: WideString;
begin
  I := HistoryInd(Index);
  S := FHistory[I];
  Result := Integer(FHistory.Objects[I]) - Length(S) + 1;
  case FStyle of
    psPascal:
      if S[1] = '''' then
        for I := 2 to Length(S) - 1 do
          if S[I] = '''' then
            Dec(Result);
  end;
end;

procedure TJvIParserW.SetHistorySize(Size: Integer);
begin
  while Size > FHistorySize do
  begin
    FHistory.Add('');
    Inc(FHistorySize);
  end;
  while Size < FHistorySize do
  begin
    FHistory.Delete(0);
    Dec(FHistorySize);
  end;
  FHistoryPtr := 0;
end;

function TJvIParserW.GetPos: Integer;
begin
  Result := pcPos - FpcProgram;
end;

procedure TJvIParserW.RollBack(Index: Integer);
begin
  FpcPos := PosEnd[Index] + FpcProgram;
  Dec(FHistoryPtr, Index);
  if FHistoryPtr < 0 then
    FHistoryPtr := FHistorySize + FHistoryPtr;
end;

//============================================================================

procedure ParseString(const S: string; Ss: TStrings);
var
  Parser: TJvIParser;
  Token: string;
begin
  Ss.BeginUpdate;
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
    Ss.EndUpdate;
  end;
end;

procedure ParseStringW(const S: WideString; Ss: TWStrings);
var
  Parser: TJvIParserW;
  Token: WideString;
begin
  Ss.BeginUpdate;
  Ss.Clear;
  Parser := TJvIParserW.Create;
  try
    Parser.pcProgram := PWideChar(S);
    Parser.pcPos := Parser.pcProgram;
    Token := Parser.Token;
    while Token <> '' do
    begin
      Ss.Add(Token);
      Token := Parser.Token;
    end;
  finally
    Parser.Free;
    Ss.EndUpdate;
  end;
end;

function IsStringConstant(const St: string): Boolean;
var
  LS: Integer;
begin
  LS := Length(St);
  Result := (LS >= 2) and (((St[1] = '''') and (St[LS] = '''')) or
    ((St[1] = '"') and (St[LS] = '"')));
end;

function IsStringConstantW(const St: WideString): Boolean;
var
  LS: Integer;
begin
  LS := Length(St);
  Result := (LS >= 2) and (((St[1] = '''') and (St[LS] = '''')) or
    ((St[1] = '"') and (St[LS] = '"')));
end;

function IsRealConstant(const St: string): Boolean;
var
  I, J: Integer;
  Point: Boolean;
begin
  Result := False;
  if (St = '.') or (St = '') then
    Exit;
  if St[1] = '-' then
    if Length(St) = 1 then
      Exit
    else
      J := 2
  else
    J := 1;
  Point := False;
  for I := J to Length(St) do
    if St[I] = '.' then
      if Point then
        Exit
      else
        Point := True
    else
    if not (St[I] in DigitSymbols) then
      Exit;
  Result := True;
end;

function IsRealConstantW(const St: WideString): Boolean;
var
  I, J: Integer;
  Point: Boolean;
begin
  Result := False;
  if (St = '.') or (St = '') then
    Exit;
  if St[1] = '-' then
    if Length(St) = 1 then
      Exit
    else
      J := 2
  else
    J := 1;
  Point := False;
  for I := J to Length(St) do
    if St[I] = '.' then
      if Point then
        Exit
      else
        Point := True
    else
    if (St[I] < WideChar('0')) or (St[I] > WideChar('9')) then
      Exit;
  Result := True;
end;

function IsIntConstant(const St: string): Boolean;
var
  I, J: Integer;
  Sym: TSysCharSet;
begin
  Result := False;
  if (Length(St) = 0) or ((Length(St) = 1) and (St[1] = '$')) then
    Exit;
  Sym := DigitSymbols;
  if (St[1] = '-') or (St[1] = '$') then
  begin
    if Length(St) = 1 then
      Exit
    else
      J := 2;
    if St[1] = '$' then
      Sym := HexadecimalSymbols;
  end
  else
    J := 1;
  for I := J to Length(St) do
    if not (St[I] in Sym) then
      Exit;
  Result := True;
end;

function IsIntConstantW(const St: WideString): Boolean;
var
  I, J: Integer;
  Sym: TSysCharSet;
begin
  Result := False;
  if (Length(St) = 0) or ((Length(St) = 1) and (St[1] = '$')) then
    Exit;
  Sym := DigitSymbols;
  if (St[1] = '-') or (St[1] = '$') then
  begin
    if Length(St) = 1 then
      Exit
    else
      J := 2;
    if St[1] = '$' then
      Sym := HexadecimalSymbols;
  end
  else
    J := 1;
  for I := J to Length(St) do
    if not CharInSetW(St[I], Sym) then
      Exit;
  Result := True;
end;

function IsIdentifier(const ID: string): Boolean;
var
  I, L: Integer;
begin
  Result := False;
  L := Length(ID);
  if L = 0 then
    Exit;
  if not (ID[1] in IdentifierFirstSymbols) then
    Exit;
  for I := 1 to L do
  begin
    if not (ID[1] in IdentifierSymbols) then
      Exit;
  end;
  Result := True;
end;

function IsIdentifierW(const ID: WideString): Boolean;
var
  I, L: Integer;
begin
  Result := False;
  L := Length(ID);
  if L = 0 then
    Exit;
  if not CharInSetW(ID[1], IdentifierFirstSymbols) then
    Exit;
  for I := 1 to L do
  begin
    if not CharInSetW(ID[1], IdentifierSymbols) then
      Exit;
  end;
  Result := True;
end;

function GetStringValue(const St: string): string;
begin
  if IsStringConstant(St) then
    Result := Copy(St, 2, Length(St) - 2)
  else
    Result := St;
end;

function GetStringValueW(const St: WideString): WideString;
begin
  if IsStringConstant(St) then
    Result := Copy(St, 2, Length(St) - 2)
  else
    Result := St;
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

