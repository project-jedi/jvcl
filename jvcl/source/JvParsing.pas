{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvParsing.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvParsing;

interface

uses
  SysUtils, Classes,
  JvTypes;

type
  TParserFunc = (pfArcTan, pfCos, pfSin, pfTan, pfAbs, pfExp, pfLn, pfLog,
    pfSqrt, pfSqr, pfInt, pfFrac, pfTrunc, pfRound, pfArcSin, pfArcCos,
    pfSign, pfNot);
  EJvParserError = class(EJVCLException);
  {$IFDEF WIN32}
  TUserFunction = function(Value: Extended): Extended;
  {$ELSE}
  TUserFunction = Pointer;
  {$ENDIF}

  TJvMathParser = class(TObject)
  private
    FCurPos: Cardinal;
    FParseText: string;
    function GetChar: Char;
    procedure NextChar;
    function GetNumber(var AValue: Extended): Boolean;
    function GetConst(var AValue: Extended): Boolean;
    function GetFunction(var AValue: TParserFunc): Boolean;
    function GetUserFunction(var Index: Integer): Boolean;
    function Term: Extended;
    function SubTerm: Extended;
    function Calculate: Extended;
  public
    // (rom) renamed from Exec
    function Execute(const AFormula: string): Extended;
    class procedure RegisterUserFunction(const Name: string; Proc: TUserFunction);
    class procedure UnregisterUserFunction(const Name: string);
  end;

function GetFormulaValue(const Formula: string): Extended;

{$IFNDEF WIN32}
function Power(Base, Exponent: Extended): Extended;
{$ENDIF}

implementation

uses
  JvxRConst;

const
  cSpecialChars = [#0..' ', '+', '-', '/', '*', ')', '^'];
  cIdentifierChars = ['A'..'Z', 'a'..'z', '_'];

  FuncNames: array [TParserFunc] of PChar =
    ('ARCTAN', 'COS', 'SIN', 'TAN', 'ABS', 'EXP', 'LN', 'LOG',
     'SQRT', 'SQR', 'INT', 'FRAC', 'TRUNC', 'ROUND', 'ARCSIN', 'ARCCOS',
     'SIGN', 'NOT');

{ Parser errors }

procedure InvalidCondition(const Str: string);
begin
  raise EJvParserError.Create(Str);
end;

{ IntPower and Power functions are copied from Borland's MATH.PAS unit }

{$IFDEF WIN32}
function IntPower(Base: Extended; Exponent: Integer): Extended;
asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X := Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
@@3:
        fwait
end;
{$ELSE}
function IntPower(Base: Extended; Exponent: Integer): Extended;
var
  Y: Longint;
begin
  Y := Abs(Exponent);
  Result := 1.0;
  while Y > 0 do
  begin
    while not Odd(Y) do
    begin
      Y := Y shr 1;
      Base := Base * Base;
    end;
    Dec(Y);
    Result := Result * Base;
  end;
  if Exponent < 0 then
    Result := 1.0 / Result;
end;
{$ENDIF WIN32}

function Power(Base, Exponent: Extended): Extended;
begin
  if Exponent = 0.0 then
    Result := 1.0
  else
  if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0
  else
  if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= MaxInt) then
    Result := IntPower(Base, Trunc(Exponent))
  else
    Result := Exp(Exponent * Ln(Base))
end;

{ User defined functions }

type
  {$IFDEF WIN32}
  TFarUserFunction = TUserFunction;
  {$ELSE}
  TFarUserFunction = function(Value: Extended): Extended;
  {$ENDIF}

var
  UserFuncList: TStrings;

function GetUserFuncList: TStrings;
begin
  if not Assigned(UserFuncList) then
  begin
    UserFuncList := TStringList.Create;
    with TStringList(UserFuncList) do
    begin
      Sorted := True;
      Duplicates := dupIgnore;
    end;
  end;
  Result := UserFuncList;
end;

procedure FreeUserFunc; far;
begin
  UserFuncList.Free;
  UserFuncList := nil;
end;

{ Parsing routines }

function GetFormulaValue(const Formula: string): Extended;
begin
  with TJvMathParser.Create do
  try
    Result := Execute(Formula);
  finally
    Free;
  end;
end;

function TJvMathParser.GetChar: Char;
begin
  Result := FParseText[FCurPos];
end;

procedure TJvMathParser.NextChar;
begin
  Inc(FCurPos);
end;

function TJvMathParser.GetNumber(var AValue: Extended): Boolean;
var
  C: Char;
  SavePos: Cardinal;
  Code: Integer;
  IsHex: Boolean;
  TmpStr: string;
begin
  Result := False;
  C := GetChar;
  SavePos := FCurPos;
  TmpStr := '';
  IsHex := False;
  if C = '$' then
  begin
    TmpStr := C;
    NextChar;
    C := GetChar;
    while C in ['0'..'9', 'A'..'F', 'a'..'f'] do
    begin
      TmpStr := TmpStr + C;
      NextChar;
      C := GetChar;
    end;
    IsHex := True;
    Result := (Length(TmpStr) > 1) and (Length(TmpStr) <= 9);
  end
  else
  if C in ['+', '-', '0'..'9', '.', DecimalSeparator] then
  begin
    if (C in ['.', DecimalSeparator]) then
      TmpStr := '0' + '.'
    else
      TmpStr := C;
    NextChar;
    C := GetChar;
    if (Length(TmpStr) = 1) and (TmpStr[1] in ['+', '-']) and
      (C in ['.', DecimalSeparator]) then
      TmpStr := TmpStr + '0';
    while C in ['0'..'9', '.', 'E', 'e', DecimalSeparator] do
    begin
      if C = DecimalSeparator then
        TmpStr := TmpStr + '.'
      else
        TmpStr := TmpStr + C;
      if (C = 'E') then
      begin
        if (Length(TmpStr) > 1) and (TmpStr[Length(TmpStr) - 1] = '.') then
          Insert('0', TmpStr, Length(TmpStr));
        NextChar;
        C := GetChar;
        if (C in ['+', '-']) then
        begin
          TmpStr := TmpStr + C;
          NextChar;
        end;
      end
      else
        NextChar;
      C := GetChar;
    end;
    if (TmpStr[Length(TmpStr)] = '.') and (Pos('E', TmpStr) = 0) then
      TmpStr := TmpStr + '0';
    Val(TmpStr, AValue, Code);
    Result := (Code = 0);
  end;
  Result := Result and (FParseText[FCurPos] in cSpecialChars);
  if Result then
  begin
    if IsHex then
      AValue := StrToInt(TmpStr);
    { else AValue := StrToFloat(TmpStr) };
  end
  else
  begin
    AValue := 0;
    FCurPos := SavePos;
  end;
end;

function TJvMathParser.GetConst(var AValue: Extended): Boolean;
begin
  Result := False;
  case FParseText[FCurPos] of
    'E':
      if FParseText[FCurPos + 1] in cSpecialChars then
      begin
        AValue := Exp(1);
        Inc(FCurPos);
        Result := True;
      end;
    'P':
      if (FParseText[FCurPos + 1] = 'I') and
        (FParseText[FCurPos + 2] in cSpecialChars) then
      begin
        AValue := Pi;
        Inc(FCurPos, 2);
        Result := True;
      end;
  end
end;

function TJvMathParser.GetUserFunction(var Index: Integer): Boolean;
var
  TmpStr: string;
  I: Integer;
begin
  Result := False;
  if (FParseText[FCurPos] in cIdentifierChars) and
    Assigned(UserFuncList) then
  begin
    with UserFuncList do
      for I := 0 to Count - 1 do
      begin
        TmpStr := Copy(FParseText, FCurPos, Length(Strings[I]));
        if (CompareText(TmpStr, Strings[I]) = 0) and
          (Objects[I] <> nil) then
        begin
          if FParseText[FCurPos + Cardinal(Length(TmpStr))] = '(' then
          begin
            Result := True;
            Inc(FCurPos, Length(TmpStr));
            Index := I;
            Exit;
          end;
        end;
      end;
  end;
  Index := -1;
end;

function TJvMathParser.GetFunction(var AValue: TParserFunc): Boolean;
var
  I: TParserFunc;
  TmpStr: string;
begin
  Result := False;
  AValue := Low(TParserFunc);
  if FParseText[FCurPos] in cIdentifierChars then
  begin
    for I := Low(TParserFunc) to High(TParserFunc) do
    begin
      TmpStr := Copy(FParseText, FCurPos, StrLen(FuncNames[I]));
      if CompareText(TmpStr, StrPas(FuncNames[I])) = 0 then
      begin
        AValue := I;
        if FParseText[FCurPos + Cardinal(Length(TmpStr))] = '(' then
        begin
          Result := True;
          Inc(FCurPos, Length(TmpStr));
          Break;
        end;
      end;
    end;
  end;
end;

function TJvMathParser.Term: Extended;
var
  Value: Extended;
  NoFunc: TParserFunc;
  UserFunc: Integer;
  Func: Pointer;
begin
  if FParseText[FCurPos] = '(' then
  begin
    Inc(FCurPos);
    Value := Calculate;
    if FParseText[FCurPos] <> ')' then
      InvalidCondition(SParseNotCramp);
    Inc(FCurPos);
  end
  else
  begin
    if not GetNumber(Value) then
      if not GetConst(Value) then
        if GetUserFunction(UserFunc) then
        begin
          Inc(FCurPos);
          Func := UserFuncList.Objects[UserFunc];
          Value := TFarUserFunction(Func)(Calculate);
          if FParseText[FCurPos] <> ')' then
            InvalidCondition(SParseNotCramp);
          Inc(FCurPos);
        end
        else
        if GetFunction(NoFunc) then
        begin
          Inc(FCurPos);
          Value := Calculate;
          try
            case NoFunc of
              pfArcTan:
                Value := ArcTan(Value);
              pfCos:
                Value := Cos(Value);
              pfSin:
                Value := Sin(Value);
              pfTan:
                if Cos(Value) = 0 then
                  InvalidCondition(SParseDivideByZero)
                else
                  Value := Sin(Value) / Cos(Value);
              pfAbs:
                Value := Abs(Value);
              pfExp:
                Value := Exp(Value);
              pfLn:
                if Value <= 0 then
                  InvalidCondition(SParseLogError)
                else
                  Value := Ln(Value);
              pfLog:
                if Value <= 0 then
                  InvalidCondition(SParseLogError)
                else
                  Value := Ln(Value) / Ln(10);
              pfSqrt:
                if Value < 0 then
                  InvalidCondition(SParseSqrError)
                else
                  Value := Sqrt(Value);
              pfSqr:
                Value := Sqr(Value);
              pfInt:
                Value := Round(Value);
              pfFrac:
                Value := Frac(Value);
              pfTrunc:
                Value := Trunc(Value);
              pfRound:
                Value := Round(Value);
              pfArcSin:
                if Value = 1 then
                  Value := Pi / 2
                else
                  Value := ArcTan(Value / Sqrt(1 - Sqr(Value)));
              pfArcCos:
                if Value = 1 then
                  Value := 0
                else
                  Value := Pi / 2 - ArcTan(Value / Sqrt(1 - Sqr(Value)));
              pfSign:
                if Value > 0 then
                  Value := 1
                else
                if Value < 0 then
                  Value := -1;
              pfNot:
                Value := not Trunc(Value);
            end;
          except
            on E: EJvParserError do
              raise
          else
            InvalidCondition(SParseInvalidFloatOperation);
          end;
          if FParseText[FCurPos] <> ')' then
            InvalidCondition(SParseNotCramp);
          Inc(FCurPos);
        end
        else
          InvalidCondition(SParseSyntaxError);
  end;
  Result := Value;
end;

function TJvMathParser.SubTerm: Extended;
var
  Value: Extended;
begin
  Value := Term;
  while FParseText[FCurPos] in ['*', '^', '/'] do
  begin
    Inc(FCurPos);
    if FParseText[FCurPos - 1] = '*' then
      Value := Value * Term
    else
    if FParseText[FCurPos - 1] = '^' then
      Value := Power(Value, Term)
    else
    if FParseText[FCurPos - 1] = '/' then
    try
      Value := Value / Term;
    except
      InvalidCondition(SParseDivideByZero);
    end;
  end;
  Result := Value;
end;

function TJvMathParser.Calculate: Extended;
var
  Value: Extended;
begin
  Value := SubTerm;
  while FParseText[FCurPos] in ['+', '-'] do
  begin
    Inc(FCurPos);
    if FParseText[FCurPos - 1] = '+' then
      Value := Value + SubTerm
    else
      Value := Value - SubTerm;
  end;
  if not (FParseText[FCurPos] in [#0, ')', '>', '<', '=', ',']) then
    InvalidCondition(SParseSyntaxError);
  Result := Value;
end;

function TJvMathParser.Execute(const AFormula: string): Extended;
var
  I, J: Integer;
begin
  J := 0;
  Result := 0;
  FParseText := '';
  for I := 1 to Length(AFormula) do
  begin
    case AFormula[I] of
      '(':
        Inc(J);
      ')':
        Dec(J);
    end;
    if AFormula[I] > ' ' then
      FParseText := FParseText + UpCase(AFormula[I]);
  end;
  if J = 0 then
  begin
    FCurPos := 1;
    FParseText := FParseText + #0;
    if (FParseText[1] in ['-', '+']) then
      FParseText := '0' + FParseText;
    Result := Calculate;
  end
  else
    InvalidCondition(SParseNotCramp);
end;

class procedure TJvMathParser.RegisterUserFunction(const Name: string;
  Proc: TUserFunction);
var
  I: Integer;
begin
  if (Length(Name) > 0) and (Name[1] in cIdentifierChars) then
  begin
    if not Assigned(Proc) then
      UnregisterUserFunction(Name)
    else
    begin
      with GetUserFuncList do
      begin
        I := IndexOf(Name);
        if I < 0 then
          I := Add(Name);
        {$IFDEF WIN32}
        Objects[I] := @Proc;
        {$ELSE}
        Objects[I] := Proc;
        {$ENDIF}
      end;
    end;
  end
  else
    InvalidCondition(SParseSyntaxError);
end;

class procedure TJvMathParser.UnregisterUserFunction(const Name: string);
var
  I: Integer;
begin
  if Assigned(UserFuncList) then
    with UserFuncList do
    begin
      I := IndexOf(Name);
      if I >= 0 then
        Delete(I);
      if Count = 0 then
        FreeUserFunc;
    end;
end;

initialization
  UserFuncList := nil;
{$IFDEF WIN32}
finalization
  FreeUserFunc;
{$ELSE}
  AddExitProc(FreeUserFunc);
{$ENDIF}

end.

