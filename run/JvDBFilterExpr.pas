{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBFilterExpr.pas, released on 2008-12-10.

The Initial Developers of the Original Code is: Andreas Hausladen
Copyright (c) 2008 Andreas Hausladen [Andreas DOTT Hausladen ATT gmx DOTT de]
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBFilterExpr;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Variants, DB, DBCommon;

type
  TJvDBFilterExpression = class(TObject)
  private
    FDataSet: TDataSet;
    FParser: TExprParser;
    FRoot: PExprNode;
    function EvalOpNode(N: PExprNode): Boolean;
    function EvalFuncNode(N: PExprNode): Variant;
    function EvaluateNode(N: PExprNode): Variant;
  public
    constructor Create(ADataSet: TDataSet; const Filter: string; const FilterOptions: TFilterOptions);
    destructor Destroy; override;
    function Evaluate: Boolean;
  end;

implementation

uses
  SqlTimSt, DateUtils, JvResources, JclSysUtils;

var
  FieldTypeMapInitialized: Boolean = False;
  FieldTypeMap: TFieldMap;

type
  TExprParserAccess = class
  protected
    FDecimalSeparator: {$IF CompilerVersion > 17.0}WideChar{$ELSE}Char{$IFEND}; // Delphi 2006+ use WideChar
    FFilter: TFilterExpr;
  end;

  TFilterExprAccess = class
  protected
    FDataSet: TDataSet;
    FFieldMap: TFieldMap;
    FOptions: TFilterOptions;
    FParserOptions: TParserOptions;
    FNodes: PExprNode;
  end;

{------------------------------------------------------------------------------}
function TrimLeftEx(const S, Blanks: string): string;
var
  I, Len: Integer;
begin
  Len := Length(S);
  I := 1;
  while (I <= Len) and (Pos(S[I], Blanks) > 0) do
    Inc(I);
  Result := Copy(S, I, MaxInt);
end;

function TrimRightEx(const S, Blanks: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (Pos(S[I], Blanks) > 0) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function TrimEx(const S, Blanks: string): string;
var
  L, R, Len: Integer;
begin
  Len := Length(S);
  L := 1;
  while (L <= Len) and (Pos(S[L], Blanks) > 0) do
    Inc(L);
  R := Len;
  while (R >= L) and (Pos(S[R], Blanks) > 0) do
    Dec(R);
  Result := Copy(S, L, R - L + 1);
end;

// Derived from "Like" by Michael Winter

function IsLike(const MaskStr, S: string): Boolean;
var
  StringPtr: PChar;
  PatternPtr: PChar;
  StringRes: PChar;
  PatternRes: PChar;
  Index: Integer;
begin
  if MaskStr = '' then
  begin
    Result := False;
    Exit;
  end;

  Result := MaskStr = '%';

  if Result or (S = '') then
    Exit;
  Index := 1;

  StringPtr := PChar(S) + Index - 1;
  PatternPtr := PChar(MaskStr);
  StringRes := nil;
  PatternRes := nil;

  repeat
    repeat
      case PatternPtr^ of
        #0:
          begin
            Result := StringPtr^ = #0;
            if Result or (StringRes = nil) or (PatternRes = nil) then
              Exit;

            StringPtr := StringRes;
            PatternPtr := PatternRes;
            Break;
          end;
        '%':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
            Break;
          end;
        '_':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ <> PatternPtr^ then
            begin
              if (StringRes = nil) or (PatternRes = nil) then
                Exit;
              StringPtr := StringRes;
              PatternPtr := PatternRes;
              Break;
            end
            else
            begin
              Inc(StringPtr);
              Inc(PatternPtr);
            end;
          end;
      end;
    until False;

    repeat
      case PatternPtr^ of
        #0:
          begin
            Result := True;
            Exit;
          end;
        '%':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
          end;
        '_':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            repeat
              if StringPtr^ = #0 then
                Exit;
              if StringPtr^ = PatternPtr^ then
                Break;
              Inc(StringPtr);
            until False;
            Inc(StringPtr);
            StringRes := StringPtr;
            Inc(PatternPtr);
            Break;
          end;
      end;
    until False;
  until False;
end;
{------------------------------------------------------------------------------}

{ TJvDBFilterExpression }

constructor TJvDBFilterExpression.Create(ADataSet: TDataSet; const Filter: string;
  const FilterOptions: TFilterOptions);
var
  FieldType: TFieldType;
  Nodes: PExprNode;

  function NodesContainsLeftRight(Root: PExprNode): Boolean;
  var
    Node: PExprNode;
  begin
    Result := True;
    Node := Nodes;
    while Node <> nil do
    begin
      if (Node.FLeft = Root) or (Node.FRight = Root) then
        Exit;
      Node := Node.FNext;
    end;
    Result := False;
  end;

begin
  inherited Create;
  FDataSet := ADataSet;

  if not FieldTypeMapInitialized then
  begin
    FieldTypeMapInitialized := True;
    for FieldType := Low(FieldType) to High(FieldType) do
      FieldTypeMap[FieldType] := Ord(FieldType);
  end;
  FParser := TExprParser.Create(ADataSet, Filter, [], [poExtSyntax], '', nil, FieldTypeMap);
  Nodes := TFilterExprAccess(TExprParserAccess(FParser).FFilter).FNodes;

  { Find root node because FNodes is the last added node which must not be the root node.
    The root node is the node which istn't referenced by any other node's Left or Right field. }
  if Nodes <> nil then
  begin
    FRoot := Nodes;
    while (FRoot.FNext <> nil) and not ((FRoot.FKind = enOperator) and not NodesContainsLeftRight(FRoot)) do
      FRoot := FRoot.FNext;
  end;
end;

destructor TJvDBFilterExpression.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

function TJvDBFilterExpression.Evaluate: Boolean;
begin
  Result := EvalOpNode(FRoot);
end;

function TJvDBFilterExpression.EvaluateNode(N: PExprNode): Variant;
begin
  if N = nil then
    Result := Unassigned
  else
  begin
    case N.FKind of
      enOperator:
        Result := EvalOpNode(N);
      enConst:
        Result := N.FData;
      enField:
        Result := FDataSet.FieldByName(N.FData).AsVariant;
      enFunc:
        Result := EvalFuncNode(N);
    else
      raise Exception.CreateRes(@RsInvalidFilterNodeKind);
    end;
  end;
end;

function TJvDBFilterExpression.EvalOpNode(N: PExprNode): Boolean;
var
  I: Integer;
  V: Variant;
begin
  if N = nil then
    Result := False
  else
  begin
    Assert(N.FKind = enOperator);
    case N.FOperator of
      coEQ:
        Result := EvaluateNode(N.FLeft) = EvaluateNode(N.FRight);
      coNE:
        Result := EvaluateNode(N.FLeft) <> EvaluateNode(N.FRight);
      coGT:
        Result := EvaluateNode(N.FLeft) > EvaluateNode(N.FRight);
      coLT:
        Result := EvaluateNode(N.FLeft) < EvaluateNode(N.FRight);
      coGE:
        Result := EvaluateNode(N.FLeft) >= EvaluateNode(N.FRight);
      coLE:
        Result := EvaluateNode(N.FLeft) <= EvaluateNode(N.FRight);
      coNOT:
        Result := not EvaluateNode(N.FLeft);
      coAND:
        Result := LongBool(EvaluateNode(N.FLeft)) and LongBool(EvaluateNode(N.FRight));
      coOR:
        Result := LongBool(EvaluateNode(N.FLeft)) or LongBool(EvaluateNode(N.FRight));
      coISBLANK:
        Result := VarIsNull(EvaluateNode(N.FLeft));
      coNOTBLANK:
        Result := not VarIsNull(EvaluateNode(N.FLeft));
      coLIKE:
        Result := IsLike(EvaluateNode(N.FLeft), EvaluateNode(N.FRight));
      coIN:
        begin
          Result := False;
          V := EvaluateNode(N.FLeft);
          if N.FArgs <> nil then
          begin
            for I := 0 to N.FArgs.Count - 1 do
            begin
              if V = EvaluateNode(N.FArgs[I]) then
              begin
                Result := True;
                Break;
              end;
            end;
          end;
        end;
      coMINUS:
        Result := -EvaluateNode(N.FLeft);
      coADD:
        Result := EvaluateNode(N.FLeft) + EvaluateNode(N.FRight);
      coSUB:
        Result := EvaluateNode(N.FLeft) - EvaluateNode(N.FRight);
      coMUL:
        Result := EvaluateNode(N.FLeft) * EvaluateNode(N.FRight);
      coDIV:
        Result := EvaluateNode(N.FLeft) / EvaluateNode(N.FRight);
    else
      raise Exception.CreateRes(@RsUnknownFilterOperation);
    end;
  end;
end;

function TJvDBFilterExpression.EvalFuncNode(N: PExprNode): Variant;
var
  V: Variant;
begin
  if N = nil then
    Result := Unassigned
  else
  begin
    if (N.FArgs <> nil) and (N.FArgs.Count > 0) then
    begin
      V := EvaluateNode(N.FArgs[0]);

      if CompareText(N.FData, 'UPPER') = 0 then
        Result := AnsiUpperCase(V)
      else
      if CompareText(N.FData, 'LOWER') = 0 then
        Result := AnsiLowerCase(V)
      else
      if CompareText(N.FData, 'TRIM') = 0 then
      begin
        if N.FArgs.Count = 1 then
          Result := Trim(V)
        else
          Result := TrimEx(V, EvaluateNode(N.FArgs[1]));
      end
      else
      if CompareText(N.FData, 'TRIMLEFT') = 0 then
      begin
        if N.FArgs.Count = 1 then
          Result := TrimLeft(V)
        else
          Result := TrimLeftEx(V, EvaluateNode(N.FArgs[1]));
      end
      else
      if CompareText(N.FData, 'TRIMRIGHT') = 0 then
      begin
        if N.FArgs.Count = 1 then
          Result := TrimRight(V)
        else
          Result := TrimRightEx(V, EvaluateNode(N.FArgs[1]));
      end
      else
      if CompareText(N.FData, 'SUBSTRING') = 0 then
      begin
        if N.FArgs.Count = 2 then
          Result := Copy(V, Integer(EvaluateNode(N.FArgs[1])), MaxInt)
        else
          Result := Copy(V, Integer(EvaluateNode(N.FArgs[1])), Integer(EvaluateNode(N.FArgs[2])));
      end
      else
      if CompareText(N.FData, 'YEAR') = 0 then
      begin
        if VarIsNullEmpty(V) then Result := -1 else
          Result := YearOf(V);
      end
      else
      if CompareText(N.FData, 'MONTH') = 0 then
      begin
        if VarIsNullEmpty(V) then Result := -1 else
          Result := MonthOf(V);
      end
      else
      if CompareText(N.FData, 'DAY') = 0 then
      begin
        if VarIsNullEmpty(V) then Result := -1 else
          Result := DayOf(V);
      end
      else
      if CompareText(N.FData, 'HOUR') = 0 then
      begin
        if VarIsNullEmpty(V) then Result := -1 else
          Result := HourOf(V);
      end
      else
      if CompareText(N.FData, 'MINUTE') = 0 then
      begin
        if VarIsNullEmpty(V) then Result := -1 else
          Result := MinuteOf(V);
      end
      else
      if CompareText(N.FData, 'SECOND') = 0 then
      begin
        if VarIsNullEmpty(V) then Result := -1 else
          Result := SecondOf(V);
      end
      else
      if CompareText(N.FData, 'TIME') = 0 then
      begin
        if VarIsNullEmpty(V) then Result := NULL else
          Result := VarSQLTimeStampCreate(TimeOf(V));
      end
      else
      if CompareText(N.FData, 'DATE') = 0 then
      begin
        if VarIsNullEmpty(V) then Result := NULL else
          Result := VarSQLTimeStampCreate(DateOf(V));
      end
      else
        raise Exception.CreateResFmt(@RsUnknownFilterFunction, [N.FData]);
    end
    else
      raise Exception.CreateResFmt(@RsMissingFilterFunctionParameters, [N.FData]);
  end;
end;

end.
