{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_System.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):  Peter Fischer-Haase <pfischer att ise-online dott de> commented as "pfh"

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : JVCL Interpreter version 2

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_System;

{$I jvcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  JvInterpreter, SysUtils;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  JvTypes, JvResources;

{ TObject }

{ function ClassType: TClass; }

procedure TObject_ClassType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := C2V(TObject(Args.Obj).ClassType);
end;

{ function ClassName: ShortString; }

procedure TObject_ClassName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObject(Args.Obj).ClassName;
end;

{ function ClassNameIs(const Name: string): Boolean; }

procedure TObject_ClassNameIs(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObject(Args.Obj).ClassNameIs(Args.Values[0]);
end;

{ function ClassParent: TClass; }

procedure TObject_ClassParent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := C2V(TObject(Args.Obj).ClassParent);
end;

{ function ClassInfo: Pointer; }

procedure TObject_ClassInfo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TObject(Args.Obj).ClassInfo);
end;

{ function InstanceSize: Longint; }

procedure TObject_InstanceSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObject(Args.Obj).InstanceSize;
end;

{ function InheritsFrom(AClass: TClass): Boolean; }

procedure TObject_InheritsFrom(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObject(Args.Obj).InheritsFrom(V2C(Args.Values[0]));
end;

(*
{ function GetInterface(const IID: TGUID; out Obj): Boolean; }
procedure TObject_GetInterface(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObject(Args.Obj).GetInterface(Args.Values[0], Args.Values[1], Args.Values[2]);
end;
*)

{ TInterfacedObject }

{ property Read RefCount: Integer }

procedure TInterfacedObject_Read_RefCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TInterfacedObject(Args.Obj).RefCount;
end;

{ procedure Move(const Source; var Dest; Count: Integer); }

procedure JvInterpreter_Move(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Move(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ParamCount: Integer; }

procedure JvInterpreter_ParamCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ParamCount;
end;

{ function ParamStr(Index: Integer): string; }

procedure JvInterpreter_ParamStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ParamStr(Args.Values[0]);
end;

{ procedure Randomize; }

procedure JvInterpreter_Randomize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Randomize;
end;

procedure JvInterpreter_Random(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Random(Integer(Args.Values[0]));
end;

{ function UpCase(Ch: Char): Char; }

procedure JvInterpreter_UpCase(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := UpCase(string(Args.Values[0])[1]);
end;

(*
{ function WideCharToString(Source: PWideChar): string; }
procedure JvInterpreter_WideCharToString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := WideCharToString(Args.Values[0]);
end;

{ function WideCharLenToString(Source: PWideChar; SourceLen: Integer): string; }
procedure JvInterpreter_WideCharLenToString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := WideCharLenToString(Args.Values[0], Args.Values[1]);
end;

{ procedure WideCharToStrVar(Source: PWideChar; var Dest: string); }
procedure JvInterpreter_WideCharToStrVar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  WideCharToStrVar(Args.Values[0], string(TVarData(Args.Values[1]).vString));
end;

{ procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer; var Dest: string); }
procedure JvInterpreter_WideCharLenToStrVar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  WideCharLenToStrVar(Args.Values[0], Args.Values[1], string(TVarData(Args.Values[2]).vString));
end;

{ function StringToWideChar(const Source: string; Dest: PWideChar; DestSize: Integer): PWideChar; }
procedure JvInterpreter_StringToWideChar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := StringToWideChar(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function OleStrToString(Source: PWideChar): string; }
procedure JvInterpreter_OleStrToString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := OleStrToString(Args.Values[0]);
end;

{ procedure OleStrToStrVar(Source: PWideChar; var Dest: string); }
procedure JvInterpreter_OleStrToStrVar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  OleStrToStrVar(Args.Values[0], string(TVarData(Args.Values[1]).vString));
end;

{ function StringToOleStr(const Source: string): PWideChar; }
procedure JvInterpreter_StringToOleStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := StringToOleStr(Args.Values[0]);
end;
*)

{ function VarType(const V: Variant): Integer; }

procedure JvInterpreter_VarType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarType(Args.Values[0]);
end;

{ function VarAsType(const V: Variant; VarType: Integer): Variant; }

procedure JvInterpreter_VarAsType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarAsType(Args.Values[0], Args.Values[1]);
end;

{ function VarIsEmpty(const V: Variant): Boolean; }

procedure JvInterpreter_VarIsEmpty(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarIsEmpty(Args.Values[0]);
end;

{ function VarIsNull(const V: Variant): Boolean; }

procedure JvInterpreter_VarIsNull(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarIsNull(Args.Values[0]);
end;

{ function VarToStr(const V: Variant): string; }

procedure JvInterpreter_VarToStr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarToStr(Args.Values[0]);
end;

{ function VarFromDateTime(DateTime: TDateTime): Variant; }

procedure JvInterpreter_VarFromDateTime(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarFromDateTime(Args.Values[0]);
end;

{ function VarToDateTime(const V: Variant): TDateTime; }

procedure JvInterpreter_VarToDateTime(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarToDateTime(Args.Values[0]);
end;

{ function VarArrayCreate(const Bounds: array of Integer; VarType: Integer): Variant; }

procedure JvInterpreter_VarArrayCreate(var Value: Variant; Args: TJvInterpreterArgs);
var
  OA: TOpenArray;
  OAV: TValueArray;
  OAS: Integer;
  I: Integer;
  AI: array of Integer;
begin
  V2OA(Args.Values[0], OA, OAV, OAS);
  if Odd(OAS) then
    raise EJVCLException.CreateRes(@RsESizeMustBeEven);
  SetLength(AI, OAS);
  for I := 0 to OAS -1 do
    AI[I] := OAV[I];
  Value := VarArrayCreate(AI, Args.Values[1]);
end;

{function VarArrayOf(const Values: array of Variant): Variant; }
procedure JvInterpreter_VarArrayOf(var Value: Variant; Args: TJvInterpreterArgs);
var
  OA: TOpenArray;
  OAV: TValueArray;
  OAS: Integer;
  I: Integer;
  AV: array of Variant;
begin
  V2OA(Args.Values[0], OA, OAV, OAS);
  SetLength(AV, OAS);
  for I := 0 to OAS -1 do
    AV[I] := OAV[I];
  Value := VarArrayOf(AV);
end;

{ function VarArrayDimCount(const A: Variant): Integer; }
procedure JvInterpreter_VarArrayDimCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarArrayDimCount(Args.Values[0]);
end;

{ function VarArrayLowBound(const A: Variant; Dim: Integer): Integer; }
procedure JvInterpreter_VarArrayLowBound(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarArrayLowBound(Args.Values[0], Args.Values[1]);
end;

{ function VarArrayHighBound(const A: Variant; Dim: Integer): Integer; }
procedure JvInterpreter_VarArrayHighBound(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarArrayHighBound(Args.Values[0], Args.Values[1]);
end;

(*{ function VarArrayLock(const A: Variant): Pointer; }
procedure JvInterpreter_VarArrayLock(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(VarArrayLock(Args.Values[0]));
end;

{ procedure VarArrayUnlock(const A: Variant); }
procedure JvInterpreter_VarArrayUnlock(var Value: Variant; Args: TJvInterpreterArgs);
begin
  VarArrayUnlock(Args.Values[0]);
end;

{ function VarArrayRef(const A: Variant): Variant; }
procedure JvInterpreter_VarArrayRef(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarArrayRef(Args.Values[0]);
end;*)

{ function VarIsArray(const A: Variant): Boolean; }
procedure JvInterpreter_VarIsArray(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := VarIsArray(Args.Values[0]);
end;

{ function Ord(const A: Variant): Integer; }

procedure JvInterpreter_Ord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  if VarType(Args.Values[0]) = varString then
    Value := Ord(VarToStr(Args.Values[0])[1])
  else
    Value := Integer(Args.Values[0]);
end;

{ function Chr(X: Byte): Char }

procedure JvInterpreter_Chr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Chr(Byte(Args.Values[0]));
end;

{ function Abs(X); }

procedure JvInterpreter_Abs(var Value: Variant; Args: TJvInterpreterArgs);
begin
  if VarType(Args.Values[0]) = varInteger then
    Value := Abs(Integer(Args.Values[0]))
  else
    Value := Abs(Extended(Args.Values[0]));
end;

{ function Length(S): Integer; }

procedure JvInterpreter_Length(var Value: Variant; Args: TJvInterpreterArgs);
begin
  if VarIsArray(Args.Values[0]) then
  begin
    if VarArrayDimCount(Args.Values[0]) > 1 then
      raise EJVCLException.CreateRes(@RsESorryForOneDimensionalArraysOnly);
    Value := VarArrayHighBound(Args.Values[0], 1)-VarArrayLowBound(Args.Values[0], 1);
  end
  else
  if TVarData(Args.Values[0]).vType = varArray then
    Value := JvInterpreterArrayLength(Args.Values[0])
  else
    Value := Length(Args.Values[0]);
end;

{ function Copy(S; Index, Count: Integer): String; }

procedure JvInterpreter_Copy(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Copy(Args.Values[0], Integer(Args.Values[1]), Integer(Args.Values[2]));
end;

{ function Round(Value: Extended): Int64; }

procedure JvInterpreter_Round(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(Round(Args.Values[0]));
end;

{ function Trunc(Value: Extended): Int64; }

procedure JvInterpreter_Trunc(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(Trunc(Args.Values[0]));
end;

{ function Pos(Substr: string; S: string): Integer; }

procedure JvInterpreter_Pos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Pos(string(Args.Values[0]), string(Args.Values[1]));
end;

//+++pfh
{procedure Delete(var S: string; Index, Count: Integer);}

procedure JvInterpreter_Delete(var Value: Variant; Args: TJvInterpreterArgs);
var
  S: string;
begin
  S := Args.Values[0];
  Delete(S, Integer(Args.Values[1]), Integer(Args.Values[2]));
  Args.Values[0] := S;
  Value := S;
end;

{procedure Insert(Source: string; var S: string; Index: Integer);}

procedure JvInterpreter_Insert(var Value: Variant; Args: TJvInterpreterArgs);
var
  S: string;
begin
  S := Args.Values[1];
  Insert(string(Args.Values[0]), S, Integer(Args.Values[2]));
  Args.Values[1] := S;
  Value := S;
end;

{ function Sqr(X: Extended): Extended; }

procedure JvInterpreter_Sqr(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Sqr(Args.Values[0]);
end;

{ function Sqrt(X: Extended): Extended; }

procedure JvInterpreter_Sqrt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Sqrt(Args.Values[0]);
end;

{ function Exp(X: Extended): Extended; }

procedure JvInterpreter_Exp(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Exp(Args.Values[0]);
end;

{ function Ln(X: Extended): Extended; }

procedure JvInterpreter_Ln(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Ln(Args.Values[0]);
end;

{ function Sin(X: Extended): Extended; }

procedure JvInterpreter_Sin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Sin(Args.Values[0]);
end;

{ function Cos(X: Extended): Extended; }

procedure JvInterpreter_Cos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Cos(Args.Values[0]);
end;

{ function Tan(X: Extended): Extended; }

procedure JvInterpreter_Tan(var Value: Variant; Args: TJvInterpreterArgs);
begin
//(p3) Tan() is defined in Math.pas which isn't available in all Delphi SKU's
//  Tan(X) = Sin(X)/ Cos(X)
  Value := Sin(Args.Values[0]) / Cos(Args.Values[0]);
end;

{ function ArcTan(X: Extended): Extended; }

procedure JvInterpreter_ArcTan(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ArcTan(Args.Values[0]);
end;
//---pfh

{ procedure SetLength(var s: ShortString; newLength: Integer); }

procedure JvInterpreter_SetLength(var Value: Variant; Args: TJvInterpreterArgs);
begin
  if TVarData(Args.Values[0]).vType <> varArray then
    SetLength(string(TVarData(Args.Values[0]).vString), Integer(Args.Values[1]))
  else
    JvInterpreterArraySetLength(Args.Values[0], Integer(Args.Values[1]));
end;

{procedure High(var Value: Variant; Args: TJvInterpreterArgs);}

procedure JvInterpreter_High(var Value: Variant; Args: TJvInterpreterArgs);
begin
  if VarIsArray(Args.Values[0]) then
  begin
    if VarArrayDimCount(Args.Values[0]) > 1 then
      raise EJVCLException.CreateRes(@RsESorryForOneDimensionalArraysOnly);
    Value := VarArrayLowBound(Args.Values[0], 1);
  end
  else
    Value := JvInterpreterArrayHigh(Args.Values[0]);
end;

{procedure Low(var Value: Variant; Args: TJvInterpreterArgs);}

procedure JvInterpreter_Low(var Value: Variant; Args: TJvInterpreterArgs);
begin
  if VarIsArray(Args.Values[0]) then
  begin
    if VarArrayDimCount(Args.Values[0]) > 1 then
      raise EJVCLException.CreateRes(@RsESorryForOneDimensionalArraysOnly);
    Value := VarArrayLowBound(Args.Values[0], 1);
  end
  else
    Value := JvInterpreterArrayLow(Args.Values[0]);
end;

{procedure DeleteFromArray(var Value: Variant; Args: TJvInterpreterArgs);}

procedure JvInterpreter_DeleteFromArray(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterArrayElementDelete(Args.Values[0], Integer(Args.Values[1]));
end;

{procedure InsertIntoArray(var Value: Variant; Args: TJvInterpreterArgs);}

procedure JvInterpreter_InsertIntoArray(var Value: Variant; Args: TJvInterpreterArgs);
begin
  JvInterpreterArrayElementInsert(Args.Values[0], Integer(Args.Values[1]), Args.Values[2]);
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cSystem = 'System';
begin
  with JvInterpreterAdapter do
  begin
    { TObject }
    AddClass(cSystem, TObject, 'TObject');
    AddGet(TObject, 'ClassType', TObject_ClassType, 0, [varEmpty], varEmpty);
    AddGet(TObject, 'ClassName', TObject_ClassName, 0, [varEmpty], varEmpty);
    AddGet(TObject, 'ClassNameIs', TObject_ClassNameIs, 1, [varEmpty], varEmpty);
    AddGet(TObject, 'ClassParent', TObject_ClassParent, 0, [varEmpty], varEmpty);
    AddGet(TObject, 'ClassInfo', TObject_ClassInfo, 0, [varEmpty], varEmpty);
    AddGet(TObject, 'InstanceSize', TObject_InstanceSize, 0, [varEmpty], varEmpty);
    AddGet(TObject, 'InheritsFrom', TObject_InheritsFrom, 1, [varEmpty], varEmpty);
    // AddGet(TObject, 'GetInterface', TObject_GetInterface, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    { TInterfacedObject }
    AddClass(cSystem, TInterfacedObject, 'TInterfacedObject');
    AddGet(TInterfacedObject, 'RefCount', TInterfacedObject_Read_RefCount, 0, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Move', JvInterpreter_Move, 3, [varEmpty, varByRef, varEmpty], varEmpty);
    AddFunction(cSystem, 'ParamCount', JvInterpreter_ParamCount, 0, [varEmpty], varEmpty);
    AddFunction(cSystem, 'ParamStr', JvInterpreter_ParamStr, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Randomize', JvInterpreter_Randomize, 0, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Random', JvInterpreter_Random, 1, [varInteger], varEmpty);
    AddFunction(cSystem, 'UpCase', JvInterpreter_UpCase, 1, [varEmpty], varEmpty);
    { AddFunction(cSystem, 'WideCharToString', JvInterpreter_WideCharToString, 1, [varEmpty], varEmpty);
      AddFunction(cSystem, 'WideCharLenToString', JvInterpreter_WideCharLenToString, 2, [varEmpty, varEmpty], varEmpty);
      AddFunction(cSystem, 'WideCharToStrVar', JvInterpreter_WideCharToStrVar, 2, [varEmpty, varByRef], varEmpty);
      AddFunction(cSystem, 'WideCharLenToStrVar', JvInterpreter_WideCharLenToStrVar, 3, [varEmpty, varEmpty, varByRef], varEmpty);
      AddFunction(cSystem, 'StringToWideChar', JvInterpreter_StringToWideChar, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
      AddFunction(cSystem, 'OleStrToString', JvInterpreter_OleStrToString, 1, [varEmpty], varEmpty);
      AddFunction(cSystem, 'OleStrToStrVar', JvInterpreter_OleStrToStrVar, 2, [varEmpty, varByRef], varEmpty);
      AddFunction(cSystem, 'StringToOleStr', JvInterpreter_StringToOleStr, 1, [varEmpty], varEmpty); }
    AddFunction(cSystem, 'VarType', JvInterpreter_VarType, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarAsType', JvInterpreter_VarAsType, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cSystem, 'VarIsEmpty', JvInterpreter_VarIsEmpty, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarIsNull', JvInterpreter_VarIsNull, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarToStr', JvInterpreter_VarToStr, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarFromDateTime', JvInterpreter_VarFromDateTime, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarToDateTime', JvInterpreter_VarToDateTime, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarArrayCreate', JvInterpreter_VarArrayCreate, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cSystem, 'VarArrayOf', JvInterpreter_VarArrayOf, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarArrayDimCount', JvInterpreter_VarArrayDimCount, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarArrayLowBound', JvInterpreter_VarArrayLowBound, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cSystem, 'VarArrayHighBound', JvInterpreter_VarArrayHighBound, 2, [varEmpty, varEmpty], varEmpty);
    {AddFunction(cSystem, 'VarArrayLock', JvInterpreter_VarArrayLock, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarArrayUnlock', JvInterpreter_VarArrayUnlock, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'VarArrayRef', JvInterpreter_VarArrayRef, 1, [varEmpty], varEmpty);}
    AddFunction(cSystem, 'VarIsArray', JvInterpreter_VarIsArray, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'ord', JvInterpreter_Ord, 1, [varEmpty], varEmpty);

    AddFunction(cSystem, 'Chr', JvInterpreter_Chr, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Abs', JvInterpreter_Abs, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Length', JvInterpreter_Length, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Copy', JvInterpreter_Copy, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFunction(cSystem, 'Round', JvInterpreter_Round, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Trunc', JvInterpreter_Trunc, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Pos', JvInterpreter_Pos, 2, [varEmpty, varEmpty], varEmpty);

    //+++pfh
    // some string functions
    AddFunction(cSystem, 'Delete', JvInterpreter_Delete, 3, [varByRef, varEmpty, varEmpty], varEmpty);
    AddFunction(cSystem, 'Insert', JvInterpreter_Insert, 3, [varEmpty, varByRef, varEmpty], varEmpty);
    // some math functions
    AddFunction(cSystem, 'Sqr', JvInterpreter_Sqr, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Sqrt', JvInterpreter_Sqrt, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Exp', JvInterpreter_Exp, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Ln', JvInterpreter_Ln, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Sin', JvInterpreter_Sin, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Cos', JvInterpreter_Cos, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Tan', JvInterpreter_Tan, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'ArcTan', JvInterpreter_ArcTan, 1, [varEmpty], varEmpty);
    //---pfh
    AddFunction(cSystem, 'SetLength', JvInterpreter_SetLength, 2, [varByRef or varString or varArray, varInteger], varEmpty);
    AddFunction(cSystem, 'High', JvInterpreter_High, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'Low', JvInterpreter_Low, 1, [varEmpty], varEmpty);
    AddFunction(cSystem, 'DeleteFromArray', JvInterpreter_DeleteFromArray, 2, [varEmpty, varEmpty], varEmpty);
    AddFunction(cSystem, 'InsertIntoArray', JvInterpreter_InsertIntoArray, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    //
    AddConst(cSystem, 'varEmpty', Ord(varEmpty));
    AddConst(cSystem, 'varSmallint', Ord(varSmallint));
    AddConst(cSystem, 'varInteger', Ord(varInteger));
    AddConst(cSystem, 'varSingle', Ord(varSingle));
    AddConst(cSystem, 'varCurrency', Ord(varCurrency));
    AddConst(cSystem, 'varDouble', Ord(varDouble));
    AddConst(cSystem, 'varDate', Ord(varDate));
    AddConst(cSystem, 'varOleStr', Ord(varOleStr));
    AddConst(cSystem, 'varDispatch', Ord(varDispatch));
    AddConst(cSystem, 'varError', Ord(varError));
    AddConst(cSystem, 'varBoolean', Ord(varBoolean));
    AddConst(cSystem, 'varVariant', Ord(varVariant));
    AddConst(cSystem, 'varUnknown', Ord(varUnknown));
    AddConst(cSystem, 'varByte', Ord(varByte));
    AddConst(cSystem, 'varStrArg', Ord(varStrArg));
    AddConst(cSystem, 'varSrting', Ord(varString));
    AddConst(cSystem, 'varAny', Ord(varAny));
    AddConst(cSystem, 'varTypeMask', Ord(varTypeMask));
    AddConst(cSystem, 'varArray', Ord(varArray));
    AddConst(cSystem, 'varByRef', Ord(varByRef));
    {$IFDEF COMPILER6_UP}
    AddConst(cSystem, 'varShortInt', Ord(varShortInt));
    AddConst(cSystem, 'varWord', Ord(varWord));
    AddConst(cSystem, 'varLongWord', Ord(varLongWord));
    AddConst(cSystem, 'varInt64', Ord(varInt64));
    {$ENDIF COMPILER6_UP}
  end;
end;

end.

