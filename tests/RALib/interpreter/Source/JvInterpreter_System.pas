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

Contributor(s):  peter Fischer-Haase <pfischer@ise-online.de> commented as "pfh" 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : JVCL Interpreter version 2

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvInterpreter_System;

interface

uses JvInterpreter {$IFDEF COMPILER6_UP}, Variants {$ENDIF};

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation


  { TObject }

{  function ClassType: TClass; }

procedure TObject_ClassType(var Value: Variant; Args: TArgs);
begin
  Value := C2V(TObject(Args.Obj).ClassType);
end;

{  function ClassName: ShortString; }

procedure TObject_ClassName(var Value: Variant; Args: TArgs);
begin
  Value := TObject(Args.Obj).ClassName;
end;

{  function ClassNameIs(const Name: string): Boolean; }

procedure TObject_ClassNameIs(var Value: Variant; Args: TArgs);
begin
  Value := TObject(Args.Obj).ClassNameIs(Args.Values[0]);
end;

{  function ClassParent: TClass; }

procedure TObject_ClassParent(var Value: Variant; Args: TArgs);
begin
  Value := C2V(TObject(Args.Obj).ClassParent);
end;

{  function ClassInfo: Pointer; }

procedure TObject_ClassInfo(var Value: Variant; Args: TArgs);
begin
  Value := P2V(TObject(Args.Obj).ClassInfo);
end;

{  function InstanceSize: Longint; }

procedure TObject_InstanceSize(var Value: Variant; Args: TArgs);
begin
  Value := TObject(Args.Obj).InstanceSize;
end;

{  function InheritsFrom(AClass: TClass): Boolean; }

procedure TObject_InheritsFrom(var Value: Variant; Args: TArgs);
begin
  Value := TObject(Args.Obj).InheritsFrom(V2C(Args.Values[0]));
end;

(*
{  function GetInterface(const IID: TGUID; out Obj): Boolean; }
procedure TObject_GetInterface(var Value: Variant; Args: TArgs);
begin
  Value := TObject(Args.Obj).GetInterface(Args.Values[0], Args.Values[1], Args.Values[2]);
end;
*)

  { TInterfacedObject }

{$IFDEF COMPILER3_UP}
{ property Read RefCount: Integer }

procedure TInterfacedObject_Read_RefCount(var Value: Variant; Args: TArgs);
begin
  Value := TInterfacedObject(Args.Obj).RefCount;
end;
{$ENDIF COMPILER3_UP}

{ procedure Move(const Source; var Dest; Count: Integer); }

procedure JvInterpreter_Move(var Value: Variant; Args: TArgs);
begin
  Move(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function ParamCount: Integer; }

procedure JvInterpreter_ParamCount(var Value: Variant; Args: TArgs);
begin
  Value := ParamCount;
end;

{ function ParamStr(Index: Integer): string; }

procedure JvInterpreter_ParamStr(var Value: Variant; Args: TArgs);
begin
  Value := ParamStr(Args.Values[0]);
end;

{ procedure Randomize; }

procedure JvInterpreter_Randomize(var Value: Variant; Args: TArgs);
begin
  Randomize;
end;

procedure JvInterpreter_Random(var Value: Variant; Args: TArgs);
begin
  Value := Random(Integer(Args.Values[0]));
end;

{ function UpCase(Ch: Char): Char; }

procedure JvInterpreter_UpCase(var Value: Variant; Args: TArgs);
begin
  Value := UpCase(string(Args.Values[0])[1]);
end;

(*
{ function WideCharToString(Source: PWideChar): string; }
procedure JvInterpreter_WideCharToString(var Value: Variant; Args: TArgs);
begin
  Value := WideCharToString(Args.Values[0]);
end;

{ function WideCharLenToString(Source: PWideChar; SourceLen: Integer): string; }
procedure JvInterpreter_WideCharLenToString(var Value: Variant; Args: TArgs);
begin
  Value := WideCharLenToString(Args.Values[0], Args.Values[1]);
end;

{ procedure WideCharToStrVar(Source: PWideChar; var Dest: string); }
procedure JvInterpreter_WideCharToStrVar(var Value: Variant; Args: TArgs);
begin
  WideCharToStrVar(Args.Values[0], string(TVarData(Args.Values[1]).vString));
end;

{ procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer; var Dest: string); }
procedure JvInterpreter_WideCharLenToStrVar(var Value: Variant; Args: TArgs);
begin
  WideCharLenToStrVar(Args.Values[0], Args.Values[1], string(TVarData(Args.Values[2]).vString));
end;

{ function StringToWideChar(const Source: string; Dest: PWideChar; DestSize: Integer): PWideChar; }
procedure JvInterpreter_StringToWideChar(var Value: Variant; Args: TArgs);
begin
  Value := StringToWideChar(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function OleStrToString(Source: PWideChar): string; }
procedure JvInterpreter_OleStrToString(var Value: Variant; Args: TArgs);
begin
  Value := OleStrToString(Args.Values[0]);
end;

{ procedure OleStrToStrVar(Source: PWideChar; var Dest: string); }
procedure JvInterpreter_OleStrToStrVar(var Value: Variant; Args: TArgs);
begin
  OleStrToStrVar(Args.Values[0], string(TVarData(Args.Values[1]).vString));
end;

{ function StringToOleStr(const Source: string): PWideChar; }
procedure JvInterpreter_StringToOleStr(var Value: Variant; Args: TArgs);
begin
  Value := StringToOleStr(Args.Values[0]);
end;
*)

{ function VarType(const V: Variant): Integer; }

procedure JvInterpreter_VarType(var Value: Variant; Args: TArgs);
begin
  Value := VarType(Args.Values[0]);
end;

{ function VarAsType(const V: Variant; VarType: Integer): Variant; }

procedure JvInterpreter_VarAsType(var Value: Variant; Args: TArgs);
begin
  Value := VarAsType(Args.Values[0], Args.Values[1]);
end;

{ function VarIsEmpty(const V: Variant): Boolean; }

procedure JvInterpreter_VarIsEmpty(var Value: Variant; Args: TArgs);
begin
  Value := VarIsEmpty(Args.Values[0]);
end;

{ function VarIsNull(const V: Variant): Boolean; }

procedure JvInterpreter_VarIsNull(var Value: Variant; Args: TArgs);
begin
  Value := VarIsNull(Args.Values[0]);
end;

{ function VarToStr(const V: Variant): string; }

procedure JvInterpreter_VarToStr(var Value: Variant; Args: TArgs);
begin
  Value := VarToStr(Args.Values[0]);
end;

{ function VarFromDateTime(DateTime: TDateTime): Variant; }

procedure JvInterpreter_VarFromDateTime(var Value: Variant; Args: TArgs);
begin
  Value := VarFromDateTime(Args.Values[0]);
end;

{ function VarToDateTime(const V: Variant): TDateTime; }

procedure JvInterpreter_VarToDateTime(var Value: Variant; Args: TArgs);
begin
  Value := VarToDateTime(Args.Values[0]);
end;

(*
{ function VarArrayCreate(const Bounds: array of Integer; VarType: Integer): Variant; }
procedure JvInterpreter_VarArrayCreate(var Value: Variant; Args: TArgs);
begin
  Value := VarArrayCreate(Args.Values[0], Args.Values[1]);
end;

{ function VarArrayOf(const Values: array of Variant): Variant; }
procedure JvInterpreter_VarArrayOf(var Value: Variant; Args: TArgs);
begin
  Value := VarArrayOf(Args.Values[0]);
end;

{ function VarArrayDimCount(const A: Variant): Integer; }
procedure JvInterpreter_VarArrayDimCount(var Value: Variant; Args: TArgs);
begin
  Value := VarArrayDimCount(Args.Values[0]);
end;

{ function VarArrayLowBound(const A: Variant; Dim: Integer): Integer; }
procedure JvInterpreter_VarArrayLowBound(var Value: Variant; Args: TArgs);
begin
  Value := VarArrayLowBound(Args.Values[0], Args.Values[1]);
end;

{ function VarArrayHighBound(const A: Variant; Dim: Integer): Integer; }
procedure JvInterpreter_VarArrayHighBound(var Value: Variant; Args: TArgs);
begin
  Value := VarArrayHighBound(Args.Values[0], Args.Values[1]);
end;

{ function VarArrayLock(const A: Variant): Pointer; }
procedure JvInterpreter_VarArrayLock(var Value: Variant; Args: TArgs);
begin
  Value := P2V(VarArrayLock(Args.Values[0]));
end;

{ procedure VarArrayUnlock(const A: Variant); }
procedure JvInterpreter_VarArrayUnlock(var Value: Variant; Args: TArgs);
begin
  VarArrayUnlock(Args.Values[0]);
end;

{ function VarArrayRef(const A: Variant): Variant; }
procedure JvInterpreter_VarArrayRef(var Value: Variant; Args: TArgs);
begin
  Value := VarArrayRef(Args.Values[0]);
end;

{ function VarIsArray(const A: Variant): Boolean; }
procedure JvInterpreter_VarIsArray(var Value: Variant; Args: TArgs);
begin
  Value := VarIsArray(Args.Values[0]);
end;
*)

{ function Ord(const A: Variant): Integer; }

procedure JvInterpreter_Ord(var Value: Variant; Args: TArgs);
begin
  if VarType(Args.Values[0]) = varString then
    Value := Ord(VarToStr(Args.Values[0])[1])
  else
    Value := Integer(Args.Values[0]);
end;


{ function Chr(X: Byte): Char }

procedure JvInterpreter_Chr(var Value: Variant; Args: TArgs);
begin
  Value := Chr(Byte(Args.Values[0]));
end;

{ function Abs(X); }

procedure JvInterpreter_Abs(var Value: Variant; Args: TArgs);
begin
  if VarType(Args.Values[0]) = varInteger then
    Value := Abs(Integer(Args.Values[0]))
  else
    Value := Abs(Extended(Args.Values[0]));
end;

{ function Length(S): Integer; }

procedure JvInterpreter_Length(var Value: Variant; Args: TArgs);
begin
  Value := Length(Args.Values[0]);
end;

{ function Copy(S; Index, Count: Integer): String; }

procedure JvInterpreter_Copy(var Value: Variant; Args: TArgs);
begin
  Value := Copy(Args.Values[0], Integer(Args.Values[1]), Integer(Args.Values[2]));
end;

{ function Round(Value: Extended): Int64; }

procedure JvInterpreter_Round(var Value: Variant; Args: TArgs);
begin
  Value := Integer(Round(Args.Values[0]));
end;

{ function Trunc(Value: Extended): Int64; }

procedure JvInterpreter_Trunc(var Value: Variant; Args: TArgs);
begin
  Value := Integer(Trunc(Args.Values[0]));
end;

{ function Pos(Substr: string; S: string): Integer; }

procedure JvInterpreter_Pos(var Value: Variant; Args: TArgs);
begin
  Value := Pos(String(Args.Values[0]), String(Args.Values[1]));
end;

//+++pfh
{procedure Delete(var S: string; Index, Count:Integer);}
procedure JvInterpreter_Delete(var value : Variant; Args : TArgs);
Var
  s : String;
Begin
  s := Args.Values[0];
  Delete(S,Integer(Args.Values[1]),Integer(Args.Values[2]));
  Args.Values[0] := s;
  Value := S;
End;

{procedure Insert(Source: string; var S: string; Index: Integer);}
procedure JvInterpreter_Insert(var value : Variant; Args : TArgs);
Var
  s : String;
Begin
  s := Args.Values[1];
  Insert(String(Args.Values[0]),S,Integer(Args.Values[2]));
  Args.Values[1] := s;
  Value := S;
End;

{ function Sqr(X: Extended): Extended; }
procedure JvInterpreter_Sqr(var value : Variant; Args : TArgs);
Begin
  Value := Sqr(Args.Values[0]);
End;

{ function Sqrt(X: Extended): Extended; }
procedure JvInterpreter_Sqrt(var value : Variant; Args : TArgs);
Begin
  Value := Sqrt(Args.Values[0]);
End;

{ function Exp(X: Extended): Extended; }
procedure JvInterpreter_Exp(var value : Variant; Args : TArgs);
Begin
  Value := Exp(Args.Values[0]);
End;

{ function Ln(X: Extended): Extended; }
procedure JvInterpreter_Ln(var value : Variant; Args : TArgs);
Begin
  Value := Ln(Args.Values[0]);
End;

{ function Sin(X: Extended): Extended; }
procedure JvInterpreter_Sin(var value : Variant; Args : TArgs);
Begin
  Value := Sin(Args.Values[0]);
End;

{ function Cos(X: Extended): Extended; }
procedure JvInterpreter_Cos(var value : Variant; Args : TArgs);
Begin
  Value := Cos(Args.Values[0]);
End;

{ function Tan(X: Extended): Extended; }
procedure JvInterpreter_Tan(var value : Variant; Args : TArgs);
Begin
  Value := Sqr(Args.Values[0]);
End;

{ function ArcTan(X: Extended): Extended; }
procedure JvInterpreter_ArcTan(var value : Variant; Args : TArgs);
Begin
  Value := Sqr(Args.Values[0]);
End;
//---pfh

{ procedure SetLength(var s: ShortString; newLength: Integer); }
procedure JvInterpreter_SetLength(var Value: Variant; Args: TArgs);
begin
  SetLength(string(TVarData(Args.Values[0]).vString), Integer(Args.Values[1]));
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  with JvInterpreterAdapter do
  begin
   { TObject }
    AddClass('System', TObject, 'TObject');
    AddGet(TObject, 'ClassType', TObject_ClassType, 0, [0], varEmpty);
    AddGet(TObject, 'ClassName', TObject_ClassName, 0, [0], varEmpty);
    AddGet(TObject, 'ClassNameIs', TObject_ClassNameIs, 1, [varEmpty], varEmpty);
    AddGet(TObject, 'ClassParent', TObject_ClassParent, 0, [0], varEmpty);
    AddGet(TObject, 'ClassInfo', TObject_ClassInfo, 0, [0], varEmpty);
    AddGet(TObject, 'InstanceSize', TObject_InstanceSize, 0, [0], varEmpty);
    AddGet(TObject, 'InheritsFrom', TObject_InheritsFrom, 1, [varEmpty], varEmpty);
   // AddGet(TObject, 'GetInterface', TObject_GetInterface, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
   { TInterfacedObject }
{$IFDEF COMPILER3_UP}
    AddClass('System', TInterfacedObject, 'TInterfacedObject');
    AddGet(TInterfacedObject, 'RefCount', TInterfacedObject_Read_RefCount, 0, [0], varEmpty);
{$ENDIF COMPILER3_UP}
    AddFun('System', 'Move', JvInterpreter_Move, 3, [varEmpty, varByRef, varEmpty], varEmpty);
    AddFun('System', 'ParamCount', JvInterpreter_ParamCount, 0, [0], varEmpty);
    AddFun('System', 'ParamStr', JvInterpreter_ParamStr, 1, [varEmpty], varEmpty);
    AddFun('System', 'Randomize', JvInterpreter_Randomize, 0, [0], varEmpty);
    AddFun('System', 'Random', JvInterpreter_Random, 1, [varInteger], varEmpty);
    AddFun('System', 'UpCase', JvInterpreter_UpCase, 1, [varEmpty], varEmpty);
  {  AddFun('System', 'WideCharToString', JvInterpreter_WideCharToString, 1, [varEmpty], varEmpty);
    AddFun('System', 'WideCharLenToString', JvInterpreter_WideCharLenToString, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('System', 'WideCharToStrVar', JvInterpreter_WideCharToStrVar, 2, [varEmpty, varByRef], varEmpty);
    AddFun('System', 'WideCharLenToStrVar', JvInterpreter_WideCharLenToStrVar, 3, [varEmpty, varEmpty, varByRef], varEmpty);
    AddFun('System', 'StringToWideChar', JvInterpreter_StringToWideChar, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddFun('System', 'OleStrToString', JvInterpreter_OleStrToString, 1, [varEmpty], varEmpty);
    AddFun('System', 'OleStrToStrVar', JvInterpreter_OleStrToStrVar, 2, [varEmpty, varByRef], varEmpty);
    AddFun('System', 'StringToOleStr', JvInterpreter_StringToOleStr, 1, [varEmpty], varEmpty); }
    AddFun('System', 'VarType', JvInterpreter_VarType, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarAsType', JvInterpreter_VarAsType, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('System', 'VarIsEmpty', JvInterpreter_VarIsEmpty, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarIsNull', JvInterpreter_VarIsNull, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarToStr', JvInterpreter_VarToStr, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarFromDateTime', JvInterpreter_VarFromDateTime, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarToDateTime', JvInterpreter_VarToDateTime, 1, [varEmpty], varEmpty);
   { AddFun('System', 'VarArrayCreate', JvInterpreter_VarArrayCreate, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('System', 'VarArrayOf', JvInterpreter_VarArrayOf, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarArrayDimCount', JvInterpreter_VarArrayDimCount, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarArrayLowBound', JvInterpreter_VarArrayLowBound, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('System', 'VarArrayHighBound', JvInterpreter_VarArrayHighBound, 2, [varEmpty, varEmpty], varEmpty);
    AddFun('System', 'VarArrayLock', JvInterpreter_VarArrayLock, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarArrayUnlock', JvInterpreter_VarArrayUnlock, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarArrayRef', JvInterpreter_VarArrayRef, 1, [varEmpty], varEmpty);
    AddFun('System', 'VarIsArray', JvInterpreter_VarIsArray, 1, [varEmpty], varEmpty); }
    AddFun('System', 'ord', JvInterpreter_Ord, 1, [varEmpty], varEmpty);

    AddFun('system', 'Chr', JvInterpreter_Chr, 1, [varEmpty], varEmpty);
    AddFun('system', 'Abs', JvInterpreter_Abs,1,[varEmpty], varEmpty);
    AddFun('system', 'Length', JvInterpreter_Length,1,[varEmpty], varEmpty);
    AddFun('system', 'Copy', JvInterpreter_Copy,3,[varEmpty,varEmpty,varEmpty], varEmpty);
    AddFun('system', 'Round', JvInterpreter_Round,1,[varEmpty], varEmpty);
    AddFun('system', 'Trunc', JvInterpreter_Trunc,1,[varEmpty], varEmpty);
    AddFun('system', 'Pos', JvInterpreter_Pos,2,[varEmpty,varEmpty], varEmpty);

//+++pfh
    // some Stringfunctions
    AddFun('system', 'Delete', JvInterpreter_Delete,3,[varByRef,varEmpty,varEmpty], varEmpty);
    AddFun('system', 'Insert', JvInterpreter_Insert,3,[varEmpty,varByRef,varEmpty], varEmpty);
    // some mathfunctions
    AddFun('system', 'Sqr', JvInterpreter_Sqr,1,[varEmpty], varEmpty);
    AddFun('system', 'Sqrt', JvInterpreter_Sqrt,1,[varEmpty], varEmpty);
    AddFun('system', 'Exp', JvInterpreter_Exp,1,[varEmpty], varEmpty);
    AddFun('system', 'Ln', JvInterpreter_Ln,1,[varEmpty], varEmpty);
    AddFun('system', 'Sin', JvInterpreter_Sin,1,[varEmpty], varEmpty);
    AddFun('system', 'Cos', JvInterpreter_Cos,1,[varEmpty], varEmpty);
    AddFun('system', 'Tan', JvInterpreter_Tan,1,[varEmpty], varEmpty);
    AddFun('system', 'ArcTan', JvInterpreter_ArcTan,1,[varEmpty], varEmpty);
//---pfh
    AddFun('system', 'SetLength', JvInterpreter_SetLength, 2, [varByRef or varString, varInteger], varEmpty);

 end; { with }
end; { RegisterJvInterpreterAdapter }

end.

