{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHook.PAS, released on 2002-07-04.

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

unit JvHook;

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ELSE}
  WinTypes, WinProcs,
  {$ENDIF}
  SysUtils;
  {, JvComponent}

type
  PClass = ^TClass;

function GetVirtualMethodAddress(AClass: TClass; AIndex: Integer): Pointer;
function SetVirtualMethodAddress(AClass: TClass; AIndex: Integer;
  NewAddress: Pointer): Pointer;
function FindVirtualMethodIndex(AClass: TClass; MethodAddr: Pointer): Integer;

implementation

{ SetVirtualMethodAddress procedure. Destroy destructor has index 0,
  first user defined virtual method has index 1. }

type
  PPointer = ^Pointer;

function GetVirtualMethodAddress(AClass: TClass; AIndex: Integer): Pointer;
var
  Table: PPointer;
begin
  Table := PPointer(AClass);
  Inc(Table, AIndex - 1);
  Result := Table^;
end;

function SetVirtualMethodAddress(AClass: TClass; AIndex: Integer;
  NewAddress: Pointer): Pointer;
{$IFDEF WIN32}
const
  PageSize = SizeOf(Pointer);
{$ENDIF}
var
  Table: PPointer;
  {$IFDEF WIN32}
  SaveFlag: DWORD;
  {$ELSE}
  Block: Pointer;
  {$ENDIF}
begin
  Table := PPointer(AClass);
  Inc(Table, AIndex - 1);
  Result := Table^;
  {$IFDEF WIN32}
  if VirtualProtect(Table, PageSize, PAGE_EXECUTE_READWRITE, @SaveFlag) then
  try
    Table^ := NewAddress;
  finally
    VirtualProtect(Table, PageSize, SaveFlag, @SaveFlag);
  end;
  {$ELSE}
  PtrRec(Block).Ofs := PtrRec(Table).Ofs;
  PtrRec(Block).Seg := AllocCSToDSAlias(PtrRec(Table).Seg);
  try
    PPointer(Block)^ := NewAddress;
  finally
    FreeSelector(PtrRec(Block).Seg);
  end;
  {$ENDIF}
end;

function FindVirtualMethodIndex(AClass: TClass; MethodAddr: Pointer): Integer;
begin
  Result := 0;
  repeat
    Inc(Result);
  until GetVirtualMethodAddress(AClass, Result) = MethodAddr;
end;

end.

