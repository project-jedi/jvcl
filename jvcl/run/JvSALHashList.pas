{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSALHashList.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSALHashList;

{$I jvcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF LINUX}
  Classes, SysUtils;

type
  TJvSALProc = procedure of object;
  TJvSALHash = function(const AString: string): Integer;
  TJvSALHashCompare = function(const Str1: string; const Str2: string): Boolean;

  PHashPointerList = ^THashPointerList;
  THashPointerList = array[1..1] of TObject;

  TJvBaseStringHashList = class(TObject)
    FList: PHashPointerList;
    FCapacity: Integer;
    FHash: TJvSALHash;
  protected
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
  public
    destructor Destroy; override;
    procedure Clear;
    property Capacity: Integer read FCapacity;
    property Items[Index: Integer]: Pointer read Get write Put; default;
  end;

  TJvHashStrings = class(TJvBaseStringHashList)
  public
    procedure AddString(AString: string; AId, AExId: TJvSALProc);
  end;

  TJvHashItems = class(TJvBaseStringHashList)
  public
    constructor Create(AHash: TJvSALHash);
    procedure AddString(AString: string; AId, AExId: TJvSALProc);
  end;

  TJvSALHashList = class(TJvBaseStringHashList)
  private
    FSecondaryHash: TJvSALHash;
    FCompare: TJvSALHashCompare;
  public
    constructor Create(Primary, Secondary: TJvSALHash; ACompare: TJvSALHashCompare);
    procedure AddString(AString: string; AId, AExId: TJvSALProc);
    function Hash(const S: string; var AId: TJvSALProc; var AExId: TJvSALProc): Boolean;
    function HashEx(const S: string; var AId: TJvSALProc; var AExId: TJvSALProc; HashValue: Integer): Boolean;
  end;

function CrcHash(const AString: string): Integer;
function ICrcHash(const AString: string): Integer;
function SmallCrcHash(const AString: string): Integer;
function ISmallCrcHash(const AString: string): Integer;
function TinyHash(const AString: string): Integer;
function ITinyHash(const AString: string): Integer;
function HashCompare(const Str1: string; const Str2: string): Boolean;
function IHashCompare(const Str1: string; const Str2: string): Boolean;

function HashSecondaryOne(const AString: string): Integer;
function HashSecondaryTwo(const AString: string): Integer;

procedure InitTables;

implementation

type
  TJvHashWord = class(TObject)
    S: string;
    Id: TJvSALProc;
    ExID: TJvSALProc;
    constructor Create(AString: string; AId, AExId: TJvSALProc);
  end;

var
  GlobalHashTable: array [#0..#255] of Byte;
  GlobalInsensitiveHashTable: array [#0..#255] of Byte;

procedure InitTables;
var
  I, K: Char;
  Temp: Byte;
begin
  for I := #0 to #255 do
    GlobalHashTable[I] := Ord(I);
  RandSeed := 255;
  for I := #1 to #255 do
  begin
    repeat
      K := Char(Random(255));
    until K <> #0;
    Temp := GlobalHashTable[I];
    GlobalHashTable[I] := GlobalHashTable[K];
    GlobalHashTable[K] := Temp;
  end;
  for I := #0 to #255 do
    GlobalInsensitiveHashTable[I] := GlobalHashTable[AnsiLowerCase(string(I))[1]];
end;

{ based on a Hash function by Cyrille de Brebisson }

function CrcHash(const AString: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(AString) do
  begin
    Result := (Result shr 4) xor (((Result xor GlobalHashTable[AString[I]]) and $F) * $1000);
    Result := (Result shr 4) xor (((Result xor (Ord(GlobalHashTable[AString[I]]) shr 4)) and $F) * $1000);
  end;
  if Result = 0 then
    Result := Length(AString) mod 8 + 1;
end;

function ICrcHash(const AString: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(AString) do
  begin
    Result := (Result shr 4) xor (((Result xor GlobalInsensitiveHashTable[AString[I]]) and $F) * $1000);
    Result := (Result shr 4) xor (((Result xor (Ord(GlobalInsensitiveHashTable[AString[I]]) shr 4)) and $F) * $1000);
  end;
  if Result = 0 then
    Result := Length(AString) mod 8 + 1;
end;

function SmallCrcHash(const AString: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(AString) do
  begin
    Result := (Result shr 4) xor (((Result xor GlobalHashTable[AString[I]]) and $F) * $80);
    Result := (Result shr 4) xor (((Result xor (Ord(GlobalHashTable[AString[I]]) shr 4)) and $F) * $80);
    if I = 3 then
      Break;
  end;
  if Result = 0 then
    Result := Length(AString) mod 8 + 1;
end;

function ISmallCrcHash(const AString: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(AString) do
  begin
    Result := (Result shr 4) xor (((Result xor GlobalInsensitiveHashTable[AString[I]]) and $F) * $80);
    Result := (Result shr 4) xor (((Result xor (Ord(GlobalInsensitiveHashTable[AString[I]]) shr 4)) and $F) * $80);
    if I = 3 then
      Break;
  end;
  if Result = 0 then
    Result := Length(AString) mod 8 + 1;
end;

function TinyHash(const AString: string): Integer;
var
  I: Integer;
begin
  Result := Length(AString);
  for I := 1 to Length(AString) do
  begin
    Inc(Result, GlobalHashTable[AString[I]]);
    Result := Result mod 128 + 1;
    if I = 2 then
      Break;
  end;
end;

function ITinyHash(const AString: string): Integer;
var
  I: Integer;
begin
  Result := Length(AString);
  for I := 1 to Length(AString) do
  begin
    Inc(Result, GlobalInsensitiveHashTable[AString[I]]);
    Result := Result mod 128 + 1;
    if I = 2 then
      Break;
  end;
end;

function HashCompare(const Str1: string; const Str2: string): Boolean;
var
  I: Integer;
begin
  Result := Length(Str1) = Length(Str2);
  if not Result then
    Exit;
  for I := 1 to Length(Str1) do
    if Str1[I] <> Str2[I] then
    begin
      Result := False;
      Break;
    end;
end;

function IHashCompare(const Str1: string; const Str2: string): Boolean;
var
  I: Integer;
begin
  Result := Length(Str1) = Length(Str2);
  if not Result then
    Exit;
  for I := 1 to Length(Str1) do
    if GlobalInsensitiveHashTable[Str1[I]] <> GlobalInsensitiveHashTable[Str2[I]] then
    begin
      Result := False;
      Break;
    end;
end;

function HashSecondaryOne(const AString: string): Integer;
begin
  Result := Length(AString);
  Inc(Result, GlobalInsensitiveHashTable[AString[Length(AString)]]);
  Result := Result mod 16 + 1;
  Inc(Result, GlobalInsensitiveHashTable[AString[1]]);
  Result := Result mod 16 + 1;
end;

function HashSecondaryTwo(const AString: string): Integer;
var
  I: Integer;
begin
  Result := Length(AString);
  for I := Length(AString) downto 1 do
  begin
    Inc(Result, GlobalInsensitiveHashTable[AString[I]]);
    Result := Result mod 32 + 1;
  end;
end;

//=== { TJvHashString } ======================================================

constructor TJvHashWord.Create(AString: string; AId, AExId: TJvSALProc);
begin
  inherited Create;
  S := AString;
  Id := AId;
  ExID := AExId;
end;

//=== { TJvBaseStringHashList } ==============================================

procedure TJvBaseStringHashList.Clear;
var
  I: Integer;
begin
  for I := 1 to FCapacity do
    FList[I].Free;
  ReallocMem(FList, 0);
  FCapacity := 0;
end;

destructor TJvBaseStringHashList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJvBaseStringHashList.Get(Index: Integer): Pointer;
begin
  Result := nil;
  if (Index > 0) and (Index <= FCapacity) then
    Result := FList[Index];
end;

procedure TJvBaseStringHashList.Put(Index: Integer; Item: Pointer);
begin
  if (Index > 0) and (Index <= FCapacity) then
    FList[Index] := Item;
end;

procedure TJvBaseStringHashList.SetCapacity(NewCapacity: Integer);
var
  I, OldCapacity: Integer;
begin
  if NewCapacity > FCapacity then
  begin
    ReallocMem(FList, (NewCapacity) * SizeOf(Pointer));
    OldCapacity := FCapacity;
    FCapacity := NewCapacity;
    for I := OldCapacity + 1 to NewCapacity do
      Items[I] := nil;
  end;
end;

//=== { TJvHashStrings } =====================================================

procedure TJvHashStrings.AddString(AString: string; AId, AExId: TJvSALProc);
begin
  SetCapacity(Capacity + 1);
  FList[Capacity] := TJvHashWord.Create(AString, AId, AExId);
end;

//=== { TJvHashItems } =======================================================

constructor TJvHashItems.Create(AHash: TJvSALHash);
begin
  inherited Create;
  FHash := AHash;
end;

procedure TJvHashItems.AddString(AString: string; AId, AExId: TJvSALProc);
var
  HashWord: TJvHashWord;
  HashStrings: TJvHashStrings;
  HashVal: Integer;
begin
  HashVal := FHash(AString);
  SetCapacity(HashVal);
  if Items[HashVal] = nil then
    Items[HashVal] := TJvHashWord.Create(AString, AId, AExId)
  else
  if FList[HashVal] is TJvHashStrings then
    TJvHashStrings(Items[HashVal]).AddString(AString, AId, AExId)
  else
  begin
    HashWord := Items[HashVal];
    HashStrings := TJvHashStrings.Create;
    Items[HashVal] := HashStrings;
    HashStrings.AddString(HashWord.S, HashWord.Id, HashWord.ExID);
    HashWord.Free;
    HashStrings.AddString(AString, AId, AExId)
  end;
end;

//=== { TJvSALHashList } =====================================================

constructor TJvSALHashList.Create(Primary, Secondary: TJvSALHash; ACompare: TJvSALHashCompare);
begin
  inherited Create;
  FHash := Primary;
  FSecondaryHash := Secondary;
  FCompare := ACompare;
end;

procedure TJvSALHashList.AddString(AString: string; AId, AExId: TJvSALProc);
var
  HashWord: TJvHashWord;
  HashValue: Integer;
  HashItems: TJvHashItems;
begin
  HashValue := FHash(AString);
  if HashValue >= FCapacity then
    SetCapacity(HashValue);
  if Items[HashValue] = nil then
    Items[HashValue] := TJvHashWord.Create(AString, AId, AExId)
  else
  if FList[HashValue] is TJvHashItems then
    TJvHashItems(Items[HashValue]).AddString(AString, AId, AExId)
  else
  begin
    HashWord := Items[HashValue];
    HashItems := TJvHashItems.Create(FSecondaryHash);
    Items[HashValue] := HashItems;
    HashItems.AddString(HashWord.S, HashWord.Id, HashWord.ExID);
    HashWord.Free;
    HashItems.AddString(AString, AId, AExId);
  end;
end;

function TJvSALHashList.Hash(const S: string; var AId: TJvSALProc;
  var AExId: TJvSALProc): Boolean;
begin
  Result := HashEx(S, AId, AExId, FHash(S));
end;

function TJvSALHashList.HashEx(const S: string; var AId: TJvSALProc;
  var AExId: TJvSALProc; HashValue: Integer): Boolean;
var
  Temp: TObject;
  HashWord: TJvHashWord;
  HashItems: TJvHashItems;
  I, ItemHash: Integer;
begin
  Result := False;
  AId := nil;
  AExId := nil;
  if (HashValue < 1) or (HashValue > Capacity) then
    Exit;
  if Items[HashValue] <> nil then
  begin
    if FList[HashValue] is TJvHashWord then
    begin
      HashWord := Items[HashValue];
      Result := FCompare(HashWord.S, S);
      if Result then
      begin
        AId := HashWord.Id;
        AExId := HashWord.ExID;
      end;
    end
    else
    begin
      HashItems := Items[HashValue];
      ItemHash := HashItems.FHash(S);
      if ItemHash > HashItems.Capacity then
        Exit;
      Temp := HashItems[ItemHash];
      if Temp <> nil then
        if Temp is TJvHashWord then
        begin
          Result := FCompare(TJvHashWord(Temp).S, S);
          if Result then
          begin
            AId := TJvHashWord(Temp).Id;
            AExId := TJvHashWord(Temp).ExID;
          end;
        end
        else
          for I := 1 to TJvHashStrings(Temp).Capacity do
          begin
            HashWord := TJvHashStrings(Temp)[I];
            Result := FCompare(HashWord.S, S);
            if Result then
            begin
              AId := HashWord.Id;
              AExId := HashWord.ExID;
              Exit;
            end;
          end;
    end;
  end;
end;

initialization
  InitTables;

end.

