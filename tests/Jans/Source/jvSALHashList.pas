{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSALHashList.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvSALHashList;

interface

uses Classes, SysUtils, Windows;

var
  janHashTable: array[#0..#255] of byte;
  janInsensitiveHashTable: array[#0..#255] of Byte;

type
  TJvSALProc = procedure of object;
  TJvSALHash = function(const aString: string): Integer;
  TJvSALHashCompare = function(const Str1: string; const Str2: string): Boolean;

  TJvHashWord = class
    S: string;
    Id: TJvSALProc;
    ExID: TJvSALProc;
    constructor Create(aString: string; anId, anExId: TJvSALProc);
  end;

  PHashPointerList = ^THashPointerList;
  THashPointerList = array[1..1] of TObject;

  TJvBaseStringHashList = class(TObject)
    FList: PHashPointerList;
    fCapacity: Integer;
  protected
    fHash: TJvSALHash;
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
  public
    destructor Destroy; override;
    procedure Clear;
    property Capacity: Integer read fCapacity;
    property Items[Index: Integer]: Pointer read Get write Put; default;
  end;

  TJvHashStrings = class(TJvBaseStringHashList)
  public
    procedure AddString(aString: string; anId, anExId: TJvSALProc);
  end;

  TJvHashItems = class(TJvBaseStringHashList)
  public
    constructor Create(aHash: TJvSALHash);
    procedure AddString(aString: string; anId, anExId: TJvSALProc);
  end;

  TJvSALHashList = class(TJvBaseStringHashList)
  private
    fSecondaryHash: TJvSALHash;
    fCompare: TJvSALHashCompare;
  public
    constructor Create(Primary, Secondary: TJvSALHash; aCompare: TJvSALHashCompare);
    procedure AddString(aString: string; anId, anExId: TJvSALProc);
    function Hash(const S: string; var anId: TJvSALProc; var anExId: TJvSALProc): Boolean;
    function HashEX(const S: string; var anId: TJvSALProc; var anExId: TJvSALProc; HashValue: Integer): Boolean;
  end;

function CrcHash(const aString: string): integer;
function ICrcHash(const aString: string): integer;
function SmallCrcHash(const aString: string): integer;
function ISmallCrcHash(const aString: string): integer;
function TinyHash(const aString: string): Integer;
function ITinyHash(const aString: string): Integer;
function HashCompare(const Str1: string; const Str2: string): Boolean;
function IHashCompare(const Str1: string; const Str2: string): Boolean;

function HashSecondaryOne(const aString: string): Integer;
function HashSecondaryTwo(const aString: string): Integer;

procedure InitTables;

implementation

procedure InitTables;
var
  I, K: Char;
  Temp: Byte;
begin
  for I := #0 to #255 do
  begin
    janHashTable[I] := Ord(I);
  end;
  RandSeed := 255;
  for I := #1 to #255 do
  begin
    repeat
      K := Char(Random(255));
    until K <> #0;
    Temp := janHashTable[I];
    janHashTable[I] := janHashTable[K];
    janHashTable[K] := Temp;
  end;
  for I := #0 to #255 do
    janInsensitiveHashTable[I] := janHashTable[AnsiLowerCase(string(I))[1]];
end;

{ based on a Hasch function by Cyrille de Brebisson }

function CrcHash(const aString: string): integer;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to length(aString) do
  begin
    Result := (Result shr 4) xor (((Result xor janHashTable[aString[I]]) and $F) * $1000);
    Result := (Result shr 4) xor (((Result xor (ord(janHashTable[aString[I]]) shr 4)) and $F) * $1000);
  end;
  if Result = 0 then Result := Length(aString) mod 8 + 1;
end;

function ICrcHash(const aString: string): integer;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to length(aString) do
  begin
    Result := (Result shr 4) xor (((Result xor janInsensitiveHashTable[aString[I]]) and $F) * $1000);
    Result := (Result shr 4) xor (((Result xor (ord(janInsensitiveHashTable[aString[I]]) shr 4)) and $F) * $1000);
  end;
  if Result = 0 then Result := Length(aString) mod 8 + 1;
end;

function SmallCrcHash(const aString: string): integer;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to length(aString) do
  begin
    Result := (Result shr 4) xor (((Result xor janHashTable[aString[I]]) and $F) * $80);
    Result := (Result shr 4) xor (((Result xor (ord(janHashTable[aString[I]]) shr 4)) and $F) * $80);
    if I = 3 then break;
  end;
  if Result = 0 then Result := Length(aString) mod 8 + 1;
end;

function ISmallCrcHash(const aString: string): integer;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to length(aString) do
  begin
    Result := (Result shr 4) xor (((Result xor janInsensitiveHashTable[aString[I]]) and $F) * $80);
    Result := (Result shr 4) xor (((Result xor (ord(janInsensitiveHashTable[aString[I]]) shr 4)) and $F) * $80);
    if I = 3 then break;
  end;
  if Result = 0 then Result := Length(aString) mod 8 + 1;
end;

function TinyHash(const aString: string): Integer;
var
  I: Integer;
begin
  Result := Length(aString);
  for i := 1 to length(aString) do
  begin
    inc(Result, janHashTable[aString[I]]);
    Result := Result mod 128 + 1;
    if I = 2 then break;
  end;
end;

function ITinyHash(const aString: string): Integer;
var
  I: Integer;
begin
  Result := Length(aString);
  for i := 1 to length(aString) do
  begin
    inc(Result, janInsensitiveHashTable[aString[I]]);
    Result := Result mod 128 + 1;
    if I = 2 then break;
  end;
end;

function HashCompare(const Str1: string; const Str2: string): Boolean;
var
  I: Integer;
begin
  if Length(Str1) <> Length(Str2) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for I := 1 to Length(Str1) do
    if Str1[I] <> Str2[I] then
    begin
      Result := False;
      Exit;
    end;
end;

function IHashCompare(const Str1: string; const Str2: string): Boolean;
var
  I: Integer;
begin
  if Length(Str1) <> Length(Str2) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for I := 1 to Length(Str1) do
    if janInsensitiveHashTable[Str1[I]] <> janInsensitiveHashTable[Str2[I]] then
    begin
      Result := False;
      Exit;
    end;
end;

function HashSecondaryOne(const aString: string): Integer;
begin
  Result := Length(aString);
  inc(Result, janInsensitiveHashTable[aString[Length(aString)]]);
  Result := Result mod 16 + 1;
  inc(Result, janInsensitiveHashTable[aString[1]]);
  Result := Result mod 16 + 1;
end;

function HashSecondaryTwo(const aString: string): Integer;
var
  I: Integer;
begin
  Result := Length(aString);
  for I := Length(aString) downto 1 do
  begin
    inc(Result, janInsensitiveHashTable[aString[I]]);
    Result := Result mod 32 + 1;
  end;
end;

{ TJvHashString }

constructor TJvHashWord.Create(aString: string; anId, anExId: TJvSALProc);
begin
  inherited Create;
  S := aString;
  Id := anId;
  ExID := anExId;
end;

{ TJvBaseStringHashList }

procedure TJvBaseStringHashList.Clear;
var
  I: Integer;
begin
  for I := 1 to fCapacity do
    if fList[I] <> nil then
      fList[I].Free;
  ReallocMem(FList, 0);
  fCapacity := 0;
end;

destructor TJvBaseStringHashList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJvBaseStringHashList.Get(Index: Integer): Pointer;
begin
  Result := nil;
  if (Index > 0) and (Index <= fCapacity) then
    Result := fList[Index];
end;

procedure TJvBaseStringHashList.Put(Index: Integer; Item: Pointer);
begin
  if (Index > 0) and (Index <= fCapacity) then
    fList[Index] := Item;
end;

procedure TJvBaseStringHashList.SetCapacity(NewCapacity: Integer);
var
  I, OldCapacity: Integer;
begin
  if NewCapacity > fCapacity then
  begin
    ReallocMem(FList, (NewCapacity) * SizeOf(Pointer));
    OldCapacity := fCapacity;
    FCapacity := NewCapacity;
    for I := OldCapacity + 1 to NewCapacity do
      Items[I] := nil;
  end;
end;

{ TJvHashStrings }

procedure TJvHashStrings.AddString(aString: string; anId, anExId: TJvSALProc);
begin
  SetCapacity(Capacity + 1);
  fList[Capacity] := TJvHashWord.Create(aString, anId, anExId);
end;

{ TJvHashItems }

procedure TJvHashItems.AddString(aString: string; anId, anExId: TJvSALProc);
var
  HashWord: TJvHashWord;
  HashStrings: TJvHashStrings;
  HashVal: Integer;
begin
  HashVal := fHash(aString);
  SetCapacity(HashVal);
  if Items[HashVal] = nil then
  begin
    Items[HashVal] := TJvHashWord.Create(aString, anId, anExId);
  end
  else if fList[HashVal] is TJvHashStrings then
  begin
    TJvHashStrings(Items[HashVal]).AddString(aString, anId, anExId);
  end
  else
  begin
    HashWord := Items[HashVal];
    HashStrings := TJvHashStrings.Create;
    Items[HashVal] := HashStrings;
    HashStrings.AddString(HashWord.S, HashWord.Id, HashWord.ExId);
    HashWord.Free;
    HashStrings.AddString(aString, anId, anExId)
  end;
end;

constructor TJvHashItems.Create(aHash: TJvSALHash);
begin
  inherited Create;
  fHash := aHash;
end;

{ TJvSALHashList }

constructor TJvSALHashList.Create(Primary, Secondary: TJvSALHash; aCompare: TJvSALHashCompare);
begin
  inherited Create;
  fHash := Primary;
  fSecondaryHash := Secondary;
  fCompare := aCompare;
end;

procedure TJvSALHashList.AddString(aString: string; anId, anExId: TJvSALProc);
var
  HashWord: TJvHashWord;
  HashValue: Integer;
  HashItems: TJvHashItems;
begin
  HashValue := fHash(aString);
  if HashValue >= fCapacity then SetCapacity(HashValue);
  if Items[HashValue] = nil then
  begin
    Items[HashValue] := TJvHashWord.Create(aString, anId, anExId);
  end
  else if fList[HashValue] is TJvHashItems then
  begin
    TJvHashItems(Items[HashValue]).AddString(aString, anId, anExId);
  end
  else
  begin
    HashWord := Items[HashValue];
    HashItems := TJvHashItems.Create(fSecondaryHash);
    Items[HashValue] := HashItems;
    HashItems.AddString(HashWord.S, HashWord.Id, HashWord.ExId);
    HashWord.Free;
    HashItems.AddString(aString, anId, anExId);
  end;
end;

function TJvSALHashList.Hash(const S: string; var anId: TJvSALProc; var anExId: TJvSALProc): Boolean;
begin
  Result := HashEX(S, anId, anExId, fHash(S));
end;

function TJvSALHashList.HashEX(const S: string; var anId: TJvSALProc; var anExId: TJvSALProc; HashValue: integer): Boolean;
var
  Temp: TObject;
  Hashword: TJvHashWord;
  HashItems: TJvHashItems;
  I, ItemHash: Integer;
begin
  Result := False;
  anID := nil;
  anExId := nil;
  if HashValue < 1 then Exit;
  if HashValue > Capacity then Exit;
  if Items[HashValue] <> nil then
  begin
    if fList[HashValue] is TJvHashWord then
    begin
      Hashword := Items[HashValue];
      Result := fCompare(HashWord.S, S);
      if Result then
      begin
        anID := HashWord.Id;
        anExId := HashWord.ExID;
      end;
    end
    else
    begin
      HashItems := Items[HashValue];
      ItemHash := HashItems.fHash(S);
      if ItemHash > HashItems.Capacity then Exit;
      Temp := HashItems[ItemHash];
      if Temp <> nil then
        if Temp is TJvHashWord then
        begin
          Result := fCompare(TJvHashWord(Temp).S, S);
          if Result then
          begin
            anID := TJvHashWord(Temp).Id;
            anExId := TJvHashWord(Temp).ExID;
          end;
        end
        else
          for I := 1 to TJvHashStrings(Temp).Capacity do
          begin
            HashWord := TJvHashStrings(Temp)[I];
            Result := fCompare(HashWord.S, S);
            if Result then
            begin
              anID := HashWord.Id;
              anExId := HashWord.ExID;
              exit;
            end;
          end;
    end;
  end;
end;

initialization
  InitTables;
  {$R+}
end.
