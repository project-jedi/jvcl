{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvObjStr.PAS, released on 2002-07-04.

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

unit JvObjectStrings;

interface

uses
  {$IFDEF COMPILER6_UP}
  SysUtils, Classes, RTLConsts;
  {$ELSE}
  SysUtils, Classes;
  {$ENDIF}

type
  TDestroyEvent = procedure(Sender, AObject: TObject) of object;
  TObjectSortCompare = function(const S1, S2: string;
    Item1, Item2: TObject): Integer of object;

  TJvObjectStrings = class(TStringList)
  private
    FOnDestroyObject: TDestroyEvent;
  protected
    procedure DestroyObject(AObject: TObject); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Remove(Index: Integer);
    procedure ParseStrings(const Values: string);
    procedure SortList(Compare: TObjectSortCompare);
    property OnDestroyObject: TDestroyEvent read FOnDestroyObject write FOnDestroyObject;
  end;

const
  {$IFDEF WIN32}
  MaxHugeListSize = MaxListSize;
  {$ELSE}
  MaxHugeListSize = (MaxLongint div SizeOf(Pointer)) - 4;
  {$ENDIF}

type
  {$IFDEF WIN32}
  TJvHugeList = class(TList);
  {$ELSE}
  TJvHugeList = class(TObject)
  private
    FList: TMemoryStream;
    FCount: Longint;
    FCapacity: Longint;
  protected
    function Get(Index: Longint): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Longint; Item: Pointer);
    procedure SetCapacity(NewCapacity: Longint);
    procedure SetCount(NewCount: Longint);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Longint;
    procedure Clear;
    procedure Delete(Index: Longint);
    procedure Exchange(Index1, Index2: Longint);
    function Expand: TJvHugeList;
    function First: Pointer;
    function IndexOf(Item: Pointer): Longint;
    procedure Insert(Index: Longint; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Longint);
    function Remove(Item: Pointer): Longint;
    procedure Pack;
    property Capacity: Longint read FCapacity write SetCapacity;
    property Count: Longint read FCount write SetCount;
    property Items[Index: Longint]: Pointer read Get write Put; default;
  end;
  {$ENDIF WIN32}

{$IFDEF WIN32}

type
  TItemSortCompare = function(Item1, Item2: TCollectionItem): Integer of object;

  TJvSortCollection = class(TCollection)
  protected
    procedure QuickSort(L, R: Integer; Compare: TItemSortCompare); virtual;
  public
    procedure Sort(Compare: TItemSortCompare);
  end;

{$ENDIF WIN32}

implementation

uses
  Consts,
  {$IFNDEF WIN32}
  JvVCLUtils,
  {$ENDIF}
  JvStrUtils;

// (rom) to JCL

procedure QuickSort(SortList: TStrings; L, R: Integer; SCompare: TObjectSortCompare);
var
  I, J: Integer;
  P: TObject;
  S: string;
begin
  repeat
    I := L;
    J := R;
    P := SortList.Objects[(L + R) shr 1];
    S := SortList[(L + R) shr 1];
    repeat
      while SCompare(SortList[I], S, SortList.Objects[I], P) < 0 do
        Inc(I);
      while SCompare(SortList[J], S, SortList.Objects[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        SortList.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

//=== TJvObjectStrings =======================================================

procedure TJvObjectStrings.DestroyObject(AObject: TObject);
begin
  if Assigned(FOnDestroyObject) then
    FOnDestroyObject(Self, AObject)
  else
  if AObject <> nil then
    AObject.Free;
end;

procedure TJvObjectStrings.Clear;
var
  I: Integer;
begin
  if Count > 0 then
  begin
    Changing;
    for I := 0 to Count - 1 do
      Objects[I] := nil;
    BeginUpdate;
    try
      inherited Clear;
    finally
      EndUpdate;
    end;
    Changed;
  end;
end;

procedure TJvObjectStrings.Delete(Index: Integer);
begin
  Objects[Index] := nil;
  inherited Delete(Index);
end;

procedure TJvObjectStrings.Remove(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TJvObjectStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    TempString := Get(CurIndex);
    TempObject := GetObject(CurIndex);
    inherited Delete(CurIndex);
    try
      InsertObject(NewIndex, TempString, TempObject);
    except
      DestroyObject(TempObject);
      raise;
    end;
  end;
end;

procedure TJvObjectStrings.PutObject(Index: Integer; AObject: TObject);
begin
  Changing;
  BeginUpdate;
  try
    if (Index < Self.Count) and (Index >= 0) then
      DestroyObject(Objects[Index]);
    inherited PutObject(Index, AObject);
  finally
    EndUpdate;
  end;
  Changed;
end;

procedure TJvObjectStrings.ParseStrings(const Values: string);
var
  Pos: Integer;
begin
  Pos := 1;
  BeginUpdate;
  try
    while Pos <= Length(Values) do
      Add(ExtractSubstr(Values, Pos, [';']));
  finally
    EndUpdate;
  end;
end;

procedure TJvObjectStrings.SortList(Compare: TObjectSortCompare);
begin
  if Sorted then
    {$IFDEF COMPILER3_UP}
    Error(SSortedListError, 0);
    {$ELSE}
    raise EListError.Create(LoadStr(SSortedListError));
    {$ENDIF}
  if Count > 0 then
  begin
    BeginUpdate;
    try
      QuickSort(Self, 0, Count - 1, Compare);
    finally
      EndUpdate;
    end;
  end;
end;

//=== TJvHugeList ============================================================

{$IFNDEF WIN32}

function ReturnAddr: Pointer; assembler;
asm
        MOV     AX,[BP].Word[2]
        MOV     DX,[BP].Word[4]
end;

procedure ListError(Index: Longint);
begin
  raise EListError.Create(LoadStr(SListIndexError) +
    Format(' (%d)', [Index]))at ReturnAddr;
end;

destructor TJvHugeList.Destroy;
begin
  Clear;
end;

function TJvHugeList.Add(Item: Pointer): Longint;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList.Position := Result * SizeOf(Pointer);
  FList.WriteBuffer(Item, SizeOf(Pointer));
  Inc(FCount);
end;

procedure TJvHugeList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TJvHugeList.Delete(Index: Longint);
begin
  if (Index < 0) or (Index >= FCount) then
    ListError(Index);
  Dec(FCount);
  if Index < FCount then
    HugeMove(FList.Memory, Index, Index + 1, FCount - Index);
end;

function TJvHugeList.Get(Index: Longint): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    ListError(Index);
  FList.Position := Index * SizeOf(Pointer);
  FList.ReadBuffer(Result, SizeOf(Pointer));
end;

procedure TJvHugeList.Put(Index: Longint; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    ListError(Index);
  FList.Position := Index * SizeOf(Pointer);
  FList.WriteBuffer(Item, SizeOf(Pointer));
end;

procedure TJvHugeList.Exchange(Index1, Index2: Longint);
var
  Item: Pointer;
begin
  Item := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, Item);
end;

function TJvHugeList.Expand: TJvHugeList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TJvHugeList.First: Pointer;
begin
  Result := Get(0);
end;

procedure TJvHugeList.Grow;
var
  Delta: Longint;
begin
  // (rom) maybe some levels more here
  if FCapacity > 8 then
    Delta := 16
  else
  if FCapacity > 4 then
    Delta := 8
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TJvHugeList.IndexOf(Item: Pointer): Longint;
begin
  Result := 0;
  while (Result < FCount) and (Get(Result) <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TJvHugeList.Insert(Index: Longint; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    ListError(Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    HugeMove(FList.Memory, Index + 1, Index, FCount - Index);
  FList.Position := Index * SizeOf(Pointer);
  FList.WriteBuffer(Item, SizeOf(Pointer));
  Inc(FCount);
end;

function TJvHugeList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TJvHugeList.Move(CurIndex, NewIndex: Longint);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      ListError(NewIndex);
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

function TJvHugeList.Remove(Item: Pointer): Longint;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end;

procedure TJvHugeList.Pack;
var
  I: Longint;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then
      Delete(I);
end;

procedure TJvHugeList.SetCapacity(NewCapacity: Longint);
var
  NewList: TMemoryStream;
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxHugeListSize) then
    ListError(NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    if NewCapacity = 0 then
      NewList := nil
    else
    begin
      NewList := TMemoryStream.Create;
      NewList.SetSize(NewCapacity * SizeOf(Pointer));
      if FCount <> 0 then
      begin
        FList.Position := 0;
        FList.ReadBuffer(NewList.Memory^, FCount * SizeOf(Pointer));
      end;
    end;
    if FCapacity <> 0 then
      FList.Free;
    FList := NewList;
    FCapacity := NewCapacity;
  end;
end;

procedure TJvHugeList.SetCount(NewCount: Longint);
begin
  if (NewCount < 0) or (NewCount > MaxHugeListSize) then
    ListError(NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  FCount := NewCount;
end;

{$ENDIF}

//=== TJvSortCollection ======================================================

{$IFDEF WIN32}

procedure TJvSortCollection.QuickSort(L, R: Integer; Compare: TItemSortCompare);
var
  I, J: Integer;
  P, P1, P2: TCollectionItem;
begin
  repeat
    I := L;
    J := R;
    P := Items[(L + R) shr 1];
    repeat
      while Compare(Items[I], P) < 0 do
        Inc(I);
      while Compare(Items[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        P1 := Items[I];
        P2 := Items[J];
        P1.Index := J;
        P2.Index := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

procedure TJvSortCollection.Sort(Compare: TItemSortCompare);
begin
  if Count > 0 then
  begin
    BeginUpdate;
    try
      QuickSort(0, Count - 1, Compare);
    finally
      EndUpdate;
    end;
  end;
end;

{$ENDIF WIN32}

end.

