{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Classes.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_Classes;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  Classes;

{ TList }
{ constructor }

procedure TList_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TList.Create);
end;

{ function Add(Item: Pointer): Integer; }

procedure TList_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TList(Args.Obj).Add(V2P(Args.Values[0]));
end;

{ procedure Clear; }

procedure TList_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TList(Args.Obj).Clear;
end;

{ procedure Delete(Index: Integer); }

procedure TList_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TList(Args.Obj).Delete(Args.Values[0]);
end;

{ procedure Exchange(Index1, Index2: Integer); }

procedure TList_Exchange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TList(Args.Obj).Exchange(Args.Values[0], Args.Values[1]);
end;

{ function Expand: TList; }

procedure TList_Expand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TList(Args.Obj).Expand);
end;

{ function First: Pointer; }

procedure TList_First(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TList(Args.Obj).First);
end;

{ function IndexOf(Item: Pointer): Integer; }

procedure TList_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TList(Args.Obj).IndexOf(V2P(Args.Values[0]));
end;

{ procedure Insert(Index: Integer; Item: Pointer); }

procedure TList_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TList(Args.Obj).Insert(Args.Values[0], V2P(Args.Values[1]));
end;

{ function Last: Pointer; }

procedure TList_Last(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TList(Args.Obj).Last);
end;

{ procedure Move(CurIndex, NewIndex: Integer); }

procedure TList_Move(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TList(Args.Obj).Move(Args.Values[0], Args.Values[1]);
end;

{ function Remove(Item: Pointer): Integer; }

procedure TList_Remove(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TList(Args.Obj).Remove(V2P(Args.Values[0]));
end;

{ procedure Pack; }

procedure TList_Pack(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TList(Args.Obj).Pack;
end;

{ procedure Sort(Compare: TListSortCompare); }

procedure TList_Sort(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  TList(Args.Obj).Sort(Args.Values[0]);
  NotImplemented('TList.Sort');
end;

{ property Read Capacity: Integer }

procedure TList_Read_Capacity(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TList(Args.Obj).Capacity;
end;

{ property Write Capacity(Value: Integer) }

procedure TList_Write_Capacity(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TList(Args.Obj).Capacity := Value;
end;

{ property Read Count: Integer }

procedure TList_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TList(Args.Obj).Count;
end;

{ property Write Count(Value: Integer) }

procedure TList_Write_Count(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TList(Args.Obj).Count := Value;
end;

{ property Read Items[Integer]: Pointer }

procedure TList_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TList(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: Pointer }

procedure TList_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TList(Args.Obj).Items[Args.Values[0]] := V2P(Value);
end;

{ property Read List: PPointerList }

procedure TList_Read_List(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TList(Args.Obj).List);
end;

{ TPersistent }

{ procedure Assign(Source: TPersistent); }

procedure TPersistent_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TPersistent(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ function GetNamePath: string; }

procedure TPersistent_GetNamePath(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPersistent(Args.Obj).GetNamePath;
end;

{ TCollectionItem }

{ constructor Create(Collection: TCollection) }

procedure TCollectionItem_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCollectionItem.Create(V2O(Args.Values[0]) as TCollection));
end;

{ property Read Collection: TCollection }

procedure TCollectionItem_Read_Collection(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCollectionItem(Args.Obj).Collection);
end;

{ property Write Collection(Value: TCollection) }

procedure TCollectionItem_Write_Collection(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCollectionItem(Args.Obj).Collection := V2O(Value) as TCollection;
end;

{ property Read ID: Integer }

procedure TCollectionItem_Read_ID(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCollectionItem(Args.Obj).ID;
end;

{ property Read Index: Integer }

procedure TCollectionItem_Read_Index(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCollectionItem(Args.Obj).Index;
end;

{ property Write Index(Value: Integer) }

procedure TCollectionItem_Write_Index(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCollectionItem(Args.Obj).Index := Value;
end;

{ property Read DisplayName: string }

procedure TCollectionItem_Read_DisplayName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCollectionItem(Args.Obj).DisplayName;
end;

{ property Write DisplayName(Value: string) }

procedure TCollectionItem_Write_DisplayName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCollectionItem(Args.Obj).DisplayName := Value;
end;

{ TCollection }

{ constructor Create(ItemClass: TCollectionItemClass) }

procedure TCollection_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCollection.Create(TCollectionItemClass(V2O(Args.Values[0]))));
end;

{ function Add: TCollectionItem; }

procedure TCollection_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCollection(Args.Obj).Add);
end;

{ procedure Assign(Source: TPersistent); }

procedure TCollection_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCollection(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ procedure BeginUpdate; }

procedure TCollection_BeginUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCollection(Args.Obj).BeginUpdate;
end;

{ procedure Clear; }

procedure TCollection_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCollection(Args.Obj).Clear;
end;

{ procedure EndUpdate; }

procedure TCollection_EndUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCollection(Args.Obj).EndUpdate;
end;

{ function FindItemID(ID: Integer): TCollectionItem; }

procedure TCollection_FindItemID(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCollection(Args.Obj).FindItemID(Args.Values[0]));
end;

{ property Read Count: Integer }

procedure TCollection_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCollection(Args.Obj).Count;
end;

{ property Read ItemClass: TCollectionItemClass }

procedure TCollection_Read_ItemClass(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TObject(TCollection(Args.Obj).ItemClass));
end;

{ property Read Items[Integer]: TCollectionItem }

procedure TCollection_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCollection(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: TCollectionItem }

procedure TCollection_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCollection(Args.Obj).Items[Args.Values[0]] := V2O(Value) as TCollectionItem;
end;

{ TStrings }

{ function Add(const S: string): Integer; }

procedure TStrings_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).Add(Args.Values[0]);
end;

{ function AddObject(const S: string; AObject: TObject): Integer; }

procedure TStrings_AddObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).AddObject(Args.Values[0], V2O(Args.Values[1]));
end;

{ procedure Append(const S: string); }

procedure TStrings_Append(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Append(Args.Values[0]);
end;

{ procedure AddStrings(Strings: TStrings); }

procedure TStrings_AddStrings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).AddStrings(V2O(Args.Values[0]) as TStrings);
end;

{ procedure Assign(Source: TPersistent); }

procedure TStrings_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ procedure BeginUpdate; }

procedure TStrings_BeginUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).BeginUpdate;
end;

{ procedure Clear; }

procedure TStrings_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Clear;
end;

{ procedure Delete(Index: Integer); }

procedure TStrings_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Delete(Args.Values[0]);
end;

{ procedure EndUpdate; }

procedure TStrings_EndUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).EndUpdate;
end;

{ function Equals(Strings: TStrings): Boolean; }

procedure TStrings_Equals(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).Equals(V2O(Args.Values[0]) as TStrings);
end;

{ procedure Exchange(Index1, Index2: Integer); }

procedure TStrings_Exchange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Exchange(Args.Values[0], Args.Values[1]);
end;

{ function IndexOf(const S: string): Integer; }

procedure TStrings_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).IndexOf(Args.Values[0]);
end;

{ function IndexOfName(const Name: string): Integer; }

procedure TStrings_IndexOfName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).IndexOfName(Args.Values[0]);
end;

{ function IndexOfObject(AObject: TObject): Integer; }

procedure TStrings_IndexOfObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).IndexOfObject(V2O(Args.Values[0]));
end;

{ procedure Insert(Index: Integer; const S: string); }

procedure TStrings_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Insert(Args.Values[0], Args.Values[1]);
end;

{ procedure InsertObject(Index: Integer; const S: string; AObject: TObject); }

procedure TStrings_InsertObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).InsertObject(Args.Values[0], Args.Values[1], V2O(Args.Values[2]));
end;

{ procedure LoadFromFile(const FileName: string); }

procedure TStrings_LoadFromFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).LoadFromFile(Args.Values[0]);
end;

{ procedure LoadFromStream(Stream: TStream); }

procedure TStrings_LoadFromStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).LoadFromStream(V2O(Args.Values[0]) as TStream);
end;

{ procedure Move(CurIndex, NewIndex: Integer); }

procedure TStrings_Move(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Move(Args.Values[0], Args.Values[1]);
end;

{ procedure SaveToFile(const FileName: string); }

procedure TStrings_SaveToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).SaveToFile(Args.Values[0]);
end;

{ procedure SaveToStream(Stream: TStream); }

procedure TStrings_SaveToStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).SaveToStream(V2O(Args.Values[0]) as TStream);
end;

{ property Read Capacity: Integer }

procedure TStrings_Read_Capacity(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).Capacity;
end;

{ property Write Capacity(Value: Integer) }

procedure TStrings_Write_Capacity(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Capacity := Value;
end;

{ property Read CommaText: string }

procedure TStrings_Read_CommaText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).CommaText;
end;

{ property Write CommaText(Value: string) }

procedure TStrings_Write_CommaText(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).CommaText := Value;
end;

{ property Read Count: Integer }

procedure TStrings_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).Count;
end;

{ property Read Names[Integer]: string }

procedure TStrings_Read_Names(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).Names[Args.Values[0]];
end;

{ property Read Values[Integer]: string }

procedure TStrings_Read_Values(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).Values[Args.Values[0]];
end;

{ property Write Values[Integer]: string }// ivan_ra

procedure TStrings_Write_Values(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Values[Args.Values[0]] := Value;
end;

{ property Read Objects[Integer]: TObject }

procedure TStrings_Read_Objects(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStrings(Args.Obj).Objects[Args.Values[0]]);
end;

{ property Write Objects[Integer]: TObject }

procedure TStrings_Write_Objects(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Objects[Args.Values[0]] := V2O(Value);
end;

{ property Read Strings[Integer]: string }

procedure TStrings_Read_Strings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).Strings[Args.Values[0]];
end;

{ property Write Strings[Integer]: string }

procedure TStrings_Write_Strings(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Strings[Args.Values[0]] := Value;
end;

{ property Read Text: string }

procedure TStrings_Read_Text(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).Text;
end;

{ property Write Text(Value: string) }

procedure TStrings_Write_Text(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStrings(Args.Obj).Text := Value;
end;

{ property Read StringsAdapter: IStringsAdapter }

procedure TStrings_Read_StringsAdapter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStrings(Args.Obj).StringsAdapter;
end;

{ property Write StringsAdapter(Value: IStringsAdapter) }

procedure TStrings_Write_StringsAdapter(const Value: Variant; Args: TJvInterpreterArgs);
begin
//  TStrings(Args.Obj).StringsAdapter := Value;
  NotImplemented('TStrings.StringsAdapter');
end;

{ TStringList }

{ constructor }

procedure TStringList_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStringList.Create);
end;

{ function Add(const S: string): Integer; }

procedure TStringList_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringList(Args.Obj).Add(Args.Values[0]);
end;

{ procedure Clear; }

procedure TStringList_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringList(Args.Obj).Clear;
end;

{ procedure Delete(Index: Integer); }

procedure TStringList_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringList(Args.Obj).Delete(Args.Values[0]);
end;

{ procedure Exchange(Index1, Index2: Integer); }

procedure TStringList_Exchange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringList(Args.Obj).Exchange(Args.Values[0], Args.Values[1]);
end;

{ function Find(const S: string; var Index: Integer): Boolean; }

procedure TStringList_Find(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringList(Args.Obj).Find(Args.Values[0], TVarData(Args.Values[1]).vInteger);
end;

{ function IndexOf(const S: string): Integer; }

procedure TStringList_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringList(Args.Obj).IndexOf(Args.Values[0]);
end;

{ procedure Insert(Index: Integer; const S: string); }

procedure TStringList_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringList(Args.Obj).Insert(Args.Values[0], Args.Values[1]);
end;

{ procedure Sort; }

procedure TStringList_Sort(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringList(Args.Obj).Sort;
end;

{ property Read Duplicates: TDuplicates }

procedure TStringList_Read_Duplicates(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringList(Args.Obj).Duplicates;
end;

{ property Write Duplicates(Value: TDuplicates) }

procedure TStringList_Write_Duplicates(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringList(Args.Obj).Duplicates := Value;
end;

{ property Read Sorted: Boolean }

procedure TStringList_Read_Sorted(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringList(Args.Obj).Sorted;
end;

{ property Write Sorted(Value: Boolean) }

procedure TStringList_Write_Sorted(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringList(Args.Obj).Sorted := Value;
end;

{ TStream }

{ function Read(var Buffer; Count: Longint): Longint; }

procedure TStream_Read(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStream(Args.Obj).Read(Args.Values[0], Args.Values[1]);
end;

{ function Write(const Buffer; Count: Longint): Longint; }

procedure TStream_Write(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStream(Args.Obj).Write(Args.Values[0], Args.Values[1]);
end;

{ function Seek(Offset: Longint; Origin: Word): Longint; }

procedure TStream_Seek(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStream(Args.Obj).Seek(Args.Values[0], Args.Values[1]);
end;

{ procedure ReadBuffer(var Buffer; Count: Longint); }

procedure TStream_ReadBuffer(var Value: Variant; Args: TJvInterpreterArgs);
var
  P: PChar;
  S: String;
begin
  SetLength(S, Integer(Args.Values[1]));
  P := PChar(S);
  TStream(Args.Obj).ReadBuffer(P^, Args.Values[1]);
  Args.Values[0] := S;
end;

{ procedure WriteBuffer(const Buffer; Count: Longint); }

procedure TStream_WriteBuffer(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStream(Args.Obj).WriteBuffer(Args.Values[0], Args.Values[1]);
end;

{ function CopyFrom(Source: TStream; Count: Longint): Longint; }

procedure TStream_CopyFrom(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStream(Args.Obj).CopyFrom(V2O(Args.Values[0]) as TStream, Args.Values[1]);
end;

{ function ReadComponent(Instance: TComponent): TComponent; }

procedure TStream_ReadComponent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStream(Args.Obj).ReadComponent(V2O(Args.Values[0]) as TComponent));
end;

{ function ReadComponentRes(Instance: TComponent): TComponent; }

procedure TStream_ReadComponentRes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStream(Args.Obj).ReadComponentRes(V2O(Args.Values[0]) as TComponent));
end;

{ procedure WriteComponent(Instance: TComponent); }

procedure TStream_WriteComponent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStream(Args.Obj).WriteComponent(V2O(Args.Values[0]) as TComponent);
end;

{ procedure WriteComponentRes(const ResName: string; Instance: TComponent); }

procedure TStream_WriteComponentRes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStream(Args.Obj).WriteComponentRes(Args.Values[0], V2O(Args.Values[1]) as TComponent);
end;

{ procedure WriteDescendent(Instance, Ancestor: TComponent); }

procedure TStream_WriteDescendent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStream(Args.Obj).WriteDescendent(V2O(Args.Values[0]) as TComponent, V2O(Args.Values[1]) as TComponent);
end;

{ procedure WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent); }

procedure TStream_WriteDescendentRes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStream(Args.Obj).WriteDescendentRes(Args.Values[0], V2O(Args.Values[1]) as TComponent, V2O(Args.Values[2]) as
    TComponent);
end;

{ procedure ReadResHeader; }

procedure TStream_ReadResHeader(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStream(Args.Obj).ReadResHeader;
end;

{ property Read Position: Longint }

procedure TStream_Read_Position(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStream(Args.Obj).Position;
end;

{ property Write Position(Value: Longint) }

procedure TStream_Write_Position(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStream(Args.Obj).Position := Value;
end;

{ property Read Size: Longint }

procedure TStream_Read_Size(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStream(Args.Obj).Size;
end;

{ property Write Size(Value: Longint) }

procedure TStream_Write_Size(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStream(Args.Obj).Size := Value;
end;

{ TFileStream }

{ constructor Create(FileName: string; Mode: Word) }

procedure TFileStream_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF MSWINDOWS}
  Value := O2V(TFileStream.Create(Args.Values[0], Args.Values[1]));
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Value := O2V(TFileStream.Create(VarToStr(Args.Values[0]), Args.Values[1]));
  {$ENDIF LINUX}
end;

{ TMemoryStream }

{ constructor Create }

procedure TMemoryStream_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TMemoryStream.Create);
end;

{ TJvStringStream  }

{ constructor Create(AString: string) }

procedure TStringStream_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStringStream.Create(Args.Values[0]));
end;

{ function Read(var Buffer; Count: Longint): Longint; }

procedure TStringStream_Read(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringStream(Args.Obj).Read(Args.Values[0], Args.Values[1]);
end;

{ function ReadString(Count: Longint): string; }

procedure TStringStream_ReadString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringStream(Args.Obj).ReadString(Args.Values[0]);
end;

{ function Seek(Offset: Longint; Origin: Word): Longint; }

procedure TStringStream_Seek(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringStream(Args.Obj).Seek(Args.Values[0], Args.Values[1]);
end;

{ function Write(const Buffer; Count: Longint): Longint; }

procedure TStringStream_Write(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringStream(Args.Obj).Write(Args.Values[0], Args.Values[1]);
end;

{ procedure WriteString(const AString: string); }

procedure TStringStream_WriteString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStringStream(Args.Obj).WriteString(Args.Values[0]);
end;

{ property Read DataString: string }

procedure TStringStream_Read_DataString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStringStream(Args.Obj).DataString;
end;

{ TComponent }

{ constructor Create(AOwner: TComponent) }

procedure TComponent_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TComponent.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure DestroyComponents; }

procedure TComponent_DestroyComponents(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).DestroyComponents;
end;

{ procedure Destroying; }

procedure TComponent_Destroying(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).Destroying;
end;

{ function FindComponent(const AName: string): TComponent; }

procedure TComponent_FindComponent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TComponent(Args.Obj).FindComponent(Args.Values[0]));
end;

{ procedure FreeNotification(AComponent: TComponent); }

procedure TComponent_FreeNotification(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).FreeNotification(V2O(Args.Values[0]) as TComponent);
end;

{ procedure FreeOnRelease; }

procedure TComponent_FreeOnRelease(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).FreeOnRelease;
end;

{ function GetParentComponent: TComponent; }

procedure TComponent_GetParentComponent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TComponent(Args.Obj).GetParentComponent);
end;

{ function HasParent: Boolean; }

procedure TComponent_HasParent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponent(Args.Obj).HasParent;
end;

{ procedure InsertComponent(AComponent: TComponent); }

procedure TComponent_InsertComponent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).InsertComponent(V2O(Args.Values[0]) as TComponent);
end;

{ procedure RemoveComponent(AComponent: TComponent); }

procedure TComponent_RemoveComponent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).RemoveComponent(V2O(Args.Values[0]) as TComponent);
end;

{ function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): Integer; }

procedure TComponent_SafeCallException(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponent(Args.Obj).SafeCallException(V2O(Args.Values[0]), V2P(Args.Values[1]));
end;

{ property Read ComObject: IUnknown }

procedure TComponent_Read_ComObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponent(Args.Obj).ComObject;
end;

{ property Read Components[Integer]: TComponent }

procedure TComponent_Read_Components(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TComponent(Args.Obj).Components[Args.Values[0]]);
end;

{ property Read ComponentCount: Integer }

procedure TComponent_Read_ComponentCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponent(Args.Obj).ComponentCount;
end;

{ property Read ComponentIndex: Integer }

procedure TComponent_Read_ComponentIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponent(Args.Obj).ComponentIndex;
end;

{ property Write ComponentIndex(Value: Integer) }

procedure TComponent_Write_ComponentIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).ComponentIndex := Value;
end;

{ property Read ComponentState: TComponentState }

procedure TComponent_Read_ComponentState(var Value: Variant; Args: TJvInterpreterArgs);
begin
  NotImplemented('TComponent.ComponentState');
  // Value := TComponent(Args.Obj).ComponentState;
end;

{ property Read ComponentStyle: TComponentStyle }

procedure TComponent_Read_ComponentStyle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  NotImplemented('TComponent.ComponentState');
  // Value := TComponent(Args.Obj).ComponentStyle;
end;

{ property Read DesignInfo: Longint }

procedure TComponent_Read_DesignInfo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponent(Args.Obj).DesignInfo;
end;

{ property Write DesignInfo(Value: Longint) }

procedure TComponent_Write_DesignInfo(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).DesignInfo := Value;
end;

{ property Read Owner: TComponent }

procedure TComponent_Read_Owner(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TComponent(Args.Obj).Owner);
end;

{ property Read VCLComObject: Pointer }

procedure TComponent_Read_VCLComObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TComponent(Args.Obj).VCLComObject);
end;

{ property Write VCLComObject(Value: Pointer) }

procedure TComponent_Write_VCLComObject(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).VCLComObject := V2P(Value);
end;

{ property Read Name: TComponentName }

procedure TComponent_Read_Name(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponent(Args.Obj).Name;
end;

{ property Write Name(Value: TComponentName) }

procedure TComponent_Write_Name(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).Name := Value;
end;

{ property Read Tag: Longint }

procedure TComponent_Read_Tag(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponent(Args.Obj).Tag;
end;

{ property Write Tag(Value: Longint) }

procedure TComponent_Write_Tag(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponent(Args.Obj).Tag := Value;
end;

type
  TJvInterpreterClassesEvent = class(TJvInterpreterEvent)
  private
    procedure NotifyEvent(Sender: TObject);
    function HelpEvent(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
  end;

procedure TJvInterpreterClassesEvent.NotifyEvent(Sender: TObject);
begin
  CallFunction(nil, [O2V(Sender)]);
end;

function TJvInterpreterClassesEvent.HelpEvent(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
begin
  Result := CallFunction(nil, [Command, Data, CallHelp]);
  CallHelp := Args.Values[2];
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cClasses = 'Classes';
begin
  with JvInterpreterAdapter do
  begin
    { TAlignment }
    AddConst(cClasses, 'taLeftJustify', Ord(taLeftJustify));
    AddConst(cClasses, 'taRightJustify', Ord(taRightJustify));
    AddConst(cClasses, 'taCenter', Ord(taCenter));
    { TShiftState }
    AddConst(cClasses, 'ssShift', Ord(ssShift));
    AddConst(cClasses, 'ssAlt', Ord(ssAlt));
    AddConst(cClasses, 'ssCtrl', Ord(ssCtrl));
    AddConst(cClasses, 'ssLeft', Ord(ssLeft));
    AddConst(cClasses, 'ssRight', Ord(ssRight));
    AddConst(cClasses, 'ssMiddle', Ord(ssMiddle));
    AddConst(cClasses, 'ssDouble', Ord(ssDouble));
    { TList }
    AddClass(cClasses, TList, 'TList');
    AddGet(TList, 'Create', TList_Create, 0, [varEmpty], varEmpty);
    AddGet(TList, 'Add', TList_Add, 1, [varEmpty], varEmpty);
    AddGet(TList, 'Clear', TList_Clear, 0, [varEmpty], varEmpty);
    AddGet(TList, 'Delete', TList_Delete, 1, [varEmpty], varEmpty);
    AddGet(TList, 'Exchange', TList_Exchange, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TList, 'Expand', TList_Expand, 0, [varEmpty], varEmpty);
    AddGet(TList, 'First', TList_First, 0, [varEmpty], varEmpty);
    AddGet(TList, 'IndexOf', TList_IndexOf, 1, [varEmpty], varEmpty);
    AddGet(TList, 'Insert', TList_Insert, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TList, 'Last', TList_Last, 0, [varEmpty], varEmpty);
    AddGet(TList, 'Move', TList_Move, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TList, 'Remove', TList_Remove, 1, [varEmpty], varEmpty);
    AddGet(TList, 'Pack', TList_Pack, 0, [varEmpty], varEmpty);
    AddGet(TList, 'Sort', TList_Sort, 1, [varEmpty], varEmpty);
    AddGet(TList, 'Capacity', TList_Read_Capacity, 0, [varEmpty], varEmpty);
    AddSet(TList, 'Capacity', TList_Write_Capacity, 0, [varEmpty]);
    AddGet(TList, 'Count', TList_Read_Count, 0, [varEmpty], varEmpty);
    AddSet(TList, 'Count', TList_Write_Count, 0, [varEmpty]);
    AddIGet(TList, 'Items', TList_Read_Items, 1, [varEmpty], varEmpty);
    AddIDGet(TList, TList_Read_Items, 1, [varEmpty], varEmpty);
    AddISet(TList, 'Items', TList_Write_Items, 1, [varNull]);
    AddIDSet(TList, TList_Write_Items, 1, [varNull]);
    AddGet(TList, 'List', TList_Read_List, 0, [varEmpty], varEmpty);
    { TPersistent }
    AddClass(cClasses, TPersistent, 'TPersistent');
    AddGet(TPersistent, 'Assign', TPersistent_Assign, 1, [varEmpty], varEmpty);
    AddGet(TPersistent, 'GetNamePath', TPersistent_GetNamePath, 0, [varEmpty], varEmpty);
    { TCollectionItem }
    AddClass(cClasses, TCollectionItem, 'TCollectionItem');
    AddGet(TCollectionItem, 'Create', TCollectionItem_Create, 1, [varEmpty], varEmpty);
    AddGet(TCollectionItem, 'Collection', TCollectionItem_Read_Collection, 0, [varEmpty], varEmpty);
    AddSet(TCollectionItem, 'Collection', TCollectionItem_Write_Collection, 0, [varEmpty]);
    AddGet(TCollectionItem, 'ID', TCollectionItem_Read_ID, 0, [varEmpty], varEmpty);
    AddGet(TCollectionItem, 'Index', TCollectionItem_Read_Index, 0, [varEmpty], varEmpty);
    AddSet(TCollectionItem, 'Index', TCollectionItem_Write_Index, 0, [varEmpty]);
    AddGet(TCollectionItem, 'DisplayName', TCollectionItem_Read_DisplayName, 0, [varEmpty], varEmpty);
    AddSet(TCollectionItem, 'DisplayName', TCollectionItem_Write_DisplayName, 0, [varEmpty]);
    { TCollection }
    AddClass(cClasses, TCollection, 'TCollection');
    AddGet(TCollection, 'Create', TCollection_Create, 1, [varEmpty], varEmpty);
    AddGet(TCollection, 'Add', TCollection_Add, 0, [varEmpty], varEmpty);
    AddGet(TCollection, 'Assign', TCollection_Assign, 1, [varEmpty], varEmpty);
    AddGet(TCollection, 'BeginUpdate', TCollection_BeginUpdate, 0, [varEmpty], varEmpty);
    AddGet(TCollection, 'Clear', TCollection_Clear, 0, [varEmpty], varEmpty);
    AddGet(TCollection, 'EndUpdate', TCollection_EndUpdate, 0, [varEmpty], varEmpty);
    AddGet(TCollection, 'FindItemID', TCollection_FindItemID, 1, [varEmpty], varEmpty);
    AddGet(TCollection, 'Count', TCollection_Read_Count, 0, [varEmpty], varEmpty);
    AddGet(TCollection, 'ItemClass', TCollection_Read_ItemClass, 0, [varEmpty], varEmpty);
    AddIGet(TCollection, 'Items', TCollection_Read_Items, 1, [varEmpty], varEmpty);
    AddIDGet(TCollection, TCollection_Read_Items, 1, [varEmpty], varEmpty);
    AddISet(TCollection, 'Items', TCollection_Write_Items, 1, [varNull]);
    AddIDSet(TCollection, TCollection_Write_Items, 1, [varNull]);
    { TStrings }
    AddClass(cClasses, TStrings, 'TStrings');
    AddGet(TStrings, 'Add', TStrings_Add, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'AddObject', TStrings_AddObject, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStrings, 'Append', TStrings_Append, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'AddStrings', TStrings_AddStrings, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'Assign', TStrings_Assign, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'BeginUpdate', TStrings_BeginUpdate, 0, [varEmpty], varEmpty);
    AddGet(TStrings, 'Clear', TStrings_Clear, 0, [varEmpty], varEmpty);
    AddGet(TStrings, 'Delete', TStrings_Delete, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'EndUpdate', TStrings_EndUpdate, 0, [varEmpty], varEmpty);
    AddGet(TStrings, 'Equals', TStrings_Equals, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'Exchange', TStrings_Exchange, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStrings, 'IndexOf', TStrings_IndexOf, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'IndexOfName', TStrings_IndexOfName, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'IndexOfObject', TStrings_IndexOfObject, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'Insert', TStrings_Insert, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStrings, 'InsertObject', TStrings_InsertObject, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TStrings, 'LoadFromFile', TStrings_LoadFromFile, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'LoadFromStream', TStrings_LoadFromStream, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'Move', TStrings_Move, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStrings, 'SaveToFile', TStrings_SaveToFile, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'SaveToStream', TStrings_SaveToStream, 1, [varEmpty], varEmpty);
    AddGet(TStrings, 'Capacity', TStrings_Read_Capacity, 0, [varEmpty], varEmpty);
    AddSet(TStrings, 'Capacity', TStrings_Write_Capacity, 0, [varEmpty]);
    AddGet(TStrings, 'CommaText', TStrings_Read_CommaText, 0, [varEmpty], varEmpty);
    AddSet(TStrings, 'CommaText', TStrings_Write_CommaText, 0, [varEmpty]);
    AddGet(TStrings, 'Count', TStrings_Read_Count, 0, [varEmpty], varEmpty);
    AddIGet(TStrings, 'Names', TStrings_Read_Names, 1, [varEmpty], varEmpty);
    AddIGet(TStrings, 'Values', TStrings_Read_Values, 1, [varEmpty], varEmpty);
    AddISet(TStrings, 'Values', TStrings_Write_Values, 1, [varNull]); // ivan_ra
    AddIGet(TStrings, 'Objects', TStrings_Read_Objects, 1, [varEmpty], varEmpty);
    AddISet(TStrings, 'Objects', TStrings_Write_Objects, 1, [varNull]);
    AddIGet(TStrings, 'Strings', TStrings_Read_Strings, 1, [varEmpty], varEmpty);
    AddISet(TStrings, 'Strings', TStrings_Write_Strings, 1, [varNull]);
    AddIDGet(TStrings, TStrings_Read_Strings, 1, [varEmpty], varEmpty);
    AddIDSet(TStrings, TStrings_Write_Strings, 1, [varNull]);
    AddGet(TStrings, 'Text', TStrings_Read_Text, 0, [varEmpty], varEmpty);
    AddSet(TStrings, 'Text', TStrings_Write_Text, 0, [varEmpty]);
    AddGet(TStrings, 'StringsAdapter', TStrings_Read_StringsAdapter, 0, [varEmpty], varEmpty);
    AddSet(TStrings, 'StringsAdapter', TStrings_Write_StringsAdapter, 0, [varEmpty]);
    { TDuplicates }
    AddConst(cClasses, 'dupIgnore', Ord(dupIgnore));
    AddConst(cClasses, 'dupAccept', Ord(dupAccept));
    AddConst(cClasses, 'dupError', Ord(dupError));
    { TStringList }
    AddClass(cClasses, TStringList, 'TStringList');
    AddGet(TStringList, 'Create', TStringList_Create, 0, [varEmpty], varEmpty);
    AddGet(TStringList, 'Add', TStringList_Add, 1, [varEmpty], varEmpty);
    AddGet(TStringList, 'Clear', TStringList_Clear, 0, [varEmpty], varEmpty);
    AddGet(TStringList, 'Delete', TStringList_Delete, 1, [varEmpty], varEmpty);
    AddGet(TStringList, 'Exchange', TStringList_Exchange, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStringList, 'Find', TStringList_Find, 2, [varEmpty, varByRef], varEmpty);
    AddGet(TStringList, 'IndexOf', TStringList_IndexOf, 1, [varEmpty], varEmpty);
    AddGet(TStringList, 'Insert', TStringList_Insert, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStringList, 'Sort', TStringList_Sort, 0, [varEmpty], varEmpty);
    AddGet(TStringList, 'Duplicates', TStringList_Read_Duplicates, 0, [varEmpty], varEmpty);
    AddSet(TStringList, 'Duplicates', TStringList_Write_Duplicates, 0, [varEmpty]);
    AddGet(TStringList, 'Sorted', TStringList_Read_Sorted, 0, [varEmpty], varEmpty);
    AddSet(TStringList, 'Sorted', TStringList_Write_Sorted, 0, [varEmpty]);
    { TStream }
    AddClass(cClasses, TStream, 'TStream');
    AddGet(TStream, 'Read', TStream_Read, 2, [varByRef, varEmpty], varEmpty);
    AddGet(TStream, 'Write', TStream_Write, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStream, 'Seek', TStream_Seek, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStream, 'ReadBuffer', TStream_ReadBuffer, 2, [varByRef, varEmpty], varEmpty);
    AddGet(TStream, 'WriteBuffer', TStream_WriteBuffer, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStream, 'CopyFrom', TStream_CopyFrom, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStream, 'ReadComponent', TStream_ReadComponent, 1, [varEmpty], varEmpty);
    AddGet(TStream, 'ReadComponentRes', TStream_ReadComponentRes, 1, [varEmpty], varEmpty);
    AddGet(TStream, 'WriteComponent', TStream_WriteComponent, 1, [varEmpty], varEmpty);
    AddGet(TStream, 'WriteComponentRes', TStream_WriteComponentRes, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStream, 'WriteDescendent', TStream_WriteDescendent, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStream, 'WriteDescendentRes', TStream_WriteDescendentRes, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TStream, 'ReadResHeader', TStream_ReadResHeader, 0, [varEmpty], varEmpty);
    AddGet(TStream, 'Position', TStream_Read_Position, 0, [varEmpty], varEmpty);
    AddSet(TStream, 'Position', TStream_Write_Position, 0, [varEmpty]);
    AddGet(TStream, 'Size', TStream_Read_Size, 0, [varEmpty], varEmpty);
    AddSet(TStream, 'Size', TStream_Write_Size, 0, [varEmpty]);
    { TFileStream }
    AddClass(cClasses, TFileStream, 'TFileStream');
    AddGet(TFileStream, 'Create', TFileStream_Create, 2, [varEmpty, varEmpty], varEmpty);
    { TMemoryStream }
    AddClass(cClasses, TMemoryStream, 'TMemoryStream');
    AddGet(TMemoryStream, 'Create', TMemoryStream_Create, 0, [varEmpty], varEmpty);
    { TJvStringStream  }
    AddClass(cClasses, TStringStream, 'TStringStream ');
    AddGet(TStringStream, 'Create', TStringStream_Create, 1, [varEmpty], varEmpty);
    AddGet(TStringStream, 'Read', TStringStream_Read, 2, [varByRef, varEmpty], varEmpty);
    AddGet(TStringStream, 'ReadString', TStringStream_ReadString, 1, [varEmpty], varEmpty);
    AddGet(TStringStream, 'Seek', TStringStream_Seek, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStringStream, 'Write', TStringStream_Write, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TStringStream, 'WriteString', TStringStream_WriteString, 1, [varEmpty], varEmpty);
    AddGet(TStringStream, 'DataString', TStringStream_Read_DataString, 0, [varEmpty], varEmpty);
    { TComponentState }
    AddConst(cClasses, 'csLoading', Ord(csLoading));
    AddConst(cClasses, 'csReading', Ord(csReading));
    AddConst(cClasses, 'csWriting', Ord(csWriting));
    AddConst(cClasses, 'csDestroying', Ord(csDestroying));
    AddConst(cClasses, 'csDesigning', Ord(csDesigning));
    AddConst(cClasses, 'csAncestor', Ord(csAncestor));
    AddConst(cClasses, 'csUpdating', Ord(csUpdating));
    AddConst(cClasses, 'csFixups', Ord(csFixups));
    { TComponentStyle }
    AddConst(cClasses, 'csInheritable', Ord(csInheritable));
    AddConst(cClasses, 'csCheckPropAvail', Ord(csCheckPropAvail));
    { TComponent }
    AddClass(cClasses, TComponent, 'TComponent');
    AddGet(TComponent, 'Create', TComponent_Create, 1, [varEmpty], varEmpty);
    AddGet(TComponent, 'DestroyComponents', TComponent_DestroyComponents, 0, [varEmpty], varEmpty);
    AddGet(TComponent, 'Destroying', TComponent_Destroying, 0, [varEmpty], varEmpty);
    AddGet(TComponent, 'FindComponent', TComponent_FindComponent, 1, [varEmpty], varEmpty);
    AddGet(TComponent, 'FreeNotification', TComponent_FreeNotification, 1, [varEmpty], varEmpty);
    AddGet(TComponent, 'FreeOnRelease', TComponent_FreeOnRelease, 0, [varEmpty], varEmpty);
    AddGet(TComponent, 'GetParentComponent', TComponent_GetParentComponent, 0, [varEmpty], varEmpty);
    AddGet(TComponent, 'HasParent', TComponent_HasParent, 0, [varEmpty], varEmpty);
    AddGet(TComponent, 'InsertComponent', TComponent_InsertComponent, 1, [varEmpty], varEmpty);
    AddGet(TComponent, 'RemoveComponent', TComponent_RemoveComponent, 1, [varEmpty], varEmpty);
    AddGet(TComponent, 'SafeCallException', TComponent_SafeCallException, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TComponent, 'ComObject', TComponent_Read_ComObject, 0, [varEmpty], varEmpty);
    AddIGet(TComponent, 'Components', TComponent_Read_Components, 1, [varEmpty], varEmpty);
    AddGet(TComponent, 'ComponentCount', TComponent_Read_ComponentCount, 0, [varEmpty], varEmpty);
    AddGet(TComponent, 'ComponentIndex', TComponent_Read_ComponentIndex, 0, [varEmpty], varEmpty);
    AddSet(TComponent, 'ComponentIndex', TComponent_Write_ComponentIndex, 0, [varEmpty]);
    AddGet(TComponent, 'ComponentState', TComponent_Read_ComponentState, 0, [varEmpty], varEmpty);
    AddGet(TComponent, 'ComponentStyle', TComponent_Read_ComponentStyle, 0, [varEmpty], varEmpty);
    AddGet(TComponent, 'DesignInfo', TComponent_Read_DesignInfo, 0, [varEmpty], varEmpty);
    AddSet(TComponent, 'DesignInfo', TComponent_Write_DesignInfo, 0, [varEmpty]);
    AddGet(TComponent, 'Owner', TComponent_Read_Owner, 0, [varEmpty], varEmpty);
    AddGet(TComponent, 'VCLComObject', TComponent_Read_VCLComObject, 0, [varEmpty], varEmpty);
    AddSet(TComponent, 'VCLComObject', TComponent_Write_VCLComObject, 0, [varEmpty]);
    AddGet(TComponent, 'Name', TComponent_Read_Name, 0, [varEmpty], varEmpty);
    AddSet(TComponent, 'Name', TComponent_Write_Name, 0, [varEmpty]);
    AddGet(TComponent, 'Tag', TComponent_Read_Tag, 0, [varEmpty], varEmpty);
    AddSet(TComponent, 'Tag', TComponent_Write_Tag, 0, [varEmpty]);

    AddHandler(cClasses, 'TNotifyEvent', TJvInterpreterClassesEvent, @TJvInterpreterClassesEvent.NotifyEvent);
    AddHandler(cClasses, 'THelpEvent', TJvInterpreterClassesEvent, @TJvInterpreterClassesEvent.HelpEvent);
  end;
  RegisterClasses([TPersistent, TCollection, TCollectionItem, TStrings,
    TStringList, TComponent]);
end;

end.

