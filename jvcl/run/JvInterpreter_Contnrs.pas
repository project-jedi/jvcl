{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Contnrs.PAS, released on 2002-07-04.

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

unit JvInterpreter_Contnrs;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  Classes, Contnrs;

{ TObjectList }

{ constructor Create }

procedure TObjectList_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TObjectList.Create);
end;

{ constructor Create(AOwnsObjects: Boolean) }

procedure TObjectList_CreateOwns(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TObjectList.Create(Args.Values[0]));
end;

{ function Add(AObject: TObject): Integer; }

procedure TObjectList_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObjectList(Args.Obj).Add(V2O(Args.Values[0]));
end;

{ function Remove(AObject: TObject): Integer; }

procedure TObjectList_Remove(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObjectList(Args.Obj).Remove(V2O(Args.Values[0]));
end;

{ function IndexOf(AObject: TObject): Integer; }

procedure TObjectList_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObjectList(Args.Obj).IndexOf(V2O(Args.Values[0]));
end;

{ function FindInstanceOf(AClass: TClass; AExact: Boolean = True; AStartAt: Integer = 0): Integer; }

procedure TObjectList_FindInstanceOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObjectList(Args.Obj).FindInstanceOf(V2C(Args.Values[0]), Args.Values[1], Args.Values[2]);
end;

{ procedure Insert(Index: Integer; AObject: TObject); }

procedure TObjectList_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TObjectList(Args.Obj).Insert(Args.Values[0], V2O(Args.Values[1]));
end;

{ property Read OwnsObjects: Boolean }

procedure TObjectList_Read_OwnsObjects(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TObjectList(Args.Obj).OwnsObjects;
end;

{ property Write OwnsObjects(Value: Boolean) }

procedure TObjectList_Write_OwnsObjects(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TObjectList(Args.Obj).OwnsObjects := Value;
end;

{ property Read Items[Integer]: TObject }

procedure TObjectList_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TObjectList(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: TObject }

procedure TObjectList_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TObjectList(Args.Obj).Items[Args.Values[0]] := V2O(Value);
end;

{ TComponentList }

{ function Add(AComponent: TComponent): Integer; }

procedure TComponentList_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponentList(Args.Obj).Add(V2O(Args.Values[0]) as TComponent);
end;

{ function Remove(AComponent: TComponent): Integer; }

procedure TComponentList_Remove(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponentList(Args.Obj).Remove(V2O(Args.Values[0]) as TComponent);
end;

{ function IndexOf(AComponent: TComponent): Integer; }

procedure TComponentList_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TComponentList(Args.Obj).IndexOf(V2O(Args.Values[0]) as TComponent);
end;

{ procedure Insert(Index: Integer; AComponent: TComponent); }

procedure TComponentList_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponentList(Args.Obj).Insert(Args.Values[0], V2O(Args.Values[1]) as TComponent);
end;

{ property Read Items[Integer]: TComponent }

procedure TComponentList_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TComponentList(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: TComponent }

procedure TComponentList_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TComponentList(Args.Obj).Items[Args.Values[0]] := V2O(Value) as TComponent;
end;

{ TClassList }

{ function Add(aClass: TClass): Integer; }

procedure TClassList_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TClassList(Args.Obj).Add(V2C(Args.Values[0]));
end;

{ function Remove(aClass: TClass): Integer; }

procedure TClassList_Remove(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TClassList(Args.Obj).Remove(V2C(Args.Values[0]));
end;

{ function IndexOf(aClass: TClass): Integer; }

procedure TClassList_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TClassList(Args.Obj).IndexOf(V2C(Args.Values[0]));
end;

{ procedure Insert(Index: Integer; aClass: TClass); }

procedure TClassList_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TClassList(Args.Obj).Insert(Args.Values[0], V2C(Args.Values[1]));
end;

{ property Read Items[Integer]: TClass }

procedure TClassList_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := C2V(TClassList(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: TClass }

procedure TClassList_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TClassList(Args.Obj).Items[Args.Values[0]] := V2C(Value);
end;

{ TOrderedList }

{ function Count: Integer; }

procedure TOrderedList_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOrderedList(Args.Obj).Count;
end;

{ function AtLeast(ACount: Integer): Boolean; }

procedure TOrderedList_AtLeast(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TOrderedList(Args.Obj).AtLeast(Args.Values[0]);
end;

{ procedure Push(AItem: Pointer); }

procedure TOrderedList_Push(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TOrderedList(Args.Obj).Push(V2P(Args.Values[0]));
end;

{ function Pop: Pointer; }

procedure TOrderedList_Pop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TOrderedList(Args.Obj).Pop);
end;

{ function Peek: Pointer; }

procedure TOrderedList_Peek(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TOrderedList(Args.Obj).Peek);
end;

{ TStack }

{ TObjectStack }

{ procedure Push(AObject: TObject); }

procedure TObjectStack_Push(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TObjectStack(Args.Obj).Push(V2O(Args.Values[0]));
end;

{ function Pop: TObject; }

procedure TObjectStack_Pop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TObjectStack(Args.Obj).Pop);
end;

{ function Peek: TObject; }

procedure TObjectStack_Peek(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TObjectStack(Args.Obj).Peek);
end;

{ TQueue }

{ TObjectQueue }

{ procedure Push(AObject: TObject); }

procedure TObjectQueue_Push(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TObjectQueue(Args.Obj).Push(V2O(Args.Values[0]));
end;

{ function Pop: TObject; }

procedure TObjectQueue_Pop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TObjectQueue(Args.Obj).Pop);
end;

{ function Peek: TObject; }

procedure TObjectQueue_Peek(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TObjectQueue(Args.Obj).Peek);
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cContnrs = 'Contnrs';
begin
  with JvInterpreterAdapter do
  begin
    { TObjectList }
    AddClass(cContnrs, TObjectList, 'TObjectList');
    AddGet(TObjectList, 'Create', TObjectList_Create, 0, [varEmpty], varEmpty);
    AddGet(TObjectList, 'CreateOwns', TObjectList_CreateOwns, 1, [varBoolean], varEmpty);
    AddGet(TObjectList, 'Add', TObjectList_Add, 1, [varObject], varEmpty);
    AddGet(TObjectList, 'Remove', TObjectList_Remove, 1, [varObject], varEmpty);
    AddGet(TObjectList, 'IndexOf', TObjectList_IndexOf, 1, [varObject], varEmpty);
    AddGet(TObjectList, 'FindInstanceOf', TObjectList_FindInstanceOf, 3, [varEmpty, varBoolean, varInteger], varEmpty);
    AddGet(TObjectList, 'Insert', TObjectList_Insert, 2, [varInteger, varObject], varEmpty);
    AddGet(TObjectList, 'OwnsObjects', TObjectList_Read_OwnsObjects, 0, [varEmpty], varEmpty);
    AddSet(TObjectList, 'OwnsObjects', TObjectList_Write_OwnsObjects, 0, [varEmpty]);
    AddGet(TObjectList, 'Items', TObjectList_Read_Items, 1, [varEmpty], varEmpty);
    AddSet(TObjectList, 'Items', TObjectList_Write_Items, 1, [varNull]);
    { TComponentList }
    AddClass(cContnrs, TComponentList, 'TComponentList');
    AddGet(TComponentList, 'Add', TComponentList_Add, 1, [varObject], varEmpty);
    AddGet(TComponentList, 'Remove', TComponentList_Remove, 1, [varObject], varEmpty);
    AddGet(TComponentList, 'IndexOf', TComponentList_IndexOf, 1, [varObject], varEmpty);
    AddGet(TComponentList, 'Insert', TComponentList_Insert, 2, [varInteger, varObject], varEmpty);
    AddGet(TComponentList, 'Items', TComponentList_Read_Items, 1, [varEmpty], varEmpty);
    AddSet(TComponentList, 'Items', TComponentList_Write_Items, 1, [varNull]);
    { TClassList }
    AddClass(cContnrs, TClassList, 'TClassList');
    AddGet(TClassList, 'Add', TClassList_Add, 1, [varEmpty], varEmpty);
    AddGet(TClassList, 'Remove', TClassList_Remove, 1, [varEmpty], varEmpty);
    AddGet(TClassList, 'IndexOf', TClassList_IndexOf, 1, [varEmpty], varEmpty);
    AddGet(TClassList, 'Insert', TClassList_Insert, 2, [varInteger, varEmpty], varEmpty);
    AddGet(TClassList, 'Items', TClassList_Read_Items, 1, [varEmpty], varEmpty);
    AddSet(TClassList, 'Items', TClassList_Write_Items, 1, [varNull]);
    { TOrderedList }
    AddClass(cContnrs, TOrderedList, 'TOrderedList');
    AddGet(TOrderedList, 'Count', TOrderedList_Count, 0, [varEmpty], varEmpty);
    AddGet(TOrderedList, 'AtLeast', TOrderedList_AtLeast, 1, [varInteger], varEmpty);
    AddGet(TOrderedList, 'Push', TOrderedList_Push, 1, [varPointer], varEmpty);
    AddGet(TOrderedList, 'Pop', TOrderedList_Pop, 0, [varEmpty], varEmpty);
    AddGet(TOrderedList, 'Peek', TOrderedList_Peek, 0, [varEmpty], varEmpty);
    { TStack }
    AddClass(cContnrs, TStack, 'TStack');
    { TObjectStack }
    AddClass(cContnrs, TObjectStack, 'TObjectStack');
    AddGet(TObjectStack, 'Push', TObjectStack_Push, 1, [varObject], varEmpty);
    AddGet(TObjectStack, 'Pop', TObjectStack_Pop, 0, [varEmpty], varEmpty);
    AddGet(TObjectStack, 'Peek', TObjectStack_Peek, 0, [varEmpty], varEmpty);
    { TQueue }
    AddClass(cContnrs, TQueue, 'TQueue');
    { TObjectQueue }
    AddClass(cContnrs, TObjectQueue, 'TObjectQueue');
    AddGet(TObjectQueue, 'Push', TObjectQueue_Push, 1, [varObject], varEmpty);
    AddGet(TObjectQueue, 'Pop', TObjectQueue_Pop, 0, [varEmpty], varEmpty);
    AddGet(TObjectQueue, 'Peek', TObjectQueue_Peek, 0, [varEmpty], varEmpty);
  end;
end;

end.

