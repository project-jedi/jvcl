{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockHashTable.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002, 2003 luxiaoban.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDockHashTable;

{$I jvcl.inc}

interface

uses
  Classes;

const
  DefaultHashSize = 20;

type
  TJvDockClientHashNode = class(TObject)
  private
    FKeyName: string;                 
    FKeyData: Pointer;                
    FPrevNode: TJvDockClientHashNode;
    FNextNode: TJvDockClientHashNode;
    FListIndex: Integer;              
  public
    property KeyName: string read FKeyName write FKeyName;
    property KeyData: Pointer read FKeyData write FKeyData;
    property PrevNode: TJvDockClientHashNode read FPrevNode write FPrevNode;
    property NextNode: TJvDockClientHashNode read FNextNode write FNextNode;
    property ListIndex: Integer read FListIndex write FListIndex;
  end;

  TJvDockControlHashTable = class(TObject)
  private
    FCurrentSize: Integer;
    FTableSize: Integer;
    FEntryList: TList;       
    FRiseException: Boolean; 
    procedure SetTableSize(const Value: Integer);     
  protected
    function HashProc(const Name: string): Integer; virtual;
    procedure DeleteListIndex(Index: Integer);
    function CreateKeyNode(const KeyName: string; KeyData: Pointer;
      ListIndex: Integer): TJvDockClientHashNode;
    function CompareKey(const Key1, Key2: string): Integer;
  public
    constructor Create(Size: Integer = DefaultHashSize; RiseExcept: Boolean = True); virtual;
    destructor Destroy; override;
    procedure CreateDictionary(Size: Integer); virtual;
    function IsIn(const Name: string): Boolean; virtual;
    function FindNode(const Name: string): TJvDockClientHashNode; virtual;
    function Find(const Name: string): Pointer; virtual;
    function Insert(const Name: string; Data: Pointer): Integer; virtual;
    procedure Remove(const Name: string); virtual;
    procedure MakeEmpty;
    property CurrentSize: Integer read FCurrentSize;
    property TableSize: Integer read FTableSize write SetTableSize;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils,
  JvDockGlobals;

//=== { TJvDockControlHashTable } ============================================

constructor TJvDockControlHashTable.Create(Size: Integer; RiseExcept: Boolean);
begin
  // (rom) added inherited Create
  inherited Create;
  CreateDictionary(Size);
  FRiseException := RiseExcept;
end;

destructor TJvDockControlHashTable.Destroy;
begin
  MakeEmpty;
  FEntryList.Free;
  inherited Destroy;
end;

function TJvDockControlHashTable.CompareKey(const Key1, Key2: string): Integer;
begin
  Result := AnsiStrComp(PChar(Key1), PChar(Key2));
end;

procedure TJvDockControlHashTable.CreateDictionary(Size: Integer);
begin
  // (rom) secured against calling it several times
  if not Assigned(FEntryList) then
  begin
    FEntryList := TList.Create;
    FEntryList.Count := Size;
    FTableSize := Size;
  end;
end;

function TJvDockControlHashTable.CreateKeyNode(const KeyName: string;
  KeyData: Pointer; ListIndex: Integer): TJvDockClientHashNode;
begin
  Result := TJvDockClientHashNode.Create;
  Result.KeyName := KeyName;
  Result.KeyData := KeyData;
  Result.ListIndex := ListIndex;
end;

procedure TJvDockControlHashTable.DeleteListIndex(Index: Integer);
var
  Node, NextNode: TJvDockClientHashNode;
begin
  Node := FEntryList[Index];
  while Node <> nil do
  begin
    NextNode := Node.NextNode;
    Node.Free;
    Node := NextNode;
  end;
  FEntryList.Delete(Index);
end;

function TJvDockControlHashTable.Find(const Name: string): Pointer;
var
  Node: TJvDockClientHashNode;
begin
  Node := FindNode(Name);
  if Node <> nil then
    Result := Node.KeyData
  else
    Result := nil;
end;

function TJvDockControlHashTable.FindNode(const Name: string): TJvDockClientHashNode;
var
  Value: Integer;
  ListIndex: Integer;
begin
  ListIndex := HashProc(Name);
  Assert((ListIndex >= 0) and (ListIndex < FTableSize), RsDockTableIndexError);
  Result := FEntryList[ListIndex];
  if Result <> nil then
    repeat
      Value := CompareKey(Name, Result.FKeyName);
      if Value = 0 then
        Break;
      Result := Result.FNextNode;
    until Result = nil;
end;

function TJvDockControlHashTable.HashProc(const Name: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Name) do
    Inc(Result, Ord(Name[I]));
  Result := Result mod FTableSize;
end;

function TJvDockControlHashTable.Insert(const Name: string; Data: Pointer): Integer;
var
  Index: Integer;
  Value: Integer;
  Node, ParentNode: TJvDockClientHashNode;
begin
  Result := -1;
  Index := HashProc(Name);
  Assert((Index >= 0) and (Index < FTableSize), RsDockTableIndexError);
  if FEntryList[Index] = nil then
    FEntryList[Index] := CreateKeyNode(Name, Data, Index)
  else
  begin
    Node := FEntryList[Index];
    repeat
      Value := CompareKey(Name, Node.FKeyName);

      if FRiseException then
        Assert(Value <> 0, RsDockNodeExistedError)
      else
      if Value = 0 then
        Exit;
      ParentNode := Node;
      Node := Node.FNextNode;
    until Node = nil;
    
    Node := CreateKeyNode(Name, Data, Index);
    Node.FPrevNode := ParentNode;
    ParentNode.NextNode := Node;
  end;
  Result := Index;
end;

function TJvDockControlHashTable.IsIn(const Name: string): Boolean;
begin
  Result := FindNode(Name) <> nil;
end;

procedure TJvDockControlHashTable.MakeEmpty;
var
  I: Integer;
begin
  for I := FEntryList.Count - 1 downto 0 do
    DeleteListIndex(I);
end;

procedure TJvDockControlHashTable.Remove(const Name: string);
var
  Node: TJvDockClientHashNode;
begin
  Node := FindNode(Name);
  if Node <> nil then
  begin
    if Node.FPrevNode <> nil then
      Node.FPrevNode.FNextNode := Node.FNextNode
    else
      FEntryList[Node.ListIndex] := Node.FNextNode;
    if Node.FNextNode <> nil then
      Node.FNextNode.FPrevNode := Node.FPrevNode;
    Node.Free;
  end;
end;

procedure TJvDockControlHashTable.SetTableSize(const Value: Integer);
begin
  FEntryList.Count := Value;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

