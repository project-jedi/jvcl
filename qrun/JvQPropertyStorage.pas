{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProps.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQPropertyStorage;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,  
  QForms, 
  JvQAppStorage, TypInfo;

type
  TJvPropInfoList = class(TObject)
  private
    FList: PPropList;
    FCount: Integer;
    FSize: Integer;
    function Get(Index: Integer): PPropInfo;
  public
    constructor Create(AObject: TObject; Filter: TTypeKinds);
    destructor Destroy; override;
    function Contains(P: PPropInfo): Boolean;
    function Find(const AName: string): PPropInfo;
    procedure Delete(Index: Integer);
    procedure Intersect(List: TJvPropInfoList);
    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get; default;
  end;


  TJvPropertyStorage = class(TObject)
  private
    FObject: TObject;
    FOwner: TComponent;
    FPrefix: string;
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    function CreateInfoList(AComponent: TComponent; StoredList: TStrings): TStrings;
    procedure FreeInfoLists(Info: TStrings);
  protected
    function ReadString(const APath, Item, Default: string): string; virtual;
    procedure WriteString(const APath, Item, Value: string); virtual;

    procedure ReadProperty(const APath, AStorageName: string; const PersObj: TPersistent; const PropName : string);
    procedure WriteProperty(const APath, AStorageName: string; const PersObj: TPersistent; const PropName : string);

    procedure EraseSection(const APath: string); virtual;
    function GetItemName(const APropName: string): string; virtual;
    function CreateStorage: TJvPropertyStorage; virtual;
  public
    procedure StoreAnyProperty(PropInfo: PPropInfo);
    procedure LoadAnyProperty(PropInfo: PPropInfo);
    procedure StoreProperties(PropList: TStrings);
    procedure LoadProperties(PropList: TStrings);
    procedure LoadObjectsProps(AComponent: TComponent; StoredList: TStrings);
    procedure StoreObjectsProps(AComponent: TComponent; StoredList: TStrings);
    property AObject: TObject read FObject write FObject;
    property Prefix: string read FPrefix write FPrefix;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property AppStoragePath: string read FAppStoragePath write FAppStoragePath;
  end;

{ Utility routines }

procedure UpdateStoredList(AComponent: TComponent; AStoredList: TStrings; FromForm: Boolean);
function CreateStoredItem(const CompName, PropName: string): string;
function ParseStoredItem(const Item: string; var CompName, PropName: string): Boolean;

const
  sPropNameDelimiter: string = '_';

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvQJCLUtils;

//=== { TJvPropInfoList } ====================================================

constructor TJvPropInfoList.Create(AObject: TObject; Filter: TTypeKinds);
begin
  inherited Create;
  if AObject <> nil then
  begin
    FCount := GetPropList(AObject.ClassInfo, Filter, nil);
    FSize := FCount * SizeOf(Pointer);
    GetMem(FList, FSize);
    GetPropList(AObject.ClassInfo, Filter, FList);
  end
  else
  begin
    FCount := 0;
    FList := nil;
  end;
end;

destructor TJvPropInfoList.Destroy;
begin
  if FList <> nil then
    FreeMem(FList, FSize);
  inherited Destroy;
end;

function TJvPropInfoList.Contains(P: PPropInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FList^[I]^ do
      if (PropType = P^.PropType) and (CompareText(Name, P^.Name) = 0) then
      begin
        Result := True;
        Exit;
      end;
  Result := False;
end;

function TJvPropInfoList.Find(const AName: string): PPropInfo;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FList^[I]^ do
      if CompareText(Name, AName) = 0 then
      begin
        Result := FList^[I];
        Exit;
      end;
  Result := nil;
end;

procedure TJvPropInfoList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then
    Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Pointer));
end;

function TJvPropInfoList.Get(Index: Integer): PPropInfo;
begin
  Result := FList^[Index];
end;

procedure TJvPropInfoList.Intersect(List: TJvPropInfoList);
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if not List.Contains(FList^[I]) then
      Delete(I);
end;

{ Utility routines }

function CreateStoredItem(const CompName, PropName: string): string;
begin
  Result := '';
  if (CompName <> '') and (PropName <> '') then
    Result := CompName + '.' + PropName;
end;

function ParseStoredItem(const Item: string; var CompName, PropName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(Item) = 0 then
    Exit;
  I := Pos('.', Item);
  if I > 0 then
  begin
    CompName := Trim(Copy(Item, 1, I - 1));
    PropName := Trim(Copy(Item, I + 1, MaxInt));
    Result := (Length(CompName) > 0) and (Length(PropName) > 0);
  end;
end;

function ReplaceComponentName(const Item, CompName: string): string;
var
  ACompName, APropName: string;
begin
  Result := '';
  if ParseStoredItem(Item, ACompName, APropName) then
    Result := CreateStoredItem(CompName, APropName);
end;

procedure UpdateStoredList(AComponent: TComponent; AStoredList: TStrings; FromForm: Boolean);
var
  I: Integer;
  Component: TComponent;
  CompName, PropName: string;
begin
  if (AStoredList = nil) or (AComponent = nil) then
    Exit;
  for I := AStoredList.Count - 1 downto 0 do
  begin
    if ParseStoredItem(AStoredList[I], CompName, PropName) then
    begin
      if FromForm then
      begin
        Component := AComponent.FindComponent(CompName);
        if Component = nil then
          AStoredList.Delete(I)
        else
          AStoredList.Objects[I] := Component;
      end
      else
      begin
        Component := TComponent(AStoredList.Objects[I]);
        if Component <> nil then
          AStoredList[I] := ReplaceComponentName(AStoredList[I], Component.Name)
        else
          AStoredList.Delete(I);
      end;
    end
    else
      AStoredList.Delete(I);
  end;
end;

//=== { TJvPropertyStorage } =================================================

function TJvPropertyStorage.GetItemName(const APropName: string): string;
begin
  Result := Prefix + APropName;
end;

procedure TJvPropertyStorage.LoadAnyProperty(PropInfo: PPropInfo);
begin
  try
    if PropInfo <> nil then
      ReadProperty (AppStoragePath, GetItemName(PropInfo^.Name), TPersistent(FObject), PropInfo^.Name);
  except
    { ignore any exception }
  end;
end;

procedure TJvPropertyStorage.StoreAnyProperty(PropInfo: PPropInfo);
begin
  if PropInfo <> nil then
    WriteProperty (AppStoragePath, GetItemName(PropInfo^.Name), TPersistent(FObject), PropInfo^.Name);
end;


procedure TJvPropertyStorage.StoreProperties(PropList: TStrings);
var
  I: Integer;
  Props: TJvPropInfoList;
begin
  Props := TJvPropInfoList.Create(AObject, tkProperties);
  try
    for I := 0 to PropList.Count - 1 do
      StoreAnyProperty(Props.Find(PropList[I]));
  finally
    Props.Free;
  end;
end;

procedure TJvPropertyStorage.LoadProperties(PropList: TStrings);
var
  I: Integer;
  Props: TJvPropInfoList;
begin
  Props := TJvPropInfoList.Create(AObject, tkProperties);
  try
    for I := 0 to PropList.Count - 1 do
      LoadAnyProperty(Props.Find(PropList[I]));
  finally
    Props.Free;
  end;
end;

function TJvPropertyStorage.CreateInfoList(AComponent: TComponent; StoredList: TStrings): TStrings;
var
  I: Integer;
  Obj: TComponent;
  Props: TJvPropInfoList;
begin
  UpdateStoredList(AComponent, StoredList, False);
  Result := TStringList.Create;
  try
    TStringList(Result).Sorted := True;
    for I := 0 to StoredList.Count - 1 do
    begin
      Obj := TComponent(StoredList.Objects[I]);
      if Result.IndexOf(Obj.Name) < 0 then
      begin
        Props := TJvPropInfoList.Create(Obj, tkProperties);
        try
          Result.AddObject(Obj.Name, Props);
        except
          Props.Free;
          raise;
        end;
      end;
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

procedure TJvPropertyStorage.FreeInfoLists(Info: TStrings);
var
  I: Integer;
begin
  for I := Info.Count - 1 downto 0 do
    Info.Objects[I].Free;
  Info.Free;
end;

procedure TJvPropertyStorage.LoadObjectsProps(AComponent: TComponent; StoredList: TStrings);
var
  Info: TStrings;
  I, Idx: Integer;
  Props: TJvPropInfoList;
  CompName, PropName: string;
begin
  Info := CreateInfoList(AComponent, StoredList);
  if Info <> nil then
  try
    FOwner := AComponent;
    for I := 0 to StoredList.Count - 1 do
    begin
      if ParseStoredItem(StoredList[I], CompName, PropName) then
      begin
        AObject := StoredList.Objects[I];
        Prefix := TComponent(AObject).Name;
        Idx := Info.IndexOf(Prefix);
        if Idx >= 0 then
        begin
          Prefix := Prefix + sPropNameDelimiter;
          Props := TJvPropInfoList(Info.Objects[Idx]);
          if Props <> nil then
            LoadAnyProperty(Props.Find(PropName));
        end;
      end;
    end;
  finally
    FOwner := nil;
    FreeInfoLists(Info);
  end;
end;

procedure TJvPropertyStorage.StoreObjectsProps(AComponent: TComponent; StoredList: TStrings);
var
  Info: TStrings;
  I, Idx: Integer;
  Props: TJvPropInfoList;
  CompName, PropName: string;
begin
  Info := CreateInfoList(AComponent, StoredList);
  if Info <> nil then
  try
    FOwner := AComponent;
    for I := 0 to StoredList.Count - 1 do
    begin
      if ParseStoredItem(StoredList[I], CompName, PropName) then
      begin
        AObject := StoredList.Objects[I];
        Prefix := TComponent(AObject).Name;
        Idx := Info.IndexOf(Prefix);
        if Idx >= 0 then
        begin
          Prefix := Prefix + sPropNameDelimiter;
          Props := TJvPropInfoList(Info.Objects[Idx]);
          if Props <> nil then
            StoreAnyProperty(Props.Find(PropName));
        end;
      end;
    end;
  finally
    FOwner := nil;
    FreeInfoLists(Info);
  end;
end;

function TJvPropertyStorage.CreateStorage: TJvPropertyStorage;
begin
  Result := TJvPropertyStorage.Create;
  Result.AppStorage := AppStorage;
end;

function TJvPropertyStorage.ReadString(const APath, Item, Default: string): string;
begin
  if Assigned(AppStorage) then
    Result := AppStorage.ReadString(AppStorage.ConcatPaths([APath, AppStorage.TranslatePropertyName(Nil, Item, True)]), Default)
  else
    Result := Default;
end;

procedure TJvPropertyStorage.WriteString(const APath, Item, Value: string);
begin
  if Assigned(AppStorage) then
    AppStorage.WriteString(AppStorage.ConcatPaths([APath, AppStorage.TranslatePropertyName(Nil, Item, False)]), Value);
end;

procedure TJvPropertyStorage.EraseSection(const APath: string);
begin
  if Assigned(AppStorage) then
    AppStorage.DeleteSubTree(APath);
end;

procedure TJvPropertyStorage.ReadProperty(const APath, AStorageName: string; const PersObj: TPersistent; const PropName : string);
begin
  if Assigned(AppStorage) then
    With AppStorage Do
      ReadProperty(ConcatPaths([APath, TranslatePropertyName(PersObj, AStorageName, True)]), PersObj, PropName, True, True);
end;

procedure TJvPropertyStorage.WriteProperty(const APath, AStorageName: string; const PersObj: TPersistent; const PropName : string);
begin
  if Assigned(AppStorage) then
    With AppStorage Do
      WriteProperty(ConcatPaths([APath, TranslatePropertyName(PersObj, AStorageName, False)]), PersObj, PropName, True);
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

