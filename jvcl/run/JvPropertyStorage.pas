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

{$I jvcl.inc}

unit JvPropertyStorage;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QForms,
  {$ENDIF VisualCLX}
  JvAppStorage, TypInfo;

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
    function StoreIntegerProperty(PropInfo: PPropInfo): string;
    function StoreCharProperty(PropInfo: PPropInfo): string;
    function StoreEnumProperty(PropInfo: PPropInfo): string;
    function StoreFloatProperty(PropInfo: PPropInfo): string;
    function StoreStringProperty(PropInfo: PPropInfo): string;
    function StoreSetProperty(PropInfo: PPropInfo): string;
    function StoreClassProperty(PropInfo: PPropInfo): string;
    function StoreStringsProperty(PropInfo: PPropInfo): string;
    function StoreComponentProperty(PropInfo: PPropInfo): string;
    function StoreLStringProperty(PropInfo: PPropInfo): string;
    function StoreWCharProperty(PropInfo: PPropInfo): string;
    function StoreVariantProperty(PropInfo: PPropInfo): string;
    procedure LoadLStringProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadWCharProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadVariantProperty(const S: string; PropInfo: PPropInfo);
    function StoreInt64Property(PropInfo: PPropInfo): string;
    procedure LoadInt64Property(const S: string; PropInfo: PPropInfo);
    procedure LoadIntegerProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadCharProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadEnumProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadFloatProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadStringProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadSetProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadClassProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadStringsProperty(const S: string; PropInfo: PPropInfo);
    procedure LoadComponentProperty(const S: string; PropInfo: PPropInfo);
    function CreateInfoList(AComponent: TComponent; StoredList: TStrings): TStrings;
    procedure FreeInfoLists(Info: TStrings);
  protected
    function ReadString(const APath, Item, Default: string): string; virtual;
    procedure WriteString(const APath, Item, Value: string); virtual;
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
  JvJCLUtils;

const
  sCount = 'Count';
  sItem = 'Item%d';
  sNull = '(null)';

type
  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

function GetPropType(PropInfo: PPropInfo): PTypeInfo;
begin
  Result := PropInfo^.PropType^;
end;

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
var
  S, Def: string;
begin
  try
    if PropInfo <> nil then
    begin
      case PropInfo^.PropType^.Kind of
        tkInteger:
          Def := StoreIntegerProperty(PropInfo);
        tkChar:
          Def := StoreCharProperty(PropInfo);
        tkEnumeration:
          Def := StoreEnumProperty(PropInfo);
        tkFloat:
          Def := StoreFloatProperty(PropInfo);
        tkWChar:
          Def := StoreWCharProperty(PropInfo);
        tkLString:
          Def := StoreLStringProperty(PropInfo);
        tkVariant:
          Def := StoreVariantProperty(PropInfo);
        tkInt64:
          Def := StoreInt64Property(PropInfo);
        tkString:
          Def := StoreStringProperty(PropInfo);
        tkSet:
          Def := StoreSetProperty(PropInfo);
        tkClass:
          Def := '';
      else
        Exit;
      end;
      if (Def <> '') or (PropInfo^.PropType^.Kind in [tkString, tkClass]) or
        (PropInfo^.PropType^.Kind in [tkLString, tkWChar]) then
        S := Trim(ReadString(AppStoragePath, GetItemName(PropInfo^.Name), Def))
      else
        S := '';
      case PropInfo^.PropType^.Kind of
        tkInteger:
          LoadIntegerProperty(S, PropInfo);
        tkChar:
          LoadCharProperty(S, PropInfo);
        tkEnumeration:
          LoadEnumProperty(S, PropInfo);
        tkFloat:
          LoadFloatProperty(S, PropInfo);
        tkWChar:
          LoadWCharProperty(S, PropInfo);
        tkLString:
          LoadLStringProperty(S, PropInfo);
        tkVariant:
          LoadVariantProperty(S, PropInfo);
        tkInt64:
          LoadInt64Property(S, PropInfo);
        tkString:
          LoadStringProperty(S, PropInfo);
        tkSet:
          LoadSetProperty(S, PropInfo);
        tkClass:
          LoadClassProperty(S, PropInfo);
      end;
    end;
  except
    { ignore any exception }
  end;
end;

procedure TJvPropertyStorage.StoreAnyProperty(PropInfo: PPropInfo);
var
  S: string;
begin
  if PropInfo <> nil then
  begin
    case PropInfo^.PropType^.Kind of
      tkInteger:
        S := StoreIntegerProperty(PropInfo);
      tkChar:
        S := StoreCharProperty(PropInfo);
      tkEnumeration:
        S := StoreEnumProperty(PropInfo);
      tkFloat:
        S := StoreFloatProperty(PropInfo);
      tkLString:
        S := StoreLStringProperty(PropInfo);
      tkWChar:
        S := StoreWCharProperty(PropInfo);
      tkVariant:
        S := StoreVariantProperty(PropInfo);
      tkInt64:
        S := StoreInt64Property(PropInfo);
      tkString:
        S := StoreStringProperty(PropInfo);
      tkSet:
        S := StoreSetProperty(PropInfo);
      tkClass:
        S := StoreClassProperty(PropInfo);
    else
      Exit;
    end;
    if (S <> '') or (PropInfo^.PropType^.Kind in [tkString, tkLString, tkWChar]) then
      WriteString(AppStoragePath, GetItemName(PropInfo^.Name), Trim(S));
  end;
end;

function TJvPropertyStorage.StoreIntegerProperty(PropInfo: PPropInfo): string;
begin
  Result := IntToStr(GetOrdProp(FObject, PropInfo));
end;

function TJvPropertyStorage.StoreCharProperty(PropInfo: PPropInfo): string;
begin
  Result := Char(GetOrdProp(FObject, PropInfo));
end;

function TJvPropertyStorage.StoreEnumProperty(PropInfo: PPropInfo): string;
begin
  Result := GetEnumName(GetPropType(PropInfo), GetOrdProp(FObject, PropInfo));
end;

function TJvPropertyStorage.StoreFloatProperty(PropInfo: PPropInfo): string;
const
  Precisions: array [TFloatType] of Integer = (7, 15, 18, 18, 19);
begin
  Result := ReplaceStr(FloatToStrF(GetFloatProp(FObject, PropInfo), ffGeneral,
    Precisions[GetTypeData(GetPropType(PropInfo))^.FloatType], 0),
    DecimalSeparator, '.');
end;

function TJvPropertyStorage.StoreStringProperty(PropInfo: PPropInfo): string;
begin
  Result := GetStrProp(FObject, PropInfo);
end;

function TJvPropertyStorage.StoreLStringProperty(PropInfo: PPropInfo): string;
begin
  Result := GetStrProp(FObject, PropInfo);
end;

function TJvPropertyStorage.StoreWCharProperty(PropInfo: PPropInfo): string;
begin
  Result := Char(GetOrdProp(FObject, PropInfo));
end;

function TJvPropertyStorage.StoreVariantProperty(PropInfo: PPropInfo): string;
begin
  Result := GetVariantProp(FObject, PropInfo);
end;

function TJvPropertyStorage.StoreInt64Property(PropInfo: PPropInfo): string;
begin
  Result := IntToStr(GetInt64Prop(FObject, PropInfo));
end;

function TJvPropertyStorage.StoreSetProperty(PropInfo: PPropInfo): string;
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I: Integer;
begin
  Result := '[';
  W := GetOrdProp(FObject, PropInfo);
  TypeInfo := GetTypeData(GetPropType(PropInfo))^.CompType^;
  for I := 0 to SizeOf(TCardinalSet) * 8 - 1 do
    if I in TCardinalSet(W) then
    begin
      if Length(Result) <> 1 then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  Result := Result + ']';
end;

function TJvPropertyStorage.StoreStringsProperty(PropInfo: PPropInfo): string;
var
  List: TObject;
  I: Integer;
  SectName: string;
begin
  Result := '';
  IF Assigned(AppStorage) then
  begin
    List := TObject(GetOrdProp(Self.FObject, PropInfo));
    SectName := AppStorage.ConcatPaths ([AppStoragePath, GetItemName(PropInfo^.Name)]);
    EraseSection(SectName);
    if (List is TStrings) and (TStrings(List).Count > 0) then
    begin
      WriteString(SectName, sCount, IntToStr(TStrings(List).Count));
      for I := 0 to TStrings(List).Count - 1 do
        WriteString(SectName, Format(sItem, [I]), TStrings(List)[I]);
    end;
  end;
end;

function TJvPropertyStorage.StoreComponentProperty(PropInfo: PPropInfo): string;
var
  Comp: TComponent;
  RootName: string;
begin
  Comp := TComponent(GetOrdProp(FObject, PropInfo));
  if Comp <> nil then
  begin
    Result := Comp.Name;
    if (Comp.Owner <> nil) and (Comp.Owner <> FOwner) then
    begin
      RootName := Comp.Owner.Name;
      if RootName = '' then
      begin
        RootName := Comp.Owner.ClassName;
        if (RootName <> '') and (UpCase(RootName[1]) = 'T') then
          Delete(RootName, 1, 1);
      end;
      Result := Format('%s.%s', [RootName, Result]);
    end;
  end
  else
    Result := sNull;
end;

function TJvPropertyStorage.StoreClassProperty(PropInfo: PPropInfo): string;
var
  Saver: TJvPropertyStorage;
  I: Integer;
  Obj: TObject;

  procedure StoreObjectProps(Obj: TObject; const APrefix, APath: string);
  var
    I: Integer;
    Props: TJvPropInfoList;
  begin
    with Saver do
    begin
      AObject := Obj;
      Prefix := APrefix;
      AppStoragePath := APath;
      Props := TJvPropInfoList.Create(AObject, tkProperties);
      try
        for I := 0 to Props.Count - 1 do
          StoreAnyProperty(Props.Items[I]);
      finally
        Props.Free;
      end;
    end;
  end;

begin
  Result := '';
  if not Assigned(AppStorage) then
    Exit;
  Obj := TObject(GetOrdProp(Self.FObject, PropInfo));
  if Obj <> nil then
  begin
    if Obj is TStrings then
      StoreStringsProperty(PropInfo)
    else
    if Obj is TCollection then
    begin
      EraseSection(AppStorage.ConcatPaths([AppStoragePath, Prefix + PropInfo^.Name]));
      Saver := CreateStorage;
      try
        WriteString(AppStoragePath, Format('%s.%s', [Prefix + PropInfo^.Name, sCount]),
          IntToStr(TCollection(Obj).Count));
        for I := 0 to TCollection(Obj).Count - 1 do
        begin
          StoreObjectProps(TCollection(Obj).Items[I],
            Format(sItem, [I]) + sPropNameDelimiter,
            AppStorage.ConcatPaths([AppStoragePath, Prefix + PropInfo^.Name]));
        end;
      finally
        Saver.Free;
      end;
    end
    else
    if Obj is TComponent then
    begin
      Result := StoreComponentProperty(PropInfo);
      Exit;
    end;
  end;
  Saver := CreateStorage;
  try
    with Saver do
    begin
      StoreObjectProps(Obj, Self.Prefix + PropInfo^.Name, Self.AppStoragePath);
    end;
  finally
    Saver.Free;
  end;
end;

procedure TJvPropertyStorage.LoadIntegerProperty(const S: string; PropInfo: PPropInfo);
begin
  SetOrdProp(FObject, PropInfo, StrToIntDef(S, 0));
end;

procedure TJvPropertyStorage.LoadCharProperty(const S: string; PropInfo: PPropInfo);
begin
  SetOrdProp(FObject, PropInfo, Integer(S[1]));
end;

procedure TJvPropertyStorage.LoadEnumProperty(const S: string; PropInfo: PPropInfo);
var
  I: Integer;
  EnumType: PTypeInfo;
begin
  EnumType := GetPropType(PropInfo);
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do
      if CompareText(GetEnumName(EnumType, I), S) = 0 then
      begin
        SetOrdProp(FObject, PropInfo, I);
        Exit;
      end;
end;

procedure TJvPropertyStorage.LoadFloatProperty(const S: string; PropInfo: PPropInfo);
begin
  SetFloatProp(FObject, PropInfo, StrToFloat(ReplaceStr(S, '.', DecimalSeparator)));
end;

procedure TJvPropertyStorage.LoadInt64Property(const S: string; PropInfo: PPropInfo);
begin
  SetInt64Prop(FObject, PropInfo, StrToInt64Def(S, 0));
end;

procedure TJvPropertyStorage.LoadLStringProperty(const S: string; PropInfo: PPropInfo);
begin
  SetStrProp(FObject, PropInfo, S);
end;

procedure TJvPropertyStorage.LoadWCharProperty(const S: string; PropInfo: PPropInfo);
begin
  SetOrdProp(FObject, PropInfo, Longint(S[1]));
end;

procedure TJvPropertyStorage.LoadVariantProperty(const S: string; PropInfo: PPropInfo);
begin
  SetVariantProp(FObject, PropInfo, S);
end;

procedure TJvPropertyStorage.LoadStringProperty(const S: string; PropInfo: PPropInfo);
begin
  SetStrProp(FObject, PropInfo, S);
end;

procedure TJvPropertyStorage.LoadSetProperty(const S: string; PropInfo: PPropInfo);
const
  Delims = [' ', ',', '[', ']'];
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I, N: Integer;
  Count: Integer;
  EnumName: string;
begin
  W := 0;
  TypeInfo := GetTypeData(GetPropType(PropInfo))^.CompType^;
  Count := WordCount(S, Delims);
  for N := 1 to Count do
  begin
    EnumName := ExtractWord(N, S, Delims);
    try
      I := GetEnumValue(TypeInfo, EnumName);
      if I >= 0 then
        Include(TCardinalSet(W), I);
    except
    end;
  end;
  SetOrdProp(FObject, PropInfo, W);
end;

procedure TJvPropertyStorage.LoadStringsProperty(const S: string; PropInfo: PPropInfo);
var
  List: TObject;
  Temp: TStrings;
  I, Cnt: Integer;
  SectName: string;
begin
  List := TObject(GetOrdProp(Self.FObject, PropInfo));
  if List is TStrings then
  begin
    SectName := AppStorage.ConcatPaths([AppStoragePath, Prefix + PropInfo^.Name]);
    Cnt := StrToIntDef(Trim(ReadString(SectName, sCount, '0')), 0);
    if Cnt > 0 then
    begin
      Temp := TStringList.Create;
      try
        for I := 0 to Cnt - 1 do
          Temp.Add(ReadString(SectName, Format(sItem, [I]), ''));
        TStrings(List).Assign(Temp);
      finally
        Temp.Free;
      end;
    end;
  end;
end;

procedure TJvPropertyStorage.LoadComponentProperty(const S: string; PropInfo: PPropInfo);
var
  RootName, Name: string;
  Root: TComponent;
  P: Integer;
begin
  if Trim(S) = '' then
    Exit;
  if CompareText(sNull, Trim(S)) = 0 then
  begin
    SetOrdProp(FObject, PropInfo, Longint(nil));
    Exit;
  end;
  P := Pos('.', S);
  if P > 0 then
  begin
    RootName := Trim(Copy(S, 1, P - 1));
    Name := Trim(Copy(S, P + 1, MaxInt));
  end
  else
  begin
    RootName := '';
    Name := Trim(S);
  end;
  if RootName <> '' then
    Root := FindGlobalComponent(RootName)
  else
    Root := FOwner;
  if Root <> nil then
    SetOrdProp(FObject, PropInfo, Longint(Root.FindComponent(Name)));
end;

procedure TJvPropertyStorage.LoadClassProperty(const S: string; PropInfo: PPropInfo);
var
  Loader: TJvPropertyStorage;
  I: Integer;
  Cnt: Integer;
  Recreate: Boolean;
  Obj: TObject;

  procedure LoadObjectProps(Obj: TObject; const APrefix, APath: string);
  var
    I: Integer;
    Props: TJvPropInfoList;
  begin
    with Loader do
    begin
      AObject := Obj;
      Prefix := APrefix;
      AppStoragePath := APath;
      Props := TJvPropInfoList.Create(AObject, tkProperties);
      try
        for I := 0 to Props.Count - 1 do
          LoadAnyProperty(Props.Items[I]);
      finally
        Props.Free;
      end;
    end;
  end;

begin
  Obj := TObject(GetOrdProp(Self.FObject, PropInfo));
  if Obj <> nil then
  begin
    if Obj is TStrings then
      LoadStringsProperty(S, PropInfo)
    else
    if Obj is TCollection then
    begin
      Loader := CreateStorage;
      try
        Cnt := TCollection(Obj).Count;
        Cnt := StrToIntDef(ReadString(AppStoragePath, Format('%s.%s',
          [Prefix + PropInfo^.Name, sCount]), IntToStr(Cnt)), Cnt);
        Recreate := TCollection(Obj).Count <> Cnt;
        TCollection(Obj).BeginUpdate;
        try
          if Recreate then
            TCollection(Obj).Clear;
          for I := 0 to Cnt - 1 do
          begin
            if Recreate then
              TCollection(Obj).Add;
            LoadObjectProps(TCollection(Obj).Items[I],
              Format(sItem, [I]) + sPropNameDelimiter,
              AppStorage.ConcatPaths([AppStoragePath, Prefix + PropInfo^.Name]));
          end;
        finally
          TCollection(Obj).EndUpdate;
        end;
      finally
        Loader.Free;
      end;
    end
    else
    if Obj is TComponent then
    begin
      LoadComponentProperty(S, PropInfo);
      Exit;
    end;
  end;
  Loader := CreateStorage;
  try
    LoadObjectProps(Obj, Self.Prefix + PropInfo^.Name, Self.AppStoragePath);
  finally
    Loader.Free;
  end;
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
    Result := AppStorage.ReadString(AppStorage.ConcatPaths([APath, AppStorage.TranslatePropertyName(Nil, Item, False)]), Default)
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

end.

