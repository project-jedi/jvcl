{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPropertyStore.pas, released on 2003-11-13.

The Initial Developer of the Original Code is Jens Fudickar
Portions created by Marcel Bestebroer are Copyright (C) 2003 Jens Fudickar
All Rights Reserved.

Contributor(s):
  Marcel Bestebroer

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPropertyStore;

{$I jvcl.inc}

interface

uses
  Classes,
  JvAppStorage, JvComponent;

type
  TJvCustomPropertyStore = class(TJvComponent)
  private
    FAppStoragePath: string;
    FAppStorage: TJvCustomAppStorage;
    FEnabled: Boolean;
    FDeleteBeforeStore: Boolean;
    FClearBeforeLoad: Boolean;
    FIntIgnoreProperties: TStringList;
    FIgnoreProperties: TStringList;
    FAutoLoad: Boolean;
    FLastLoadTime: TDateTime;
    FIgnoreLastLoadTime: Boolean;
    FCombinedIgnoreProperties: TStrings;
    FOnBeforeLoadProperties: TNotifyEvent;
    FOnAfterLoadProperties: TNotifyEvent;
    FOnBeforeStoreProperties: TNotifyEvent;
    FOnAfterStoreProperties: TNotifyEvent;
    procedure SetAutoLoad(Value: Boolean);
    function GetIgnoreProperties: TStrings;
    procedure SetIgnoreProperties(Value: TStrings);
    function GetPropCount(Instance: TPersistent): Integer;
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    procedure CloneClass(Src, Dest: TPersistent);
    function GetLastSaveTime: TDateTime;
  protected
    procedure UpdateChildPaths(OldPath: string = ''); virtual;
    procedure SetPath(Value: string); virtual;
    procedure SetAppStorage(Value: TJvCustomAppStorage);
    procedure Loaded; override;
    procedure DisableAutoLoadDown;
    procedure LoadData; virtual;
    procedure StoreData; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetCombinedIgnoreProperties: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AppStorage: TJvCustomAppStorage read FAppStorage write SetAppStorage;
    procedure StoreProperties; virtual;
    procedure LoadProperties; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    function TranslatePropertyName(AName: string): string; virtual;
    property CombinedIgnoreProperties: TStrings read GetCombinedIgnoreProperties;
  published
    property AutoLoad: Boolean read FAutoLoad write SetAutoLoad;
    property AppStoragePath: string read FAppStoragePath write SetPath;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property DeleteBeforeStore: Boolean read FDeleteBeforeStore write FDeleteBeforeStore
      default False;
    property ClearBeforeLoad: Boolean read FClearBeforeLoad write FClearBeforeLoad default False;
    property IgnoreLastLoadTime: Boolean read FIgnoreLastLoadTime write FIgnoreLastLoadTime
      default False;
    property IgnoreProperties: TStrings read GetIgnoreProperties write SetIgnoreProperties;
    property OnBeforeLoadProperties: TNotifyEvent read FOnBeforeLoadProperties
      write FOnBeforeLoadProperties;
    property OnAfterLoadProperties: TNotifyEvent read FOnAfterLoadProperties
      write FOnAfterLoadProperties;
    property OnBeforeStoreProperties: TNotifyEvent read FOnBeforeStoreProperties
      write FOnBeforeStoreProperties;
    property OnAfterStoreProperties: TNotifyEvent read FOnAfterStoreProperties
      write FOnAfterStoreProperties;
  end;

  TJvCustomPropertyListStore = class(TJvCustomPropertyStore)
  private
    FItems: TStringList;
    FFreeObjects: Boolean;
    FCreateListEntries: Boolean;
    function GetItems: TStrings;
  protected
    function GetString(Index: Integer): string;
    function GetObject(Index: Integer): TObject;
    procedure SetString(Index: Integer; Value: string);
    procedure SetObject(Index: Integer; Value: TObject);
    function GetCount: Integer;
    procedure ReadSLOItem(Sender: TJvCustomAppStorage; const Path: string; const List: TObject;const Index: Integer; const ItemName: string);
    procedure WriteSLOItem(Sender: TJvCustomAppStorage; const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
    procedure DeleteSLOItems(Sender: TJvCustomAppStorage; const Path: string; const List: TObject;
      const First, Last: Integer; const ItemName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StoreData; override;
    procedure LoadData; override;
    procedure Clear; override;
    function CreateObject: TObject; virtual;
    property Strings [Index: Integer]: string read GetString write SetString;
    property Objects[Index: Integer]: TObject read GetObject write SetObject;
    property Items: TStrings read GetItems;
    property Count: Integer read GetCount;
  published
    { Defines if the Items.Objects- Objects will be freed inside the clear procedure }
    property FreeObjects: Boolean read FFreeObjects Write FFreeObjects default True;
    { Defines if new List entries will be created if there are stored entries, which
      are not in the current object }
    property CreateListEntries: Boolean read FCreateListEntries write FCreateListEntries
      default True;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  {$IFDEF VCL}
  Consts,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QConsts,
  {$ENDIF VisualCLX}
  SysUtils, TypInfo;

const
  cLastSaveTime = 'Last Save Time';
  cObject = 'Object';

//=== { TCombinedStrings } ===================================================

type
  // Read-only TStrings combining multiple TStrings instances in a single list
  TCombinedStrings = class(TStrings)
  private
    FList: TList;
  protected
    function Get(Index: Integer): string; override;
    function GetObject(Index: Integer): TObject; override;
    function GetCount: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddStrings(Strings: TStrings); override;
//    procedure DeleteStrings(Strings: TStrings);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

constructor TCombinedStrings.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TCombinedStrings.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TCombinedStrings.Get(Index: Integer): string;
var
  OrgIndex: Integer;
  I: Integer;
begin
  OrgIndex := Index;
  I := 0;
  if Index < 0 then
    Error(SListIndexError, Index);
  while (I < FList.Count) and (Index >= TStrings(FList[I]).Count) do
  begin
    Dec(Index, TStrings(FList[I]).Count);
    Inc(I);
  end;
  if I >= FList.Count then
    Error(SListIndexError, OrgIndex);
  Result := TStrings(FList[I])[Index];
end;

function TCombinedStrings.GetObject(Index: Integer): TObject;
var
  OrgIndex: Integer;
  I: Integer;
begin
  OrgIndex := Index;
  I := 0;
  if Index < 0 then
    Error(SListIndexError, Index);
  while (Index < TStrings(FList[I]).Count) and (I < FList.Count) do
  begin
    Dec(Index, TStrings(FList[I]).Count);
    Inc(I);
  end;
  if I >= FList.Count then
    Error(SListIndexError, OrgIndex);
  Result := TStrings(FList[I]).Objects[Index];
end;

function TCombinedStrings.GetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FList.Count - 1 do
    Inc(Result, TStrings(FList[I]).Count);
end;

procedure TCombinedStrings.AddStrings(Strings: TStrings);
begin
  if FList.IndexOf(Strings) = -1 then
    FList.Add(Strings);
end;

(*
procedure TCombinedStrings.DeleteStrings(Strings: TStrings);
begin
  FList.Remove(Strings);
end;
*)

procedure TCombinedStrings.Clear;
begin
  FList.Clear;
end;

procedure TCombinedStrings.Delete(Index: Integer);
begin
end;

procedure TCombinedStrings.Insert(Index: Integer; const S: string);
begin
end;

//=== { TJvCustomPropertyStore } =============================================

constructor TJvCustomPropertyStore.Create(AOwner: TComponent);
const
  IgnorePropertyList: array [1..14] of PChar =
   (
    'AboutJVCL',
    'AppStoragePath',
    'AutoLoad',
    'ClearBeforeLoad',
    'Name',
    'Tag',
    'Enabled',
    'DeleteBeforeStore',
    'IgnoreLastLoadTime',
    'IgnoreProperties',
    'OnBeforeLoadProperties',
    'OnAfterLoadProperties',
    'OnBeforeStoreProperties',
    'OnAfterStoreProperties'
   );
var
  I: Integer;
begin
  inherited Create(AOwner);
  FLastLoadTime := Now;
  FAppStorage := nil;
  FEnabled := True;
  FDeleteBeforeStore := False;
  FAutoLoad := False;
  FIntIgnoreProperties := TStringList.Create;
  FIgnoreProperties := TStringList.Create;
  FIgnoreLastLoadTime := False;
  FCombinedIgnoreProperties := TCombinedStrings.Create;
  for I := Low(IgnorePropertyList) to High(IgnorePropertyList) do
    FIntIgnoreProperties.Add(IgnorePropertyList[I]);
end;

destructor TJvCustomPropertyStore.Destroy;
begin
  if not (csDesigning in ComponentState) then
    if AutoLoad then
      StoreProperties;
  FreeAndNil(FCombinedIgnoreProperties);
  FreeAndNil(FIntIgnoreProperties);
  FreeAndNil(FIgnoreProperties);
  Clear;
  inherited Destroy;
end;

procedure TJvCustomPropertyStore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAppStorage) then
    FAppStorage := nil;
end;

function TJvCustomPropertyStore.GetCombinedIgnoreProperties: TStrings;
begin
  FCombinedIgnoreProperties.Assign(FIntIgnoreProperties);
  FCombinedIgnoreProperties.AddStrings(FIgnoreProperties);
  Result := FCombinedIgnoreProperties;
end;

function TJvCustomPropertyStore.GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Instance.ClassInfo);
  Result := Data^.PropCount;
end;

function TJvCustomPropertyStore.GetPropName(Instance: TPersistent; Index: Integer): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.ClassInfo);
  GetMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := PropInfo^.Name;
  finally
    FreeMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  end;
end;

procedure TJvCustomPropertyStore.CloneClass(Src, Dest: TPersistent);
var
  Index: Integer;
  SrcPropInfo: PPropInfo;
  DestPropInfo: PPropInfo;
begin
  for Index := 0 to GetPropCount(Src) - 1 do
    if CompareText(GetPropName(Src, Index), 'Name') <> 0 then
    begin
      SrcPropInfo  := GetPropInfo(Src.ClassInfo, GetPropName(Src, Index));
      DestPropInfo := GetPropInfo(Dest.ClassInfo, GetPropName(Src, Index));
      if (DestPropInfo <> nil) and (DestPropInfo^.PropType^.Kind = SrcPropInfo^.PropType^.Kind) then
        case DestPropInfo^.PropType^.Kind of
          tkLString, tkString:
            SetStrProp(Dest, DestPropInfo, GetStrProp(Src, SrcPropInfo));
          tkInteger, tkChar, tkEnumeration, tkSet:
            SetOrdProp(Dest, DestPropInfo, GetOrdProp(Src, SrcPropInfo));
          tkFloat:
            SetFloatProp(Dest, DestPropInfo, GetFloatProp(Src, SrcPropInfo));
          tkVariant:
            SetVariantProp(Dest, DestPropInfo, GetVariantProp(Src, SrcPropInfo));
          tkClass:
            TPersistent(GetOrdProp(Dest, DestPropInfo)).Assign(
              TPersistent(GetOrdProp(Src, SrcPropInfo)));
          tkMethod:
            SetMethodProp(Dest, DestPropInfo, GetMethodProp(Src, SrcPropInfo));
        end;
    end;
end;

procedure TJvCustomPropertyStore.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    if AutoLoad then
      LoadProperties;
end;

procedure TJvCustomPropertyStore.Assign(Source: TPersistent);
begin
  if Source is Self.ClassType then
    CloneClass(Source, Self)
  else
    inherited Assign(Source);
end;

procedure TJvCustomPropertyStore.Clear;
begin
end;

function TJvCustomPropertyStore.TranslatePropertyName(AName: string): string;
begin
  Result := AName;
end;

procedure TJvCustomPropertyStore.SetAutoLoad(Value: Boolean);
begin
  if not Assigned(Owner) then
    Exit;
  if Owner is TJvCustomPropertyStore then
    FAutoLoad := False
  else
  if Value <> AutoLoad then
    FAutoLoad := Value;
end;

procedure TJvCustomPropertyStore.DisableAutoLoadDown;
var
  Index: Integer;
  PropName: string;
begin
  for Index := 0 to GetPropCount(Self) - 1 do
  begin
    PropName := GetPropName(Self, Index);
    if IgnoreProperties.IndexOf(PropName) < 0 then
      if FIntIgnoreProperties.IndexOf(PropName) < 0 then
        if PropType(Self, GetPropName(Self, Index)) = tkClass then
          if (TPersistent(GetOrdProp(Self, PropName)) is TJvCustomPropertyStore) then
            TJvCustomPropertyStore(TPersistent(GetOrdProp(Self, PropName))).AutoLoad := False;
  end;
end;

procedure TJvCustomPropertyStore.UpdateChildPaths(OldPath: string);
var
  Index: Integer;
  VisPropName: string;
  PropName: string;
begin
  if OldPath = '' then
    OldPath := AppStoragePath;
  for Index := 0 to GetPropCount(Self) - 1 do
  begin
    PropName := GetPropName(Self, Index);
    VisPropName := AppStorage.TranslatePropertyName(Self, PropName, False);
    if IgnoreProperties.IndexOf(PropName) >= 0 then
      Continue;
    if FIntIgnoreProperties.IndexOf(PropName) >= 0 then
      Continue;
    if PropType(Self, GetPropName(Self, Index)) = tkClass then
      if (TPersistent(GetOrdProp(Self, PropName)) is TJvCustomPropertyStore) then
        if (TJvCustomPropertyStore(TPersistent(GetOrdProp(Self, PropName))).AppStoragePath =
          AppStorage.ConcatPaths([OldPath, VisPropName])) or
          (TJvCustomPropertyStore(TPersistent(GetOrdProp(Self, PropName))).AppStoragePath = '') then
            TJvCustomPropertyStore(TPersistent(GetOrdProp(Self, PropName))).AppStoragePath :=
            AppStorage.ConcatPaths([AppStoragePath, VisPropName]);
  end;
end;

procedure TJvCustomPropertyStore.SetPath(Value: string);
var
  OldPath: string;
begin
  OldPath := FAppStoragePath;
  if Value <> AppStoragePath then
    FAppStoragePath := Value;
  UpdateChildPaths(OldPath);
end;

procedure TJvCustomPropertyStore.SetAppStorage(Value: TJvCustomAppStorage);
var
  Index: Integer;
begin
  if Value <> FAppStorage then
  begin
    for Index := 0 to ComponentCount - 1 do
      if Components[Index] is TJvCustomPropertyStore then
        TJvCustomPropertyStore(Components[Index]).AppStorage := Value;
    FAppStorage := Value;
  end;
end;

function TJvCustomPropertyStore.GetIgnoreProperties: TStrings;
begin
  Result := FIgnoreProperties;
end;

procedure TJvCustomPropertyStore.SetIgnoreProperties(Value: TStrings);
begin
  FIgnoreProperties.Assign(Value);
end;

function TJvCustomPropertyStore.GetLastSaveTime: TDateTime;
begin
  Result := 0;
  if not Enabled then
    Exit;
  if AppStoragePath = '' then
    Exit;
  try
    if AppStorage.ValueStored(AppStorage.ConcatPaths([AppStoragePath, cLastSaveTime])) then
      Result := AppStorage.ReadDateTime(AppStorage.ConcatPaths([AppStoragePath, cLastSaveTime]));
  except
    on e:Exception do
      Result := 0;
  end;
end;

procedure TJvCustomPropertyStore.LoadProperties;
begin
  if not Enabled then
    Exit;
  if not Assigned(AppStorage) then
    Exit;
  UpdateChildPaths;
  FLastLoadTime := Now;
  if ClearBeforeLoad then
    Clear;
  if Assigned(FOnBeforeLoadProperties) then
    FOnBeforeLoadProperties(Self);
  LoadData;
  AppStorage.ReadPersistent(AppStoragePath, Self, True, True, CombinedIgnoreProperties);
  if Assigned(FOnAfterLoadProperties) then
    FOnAfterLoadProperties(Self);
end;

procedure TJvCustomPropertyStore.StoreProperties;
var
  SaveProperties: Boolean;
begin
  if not Enabled then
    Exit;
  if not Assigned(AppStorage) then
    Exit;
  UpdateChildPaths;
  DisableAutoLoadDown;
  SaveProperties := IgnoreLastLoadTime or (GetLastSaveTime < FLastLoadTime);
  if DeleteBeforeStore then
    AppStorage.DeleteSubTree(AppStoragePath);
  if not IgnoreLastLoadTime then
    AppStorage.WriteString(AppStorage.ConcatPaths([AppStoragePath, cLastSaveTime]), DateTimeToStr(Now));
  if Assigned(FOnBeforeStoreProperties) then
    FOnBeforeStoreProperties(Self);
  if SaveProperties then
    StoreData;
  AppStorage.WritePersistent(AppStoragePath, Self, True, CombinedIgnoreProperties);
  if Assigned(FOnAfterStoreProperties) then
    FOnAfterStoreProperties(Self);
end;

procedure TJvCustomPropertyStore.LoadData;
begin
end;

procedure TJvCustomPropertyStore.StoreData;
begin
end;

//=== { TJvCustomPropertyListStore } =========================================

constructor TJvCustomPropertyListStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  CreateListEntries := True;
  FreeObjects := True;
  FIntIgnoreProperties.Add('FreeObjects');
  FIntIgnoreProperties.Add('CreateListEntries');
end;

destructor TJvCustomPropertyListStore.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TJvCustomPropertyListStore.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TJvCustomPropertyListStore.StoreData;
begin
  inherited StoreData;
  AppStorage.WriteList(AppStoragePath, nil, Count, WriteSLOItem, DeleteSLOItems);
end;

procedure TJvCustomPropertyListStore.LoadData;
begin
  inherited LoadData;
  AppStorage.ReadList(AppStoragePath, nil, ReadSLOItem);
end;

procedure TJvCustomPropertyListStore.Clear;
var
  I: Integer;
begin
  if FreeObjects then
    for I := 0 to Count - 1 do
      if Assigned(Objects[I]) then
        Objects[I].Free;
  Items.Clear;
  inherited Clear;
end;

function TJvCustomPropertyListStore.CreateObject: TObject;
begin
  Result := nil;
end;

function TJvCustomPropertyListStore.GetString(Index: Integer): string;
begin
  Result := Items.Strings[Index];
end;

function TJvCustomPropertyListStore.GetObject(Index: Integer): TObject;
begin
  Result := Items.Objects[Index];
end;

procedure TJvCustomPropertyListStore.SetString(Index: Integer; Value: string);
begin
  Items.Strings[Index] := Value;
end;

procedure TJvCustomPropertyListStore.SetObject(Index: Integer; Value: TObject);
begin
  Items.Objects[Index] := Value;
end;

function TJvCustomPropertyListStore.GetCount: Integer;
begin
  Result := Items.Count;
end;

procedure TJvCustomPropertyListStore.ReadSLOItem(Sender: TJvCustomAppStorage; const Path: string;
  const List: TObject; const Index: Integer; const ItemName: string);
var
  NewObject:  TObject;
  ObjectName: string;
begin
  if Index >= Count then
  begin
    if not CreateListEntries then
      Exit;
    if Sender.PathExists(Sender.ConcatPaths([Path, cObject + IntToStr(Index)])) then
    begin
      NewObject := CreateObject;
      if Assigned(NewObject) then
      begin
        if NewObject is TJvCustomPropertyStore then
        begin
          TJvCustomPropertyStore(NewObject).AppStoragePath := Sender.ConcatPaths([Path, cObject +
            IntToStr(Index)]);
          TJvCustomPropertyStore(NewObject).LoadProperties;
        end
        else
        if NewObject is TPersistent then
          Sender.ReadPersistent(Sender.ConcatPaths([Path, cObject + IntToStr(Index)]),
            TPersistent(NewObject), True, True, CombinedIgnoreProperties);
      end;
      if Sender.ValueStored(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)])) then
        ObjectName := Sender.ReadString(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)]))
      else
        ObjectName := '';
      Items.AddObject(ObjectName, NewObject);
    end
    else
      Items.Add(Sender.ReadString(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)])))
  end
  else
  if Sender.ValueStored(Sender.ConcatPaths([Path, cObject + IntToStr(Index)])) then
  begin
    if Assigned(Objects[Index]) then
    begin
      if Objects[Index] is TJvCustomPropertyStore then
      begin
        TJvCustomPropertyStore(Objects[Index]).AppStoragePath := Sender.ConcatPaths([Path, cObject +
          IntToStr(Index)]);
        TJvCustomPropertyStore(Objects[Index]).LoadProperties;
      end
      else
      if Objects[Index] is TPersistent then
        Sender.ReadPersistent(Sender.ConcatPaths([Path, cObject + IntToStr(Index)]),
          TPersistent(Objects[Index]), True, True, CombinedIgnoreProperties);
    end;
    if Sender.ValueStored(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)])) then
      Strings[Index] := Sender.ReadString(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)]))
    else
      Strings[Index] := '';
  end
  else
    Strings[Index] := Sender.ReadString(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)]));
end;

procedure TJvCustomPropertyListStore.WriteSLOItem(Sender: TJvCustomAppStorage; const Path: string; const List: TObject;
  const Index: Integer; const ItemName: string);
begin
  if Assigned(Objects[Index]) then
  begin
    if Objects[Index] is TJvCustomPropertyStore then
    begin
      TJvCustomPropertyStore(Objects[Index]).AppStoragePath := Sender.ConcatPaths([Path, cObject +
        IntToStr(Index)]);
      TJvCustomPropertyStore(Objects[Index]).StoreProperties;
    end
    else
    if Objects[Index] is TPersistent then
      Sender.WritePersistent(Sender.ConcatPaths([Path, cObject + IntToStr(Index)]),
        TPersistent(Objects[Index]), True, CombinedIgnoreProperties);
    if Strings[Index] <> '' then
      Sender.WriteString(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)]), Strings[Index]);
  end
  else
    Sender.WriteString(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)]), Strings[Index]);
end;

procedure TJvCustomPropertyListStore.DeleteSLOItems(Sender: TJvCustomAppStorage; const Path: string;
  const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  for I := First to Last do
  begin
    Sender.DeleteValue(Sender.ConcatPaths([Path, ItemName + IntToStr(I)]));
    Sender.DeleteValue(Sender.ConcatPaths([Path, cObject + IntToStr(I)]));
  end;
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
