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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id: jvcl/run/JvPropertyStore.pas jfudickar date $

unit JvPropertyStore;

{$I jvcl.inc}

interface

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  Classes,
  JvAppStorage, JvComponentBase, JvPropertyStoreEditorIntf;

type
  TJvIgnorePropertiesStringList = class(TStringList)
  public
    constructor Create;
    procedure AddDelete(AItem: string; ADelete: Boolean);
  end;

  TJvCustomPropertyStoreClass = class of TJvCustomPropertyStore;

  TJvCustomPropertyStore = class(TJvComponent, IJvPropertyEditorHandler)
  private
    FAppStoragePath: string;
    FAppStorage: TJvCustomAppStorage;
    FEnabled: Boolean;
    FReadOnly: Boolean;
    FDeleteBeforeStore: Boolean;
    FClearBeforeLoad: Boolean;
    FIntIgnoreProperties: TStringList;
    FIgnoreProperties: TJvIgnorePropertiesStringList;
    FAutoLoad: Boolean;
    FLastLoadTime: TDateTime;
    FIgnoreLastLoadTime: Boolean;
    FCombinedIgnoreProperties: TStringList;
    FOnBeforeLoadProperties: TNotifyEvent;
    FOnAfterLoadProperties: TNotifyEvent;
    FOnBeforeStoreProperties: TNotifyEvent;
    FOnAfterStoreProperties: TNotifyEvent;
    FSynchronizeStoreProperties: Boolean;
    FSynchronizeLoadProperties: Boolean;
    procedure SetAutoLoad(Value: Boolean);
    function GetIgnoreProperties: TJvIgnorePropertiesStringList;
    procedure SetIgnoreProperties(Value: TJvIgnorePropertiesStringList);
    function GetLastSaveTime: TDateTime;
    function GetPropCount(Instance: TPersistent): Integer;
    function GetPropName(Instance: TPersistent; Index: Integer): string;
  protected
    procedure CloneClassProperties(Src, Dest: TPersistent); virtual;
    procedure UpdateChildPaths(OldPath: string = ''); virtual;
    procedure SetAppStoragePath(Value: string); virtual;
    procedure SetAppStorage(Value: TJvCustomAppStorage); virtual;
    procedure Loaded; override;
    procedure DisableAutoLoadDown;
    procedure LoadData; virtual;
    procedure StoreData; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCombinedIgnoreProperties: TStringList;
    function GetPropertyCount: Integer;
    function GetPropertyName(Index: Integer): string;
    //1 Returns the given property as TJvCustomPropertyStore or returns nil
    function GetPropertyJvCustomPropertyStore(PropName: string): TJvCustomPropertyStore;
    procedure StoreXMLProperties; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StoreProperties; virtual;
    procedure LoadProperties; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    function IgnoreProperty(const PropertyName: string): Boolean;
    //1 // This function defines, if the properties should be stored in this moment
    function StorePropertiesNow: Boolean; virtual;
    function TranslatePropertyName(AName: string): string; virtual;
    property AppStorage: TJvCustomAppStorage read FAppStorage write SetAppStorage;
    property CombinedIgnoreProperties: TStringList read GetCombinedIgnoreProperties;
    property IgnoreProperties: TJvIgnorePropertiesStringList read GetIgnoreProperties write SetIgnoreProperties;
    property AutoLoad: Boolean read FAutoLoad write SetAutoLoad;
    property AppStoragePath: string read FAppStoragePath write SetAppStoragePath;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property DeleteBeforeStore: Boolean read FDeleteBeforeStore write FDeleteBeforeStore default False;
    property ClearBeforeLoad: Boolean read FClearBeforeLoad write FClearBeforeLoad default False;
    property IgnoreLastLoadTime: Boolean read FIgnoreLastLoadTime write FIgnoreLastLoadTime default False;
    property OnBeforeLoadProperties: TNotifyEvent read FOnBeforeLoadProperties write FOnBeforeLoadProperties;
    property OnAfterLoadProperties: TNotifyEvent read FOnAfterLoadProperties write FOnAfterLoadProperties;
    property OnBeforeStoreProperties: TNotifyEvent read FOnBeforeStoreProperties write FOnBeforeStoreProperties;
    property OnAfterStoreProperties: TNotifyEvent read FOnAfterStoreProperties write FOnAfterStoreProperties;
    property PropertyCount: Integer read GetPropertyCount;
    property PropertyName[Index: Integer]: string read GetPropertyName;
    //1 Synchronize the StoreProperties procedure
    /// Defines if the execution of the StoreProperties procedure for the current
    /// AppStoragePath should be synchronized via a global mutex
    /// When the SynchronizeLoadProperties is also definde the load and store
    /// procedures will be synched with the same mutex.
    property SynchronizeStoreProperties: Boolean read FSynchronizeStoreProperties write FSynchronizeStoreProperties default False;
    //1 Synchronize the LoadProperties procedure
    /// Defines if the execution of the LoadProperties procedure for the current
    /// AppStoragePath should be synchronized via a global mutex.
    /// When the SynchronizeStoreProperties is also definde the load and store
    /// procedures will be synched with the same mutex.
    property SynchronizeLoadProperties: Boolean read FSynchronizeLoadProperties write FSynchronizeLoadProperties default False;
    property Tag;

    //1 Creates a new instance of the same objecttype and assigns the property contents to the new instance
    function Clone(AOwner: TComponent): TJvCustomPropertyStore; virtual;
    //IJvPropertyEditorHandler = interface
    function EditIntf_GetVisibleObjectName: string; virtual;
    function EditIntf_TranslatePropertyName(const PropertyName: string): string; virtual;
    function EditIntf_DisplayProperty(const PropertyName: string): Boolean; virtual;
    function EditIntf_GetObjectHint: string; virtual;
    function EditIntf_GetPropertyHint(const PropertyName: string): string; virtual;
    function EditIntf_IsPropertySimple(const PropertyName: string): Boolean; virtual;
    function ValidateData: Boolean; virtual;
  end;

  TJvCustomPropertyListStore = class(TJvCustomPropertyStore, IJvPropertyListEditorHandler)
  private
    FItems: TStringList;
    FFreeObjects: Boolean;
    FCreateListEntries: Boolean;
    FItemName: string;
    FItemsObjectName: string;
    function GetItems: TStringList;
    //IJvPropertyListEditorHandler = interface
    function ListEditIntf_ObjectCount: integer;
    function ListEditIntf_GetObject(Index: integer): TPersistent;
    procedure ListEditIntf_MoveObjectPosition(CurIndex, NewIndex: Integer);
    procedure ListEditIntf_SortObjects(iAscending : Boolean);
    function ListEditIntf_CloneNewObject(Index: integer): TPersistent;
    procedure ListEditIntf_DeleteObject(Index: integer);
    function ListEditIntf_IndexOfObject(AObject : TPersistent): Integer;
  protected
    function GetString(Index: Integer): string;
    function GetObject(Index: Integer): TObject;
    procedure SetString(Index: Integer; Value: string);
    procedure SetObject(Index: Integer; Value: TObject);
    function GetCount: Integer;
    procedure ReadSLOItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    procedure WriteSLOItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    procedure DeleteSLOItems(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);
    function CreateItemList: TStringList; virtual;
    function CreateObject: TPersistent; virtual; abstract;
    function GetSorted: Boolean;
    procedure SetSorted(Value: Boolean);
    function GetDuplicates: TDuplicates;
    function ListEditIntf_CreateNewObject: TPersistent; virtual;
    procedure SetDuplicates(Value: TDuplicates);
    procedure StoreData; override;
    procedure LoadData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IndexOf(const s: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    property Strings[Index: Integer]: string read GetString write SetString;
    property Objects[Index: Integer]: TObject read GetObject write SetObject;
    property Items: TStringList read GetItems;
    property Count: Integer read GetCount;
    { Defines if the Items.Objects- Objects will be freed inside the clear procedure }
    property FreeObjects: Boolean read FFreeObjects write FFreeObjects default True;
    { Defines if new List entries will be created if there are stored entries, which
      are not in the current object }
    property CreateListEntries: Boolean read FCreateListEntries write FCreateListEntries default True;
    property ItemName: string read FItemName write FItemName;
    //1 Name to read the value of the object from the appstorage path
    /// Using this path addition the internal name of the object is read from
    /// the appstorage.
    /// This property is necessary only for the xml appstorage.
    /// When in this case the property is not defined the internal items string
    /// value couldn't be recovered from the appstorage (out of xml
    /// restrictions with array elements)
    property ItemsObjectName: string read FItemsObjectName write FItemsObjectName;
    property Sorted: Boolean read GetSorted write SetSorted;
    function CreateAddObject(const aObjectName: String): TPersistent;
    function ValidateData: Boolean; override;
  end;

procedure StorePropertyStorageToAppStorage(iPropertyStore: TJvCustomPropertyStore; iAppStorage: TJvCustomAppStorage; const
  iAppStoragePath: string);


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile:
      '$URL: jvcl/run/JvPropertyStore.pas $';
    Revision: '$Revision: 0559eb86d50554080bc539a2225603400dfb6baf $';
    Date: '$Date: 2013-04-09 22:23:19 +0000 $';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  RTLConsts, SysUtils, TypInfo,
  JclSynch,
  JvStrings, JvResources, JvJVCLUtils;

const
  cLastSaveTime = 'LastSaveTime';
  cObject = 'Object';
  cItem = 'Item';

  //=== { TCombinedStrings } ===================================================

type
  // Read-only TStrings combining multiple TStrings instances in a single list
  TCombinedStrings = class(TStringList)
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
  Sorted := True;
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

constructor TJvIgnorePropertiesStringList.Create;
begin
  inherited Create;
  Sorted := True;
end;

//=== { TJvIgnorePropertiesStringList } ======================================

procedure TJvIgnorePropertiesStringList.AddDelete(AItem: string; ADelete:
  Boolean);
begin
  if ADelete then
  begin
    if IndexOf(AItem) >= 0 then
      Delete(IndexOf(AItem));
  end
  else
  begin
    if IndexOf(AItem) < 0 then
      Add(AItem);
  end;
end;

//=== { TJvCustomPropertyStore } =============================================

constructor TJvCustomPropertyStore.Create(AOwner: TComponent);
const
  IgnorePropertyList: array[1..18] of string =
    (
    'AboutJVCL',
    'AppStorage',
    'AppStoragePath',
    'AutoLoad',
    'ClearBeforeLoad',
    'Name',
    'Tag',
    'Enabled',
    'ReadOnly',
    'DeleteBeforeStore',
    'IgnoreLastLoadTime',
    'IgnoreProperties',
    'OnBeforeLoadProperties',
    'OnAfterLoadProperties',
    'OnBeforeStoreProperties',
    'OnAfterStoreProperties',
    'SynchronizeLoadProperties',
    'SynchronizeStoreProperties'
    );
var
  I: Integer;
begin
  inherited Create(AOwner);
  FLastLoadTime := Now;
  FAppStorage := nil;
  FEnabled := True;
  FReadOnly := False;
  FDeleteBeforeStore := False;
  FAutoLoad := False;
  FIntIgnoreProperties := TStringList.Create;
  FIntIgnoreProperties.Sorted := True;
  FIntIgnoreProperties.Duplicates := dupIgnore;
  FIgnoreProperties := TJvIgnorePropertiesStringList.Create;
  FIgnoreProperties.Sorted := True;
  FIgnoreProperties.Duplicates := dupIgnore;
  FIgnoreLastLoadTime := False;
  FCombinedIgnoreProperties := TCombinedStrings.Create;
  FCombinedIgnoreProperties.Sorted := True;
  FCombinedIgnoreProperties.Duplicates := dupIgnore;
  for I := Low(IgnorePropertyList) to High(IgnorePropertyList) do
    FIntIgnoreProperties.Add(IgnorePropertyList[I]);
  FSynchronizeStoreProperties := False;
  FSynchronizeLoadProperties := False;
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

procedure TJvCustomPropertyStore.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAppStorage) then
    FAppStorage := nil;
end;

function TJvCustomPropertyStore.GetCombinedIgnoreProperties: TStringList;
var
  I: Integer;
begin
  FCombinedIgnoreProperties.clear;
  for I := 0 to FIntIgnoreProperties.Count - 1 do
    FCombinedIgnoreProperties.Add(FIntIgnoreProperties[i]);
  for I := 0 to FIgnoreProperties.Count - 1 do
    FCombinedIgnoreProperties.Add(FIgnoreProperties[i]);
  Result := FCombinedIgnoreProperties;
end;

function TJvCustomPropertyStore.GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Instance.ClassInfo);
  Result := Data.PropCount;
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
    Result := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo^.Name);
  finally
    FreeMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  end;
end;

procedure TJvCustomPropertyStore.CloneClassProperties(Src, Dest: TPersistent);
var
  Index: Integer;
  SrcPropInfo: PPropInfo;
  DestPropInfo: PPropInfo;
  PropName : String;

  function GetPropKind(PropInfo: PPropInfo): TTypeKind;
  begin
    Result := PropInfo.PropType^.Kind;
  end;

begin
  for Index := 0 to GetPropCount(Src) - 1 do
  begin
    PropName := GetPropName(Src, Index);
    if CompareText(PropName, 'Name') <> 0 then
    begin
      SrcPropInfo := GetPropInfo(Src.ClassInfo, PropName);
      DestPropInfo := GetPropInfo(Dest.ClassInfo, PropName);
      if (DestPropInfo <> nil) and (GetPropKind(DestPropInfo) = GetPropKind(SrcPropInfo)) and Assigned(DestPropInfo.SetProc) then
        case GetPropKind(DestPropInfo) of
          {$IFDEF UNICODE} tkUString, {$ENDIF}
          tkLString, tkString:
            SetStrProp(Dest, DestPropInfo, GetStrProp(Src, SrcPropInfo));
          tkInteger, tkChar, tkEnumeration, tkSet:
            SetOrdProp(Dest, DestPropInfo, GetOrdProp(Src, SrcPropInfo));
          tkFloat:
            SetFloatProp(Dest, DestPropInfo, GetFloatProp(Src, SrcPropInfo));
          tkVariant:
            SetVariantProp(Dest, DestPropInfo, GetVariantProp(Src, SrcPropInfo));
          tkClass:
            if GetObjectProp(Src, SrcPropInfo) is TPersistent then
              TPersistent(GetObjectProp(Dest,
                DestPropInfo)).Assign(TPersistent(GetObjectProp(Src, SrcPropInfo)));
          tkMethod:
            SetMethodProp(Dest, DestPropInfo, GetMethodProp(Src, SrcPropInfo));
        end;
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
    CloneClassProperties(Source, Self)
  else
    inherited Assign(Source);
end;

procedure TJvCustomPropertyStore.Clear;
begin
end;

function TJvCustomPropertyStore.Clone(AOwner: TComponent):
    TJvCustomPropertyStore;
begin
  Result := TJvCustomPropertyStoreClass(ClassType).Create(AOwner);
  Result.Assign(Self);
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
  else if Value <> AutoLoad then
    FAutoLoad := Value;
end;

procedure TJvCustomPropertyStore.DisableAutoLoadDown;
var
  Index: Integer;
  PropName: string;
  PropertyStore: TJvCustomPropertyStore;
begin
  for Index := 0 to GetPropCount(Self) - 1 do
  begin
    PropName := GetPropName(Self, Index);
    if not IgnoreProperty(PropName) then
    begin
      PropertyStore := GetPropertyJvCustomPropertyStore(PropName);
      if Assigned(PropertyStore) then
        PropertyStore.AutoLoad := False;
    end;
  end;
end;

function TJvCustomPropertyStore.EditIntf_DisplayProperty(const PropertyName: string): Boolean;
begin
  Result := not (IgnoreProperty(PropertyName));
end;

function TJvCustomPropertyStore.EditIntf_GetObjectHint: string;
begin
  Result := '';
end;

function TJvCustomPropertyStore.EditIntf_GetPropertyHint(const PropertyName: string): string;
begin
  Result := '';
end;

function TJvCustomPropertyStore.EditIntf_GetVisibleObjectName: string;
begin
  Result := '';
end;

function TJvCustomPropertyStore.EditIntf_IsPropertySimple(const PropertyName: string): Boolean;
var
  I: Integer;
  PropertyStore :TJvCustomPropertyStore;
begin
  if PropertyName = '' then
  begin
    Result := true;
    for I := 0 to GetPropCount(Self) - 1 do
      if EditIntf_DisplayProperty(GetPropName(Self, I)) then
      begin
        Result := EditIntf_IsPropertySimple(GetPropName(Self, I));
        if not Result then
          Exit;
      end;
  end
  else if IsPublishedProp(Self, PropertyName) then
  begin
    PropertyStore := GetPropertyJvCustomPropertyStore(PropertyName);
    if Assigned(PropertyStore) then
      if PropertyStore is TJvCustomPropertyListStore then
        Result := False
      else
        Result := PropertyStore.EditIntf_IsPropertySimple('')
    else
      Result := True
  end
  else
    Result := True;
end;

function TJvCustomPropertyStore.EditIntf_TranslatePropertyName(const PropertyName: string): string;
var
  s: string;
  I: Integer;
  c: string;
  lastLower: Boolean;
  LastBlank: Boolean;
begin
  s := '';
  LastLower := False;
  LastBlank := False;
  for I := 1 to Length(PropertyName) do
  begin
    c := Copy(PropertyName, i, 1);
    if (c = '_') or (c = '.') or (c = ' ') then
    begin
      s := s + ' ';
      LastBlank := True;
      LastLower := False;
    end
    else if (c = Uppercase(c)) then
    begin
      if LastLower and not LastBlank then
      begin
        s := s + ' ' + c;
        LastBlank := True;
      end
      else
      begin
        s := s + c;
        LastBlank := False;
      end;
      LastLower := False;
    end
    else
    begin
      s := s + c;
      LastLower := true;
      LastBlank := False;
    end
  end;
  Result := s;
end;

procedure TJvCustomPropertyStore.UpdateChildPaths(OldPath: string);
var
  Index: Integer;
  VisPropName: string;
  PropName: string;
  PropertyStore: TJvCustomPropertyStore;
begin
  if Assigned(AppStorage) then
  begin
    if OldPath = '' then
      OldPath := AppStoragePath;
    for Index := 0 to GetPropCount(Self) - 1 do
    begin
      PropName := GetPropName(Self, Index);
      if not IgnoreProperty(PropName) then
      begin
        PropertyStore := GetPropertyJvCustomPropertyStore (PropName);
        if Assigned(PropertyStore) then
        begin
          VisPropName := AppStorage.TranslatePropertyName(Self, PropName, False);
          if (PropertyStore.AppStoragePath = AppStorage.ConcatPaths([OldPath,
            VisPropName])) or (PropertyStore.AppStoragePath = '') then
            PropertyStore.AppStoragePath :=
              AppStorage.ConcatPaths([AppStoragePath, VisPropName]);
        end;
      end;
    end;
  end;
end;

procedure TJvCustomPropertyStore.SetAppStoragePath(Value: string);
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
  PropName: string;
  PropertyStore: TJvCustomPropertyStore;
begin
  if ReplaceComponentReference(Self, Value, tComponent(FAppStorage)) then
  begin
    for Index := 0 to GetPropCount(Self) - 1 do
    begin
      PropName := GetPropName(Self, Index);
      if not IgnoreProperty(PropName) then
      begin
        PropertyStore := GetPropertyJvCustomPropertyStore(PropName);
        if Assigned(PropertyStore) then
          PropertyStore.AppStorage := Value;
      end;
    end;
    UpdateChildPaths;
  end;
end;

function TJvCustomPropertyStore.GetIgnoreProperties: TJvIgnorePropertiesStringList;
begin
  Result := FIgnoreProperties;
end;

procedure TJvCustomPropertyStore.SetIgnoreProperties(Value: TJvIgnorePropertiesStringList);
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
    if AppStorage.ValueStored(AppStorage.ConcatPaths([AppStoragePath,
      cLastSaveTime])) then
      Result := AppStorage.ReadDateTime(AppStorage.ConcatPaths([AppStoragePath,
        cLastSaveTime]));
  except
    Result := 0;
  end;
end;

function TJvCustomPropertyStore.GetPropertyCount: Integer;
begin
  Result := GetPropCount(self);
end;

function TJvCustomPropertyStore.GetPropertyJvCustomPropertyStore(PropName: string): TJvCustomPropertyStore;
begin
  if (PropType(Self, PropName) = tkClass) and
     (TPersistent(GetObjectProp(Self, PropName)) is TJvCustomPropertyStore) then
        Result := TJvCustomPropertyStore(TPersistent(GetObjectProp(Self, PropName)))
  else
    Result := nil;
end;

function TJvCustomPropertyStore.GetPropertyName(Index: Integer): string;
begin
  Result := GetPropName(Self, Index);
end;

function TJvCustomPropertyStore.IgnoreProperty(const PropertyName: string):
  Boolean;
begin
  Result := (FIntIgnoreProperties.IndexOf(PropertyName) >= 0) or
    (IgnoreProperties.IndexOf(PropertyName) >= 0);
end;

procedure TJvCustomPropertyStore.LoadProperties;
var
  Mutex: TJclMutex;

  procedure ExecuteLoadProperties;
  begin
    AppStorage.BeginUpdate;
    try
      UpdateChildPaths;
      FLastLoadTime := Now;
      if ClearBeforeLoad then
        Clear;
      if Assigned(FOnBeforeLoadProperties) then
        FOnBeforeLoadProperties(Self);
      LoadData;
      AppStorage.ReadPersistent(AppStoragePath, Self, True, True,
        CombinedIgnoreProperties);
      if Assigned(FOnAfterLoadProperties) then
        FOnAfterLoadProperties(Self);
    finally
      AppStorage.EndUpdate;
    end;
  end;

begin
  if not Enabled then
    Exit;
  if not Assigned(AppStorage) then
    Exit;

  if SynchronizeLoadProperties then
  begin
    if SynchronizeStoreProperties then
      Mutex := TJclMutex.Create(nil, False,
        string(B64Encode(AnsiString(RsJvPropertyStoreMutexLoadStorePropertiesProcedureName +
          AppStoragePath))))
    else
      Mutex := TJclMutex.Create(nil, False,
        string(B64Encode(AnsiString(RsJvPropertyStoreMutexLoadPropertiesProcedureName +
          AppStoragePath))));
    try
      if Mutex.WaitForever = wrSignaled then
        try
          ExecuteLoadProperties;
        finally
          Mutex.Release;
        end
      else
        raise
          Exception.CreateResFmt(@RsJvPropertyStoreEnterMutexTimeout, [RsJvPropertyStoreMutexStorePropertiesProcedureName]);
    finally
      FreeAndNil(Mutex);
    end;
  end
  else
    ExecuteLoadProperties;
end;

procedure TJvCustomPropertyStore.StoreProperties;
var
  Mutex: TJclMutex;

  procedure ExecuteStoreProperties;
  var SaveProperties: Boolean;
  begin
    AppStorage.BeginUpdate;
    try
      UpdateChildPaths;
      DisableAutoLoadDown;
      SaveProperties := IgnoreLastLoadTime or (GetLastSaveTime < FLastLoadTime);
      if SaveProperties then
      begin
        if DeleteBeforeStore then
          AppStorage.DeleteSubTree(AppStoragePath);
        if StorePropertiesNow then
        begin
          if not IgnoreLastLoadTime then
            AppStorage.WriteString(AppStorage.ConcatPaths([AppStoragePath, cLastSaveTime]), DateTimeToStr(Now));
          if Assigned(FOnBeforeStoreProperties) then
            FOnBeforeStoreProperties(Self);
          StoreData;
          StoreXMLProperties;
          AppStorage.WritePersistent(AppStoragePath, Self, True, CombinedIgnoreProperties);
          if Assigned(FOnAfterStoreProperties) then
            FOnAfterStoreProperties(Self);
        end;
      end;
    finally
      AppStorage.EndUpdate;
    end;
  end;

begin
  if not Enabled then
    Exit;
  if ReadOnly then
    Exit;
  if not Assigned(AppStorage) then
    Exit;

  if SynchronizeStoreProperties then
  begin
    if SynchronizeLoadProperties then
      Mutex := TJclMutex.Create(nil, False,
        string(B64Encode(AnsiString(RsJvPropertyStoreMutexLoadStorePropertiesProcedureName +
          AppStoragePath))))
    else
      Mutex := TJclMutex.Create(nil, False,
        string(B64Encode(AnsiString(RsJvPropertyStoreMutexStorePropertiesProcedureName +
          AppStoragePath))));
    try
      if Mutex.WaitForever = wrSignaled then
        try
          ExecuteStoreProperties;
        finally
          Mutex.Release;
        end
      else
        raise
          Exception.CreateResFmt(@RsJvPropertyStoreEnterMutexTimeout, [RsJvPropertyStoreMutexStorePropertiesProcedureName]);
    finally
      FreeAndNil(Mutex);
    end;
  end
  else
    ExecuteStoreProperties;
end;

procedure TJvCustomPropertyStore.LoadData;
begin
end;

procedure TJvCustomPropertyStore.StoreData;
begin
end;

procedure TJvCustomPropertyStore.StoreXMLProperties;
begin
end;

function TJvCustomPropertyStore.StorePropertiesNow: Boolean;
begin
  Result := True;
end;

function TJvCustomPropertyStore.ValidateData: Boolean;
var
  PropName: string;
  PropertyStore: TJvCustomPropertyStore;
  Index: Integer;
begin
  Result := True;
  for Index := 0 to GetPropertyCount - 1 do
  begin
    PropName := GetPropertyName(Index);
    if not IgnoreProperty(PropName) then
    begin
      PropertyStore := GetPropertyJvCustomPropertyStore(PropName);
      if Assigned(PropertyStore) then
        Result := Result and PropertyStore.ValidateData;
    end;
  end;
end;

//=== { TJvCustomPropertyListStore } =========================================

constructor TJvCustomPropertyListStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := CreateItemList;
  CreateListEntries := True;
  FreeObjects := True;
  FItemName := cItem;
  FIntIgnoreProperties.Add('ItemName');
  FIntIgnoreProperties.Add('FreeObjects');
  FIntIgnoreProperties.Add('CreateListEntries');
  FItemsObjectName := 'ItemName';
end;

destructor TJvCustomPropertyListStore.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TJvCustomPropertyListStore.CreateAddObject(const aObjectName: String):
    TPersistent;
begin
  Result := CreateObject;
  Items.AddObject(aObjectName, Result);
end;

function TJvCustomPropertyListStore.GetItems: TStringList;
begin
  Result := FItems;
end;

procedure TJvCustomPropertyListStore.StoreData;
begin
  inherited StoreData;
  AppStorage.WriteList(AppStoragePath, nil, Count, WriteSLOItem, DeleteSLOItems, ItemName);
end;

procedure TJvCustomPropertyListStore.LoadData;
begin
  inherited LoadData;
  AppStorage.ReadList(AppStoragePath, nil, ReadSLOItem, ItemName);
end;

procedure TJvCustomPropertyListStore.Clear;
var
  I: Integer;
  obj : TObject;
begin
  if Assigned(Items) then
  begin
    if FreeObjects then
      for I := Count - 1 downto 0 do
        if Assigned(Objects[I]) then
        begin
          obj := Objects[I];
          Objects[I] := nil;
          obj.Free;
        end;
    Items.Clear;
  end;
  inherited Clear;
end;

function TJvCustomPropertyListStore.CreateItemList: TStringList;
begin
  Result := TStringList.Create;
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
  if Assigned(Items) then
    Result := Items.Count
  else
    Result := -1;
end;

function TJvCustomPropertyListStore.GetSorted: Boolean;
begin
  Result := FItems.Sorted;
end;

procedure TJvCustomPropertyListStore.SetSorted(Value: Boolean);
begin
  FItems.Sorted := Value;
end;

function TJvCustomPropertyListStore.GetDuplicates: TDuplicates;
begin
  Result := FItems.Duplicates;
end;

procedure TJvCustomPropertyListStore.SetDuplicates(Value: TDuplicates);
begin
  FItems.Duplicates := Value;
end;

procedure TJvCustomPropertyListStore.ReadSLOItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName:
    string);
var
  NewObject: TObject;
  NewObjectName: string;
  Obj: TObject;
begin
  if Index >= Count then
  begin
    if not CreateListEntries then
      Exit;
    NewObject := CreateObject;
    if Assigned(NewObject) then
    begin
      if NewObject is TJvCustomPropertyStore then
      begin
        TJvCustomPropertyStore(NewObject).AppStoragePath :=
          Sender.ConcatPaths([Path, Sender.ItemNameIndexPath(ItemName, Index)]);
        TJvCustomPropertyStore(NewObject).AppStorage := Sender;
        TJvCustomPropertyStore(NewObject).LoadProperties;
      end
      else if NewObject is TPersistent then
        Sender.ReadPersistent(Sender.ConcatPaths([Path,
          Sender.ItemNameIndexPath(ItemName, Index)]),
          TPersistent(NewObject), True, True, CombinedIgnoreProperties);
      if Sender.ValueStored(Sender.ConcatPaths([Path,
        Sender.ItemNameIndexPath(ItemName, Index), ItemsObjectName])) then
        NewObjectName := Sender.ReadString(Sender.ConcatPaths([Path,
          Sender.ItemNameIndexPath(ItemName, Index), ItemsObjectName]))
      else
        NewObjectName := '';
      Items.AddObject(NewObjectName, NewObject);
    end
    else
      Items.Add(Sender.ReadString(Sender.ConcatPaths([Path,
        Sender.ItemNameIndexPath(ItemName, Index)])))
  end
  else if Assigned(Objects[Index]) then
  begin
    Obj := Objects[Index];
    if Obj is TJvCustomPropertyStore then
    begin
      TJvCustomPropertyStore(Obj).AppStoragePath :=
        Sender.ConcatPaths([Path, Sender.ItemNameIndexPath(ItemName, Index)]);
      TJvCustomPropertyStore(Obj).AppStorage := Sender;
      TJvCustomPropertyStore(Obj).LoadProperties;
    end
    else if Obj is TPersistent then
      Sender.ReadPersistent(Sender.ConcatPaths([Path,
        Sender.ItemNameIndexPath(ItemName, Index)]),
        TPersistent(Obj), True, True, CombinedIgnoreProperties);
    if Sender.ValueStored(Sender.ConcatPaths([Path,
      Sender.ItemNameIndexPath(ItemName, Index), ItemsObjectName])) then
      Strings[Index] := Sender.ReadString(Sender.ConcatPaths([Path,
        Sender.ItemNameIndexPath(ItemName, Index), ItemsObjectName]))
    else
      Strings[Index] := '';
  end
  else
    Strings[Index] := Sender.ReadString(Sender.ConcatPaths([Path,
      Sender.ItemNameIndexPath(ItemName, Index)]));
end;

procedure TJvCustomPropertyListStore.WriteSLOItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName:
    string);
var
  Obj : TObject;
begin
  Obj := Objects[Index];
  if Assigned(Obj) then
  begin
    if Obj is TJvCustomPropertyStore then
    begin
      TJvCustomPropertyStore(Obj).AppStoragePath :=
        Sender.ConcatPaths([Path, Sender.ItemNameIndexPath(ItemName, Index)]);
      TJvCustomPropertyStore(Obj).AppStorage := Sender;
      TJvCustomPropertyStore(Obj).StoreProperties;
    end
    else if Obj is TPersistent then
      Sender.WritePersistent(Sender.ConcatPaths([Path,
        Sender.ItemNameIndexPath(ItemName, Index)]),
        TPersistent(Obj), True, CombinedIgnoreProperties);
    if Strings[Index] <> '' then
      Sender.WriteString(Sender.ConcatPaths([Path,
        Sender.ItemNameIndexPath(ItemName, Index), ItemsObjectName]), Strings[Index]);
  end
  else
    Sender.WriteString(Sender.ConcatPaths([Path,
      Sender.ItemNameIndexPath(ItemName, Index)]), Strings[Index]);
end;

procedure TJvCustomPropertyListStore.DeleteSLOItems(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const
    ItemName: string);
var
  I: Integer;
begin
  for I := Last downto first do
    Sender.DeleteValue(Sender.ConcatPaths([Path,
      Sender.ItemNameIndexPath(ItemName, i)]));
end;

function TJvCustomPropertyListStore.IndexOf(const s: string): Integer;
begin
  Result := FItems.IndexOf(s);
end;

function TJvCustomPropertyListStore.IndexOfObject(AObject: TObject): Integer;
begin
  Result := FItems.IndexOfObject(AObject);
end;

procedure TJvCustomPropertyListStore.ListEditIntf_MoveObjectPosition(CurIndex,
    NewIndex: Integer);
begin
  if (CurIndex >= 0) and (CurIndex < Count) and
     (NewIndex >= 0) and (NewIndex < Count) then
    Items.Move(CurIndex, NewIndex);
end;

function TJvCustomPropertyListStore.ListEditIntf_CloneNewObject(Index:
    integer): TPersistent;
var
  Obj: TObject;
begin
  if (Index >= 0) and (Index < Count) and Assigned(Objects[Index]) then
  begin
    Obj := Objects[Index];
    if (Obj is TJvCustomPropertyStore) then
    begin
      Result := TPersistent(TJvCustomPropertyStore(Obj).Clone(self));
      Items.AddObject ('New '+ItemName, Result);
    end
    else
    begin
      Result := ListEditIntf_CreateNewObject;
      if (Obj is TPersistent)then
        TPersistent(Result).Assign(TPersistent(Obj));
    end
  end
  else
    Result := nil;
end;

function TJvCustomPropertyListStore.ListEditIntf_CreateNewObject: TPersistent;
begin
  Result := CreateObject;
  Items.AddObject ('New '+ItemName, Result);
end;

procedure TJvCustomPropertyListStore.ListEditIntf_DeleteObject(Index: integer);
var obj : TObject;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    if Assigned(Objects[Index]) then
    begin
      obj := Objects[Index];
      Items.Objects[Index] := nil;
      obj.Free;
    end;
    Items.Delete(Index);
  end;
end;

function TJvCustomPropertyListStore.ListEditIntf_GetObject(Index: integer):
    TPersistent;
begin
  if (Index >= 0) and (Index < Count) and (Objects[Index] is TPersistent) then
    Result := TPersistent(Objects[Index])
  else
    Result := nil;
  ;
end;

function TJvCustomPropertyListStore.ListEditIntf_ObjectCount: integer;
begin
  Result := Count;
end;

function StringListSortCompareDesc(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if List[Index1] = List[Index2] then
    Result := 0
  else if List[Index1] < List[Index2] then
    Result := 1
  else
    Result := -1;
end;

procedure StorePropertyStorageToAppStorage(iPropertyStore: TJvCustomPropertyStore; iAppStorage: TJvCustomAppStorage; const
  iAppStoragePath: string);
var OldAppStorage : TJvCustomAppStorage;
    OldAppStoragePath : String;
begin
  if not Assigned(iPropertyStore) or not Assigned(iAppStorage) then
    Exit;
  OldAppStoragePath := iPropertyStore.AppStoragePath;
  OldAppStorage := iPropertyStore.AppStorage;
  try
    iPropertyStore.AppStoragePath := iAppStoragePath;
    iPropertyStore.AppStorage := iAppStorage;
    iPropertyStore.AppStorage.Reload;
    iPropertyStore.StoreProperties;
    iPropertyStore.AppStorage.Flush;
  finally
    iPropertyStore.AppStoragePath := OldAppStoragePath;
    iPropertyStore.AppStorage := OldAppStorage;
  end;
end;

function TJvCustomPropertyListStore.ListEditIntf_IndexOfObject(AObject : TPersistent): Integer;
begin
  Result := Items.IndexOfObject(AObject);
end;

procedure TJvCustomPropertyListStore.ListEditIntf_SortObjects(iAscending :
    Boolean);
begin
  if iAscending then
    FItems.Sort
  else
    FItems.CustomSort(StringListSortCompareDesc);
end;

function TJvCustomPropertyListStore.ValidateData: Boolean;
var
  Index: Integer;
begin
  Result := True;
  for Index := 0 to Count - 1 do
    if Assigned(Objects[Index]) and (Objects[Index] is TJvCustomPropertyStore) then
      Result := Result and TJvCustomPropertyStore(Objects[Index]).ValidateData;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
