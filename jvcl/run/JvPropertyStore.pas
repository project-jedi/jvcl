unit JvPropertyStore;

interface

uses Typinfo, Windows, Classes, JvComponent, JvAppStore, Graphics;


type
  tJvCustomPropertyStore = class(TJvComponent)
  private
    fPath: string;
    fAppStore: tJvCustomAppStore;
    fEnabled: boolean;
    fDeleteBeforeStore: boolean;
    fClearBeforeLoad: boolean;
    FIntIgnoreProperties: TStrings;
    FIgnoreProperties: TStrings;
    FAutoLoad: boolean;
    fLastLoadTime: tDateTime;
    fIgnoreLastLoadTime: boolean;
    fOnBeforeLoadProperties: TNotifyEvent;
    fOnAfterLoadProperties: TNotifyEvent;
    fOnBeforeStoreProperties: TNotifyEvent;
    fOnAfterStoreProperties: TNotifyEvent;
    procedure SetAutoLoad(Value: boolean);
    procedure SetIgnoreProperties(Value: TStrings);
    function GetPropCount(Instance: TPersistent): integer;
    function GetPropName(Instance: TPersistent; Index: integer): string;
    procedure CloneClass(Src, Dest: TPersistent);
    function GetLastSaveTime: tDateTime;
  protected
    procedure SetPath(Value: string); virtual;
    procedure SetAppStore(Value: tJvCustomAppStore);
    procedure Loaded; override;
    procedure DisableAutoLoadDown;

    function TranslatePropertyName(PropertyName: string): string; virtual;

    procedure LoadData; virtual;
    procedure StoreData; virtual;
    procedure CustomSave(pPropertyName: string;
      var pContinueSave: boolean); virtual;
    procedure CustomLoad(pPropertyName: string;
      var pContinueLoad: boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AppStore: tJvCustomAppStore Read fAppStore Write SetAppStore;
    procedure StoreProperties; virtual;
    procedure LoadProperties; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;

  Published
    property AutoLoad: boolean Read FAutoLoad Write SetAutoLoad;
    property Path: string Read fPath Write SetPath;
    property Enabled: boolean Read fEnabled Write fEnabled DEFAULT true;
    property DeleteBeforeStore: boolean Read fDeleteBeforeStore
      Write fDeleteBeforeStore DEFAULT false;
    property ClearBeforeLoad: boolean Read fClearBeforeLoad
      Write fClearBeforeLoad DEFAULT false;
    property IgnoreLastLoadTime: boolean Read fIgnoreLastLoadTime
      Write fIgnoreLastLoadTime;
    property IgnoreProperties: TStrings Read FIgnoreProperties Write SetIgnoreProperties;
    property OnBeforeLoadProperties: TNotifyEvent
      Read fOnBeforeLoadProperties Write fOnBeforeLoadProperties;
    property OnAfterLoadProperties: TNotifyEvent
      Read fOnAfterLoadProperties Write fOnAfterLoadProperties;
    property OnBeforeStoreProperties: TNotifyEvent
      Read fOnBeforeStoreProperties Write fOnBeforeStoreProperties;
    property OnAfterStoreProperties: TNotifyEvent
      Read fOnAfterStoreProperties Write fOnAfterStoreProperties;

  end;

  tJvCustomPropertyListStore = class(tJvCustomPropertyStore)
  private
    fItems: TStringList;
    fFreeObjects: boolean;
    fCreateListEntries: boolean;
  protected
    function GetString(Index: integer): string;
    function GetObject(Index: integer): TObject;
    procedure SetString(Index: integer; Value: string);
    procedure SetObject(Index: integer; Value: TObject);
    function GetCount: integer;
    procedure ReadSLOItem(Sender: TJvCustomAppStore; const Path: string;
      const Index: integer);
    procedure WriteSLOItem(Sender: TJvCustomAppStore; const Path: string;
      const Index: integer);
    procedure DeleteSLOItems(Sender: TJvCustomAppStore; const Path: string;
      const First, Last: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StoreData; override;
    procedure LoadData; override;
    procedure Clear; override;
    function CreateObject: TObject; virtual;
    property Strings [Index: integer]: string Read GetString Write SetString;
    property Objects[Index: integer]: TObject Read GetObject Write SetObject;
    property Items: TStringList Read fItems;
    property Count: integer Read GetCount;
  published
    { Defines if the Items.Objects- Objects will be freeded inside the clear procedure }
    property FreeObjects: boolean Read fFreeObjects Write fFreeObjects default true;
    { Defines it new List entries will be created if there are stored entries, which
      are not in the current object }
    property CreateListEntries: boolean Read fCreateListEntries
      Write fCreateListEntries default true;
  end;


implementation

uses
  JclRTTI, SysUtils{, menus};


const
  cLastSaveTime = 'Last Save Time';

{ What is this for? RegisterClass should be placed in an initialization section I think...
procedure Register;
begin
  RegisterNoIcon([tJvCustomPropertyStore]);
  RegisterClass(tJvCustomPropertyStore);
end;
}

 //-----------------------------------------------------------------------------
 //===tJvCustomPropertyStore====================================================

constructor tJvCustomPropertyStore.Create(AOwner: TComponent);
begin
  inherited Create(Aowner);
  fLastLoadTime := Now;
  fAppStore     := nil;
  fEnabled      := true;
  fDeleteBeforeStore := false;
  fAutoLoad     := false;
  FIntIgnoreProperties := TStringList.Create;
  FIgnoreProperties := TStringList.Create;
  fIgnoreLastLoadTime := false;
  FIntIgnoreProperties.Add('AboutJVCL');
  FIntIgnoreProperties.Add('Path');
  FIntIgnoreProperties.Add('AutoLoad');
  FIntIgnoreProperties.Add('Name');
  FIntIgnoreProperties.Add('Tag');
  FIntIgnoreProperties.Add('Enabled');
  FIntIgnoreProperties.Add('DeleteBeforeStore');
  FIntIgnoreProperties.Add('IgnoreLastLoadTime');
  FIntIgnoreProperties.Add('IgnoreProperties');
end;    {*** tJvCustomPropertyStore.create ***}

destructor tJvCustomPropertyStore.Destroy;
begin
  if not (csDesigning in ComponentState) then
    if AutoLoad then
      StoreProperties;
  FIntIgnoreProperties.Free;
  FIgnoreProperties.Free;
  Clear;
  inherited Destroy;
end;    {*** destructor tJvCustomPropertyStore.destroy ***}


procedure tJvCustomPropertyStore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = fAppStore) then
    fAppstore := nil;
end;


//Returns the number of properties of a given object
function tJvCustomPropertyStore.GetPropCount(Instance: TPersistent): integer;
var
  Data: PTypeData;
begin
  Data   := GetTypeData(Instance.Classinfo);
  Result := Data^.PropCount;
end;

//Returns the property name of an instance at a certain index
function tJvCustomPropertyStore.GetPropName(Instance: TPersistent;
  Index: integer): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data:     PTypeData;
begin
  Result := '';
  Data   := GetTypeData(Instance.Classinfo);
  GetMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result   := PropInfo^.Name;
  finally
    FreeMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  end;
end;

//Copy RTTI properties from one class to another
procedure tJvCustomPropertyStore.CloneClass(Src, Dest: TPersistent);
var
  Index: integer;
  SrcPropInfo: PPropInfo;
  DestPropInfo: PPropInfo;
begin
  for Index := 0 to GetPropCount(Src) - 1 do
  begin
    if (CompareText(GetPropName(Src, Index), 'Name') = 0) then
      Continue;
    SrcPropInfo  := GetPropInfo(Src.ClassInfo, GetPropName(Src, Index));
    DestPropInfo := GetPropInfo(Dest.ClassInfo, GetPropName(Src, Index));
    if DestPropInfo <> nil then
      if DestPropInfo^.PropType^.Kind = SrcPropInfo^.PropType^.Kind then
      begin
        case DestPropInfo^.PropType^.Kind of
          tkLString, tkString: SetStrProp(Dest, DestPropInfo,
              GetStrProp(Src, SrcPropInfo));
          tkInteger, tkChar, tkEnumeration, tkSet: SetOrdProp(
              Dest, DestPropInfo, GetOrdProp(Src, SrcPropInfo));
          tkFloat: SetFloatProp(Dest, DestPropInfo, GetFloatProp(Src, SrcPropInfo));
          tkVariant: SetVariantProp(Dest, DestPropInfo,
              GetVariantProp(Src, SrcPropInfo));
          tkClass:
          begin
            if (TPersistent(GetOrdProp(Src, SrcPropInfo)) is TStrings) and
              (TPersistent(GetOrdProp(Dest, DestPropInfo)) is TStrings) then
              TPersistent(GetOrdProp(Dest, DestPropInfo)).Assign(
                TPersistent(GetOrdProp(Src, SrcPropInfo)))
            else if (TPersistent(GetOrdProp(Src, SrcPropInfo)) is TPersistent) and
              (TPersistent(GetOrdProp(Dest, DestPropInfo)) is TPersistent) then
              TPersistent(GetOrdProp(Dest, DestPropInfo)).Assign(
                TPersistent(GetOrdProp(Src, SrcPropInfo)));
          end;
          tkMethod: SetMethodProp(Dest, DestPropInfo, GetMethodProp(Src, SrcPropInfo));
        end;
      end;
  end;
end;

procedure tJvCustomPropertyStore.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    if AutoLoad then
      LoadProperties;
end;

procedure tJvCustomPropertyStore.Assign(Source: TPersistent);
begin
  if Source is Self.ClassType then
    CloneClass(Source, Self)
  else
    inherited Assign(Source);
end;   {*** procedure tJvCustomPropertyStore.Assign ***}

procedure tJvCustomPropertyStore.Clear;
begin

end;

procedure tJvCustomPropertyStore.SetAutoLoad(Value: boolean);
begin
  fAutoLoad := Value;
  if not Assigned(Owner) then
    Exit;
  if Owner is tJvCustomPropertyStore then
    fAutoLoad := false;
end;   {*** procedure tJvCustomPropertyStore.SetAutoLoad ***}

procedure tJvCustomPropertyStore.DisableAutoLoadDown;
var
  Index:    integer;
  PropName: string;
begin
  for Index := 0 to GetPropCount(Self) - 1 do
  begin
    PropName := GetPropName(Self, Index);
    if (FIgnoreProperties.Indexof(Propname) >= 0) then
      Continue;
    if (FIntIgnoreProperties.Indexof(Propname) >= 0) then
      Continue;
    if PropType(Self, GetPropName(Self, Index)) = tkClass then
      if (TPersistent(GetOrdProp(Self, PropName)) is tJvCustomPropertyStore) then
        tJvCustomPropertyStore(TPersistent(GetOrdProp(Self, PropName))).AutoLoad :=
          false;
  end;   {*** for Index:=0 to GetPropCount(Self)-1 do ***}
end;

procedure tJvCustomPropertyStore.SetPath(Value: string);
var
  Index:    integer;
  VisPropName: string;
  PropName: string;
begin
 //  if Value = fPath then
 //    Exit;
  for Index := 0 to GetPropCount(Self) - 1 do
  begin
    PropName    := GetPropName(Self, Index);
    VisPropName := TranslatePropertyName(PropName);
    if (FIgnoreProperties.Indexof(Propname) >= 0) then
      Continue;
    if (FIntIgnoreProperties.Indexof(Propname) >= 0) then
      Continue;
    if PropType(Self, GetPropName(Self, Index)) = tkClass then
      if (TPersistent(GetOrdProp(Self, PropName)) is tJvCustomPropertyStore) then
        if (tJvCustomPropertyStore(TPersistent(GetOrdProp(Self, PropName))).Path =
          fPath + '\' + VisPropName) or
          (tJvCustomPropertyStore(TPersistent(GetOrdProp(Self, PropName))).Path = '') then
          tJvCustomPropertyStore(TPersistent(GetOrdProp(Self, PropName))).Path :=
            Value + '\' + VisPropName;
  end;   {*** for Index:=0 to GetPropCount(Self)-1 do ***}
  fPath := Value;
end;   {*** procedure tJvCustomPropertyStore.SetPath ***}

procedure TJvCustomPropertyStore.SetAppStore(Value: TJvCustomAppStore);
var
  Index: integer;
begin
  if Value = fAppStore then
    Exit;
  for Index := 0 to ComponentCount - 1 do
    if Components[Index] is TJvCustomPropertyStore then
      TJvCustomPropertyStore(Components[Index]).AppStore := Value;
  fAppStore := Value;
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
  if Path = '' then
    Exit;
  if Appstore.ValueStored(Path + '\' + cLastSaveTime) then
    Result := Appstore.ReadDateTime(Path + '\' + cLastSaveTime)
  else
    Result := 0;
end;

procedure tJvCustomPropertyStore.LoadProperties;
var
  Index:    integer;
  PropName: string;
  ContinueLoad: boolean;
  VisPropName: string;
begin
  if not Enabled then
    Exit;
  if not assigned(AppStore) then
    Exit;
  Path := Path;
  fLastLoadTime := Now;
  if ClearBeforeLoad then
    Clear;
  if Assigned(fOnBeforeLoadProperties) then
    fOnBeforeLoadProperties(self);
  LoadData;
  for Index := 0 to GetPropCount(Self) - 1 do
  begin
    PropName    := GetPropName(Self, Index);
    VisPropName := TranslatePropertyName(PropName);
    if (FIgnoreProperties.Indexof(Propname) >= 0) or
      (FIntIgnoreProperties.Indexof(Propname) >= 0) then
      Continue;
    CustomLoad(Propname, ContinueLoad);
    if not ContinueLoad then
      Continue;
    case PropType(Self, GetPropName(Self, Index)) of
      tkLString,
      tkWString,
      tkString: SetStrProp(Self, PropName, AppStore.ReadString(
          Path + '\' + VisPropName, GetStrProp(Self, PropName)));
      tkEnumeration,// : SetOrdProp(Self,Propname, AppStore.ReadEnumeration(Path+'\'+VisPropName, GetOrdProp(Self,PropName)));
      tkSet,
      tkChar,
      tkInteger: SetOrdProp(Self, Propname, AppStore.ReadInteger(
          Path + '\' + VisPropName, GetOrdProp(Self, PropName)));
      tkInt64: SetInt64Prop(Self, Propname, AppStore.ReadInteger(
          Path + '\' + VisPropName, GetInt64Prop(Self, PropName)));
      tkFloat: SetFloatProp(Self, Propname,
          AppStore.ReadFloat(Path + '\' + VisPropName, GetFloatProp(Self, PropName)));
      tkClass:
      begin
        if (TPersistent(GetOrdProp(Self, PropName)) is TStrings) then
          AppStore.ReadStringList(Path + '\' + VisPropName,
            TStrings(GetOrdProp(Self, PropName)))
        else if (TPersistent(GetOrdProp(Self, PropName)) is tJvCustomPropertyStore) then
          tJvCustomPropertyStore(TPersistent(GetOrdProp(Self, PropName))).LoadProperties
        else if (TPersistent(GetOrdProp(Self, PropName)) is TPersistent) then
          AppStore.ReadPersistent(Path + '\' + VisPropName,
            TPersistent(GetOrdProp(Self, PropName)));
      end;   {*** tkClass: ***}
    end;   {*** case PropType(Self,GetPropName(Self,Index)) of ***}
  end;   {*** for Index := 0 to GetPropCount(Self) - 1 do ***}
  if Assigned(fOnAfterLoadProperties) then
    fOnAfterLoadProperties(self);
end;

procedure tJvCustomPropertyStore.StoreProperties;
var
  Index:    integer;
  PropName: string;
  VisPropName: string;
  ContinueSave: boolean;
  SaveProperties: boolean;
begin
  if not Enabled then
    Exit;
  if not assigned(AppStore) then
    Exit;
  Path := Path;
  DisableAutoLoadDown;
  if not IgnoreLastLoadTime then
    SaveProperties := GetLastSaveTime < fLastLoadTime
  else
    SaveProperties := true;
  if DeleteBeforeStore then
    AppStore.DeleteSubTree(Path);
  if not IgnoreLastLoadTime then
    Appstore.WriteString(Path + '\' + cLastSaveTime, DateTimeToStr(Now));
  if Assigned(fOnBeforeStoreProperties) then
    fOnBeforeStoreProperties(self);
  if SaveProperties then
    StoreData;
  for Index := 0 to GetPropCount(Self) - 1 do
  begin
    PropName    := GetPropName(Self, Index);
    VisPropName := TranslatePropertyName(PropName);
    if (FIgnoreProperties.Indexof(Propname) >= 0) or
      (FIntIgnoreProperties.Indexof(Propname) >= 0) then
      Continue;
    if SaveProperties then
      CustomSave(Propname, ContinueSave);
    if not ContinueSave then
      Continue;
    case PropType(Self, GetPropName(Self, Index)) of
      tkLString,
      tkWString,
      tkString: if SaveProperties then
          AppStore.WriteString(Path + '\' + VisPropName, GetStrProp(Self, PropName));
      tkEnumeration,// : IF SaveProperties THEN AppStore.WriteEnumeration(Path+'\'+VisPropName,GetOrdProp(Self,PropName));
      tkSet,
      tkChar,
      tkInteger: if SaveProperties then
          AppStore.WriteInteger(Path + '\' + VisPropName, GetOrdProp(Self, PropName));
      tkInt64: if SaveProperties then
          AppStore.WriteString(Path + '\' + VisPropName,
            IntToStr(GetInt64Prop(Self, PropName)));
      tkFloat: if SaveProperties then
          AppStore.WriteFloat(Path + '\' + VisPropName, GetFloatProp(Self, PropName));
      tkClass:
      begin
        if (TPersistent(GetOrdProp(Self, PropName)) is TStrings) and SaveProperties then
          AppStore.WriteStringList(Path + '\' + VisPropName,
            TStrings(GetOrdProp(Self, PropName)))
        else if (TPersistent(GetOrdProp(Self, PropName)) is tJvCustomPropertyStore) then
          tJvCustomPropertyStore(
            TPersistent(GetOrdProp(Self, PropName))).StoreProperties
        else if (TPersistent(GetOrdProp(Self, PropName)) is TPersistent) and
          SaveProperties then
          AppStore.WritePersistent(Path + '\' + VisPropName,
            TPersistent(GetOrdProp(Self, PropName)));
      end;   {*** tkClass: ***}
    end;   {*** case PropType(Self,GetPropName(Self,Index)) of ***}
  end;   {*** for Index := 0 to GetPropCount(Self) - 1 do ***}
  if Assigned(fOnAfterStoreProperties) then
    fOnAfterStoreProperties(self);
end;


procedure tJvCustomPropertyStore.CustomSave(pPropertyName: string;
  var pContinueSave: boolean);
begin
  pContinueSave := true;
end;

procedure tJvCustomPropertyStore.CustomLoad(pPropertyName: string;
  var pContinueLoad: boolean);
begin
  pContinueLoad := true;
end;

procedure tJvCustomPropertyStore.LoadData;
begin
end;

procedure tJvCustomPropertyStore.StoreData;
begin
end;

function tJvCustomPropertyStore.TranslatePropertyName(PropertyName: string): string;
begin
  Result := PropertyName;
end;

 //-----------------------------------------------------------------------------
 //===tJvCustomPropertyListStore================================================
constructor tJvCustomPropertyListStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fItems      := TStringList.Create;
  CreateListEntries := true;
  FreeObjects := true;
  FIntIgnoreProperties.Add('FreeObjects');
  FIntIgnoreProperties.Add('CreateListEntries');
end;

destructor tJvCustomPropertyListStore.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure tJvCustomPropertyListStore.StoreData;
begin
  inherited StoreData;
  AppStore.WriteList(Path, Count, WriteSLOItem, DeleteSLOItems);
end;

procedure tJvCustomPropertyListStore.LoadData;
begin
  inherited LoadData;
  AppStore.ReadList(Path, ReadSLOItem);
end;

procedure tJvCustomPropertyListStore.Clear;
var
  i: integer;
begin
  if Assigned(fItems) then
  begin
    if FreeObjects then
      for i := 0 to Count - 1 do
        if Assigned(Objects[i]) then
          Objects[i].Free;
    Items.Clear;
  end;
  inherited Clear;
end;

function tJvCustomPropertyListStore.CreateObject: TObject;
begin
  Result := nil;
end;

function tJvCustomPropertyListStore.GetString(Index: integer): string;
begin
  Result := Items.Strings[Index];
end;

function tJvCustomPropertyListStore.GetObject(Index: integer): TObject;
begin
  Result := Items.Objects[Index];
end;

procedure tJvCustomPropertyListStore.SetString(Index: integer; Value: string);
begin
  Items.Strings[Index] := Value;
end;

procedure tJvCustomPropertyListStore.SetObject(Index: integer; Value: TObject);
begin
  Items.Objects[Index] := Value;
end;

function tJvCustomPropertyListStore.GetCount: integer;
begin
  Result := Items.Count;
end;

procedure tJvCustomPropertyListStore.ReadSLOItem(Sender: TJvCustomAppStore;
  const Path: string; const Index: integer);
var
  NewObject:  TObject;
  ObjectName: string;
begin
  if Index >= Count then
  begin
    if not CreateListEntries then
      Exit;
    if Sender.PathExists(Path + '\Object' + IntToStr(Index)) then
    begin
      NewObject := CreateObject;
      if Assigned(NewObject) then
      begin
        if NewObject is tJvCustomPropertyStore then
        begin
          tJvCustomPropertyStore(NewObject).Path := Path + '\Object' + IntToStr(Index);
          tJvCustomPropertyStore(NewObject).LoadProperties;
        end
        else if NewObject is TPersistent then
          Sender.ReadPersistent(Path + '\Object' + IntToStr(Index),
            TPersistent(NewObject));
      end;
      if Sender.ValueStored(Path + '\Item' + IntToStr(Index)) then
        ObjectName := Sender.ReadString(Path + '\Item' + IntToStr(Index))
      else
        ObjectName := '';
      Items.AddObject(ObjectName, NewObject);
    end
    else
      Items.Add(Sender.ReadString(Path + '\Item' + IntToStr(Index)))
  end
  else if Sender.ValueStored(Path + '\Object' + IntToStr(Index)) then
  begin
    if Assigned(Objects[Index]) then
    begin
      if Objects[Index] is tJvCustomPropertyStore then
      begin
        tJvCustomPropertyStore(Objects[Index]).Path :=
          Path + '\Object' + IntToStr(Index);
        tJvCustomPropertyStore(Objects[Index]).LoadProperties;
      end
      else if Objects[Index] is TPersistent then
        Sender.ReadPersistent(Path + '\Object' + IntToStr(Index),
          TPersistent(Objects[Index]));
    end;
    if Sender.ValueStored(Path + '\Item' + IntToStr(Index)) then
      Strings[Index] := Sender.ReadString(Path + '\Item' + IntToStr(Index))
    else
      Strings[Index] := '';
  end
  else
    Strings[Index] := Sender.ReadString(Path + '\Item' + IntToStr(Index));
end;

procedure tJvCustomPropertyListStore.WriteSLOItem(Sender: TJvCustomAppStore;
  const Path: string; const Index: integer);
begin
  if Assigned(Objects[Index]) then
  begin
    if Objects[Index] is tJvCustomPropertyStore then
    begin
      tJvCustomPropertyStore(Objects[Index]).Path := Path + '\Object' + IntToStr(Index);
      tJvCustomPropertyStore(Objects[Index]).StoreProperties;
    end
    else if Objects[Index] is TPersistent then
      Sender.WritePersistent(Path + '\Object' + IntToStr(Index),
        TPersistent(Objects[Index]));
    if Strings[Index] <> '' then
      Sender.WriteString(Path + '\Item' + IntToStr(Index), Strings[Index]);
  end
  else
    Sender.WriteString(Path + '\Item' + IntToStr(Index), Strings[Index]);
end;

procedure tJvCustomPropertyListStore.DeleteSLOItems(Sender: TJvCustomAppStore;
  const Path: string; const First, Last: integer);
var
  I: integer;
begin
  for I := First to Last do
  begin
    Sender.DeleteValue(Path + '\Item' + IntToStr(I));
    Sender.DeleteValue(Path + '\Object' + IntToStr(I));
  end;
end;


end.
