unit CompDescription;

interface
uses classes, TypInfo, dsgnintf;

type
  TPropInfos = class;
  TPropInform = class;

  TComponentDescription = class(TComponent)
  private
    FPropInfos: TPropInfos;
    FNote: string;
    FClass_Name: string;

    PropList: PPropList;
    NumProps: word;
  public
    constructor Create(AOwner, Component: TComponent);
    destructor Destroy; override;
    procedure LoadProperties(Component: TComponent);
  published
    property Class_Name: string read FClass_Name write FClass_Name;
    property Note: string read FNote write FNote;
    property PropInfos: TPropInfos read FPropInfos write FPropInfos;
  end;

  TPropInfos = class(TCollection)
  private
    procedure AddPropInfo(PropInfo: PPropInfo; Component: TComponent);
    function GetItem(Index: Integer): TPropInform;
    procedure SetItem(Index: Integer; const Value: TPropInform);
  public
    function Add: TPropInform;
    function Insert(Index: Integer): TPropInform;
    property Items[Index: Integer]: TPropInform read GetItem  write SetItem; default;
  end;

  TPropInform = class(TCollectionItem)
  private
    FName: string;
    FTypeName: string;
    FTypeKind: TTypeKind;
    FChecked: boolean;
    FMakeHref: boolean;
    FNote: string;
  public
    Info: PPropInfo;
  published
    property Name: string read FName write FName;
    property TypeName: string read FTypeName write FTypeName;
    property TypeKind: TTypeKind read FTypeKind write FTypeKind;
    property Checked: boolean read FChecked write FChecked;
    property MakeHref: boolean read FMakeHref write FMakeHref;
    property Note: string read FNote write FNote;
  end;

implementation


constructor TComponentDescription.Create(AOwner, Component: TComponent);
begin
  inherited Create(AOwner);
  PropInfos := TPropInfos.Create(TPropInform);
  LoadProperties(Component);
end;

destructor TComponentDescription.Destroy;
begin
  FreeMem(PropList, NumProps*sizeof(pointer));
  PropInfos.Free;
  inherited;
end;

procedure TComponentDescription.LoadProperties(Component: TComponent);
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  i, j: integer;
  AName, PropName, sPropValue: string;
  PropObject: TObject;
begin
  if NumProps > 0 then
    FreeMem(PropList, NumProps*sizeof(pointer));

  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := TypeInf^.Name;
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  GetMem(PropList, NumProps*sizeof(pointer));
  try
    { Получаем список свойств }
    GetPropInfos(TypeInf, PropList);

    for i := 0 to NumProps-1 do
    begin
      PropInfos.AddPropInfo(PropList^[i], Component);

      PropName := PropList^[i]^.Name;

      PropTypeInf := PropList^[i]^.PropType^;
      PropInfo := PropList^[i];
    end;
  finally

  end;
end;

procedure TPropInfos.AddPropInfo(PropInfo: PPropInfo; Component: TComponent);
var
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
  j: integer;
  sNote: string;
begin
  with TPropInform(Add) do
  begin
    Name := PropInfo^.Name;
    TypeName := PropInfo^.PropType^.Name;
    TypeKind := PropInfo^.PropType^.Kind;
    Info := PropInfo;
    sNote := '';

    if TypeKind in [tkEnumeration, tkSet] then
    begin
      if TypeKind = tkSet then
        TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^
      else
        TypeInfo := PropInfo.PropType^;

      TypeData := GetTypeData(TypeInfo);

      for j := TypeData^.MinValue to TypeData^.MaxValue do
       begin
         if sNote <> '' then if TypeKind = tkSet then sNote := sNote + ' | ' else sNote := sNote + ', ';
         sNote := sNote + GetEnumName(TypeInfo, j);
       end;
      sNote := '[' + sNote + ']';
    end;
    Note := sNote;

    Checked := not IsPublishedProp(Component.ClassParent, Name);
  end;
end;

function TPropInfos.Add: TPropInform;
begin
  Result := TPropInform(inherited Add);
end;

function TPropInfos.Insert(Index: Integer): TPropInform;
begin
  Result := TPropInform(inherited Insert(Index));
end;

function TPropInfos.GetItem(Index: Integer): TPropInform;
begin
  Result := TPropInform(inherited Items[Index]);
end;

procedure TPropInfos.SetItem(Index: Integer; const Value: TPropInform);
begin
  Items[Index].Assign(Value);
end;

end.
