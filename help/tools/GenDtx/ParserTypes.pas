unit ParserTypes;

interface

uses
  Classes, Contnrs;

const
  CParamDescription = 'Description for this parameter';
  CItemDescription = '  Description for %s'#13#10;
type
  { -- }
  TClassVisibility = (inPrivate, inProtected, inPublic, inPublished);
  TClassVisibilities = set of TClassVisibility;

  TDelphiType = (dtClass, dtConst, dtDispInterface, dtFunction, dtFunctionType,
    dtInterface, dtMethodFunc, dtMethodProc, dtProcedure, dtProcedureType,
    dtProperty, dtRecord, dtResourceString, dtEnum, dtType, dtVar,
    dtClassField, dtMetaClass);
  TDelphiTypes = set of TDelphiType;

  TMethodType = (mtNormal, mtConstructor, mtDestructor);
  TDirective = (diAbstract, diCdecl, diDynamic, diObject, diOf, diOverload,
    diOverride, diPascal, diRegister, diReintroduce, diSafecall, diStdcall,
    diVirtual, diAssembler, diDeprecated, diPlatform, diForward, diExport);
  TDirectives = set of TDirective;

  TPropertySpecifier = (psIndex, psRead, psWrite, psStored, psDefault, psNoDefault, psImplements);
  TPropertySpecifiers = set of TPropertySpecifier;
  { -- Filter }
  TDuplicatesType = (dtHide, dtHideCaseSensitive, dtOnlyDuplicates,
    dtOnlyCaseSensitiveDuplicates, dtAll);
  TClassPropertyType = (cptInherited, cptNonInherited);
  TClassPropertyTypes = set of TClassPropertyType;

  TFourState = (fsNo, fsDontCare, fsOneOf, fsYes);
  TTriState = (tsNo, tsDontCare, tsYes);

  TFilterData = record
    RShow: TDelphiTypes;
    RDuplicates: TDuplicatesType;
    { Class }
    RClass_DescendantOf: string;
    { Property }
    RProperty_ShowInherited: TTriState;
    RProperty_ShowArray: TTriState;
    RProperty_MustExcludeSpecifiers: TPropertySpecifiers;
    RProperty_MustIncludeOneOfSpecifiers: TPropertySpecifiers;
    RProperty_MustIncludeSpecifiers: TPropertySpecifiers;
    RProperty_Scope: TClassVisibilities;
    RProperty_In: TStringList;
    { Method-procedure }
    RMethodProcedure_ShowConstructor: TTriState;
    RMethodProcedure_ShowDestructor: TTriState;
    RMethodProcedure_ShowClassMethod: TTriState;
    RMethodProcedure_Scope: TClassVisibilities;
    RMethodProcedure_MustExcludeDirectives: TDirectives;
    RMethodProcedure_MustIncludeOneOfDirectives: TDirectives;
    RMethodProcedure_MustIncludeDirectives: TDirectives;
    RMethodProcedure_MinimalParamCount: Integer; { -1 -> ignore }
    RMethodProcedure_MaximalParamCount: Integer; { -1 -> ignore }
    { Method-function }
    RMethodFunction_ShowClassMethod: TTriState;
    RMethodFunction_Scope: TClassVisibilities;
    RMethodFunction_MustExcludeDirectives: TDirectives;
    RMethodFunction_MustIncludeOneOfDirectives: TDirectives;
    RMethodFunction_MustIncludeDirectives: TDirectives;
    RMethodFunction_MinimalParamCount: Integer; { -1 -> ignore }
    RMethodFunction_MaximalParamCount: Integer; { -1 -> ignore }
    { Procedure }
    RProcedure_ShowClassMethod: TTriState;
    RProcedure_MustIncludeDirectives: TDirectives;
    RProcedure_MustIncludeOneOfDirectives: TDirectives;
    RProcedure_MustExcludeDirectives: TDirectives;
    { Function }
    RFunction_ShowClassMethod: TTriState;
    RFunction_MustIncludeDirectives: TDirectives;
    RFunction_MustIncludeOneOfDirectives: TDirectives;
    RFunction_MustExcludeDirectives: TDirectives;
  end;
  { -- }

const
  CDirectives: array[TDirective] of string =
  ('abstract', 'cdecl', 'dynamic', 'object', 'of', 'overload', 'override',
    'pascal', 'register', 'reintroduce', 'safecall', 'stdcall', 'virtual',
    'assembler', 'deprecated', 'platform', 'forward', 'export');

  CPropertySpecifiers: array[TPropertySpecifier] of string =
  ('index', 'read', 'write', 'stored', 'default', 'nodefault', 'implements');
type
  TAbstractItem = class;

  TTypeList = class(TList)
  private
    FAuthor: string;
    FFileName: string;
    function GetItem(Index: Integer): TAbstractItem;
    procedure SetItem(Index: Integer; const Value: TAbstractItem);
  public
    destructor Destroy; override;
    function Add(AItem: TAbstractItem): Integer;
    function IndexOfName(const SimpleName: string): Integer;
    procedure Clear; override;
    procedure SortIt;
    procedure CalculateCombines;
    property Items[Index: Integer]: TAbstractItem read GetItem write SetItem;
    default;
    property Author: string read FAuthor write FAuthor;
    property FileName: string read FFileName write FFileName;
  end;

  (*function GetTitleStr(AItem: TAbstractItem): string;
  const
    CTitleFunction = '<TITLE %s function>';
    CTitleProcedure = '<TITLE %s procedure>';
    CTitleType = '<TITLE %s type>';
  begin
    case AItem.DelphiType of
      dtFunction:
        Result := Format(CTitleFunction, [AItem.Name]);
      dtProcedure:
        Result := Format(CTitleProcedure, [AItem.Name]);
      dtType, dtRecord, dtEnum, dtProcedureType, dtFunctionType:
        Result := Format(CTitleType, [AItem.Name]);
    else
      Result := '';
    end;
  end;*)

  TAbstractItem = class(TObject)
  private
    FTypeList: TTypeList;
    FSimpleName: string;
    FCombineList: TObjectList;
    FCombineWithList: TObjectList;
    function GetParamString: string;
    function GetRealParamString: string;

    function GetAddDescriptionString: string; virtual;
    function GetAddSummaryString: string; virtual;
    function GetCanCombine: Boolean;
    function GetClassString: string; virtual;
    function GetCombineCount: Integer;
    function GetCombineString: string; virtual;
    function GetCombineWithCount: Integer;
    function GetDelphiType: TDelphiType; virtual; abstract;
    function GetItemsString: string; virtual;
    function GetParamList: TStrings; virtual;
    function GetReferenceName: string; virtual;
    function GetSortName: string; virtual;
    function GetTitleName: string; virtual;
    function GetValueString: string; virtual;
    function GetRealParamList: TStrings; virtual;
  protected
    procedure AddCombine(AItem: TAbstractItem);
    procedure AddCombineWith(AItem: TAbstractItem);
  public
    constructor Create(const AName: string); virtual;

    function IncludedInFilter(const AData: TFilterData): Boolean; virtual;

    property TypeList: TTypeList read FTypeList;
    { Simple name,   zonder . zonder @ }
    property SimpleName: string read FSimpleName write FSimpleName;
    { Reference name       met @ met . }
    property ReferenceName: string read GetReferenceName;
    { Title name     zonder . met 'function', 'type', 'procedure' }
    property TitleName: string read GetTitleName;
    property SortName: string read GetSortName;
    property DelphiType: TDelphiType read GetDelphiType;
    property ItemsString: string read GetItemsString;
    property ParamString: string read GetParamString;
    property RealParamString: string read GetRealParamString;
    property ParamList: TStrings read GetParamList;
    property RealParamList: TStrings read GetRealParamList;
    property ValueString: string read GetValueString;
    property ClassString: string read GetClassString;
    property CombineString: string read GetCombineString;
    property AddDescriptionString: string read GetAddDescriptionString;
    property AddSummaryString: string read GetAddSummaryString;
    { Voor function of object type > 0 als > 1 dan CanCombine = false }
    property CombineCount: Integer read GetCombineCount;
    { Voor event property = 1 }
    property CombineWithCount: Integer read GetCombineWithCount;
    property CanCombine: Boolean read GetCanCombine;
    { Returns CombineList }
  end;

  TValueItem = class(TAbstractItem)
  private
    FValue: string;
    function GetValueString: string; override;
    procedure SetValue(const AValue: string);
  protected
    function FormatValue(const S: string): string;
  public
    property Value: string read FValue write SetValue;
  end;

  TListItem = class(TAbstractItem)
  private
    FItems: TStringList;
    function GetItemsString: string; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    { For compare }
    procedure AddToList(AStrings: TStrings);
    property Items: TStringList read FItems;
  end;

  TBaseFuncItem = class(TAbstractItem)
  private
    FParams: TStringList;
    FParamTypes: TStringList;
    FDirectives: TDirectives;
    //function GetRealParamString: string; override;
    function GetReferenceName: string; override;
    function GetAddDescriptionString: string; override;
    function GetRealParamList: TStrings; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    property Params: TStringList read FParams;
    property ParamTypes: TStringList read FParamTypes;
    property Directives: TDirectives read FDirectives write FDirectives;
  end;

  TClassItem = class;

  TClassMemberOrFieldItem = class(TAbstractItem)
  private
    FOwnerClass: TClassItem;
    FPosition: TClassVisibility;
    function GetAddDescriptionString: string; override;
    function GetReferenceName: string; override;
    function GetClassString: string; override;
  public
    function IncludedInFilter(const AData: TFilterData): Boolean; override;

    property OwnerClass: TClassItem read FOwnerClass write FOwnerClass;
    property Position: TClassVisibility read FPosition write FPosition;
  end;

  TParamClassMethodItem = class(TClassMemberOrFieldItem)
  private
    FParams: TStringList;
    FParamTypes: TStringList;
    FDirectives: TDirectives;
    FIsClassMethod: Boolean;
    function GetRealParamList: TStrings; override;
    //function GetRealParamString: string; override;
    function GetReferenceName: string; override;
    function GetAddDescriptionString: string; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    property Params: TStringList read FParams;
    property ParamTypes: TStringList read FParamTypes;
    property Directives: TDirectives read FDirectives write FDirectives;
    property IsClassMethod: Boolean read FIsClassMethod write FIsClassMethod;
  end;

  TClassItem = class(TAbstractItem)
  private
    FList: TList;
    FAncestor: string;
    FInterfaces: TStrings;
    function GetItem(Index: Integer): TAbstractItem;
    procedure SetItem(Index: Integer; const Value: TAbstractItem);
    function GetDelphiType: TDelphiType; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    function IncludedInFilter(const AData: TFilterData): Boolean; override;

    procedure AddProcedure(AItem: TClassMemberOrFieldItem);
    procedure AddFunction(AItem: TClassMemberOrFieldItem);
    procedure AddProperty(AItem: TClassMemberOrFieldItem);
    property Items[Index: Integer]: TAbstractItem read GetItem write SetItem; default;
    property Ancestor: string read FAncestor write FAncestor;
    property Interfaces: TStrings read FInterfaces;
  end;

  TInterfaceItem = class(TClassItem)
  private
    function GetDelphiType: TDelphiType; override;
  end;

  TConstItem = class(TValueItem)
  private
    function GetDelphiType: TDelphiType; override;
  end;

  TFunctionItem = class(TBaseFuncItem)
  private
    function GetTitleName: string; override;
    function GetDelphiType: TDelphiType; override;
  public
    function IncludedInFilter(const AData: TFilterData): Boolean; override;
  end;

  TFunctionTypeItem = class(TBaseFuncItem)
  private
    function GetAddDescriptionString: string; override;
    function GetTitleName: string; override;
    function GetDelphiType: TDelphiType; override;
  end;

  TMethodFuncItem = class(TParamClassMethodItem)
  private
    function GetDelphiType: TDelphiType; override;
  public
    function IncludedInFilter(const AData: TFilterData): Boolean; override;
  end;

  TMethodProcItem = class(TParamClassMethodItem)
  private
    FMethodType: TMethodType;
    function GetDelphiType: TDelphiType; override;
    function GetSortName: string; override;
  public
    function IncludedInFilter(const AData: TFilterData): Boolean; override;

    property MethodType: TMethodType read FMethodType write FMethodType;
  end;

  TClassPropertyItem = class(TClassMemberOrFieldItem)
  private
    FIsArray: Boolean;
    FIsInherited: Boolean;
    FSpecifiers: TPropertySpecifiers;
    FTypeStr: string;
    FParamTypes: TStringList;
    FParams: TStringList;
    function GetAddDescriptionString: string; override;
    function GetDelphiType: TDelphiType; override;
    function GetParamList: TStrings; override;
    function GetParams: TStringList;
    function GetParamTypes: TStringList;
    function GetRealParamList: TStrings; override;
  public
    destructor Destroy; override;

    function IncludedInFilter(const AData: TFilterData): Boolean; override;

    property IsArray: Boolean read FIsArray write FIsArray;
    property IsInherited: Boolean read FIsInherited write FIsInherited;
    property Params: TStringList read GetParams;
    property ParamTypes: TStringList read GetParamTypes;
    property Specifiers: TPropertySpecifiers read FSpecifiers write FSpecifiers;
    property TypeStr: string read FTypeStr write FTypeStr;
  end;

  TClassFieldItem = class(TClassMemberOrFieldItem)
  private
    FTypeStr: string;
    function GetDelphiType: TDelphiType; override;
  public
    property TypeStr: string read FTypeStr write FTypeStr;
  end;

  TProcedureItem = class(TBaseFuncItem)
  private
    function GetTitleName: string; override;
    function GetDelphiType: TDelphiType; override;
  public
    function IncludedInFilter(const AData: TFilterData): Boolean; override;
  end;

  TProcedureTypeItem = class(TBaseFuncItem)
  private
    function GetAddDescriptionString: string; override;
    function GetTitleName: string; override;
    function GetDelphiType: TDelphiType; override;
  end;

  TRecordItem = class(TListItem)
  private
    function GetTitleName: string; override;
    function GetDelphiType: TDelphiType; override;
  end;

  TResourceStringItem = class(TValueItem)
  private
    function GetDelphiType: TDelphiType; override;
  end;

  TEnumItem = class(TListItem)
  private
    function GetTitleName: string; override;
    function GetDelphiType: TDelphiType; override;
  end;

  TTypeItem = class(TValueItem)
  private
    function GetAddDescriptionString: string; override;
    function GetDelphiType: TDelphiType; override;
    function GetTitleName: string; override;
  end;

  TMetaClassItem = class(TTypeItem)
  private
    function GetAddDescriptionString: string; override;
    function GetAddSummaryString: string; override;
    function GetDelphiType: TDelphiType; override;
  end;

  TVarItem = class(TValueItem)
  private
    function GetDelphiType: TDelphiType; override;
    function GetTitleName: string; override;
  end;

implementation

uses
  SysUtils, Settings,
  Math; // voor Max

const
  CTitleFunction = '%s function';
  CTitleProcedure = '%s procedure';
  CTitleType = '%s type';
  CTitleVariable = '%s variable';

function TriStateOk(const ATriState: TTriState; const ABool: Boolean): Boolean;
begin
  Result := (ATriState = tsDontCare) or (ABool = (ATriState = tsYes));
end;

function FillTo(const S: string; Count: Integer): string;
begin
  SetLength(Result, Count);
  FillChar(PChar(Result)^, Count, ' ');
  Move(PChar(S)^, PChar(Result)^, Length(S));
end;

function ParamListToString(AStrings: TStrings): string;
var
  I: Integer;
  MaxLength: Integer;
begin
  Result := '';
  if (AStrings = nil) or (AStrings.Count = 0) then
    Exit;

  MaxLength := -1;
  for I := 0 to AStrings.Count - 1 do
    MaxLength := Max(MaxLength, Length(AStrings[I]));
  Inc(MaxLength);
  for I := 0 to AStrings.Count - 1 do
    Result := Result + '  ' + FillTo(AStrings[I], MaxLength) + '- ' + CParamDescription + #13#10;
  { Laatste return eraf halen }
  Delete(Result, Length(Result) - 1, 2);
end;

{ TAbstractItem }

procedure TAbstractItem.AddCombine(AItem: TAbstractItem);
begin
  if not Assigned(FCombineList) then
    FCombineList := TObjectList.Create(False);

  FCombineList.Add(AItem);
end;

procedure TAbstractItem.AddCombineWith(AItem: TAbstractItem);
begin
  if not Assigned(FCombineWithList) then
    FCombineWithList := TObjectList.Create(False);

  FCombineWithList.Add(AItem);
end;

constructor TAbstractItem.Create(const AName: string);
begin
  FSimpleName := AName;
end;

function TAbstractItem.GetAddDescriptionString: string;
begin
  Result := '';
end;

function TAbstractItem.GetAddSummaryString: string;
begin

end;

function TAbstractItem.GetCanCombine: Boolean;
begin
  Result := CombineCount = 1;
end;

(*function TAbstractItem.GetCanCombineWith: Boolean;
var
  I: Integer;
begin
  Result := Assigned(FCombineWithList) and (CombineWithCount > 0);

  if not Result then
    Exit;

  Result := False;
  for I := 0 to FCombineWithList.Count - 1 do
    Result := Result or (TAbstractItem(FCombineWithList[I]).CombineCount = 1);
end;*)

function TAbstractItem.GetClassString: string;
begin
  Result := '';
end;

function TAbstractItem.GetCombineCount: Integer;
begin
  if Assigned(FCombineList) then
    Result := FCombineList.Count
  else
    Result := 0;
end;

function TAbstractItem.GetCombineString: string;
begin
  if Assigned(FCombineList) and (FCombineList.Count = 1) then
    Result := TAbstractItem(FCombineList[0]).ReferenceName
  else
    Result := '';
end;

function TAbstractItem.GetCombineWithCount: Integer;
begin
  if Assigned(FCombineWithList) then
    Result := FCombineWithList.Count
  else
    Result := 0;
end;

(*function TAbstractItem.GetCombineWithString: string;
begin
  Result := '';
end;*)

function TAbstractItem.GetItemsString: string;
begin
  Result := '';
end;

function TAbstractItem.GetParamList: TStrings;
begin
  if CanCombine then
    Result := nil
  else
    Result := RealParamList;
end;

function TAbstractItem.GetParamString: string;
begin
  Result := ParamListToString(ParamList);
  {if CanCombine then
    Result := ''
  else
    Result := RealParamString;}
end;

function TAbstractItem.GetRealParamList: TStrings;
begin
  Result := nil;
end;

function TAbstractItem.GetRealParamString: string;
begin
  Result := '';
end;

function TAbstractItem.GetReferenceName: string;
begin
  Result := SimpleName;
end;

function TAbstractItem.GetSortName: string;
begin
  { Standaard de complete naam gebruiken, bij constructors, destructors
    doen we wat anders }
  Result := ReferenceName;
end;

function TAbstractItem.GetTitleName: string;
begin
  Result := '';
end;

function TAbstractItem.GetValueString: string;
begin
  Result := '';
end;

function TAbstractItem.IncludedInFilter(const AData: TFilterData): Boolean;
begin
  Result := DelphiType in AData.RShow;
end;

{ TBaseFuncItem }

constructor TBaseFuncItem.Create(const AName: string);
begin
  inherited Create(AName);
  FParams := TStringList.Create;
  FParamTypes := TStringList.Create;
end;

destructor TBaseFuncItem.Destroy;
begin
  FParams.Free;
  FParamTypes.Free;
  inherited;
end;

function TBaseFuncItem.GetReferenceName: string;
var
  I: Integer;
begin
  Result := inherited GetReferenceName;

  if not (diOverload in Directives) then
    Exit;

  for I := 0 to FParamTypes.Count - 1 do
    Result := Result + '@' + FParamTypes[I];
end;

{function TBaseFuncItem.GetRealParamString: string;
begin
  Result := ParamListToString(FParams);
end;}

function TBaseFuncItem.GetAddDescriptionString: string;
begin
  if diOverload in Directives then
    Result :=
      '  This is an overloaded function/procedure, if possible you may combine the description'#13#10 +
      '  of all these functions into 1 general description. If you do so, combine all "Parameter" '#13#10 +
      '  lists into 1 list, and leave the "Summary", "Description" etc. fields empty for all'#13#10 +
      '  other overloaded functions with the same name.'#13#10
  else
    Result := '';
end;

{function TBaseFuncItem.GetParamList: TStrings;
begin
end;}

function TBaseFuncItem.GetRealParamList: TStrings;
begin
  Result := FParams;
end;

{ TListItem }

procedure TListItem.AddToList(AStrings: TStrings);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    AStrings.Add('@@' + ReferenceName + '.' + FItems[I]);
end;

constructor TListItem.Create(const AName: string);
begin
  inherited Create(AName);
  FItems := TStringList.Create;
end;

destructor TListItem.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TListItem.GetItemsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FItems.Count - 1 do
    Result := Result + '@@' + ReferenceName + '.' + FItems[I] + #13#10 + Format(CItemDescription, [FItems[I]]);
  if Result > '' then
    { Laatste enter weghalen }
    Delete(Result, Length(Result) - 1, 2);
end;

{ TClassItem }

procedure TClassItem.AddFunction(AItem: TClassMemberOrFieldItem);
begin
  AItem.FOwnerClass := Self;
  FList.Add(AItem);
end;

procedure TClassItem.AddProcedure(AItem: TClassMemberOrFieldItem);
begin
  AItem.FOwnerClass := Self;
  FList.Add(AItem);
end;

procedure TClassItem.AddProperty(AItem: TClassMemberOrFieldItem);
begin
  AItem.FOwnerClass := Self;
  FList.Add(AItem);
end;

constructor TClassItem.Create(const AName: string);
begin
  inherited Create(AName);
  FList := TList.Create;
end;

destructor TClassItem.Destroy;
begin
  FList.Free;
  inherited;
end;

function TClassItem.GetDelphiType: TDelphiType;
begin
  Result := dtClass;
end;

function TClassItem.GetItem(Index: Integer): TAbstractItem;
begin
  Result := FList[Index];
end;

function TClassItem.IncludedInFilter(const AData: TFilterData): Boolean;
begin
  Result := inherited IncludedInFilter(AData);
  if not Result then
    Exit;

  if (AData.RClass_DescendantOf > '') then
    Result := TSettings.Instance.IsDescendantOf(SimpleName, AData.RClass_DescendantOf);
end;

procedure TClassItem.SetItem(Index: Integer; const Value: TAbstractItem);
begin
  FList[Index] := Value;
end;

{ TTypeList }

function TTypeList.Add(AItem: TAbstractItem): Integer;
begin
  AItem.FTypeList := Self;
  Result := inherited Add(AItem);
end;

procedure TTypeList.CalculateCombines;
var
  I: Integer;

  procedure Examine(const S: string);
  var
    Indx: Integer;
  begin
    Indx := IndexOfName(S);
    if Indx < 0 then
      Exit;

    Items[I].AddCombine(Items[Indx]);
    Items[Indx].AddCombineWith(Items[I]);
  end;

  procedure ExamineEvent(const S: string);
  var
    Indx: Integer;
  begin
    if S = '' then
      Exit;

    Indx := 0;
    while Indx < Count do
    begin
      if (Items[Indx] is TClassPropertyItem) and SameText(TClassPropertyItem(Items[Indx]).TypeStr, S) then
      begin
        Items[I].AddCombine(Items[Indx]);
        Items[Indx].AddCombineWith(Items[I]);
      end;

      Inc(Indx);
    end;
  end;

var
  S: string;
begin
  for I := 0 to Count - 1 do
    if Items[I] is TTypeItem then
    begin
      S := Items[I].ValueString;
      if S = '' then
        Continue;

      if StrLIComp(PChar(S), 'set of', 6) = 0 then
      begin
        S := Trim(Copy(S, 8, MaxInt));
        while (Length(S) > 0) and (S[Length(S)] in [' ', ';']) do
          System.Delete(S, Length(S), 1);

        Examine(S);
        Continue;
      end;

      if S[1] = '^' then
      begin
        System.Delete(S, 1, 1);
        S := Trim(S);
        while (Length(S) > 0) and (S[Length(S)] in [' ', ';']) do
          System.Delete(S, Length(S), 1);

        Examine(S);
        Continue;
      end;
    end
    else
      if (Items[I] is TFunctionTypeItem) or (Items[I] is TProcedureTypeItem) then
    begin
      S := Items[I].SimpleName;
      ExamineEvent(S);
    end;
end;

procedure TTypeList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  inherited;
end;

destructor TTypeList.Destroy;
begin
  Clear;
  inherited;
end;

function TTypeList.GetItem(Index: Integer): TAbstractItem;
begin
  Result := inherited Items[Index];
end;

function TTypeList.IndexOfName(const SimpleName: string): Integer;
begin
  Result := 0;
  while (Result < Count) and not SameText(Items[Result].SimpleName, SimpleName) do
    Inc(Result);
  if Result >= Count then
    Result := -1;
end;

procedure TTypeList.SetItem(Index: Integer; const Value: TAbstractItem);
begin
  inherited Items[Index] := Value;
end;

function SortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TAbstractItem(Item1).SortName,
    TAbstractItem(Item2).SortName);
end;

procedure TTypeList.SortIt;
begin
  Sort(SortCompare);
end;

{ TParamClassMethodItem }

constructor TParamClassMethodItem.Create(const AName: string);
begin
  inherited Create(AName);
  FParams := TStringList.Create;
  FParamTypes := TStringList.Create;
end;

destructor TParamClassMethodItem.Destroy;
begin
  FParams.Free;
  FParamTypes.Free;
  inherited;
end;

function TParamClassMethodItem.GetReferenceName: string;
var
  I: Integer;
begin
  Result := inherited GetReferenceName;

  if not (diOverload in Directives) then
    Exit;

  for I := 0 to FParamTypes.Count - 1 do
    Result := Result + '@' + FParamTypes[I];
end;

{function TParamClassMethodItem.GetRealParamString: string;
begin
  Result := ParamListToString(FParams);
end;}

function TParamClassMethodItem.GetAddDescriptionString: string;
begin
  Result := '';

  if diOverride in Directives then
    Result := Result +
      '  This is an overridden method, you don''t have to describe these' +
      '  if it does the same as the inherited method'#13#10;

  if diOverload in Directives then
    Result := Result +
      '  This is an overloaded function/procedure, if possible you may combine the description'#13#10 +
      '  of all these functions into 1 general description. If you do so, combine all "Parameter" '#13#10 +
      '  lists into 1 list, and leave the "Summary", "Description" etc. fields empty for all'#13#10 +
      '  other overloaded functions with the same name.'#13#10;

  Result := Result + inherited GetAddDescriptionString;
end;

{function TParamClassMethodItem.GetParamList: TStrings;
begin
  if CanCombine then
    Result := nil
  else
    Result := FParams;
end;}

function TParamClassMethodItem.GetRealParamList: TStrings;
begin
  Result := FParams;
end;

{ TVarItem }

function TVarItem.GetDelphiType: TDelphiType;
begin
  Result := dtVar;
end;

function TVarItem.GetTitleName: string;
begin
  Result := Format(CTitleVariable, [SimpleName]);
end;

{ TTypeItem }

function TTypeItem.GetAddDescriptionString: string;
begin
  Result :=
    '  You don''t have to document already described items such as sets,'#13#10 +
    '  pointers to records etc.'#13#10;
end;

function TTypeItem.GetDelphiType: TDelphiType;
begin
  Result := dtType;
end;

function TTypeItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

{ TEnumItem }

function TEnumItem.GetDelphiType: TDelphiType;
begin
  Result := dtEnum;
end;

function TEnumItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

{ TResourceStringItem }

function TResourceStringItem.GetDelphiType: TDelphiType;
begin
  Result := dtResourceString;
end;

{ TRecordItem }

function TRecordItem.GetDelphiType: TDelphiType;
begin
  Result := dtRecord;
end;

function TRecordItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

{ TProcedureTypeItem }

function TProcedureTypeItem.GetAddDescriptionString: string;
var
  I: Integer;
begin
  Result := '  This type is used by (for reference):'#13#10;
  if not Assigned(FCombineList) or (CombineCount = 0) then
  begin
    Result := Result + '    Nothing in this unit.'#13#10;
    Exit;
  end;

  for I := 0 to FCombineList.Count - 1 do
    Result := Result + Format('    %s'#13#10, [TAbstractItem(FCombineList[I]).ReferenceName]);
end;

function TProcedureTypeItem.GetDelphiType: TDelphiType;
begin
  Result := dtProcedureType;
end;

function TProcedureTypeItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

{ TProcedureItem }

function TProcedureItem.GetDelphiType: TDelphiType;
begin
  Result := dtProcedure;
end;

function TProcedureItem.GetTitleName: string;
begin
  Result := Format(CTitleProcedure, [SimpleName]);
end;

function TProcedureItem.IncludedInFilter(
  const AData: TFilterData): Boolean;
begin
  Result := inherited IncludedInFilter(AData);
  if not Result then
    Exit;

  with AData do
    Result :=
      (RProcedure_MustExcludeDirectives * Directives = []) and
      (RProcedure_MustIncludeDirectives * Directives = RProcedure_MustIncludeDirectives) and

    ((RProcedure_MustIncludeOneOfDirectives = []) or
      (RProcedure_MustIncludeOneOfDirectives * Directives <> []));

  //RProcedure_MinimalParamCount: Integer; { -1 -> ignore }
  //RProcedure_MaximalParamCount: Integer; { -1 -> ignore }
end;

{ TClassPropertyItem }

destructor TClassPropertyItem.Destroy;
begin
  FParams.Free;
  FParamTypes.Free;
  inherited;
end;

function TClassPropertyItem.GetAddDescriptionString: string;
begin
  if Position = inPrivate then
    Result := inherited GetAddDescriptionString
  else
    Result := '';
end;

function TClassPropertyItem.GetDelphiType: TDelphiType;
begin
  Result := dtProperty;
end;

function TClassPropertyItem.GetParamList: TStrings;
begin
  if (CombineWithCount = 1) and (TAbstractItem(FCombineWithList[0]).CombineCount = 1) then
    Result := TAbstractItem(FCombineWithList[0]).RealParamList
  else
    Result := RealParamList;
end;

{function TClassPropertyItem.GetParamString: string;
begin
  if (CombineWithCount = 1) and (TAbstractItem(FCombineWithList[0]).CombineCount = 1) then
    Result := TAbstractItem(FCombineWithList[0]).RealParamString
  else
    Result := '';
end;}

function TClassPropertyItem.GetParams: TStringList;
begin
  if FParams = nil then
    FParams := TStringList.Create;
  Result := FParams;
end;

function TClassPropertyItem.GetParamTypes: TStringList;
begin
  if not Assigned(FParamTypes) then
    FParamTypes := TStringList.Create;
  Result := FParamTypes;
end;

function TClassPropertyItem.GetRealParamList: TStrings;
begin
  Result := FParams;
end;

function TClassPropertyItem.IncludedInFilter(
  const AData: TFilterData): Boolean;
begin
  Result := inherited IncludedInFilter(AData);
  if not Result then
    Exit;

  //Result :=
  //  [{psIndex, psDefault,} psNoDefault, psImplements] * Specifiers <> [];
  //  ((cptInherited in AData.RShow_PropertyTypes) and IsInherited) or
  //  ((cptNonInherited in AData.RShow_PropertyTypes) and not IsInherited);
  with AData do
    Result :=
      TriStateOk(RProperty_ShowInherited, IsInherited) and
      TriStateOk(RProperty_ShowArray, IsArray) and
      (Position in RProperty_Scope) and
      (RProperty_MustExcludeSpecifiers * Specifiers = []) and
      (RProperty_MustIncludeSpecifiers * Specifiers = RProperty_MustIncludeSpecifiers) and

    ((RProperty_MustIncludeOneOfSpecifiers = []) or
      (RProperty_MustIncludeOneOfSpecifiers * Specifiers <> [])) and

    (not Assigned(RProperty_In) or (RProperty_In.IndexOf(SimpleName) >= 0));
end;

{ TMethodProcItem }

function TMethodProcItem.GetDelphiType: TDelphiType;
begin
  Result := dtMethodProc;
end;

function TMethodProcItem.GetSortName: string;
begin
  case MethodType of
    mtNormal: Result := inherited GetSortName;
    mtConstructor: Result := OwnerClass.SortName + '.'#1 + SimpleName;
    mtDestructor: Result := OwnerClass.SortName + '.'#2 + SimpleName;
  else
    begin
      Assert(False, 'GetSortName');
      Result := '';
    end;
  end;
end;

function TMethodProcItem.IncludedInFilter(
  const AData: TFilterData): Boolean;
begin
  Result := inherited IncludedInFilter(AData);
  if not Result then
    Exit;

  with AData do
    Result :=
      TriStateOk(RMethodProcedure_ShowConstructor, MethodType = mtConstructor) and
      TriStateOk(RMethodProcedure_ShowDestructor, MethodType = mtDestructor) and
      TriStateOk(RMethodProcedure_ShowClassMethod, IsClassMethod) and
      (Position in RMethodProcedure_Scope) and
      (RMethodProcedure_MustExcludeDirectives * Directives = []) and
      (RMethodProcedure_MustIncludeDirectives * Directives = RMethodProcedure_MustIncludeDirectives) and

    ((RMethodProcedure_MustIncludeOneOfDirectives = []) or
      (RMethodProcedure_MustIncludeOneOfDirectives * Directives <> []));

  //RMethodProcedure_MinimalParamCount: Integer; { -1 -> ignore }
  //RMethodProcedure_MaximalParamCount: Integer; { -1 -> ignore }
end;

{ TMethodFuncItem }

function TMethodFuncItem.GetDelphiType: TDelphiType;
begin
  Result := dtMethodFunc;
end;

function TMethodFuncItem.IncludedInFilter(
  const AData: TFilterData): Boolean;
begin
  Result := inherited IncludedInFilter(AData);
  if not Result then
    Exit;

  with AData do
    Result :=
      TriStateOk(RMethodFunction_ShowClassMethod, IsClassMethod) and
      (Position in RMethodFunction_Scope) and
      (RMethodFunction_MustExcludeDirectives * Directives = []) and
      (RMethodFunction_MustIncludeDirectives * Directives = RMethodFunction_MustIncludeDirectives) and

    ((RMethodFunction_MustIncludeOneOfDirectives = []) or
      (RMethodFunction_MustIncludeOneOfDirectives * Directives <> []));

  //RMethodFunction_MinimalParamCount: Integer; { -1 -> ignore }
  //RMethodFunction_MaximalParamCount: Integer; { -1 -> ignore }
end;

{ TFunctionTypeItem }

function TFunctionTypeItem.GetAddDescriptionString: string;
var
  I: Integer;
begin
  if not Assigned(FCombineList) or (CombineCount = 0) then
  begin
    Result := '    Nothing in this unit.';
    Exit;
  end;

  Result := '';
  for I := 0 to FCombineList.Count - 1 do
    Result := Result + Format('    %s'#13#10, [TAbstractItem(FCombineList[I]).ReferenceName]);
end;

function TFunctionTypeItem.GetDelphiType: TDelphiType;
begin
  Result := dtFunctionType;
end;

function TFunctionTypeItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

{ TFunctionItem }

function TFunctionItem.GetDelphiType: TDelphiType;
begin
  Result := dtFunction;
end;

function TFunctionItem.GetTitleName: string;
begin
  Result := Format(CTitleFunction, [SimpleName]);
end;

function TFunctionItem.IncludedInFilter(const AData: TFilterData): Boolean;
begin
  Result := inherited IncludedInFilter(AData);
  if not Result then
    Exit;

  with AData do
    Result :=
      (RFunction_MustExcludeDirectives * Directives = []) and
      (RFunction_MustIncludeDirectives * Directives = RFunction_MustIncludeDirectives) and

    ((RFunction_MustIncludeOneOfDirectives = []) or
      (RFunction_MustIncludeOneOfDirectives * Directives <> []));

  //RFunction_MinimalParamCount: Integer; { -1 -> ignore }
  //RFunction_MaximalParamCount: Integer; { -1 -> ignore }
end;

{ TConstItem }

function TConstItem.GetDelphiType: TDelphiType;
begin
  Result := dtConst;
end;

{ TClassMemberOrFieldItem }

function TClassMemberOrFieldItem.GetAddDescriptionString: string;
const
  CPosition: array[TClassVisibility] of string = (
    'private', 'protected', 'public', 'published');
begin
  if Position in [inPrivate, inProtected] then
    Result := Format('  This is a %s member, you don''t have to describe these'#13#10,
      [CPosition[Position]])
  else
    Result := inherited GetAddDescriptionString;
end;

function TClassMemberOrFieldItem.GetClassString: string;
begin
  Result := OwnerClass.SimpleName;
end;

function TClassMemberOrFieldItem.GetReferenceName: string;
begin
  Result := OwnerClass.ReferenceName + '.' + inherited GetReferenceName;
end;

function TClassMemberOrFieldItem.IncludedInFilter(
  const AData: TFilterData): Boolean;
begin
  Result := inherited IncludedInFilter(AData);
  if not Result then
    Exit;

  if (AData.RClass_DescendantOf > '') then
    Result := TSettings.Instance.IsDescendantOf(
      OwnerClass.SimpleName, AData.RClass_DescendantOf);
end;

{ TValueItem }

function TValueItem.FormatValue(const S: string): string;

  procedure StrReplace(const OldPattern, NewPattern: string);
  var
    Tmp: string;
  begin
    Tmp := Result;
    Result := StringReplace(Tmp, OldPattern, NewPattern, [rfReplaceAll, rfIgnoreCase]);
  end;
begin
  Result := Trim(S);
  StrReplace('^ ', '^');
  StrReplace('^', '^');
  StrReplace('. ', '.');
  StrReplace(' .', '.');
  StrReplace('[ ', '[');
  StrReplace('] ', ']');
  StrReplace(' [', '[');
  StrReplace(' ]', ']');
  StrReplace('( ', '(');
  StrReplace(') ', ')');
  StrReplace(' (', '(');
  StrReplace(' )', ')');
  StrReplace('array[', 'array [');
  StrReplace(']of', '] of');
  StrReplace('of(', 'of (');
  //StrReplace('; ', ';');
  StrReplace('@ ', '@');
  StrReplace(' ;', ';');
  StrReplace(' :', ':');
  StrReplace(',', ', ');
  StrReplace(' ,', ',');
  StrReplace('=', ' = ');
  StrReplace('  ', ' ');
end;

function TValueItem.GetValueString: string;
begin
  Result := FValue;
end;

procedure TValueItem.SetValue(const AValue: string);
begin
  FValue := FormatValue(AValue);
end;

{ TInterfaceItem }

function TInterfaceItem.GetDelphiType: TDelphiType;
begin
  Result := dtInterface;
end;

{ TClassFieldItem }

function TClassFieldItem.GetDelphiType: TDelphiType;
begin
  Result := dtClassField;
end;

{ TMetaClassItem }

function TMetaClassItem.GetAddDescriptionString: string;
begin
  Result := Format(
    '  %s is the metaclass for %s. Its value is the class reference for'#13#10 +
    '  %s or for one of its descendants.', [SimpleName, Value, Value]);
end;

function TMetaClassItem.GetAddSummaryString: string;
begin
  Result :=
    Format('  Defines the metaclass for %s.', [Value]);
end;

function TMetaClassItem.GetDelphiType: TDelphiType;
begin
  Result := dtMetaClass;
end;

end.

