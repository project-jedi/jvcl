unit ItemFilter;

interface

uses
  Classes,
  ParserTypes;

type
  TItemFilter = class;

  TPropertyFilter = class(TPersistent)
  private
    FFilter: TItemFilter;
    FScope: TClassVisibilities;
    FMustIncludeSpecifiers: TPropertySpecifiers;
    FMustIncludeOneOfSpecifiers: TPropertySpecifiers;
    FMustExcludeSpecifiers: TPropertySpecifiers;
    FInList: TStrings;
    FShowInherited: TTriState;
    FShowArray: TTriState;
    procedure SetInList(const Value: TStrings);
  public
    constructor Create(AOwnerFilter: TItemFilter); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; virtual;

    function Includes(AItem: TAbstractItem): Boolean;

    property ShowInherited: TTriState read FShowInherited write FShowInherited;
    property ShowArray: TTriState read FShowArray write FShowArray;
    property MustExcludeSpecifiers: TPropertySpecifiers read FMustExcludeSpecifiers write FMustExcludeSpecifiers;
    property MustIncludeOneOfSpecifiers: TPropertySpecifiers read FMustIncludeOneOfSpecifiers write
      FMustIncludeOneOfSpecifiers;
    property MustIncludeSpecifiers: TPropertySpecifiers read FMustIncludeSpecifiers write FMustIncludeSpecifiers;
    property Scope: TClassVisibilities read FScope write FScope;
    property InList: TStrings read FInList write SetInList;
  end;

  TMethodFilter = class(TPersistent)
  private
    FFilter: TItemFilter;
    FMinimalParamCount: Integer;
    FMaximalParamCount: Integer;
    FScope: TClassVisibilities;
    FMustExcludeDirectives: TDirectives;
    FMustIncludeDirectives: TDirectives;
    FMustIncludeOneOfDirectives: TDirectives;
    FShowClassMethod: TTriState;
    FShowDestructor: TTriState;
    FShowConstructor: TTriState;
  public
    constructor Create(AOwnerFilter: TItemFilter); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; virtual;

    function Includes(AItem: TAbstractItem): Boolean;

    property ShowConstructor: TTriState read FShowConstructor write FShowConstructor;
    property ShowDestructor: TTriState read FShowDestructor write FShowDestructor;
    property ShowClassMethod: TTriState read FShowClassMethod write FShowClassMethod;
    property Scope: TClassVisibilities read FScope write FScope;
    property MustExcludeDirectives: TDirectives read FMustExcludeDirectives write FMustExcludeDirectives;
    property MustIncludeOneOfDirectives: TDirectives read FMustIncludeOneOfDirectives write
      FMustIncludeOneOfDirectives;
    property MustIncludeDirectives: TDirectives read FMustIncludeDirectives write FMustIncludeDirectives;
    property MinimalParamCount: Integer read FMinimalParamCount write FMinimalParamCount; { -1 -> ignore }
    property MaximalParamCount: Integer read FMaximalParamCount write FMaximalParamCount; { -1 -> ignore }
  end;

  TProcedureFunctionFilter = class(TPersistent)
  private
    FFilter: TItemFilter;
    FMustExcludeDirectives: TDirectives;
    FMustIncludeOneOfDirectives: TDirectives;
    FMustIncludeDirectives: TDirectives;
  public
    constructor Create(AOwnerFilter: TItemFilter); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; virtual;

    function Includes(AItem: TAbstractItem): Boolean;

    property MustIncludeDirectives: TDirectives read FMustIncludeDirectives write FMustIncludeDirectives;
    property MustIncludeOneOfDirectives: TDirectives read FMustIncludeOneOfDirectives write
      FMustIncludeOneOfDirectives;
    property MustExcludeDirectives: TDirectives read FMustExcludeDirectives write FMustExcludeDirectives;
  end;

  TClassFilter = class(TPersistent)
  private
    FFilter: TItemFilter;
    FDescendantOf: string;
  public
    constructor Create(AOwnerFilter: TItemFilter); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; virtual;

    function Includes(AItem: TAbstractItem): Boolean;

    property DescendantOf: string read FDescendantOf write FDescendantOf;
  end;

  TItemFilter = class(TPersistent)
  private
    FSearchInImplementationSection: Boolean;
    FSearchInInterfaceSection: Boolean;
    FClassFilter: TClassFilter;
    FDuplicates: TDuplicatesType;
    FMethodProcedureFilter: TMethodFilter;
    FMethodFunctionFilter: TMethodFilter;
    FProcedureFilter: TProcedureFunctionFilter;
    FFunctionFilter: TProcedureFunctionFilter;
    FPropertyFilter: TPropertyFilter;
    FDelphiTypes: TDelphiTypes;
    procedure SetClassFilter(const Value: TClassFilter);
    procedure SetFunctionFilter(const Value: TProcedureFunctionFilter);
    procedure SetMethodFunctionFilter(const Value: TMethodFilter);
    procedure SetMethodProcedureFilter(const Value: TMethodFilter);
    procedure SetProcedureFilter(const Value: TProcedureFunctionFilter);
    procedure SetPropertyFilter(const Value: TPropertyFilter);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; virtual;

    function Includes(AItem: TAbstractItem): Boolean;

    property DelphiTypes: TDelphiTypes read FDelphiTypes write FDelphiTypes;
    property Duplicates: TDuplicatesType read FDuplicates write FDuplicates;
    property SearchInInterfaceSection: Boolean read FSearchInInterfaceSection write FSearchInInterfaceSection;
    property SearchInImplementationSection: Boolean read FSearchInImplementationSection write
      FSearchInImplementationSection;

    property MethodFunctionFilter: TMethodFilter read FMethodFunctionFilter write SetMethodFunctionFilter;
    property MethodProcedureFilter: TMethodFilter read FMethodProcedureFilter write SetMethodProcedureFilter;
    property PropertyFilter: TPropertyFilter read FPropertyFilter write SetPropertyFilter;
    property ProcedureFilter: TProcedureFunctionFilter read FProcedureFilter write SetProcedureFilter;
    property FunctionFilter: TProcedureFunctionFilter read FFunctionFilter write SetFunctionFilter;
    property ClassFilter: TClassFilter read FClassFilter write SetClassFilter;
  end;

implementation

uses
  Settings;

//=== Local procedures =======================================================

function TriStateOk(const ATriState: TTriState; const ABool: Boolean): Boolean;
begin
  Result := (ATriState = tsDontCare) or (ABool = (ATriState = tsYes));
end;

//=== TClassFilter ===========================================================

procedure TClassFilter.Assign(Source: TPersistent);
var
  Src: TClassFilter;
begin
  if Source is TClassFilter then
  begin
    Src := Source as TClassFilter;

    FDescendantOf := Src.FDescendantOf;
  end
  else
    inherited Assign(Source);
end;

constructor TClassFilter.Create(AOwnerFilter: TItemFilter);
begin
  inherited Create;

  FFilter := AOwnerFilter;

  Reset;
end;

function TClassFilter.Includes(AItem: TAbstractItem): Boolean;
begin
  Result := AItem is TClassItem;
  if not Result then
    Exit;

  with AItem as TClassItem do
  begin
    if Self.DescendantOf > '' then
      Result := TSettings.Instance.IsDescendantOf(SimpleName, Self.DescendantOf)
    else
      Result := True;
  end;
end;

procedure TClassFilter.Reset;
begin
  FDescendantOf := '';
end;

//=== TItemFilter ============================================================

procedure TItemFilter.Assign(Source: TPersistent);
var
  Src: TItemFilter;
begin
  if Source is TItemFilter then
  begin
    Src := Source as TItemFilter;

    FSearchInImplementationSection := Src.FSearchInImplementationSection;
    FSearchInInterfaceSection := Src.FSearchInInterfaceSection;
    FDuplicates := Src.FDuplicates;
    FDelphiTypes := Src.FDelphiTypes;

    FClassFilter.Assign(Src.FClassFilter);
    FMethodProcedureFilter.Assign(Src.FMethodProcedureFilter);
    FMethodFunctionFilter.Assign(FMethodFunctionFilter);
    FProcedureFilter.Assign(FProcedureFilter);
    FFunctionFilter.Assign(FFunctionFilter);
    FPropertyFilter.Assign(FPropertyFilter);
  end
  else
    inherited Assign(Source);
end;

constructor TItemFilter.Create;
begin
  inherited Create;

  FMethodProcedureFilter := TMethodFilter.Create(Self);
  FMethodFunctionFilter := TMethodFilter.Create(Self);
  FProcedureFilter := TProcedureFunctionFilter.Create(Self);
  FFunctionFilter := TProcedureFunctionFilter.Create(Self);
  FPropertyFilter := TPropertyFilter.Create(Self);
  FClassFilter := TClassFilter.Create(Self);

  Reset;
end;

destructor TItemFilter.Destroy;
begin
  FMethodProcedureFilter.Free;
  FMethodFunctionFilter.Free;
  FProcedureFilter.Free;
  FFunctionFilter.Free;
  FPropertyFilter.Free;
  FClassFilter.Free;

  inherited Destroy;
end;

function TItemFilter.Includes(AItem: TAbstractItem): Boolean;
begin
  Result := AItem.DelphiType in DelphiTypes;
  if not Result then
    Exit;

  case AItem.DelphiType of
    dtClass: Result := ClassFilter.Includes(AItem);
    dtFunction: Result := FunctionFilter.Includes(AItem);
    dtMethodFunc: Result := MethodFunctionFilter.Includes(AItem);
    dtMethodProc: Result := MethodProcedureFilter.Includes(AItem);
    dtProcedure: Result := ProcedureFilter.Includes(AItem);
    dtProperty: Result := PropertyFilter.Includes(AItem);
  else
    Result := True;
  end;
end;

procedure TItemFilter.Reset;
begin
  FSearchInImplementationSection := False;
  FSearchInInterfaceSection := True;
  FDelphiTypes := [];
  FDuplicates := dtAll;

  FClassFilter.Reset;
  FMethodProcedureFilter.Reset;
  FMethodFunctionFilter.Reset;
  FProcedureFilter.Reset;
  FFunctionFilter.Reset;
  FPropertyFilter.Reset;
end;

procedure TItemFilter.SetClassFilter(const Value: TClassFilter);
begin
  FClassFilter.Assign(Value);
end;

procedure TItemFilter.SetFunctionFilter(
  const Value: TProcedureFunctionFilter);
begin
  FFunctionFilter.Assign(Value);
end;

procedure TItemFilter.SetMethodFunctionFilter(const Value: TMethodFilter);
begin
  FMethodFunctionFilter.Assign(Value);
end;

procedure TItemFilter.SetMethodProcedureFilter(const Value: TMethodFilter);
begin
  FMethodProcedureFilter.Assign(Value);
end;

procedure TItemFilter.SetProcedureFilter(
  const Value: TProcedureFunctionFilter);
begin
  FProcedureFilter.Assign(Value);
end;

procedure TItemFilter.SetPropertyFilter(const Value: TPropertyFilter);
begin
  FPropertyFilter.Assign(Value);
end;

//=== TMethodFilter ==========================================================

procedure TMethodFilter.Assign(Source: TPersistent);
var
  Src: TMethodFilter;
begin
  if Source is TMethodFilter then
  begin
    Src := Source as TMethodFilter;

    FMinimalParamCount := Src.FMinimalParamCount;
    FMaximalParamCount := Src.FMaximalParamCount;
    FScope := Src.FScope;
    FMustExcludeDirectives := Src.FMustExcludeDirectives;
    FMustIncludeDirectives := Src.FMustIncludeDirectives;
    FMustIncludeOneOfDirectives := Src.FMustIncludeOneOfDirectives;
    FShowClassMethod := Src.FShowClassMethod;
    FShowDestructor := Src.FShowDestructor;
    FShowConstructor := Src.FShowConstructor;
  end
  else
    inherited Assign(Source);
end;

constructor TMethodFilter.Create(AOwnerFilter: TItemFilter);
begin
  inherited Create;

  FFilter := AOwnerFilter;

  Reset;
end;

function TMethodFilter.Includes(AItem: TAbstractItem): Boolean;
begin
  Result := AItem is TParamClassMethodItem;
  if not Result then
    Exit;

  with AItem as TParamClassMethodItem do
  begin
    if FFilter.ClassFilter.DescendantOf > '' then
    begin
      Result := TSettings.Instance.IsDescendantOf(ClassString,
        FFilter.ClassFilter.DescendantOf);
      if not Result then
        Exit;

      Result :=
        TriStateOk(Self.ShowClassMethod, IsClassMethod) and
        (Position in Self.Scope) and
        (Self.MustExcludeDirectives * Directives = []) and
        (Self.MustIncludeDirectives * Directives = Self.MustIncludeDirectives) and
        ((Self.MustIncludeOneOfDirectives = []) or
        (Self.MustIncludeOneOfDirectives * Directives <> []));
      if not Result then
        Exit;
    end;
  end;

  if AItem is TMethodProcItem then
    with AItem as TMethodProcItem do
    begin
      Result :=
        TriStateOk(Self.ShowConstructor, MethodType = mtConstructor) and
        TriStateOk(Self.ShowDestructor, MethodType = mtDestructor);
      if not Result then
        Exit;
    end;
end;

procedure TMethodFilter.Reset;
begin
  FMinimalParamCount := -1;
  FMaximalParamCount := -1;
  FScope := [Low(TClassVisibility)..High(TClassVisibility)];
  FMustExcludeDirectives := [];
  FMustIncludeDirectives := [];
  FMustIncludeOneOfDirectives := [];
  FShowClassMethod := tsDontCare;
  FShowDestructor := tsDontCare;
  FShowConstructor := tsDontCare;
end;

//=== TProcedureFunctionFilter ===============================================

procedure TProcedureFunctionFilter.Assign(Source: TPersistent);
var
  Src: TProcedureFunctionFilter;
begin
  if Source is TProcedureFunctionFilter then
  begin
    Src := Source as TProcedureFunctionFilter;

    FMustExcludeDirectives := Src.FMustExcludeDirectives;
    FMustIncludeOneOfDirectives := Src.FMustIncludeOneOfDirectives;
    FMustIncludeDirectives := Src.FMustIncludeDirectives;
  end
  else
    inherited Assign(Source);
end;

constructor TProcedureFunctionFilter.Create(AOwnerFilter: TItemFilter);
begin
  inherited Create;

  FFilter := AOwnerFilter;

  Reset;
end;

function TProcedureFunctionFilter.Includes(AItem: TAbstractItem): Boolean;
begin
  Result := AItem is TBaseFuncItem;
  if not Result then
    Exit;

  with AItem as TBaseFuncItem do
  begin
    Result :=
      (Self.MustExcludeDirectives * Directives = []) and
      (Self.MustIncludeDirectives * Directives = Self.MustIncludeDirectives) and
      ((Self.MustIncludeOneOfDirectives = []) or
      (Self.MustIncludeOneOfDirectives * Directives <> []));
  end
end;

procedure TProcedureFunctionFilter.Reset;
begin
  FMustExcludeDirectives := [];
  FMustIncludeOneOfDirectives := [];
  FMustIncludeDirectives := [];
end;

//=== TPropertyFilter ========================================================

procedure TPropertyFilter.Assign(Source: TPersistent);
var
  Src: TPropertyFilter;
begin
  if Source is TPropertyFilter then
  begin
    Src := Source as TPropertyFilter;

    FScope := Src.FScope;
    FMustIncludeSpecifiers := Src.FMustIncludeSpecifiers;
    FMustIncludeOneOfSpecifiers := Src.FMustIncludeOneOfSpecifiers;
    FMustExcludeSpecifiers := Src.FMustExcludeSpecifiers;
    FInList := Src.FInList;
    FShowInherited := Src.FShowInherited;
    FShowArray := Src.FShowArray;
  end
  else

    inherited Assign(Source);
end;

constructor TPropertyFilter.Create(AOwnerFilter: TItemFilter);
begin
  inherited Create;

  FFilter := AOwnerFilter;

  FInList := TStringList.Create;
  TStringList(FInList).Sorted := True;
  TStringList(FInList).Duplicates := dupIgnore;

  Reset;
end;

destructor TPropertyFilter.Destroy;
begin
  FInList.Free;
  inherited Destroy;
end;

function TPropertyFilter.Includes(AItem: TAbstractItem): Boolean;
begin
  Result := AItem is TClassPropertyItem;
  if not Result then
    Exit;

  with AItem as TClassPropertyItem do
  begin
    if FFilter.ClassFilter.DescendantOf > '' then
    begin
      Result := TSettings.Instance.IsDescendantOf(ClassString,
        FFilter.ClassFilter.DescendantOf);
      if not Result then
        Exit;
    end;

    Result :=
      TriStateOk(Self.ShowInherited, IsInherited) and
      TriStateOk(Self.ShowArray, IsArray) and
      (Position in Self.Scope) and
      (Self.MustExcludeSpecifiers * Specifiers = []) and
      (Self.MustIncludeSpecifiers * Specifiers = Self.MustIncludeSpecifiers) and
      ((Self.MustIncludeOneOfSpecifiers = []) or
      (Self.MustIncludeOneOfSpecifiers * Specifiers <> [])) and
      ((Self.InList.Count = 0) or (Self.InList.IndexOf(SimpleName) >= 0));
  end;
end;

procedure TPropertyFilter.Reset;
begin
  FScope := [Low(TClassVisibility)..High(TClassVisibility)];
  FMustIncludeSpecifiers := [];
  FMustIncludeOneOfSpecifiers := [];
  FMustExcludeSpecifiers := [];
  FInList.Clear;
  FShowInherited := tsDontCare;
  FShowArray := tsDontCare;
end;

procedure TPropertyFilter.SetInList(const Value: TStrings);
begin
  FInList.Assign(Value);
end;

end.
