{$I JVCL.INC}
unit JvValidators;

interface
uses
  Windows, SysUtils, Classes, Controls, TypInfo, JvTypes;

type
  EValidatorError = class(EJVCLException);
  // Implemented by classes that can return the value to validate against.
  // The validator classes first check if the ObjectToValidate supports this interface
  // and if it does, uses the value returned from GetValidationPropertyValue instead of
  // extracting it from RTTI (using ObjectToValidate and PropertyToValidate)
  // The good thing about implementing this interface is that the value to validate do
  // not need to be a published property but can be anything
  IJvValidationProperty = interface
    ['{564FD9F5-BE57-4559-A6AF-B0624C956E50}']
    function GetValidationPropertyValue: Variant;
  end;

  IJvValidationSummary = interface
  ['{F2E4F4E5-E831-4514-93C9-0E2ACA941DCF}']
    procedure AddError(const ErrorMessage:string);
    procedure RemoveError(const ErrorMessage:string);
  end;

  TJvBaseValidator = class;
  TJvBaseValidatorClass = class of TJvBaseValidator;

  TJvBaseValidator = class(TPersistent)
  private
    FEnabled, FIsValid: boolean;
    FPropertyToValidate: string;
    FErrorMessage: string;
    FObjectToValidate: TControl;
    FValidationSummary: IJvValidationSummary;
    FName: string;
  protected
    function GetValidationPropertyValue: Variant; virtual;
    procedure SetIsValid(const Value: boolean); virtual;
    function GetIsValid: Boolean; virtual;
    procedure Validate; virtual; abstract;
  public
    constructor Create;
    procedure Assign(Source:TPersistent);override;
  published
    property Name:string read FName write FName;
    property ValidationSummary:IJvValidationSummary read FValidationSummary write FValidationSummary;
    property IsValid: boolean read GetIsValid write SetIsValid;
    property ObjectToValidate: TControl read FObjectToValidate write FObjectToValidate;
    property PropertyToValidate: string read FPropertyToValidate write FPropertyToValidate;
    property Enabled: boolean read FEnabled write FEnabled;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
  end;

  TJvRequiredFieldValidator = class(TJvBaseValidator)
  protected
    procedure Validate; override;
  end;

  TJvValidateCompareOperator = (vcoLessThan, vcoLessOrEqual, vcoEqual, vcoGreaterOrEqual, vcoGreaterThan);

  TJvCompareValidator = class(TJvBaseValidator)
  private
    FValueToCompare: Variant;
    FOperator: TJvValidateCompareOperator;
  protected
    procedure Validate; override;
  published
    property ValueToCompare: Variant read FValueToCompare write FValueToCompare;
    property Operator: TJvValidateCompareOperator read FOperator write FOperator;
  end;

  TJvRangeValidator = class(TJvBaseValidator)
  private
    FMinimumValue: Variant;
    FMaximumValue: Variant;
  protected
    procedure Validate; override;
  published
    property MinimumValue: Variant read FMinimumValue write FMinimumValue;
    property MaximumValue: Variant read FMaximumValue write FMaximumValue;
  end;

  TJvRegularExpressionValidator = class(TJvBaseValidator)
  private
    FValidationExpression: string;
  protected
    procedure Validate; override;
  published
    property ValidationExpression: string read FValidationExpression write FValidationExpression;
  end;

  TJvCustomValidateEvent = procedure(Sender: TObject; ValueToValidate: Variant; var IsValid: boolean) of object;
  TJvCustomValidator = class(TJvBaseValidator)
  private
    FOnValidate: TJvCustomValidateEvent;
  protected
    function DoValidate: boolean; virtual;
    procedure Validate; override;
  published
    property OnValidate: TJvCustomValidateEvent read FOnValidate write FOnValidate;
  end;

  TJvValidateFailEvent = procedure (Sender:TObject; Validator:TJvBaseValidator;var Continue:boolean) of object;

  TJvValidatorList = class(TPersistent)
  private
    FOnValidateFailed: TJvValidateFailEvent;
    FValidators:TList;
    function GetCount: integer;
    function GetValidators(Index: integer): TJvBaseValidator;
  protected
    function DoValidateFailed(const AValidator:TJvBaseValidator):boolean;
  public
    constructor Create;
    destructor Destroy;override;
    procedure Assign(Source:TPersistent);override;
    function Add(AValidatorClass:TJvBaseValidatorClass):TJvBaseValidator;overload;
    function Add(AValidator:TJvBaseValidator):integer;overload;
    function Remove(Index:integer):TJvBaseValidator;
    procedure Delete(Index:integer);
    procedure Clear;
    function Validate:boolean;
    property Validators[Index:integer]:TJvBaseValidator read GetValidators;default;
    property Count:integer read getCount;
    property OnValidateFailed:TJvValidateFailEvent read FOnValidateFailed write FOnValidateFailed;
  end;

  TJvValidationSummary = class(TInterfacedObject,IJvValidationSummary)
  private
    FSummaries:TStrings;
    FOnChange: TNotifyEvent;
    function GetSummaries: TStrings;
  protected
    { IJvValidationSummary }
    procedure AddError(const ErrorMessage:string);
    procedure RemoveError(const ErrorMessage:string);

    procedure Change;virtual;
  public
    destructor Destroy;override;
    property Summaries:TStrings read GetSummaries;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvValidator = class(TComponent)
  private
    FOnValidateFailed: TJvValidateFailEvent;
    FValidators: TJvValidatorList;
    procedure SetValidators(const Value: TJvValidatorList);
  protected
    function DoValidateFailed(const AValidator:TJvBaseValidator):boolean;
    procedure DoInternalValidateFailed(Sender:TObject; AValidator:TJvBaseValidator; var Continue:boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validate:boolean;
    property Validators:TJvValidatorList read FValidators write SetValidators;
  published
    property OnValidateFailed:TJvValidateFailEvent read FOnValidateFailed write FOnValidateFailed;
  end;

procedure Register;

implementation
uses
{$IFDEF COMPILER6_UP}
  Variants,
{$ENDIF}
  JclUnicode;
procedure Register;
begin
//  RegisterComponents('JVCL',[TJvValidator]);
end;

{$IFNDEF COMPILER6_UP}
type
  TVariantRelationship = (vrEqual, vrLessThan, vrGreaterThan, vrNotEqual);

function FindVarData(const V: Variant): PVarData;
begin
  Result := @TVarData(V);
  while Result.VType = varByRef or varVariant do
    Result := PVarData(Result.VPointer);
end;

function VarCompareValue(const A, B: Variant): TVariantRelationship;
const
  CTruth: array[Boolean] of TVariantRelationship = (vrNotEqual, vrEqual);
var
  LA, LB: TVarData;
begin
  LA := FindVarData(A)^;
  LB := FindVarData(B)^;
  if LA.VType = varEmpty then
    Result := CTruth[LB.VType = varEmpty]
  else if LA.VType = varNull then
    Result := CTruth[LB.VType = varNull]
  else if LB.VType in [varEmpty, varNull] then
    Result := vrNotEqual
  else if A = B then
    Result := vrEqual
  else if A < B then
    Result := vrLessThan
  else
    Result := vrGreaterThan;
end;
{$ENDIF}

{ TJvBaseValidator }

procedure TJvBaseValidator.Assign(Source: TPersistent);
begin
  if Source is TJvBaseValidator then
  begin
    Name               := TJvBaseValidator(Source).Name;
    ObjectToValidate   := TJvBaseValidator(Source).ObjectToValidate;
    PropertyToValidate := TJvBaseValidator(Source).PropertyToValidate;
    ValidationSummary  := TJvBaseValidator(Source).ValidationSummary;
    Enabled            := TJvBaseValidator(Source).Enabled;
    IsValid            := TJvBaseValidator(Source).IsValid;
    ErrorMessage       := TJvBaseValidator(Source).ErrorMessage;
    Exit;
  end;
  inherited;
end;

constructor TJvBaseValidator.Create;
begin
  inherited Create;
  FIsValid := true;
  FEnabled := true;
end;

function TJvBaseValidator.GetIsValid: Boolean;
begin
  Result := FIsValid;
end;

function TJvBaseValidator.GetValidationPropertyValue: Variant;
var
  ValProp: IJvValidationProperty;
begin
  Result := NULL;
  if (FObjectToValidate <> nil) then
  begin
    if Supports(FObjectToValidate, IJvValidationProperty, ValProp) then
      Result := ValProp.GetValidationPropertyValue
    else if (FPropertyToValidate <> '') then
      Result := GetPropValue(FObjectToValidate, FPropertyToValidate, false);
  end;
end;
function ReadStreamStr(Stream: TStream):string;
var ACount:integer;
begin
  Stream.Read(ACount,sizeof(ACount));
  SetLength(Result,ACount);
  if ACount > 0 then Stream.Read(Result[1],ACount);
end;

procedure WriteStreamStr(Stream: TStream;const Value:string);
var ACount:integer;
begin
  ACount := Length(Value);
  Stream.Write(ACount,sizeof(ACount));
  if ACount > 0 then
    Stream.Write(Value[1],ACount);
end;

procedure TJvBaseValidator.SetIsValid(const Value: boolean);
begin
  FIsValid := Value;
  if (ValidationSummary <> nil) then
  begin
    if ErrorMessage <> '' then
    begin
      ValidationSummary.RemoveError(ErrorMessage);
      if not FIsValid then
        ValidationSummary.AddError(ErrorMessage);
    end;
  end;
end;


{ TJvRequiredFieldValidator }

procedure TJvRequiredFieldValidator.Validate;
var
  R: Variant;
begin
  R := GetValidationPropertyValue;
  IsValid := not VarIsNull(R) and not VarIsEmpty(R) and (R <> '');
end;

{ TJvCustomValidator }

function TJvCustomValidator.DoValidate: boolean;
begin
  if Assigned(FOnValidate) then
    FOnValidate(self, GetValidationPropertyValue, Result)
  else
    Result := IsValid;
end;

procedure TJvCustomValidator.Validate;
begin
  IsValid := DoValidate;
end;

{ TJvRegularExpressionValidator }

function MatchesMask(const Filename, Mask: string; const SearchFlags: TSearchFlags = [sfCaseSensitive]): boolean;
var
  URE: TURESearch;
  SL: TWideStringList;
begin
  // use the regexp engine in JclUnicode
  SL := TWideStringList.Create;
  try
    URE := TURESearch.Create(SL);
    try
      URE.FindPrepare(Mask, SearchFlags);
      // this could be overkill for long strings and many matches,
      // but it's a lot simpler than calling FindFirst...
      Result := URE.FindAll(Filename);
    finally
      URE.Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure TJvRegularExpressionValidator.Validate;
var
  R: string;
begin
  R := VarToStr(GetValidationPropertyValue);
  IsValid := (R = ValidationExpression) or MatchesMask(R, ValidationExpression);
end;

{ TJvCompareValidator }

procedure TJvCompareValidator.Validate;
var
  VR: TVariantRelationship;
begin
  VR := VarCompareValue(GetValidationPropertyValue, ValueToCompare);
  case Operator of
    vcoLessThan:
      IsValid := VR = vrLessThan;
    vcoLessOrEqual:
      IsValid := (VR = vrLessThan) or (VR = vrEqual);
    vcoEqual:
      IsValid := (VR = vrEqual);
    vcoGreaterOrEqual:
      IsValid := (VR = vrGreaterThan) or (VR = vrEqual);
    vcoGreaterThan:
      IsValid := (VR = vrGreaterThan);
  end;
end;

{ TJvRangeValidator }

procedure TJvRangeValidator.Validate;
var
  VR: TVariantRelationship;
begin
  VR := VarCompareValue(GetValidationPropertyValue, MinimumValue);
  IsValid := (VR = vrGreaterThan) or (VR = vrEqual);
  if IsValid then
  begin
    VR := VarCompareValue(GetValidationPropertyValue, MaximumValue);
    IsValid := (VR = vrLessThan) or (VR = vrEqual);
  end;
end;

{ TJvValidationSummary }

procedure TJvValidationSummary.AddError(const ErrorMessage: string);
begin
  if Summaries.IndexOf(ErrorMessage) < 0 then
  begin
    Summaries.Add(ErrorMessage);
    Change;
  end;
end;

procedure TJvValidationSummary.RemoveError(const ErrorMessage: string);
var i:integer;
begin
  i := Summaries.IndexOf(ErrorMessage);
  if i > -1 then
  begin
    Summaries.Delete(i);
    Change;
  end;
end;

destructor TJvValidationSummary.Destroy;
begin
  FreeAndNil(FSummaries);
  inherited Destroy;
end;

function TJvValidationSummary.GetSummaries: TStrings;
begin
  if FSummaries = nil then
    FSummaries := TStringlist.Create;
  Result := FSummaries;
end;

procedure TJvValidationSummary.Change;
begin
  if Assigned(FOnChange) then FOnChange(self);
end;

{ TJvValidatorList }

function TJvValidatorList.Add(AValidatorClass: TJvBaseValidatorClass): TJvBaseValidator;
var i:integer;
begin
  if AValidatorClass = nil then
    raise EValidatorError.Create('AValidatorClass cannot be nil');
  i := Add(AValidatorClass.Create);
  Result := Validators[i];
end;

function TJvValidatorList.Add(AValidator: TJvBaseValidator): integer;
begin
  if AValidator = nil then
    raise EValidatorError.Create('AValidator cannot be nil');
  Result := FValidators.Add(AValidator);
end;

procedure TJvValidatorList.Assign(Source: TPersistent);
var i,j:integer;
begin
  if Source is TJvValidatorList then
  begin
    Clear;
    for i := 0 to TJvValidatorList(Source).Count - 1 do
      Add(TJvBaseValidatorClass(TJvValidatorList(Source)[i].ClassType)).Assign(TJvValidatorList(Source)[i]);
    Exit;
  end;
  inherited;
end;

procedure TJvValidatorList.Clear;
var i:integer;
begin
  for i := 0 to FValidators.Count - 1 do
    Validators[i].Free;
  FValidators.Clear;
end;

constructor TJvValidatorList.Create;
begin
  inherited Create;
  FValidators := TList.Create;
end;

procedure TJvValidatorList.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EValidatorError.CreateFmt('Invalid index (%d)',[Index]);
  TJvBaseValidator(FValidators[Index]).Free;
  FValidators.Delete(Index);
end;

destructor TJvValidatorList.Destroy;
begin
  Clear;
  FreeAndNil(FValidators);
  inherited;
end;

function TJvValidatorList.DoValidateFailed(const AValidator: TJvBaseValidator): boolean;
begin
  Result := true;
  if Assigned(FOnValidateFailed) then
    FOnValidateFailed(self,AValidator,Result);
end;

function TJvValidatorList.GetCount: integer;
begin
  Result := FValidators.Count;
end;

function TJvValidatorList.GetValidators(Index: integer): TJvBaseValidator;
begin
  if (Index < 0) or (Index >= Count) then
    raise EValidatorError.CreateFmt('Invalid index (%d)',[Index]);
  Result := TJvBaseValidator(FValidators[Index]);
end;

function TJvValidatorList.Remove(Index: integer): TJvBaseValidator;
begin
  if (Index < 0) or (Index >= Count) then
    raise EValidatorError.CreateFmt('Invalid index (%d)',[Index]);
  Result := TJvBaseValidator(FValidators[Index]);
  FValidators.Delete(Index);
end;

function TJvValidatorList.Validate: boolean;
var i:integer;
begin
  Result := true;
  for i := 0 to Count - 1 do
  begin
    Validators[i].Validate;
    if not Validators[i].IsValid then
    begin
      Result := false;
      if not DoValidateFailed(Validators[i]) then Exit;
    end;
  end;
end;

{ TJvValidator }

constructor TJvValidator.Create(AOwner: TComponent);
begin
  inherited;
  FValidators := TJvValidatorList.Create;
  FValidators.OnValidateFailed := DoInternalValidateFailed;
end;

destructor TJvValidator.Destroy;
begin
  FValidators.Free;
  inherited;
end;

procedure TJvValidator.DoInternalValidateFailed(Sender: TObject; AValidator: TJvBaseValidator; var Continue:boolean);
begin
  Continue := DoValidateFailed(AValidator);
end;

function TJvValidator.DoValidateFailed(
  const AValidator: TJvBaseValidator): boolean;
begin
  Result := true;
  if Assigned(FonValidateFailed) then
    FonValidateFailed(self,AValidator,Result);
end;

procedure TJvValidator.SetValidators(const Value: TJvValidatorList);
begin
  FValidators.Assign(Value);
end;

function TJvValidator.Validate: boolean;
begin
  Result := FValidators.Validate;
end;

end.

