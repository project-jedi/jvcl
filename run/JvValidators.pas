{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvValidators.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvValidators;

interface

uses
  SysUtils, Classes,
  Windows, Controls, Forms,
  JvComponent, JvErrorIndicator, JvFinalize;

type
  EValidatorError = class(Exception);
  // Implemented by classes that can return the value to validate against.
  // The validator classes first check if the ControlToValidate supports this interface
  // and if it does, uses the value returned from GetValidationPropertyValue instead of
  // extracting it from RTTI (using ControlToValidate and PropertyToValidate)
  // The good thing about implementing this interface is that the value to validate do
  // not need to be a published property but can be anything, even a calculated value
  IJvValidationProperty = interface
    ['{564FD9F5-BE57-4559-A6AF-B0624C956E50}']
    function GetValidationPropertyValue: Variant;
    function GetValidationPropertyName: WideString;
  end;

  IJvValidationSummary = interface
    ['{F2E4F4E5-E831-4514-93C9-0E2ACA941DCF}']
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AddError(const ErrorMessage: string);
    procedure RemoveError(const ErrorMessage: string);
  end;

  TJvBaseValidator = class;
  TJvValidators = class;
  TJvBaseValidatorClass = class of TJvBaseValidator;

  TJvBaseValidator = class(TJvComponent)
  private
    FEnabled: Boolean;
    FValid: Boolean;
    FPropertyToValidate: string;
    FErrorMessage: string;
    FControlToValidate: TControl;
    FValidator: TJvValidators;
    FOnValidateFailed: TNotifyEvent;
    procedure SetControlToValidate(Value: TControl);
  protected
    function GetValidationPropertyValue: Variant; virtual;
    procedure SetValid(const Value: Boolean); virtual;
    function GetValid: Boolean; virtual;
    procedure DoValidateFailed; dynamic;
    procedure Validate; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParentComponent(Value: TComponent); override;
    procedure ReadState(Reader: TReader); override;

    // get the number of registered base validator classes
    class function BaseValidatorsCount: Integer;
    // get info on a registered class
    class procedure GetBaseValidatorInfo(Index: Integer; var DisplayName: string;
      var ABaseValidatorClass: TJvBaseValidatorClass);
  public
    // register a new base validator class. DisplayName is used by the design-time editor.
    // A class with an empty DisplayName will not sshow up in the editor
    class procedure RegisterBaseValidator(const DisplayName: string; AValidatorClass: TJvBaseValidatorClass);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    property Value: Variant read GetValidationPropertyValue;
  published
    property Valid: Boolean read GetValid write SetValid;
    // the control to validate
    property ControlToValidate: TControl read FControlToValidate write SetControlToValidate;
    // the property in ControlToValidate to validate against
    property PropertyToValidate: string read FPropertyToValidate write FPropertyToValidate;
    property Enabled: Boolean read FEnabled write FEnabled;
    // the message to display in case of error
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    // triggered when Valid is set to False
    property OnValidateFailed: TNotifyEvent read FOnValidateFailed write FOnValidateFailed;
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

  TJvCustomValidateEvent = procedure(Sender: TObject; ValueToValidate: Variant; var Valid: Boolean) of object;

  TJvCustomValidator = class(TJvBaseValidator)
  private
    FOnValidate: TJvCustomValidateEvent;
  protected
    function DoValidate: Boolean; virtual;
    procedure Validate; override;
  published
    property OnValidate: TJvCustomValidateEvent read FOnValidate write FOnValidate;
  end;
  // compares the properties of two controls
  // if CompareToControl implements the IJvValidationProperty interface, the value
  // to compare is taken from GetValidationPropertyValue, otherwise RTTI is used to get the
  // property value
  TJvControlsCompareValidator = class(TJvBaseValidator)
  private
    FCompareToControl: TControl;
    FCompareToProperty: string;
    FOperator: TJvValidateCompareOperator;
    FAllowNull: Boolean;
  protected
    procedure Validate; override;
    function GetPropertyValueToCompare: Variant;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property CompareToControl: TControl read FCompareToControl write FCompareToControl;
    property CompareToProperty: string read FCompareToProperty write FCompareToProperty;
    property Operator: TJvValidateCompareOperator read FOperator write FOperator;
    property AllowNull: Boolean read FAllowNull write FAllowNull default True;
  end;

  TJvValidateFailEvent = procedure(Sender: TObject; BaseValidator: TJvBaseValidator; var Continue: Boolean) of object;

  TJvValidators = class(TJvComponent)
  private
    FOnValidateFailed: TJvValidateFailEvent;
    FItems: TList;
    FValidationSummary: IJvValidationSummary;
    FErrorIndicator: IJvErrorIndicator;
    {$IFNDEF COMPILER6_UP}
    FValidationSummaryComponent: TComponent;
    FErrorIndicatorComponent: TComponent;
    procedure SetValidationSummaryComponent(Value: TComponent);
    procedure SetErrorIndicatorComponent(Value: TComponent);
    {$ENDIF COMPILER6_UP}
    procedure SetValidationSummary(const Value: IJvValidationSummary);
    procedure SetErrorIndicator(const Value: IJvErrorIndicator);
    function GetCount: Integer;
    function GetItem(Index: Integer): TJvBaseValidator;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function DoValidateFailed(const ABaseValidator: TJvBaseValidator): Boolean; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Insert(AValidator: TJvBaseValidator);
    procedure Remove(AValidator: TJvBaseValidator);
    procedure Exchange(Index1, Index2: Integer);
    function Validate: Boolean;
    property Items[Index: Integer]: TJvBaseValidator read GetItem; default;
    property Count: Integer read GetCount;
  published
    {$IFDEF COMPILER6_UP}
    property ValidationSummary: IJvValidationSummary read FValidationSummary write SetValidationSummary;
    property ErrorIndicator: IJvErrorIndicator read FErrorIndicator write SetErrorIndicator;
    {$ELSE}
    property ValidationSummary: TComponent read FValidationSummaryComponent write SetValidationSummaryComponent;
    property ErrorIndicator: TComponent read FErrorIndicatorComponent write SetErrorIndicatorComponent;
    {$ENDIF COMPILER6_UP}
    property OnValidateFailed: TJvValidateFailEvent read FOnValidateFailed write FOnValidateFailed;
  end;

  TJvValidationSummary = class(TJvComponent, IUnknown, IJvValidationSummary)
  private
    FUpdateCount: Integer;
    FPendingUpdates: Integer;
    FSummaries: TStringList;
    FOnChange: TNotifyEvent;
    FOnRemoveError: TNotifyEvent;
    FOnAddError: TNotifyEvent;
    function GetSummaries: TStrings;
  protected
    { IJvValidationSummary }
    procedure AddError(const ErrorMessage: string);
    procedure RemoveError(const ErrorMessage: string);
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Change; virtual;
  public
    destructor Destroy; override;
    property Summaries: TStrings read GetSummaries;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnAddError: TNotifyEvent read FOnAddError write FOnAddError;
    property OnRemoveError: TNotifyEvent read FOnRemoveError write FOnRemoveError;
  end;

implementation

uses
  Masks,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  TypInfo,
  {$IFDEF VCL}
  JclUnicode, // for reg exp support
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  JvQWStrUtils,
  {$ENDIF VisualCLX}
  JvTypes, JvResources;

const
  sUnitName = 'JvValidators';

var
  GlobalValidatorsList: TStringList = nil;

procedure RegisterBaseValidators; forward;

function ValidatorsList: TStringList;
begin
  if not Assigned(GlobalValidatorsList) then
  begin
    GlobalValidatorsList := TStringList.Create;
    AddFinalizeObjectNil(sUnitname, TObject(GlobalValidatorsList));
   // register
    RegisterBaseValidators;
  end;
  Result := GlobalValidatorsList;
end;

procedure Debug(const Msg: string); overload;
begin
//  Application.MessageBox(PChar(Msg),PChar('Debug'),MB_OK or MB_TASKMODAL)
end;

procedure Debug(const Msg: string; const Fmt: array of const); overload;
begin
  Debug(Format(Msg, Fmt));
end;

function ComponentName(Comp: TComponent): string;
begin
  if Comp = nil then
    Result := 'nil'
  else
  if Comp.Name <> '' then
    Result := Comp.Name
  else
    Result := Comp.ClassName;
end;

{$IFNDEF COMPILER6_UP}

// these types and functions were introduced in D6
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
  CTruth: array [Boolean] of TVariantRelationship = (vrNotEqual, vrEqual);
var
  LA, LB: TVarData;
begin
  LA := FindVarData(A)^;
  LB := FindVarData(B)^;
  if LA.VType = varEmpty then
    Result := CTruth[LB.VType = varEmpty]
  else
  if LA.VType = varNull then
    Result := CTruth[LB.VType = varNull]
  else
  if LB.VType in [varEmpty, varNull] then
    Result := vrNotEqual
  else
  if A = B then
    Result := vrEqual
  else
  if A < B then
    Result := vrLessThan
  else
    Result := vrGreaterThan;
end;

{$ENDIF COMPILER6_UP}

//=== TJvBaseValidator =======================================================

constructor TJvBaseValidator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValid := True;
  FEnabled := True;
end;

destructor TJvBaseValidator.Destroy;
begin
  Debug('TJvBaseValidator.Destroy: FValidator is %s', [ComponentName(FValidator)]);
  ControlToValidate := nil;
  if FValidator <> nil then
  begin
    FValidator.Remove(Self);
    FValidator := nil;
  end;
  inherited Destroy;
end;

class procedure TJvBaseValidator.RegisterBaseValidator(const DisplayName: string; AValidatorClass:
  TJvBaseValidatorClass);
begin
  if ValidatorsList.IndexOfObject(Pointer(AValidatorClass)) < 0 then
  begin
    Classes.RegisterClass(TPersistentClass(AValidatorClass));
    ValidatorsList.AddObject(DisplayName, Pointer(AValidatorClass));
  end;
end;

class function TJvBaseValidator.BaseValidatorsCount: Integer;
begin
  Result := ValidatorsList.Count;
end;

class procedure TJvBaseValidator.GetBaseValidatorInfo(Index: Integer;
  var DisplayName: string; var ABaseValidatorClass: TJvBaseValidatorClass);
begin
  if (Index < 0) or (Index >= ValidatorsList.Count) then
    raise EJVCLException.CreateResFmt(@RsEInvalidIndexd, [Index]);
  DisplayName := ValidatorsList[Index];
  ABaseValidatorClass := TJvBaseValidatorClass(ValidatorsList.Objects[Index]);
end;

function TJvBaseValidator.GetValid: Boolean;
begin
  Result := FValid;
end;

function TJvBaseValidator.GetParentComponent: TComponent;
begin
  Debug('TJvBaseValidator.GetParentComponent: Parent is %s', [ComponentName(FValidator)]);
  Result := FValidator;
end;

function TJvBaseValidator.GetValidationPropertyValue: Variant;
var
  ValProp: IJvValidationProperty;
  PropInfo: PPropInfo;
begin
  Result := Null;
  if FControlToValidate <> nil then
  begin
    if Supports(FControlToValidate, IJvValidationProperty, ValProp) then
      Result := ValProp.GetValidationPropertyValue
    else
    if FPropertyToValidate <> '' then
    begin
      PropInfo := GetPropInfo(FControlToValidate, FPropertyToValidate);
      if (PropInfo <> nil) and (PropInfo^.GetProc <> nil) then
        Result := GetPropValue(FControlToValidate, FPropertyToValidate, False);
    end;
  end;
end;

function TJvBaseValidator.HasParent: Boolean;
begin
  Debug('TJvBaseValidator.HasParent');
  Result := True;
end;

procedure TJvBaseValidator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = ControlToValidate then
      ControlToValidate := nil;
end;

procedure TJvBaseValidator.SetValid(const Value: Boolean);
begin
  FValid := Value;
  if not FValid then
    DoValidateFailed;
end;

procedure TJvBaseValidator.SetControlToValidate(Value: TControl);
var
  Obj: IJvValidationProperty;
begin
  if FControlToValidate <> Value then
  begin
    if FControlToValidate <> nil then
      FControlToValidate.RemoveFreeNotification(Self);
    FControlToValidate := Value;
    if FControlToValidate <> nil then
    begin
      FControlToValidate.FreeNotification(Self);
      if Supports(FControlToValidate, IJvValidationProperty, Obj) then
        PropertyToValidate := Obj.GetValidationPropertyName;
    end;
  end;
end;

procedure TJvBaseValidator.SetParentComponent(Value: TComponent);
begin
  if not (csLoading in ComponentState) then
  begin
    Debug('TJvBaseValidator.SetParentComponent: Parent is %s, changing to %s',
      [ComponentName(FValidator), ComponentName(Value)]);
    if FValidator <> nil then
    begin
      Debug('FValidator.Remove');
      FValidator.Remove(Self);
    end;
    if (Value <> nil) and (Value is TJvValidators) then
    begin
      Debug('FValidator.Insert');
      TJvValidators(Value).Insert(Self);
    end;
  end;
end;

procedure TJvBaseValidator.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  Debug('TJvBaseValidator.ReadState: Reader.Parent is %s', [ComponentName(Reader.Parent)]);
  if Reader.Parent is TJvValidators then
  begin
    if FValidator <> nil then
      FValidator.Remove(Self);
    FValidator := TJvValidators(Reader.Parent);
    FValidator.Insert(Self);
  end;
end;

procedure TJvBaseValidator.DoValidateFailed;
begin
  if Assigned(FOnValidateFailed) then
    FOnValidateFailed(Self);
end;

//=== TJvRequiredFieldValidator ==============================================

procedure TJvRequiredFieldValidator.Validate;
var
  R: Variant;
begin
  R := GetValidationPropertyValue;
  Valid := VarCompareValue(R, '') <> vrEqual;
end;

//=== TJvCustomValidator =====================================================

function TJvCustomValidator.DoValidate: Boolean;
begin
  Result := Valid;
  if Assigned(FOnValidate) then
    FOnValidate(Self, GetValidationPropertyValue, Result);
end;

procedure TJvCustomValidator.Validate;
begin
  Valid := DoValidate;
end;

//=== TJvRegularExpressionValidator ==========================================

function MatchesMask(const Filename, Mask: string;
  const SearchFlags: TSearchFlags = [sfCaseSensitive]): Boolean;
{var
  URE: TURESearch;
  SL: TWideStringList;}
begin
  Result := Masks.MatchesMask(Filename, Mask);
  (*
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
  *)
end;

procedure TJvRegularExpressionValidator.Validate;
var
  R: string;
begin
  R := VarToStr(GetValidationPropertyValue);
  Valid := (R = ValidationExpression) or MatchesMask(R, ValidationExpression);
end;

//=== TJvCompareValidator ====================================================

procedure TJvCompareValidator.Validate;
var
  VR: TVariantRelationship;
begin
  VR := VarCompareValue(GetValidationPropertyValue, ValueToCompare);
  case Operator of
    vcoLessThan:
      Valid := VR = vrLessThan;
    vcoLessOrEqual:
      Valid := (VR = vrLessThan) or (VR = vrEqual);
    vcoEqual:
      Valid := (VR = vrEqual);
    vcoGreaterOrEqual:
      Valid := (VR = vrGreaterThan) or (VR = vrEqual);
    vcoGreaterThan:
      Valid := (VR = vrGreaterThan);
  end;
end;

//=== TJvRangeValidator ======================================================

procedure TJvRangeValidator.Validate;
var
  VR: TVariantRelationship;
begin
  VR := VarCompareValue(GetValidationPropertyValue, MinimumValue);
  Valid := (VR = vrGreaterThan) or (VR = vrEqual);
  if Valid then
  begin
    VR := VarCompareValue(GetValidationPropertyValue, MaximumValue);
    Valid := (VR = vrLessThan) or (VR = vrEqual);
  end;
end;

//=== TJvControlsCompareValidator ============================================

constructor TJvControlsCompareValidator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowNull := True;
end;

function TJvControlsCompareValidator.GetPropertyValueToCompare: Variant;
var
  ValProp: IJvValidationProperty;
  PropInfo: PPropInfo;
begin
  Result := Null;
  if FCompareToControl <> nil then
  begin
    if Supports(FCompareToControl, IJvValidationProperty, ValProp) then
      Result := ValProp.GetValidationPropertyValue
    else
    if FCompareToProperty <> '' then
    begin
      PropInfo := GetPropInfo(FCompareToControl, FCompareToProperty);
      if (PropInfo <> nil) and (PropInfo^.GetProc <> nil) then
        Result := GetPropValue(FCompareToControl, FCompareToProperty, False);
    end;
  end;
end;

procedure TJvControlsCompareValidator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = CompareToControl) then
    CompareToControl := nil;
end;

procedure TJvControlsCompareValidator.Validate;
var
  Val1, Val2: Variant;
  VR: TVariantRelationship;
begin
  Val1 := GetValidationPropertyValue;
  Val2 := GetPropertyValueToCompare;
  if not AllowNull and
    ((TVarData(Val1).VType in [varEmpty, varNull]) or (TVarData(Val2).VType in [varEmpty, varNull])) then
  begin
    Valid := False;
    Exit;
  end;
  VR := VarCompareValue(Val1, Val2);
  case Operator of
    vcoLessThan:
      Valid := VR = vrLessThan;
    vcoLessOrEqual:
      Valid := (VR = vrLessThan) or (VR = vrEqual);
    vcoEqual:
      Valid := (VR = vrEqual);
    vcoGreaterOrEqual:
      Valid := (VR = vrGreaterThan) or (VR = vrEqual);
    vcoGreaterThan:
      Valid := (VR = vrGreaterThan);
  end;
end;

//=== TJvValidators ==========================================================

constructor TJvValidators.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
end;

destructor TJvValidators.Destroy;
var
  V: TJvBaseValidator;
begin
  Debug('TJvValidators.Destroy: Count is %d', [FItems.Count]);
  while FItems.Count > 0 do
  begin
    V := TJvBaseValidator(FItems.Last);
    V.FValidator := nil;
    V.Free;
    FItems.Delete(FItems.Count - 1);
  end;
  FItems.Free;
  inherited Destroy;
end;

function TJvValidators.DoValidateFailed(const ABaseValidator: TJvBaseValidator): Boolean;
begin
  Result := True;
  if Assigned(FOnValidateFailed) then
    FOnValidateFailed(Self, ABaseValidator, Result);
end;

function TJvValidators.Validate: Boolean;
var
  I: Integer;
begin
  Result := True;
  if ValidationSummary <> nil then
    FValidationSummary.BeginUpdate;
  try
    for I := 0 to Count - 1 do
    begin
      if Items[I].Enabled then
      begin
        Items[I].Validate;
        if not Items[I].Valid then
        begin
          if (Items[I].ErrorMessage <> '') and (Items[I].ControlToValidate <> nil) then
          begin
            if ValidationSummary <> nil then
              FValidationSummary.AddError(Items[I].ErrorMessage);
            if ErrorIndicator <> nil then
              FErrorIndicator.SetError(Items[I].ControlToValidate, Items[I].ErrorMessage);
          end;
          Result := False;
          if not DoValidateFailed(Items[I]) then
            Exit;
        end;
      end;
    end;
  finally
    if ValidationSummary <> nil then
      FValidationSummary.EndUpdate;
  end;
end;

procedure TJvValidators.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    {$IFDEF COMPILER6_UP}
    if Assigned(ValidationSummary) and AComponent.IsImplementorOf(ValidationSummary) then
      ValidationSummary := nil;
    if Assigned(ErrorIndicator) and AComponent.IsImplementorOf(ErrorIndicator) then
      ErrorIndicator := nil;
    {$ELSE}
    if ValidationSummary = AComponent then
      ValidationSummary := nil;
    if ErrorIndicator = AComponent then
      ErrorIndicator := nil;
    {$ENDIF COMPILER6_UP}
  end;
end;

procedure TJvValidators.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  Debug('TJvValidators.GetChildren: Count is %d, Root is %s', [Count, ComponentName(Root)]);
  for I := 0 to Count - 1 do
    Proc(Items[I]);
end;

procedure TJvValidators.SetValidationSummary(const Value: IJvValidationSummary);
begin
  {$IFDEF COMPILER6_UP}
  ReferenceInterface(FValidationSummary, opRemove);
  FValidationSummary := Value;
  ReferenceInterface(FValidationSummary, opInsert);
  {$ELSE}
  FValidationSummary := Value;
  {$ENDIF COMPILER6_UP}
end;

{$IFNDEF COMPILER6_UP}

procedure TJvValidators.SetValidationSummaryComponent(Value: TComponent);
var
  Obj: IJvValidationSummary;
begin
  if Value <> FValidationSummaryComponent then
  begin
    if FValidationSummaryComponent <> nil then
      FValidationSummaryComponent.RemoveFreeNotification(Self);
    if Value = nil then
    begin
      FValidationSummaryComponent := nil;
      SetValidationSummary(nil);
      Exit;
    end;
    if not Supports(Value, IJvValidationSummary, Obj) then
      raise EValidatorError.CreateResFmt(@RsEInterfaceNotSupported, [Value.Name, 'IJvValidationSummary']);
    if Value = Self then
      raise EValidatorError.CreateRes(@RsECircularReference);
    SetValidationSummary(Obj);
    FValidationSummaryComponent := Value;
    FValidationSummaryComponent.FreeNotification(Self);
  end;
end;

procedure TJvValidators.SetErrorIndicatorComponent(Value: TComponent);
var
  Obj: IJvErrorIndicator;
begin
  if Value <> FErrorIndicatorComponent then
  begin
    if FErrorIndicatorComponent <> nil then
      FErrorIndicatorComponent.RemoveFreeNotification(Self);
    if Value = nil then
    begin
      FErrorIndicatorComponent := nil;
      SetErrorIndicator(nil);
      Exit;
    end;
    if not Supports(Value, IJvErrorIndicator, Obj) then
      raise EValidatorError.CreateResFmt(@RsEInterfaceNotSupported, [Value.Name, 'IJvErrorIndicator']);
    if Value = Self then
      raise EValidatorError.CreateRes(@RsECircularReference);
    SetErrorIndicator(Obj);
    FErrorIndicatorComponent := Value;
    FErrorIndicatorComponent.FreeNotification(Self);
  end;
end;

{$ENDIF COMPILER6_UP}

procedure TJvValidators.Insert(AValidator: TJvBaseValidator);
begin
  Debug('TJvValidators.Insert: inserting %s', [ComponentName(AValidator)]);
  Assert(AValidator <> nil, RsEInsertNilValidator);
  AValidator.FValidator := Self;
  if FItems.IndexOf(AValidator) < 0 then
    FItems.Add(AValidator);
end;

procedure TJvValidators.Remove(AValidator: TJvBaseValidator);
begin
  Debug('TJvValidators.Remove: removing %s', [ComponentName(AValidator)]);
  Assert(AValidator <> nil, RsERemoveNilValidator);
  Assert(AValidator.FValidator = Self, RsEValidatorNotChild);
  AValidator.FValidator := nil;
  FItems.Remove(AValidator);
end;

function TJvValidators.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvValidators.GetItem(Index: Integer): TJvBaseValidator;
begin
  Result := TJvBasevalidator(FItems[Index]);
end;

procedure TJvValidators.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

procedure TJvValidators.SetErrorIndicator(const Value: IJvErrorIndicator);
begin
  {$IFDEF COMPILER6_UP}
  ReferenceInterface(FErrorIndicator, opRemove);
  FErrorIndicator := Value;
  ReferenceInterface(FErrorIndicator, opInsert);
  {$ELSE}
  FErrorIndicator := Value;
  {$ENDIF COMPILER6_UP}
end;

//=== TJvValidationSummary ===================================================

destructor TJvValidationSummary.Destroy;
begin
  FSummaries.Free;
  inherited Destroy;
end;

procedure TJvValidationSummary.AddError(const ErrorMessage: string);
begin
  if Summaries.IndexOf(ErrorMessage) < 0 then
  begin
    Summaries.Add(ErrorMessage);
    if (FUpdateCount = 0) and Assigned(FOnAddError) then
      FOnAddError(Self);
    Change;
  end;
end;

procedure TJvValidationSummary.RemoveError(const ErrorMessage: string);
var
  I: Integer;
begin
  I := Summaries.IndexOf(ErrorMessage);
  if I > -1 then
  begin
    Summaries.Delete(I);
    if (FUpdateCount = 0) and Assigned(FOnRemoveError) then
      FOnRemoveError(Self);
    Change;
  end;
end;

function TJvValidationSummary.GetSummaries: TStrings;
begin
  if FSummaries = nil then
    FSummaries := TStringList.Create;
  Result := FSummaries;
end;

procedure TJvValidationSummary.Change;
begin
  if FUpdateCount <> 0 then
  begin
    Inc(FPendingUpdates);
    Exit;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvValidationSummary.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvValidationSummary.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  if (FUpdateCount = 0) and (FPendingUpdates > 0) then
  begin
    Change;
    FPendingUpdates := 0;
  end;
end;

procedure RegisterBaseValidators;
begin
  TJvBaseValidator.RegisterBaseValidator('Required Field Validator', TJvRequiredFieldValidator);
  TJvBaseValidator.RegisterBaseValidator('Compare Validator', TJvCompareValidator);
  TJvBaseValidator.RegisterBaseValidator('Range Validator', TJvRangeValidator);
  TJvBaseValidator.RegisterBaseValidator('Regular Expression Validator', TJvRegularExpressionValidator);
  TJvBaseValidator.RegisterBaseValidator('Custom Validator', TJvCustomValidator);
  TJvBaseValidator.RegisterBaseValidator('Controls Compare Validator', TJvControlsCompareValidator);
end;

initialization
  // (p3) do NOT touch! This is required to make the registration work!!!
  RegisterBaseValidators;

finalization
  FinalizeUnit(sUnitName);

end.

