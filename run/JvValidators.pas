{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvValidators.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3 at sourceforge dot net] .
Portions created by Peter Th�rnqvist are Copyright (C) 2003 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvValidators;

{$I jvcl.inc}
// NB: this is here so a user can disable DB support if he wants to
// NB2: this need not be defined in the design package because GetDataLink is
// defined differently depending on this define
{.$DEFINE JVVALIDATORS_SUPPORTS_DBCONTROLS}

interface

uses
  {$IFDEF JVVALIDATORS_SUPPORTS_DBCONTROLS}
  DB,
  {$ENDIF JVVALIDATORS_SUPPORTS_DBCONTROLS}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, Controls, Forms,
  JvComponentBase, JvErrorIndicator;

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
    FGroupName: string;
    FControlToValidate: TControl;
    FErrorControl: TControl;
    FValidator: TJvValidators;
    FOnValidateFailed: TNotifyEvent;

    procedure SetControlToValidate(Value: TControl);
    procedure SetErrorControl(Value: TControl);
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
    {$IFDEF JVVALIDATORS_SUPPORTS_DBCONTROLS}
    // return a TDataLink if the control is a DB control or nil if is not
    function GetDataLink(AControl:TControl):TDataLink;virtual;
    {$ELSE}
    function GetDataLink(AControl:TControl):TObject;virtual;
    {$ENDIF JVVALIDATORS_SUPPORTS_DBCONTROLS}
    // register a new base validator class. DisplayName is used by the design-time editor.
    // A class with an empty DisplayName will not sshow up in the editor
    class procedure RegisterBaseValidator(const DisplayName: string; AValidatorClass: TJvBaseValidatorClass);
    class procedure UnregisterBaseValidator(AValidatorClass: TJvBaseValidatorClass);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    property Value: Variant read GetValidationPropertyValue;
  published
    property Valid: Boolean read GetValid write SetValid default true;
    // the control that is used to align the error indicator (nil means that the ControlToValidate should be used)
    property ErrorControl: TControl read FErrorControl write SetErrorControl;
    // the control to validate
    property ControlToValidate: TControl read FControlToValidate write SetControlToValidate;
    // the property in ControlToValidate to validate against
    property PropertyToValidate: string read FPropertyToValidate write FPropertyToValidate;
    // make this validator a part of a group so it can be validated separately using Validate(GroupName)
    property GroupName:string read FGroupName write FGroupName;
    property Enabled: Boolean read FEnabled write FEnabled default true;
    // the message to display in case of error
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    // triggered when Valid is set to False
    property OnValidateFailed: TNotifyEvent read FOnValidateFailed write FOnValidateFailed;
  end;

  TJvRequiredFieldValidator = class(TJvBaseValidator)
  private
    FAllowBlank: Boolean;
  protected
    procedure Validate; override;
  published
    property AllowBlank: Boolean read FAllowBlank write FAllowBlank default true;
  end;

  TJvValidateCompareOperator = (vcoLessThan, vcoLessOrEqual, vcoEqual, vcoGreaterOrEqual, vcoGreaterThan, vcoNotEqual);

  TJvCompareValidator = class(TJvBaseValidator)
  private
    FValueToCompare: Variant;
    FOperator: TJvValidateCompareOperator;
  protected
    procedure Validate; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ValueToCompare: Variant read FValueToCompare write FValueToCompare;
    property Operator: TJvValidateCompareOperator read FOperator write FOperator default vcoEqual;
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
    procedure SetCompareToControl(const Value: TControl);
  protected
    procedure Validate; override;
    function GetPropertyValueToCompare: Variant;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property CompareToControl: TControl read FCompareToControl write SetCompareToControl;
    property CompareToProperty: string read FCompareToProperty write FCompareToProperty;
    property Operator: TJvValidateCompareOperator read FOperator write FOperator default vcoEqual;
    property AllowNull: Boolean read FAllowNull write FAllowNull default True;
  end;

  TJvValidateFailEvent = procedure(Sender: TObject; BaseValidator: TJvBaseValidator; var Continue: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvValidators = class(TJvComponent)
  private
    FOnValidateFailed: TJvValidateFailEvent;
    FItems: TList;
    FValidationSummary: IJvValidationSummary;
    FErrorIndicator: IJvErrorIndicator;
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
    function Validate: Boolean; overload;
    function Validate(const GroupName:string): Boolean; overload;
    property Items[Index: Integer]: TJvBaseValidator read GetItem; default;
    property Count: Integer read GetCount;
  published
    property ValidationSummary: IJvValidationSummary read FValidationSummary write SetValidationSummary;
    property ErrorIndicator: IJvErrorIndicator read FErrorIndicator write SetErrorIndicator;
    property OnValidateFailed: TJvValidateFailEvent read FOnValidateFailed write FOnValidateFailed;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
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

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

const
  cValidatorsDBValue = '(DBValue)';

implementation

uses
  {$IFDEF JVVALIDATORS_SUPPORTS_DBCONTROLS}
  DBCtrls,
  {$ENDIF JVVALIDATORS_SUPPORTS_DBCONTROLS}
  Masks,
  Variants,
  TypInfo,
//  JclUnicode, // for reg exp support
  JvTypes, JvResources, JvJVCLUtils;

var
  GlobalValidatorsList: TStringList = nil;

procedure RegisterBaseValidators; forward;

function ValidatorsList: TStringList;
begin
  if not Assigned(GlobalValidatorsList) then
  begin
    GlobalValidatorsList := TStringList.Create;
    // register
    //RegisterBaseValidators; is registered in initialization
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

//=== { TJvBaseValidator } ===================================================

constructor TJvBaseValidator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValid := True;
  FEnabled := True;
end;

destructor TJvBaseValidator.Destroy;
begin
  Debug('TJvBaseValidator.Destroy: FValidator is %s', [ComponentName(FValidator)]);
  ErrorControl := nil;
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

class procedure TJvBaseValidator.UnregisterBaseValidator(AValidatorClass: TJvBaseValidatorClass);
var
  ClassIndex: Integer;
begin
  ClassIndex := ValidatorsList.IndexOfObject(Pointer(AValidatorClass));
  if ClassIndex >= 0 then
  begin
    Classes.UnregisterClass(TPersistentClass(AValidatorClass));
    ValidatorsList.Delete(ClassIndex);
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
  {$IFDEF JVVALIDATORS_SUPPORTS_DBCONTROLS}
  DataLink:TDataLink;
  {$ENDIF JVVALIDATORS_SUPPORTS_DBCONTROLS}
begin
  Result := Null;
  if FControlToValidate <> nil then
  begin
    if Supports(FControlToValidate, IJvValidationProperty, ValProp) then
      Result := ValProp.GetValidationPropertyValue
    {$IFDEF JVVALIDATORS_SUPPORTS_DBCONTROLS}
    else if AnsiSameText(FPropertyToValidate,cValidatorsDBValue) then
    begin
      DataLink := GetDataLink(FControlToValidate);
      if (DataLink is TFieldDataLink) and (TFieldDataLink(DataLink).Field <> nil) then
        Result := TFieldDataLink(DataLink).Field.DisplayText;
    end
    {$ENDIF JVVALIDATORS_SUPPORTS_DBCONTROLS}
    else if FPropertyToValidate <> '' then
    begin
      PropInfo := GetPropInfo(FControlToValidate, FPropertyToValidate);
      if (PropInfo <> nil) and (PropInfo^.GetProc <> nil) then
      begin
        Result := GetPropValue(FControlToValidate, FPropertyToValidate, False);
        if (PropInfo.PropType^ = TypeInfo(TDateTime)) or
           (PropInfo.PropType^ = TypeInfo(TDate)) or
           (PropInfo.PropType^ = TypeInfo(TTime)) then
          Result := VarAsType(Result, varDate);
      end;
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
  begin
    if AComponent = ControlToValidate then
      ControlToValidate := nil;
    if AComponent = ErrorControl then
      ErrorControl := nil;
  end;
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
  if ReplaceComponentReference(Self, Value, TComponent(FControlToValidate)) then
    if FControlToValidate <> nil then
      if not (csLoading in ComponentState) then
      begin
        if Supports(FControlToValidate, IJvValidationProperty, Obj) then
          PropertyToValidate := Obj.GetValidationPropertyName
        else
          PropertyToValidate := '';
      end;
end;

procedure TJvBaseValidator.SetErrorControl(Value: TControl);
begin
  ReplaceComponentReference(Self, Value, TComponent(FErrorControl));
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

{$IFDEF JVVALIDATORS_SUPPORTS_DBCONTROLS}
function TJvBaseValidator.GetDataLink(AControl:TControl): TDataLink;
begin
  if AControl <> nil then
    Result := TDataLink(AControl.Perform(CM_GETDATALINK, 0, 0))
  else
    Result := nil;
end;
{$ELSE}
function TJvBaseValidator.GetDataLink(AControl:TControl):TObject;
begin
  Result := nil;
end;
{$ENDIF JVVALIDATORS_SUPPORTS_DBCONTROLS}


//=== { TJvRequiredFieldValidator } ==========================================

procedure TJvRequiredFieldValidator.Validate;
var
  R: Variant;
begin
  R := GetValidationPropertyValue;
  case VarType(R) of
    varDate:
      Valid := VarCompareValue(R, 0) <> vrEqual; // zero is the invalid value for dates
    varSmallint,
    varInteger,
    varSingle,
    varDouble,
    varCurrency,
    varBoolean,
    varByte:
      ; // nothing to do because all values are valid
  else
    if FAllowBlank then
      Valid := VarCompareValue(R, '') <> vrEqual
    else
      Valid := Trim(VarToStr(R)) <> '';
  end;
end;

//=== { TJvCustomValidator } =================================================

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

//=== { TJvRegularExpressionValidator } ======================================

function MatchesMask(const Filename, Mask: string{;
  const SearchFlags: TSearchFlags = [sfCaseSensitive]}): Boolean;
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

//=== { TJvCompareValidator } ================================================

constructor TJvCompareValidator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperator := vcoEqual;
end;

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
    vcoNotEqual:
      Valid := VR <> vrEqual;
  end;
end;

//=== { TJvRangeValidator } ==================================================

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

//=== { TJvControlsCompareValidator } ========================================

constructor TJvControlsCompareValidator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowNull := True;
  FOperator := vcoEqual;
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

procedure TJvControlsCompareValidator.SetCompareToControl(const Value: TControl);
var
  Obj: IJvValidationProperty;
begin
  if ReplaceComponentReference(Self, Value, TComponent(FCompareToControl)) then
    if FCompareToControl <> nil then
    begin
      if not (csLoading in ComponentState) then
      begin
        if Supports(FCompareToControl, IJvValidationProperty, Obj) then
          CompareToProperty := Obj.GetValidationPropertyName
        else
          CompareToProperty := '';
      end;
    end;
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
    vcoNotEqual:
      Valid := (VR <> vrEqual);
  end;
end;

//=== { TJvValidators } ======================================================

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

function TJvValidators.Validate(const GroupName:string): Boolean;
var
  I: Integer;
  Controls: TList;
  ErrCtrl: TControl;
begin
  Result := True;
  if ValidationSummary <> nil then
    FValidationSummary.BeginUpdate;
  try
    Controls := TList.Create;
    if FErrorIndicator <> nil then
      FErrorIndicator.BeginUpdate;
    try
      { Get all controls that should be validated }
      if FErrorIndicator <> nil then
        for I := 0 to Count - 1 do
        begin
          ErrCtrl := Items[i].ErrorControl;
          if ErrCtrl = nil then
            ErrCtrl := Items[i].ControlToValidate;
          if ErrCtrl <> nil then
            if Controls.IndexOf(ErrCtrl) = -1 then
              Controls.Add(ErrCtrl);
        end;

      for I := 0 to Count - 1 do
      begin
        if Items[I].Enabled and ((Items[I].GroupName = '') or AnsiSameText(GroupName, Items[I].GroupName)) then
        begin
          Items[I].Validate;
          if not Items[I].Valid then
          begin
            if (Items[I].ErrorMessage <> '') and (Items[I].ControlToValidate <> nil) then
            begin
              ErrCtrl := Items[I].ErrorControl;
              if ErrCtrl = nil then
                ErrCtrl := Items[i].ControlToValidate;

              if ValidationSummary <> nil then
                FValidationSummary.AddError(Items[I].ErrorMessage);
              if ErrorIndicator <> nil then
                FErrorIndicator.SetError(ErrCtrl, Items[I].ErrorMessage);
              if FErrorIndicator <> nil then
                Controls.Remove(ErrCtrl); { control is not valid }
            end;
            Result := False;
            if not DoValidateFailed(Items[I]) then
              Exit;
          end;
        end;
      end;
      { Clear ErrorIndicators for controls that are valid }
      if FErrorIndicator <> nil then
        for I := 0 to Controls.Count - 1 do
          FErrorIndicator.SetError(Controls[I], ''); // clear error indicator
    finally
      if FErrorIndicator <> nil then
        FErrorIndicator.EndUpdate;
      Controls.Free;
    end;
  finally
    if ValidationSummary <> nil then
      FValidationSummary.EndUpdate;
  end;
end;

function TJvValidators.Validate: Boolean;
begin
  Result := Validate('');
end;

procedure TJvValidators.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Assigned(ValidationSummary) and AComponent.IsImplementorOf(ValidationSummary) then
      ValidationSummary := nil;
    if Assigned(ErrorIndicator) and AComponent.IsImplementorOf(ErrorIndicator) then
      ErrorIndicator := nil;
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
  ReferenceInterface(FValidationSummary, opRemove);
  FValidationSummary := Value;
  ReferenceInterface(FValidationSummary, opInsert);
end;

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
  Result := TJvBaseValidator(FItems[Index]);
end;

procedure TJvValidators.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

procedure TJvValidators.SetErrorIndicator(const Value: IJvErrorIndicator);
begin
  ReferenceInterface(FErrorIndicator, opRemove);
  FErrorIndicator := Value;
  ReferenceInterface(FErrorIndicator, opInsert);
end;

//=== { TJvValidationSummary } ===============================================

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
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

  // (p3) do NOT touch! This is required to make the registration work on formulars!!!
  RegisterBaseValidators;

finalization
  FreeAndNil(GlobalValidatorsList);

  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
