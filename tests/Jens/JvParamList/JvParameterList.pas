{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens.fudickar@oratool.de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens.fudickar@oratool.de]

Last Modified: 2003-11-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvParameterList;

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Forms, Controls,
  Dialogs, ComCtrls,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvTypes, JvDynControlEngine, JvDynControlEngine_Interface, jvDsaDialogs,
  JvComponent, JvPanel, JvPropertyStore, JvAppStore, JvAppStoreSelectList;

type
  TJvParameterList = class
    ;
  TJvParameterListPropertyStore = class
    ;
  TJvParameterPropertyValues = class
    ;
  TJvParameterListSelectList = class
    ;

  TJvParameterListEnableDisableReason = class (TPersistent)
  private
    FRemoteParameterName: string;
    FValue: variant;
    FIsEmpty: boolean;
    FIsNotEmpty: boolean;
  protected
    procedure SetAsString(Value: string);
    function GetAsString: string;
    procedure SetAsDouble(Value: double);
    function GetAsDouble: double;
    procedure SetAsInteger(Value: integer);
    function GetAsInteger: integer;
    procedure SetAsBoolean(Value: boolean);
    function GetAsBoolean: boolean;
    procedure SetAsDate(Value: TDateTime);
    function GetAsDate: TDateTime;
    procedure SetAsVariant(Value: variant);
    function GetAsVariant: variant;
    procedure SetIsEmpty(Value: boolean);
    procedure SetIsNotEmpty(Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    property AsString: string read GetAsString write SetAsString;
    property AsDouble: double read GetAsDouble write SetAsDouble;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
    property IsEmpty: boolean read FIsEmpty write SetIsEmpty;
    property IsNotEmpty: boolean read FIsNotEmpty write SetIsNotEmpty;
    property RemoteParameterName: string read FRemoteParameterName write FRemoteParameterName;
  end;

  TJvParameterListEnableDisableReasonList = class (TStringList)
  public
    procedure Clear; override;
    procedure AddReasonVariant(RemoteParameterName: string; Value: variant);
    procedure AddReason(RemoteParameterName: string; Value: boolean); overload;
    procedure AddReason(RemoteParameterName: string; Value: integer); overload;
    procedure AddReason(RemoteParameterName: string; Value: double); overload;
    procedure AddReason(RemoteParameterName: string; Value: string); overload;
    procedure AddReason(RemoteParameterName: string; Value: TDateTime); overload;
    procedure AddReasonIsEmpty(RemoteParameterName: string);
    procedure AddReasonIsNotEmpty(RemoteParameterName: string);
  end;

  TJvParameterPropertyValue = class (TPersistent)
  private
    FPropertyName: string;
    FPropertyValue: variant;
  public
    property PropertyName: string read FPropertyName write FPropertyName;
    property PropertyValue: variant read FPropertyValue write FPropertyValue;
  end;

  TJvParameterPropertyValues = class (TStringList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddValue(AName: string; AValue: variant);
  end;

  TJvBaseParameter = class (TComponent)
  private
    FCaption: string;
    FValue: variant;
    FWidth: integer;
    FHeight: integer;
    FSearchName: string;
    FRequired: boolean;
    FReadOnly: boolean;
    FReloadValueFromRegistry: boolean;
    FParentParameterName: string;
    FTabOrder: integer;
    FParameterList: TJvParameterList;
    FWinControl: TWinControl;
    FJvDynControl: IJvDynControl;
    FJvDynControlData: IJvDynControlData;
    FHint: string;
    FColor: TColor;
    FEnabled: boolean;
    FHelpContext: THelpContext;
    FDisableReasons: TJvParameterListEnableDisableReasonList;
    FEnableReasons: TJvParameterListEnableDisableReasonList;
    FVisible: boolean;
  protected
    procedure SetAsString(Value: string); virtual;
    function GetAsString: string; virtual;
    procedure SetAsDouble(Value: double); virtual;
    function GetAsDouble: double; virtual;
    procedure SetAsInteger(Value: integer); virtual;
    function GetAsInteger: integer; virtual;
    procedure SetAsBoolean(Value: boolean); virtual;
    function GetAsBoolean: boolean; virtual;
    procedure SetAsDate(Value: TDateTime); virtual;
    function GetAsDate: TDateTime; virtual;
    procedure SetAsVariant(Value: variant); virtual;
    function GetAsVariant: variant; virtual;
    function GetParameterNameExt: string; virtual;
    function GetParameterNameBase: string;
    function GetParameterName: string;
    procedure SetWinControl(Value: TWinControl);
    function GetWinControl: TWinControl;
    function GetWinControlData: variant; virtual;
    procedure SetWinControlData(Value: variant); virtual;

    procedure SetEnabled(Value: boolean); virtual;

    procedure HandleEnableDisable(Sender: TObject);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetDynControlEngine: TJvDynControlEngine;
    property Color: TColor read FColor write FColor;
    property JvDynControl: IJvDynControl read FJvDynControl;
    property JvDynControlData: IJvDynControlData read FJvDynControlData;
    property Value: variant read FValue write FValue;
    property WinControl: TWinControl read GetWinControl write SetWinControl;
  public
    constructor Create(AParameterList: TJvParameterList); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: variant): boolean; virtual;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); virtual; abstract;
    property WinControlData: variant read GetWinControlData write SetWinControlData;
    procedure GetData; virtual;
    procedure SetData; virtual;
    property ParameterList: TJvParameterList read FParameterList write FParameterList;
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine;
  published
    {the next properties implements the possibilities to read and write the data }
    property AsString: string read GetAsString write SetAsString;
    property AsDouble: double read GetAsDouble write SetAsDouble;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
    {this name is used to identify the parameter in the parameterlist,
     this value must be defined before inserting into the parameterlist }
    property SearchName: string read FSearchName write FSearchName;
    {should this value be saved in the registry by the parameterlist }
    property ReloadValueFromRegistry: boolean read FReloadValueFromRegistry write FReloadValueFromRegistry;
    {the searchname of the parentparameter. The parameter must be a
     descent of TJvPanelParameter or TTabControlParamter. If the
     parent parameter is a TJvTabControlParameter, then the ParentParameterName must be
     "searchname.tabname" of the TJvTabControlParameter}
    property ParentParameterName: string read FParentParameterName write FParentParameterName;
    {Is the value required, will be checked in the validate function}
    property Required: boolean read FRequired write FRequired;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property Enabled: boolean read FEnabled write SetEnabled;
    property Visible: boolean read FVisible write FVisible;
    {the next properties find their expressions in the same properties of TWinControl }
    property Caption: string read FCaption write FCaption;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property Hint: string read FHint write FHint;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property TabOrder: integer read FTabOrder write FTabOrder;
    property DisableReasons: TJvParameterListEnableDisableReasonList read FDisableReasons;
    property EnableReasons: TJvParameterListEnableDisableReasonList read FEnableReasons;
  end;

  TJvParameterListMessages = class (TPersistent)
  private
    FCaption: string;
    FOkButton: string;
    FCancelButton: string;
    FHistoryLoadButton: string;
    FHistorySaveButton: string;
    FHistoryClearButton: string;
    FHistoryLoadCaption: string;
    FHistorySaveCaption: string;
    FHistoryClearCaption: string;
  public
    constructor Create;
  published
    property Caption: string read FCaption write FCaption;
    property OkButton: string read FOkButton write FOkButton;
    property CancelButton: string read FCancelButton write FCancelButton;
    property HistoryLoadButton: string read FHistoryLoadButton write FHistoryLoadButton;
    property HistorySaveButton: string read FHistorySaveButton write FHistorySaveButton;
    property HistoryClearButton: string read FHistoryClearButton write FHistoryClearButton;
    property HistoryLoadCaption: string read FHistoryLoadCaption write FHistoryLoadCaption;
    property HistorySaveCaption: string read FHistorySaveCaption write FHistorySaveCaption;
    property HistoryClearCaption: string read FHistoryClearCaption write FHistoryClearCaption;
  end;

  TJvParameterList = class (TJvComponent)
  private
    FMessages: TJvParameterListMessages;
    FIntParameterList: TStringList;
    FArrangeSettings: TJvArrangeSettings;
    FDynControlEngine: TJvDynControlEngine;
    FParameterDialog: TCustomForm;
    FWidth: integer;
    FHeight: integer;
    FAutoWidth: boolean;
    FAutoHeight: boolean;
    FMaxWidth: integer;
    FMaxHeight: integer;
    FOkButtonVisible: boolean;
    FCancelButtonVisible: boolean;
    FParameterListPropertyStore: TJvParameterListPropertyStore;
    FHistoryEnabled: boolean;
    FLastHistoryName: string;
    FParameterListSelectList: TJvParameterListSelectList;
    function AddObject(const S: string; AObject: TObject): integer;
    procedure InsertObject(Index: integer; const S: string; AObject: TObject);
    procedure OnOkButtonClick(Sender: TObject);
    procedure OnCancelButtonClick(Sender: TObject);
  protected
    ArrangePanel: TJvPanel;
    ScrollBox: TScrollBox;
    RightPanel: TJvPanel;
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
    procedure SetPath(Value: string);
    function GetPath: string;
    function GetAppStore: TJvCustomAppStore;
    procedure SetAppStore(Value: TJvCustomAppStore);

    procedure SetDynControlEngine(Value: TJvDynControlEngine);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetParentByName(MainParent: TWinControl; SearchName: string): TWinControl;
    function GetCount: integer;

    procedure SetParameters(Index: integer; Value: TJvBaseParameter);
    function GetParameters(Index: integer): TJvBaseParameter;

    function GetCurrentWidth: integer;
    function GetCurrentHeight: integer;

    procedure HistoryLoadClick(Sender: TObject);
    procedure HistorySaveClick(Sender: TObject);
    procedure HistoryClearClick(Sender: TObject);

    property IntParameterList: TStringList read FIntParameterList;

    property ParameterDialog: TCustomForm read FParameterDialog;
    property ParameterListSelectList: TJvParameterListSelectList read FParameterListSelectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Saves the data of all allowed parameters to the registry }
    procedure StoreData;
    { load the data of all allowed parameters from the registry }
    procedure LoadData;
    {Adds a new Parameter to the parameterlist }
    procedure AddParameter(AParameter: TJvBaseParameter);
    {returns the parameter identified by the Searchname}
    function ParameterByName(ASearchName: string): TJvBaseParameter;
    {returns True id the parameter identified by the Searchname exists}
    function ExistsParameter(ASearchName: string): boolean;
    {returns the parameter identified by index-position}
    function ParamByIndex(AIndex: integer): TJvBaseParameter;
    {executes a dialog to enter all Parameter-Data,
     returns True when ok-button pressed}
    function ShowParameterDialog: boolean;
    { Creates the ParameterDialog }
    procedure CreateParameterDialog;
    {creates the components of all parameters on any WinControl}
    procedure CreateWinControlsOnParent(ParameterParent: TWinControl);
    {Destroy the WinControls of all parameters}
    procedure DestroyWinControls;
    { reads the data of all parameters from the WinControls}
    procedure GetDataFromWinControls;
    procedure SetDataToWinControls;
    { validates the data of all parameters without filling the data into
     the parameters }
    function ValidateDataAtWinControls: boolean;
    {deletes alll Parameters from the Parameterlist}
    procedure Clear;
    {this procedure checks the autoscroll-property of the internal
     scrollbox. This function should only be called, after the size of
     the parent-panel has changed}
    procedure CheckScrollBoxAutoScroll;
    { count of parameters }
    property Count: integer read GetCount;
    {returns the current height of the created main-parameter-panel}
    property CurrentWidth: integer read GetCurrentWidth;
    {returns the current height of the created main-parameter-panel}
    property CurrentHeight: integer read GetCurrentHeight;
    property DynControlEngine: TJvDynControlEngine read FDynControlEngine write SetDynControlEngine;
    { Property to get access to the parameters }
    property Parameters[Index: integer]: TJvBaseParameter read GetParameters write SetParameters;
  published
    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write SetArrangeSettings;
    property Messages: TJvParameterListMessages read FMessages;
    property Path: string read GetPath write SetPath;
    {Width of the dialog. When width = 0, then the width will be calculated }
    property Width: integer read FWidth write FWidth;
    {Height of the dialog. When height = 0, then the Height will be calculated }
    property Height: integer read FHeight write FHeight;
    {Property to define that the dialog height should be calculated automaticly }
    property AutoWidth: boolean read FAutoWidth write FAutoWidth;
    {Property to define that the dialog height should be calculated automaticly }
    property AutoHeight: boolean read FAutoHeight write FAutoHeight;
    {Maximum ClientWidth of the Dialog}
    property MaxWidth: integer read FMaxWidth write FMaxWidth default 400;
    {Maximum ClientHeight of the Dialog}
    property MaxHeight: integer read FMaxHeight write FMaxHeight default 600;
    property OkButtonVisible: boolean read FOkButtonVisible write FOkButtonVisible;
    property CancelButtonVisible: boolean read FCancelButtonVisible write FCancelButtonVisible;
    property HistoryEnabled: boolean read FHistoryEnabled write FHistoryEnabled;
    property LastHistoryName: string read FLastHistoryName write FLastHistoryName;
    property AppStore: TJvCustomAppStore read GetAppStore write SetAppStore;
  end;

  TJvParameterListSelectList = class (TJvAppStoreSelectList)
  private
    FParameterList: TJvParameterList;
  protected
    function GetDynControlEngine: TJvDynControlEngine; override;
    function GetParameterList: TJvParameterList; virtual;
    procedure SetParameterList(Value: TJvParameterList); virtual;
    function GetAppStore: TJvCustomAppStore; override;
    procedure SetAppStore(Value: TJvCustomAppStore); override;
  public
    // constructor Create(AOwner: TComponent); override;
    // destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RestoreParameterList(ACaption: string = '');
    procedure SaveParameterList(ACaption: string = '');
  published
    property ParameterList: TJvParameterList read GetParameterList write SetParameterList;
  end;

  TJvParameterListPropertyStore = class (TJvCustomPropertyStore)
  private
    FParameterList: TJvParameterList;
  protected
    procedure LoadData; override;
    procedure StoreData; override;
  public
    property ParameterList: TJvParameterList read FParameterList write FParameterList;
  end;

implementation

uses
  JvParameterList_Parameter;

resourcestring
  SErrParameterMustBeEntered = 'Parameter %s must be entered!';
  SHistorySelectPath = 'History';

  SCaption      = '';
  SOkButton     = '&Ok';
  SCancelButton = '&Cancel';
  SHistoryLoadButton = '&Load';
  SHistorySaveButton = '&Save';
  SHistoryClearButton = 'Cl&ear';
  SHistoryLoadCaption = 'Load Parameter Settings';
  SHistorySaveCaption = 'Save Parameter Settings';
  SHistoryClearCaption = 'Manage Parameter Settings';

  SNoParametersDefined      = 'TJvParameterList.ShowParameterDialog: No Parameters defined';
  SAddObjectWrongObjectType = 'TJvParameterList.AddObject: Wrong object type';
  SAddObjectSearchNameNotDefined = 'TJvParameterList.AddObject: SearchName not defined';
  SInsertObjectWrongObjectType = 'TJvParameterList.InsertObject: Wrong object type';
  SInsertObjectSearchNameNotDefined = 'TJvParameterList.InsertObject: SearchName not defined';

//=== TJvParameterListMessages ===============================================

constructor TJvParameterListMessages.Create;
begin
  inherited Create;
  Caption      := SCaption;
  OkButton     := SOkButton;
  CancelButton := SCancelButton;
  HistoryLoadButton := SHistoryLoadButton;
  HistorySaveButton := SHistorySaveButton;
  HistoryClearButton := SHistoryClearButton;
  HistoryLoadCaption := SHistoryLoadCaption;
  HistorySaveCaption := SHistorySaveCaption;
  HistoryClearCaption := SHistoryClearCaption;
end;

//=== TJvParameterListEnableDisableReason ====================================

procedure TJvParameterListEnableDisableReason.SetAsString(Value: string);
begin
  AsVariant := Value;
end;

function TJvParameterListEnableDisableReason.GetAsString: string;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetAsDouble(Value: double);
begin
  AsVariant := Value;
end;

function TJvParameterListEnableDisableReason.GetAsDouble: double;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetAsInteger(Value: integer);
begin
  AsVariant := Value;
end;

function TJvParameterListEnableDisableReason.GetAsInteger: integer;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetAsBoolean(Value: boolean);
begin
  if Value then
    AsVariant := 'TRUE'
  else
    AsVariant := 'FALSE';
end;

function TJvParameterListEnableDisableReason.GetAsBoolean: boolean;
var
  S: string;
begin
  S      := FValue;
  Result := S = 'TRUE';
end;

procedure TJvParameterListEnableDisableReason.SetAsDate(Value: TDateTime);
begin
  AsVariant := VarFromDateTime(Value);
end;

function TJvParameterListEnableDisableReason.GetAsDate: TDateTime;
begin
  Result := VarToDateTime(FValue);
end;

procedure TJvParameterListEnableDisableReason.SetAsVariant(Value: variant);
begin
  FValue := Value;
end;

function TJvParameterListEnableDisableReason.GetAsVariant: variant;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetIsEmpty(Value: boolean);
begin
  // IsEmpty and NotIsEmtpy can both be false, in this case the Reason looks
  // for the value to activate/deactivate
  FIsEmpty := Value;
  if Value then
    IsNotEmpty := false;
end;

procedure TJvParameterListEnableDisableReason.SetIsNotEmpty(Value: boolean);
begin
  // IsEmpty and NotIsEmtpy can both be false, in this case the Reason looks
  // for the value to activate/deactivate
  FIsNotEmpty := Value;
  if Value then
    IsEmpty := false;
end;

procedure TJvParameterListEnableDisableReason.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  AsVariant  := TJvParameterListEnableDisableReason(Source).AsVariant;
  IsEmpty    := TJvParameterListEnableDisableReason(Source).IsEmpty;
  IsNotEmpty := TJvParameterListEnableDisableReason(Source).IsNotEmpty;
  RemoteParameterName := TJvParameterListEnableDisableReason(Source).RemoteParameterName;
end;

//=== TJvParameterListEnableDisableReasonList ================================

procedure TJvParameterListEnableDisableReasonList.Clear;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited Clear;
end;

procedure TJvParameterListEnableDisableReasonList.AddReasonVariant(RemoteParameterName: string; Value: variant);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsVariant := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName: string; Value: boolean);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsBoolean := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName: string; Value: integer);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsInteger := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName: string; Value: double);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsDouble := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName: string; Value: string);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsString := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName: string; Value: TDateTime);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsDate := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReasonIsEmpty(RemoteParameterName: string);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.IsEmpty := true;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReasonIsNotEmpty(RemoteParameterName: string);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.IsNotEmpty := true;
  AddObject(RemoteParameterName, Reason);
end;

//=== TJvParameterPropertyValues =============================================

constructor TJvParameterPropertyValues.Create;
begin
  inherited Create;
  Sorted     := true;
  Duplicates := dupIgnore;
end;

destructor TJvParameterPropertyValues.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJvParameterPropertyValues.Clear;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
end;

procedure TJvParameterPropertyValues.AddValue(AName: string; AValue: variant);
var
  Value: TJvParameterPropertyValue;
begin
  Value := TJvParameterPropertyValue.Create;
  Value.PropertyName := AName;
  Value.PropertyValue := AValue;
  AddObject(AName, Value)
end;

//=== TJvBaseParameter =======================================================

constructor TJvBaseParameter.Create(AParameterList: TJvParameterList);
begin
  inherited Create(AParameterList);
  FReloadValueFromRegistry := true;
  FTabOrder := -1;
  FParameterList := AParameterList;
  FWinControl := nil;
  FJvDynControl := nil;
  FJvDynControlData := nil;
  Color    := clBtnFace;
  FEnabled := true;
  FVisible := true;
  FEnableReasons := TJvParameterListEnableDisableReasonList.Create;
  FDisableReasons := TJvParameterListEnableDisableReasonList.Create;
end;

destructor TJvBaseParameter.Destroy;
begin
  FreeAndNil(FEnableReasons);
  FreeAndNil(FDisableReasons);
  inherited Destroy;
end;

procedure TJvBaseParameter.SetAsString(Value: string);
begin
  AsVariant := Value;
end;

function TJvBaseParameter.GetAsString: string;
begin
  if VarIsNull(AsVariant) then
    Result := ''
  else
    Result := AsVariant;
end;

procedure TJvBaseParameter.SetAsDouble(Value: double);
begin
  AsVariant := Value;
end;

function TJvBaseParameter.GetAsDouble: double;
begin
  if AsString = '' then
    Result := 0
  else
    Result := AsVariant;
end;

procedure TJvBaseParameter.SetAsInteger(Value: integer);
begin
  AsVariant := Value;
end;

function TJvBaseParameter.GetAsInteger: integer;
begin
  if VarIsNull(AsVariant) then
    Result := 0
  else
    Result := AsVariant;
end;

procedure TJvBaseParameter.SetAsBoolean(Value: boolean);
begin
  if Value then
    AsVariant := 'TRUE'
  else
    AsVariant := 'FALSE';
end;

function TJvBaseParameter.GetAsBoolean: boolean;
var
  S: string;
begin
  if VarIsNull(FValue) then
    Result := false
  else
  begin
    S      := AsVariant;
    Result := UpperCase(s) = 'TRUE';
  end;
end;

procedure TJvBaseParameter.SetAsDate(Value: TDateTime);
begin
  AsVariant := VarFromDateTime(Value);
end;

function TJvBaseParameter.GetAsDate: TDateTime;
begin
  if VarIsNull(FValue) then
    Result := 0
  else
    Result := VarToDateTime(FValue);
end;

procedure TJvBaseParameter.SetAsVariant(Value: variant);
begin
  FValue := Value;
 //  if Assigned(FJvDynControlData) then
 //    FJvDynControlData.Value := Value;
end;

function TJvBaseParameter.GetAsVariant: variant;
begin
 //  if Assigned(FJvDynControlData) then
 //    Result := FJvDynControlData.Value
 //  else
  Result := FValue;
end;

procedure TJvBaseParameter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FWinControl) and (Operation = opRemove) then
  begin
    FWinControl   := nil;
    FJvDynControl := nil;
    FJvDynControlData := nil;
  end;
end;

function TJvBaseParameter.GetWinControlData: variant;
begin
  Result := Null;
  if Assigned(JvDynControlData) then
    Result := JvDynControlData.ControlValue;
end;

procedure TJvBaseParameter.SetWinControlData(Value: variant);
begin
  if Assigned(JvDynControlData) then
    JvDynControlData.ControlValue := Value;
end;

function TJvBaseParameter.GetDynControlEngine: TJvDynControlEngine;
begin
  Result := nil;
  if Assigned(ParameterList) then
    Result := ParameterList.DynControlEngine;
end;

type
  THackWinControl = class (TWinControl)
  public
    property OnExit;
  end;

procedure TJvBaseParameter.SetWinControl(Value: TWinControl);

begin
  FJvDynControl := nil;
  FWinControl   := Value;
  if not Assigned(Value) then
    Exit;
  Supports(FWinControl, IJvDynControl, FJvDynControl);
  Supports(FWinControl, IJvDynControlData, FJvDynControlData);

  JvDynControl.ControlSetCaption(Caption);
  if Assigned(JvDynControlData) then
    JvDynControlData.ControlSetReadOnly(ReadOnly);
  WinControl.Visible := Visible;
  WinControl.Enabled := Enabled;
  WinControl.Hint    := Hint;
  WinControl.HelpContext := HelpContext;
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
end;

function TJvBaseParameter.GetWinControl: TWinControl;
begin
  Result := FWinControl
end;

procedure TJvBaseParameter.SetEnabled(Value: boolean);
begin
  FEnabled := Value;
  if Assigned(WinControl) then
    WinControl.Enabled := Value;
end;

procedure TJvBaseParameter.GetData;
begin
  FValue := Null;
  if Assigned(WinControl) then
    FValue := WinControlData;
end;

procedure TJvBaseParameter.SetData;
begin
  if Assigned(WinControl) then
    WinControlData := FValue;
end;

procedure TJvBaseParameter.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  AsVariant  := TJvBaseParameter(Source).AsVariant;
  Caption    := TJvBaseParameter(Source).Caption;
  SearchName := TJvBaseParameter(Source).SearchName;
  Width      := TJvBaseParameter(Source).Width;
  Height     := TJvBaseParameter(Source).Height;
  Required   := TJvBaseParameter(Source).Required;
  ParentParameterName := TJvBaseParameter(Source).ParentParameterName;
  ReloadValueFromRegistry := TJvBaseParameter(Source).ReloadValueFromRegistry;
  TabOrder   := TJvBaseParameter(Source).TabOrder;
  FParameterList := TJvBaseParameter(Source).ParameterList;
  Color      := TJvBaseParameter(Source).Color;
  ReadOnly   := TJvBaseParameter(Source).ReadOnly;
  Enabled    := TJvBaseParameter(Source).Enabled;
  FEnableReasons.Assign(TJvBaseParameter(Source).FEnableReasons);
  FDisableReasons.Assign(TJvBaseParameter(Source).FDisableReasons);
end;

function TJvBaseParameter.Validate(var AData: variant): boolean;
begin
  if not Required or not Enabled then
    Result := true
  else
    Result := not VarIsNull(AData);
  if not Result then
    JvDSADialogs.MessageDlg(Format(SErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
end;

function TJvBaseParameter.GetParameterNameExt: string;
begin
  Result := '';
end;

function TJvBaseParameter.GetParameterNameBase: string;
begin
  Result := 'ParameterItem' + SearchName;
end;

function TJvBaseParameter.GetParameterName: string;
begin
  Result := GetParameterNameBase + GetParameterNameExt;
end;

procedure TJvBaseParameter.HandleEnableDisable(Sender: TObject);
var
  IEnable:   integer;
  Reason:    TJvParameterListEnableDisableReason;
  I, J:      integer;
  Parameter: TJvBaseParameter;
  HandleParameter: TJvBaseParameter;
  Data:      variant;
begin
  if not (Sender is TWinControl) then
    Exit;
  if not Assigned(ParameterList) then
    Exit;
  HandleParameter := nil;
  for I := 0 to ParameterList.Count - 1 do
    if Assigned(ParameterList.ParamByIndex(I).WinControl) then
      if Sender = ParameterList.ParamByIndex(I).WinControl then
      begin
        HandleParameter := ParameterList.ParamByIndex(I);
        Break;
      end; {*** IF Sender = ParameterList.ParamByIndex(I).WinControl ***}
  if not Assigned(HandleParameter) then
    Exit;
  Data := HandleParameter.GetWinControlData;
  if VarIsNull(Data) then
    Exit;
  for I := 0 to ParameterList.Count - 1 do
  begin
    Parameter := ParameterList.ParamByIndex(I);
    if not Assigned(Parameter) then
      Continue;
    IEnable := 0;
    if Parameter.EnableReasons.Count > 0 then
    begin
      IEnable := -1;
      for J := 0 to Parameter.EnableReasons.Count - 1 do
      begin
        Reason := TJvParameterListEnableDisableReason(Parameter.EnableReasons.Objects[J]);
        if not Assigned(Reason) then
          Continue;
        if Reason.RemoteParameterName <> HandleParameter.SearchName then
          Continue;
        if VarIsNull(Reason.AsVariant) then
          Continue;
        if IEnable = 0 then
          IEnable := -1;
        if (Reason.AsVariant = Data) then
          IEnable := 1;
      end;
    end;
    if Parameter.DisableReasons.Count > 0 then
    begin
      for J := 0 to Parameter.DisableReasons.Count - 1 do
      begin
        Reason := TJvParameterListEnableDisableReason(Parameter.DisableReasons.Objects[J]);
        if not Assigned(Reason) then
          Continue;
        if Reason.RemoteParameterName <> HandleParameter.SearchName then
          Continue;
        if VarIsNull(Reason.AsVariant) then
          Continue;
        if IEnable = 0 then
          IEnable := 1;
        if Reason.AsVariant = Data then
          IEnable := -1;
      end;
    end;
    case IEnable of
      -1:
        Parameter.Enabled := false;
      1:
        Parameter.Enabled := true;
    end;
  end;
end;

//=== TJvParameterList =======================================================

constructor TJvParameterList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessages := TJvParameterListMessages.Create;
  FParameterListPropertyStore := TJvParameterListPropertyStore.Create(nil);
  FParameterListPropertyStore.Parameterlist := Self;
  FIntParameterList := TStringList.Create;
  FDynControlEngine := DefaultDynControlEngine;
  FArrangeSettings := TJvArrangeSettings.Create(nil);
  with FArrangeSettings do
  begin
    AutoArrange  := true;
    WrapControls := true;
    AutoSize     := asBoth;
    DistanceVertical := 3;
    DistanceHorizontal := 3;
    BorderLeft   := 5;
    BorderTop    := 5;
  end;
  ScrollBox    := nil;
  RightPanel   := nil;
  ArrangePanel := nil;
  FMaxWidth    := 600;
  FMaxHeight   := 400;
  fOkbuttonVisible := true;
  FCancelButtonVisible := true;
  FHistoryEnabled := false;
  FLastHistoryName := '';
  FParameterListSelectList := TJvParameterListSelectList.Create(Self);
  FParameterListSelectList.ParameterList := Self;
end;

destructor TJvParameterList.Destroy;
begin
  DestroyWinControls;
  FreeAndNil(FParameterListSelectList);
  FreeAndNil(FIntParameterList);
  FreeAndNil(FParameterListPropertyStore);
  FreeAndNil(FArrangeSettings);
  FreeAndNil(FMessages);
  inherited Destroy;
end;

procedure TJvParameterList.AddParameter(AParameter: TJvBaseParameter);
begin
  AddObject(AParameter.SearchName, AParameter);
end;

function TJvParameterList.ExistsParameter(ASearchName: string): boolean;
begin
  Result := Assigned(ParameterByName(ASearchName));
end;

function TJvParameterList.ParameterByName(ASearchName: string): TJvBaseParameter;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if UpperCase(Parameters[I].SearchName) = UpperCase(ASearchName) then
    begin
      Result := Parameters[I];
      Break;
    end;
end;

function TJvParameterList.ParamByIndex(AIndex: integer): TJvBaseParameter;
begin
  Result := Parameters[AIndex];
end;

procedure TJvParameterList.Assign(Source: TPersistent);
begin
  // (rom) no inherited Assign?
  Messages.Assign(TJvParameterList(Source).Messages);
  ArrangeSettings := TJvParameterList(Source).ArrangeSettings;
  AppStore   := TJvParameterList(Source).AppStore;
  Width      := TJvParameterList(Source).Width;
  Height     := TJvParameterList(Source).Height;
  MaxWidth   := TJvParameterList(Source).MaxWidth;
  MaxHeight  := TJvParameterList(Source).MaxHeight;
  AutoWidth  := TJvParameterList(Source).AutoWidth;
  AutoHeight := TJvParameterList(Source).AutoHeight;
  OkButtonVisible := TJvParameterList(Source).OkButtonVisible;
  CancelButtonVisible := TJvParameterList(Source).CancelButtonVisible;
  FIntParameterList.Assign(TJvParameterList(Source).FIntParameterList);
  HistoryEnabled := TJvParameterList(Source).HistoryEnabled;
  Path := TJvParameterList(Source).Path;
end;

procedure TJvParameterList.SetPath(Value: string);
begin
  FParameterListPropertyStore.Path := Value;
  if Assigned(AppStore) then
    FParameterListSelectList.SelectPath := AppStore.ConcatPaths([Value, SHistorySelectPath])
end;

function TJvParameterList.GetPath: string;
begin
  Result := FParameterListPropertyStore.Path;
end;

function TJvParameterList.GetAppStore: TJvCustomAppStore;
begin
  Result := FParameterListPropertyStore.AppStore;
end;

procedure TJvParameterList.SetAppStore(Value: TJvCustomAppStore);
begin
  FParameterListPropertyStore.AppStore := Value;
  if Assigned(Value) then
    FParameterListSelectList.SelectPath := Value.ConcatPaths([FParameterListPropertyStore.Path, SHistorySelectPath])
end;

procedure TJvParameterList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = ScrollBox then
      ScrollBox := nil;
    if AComponent = RightPanel then
      RightPanel := nil;
    if AComponent = ArrangePanel then
      ArrangePanel := nil;
    if AComponent = FParameterListPropertyStore then
      FParameterListPropertyStore := nil;
  end;
end;

procedure TJvParameterList.SetDynControlEngine(Value: TJvDynControlEngine);
begin
  FDynControlEngine := Value;
end;

procedure TJvParameterList.StoreData;
begin
  if Path <> '' then
    FParameterListPropertyStore.StoreData;
end;

procedure TJvParameterList.LoadData;
begin
  if Path <> '' then
    FParameterListPropertyStore.LoadData;
end;

procedure TJvParameterList.OnOkButtonClick(Sender: TObject);
begin
  if ValidateDataAtWinControls then
    ParameterDialog.ModalResult := mrOk;
end;

procedure TJvParameterList.OnCancelButtonClick(Sender: TObject);
begin
  ParameterDialog.ModalResult := mrCancel;
end;

type
  THackPanel = class (TCustomControl)
  public
    property Canvas;
  end;

procedure TJvParameterList.CreateParameterDialog;
var
  MainPanel, BottomPanel, HistoryPanel, ButtonPanel: TWinControl;
  OkButton, CancelButton: TWinControl;
  LoadButton, SaveButton, ClearButton: TWinControl;
  ButtonLeft: integer;
  ITmpPanel:  IJvDynControlPanel;
begin
  FreeAndNil(FParameterDialog);

  FParameterDialog := DynControlEngine.CreateForm('', '');

  with TForm(ParameterDialog) do
  begin
    BorderIcons := [];
    DefaultMonitor := dmActiveForm;
    FormStyle := fsNormal;
    BorderStyle := bsDialog;
    Position := poScreenCenter;
  end;

  if Height > 0 then
    ParameterDialog.Height := Height;
  if Width > 0 then
    ParameterDialog.Width := Width;

  BottomPanel := DynControlEngine.CreatePanelControl(Self, ParameterDialog, 'BottomPanel', '', alBottom);
  if not Supports(BottomPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.Create(SIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvRaised, 1, bsNone, 0);

  MainPanel := DynControlEngine.CreatePanelControl(Self, ParameterDialog, 'MainPanel', '', alClient);
  if not Supports(MainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.Create(SIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvRaised, 1, bsNone, 3);

  ButtonPanel := DynControlEngine.CreatePanelControl(Self, BottomPanel, 'BottonPanel', '', alRight);
  if not Supports(ButtonPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.Create(SIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 0);

  OkButton     := DynControlEngine.CreateButton(Self, ButtonPanel, 'OkButton', Messages.OkButton, '', OnOkButtonClick, true, false);
  CancelButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'CancelButton', Messages.CancelButton, '', OnCancelButtonClick, false, true);

  BottomPanel.Height := OkButton.Height + 6 + 2;

  OkButton.Top     := 3;
  OkButton.Left    := 3;
  OkButton.Visible := OkButtonVisible;
  OkButton.Enabled := OkButtonVisible;
  if OkButton.Visible then
    ButtonLeft := OkButton.Left + OkButton.Width + 3
  else
    ButtonLeft := 0;

  CancelButton.Top     := 3;
  CancelButton.Left    := ButtonLeft + 3;
  CancelButton.Visible := CancelButtonVisible;
  CancelButton.Enabled := CancelButtonVisible;
  if CancelButton.Visible then
    ButtonLeft := ButtonLeft + 3 + CancelButton.Width + 3;

  ButtonPanel.Width := ButtonLeft + 3;

  OkButton.Anchors     := [akTop, akRight];
  CancelButton.Anchors := [akTop, akRight];

  if HistoryEnabled and (Path <> '') then
  begin
    ButtonLeft   := 0;
    HistoryPanel := DynControlEngine.CreatePanelControl(Self, BottomPanel, 'HistoryPanel', '', alLeft);
    if not Supports(HistoryPanel, IJvDynControlPanel, ITmpPanel) then
      raise EIntfCastError.Create(SIntfCastError);
    with ITmpPanel do
      ControlSetBorder(bvNone, bvNone, 0, bsNone, 0);
    with HistoryPanel do
      Height := 25;
    LoadButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'LoadButton', Messages.HistoryLoadButton, '', HistoryLoadClick, false, false);
    with LoadButton do
    begin
      Left   := 6;
      Top    := 5;
      Height := 20;
      Width  := THackPanel(HistoryPanel).Canvas.TextWidth(Messages.HistoryLoadButton) + 5;
      ButtonLeft := Left + Width + 5;
    end;
    SaveButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'SaveButton', Messages.HistorySaveButton, '', HistorySaveClick, false, false);
    with SaveButton do
    begin
      Left   := ButtonLeft;
      Top    := 5;
      Height := 20;
      Width  := THackPanel(HistoryPanel).Canvas.TextWidth(Messages.HistorySaveButton) + 5;
      ButtonLeft := Left + Width + 5;
    end;
    ClearButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'ClearButton', Messages.HistoryClearButton, '', HistoryClearClick, false, false);
    with ClearButton do
    begin
      Left   := ButtonLeft;
      Top    := 5;
      Height := 20;
      Width  := THackPanel(HistoryPanel).Canvas.TextWidth(Messages.HistoryClearButton) + 5;
      ButtonLeft := Left + Width + 5;
    end;
    HistoryPanel.Width := ButtonLeft;
  end;

  CreateWinControlsOnParent(MainPanel);

  if AutoWidth then
    if ArrangePanel.Width > TForm(ParameterDialog).ClientWidth then
      if ArrangePanel.Width + RightPanel.Width > MaxWidth then
        TForm(ParameterDialog).ClientWidth := MaxWidth
      else
        TForm(ParameterDialog).ClientWidth := ArrangePanel.Width;
  if AutoHeight then
    if ArrangePanel.Height + BottomPanel.Height > TForm(ParameterDialog).ClientHeight then
      if ArrangePanel.Height + BottomPanel.Height > MaxHeight then
        TForm(ParameterDialog).ClientHeight := MaxHeight + 5
      else
        TForm(ParameterDialog).ClientHeight := ArrangePanel.Height + BottomPanel.Height + 5;

  if (ButtonPanel.Width + HistoryPanel.Width) > BottomPanel.Width then
  begin
    ButtonPanel.Align  := alBottom;
    ButtonPanel.Height := BottomPanel.Height;
    BottomPanel.Height := BottomPanel.Height * 2 + 1;
    HistoryPanel.Align := alClient;
  end;
  CheckScrollBoxAutoScroll;
end;

function TJvParameterList.ShowParameterDialog: boolean;
begin
  if Count = 0 then
    EJVCLException.Create(SNoParametersDefined);
  CreateParameterDialog;
  try
    SetDataToWinControls;
    ParameterDialog.ShowModal;
    Result := ParameterDialog.ModalResult = mrOk;
    if Result then
      GetDataFromWinControls;
  finally
    FreeAndNil(FParameterDialog);
  end;
end;

function TJvParameterList.GetParentByName(MainParent: TWinControl; SearchName: string): TWinControl;
var
  Parameter: TJvBaseParameter;
  I, J:      integer;
begin
  Result := MainParent;
  if (SearchName = '') or not Assigned(MainParent) then
    Exit;
  for I := 0 to Count - 1 do
    if Parameters[I].Visible then
 //      if Parameters[I] is TJvTabControlParameter then
 //        for J := 0 to TJvTabControlParameter(Parameters[I]).Tabs.Count - 1 do
//          if Uppercase(Parameters[I].SearchName + '.' + TJvTabControlParameter(Parameters[I]).Tabs[J]) = Uppercase(SearchName) then
//          begin
//            Result := TWinControl(TJvTabControlParameter(Parameters[I]).TabWinControls.Objects[J]);
//            break;
//          end   {*** IF Uppercase(TJvBaseParameter(Objects[I]).SearchName) = Uppercase(ASearchName) THEN ***}
//          else
    else if UpperCase(Parameters[I].SearchName) = UpperCase(SearchName) then
    begin
      Parameter := Parameters[I];
      if Parameter is TJvArrangeParameter then
      begin
        Result := TWinControl(Parameter.WinControl);
        Break;
      end;
    end;
end;

procedure TJvParameterList.HistoryLoadClick(Sender: TObject);
begin
  ParameterListSelectList.RestoreParameterList(Messages.HistoryLoadCaption);
end;

procedure TJvParameterList.HistorySaveClick(Sender: TObject);
begin
  ParameterListSelectList.SaveParameterList(Messages.HistorySaveCaption);
end;

procedure TJvParameterList.HistoryClearClick(Sender: TObject);
begin
  ParameterListSelectList.ManageSelectList(Messages.HistoryClearCaption);
end;

procedure TJvParameterList.CreateWinControlsOnParent(ParameterParent: TWinControl);
var
  I: integer;
begin
  FreeAndNil(ScrollBox);
  ScrollBox := TScrollBox.Create(Self);
  ScrollBox.Parent := ParameterParent;
  with ScrollBox do
  begin
    AutoScroll := false;
    BorderStyle := bsNone;
    Align := alClient;
  end;
  RightPanel := TJvPanel.Create(Self);
  RightPanel.Parent := ScrollBox;
  with RightPanel do
  begin
    //Align := alNone;
    Transparent := True;
    Align   := alRight;
    BorderStyle := bsNone;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Left    := 0;
    Top     := 0;
    Width   := 20;
    Visible := false;
  end;
  FreeAndNil(ArrangePanel);
  ArrangePanel      := TJvPanel.Create(Self);
  ArrangePanel.Parent := ScrollBox;
  ArrangePanel.Name := 'MainArrangePanel';
  with ArrangePanel do
  begin
    //Align := alNone;
    Transparent := True;
    Align := alTop;
    BorderStyle := bsNone;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Caption := '';
    Left := 0;
    Top  := 0;
  end;
  ArrangePanel.ArrangeSettings := ArrangeSettings;
  try
    ArrangePanel.DisableArrange;
    for I := 0 to Count - 1 do
      if Parameters[I].Visible then
      begin
        Parameters[I].CreateWinControlOnParent(
          GetParentByName(ArrangePanel, Parameters[I].ParentParameterName));
        Parameters[I].WinControlData := Parameters[I].AsVariant;
      end;
    for I := 0 to Count - 1 do
      if Parameters[I].Visible then
        if Assigned(Parameters[I].WinControl) then
          if Assigned(THackWinControl(Parameters[I].WinControl).OnExit) then
            THackWinControl(Parameters[I].WinControl).OnExit(Parameters[I].WinControl);
  finally
    ArrangePanel.EnableArrange;
  end;
  ArrangePanel.ArrangeControls;
  CheckScrollBoxAutoScroll;
end;

procedure TJvParameterList.CheckScrollBoxAutoScroll;
begin
  if not Assigned(ScrollBox) then
    Exit;
  if not Assigned(ArrangePanel) then
    Exit;
  RightPanel.Visible   := false;
  ScrollBox.AutoScroll := false;
  if (ArrangePanel.Width > ScrollBox.Width) {OR
     (ArrangePanel.Width > MaxWidth) }then
  begin
    RightPanel.Visible   := true;
    ScrollBox.AutoScroll := true;
  end;
  if (ArrangePanel.Height > ScrollBox.Height) {OR
     (ArrangePanel.Height > MaxHeight) }then
    ScrollBox.AutoScroll := true;
end;

procedure TJvParameterList.DestroyWinControls;
var
  I: integer;
begin
  FreeAndNil(ArrangePanel);
  FreeAndNil(ScrollBox);
end;

procedure TJvParameterList.GetDataFromWinControls;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    if Parameters[I].Visible then
      Parameters[I].GetData;
end;

procedure TJvParameterList.SetDataToWinControls;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    if Parameters[I].Visible then
      Parameters[I].SetData;
end;

function TJvParameterList.ValidateDataAtWinControls: boolean;
var
  I: integer;
  V: variant;
  B: boolean;
begin
  Result := false;
  for I := 0 to Count - 1 do
    if Parameters[I].Visible then
    begin
      V := Parameters[I].WinControlData;
      B := Parameters[I].Validate(V);
      Parameters[I].WinControlData := V;
      if not B then
      begin
        if Assigned(Parameters[I].WinControl) then
          Parameters[I].WinControl.SetFocus;
        Exit;
      end;
    end;
  Result := true;
end;

function TJvParameterList.GetCount: integer;
begin
  Result := FIntParameterList.Count;
end;

function TJvParameterList.AddObject(const S: string; AObject: TObject): integer;
begin
  if not (AObject is TJvBaseParameter) then
    raise EJVCLException.Create(SAddObjectWrongObjectType);
  if TJvBaseParameter(AOBject).SearchName = '' then
    raise EJVCLException.Create(SAddObjectSearchNameNotDefined);
  TJvBaseParameter(AObject).ParameterList := Self;
  Result := FIntParameterList.AddObject(S, AObject);
end;

procedure TJvParameterList.InsertObject(Index: integer; const S: string; AObject: TObject);
begin
  if not (AObject is TJvBaseParameter) then
    raise EJVCLException.Create(SInsertObjectWrongObjectType);
  if TJvBaseParameter(AOBject).SearchName = '' then
    raise EJVCLException.Create(SInsertObjectSearchNameNotDefined);
  TJvBaseParameter(AObject).ParameterList := Self;
  FIntParameterList.InsertObject(Index, S, AObject);
end;

procedure TJvParameterList.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
  if Assigned(ArrangePanel) then
    ArrangePanel.ArrangeSettings := ArrangeSettings;
end;

procedure TJvParameterList.SetParameters(Index: integer; Value: TJvBaseParameter);
begin
  if (Index >= 0) and (Index < FIntParameterList.Count) then
    FIntParameterList.Objects[Index] := Value;
end;

function TJvParameterList.GetParameters(Index: integer): TJvBaseParameter;
begin
  if (Index >= 0) and (Index < FIntParameterList.Count) then
    Result := TJvBaseParameter(FIntParameterList.Objects[Index])
  else
    Result := nil;
end;

function TJvParameterList.GetCurrentWidth: integer;
begin
  if Width > 0 then
    Result := Width
  else if Assigned(ArrangePanel) then
    if ArrangePanel.Align in [alTop, alBottom, alClient] then
      Result := ArrangePanel.ArrangeWidth
    else
      Result := ArrangePanel.Width
  else
    Result := 0;
  if Result > MaxWidth then
    Result := MaxWidth;
end;

function TJvParameterList.GetCurrentHeight: integer;
begin
  if Height > 0 then
    Result := Height
  else if Assigned(ArrangePanel) then
  begin
    if ArrangePanel.Align in [alleft, alRight, alClient] then
      Result := ArrangePanel.ArrangeHeight
    else
      Result := ArrangePanel.Height;
  end
  else
    Result := 0;
  if Result > MaxHeight then
    Result := MaxHeight;
end;

procedure TJvParameterList.Clear;
begin
  FIntParameterList.Clear;
end;

//=== TJvParameterListPropertyStore ==========================================

procedure TJvParameterListPropertyStore.LoadData;
var
  I: integer;
begin
  with ParameterList do
    for I := 0 to ParameterList.Count - 1 do
      if not (Parameters[I] is tJvNoDataParameter) then
        with Parameters[I] do
          if ReloadValueFromRegistry then
            if Parameters[I] is TJvListParameter then
              with TJvListParameter(Parameters[I]) do
                ItemIndex := AppStore.ReadInteger(AppStore.ConcatPaths([Path, SearchName]), ItemIndex)
            else
              AsString := AppStore.ReadString(AppStore.ConcatPaths([Path, SearchName]), AsString);
end;

procedure TJvParameterListPropertyStore.StoreData;
var
  I: integer;
begin
  with ParameterList do
    for I := 0 to ParameterList.Count - 1 do
      if not (Parameters[I] is tJvNoDataParameter) then
        with Parameters[I] do
          if ReloadValueFromRegistry then
            if Parameters[I] is TJvListParameter then
              with TJvListParameter(Parameters[I]) do
                AppStore.WriteInteger(AppStore.ConcatPaths([Path, SearchName]), ItemIndex)
            else
              AppStore.WriteString(AppStore.ConcatPaths([Path, SearchName]), AsString);
end;

//=== TJvParameterListPropertyStore ==========================================

function TJvParameterListSelectList.GetDynControlEngine: TJvDynControlEngine;
begin
  Result := FParameterList.DynControlEngine;
end;

function TJvParameterListSelectList.GetParameterList: TJvParameterList;
begin
  Result := FParameterList;
end;

procedure TJvParameterListSelectList.SetParameterList(Value: TJvParameterList);
begin
  FParameterList := Value;
end;

function TJvParameterListSelectList.GetAppStore: TJvCustomAppStore;
begin
  if Assigned(FParameterList) then
    Result := FParameterList.AppStore;
end;

procedure TJvParameterListSelectList.SetAppStore(Value: TJvCustomAppStore);
begin
  if Assigned(FParameterList) then
    FParameterList.AppStore := Value;
end;

procedure TJvParameterListSelectList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FParameterList) then
    FParameterList := nil;
end;

procedure TJvParameterListSelectList.RestoreParameterList(ACaption: string = '');
var
  SavePath: string;
begin
  if not Assigned(ParameterList) then
    Exit;
  SavePath := ParameterList.Path;
  try
    ParameterList.Path := GetSelectPath(sloLoad, ACaption);
    if ParameterList.Path <> '' then
    begin
      ParameterList.LoadData;
      ParameterList.SetDataToWinControls;
    end;
  finally
    ParameterList.Path := SavePath;
  end;
end;

procedure TJvParameterListSelectList.SaveParameterList(ACaption: string = '');
var
  SavePath: string;
begin
  if not Assigned(ParameterList) then
    Exit;
  SavePath := ParameterList.Path;
  try
    ParameterList.Path := GetSelectPath(sloStore, ACaption);
    if ParameterList.Path <> '' then
    begin
      ParameterList.GetDataFromWinControls;
      ParameterList.StoreData;
    end;
  finally
    ParameterList.Path := SavePath;
  end;
end;

end.
