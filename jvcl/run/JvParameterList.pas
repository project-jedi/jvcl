{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvParameterList;

interface

uses
  Classes, SysUtils,

  {$IFDEF VCL}
  StdCtrls, ExtCtrls, Graphics, Forms, Controls, Dialogs, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QStdCtrls, QExtCtrls, Types, QGraphics, QForms, QControls, QDialogs, QComCtrls,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvTypes, JvDynControlEngine, JvDynControlEngineIntf, JvDsaDialogs,
  JvComponent, JvPanel, JvPropertyStore, JvAppStorage, JvAppStorageSelectList;

type
  TJvParameterList = class;
  TJvParameterListPropertyStore = class;
  TJvParameterPropertyValues = class;
  TJvParameterListSelectList = class;
  TJvBaseParameter = class;

  TJvParameterListEvent = procedure(const ParameterList: TJvParameterList; const Parameter: TJvBaseParameter) of object;

  TJvParameterListEnableDisableReason = class(TPersistent)
  private
    FRemoteParameterName: string;
    FValue: Variant;
    FIsEmpty: Boolean;
    FIsNotEmpty: Boolean;
  protected
    procedure SetAsString(Value: string);
    function GetAsString: string;
    procedure SetAsDouble(Value: Double);
    function GetAsDouble: Double;
    procedure SetAsInteger(Value: Integer);
    function GetAsInteger: Integer;
    procedure SetAsBoolean(Value: Boolean);
    function GetAsBoolean: Boolean;
    procedure SetAsDate(Value: TDateTime);
    function GetAsDate: TDateTime;
    procedure SetAsVariant(Value: Variant);
    function GetAsVariant: Variant;
    procedure SetIsEmpty(Value: Boolean);
    procedure SetIsNotEmpty(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    property AsString: string read GetAsString write SetAsString;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property IsEmpty: Boolean read FIsEmpty write SetIsEmpty;
    property IsNotEmpty: Boolean read FIsNotEmpty write SetIsNotEmpty;
    property RemoteParameterName: string read FRemoteParameterName write FRemoteParameterName;
  end;

  TJvParameterListEnableDisableReasonList = class(TStringList)
  public
    procedure Clear; override;
    procedure AddReasonVariant(RemoteParameterName: string; Value: Variant);
    procedure AddReason(RemoteParameterName: string; Value: Boolean); overload;
    procedure AddReason(RemoteParameterName: string; Value: Integer); overload;
    procedure AddReason(RemoteParameterName: string; Value: Double); overload;
    procedure AddReason(RemoteParameterName: string; Value: string); overload;
    procedure AddReason(RemoteParameterName: string; Value: TDateTime); overload;
    procedure AddReasonIsEmpty(RemoteParameterName: string);
    procedure AddReasonIsNotEmpty(RemoteParameterName: string);
  end;

  TJvParameterPropertyValue = class(TPersistent)
  private
    FPropertyName: string;
    FPropertyValue: Variant;
  public
    property PropertyName: string read FPropertyName write FPropertyName;
    property PropertyValue: Variant read FPropertyValue write FPropertyValue;
  end;

  TJvParameterPropertyValues = class(TStringList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddValue(AName: string; AValue: Variant);
  end;

  TJvBaseParameter = class(TJvComponent)
  private
    FCaption: string;
    FValue: Variant;
    FWidth: Integer;
    FHeight: Integer;
    FSearchName: string;
    FRequired: Boolean;
    FReadOnly: Boolean;
    FStoreValueToAppStorage: Boolean;
    FStoreValueCrypted: Boolean;
    FParentParameterName: string;
    FTabOrder: Integer;
    FParameterList: TJvParameterList;
    FWinControl: TWinControl;
    FJvDynControl: IJvDynControl;
    FJvDynControlData: IJvDynControlData;
    FHint: string;
    FColor: TColor;
    FEnabled: Boolean;
    FHelpContext: THelpContext;
    FDisableReasons: TJvParameterListEnableDisableReasonList;
    FEnableReasons: TJvParameterListEnableDisableReasonList;
    FVisible: Boolean;
    FOnEnterParameter: TJvParameterListEvent;
    FOnExitParameter: TJvParameterListEvent;
  protected
    procedure SetAsString(Value: string); virtual;
    function GetAsString: string; virtual;
    procedure SetAsDouble(Value: Double); virtual;
    function GetAsDouble: Double; virtual;
    procedure SetAsInteger(Value: Integer); virtual;
    function GetAsInteger: Integer; virtual;
    procedure SetAsBoolean(Value: Boolean); virtual;
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsDate(Value: TDateTime); virtual;
    function GetAsDate: TDateTime; virtual;
    procedure SetAsVariant(Value: Variant); virtual;
    function GetAsVariant: Variant; virtual;
    function GetParameterNameExt: string; virtual;
    function GetParameterNameBase: string;
    function GetParameterName: string;
    procedure SetWinControl(Value: TWinControl);
    function GetWinControl: TWinControl;
    function GetWinControlData: Variant; virtual;
    procedure SetWinControlData(Value: Variant); virtual;

    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure SetHeight(Value: Integer); virtual;
    procedure SetWidth(Value: Integer); virtual;
    procedure SetTabOrder(Value: Integer); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetDynControlEngine: TJvDynControlEngine;
    property Color: TColor read FColor write FColor;
    property JvDynControl: IJvDynControl read FJvDynControl;
    property JvDynControlData: IJvDynControlData read FJvDynControlData;
    property Value: Variant read FValue write FValue;
    property WinControl: TWinControl read GetWinControl write SetWinControl;
    procedure SetWinControlProperties; virtual;
  public
    constructor Create(AParameterList: TJvParameterList); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Validate(var AData: Variant): Boolean; virtual;
    procedure CreateWinControlOnParent(ParameterParent: TWinControl); virtual; abstract;
    property WinControlData: Variant read GetWinControlData write SetWinControlData;
    procedure GetData; virtual;
    procedure SetData; virtual;
    property ParameterList: TJvParameterList read FParameterList write FParameterList;
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine;
  published
    {the next properties implements the possibilities to read and write the data }
    property AsString: string read GetAsString write SetAsString;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    {this name is used to identify the parameter in the parameterlist,
     this value must be defined before inserting into the parameterlist }
    property SearchName: string read FSearchName write FSearchName;
    {should this value be saved by the parameterlist }
    property StoreValueToAppStorage: Boolean read FStoreValueToAppStorage write FStoreValueToAppStorage;
    {should this value be crypted before save }
    property StoreValueCrypted: Boolean read FStoreValueCrypted write FStoreValueCrypted;
    {the searchname of the parentparameter. The parameter must be a
     descent of TJvPanelParameter or TTabControlParamter. If the
     parent parameter is a TJvTabControlParameter, then the ParentParameterName must be
     "searchname.tabname" of the TJvTabControlParameter}
    property ParentParameterName: string read FParentParameterName write FParentParameterName;
    {Is the value required, will be checked in the validate function}
    property Required: Boolean read FRequired write FRequired;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
    {the next properties find their expressions in the same properties of TWinControl }
    property Caption: string read FCaption write FCaption;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Hint: string read FHint write FHint;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property TabOrder: Integer read FTabOrder write SetTabOrder;
    property DisableReasons: TJvParameterListEnableDisableReasonList read FDisableReasons;
    property EnableReasons: TJvParameterListEnableDisableReasonList read FEnableReasons;
    property OnEnterParameter: TJvParameterListEvent read FOnEnterParameter write FOnEnterParameter;
    property OnExitParameter: TJvParameterListEvent read FOnExitParameter write FOnExitParameter;
  end;

  TJvParameterListMessages = class(TPersistent)
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

  TJvParameterList = class(TJvComponent)
  private
    FMessages: TJvParameterListMessages;
    FIntParameterList: TStringList;
    FArrangeSettings: TJvArrangeSettings;
    FDynControlEngine: TJvDynControlEngine;
    FParameterDialog: TCustomForm;
    FWidth: Integer;
    FHeight: Integer;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FOkButtonVisible: Boolean;
    FCancelButtonVisible: Boolean;
    FParameterListPropertyStore: TJvParameterListPropertyStore;
    FHistoryEnabled: Boolean;
    FLastHistoryName: string;
    FParameterListSelectList: TJvParameterListSelectList;
    FOkButtonDisableReasons: TJvParameterListEnableDisableReasonList;
    FOkButtonEnableReasons: TJvParameterListEnableDisableReasonList;
    function AddObject(const S: string; AObject: TObject): Integer;
    procedure OnOkButtonClick(Sender: TObject);
    procedure OnCancelButtonClick(Sender: TObject);
  protected
    OkButton: TButton;
    ArrangePanel: TJvPanel;
    ScrollBox: TScrollBox;
    RightPanel: TJvPanel;
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
    procedure SetAppStoragePath(Value: string);
    function GetAppStoragePath: string;
    function GetJvAppStorage: TJvCustomAppStorage;
    procedure SetJvAppStorage(Value: TJvCustomAppStorage);

    procedure SetDynControlEngine(Value: TJvDynControlEngine);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetParentByName(MainParent: TWinControl; SearchName: string): TWinControl;
    function GetCount: Integer;

    procedure SetParameters(Index: Integer; Value: TJvBaseParameter);
    function GetParameters(Index: Integer): TJvBaseParameter;

    function GetCurrentWidth: Integer;
    function GetCurrentHeight: Integer;

    procedure HistoryLoadClick(Sender: TObject);
    procedure HistorySaveClick(Sender: TObject);
    procedure HistoryClearClick(Sender: TObject);

    function GetEnableDisableReasonState(ADisableReasons, AEnableReasons: TJvParameterListEnableDisableReasonList):
      Integer;

    procedure DialogShow(Sender: TObject);

    property IntParameterList: TStringList read FIntParameterList;

    property ParameterDialog: TCustomForm read FParameterDialog;
    property ParameterListSelectList: TJvParameterListSelectList read FParameterListSelectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Saves the data of all allowed parameters to the AppStorage }
    procedure StoreData;
    { load the data of all allowed parameters from the AppStorage }
    procedure LoadData;
    {Adds a new Parameter to the parameterlist }
    procedure AddParameter(AParameter: TJvBaseParameter);
    {returns the parameter identified by the Searchname}
    function ParameterByName(ASearchName: string): TJvBaseParameter;
    {returns True id the parameter identified by the Searchname exists}
    function ExistsParameter(ASearchName: string): Boolean;
    {returns the parameter identified by index-position}
    function ParamByIndex(AIndex: Integer): TJvBaseParameter;
    {executes a dialog to enter all Parameter-Data,
     returns True when ok-button pressed}
    function ShowParameterDialog: Boolean;
    { Creates the ParameterDialog }
    procedure CreateParameterDialog;
    { Checks the Disable/Enable-Reason of all Parameters }
    procedure HandleEnableDisable;
    {creates the components of all parameters on any WinControl}
    procedure CreateWinControlsOnParent(ParameterParent: TWinControl);
    {Destroy the WinControls of all parameters}
    procedure DestroyWinControls;
    { reads the data of all parameters from the WinControls}
    procedure GetDataFromWinControls;
    procedure SetDataToWinControls;
    { validates the data of all parameters without filling the data into
     the parameters }
    function ValidateDataAtWinControls: Boolean;
    {deletes alll Parameters from the Parameterlist}
    procedure Clear;
    {this procedure checks the autoscroll-property of the internal
     scrollbox. This function should only be called, after the size of
     the parent-panel has changed}
    procedure CheckScrollBoxAutoScroll;
    { count of parameters }
    property Count: Integer read GetCount;
    {returns the current height of the created main-parameter-panel}
    property CurrentWidth: Integer read GetCurrentWidth;
    {returns the current height of the created main-parameter-panel}
    property CurrentHeight: Integer read GetCurrentHeight;
    property DynControlEngine: TJvDynControlEngine read FDynControlEngine write SetDynControlEngine;
    { Property to get access to the parameters }
    property Parameters[Index: Integer]: TJvBaseParameter read GetParameters write SetParameters;
    // Enable/DisableReason for the OkButton
    property OkButtonDisableReasons: TJvParameterListEnableDisableReasonList read FOkButtonDisableReasons write
      FOkButtonDisableReasons;
    property OkButtonEnableReasons: TJvParameterListEnableDisableReasonList read FOkButtonEnableReasons write
      FOkButtonEnableReasons;
    procedure OnEnterParameterControl(Sender: TObject);
    procedure OnExitParameterControl(Sender: TObject);
    procedure OnChangeParameterControl(Sender: TObject);
    procedure OnClickParameterControl(Sender: TObject);
  published
    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write SetArrangeSettings;
    property Messages: TJvParameterListMessages read FMessages;
    {AppStoragePath for the Parameter-Storage using AppStorage}
    property AppStoragePath: string read GetAppStoragePath write SetAppStoragePath;
    {Width of the dialog. When width = 0, then the width will be calculated }
    property Width: Integer read FWidth write FWidth;
    {Height of the dialog. When height = 0, then the Height will be calculated }
    property Height: Integer read FHeight write FHeight;
    {Maximum ClientWidth of the Dialog}
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 400;
    {Maximum ClientHeight of the Dialog}
    property MaxHeight: Integer read FMaxHeight write FMaxHeight default 600;
    property OkButtonVisible: Boolean read FOkButtonVisible write FOkButtonVisible;
    property CancelButtonVisible: Boolean read FCancelButtonVisible write FCancelButtonVisible;
    property HistoryEnabled: Boolean read FHistoryEnabled write FHistoryEnabled;
    property LastHistoryName: string read FLastHistoryName write FLastHistoryName;
    property AppStorage: TJvCustomAppStorage read GetJvAppStorage write SetJvAppStorage;
  end;

  TJvParameterListSelectList = class(TJvAppStorageSelectList)
  private
    FParameterList: TJvParameterList;
  protected
    function GetDynControlEngine: TJvDynControlEngine; override;
    function GetParameterList: TJvParameterList; virtual;
    procedure SetParameterList(Value: TJvParameterList); virtual;
    function GetAppStorage: TJvCustomAppStorage; override;
    procedure SetAppStorage(Value: TJvCustomAppStorage); override;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RestoreParameterList(ACaption: string = '');
    procedure SaveParameterList(ACaption: string = '');
  published
    property ParameterList: TJvParameterList read GetParameterList write SetParameterList;
  end;

  TJvParameterListPropertyStore = class(TJvCustomPropertyStore)
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
  JvParameterListParameter, JvResources;

const
  cFalse = 'FALSE';
  cTrue = 'TRUE';

//=== TJvParameterListMessages ===============================================

constructor TJvParameterListMessages.Create;
begin
  inherited Create;
  Caption := RsDialogCaption;
  OkButton := RsOkButton;
  CancelButton := RsCancelButton;
  HistoryLoadButton := RsHistoryLoadButton;
  HistorySaveButton := RsHistorySaveButton;
  HistoryClearButton := RsHistoryClearButton;
  HistoryLoadCaption := RsHistoryLoadCaption;
  HistorySaveCaption := RsHistorySaveCaption;
  HistoryClearCaption := RsHistoryClearCaption;
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

procedure TJvParameterListEnableDisableReason.SetAsDouble(Value: Double);
begin
  AsVariant := Value;
end;

function TJvParameterListEnableDisableReason.GetAsDouble: Double;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetAsInteger(Value: Integer);
begin
  AsVariant := Value;
end;

function TJvParameterListEnableDisableReason.GetAsInteger: Integer;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetAsBoolean(Value: Boolean);
begin
  if Value then
    AsVariant := cTrue
  else
    AsVariant := cFalse;
end;

function TJvParameterListEnableDisableReason.GetAsBoolean: Boolean;
var
  S: string;
begin
  S := FValue;
  Result := S = cTrue;
end;

procedure TJvParameterListEnableDisableReason.SetAsDate(Value: TDateTime);
begin
  AsVariant := VarFromDateTime(Value);
end;

function TJvParameterListEnableDisableReason.GetAsDate: TDateTime;
begin
  Result := VarToDateTime(FValue);
end;

procedure TJvParameterListEnableDisableReason.SetAsVariant(Value: Variant);
begin
  FValue := Value;
end;

function TJvParameterListEnableDisableReason.GetAsVariant: Variant;
begin
  Result := FValue;
end;

procedure TJvParameterListEnableDisableReason.SetIsEmpty(Value: Boolean);
begin
  // IsEmpty and NotIsEmtpy can both be False, in this case the Reason looks
  // for the value to activate/deactivate
  FIsEmpty := Value;
  if Value then
    IsNotEmpty := False;
end;

procedure TJvParameterListEnableDisableReason.SetIsNotEmpty(Value: Boolean);
begin
  // IsEmpty and NotIsEmtpy can both be False, in this case the Reason looks
  // for the value to activate/deactivate
  FIsNotEmpty := Value;
  if Value then
    IsEmpty := False;
end;

procedure TJvParameterListEnableDisableReason.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  AsVariant := TJvParameterListEnableDisableReason(Source).AsVariant;
  IsEmpty := TJvParameterListEnableDisableReason(Source).IsEmpty;
  IsNotEmpty := TJvParameterListEnableDisableReason(Source).IsNotEmpty;
  RemoteParameterName := TJvParameterListEnableDisableReason(Source).RemoteParameterName;
end;

//=== TJvParameterListEnableDisableReasonList ================================

procedure TJvParameterListEnableDisableReasonList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited Clear;
end;

procedure TJvParameterListEnableDisableReasonList.AddReasonVariant(RemoteParameterName: string; Value: Variant);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsVariant := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName: string; Value: Boolean);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsBoolean := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName: string; Value: Integer);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsInteger := Value;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName: string; Value: Double);
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
  Reason.IsEmpty := True;
  AddObject(RemoteParameterName, Reason);
end;

procedure TJvParameterListEnableDisableReasonList.AddReasonIsNotEmpty(RemoteParameterName: string);
var
  Reason: TJvParameterListEnableDisableReason;
begin
  Reason := TJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.IsNotEmpty := True;
  AddObject(RemoteParameterName, Reason);
end;

//=== TJvParameterPropertyValues =============================================

constructor TJvParameterPropertyValues.Create;
begin
  inherited Create;
  Sorted := True;
  Duplicates := dupIgnore;
end;

destructor TJvParameterPropertyValues.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJvParameterPropertyValues.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
end;

procedure TJvParameterPropertyValues.AddValue(AName: string; AValue: Variant);
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
  FStoreValueToAppStorage := True;
  FStoreValueCrypted := False;
  FTabOrder := -1;
  FParameterList := AParameterList;
  FWinControl := nil;
  FJvDynControl := nil;
  FJvDynControlData := nil;
  Color := clBtnFace;
  FEnabled := True;
  FVisible := True;
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

procedure TJvBaseParameter.SetAsDouble(Value: Double);
begin
  AsVariant := Value;
end;

function TJvBaseParameter.GetAsDouble: Double;
begin
  if AsString = '' then
    Result := 0
  else
    Result := AsVariant;
end;

procedure TJvBaseParameter.SetAsInteger(Value: Integer);
begin
  AsVariant := Value;
end;

function TJvBaseParameter.GetAsInteger: Integer;
begin
  if VarIsNull(AsVariant) then
    Result := 0
  else
    Result := AsVariant;
end;

procedure TJvBaseParameter.SetAsBoolean(Value: Boolean);
begin
  if Value then
    AsVariant := cTrue
  else
    AsVariant := cFalse;
end;

function TJvBaseParameter.GetAsBoolean: Boolean;
var
  S: string;
begin
  if VarIsNull(FValue) then
    Result := False
  else
  begin
    S := AsVariant;
    Result := UpperCase(s) = cTrue;
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

procedure TJvBaseParameter.SetAsVariant(Value: Variant);
begin
  FValue := Value;
 //  if Assigned(FJvDynControlData) then
 //    FJvDynControlData.Value := Value;
end;

function TJvBaseParameter.GetAsVariant: Variant;
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
    FWinControl := nil;
    FJvDynControl := nil;
    FJvDynControlData := nil;
  end;
end;

function TJvBaseParameter.GetWinControlData: Variant;
begin
  Result := Null;
  if Assigned(JvDynControlData) then
    Result := JvDynControlData.ControlValue;
end;

procedure TJvBaseParameter.SetWinControlData(Value: Variant);
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
  THackWinControl = class(TWinControl)
  public
    property OnExit;
  end;

procedure TJvBaseParameter.SetWinControl(Value: TWinControl);
begin
  FJvDynControl := nil;
  FWinControl := Value;
  if not Assigned(Value) then
    Exit;
  Supports(FWinControl, IJvDynControl, FJvDynControl);
  Supports(FWinControl, IJvDynControlData, FJvDynControlData);

  SetWinControlProperties;
end;

procedure TJvBaseParameter.SetWinControlProperties;
var
  IDynControlReadOnly: IJvDynControlReadOnly;
begin
  if Assigned(WinControl) then
  begin
    JvDynControl.ControlSetCaption(Caption);
    if Supports(FWinControl, IJvDynControlReadOnly, IDynControlReadOnly) then
    begin
      IDynControlReadOnly.ControlSetReadOnly(ReadOnly);
      SetEnabled(FEnabled);
    end
    else
      SetEnabled(FEnabled and not ReadOnly);
    SetVisible(FVisible);
    if FTabOrder >= 0 then
      SetTabOrder(FTabOrder);
    if FWidth > 0 then
      SetWidth(FWidth);
    if FHeight > 0 then
      SetHeight(FHeight);
    WinControl.Hint := Hint;
    WinControl.HelpContext := HelpContext;
    JvDynControl.ControlSetOnEnter(ParameterList.OnExitParameterControl);
    JvDynControl.ControlSetOnExit(ParameterList.OnExitParameterControl);
    if Assigned(JvDynControlData) then
      JvDynControlData.ControlSetOnChange(ParameterList.OnChangeParameterControl);
  end;
end;

function TJvBaseParameter.GetWinControl: TWinControl;
begin
  Result := FWinControl
end;

procedure TJvBaseParameter.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  if Assigned(WinControl) then
    WinControl.Enabled := Value;
end;

procedure TJvBaseParameter.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  if Assigned(WinControl) then
    WinControl.Visible := Value;
end;

procedure TJvBaseParameter.SetHeight(Value: Integer);
begin
  FHeight := Value;
  if Assigned(WinControl) then
    WinControl.Height := Value;
end;

procedure TJvBaseParameter.SetWidth(Value: Integer);
begin
  FWidth := Value;
  if Assigned(WinControl) then
    WinControl.Width := Value;
end;

procedure TJvBaseParameter.SetTabOrder(Value: Integer);
begin
  FTabOrder := Value;
  if Assigned(WinControl) then
    WinControl.TabOrder := Value;
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
  AsVariant := TJvBaseParameter(Source).AsVariant;
  Caption := TJvBaseParameter(Source).Caption;
  SearchName := TJvBaseParameter(Source).SearchName;
  Width := TJvBaseParameter(Source).Width;
  Height := TJvBaseParameter(Source).Height;
  Required := TJvBaseParameter(Source).Required;
  ParentParameterName := TJvBaseParameter(Source).ParentParameterName;
  StoreValueToAppStorage := TJvBaseParameter(Source).StoreValueToAppStorage;
  StoreValueCrypted := TJvBaseParameter(Source).StoreValueCrypted;
  TabOrder := TJvBaseParameter(Source).TabOrder;
  FParameterList := TJvBaseParameter(Source).ParameterList;
  Color := TJvBaseParameter(Source).Color;
  ReadOnly := TJvBaseParameter(Source).ReadOnly;
  Enabled := TJvBaseParameter(Source).Enabled;
  FEnableReasons.Assign(TJvBaseParameter(Source).FEnableReasons);
  FDisableReasons.Assign(TJvBaseParameter(Source).FDisableReasons);
end;

function TJvBaseParameter.Validate(var AData: Variant): Boolean;
begin
  if not Required or not Enabled then
    Result := True
  else
    Result := not VarIsNull(AData);
  if not Result then
    DSADialogsMessageDlg(Format(RsErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
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
    AutoArrange := True;
    WrapControls := True;
    AutoSize := asBoth;
    DistanceVertical := 3;
    DistanceHorizontal := 3;
    BorderLeft := 5;
    BorderTop := 5;
  end;
  ScrollBox := nil;
  RightPanel := nil;
  ArrangePanel := nil;
  FMaxWidth := 600;
  FMaxHeight := 400;
  fOkbuttonVisible := True;
  FCancelButtonVisible := True;
  FHistoryEnabled := False;
  FLastHistoryName := '';
  FParameterListSelectList := TJvParameterListSelectList.Create(Self);
  FParameterListSelectList.ParameterList := Self;
  FOkButtonDisableReasons := TJvParameterListEnableDisableReasonList.Create;
  FOkButtonEnableReasons := TJvParameterListEnableDisableReasonList.Create;
end;

destructor TJvParameterList.Destroy;
begin
  DestroyWinControls;
  FreeAndNil(FParameterListSelectList);
  FreeAndNil(FIntParameterList);
  FreeAndNil(FParameterListPropertyStore);
  FreeAndNil(FArrangeSettings);
  FreeAndNil(FMessages);
  FreeAndNil(FOkButtonDisableReasons);
  FreeAndNil(FOkButtonEnableReasons);
  inherited Destroy;
end;

procedure TJvParameterList.AddParameter(AParameter: TJvBaseParameter);
begin
  AddObject(AParameter.SearchName, AParameter);
end;

function TJvParameterList.ExistsParameter(ASearchName: string): Boolean;
begin
  Result := Assigned(ParameterByName(ASearchName));
end;

function TJvParameterList.ParameterByName(ASearchName: string): TJvBaseParameter;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if UpperCase(Parameters[I].SearchName) = UpperCase(ASearchName) then
    begin
      Result := Parameters[I];
      Break;
    end;
end;

function TJvParameterList.ParamByIndex(AIndex: Integer): TJvBaseParameter;
begin
  Result := Parameters[AIndex];
end;

procedure TJvParameterList.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Messages.Assign(TJvParameterList(Source).Messages);
  ArrangeSettings := TJvParameterList(Source).ArrangeSettings;
  AppStorage := TJvParameterList(Source).AppStorage;
  Width := TJvParameterList(Source).Width;
  Height := TJvParameterList(Source).Height;
  MaxWidth := TJvParameterList(Source).MaxWidth;
  MaxHeight := TJvParameterList(Source).MaxHeight;
  OkButtonVisible := TJvParameterList(Source).OkButtonVisible;
  CancelButtonVisible := TJvParameterList(Source).CancelButtonVisible;
  FIntParameterList.Assign(TJvParameterList(Source).FIntParameterList);
  HistoryEnabled := TJvParameterList(Source).HistoryEnabled;
  AppStoragePath := TJvParameterList(Source).AppStoragePath;
end;

procedure TJvParameterList.SetAppStoragePath(Value: string);
begin
  FParameterListPropertyStore.AppStoragePath := Value;
  if Assigned(AppStorage) then
    FParameterListSelectList.SelectPath := AppStorage.ConcatPaths([Value, RsHistorySelectPath])
end;

function TJvParameterList.GetAppStoragePath: string;
begin
  Result := FParameterListPropertyStore.AppStoragePath;
end;

function TJvParameterList.GetJvAppStorage: TJvCustomAppStorage;
begin
  Result := FParameterListPropertyStore.AppStorage;
end;

procedure TJvParameterList.SetJvAppStorage(Value: TJvCustomAppStorage);
begin
  FParameterListPropertyStore.AppStorage := Value;
  if Assigned(Value) then
    FParameterListSelectList.SelectPath := Value.ConcatPaths([FParameterListPropertyStore.AppStoragePath, RsHistorySelectPath])
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
    if AComponent = OkButton then
      OkButton := nil;
  end;
end;

procedure TJvParameterList.SetDynControlEngine(Value: TJvDynControlEngine);
begin
  FDynControlEngine := Value;
end;

procedure TJvParameterList.StoreData;
begin
  if AppStoragePath <> '' then
    FParameterListPropertyStore.StoreData;
end;

procedure TJvParameterList.LoadData;
begin
  if AppStoragePath <> '' then
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

procedure TJvParameterList.OnEnterParameterControl(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(Sender) then
    for I := 0 to Count - 1 do
      if Parameters[I].WinControl = Sender then
      begin
        if Assigned(Parameters[I].OnEnterParameter) then
          Parameters[I].OnEnterParameter(Self, Parameters[I]);
        Break;
      end;
end;

procedure TJvParameterList.OnExitParameterControl(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(Sender) then
    for I := 0 to Count - 1 do
      if Parameters[I].WinControl = Sender then
      begin
        if Assigned(Parameters[I].OnExitParameter) then
          Parameters[I].OnExitParameter(Self, Parameters[I]);
        Break;
      end;
  HandleEnableDisable;
end;

procedure TJvParameterList.OnChangeParameterControl(Sender: TObject);
begin
  HandleEnableDisable;
end;

procedure TJvParameterList.OnClickParameterControl(Sender: TObject);
begin
end;

type
  THackPanel = class(TCustomControl)
  public
    property Canvas;
  end;

procedure TJvParameterList.CreateParameterDialog;
var
  MainPanel, BottomPanel, HistoryPanel, ButtonPanel: TWinControl;
  CancelButton: TWinControl;
  LoadButton, SaveButton, ClearButton: TWinControl;
  ButtonLeft: Integer;
  ITmpPanel: IJvDynControlPanel;
begin
  FreeAndNil(FParameterDialog);

  FParameterDialog := DynControlEngine.CreateForm(Messages.Caption, '');

  with TForm(ParameterDialog) do
  begin
    BorderIcons := [];
    {$IFDEF VCL}
    DefaultMonitor := dmActiveForm;
    BorderStyle := bsDialog;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    BorderStyle := fbsDialog;
    {$ENDIF VisualCLX}
    FormStyle := fsNormal;
    Position := poScreenCenter;
    OnShow := DialogShow;
  end;

  if Height > 0 then
    ParameterDialog.Height := Height;
  if Width > 0 then
    ParameterDialog.Width := Width;

  BottomPanel := DynControlEngine.CreatePanelControl(Self, ParameterDialog, 'BottomPanel', '', alBottom);
  if not Supports(BottomPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvRaised, 1, bsNone, 0);

  MainPanel := DynControlEngine.CreatePanelControl(Self, ParameterDialog, 'MainPanel', '', alClient);
  if not Supports(MainPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvRaised, 1, bsNone, 3);

  ButtonPanel := DynControlEngine.CreatePanelControl(Self, BottomPanel, 'BottonPanel', '', alRight);
  if not Supports(ButtonPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 0);

  OkButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'OkButton', Messages.OkButton, '',
    OnOkButtonClick, True, False);
  CancelButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'CancelButton', Messages.CancelButton, '',
    OnCancelButtonClick, False, True);

  BottomPanel.Height := OkButton.Height + 6 + 2;

  OkButton.Top := 3;
  OkButton.Left := 3;
  OkButton.Visible := OkButtonVisible;
  OkButton.Enabled := OkButtonVisible;
  if OkButton.Visible then
    ButtonLeft := OkButton.Left + OkButton.Width + 3
  else
    ButtonLeft := 0;

  CancelButton.Top := 3;
  CancelButton.Left := ButtonLeft + 3;
  CancelButton.Visible := CancelButtonVisible;
  CancelButton.Enabled := CancelButtonVisible;
  if CancelButton.Visible then
    ButtonLeft := ButtonLeft + 3 + CancelButton.Width + 3;

  ButtonPanel.Width := ButtonLeft + 3;

  OkButton.Anchors := [akTop, akRight];
  CancelButton.Anchors := [akTop, akRight];

  if HistoryEnabled and (AppStoragePath <> '') then
  begin
    HistoryPanel := DynControlEngine.CreatePanelControl(Self, BottomPanel, 'HistoryPanel', '', alLeft);
    if not Supports(HistoryPanel, IJvDynControlPanel, ITmpPanel) then
      raise EIntfCastError.CreateRes(@RsEIntfCastError);
    with ITmpPanel do
      ControlSetBorder(bvNone, bvNone, 0, bsNone, 0);
    with HistoryPanel do
      Height := 25;
    LoadButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'LoadButton', Messages.HistoryLoadButton, '',
      HistoryLoadClick, False, False);
    with LoadButton do
    begin
      Left := 6;
      Top := 5;
      Height := 20;
      Width := THackPanel(HistoryPanel).Canvas.TextWidth(Messages.HistoryLoadButton) + 5;
      ButtonLeft := Left + Width + 5;
    end;
    SaveButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'SaveButton', Messages.HistorySaveButton, '',
      HistorySaveClick, False, False);
    with SaveButton do
    begin
      Left := ButtonLeft;
      Top := 5;
      Height := 20;
      Width := THackPanel(HistoryPanel).Canvas.TextWidth(Messages.HistorySaveButton) + 5;
      ButtonLeft := Left + Width + 5;
    end;
    ClearButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'ClearButton', Messages.HistoryClearButton, '',
      HistoryClearClick, False, False);
    with ClearButton do
    begin
      Left := ButtonLeft;
      Top := 5;
      Height := 20;
      Width := THackPanel(HistoryPanel).Canvas.TextWidth(Messages.HistoryClearButton) + 5;
      ButtonLeft := Left + Width + 5;
    end;
    HistoryPanel.Width := ButtonLeft;
  end
  else
    HistoryPanel := nil;

  CreateWinControlsOnParent(MainPanel);

  if Width <= 0 then
    if ArrangeSettings.AutoSize in [asWidth, asBoth] then
      if ArrangePanel.Width > TForm(ParameterDialog).ClientWidth then
        if ArrangePanel.Width + RightPanel.Width > MaxWidth then
          TForm(ParameterDialog).ClientWidth := MaxWidth
        else
          TForm(ParameterDialog).ClientWidth := ArrangePanel.Width;
  if Height <= 0 then
    if ArrangeSettings.AutoSize in [asHeight, asBoth] then
      if ArrangePanel.Height + BottomPanel.Height > TForm(ParameterDialog).ClientHeight then
        if ArrangePanel.Height + BottomPanel.Height > MaxHeight then
          TForm(ParameterDialog).ClientHeight := MaxHeight + 5
        else
          TForm(ParameterDialog).ClientHeight := ArrangePanel.Height + BottomPanel.Height + 5;

  if Assigned(HistoryPanel) then
    if (ButtonPanel.Width + HistoryPanel.Width) > BottomPanel.Width then
    begin
      ButtonPanel.Align := alBottom;
      ButtonPanel.Height := BottomPanel.Height;
      BottomPanel.Height := BottomPanel.Height * 2 + 1;
      HistoryPanel.Align := alClient;
    end;
  CheckScrollBoxAutoScroll;
end;

function TJvParameterList.ShowParameterDialog: Boolean;
begin
  if Count = 0 then
    EJVCLException.CreateRes(@RsENoParametersDefined);
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
  I: Integer;
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
    else
    if UpperCase(Parameters[I].SearchName) = UpperCase(SearchName) then
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

procedure TJvParameterList.DialogShow(Sender: TObject);
begin
  if Count > 0 then
    if Parameters[0].Visible then
      if Assigned(Parameters[0].WinControl) then
        Parameters[0].WinControl.SetFocus;
end;

function TJvParameterList.GetEnableDisableReasonState(ADisableReasons, AEnableReasons:
  TJvParameterListEnableDisableReasonList): Integer;
var
  J: Integer;
  IEnable: Integer;
  Reason: TJvParameterListEnableDisableReason;
  SearchParameter: TJvBaseParameter;
  Data: Variant;
begin
  IEnable := 0;
  if AEnableReasons.Count > 0 then
  begin
    for J := 0 to AEnableReasons.Count - 1 do
    begin
      Reason := TJvParameterListEnableDisableReason(AEnableReasons.Objects[J]);
      if not Assigned(Reason) then
        Continue;
      if VarIsNull(Reason.AsVariant) then
        Continue;
      SearchParameter := ParameterByName(Reason.RemoteParameterName);
      if not Assigned(SearchParameter) then
        Continue;
      if not Assigned(SearchParameter.WinControl) then
        Continue;
      Data := SearchParameter.GetWinControlData;
      if VarIsEmpty(Data) and Reason.IsEmpty and (IEnable <> -1) then
        IEnable := 1;
      if (not VarIsEmpty(Data)) and Reason.IsNotEmpty and (IEnable <> -1) then
        IEnable := 1;
      try
        if (Reason.AsVariant = Data) and (IEnable <> -1) then
          IEnable := 1;
      except
      end;
    end;
    if IEnable = 0 then
      IEnable := -1;
  end;
  if ADisableReasons.Count > 0 then
  begin
    for J := 0 to ADisableReasons.Count - 1 do
    begin
      Reason := TJvParameterListEnableDisableReason(ADisableReasons.Objects[J]);
      if not Assigned(Reason) then
        Continue;
      if VarIsNull(Reason.AsVariant) then
        Continue;
      SearchParameter := ParameterByName(Reason.RemoteParameterName);
      if not Assigned(SearchParameter) then
        Continue;
      if not Assigned(SearchParameter.WinControl) then
        Continue;
      Data := SearchParameter.GetWinControlData;
      if (VarIsEmpty(Data) or (VarToStr(Data) = '')) and Reason.IsEmpty then
        IEnable := -1;
      if (not (VarIsEmpty(Data) or (VarToStr(Data) = ''))) and Reason.IsNotEmpty then
        IEnable := -1;
      try
        if Reason.AsVariant = Data then
          IEnable := -1;
      except
      end;
    end;
    if IEnable = 0 then
      IEnable := 1;
  end;
  Result := IEnable;
end;

procedure TJvParameterList.HandleEnableDisable;
var
  I: Integer;
  Parameter: TJvBaseParameter;
  IEnable: Integer;
begin
  for I := 0 to Count - 1 do
    if Assigned(ParamByIndex(I).WinControl) then
    begin
      Parameter := ParamByIndex(I);
      IEnable := GetEnableDisableReasonState(Parameter.DisableReasons, Parameter.EnableReasons);
      case IEnable of
        -1:
          Parameter.Enabled := False;
        1:
          Parameter.Enabled := True;
      end;
    end;
  if Assigned(OkButton) then
  begin
    IEnable := GetEnableDisableReasonState(OkButtonDisableReasons, OkButtonEnableReasons);
    case IEnable of
      -1:
        OkButton.Enabled := False;
      1:
        OkButton.Enabled := True;
    end;
  end;
end;

procedure TJvParameterList.CreateWinControlsOnParent(ParameterParent: TWinControl);
var
  I: Integer;
begin
  FreeAndNil(ScrollBox);
  ScrollBox := TScrollBox.Create(Self);
  ScrollBox.Parent := ParameterParent;
  with ScrollBox do
  begin
    AutoScroll := False;
    BorderStyle := bsNone;
    Align := alClient;
  end;
  RightPanel := TJvPanel.Create(Self);
  RightPanel.Parent := ScrollBox;
  with RightPanel do
  begin
    Transparent := True;
    Align := alRight;
    BorderStyle := bsNone;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Left := 0;
    Top := 0;
    Width := 20;
    Visible := False;
  end;
  FreeAndNil(ArrangePanel);
  ArrangePanel := TJvPanel.Create(Self);
  ArrangePanel.Parent := ScrollBox;
  ArrangePanel.Name := 'MainArrangePanel';
  with ArrangePanel do
  begin
    Transparent := True;
    Align := alTop;
    BorderStyle := bsNone;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Caption := '';
    Left := 0;
    Top := 0;
  end;
  ArrangePanel.ArrangeSettings := ArrangeSettings;
  case ArrangeSettings.AutoSize of
    asNone:
      ArrangePanel.ArrangeSettings.AutoSize := asHeight;
    asWidth:
      ArrangePanel.ArrangeSettings.AutoSize := asBoth;
  end;
  try
    ArrangePanel.DisableArrange;
    for I := 0 to Count - 1 do
      if Parameters[I].Visible then
      begin
        Parameters[I].CreateWinControlOnParent(
          GetParentByName(ArrangePanel, Parameters[I].ParentParameterName));
        Parameters[I].WinControlData := Parameters[I].AsVariant;
      end;
    HandleEnableDisable;
 //    for I := 0 to Count - 1 do
 //      if Parameters[I].Visible then
 //        if Assigned(Parameters[I].WinControl) then
 //          if Assigned(THackWinControl(Parameters[I].WinControl).OnExit) then
 //            THackWinControl(Parameters[I].WinControl).OnExit(Parameters[I].WinControl);
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
  RightPanel.Visible := False;
  ScrollBox.AutoScroll := False;
  if ArrangePanel.Width > ScrollBox.Width then
  begin
    RightPanel.Visible := True;
    ScrollBox.AutoScroll := True;
  end;
  if (ArrangePanel.Height > ScrollBox.Height) {OR
     (ArrangePanel.Height > MaxHeight) } then
    ScrollBox.AutoScroll := True;
end;

procedure TJvParameterList.DestroyWinControls;
begin
  FreeAndNil(ArrangePanel);
  FreeAndNil(ScrollBox);
end;

procedure TJvParameterList.GetDataFromWinControls;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Parameters[I].Visible then
      Parameters[I].GetData;
end;

procedure TJvParameterList.SetDataToWinControls;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Parameters[I].Visible then
      Parameters[I].SetData;
end;

function TJvParameterList.ValidateDataAtWinControls: Boolean;
var
  I: Integer;
  V: Variant;
  B: Boolean;
begin
  Result := False;
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
  Result := True;
end;

function TJvParameterList.GetCount: Integer;
begin
  Result := FIntParameterList.Count;
end;

function TJvParameterList.AddObject(const S: string; AObject: TObject): Integer;
begin
  if not (AObject is TJvBaseParameter) then
    raise EJVCLException.CreateRes(@RsEAddObjectWrongObjectType);
  if TJvBaseParameter(AOBject).SearchName = '' then
    raise EJVCLException.CreateRes(@RsEAddObjectSearchNameNotDefined);
  if IntParameterList.IndexOf(S) >= 0 then
    raise Exception.CreateResFmt(@RsEAddObjectDuplicateSearchNamesNotAllowed, [S]);
  TJvBaseParameter(AObject).ParameterList := Self;
  Result := FIntParameterList.AddObject(S, AObject);
end;

procedure TJvParameterList.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
  if Assigned(ArrangePanel) then
    ArrangePanel.ArrangeSettings := ArrangeSettings;
end;

procedure TJvParameterList.SetParameters(Index: Integer; Value: TJvBaseParameter);
begin
  if (Index >= 0) and (Index < FIntParameterList.Count) then
    FIntParameterList.Objects[Index] := Value;
end;

function TJvParameterList.GetParameters(Index: Integer): TJvBaseParameter;
begin
  if (Index >= 0) and (Index < FIntParameterList.Count) then
    Result := TJvBaseParameter(FIntParameterList.Objects[Index])
  else
    Result := nil;
end;

function TJvParameterList.GetCurrentWidth: Integer;
begin
  if Width > 0 then
    Result := Width
  else
  if Assigned(ArrangePanel) then
    if ArrangePanel.Align in [alTop, alBottom, alClient] then
      Result := ArrangePanel.ArrangeWidth
    else
      Result := ArrangePanel.Width
  else
    Result := 0;
  if Result > MaxWidth then
    Result := MaxWidth;
end;

function TJvParameterList.GetCurrentHeight: Integer;
begin
  if Height > 0 then
    Result := Height
  else
  if Assigned(ArrangePanel) then
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
  I: Integer;
begin
  with ParameterList do
    for I := 0 to ParameterList.Count - 1 do
      if not (Parameters[I] is TJvNoDataParameter) then
        with Parameters[I] do
          if StoreValueToAppStorage then
          begin
            if StoreValueCrypted then
              AppStorage.EnablePropertyValueCrypt;
            if Parameters[I] is TJvListParameter then
              with TJvListParameter(Parameters[I]) do
                ItemIndex := AppStorage.ReadInteger(AppStorage.ConcatPaths([AppStoragePath, SearchName]), ItemIndex)
            else
              AsString := AppStorage.ReadString(AppStorage.ConcatPaths([AppStoragePath, SearchName]), AsString);
            if StoreValueCrypted then
              AppStorage.DisablePropertyValueCrypt;
          end;
end;

procedure TJvParameterListPropertyStore.StoreData;
var
  I: Integer;
begin
  with ParameterList do
    for I := 0 to ParameterList.Count - 1 do
      if not (Parameters[I] is TJvNoDataParameter) then
        with Parameters[I] do
          if StoreValueToAppStorage then
          begin
            if StoreValueCrypted then
              AppStorage.EnablePropertyValueCrypt;
            if Parameters[I] is TJvListParameter then
              with TJvListParameter(Parameters[I]) do
                AppStorage.WriteInteger(AppStorage.ConcatPaths([AppStoragePath, SearchName]), ItemIndex)
            else
              AppStorage.WriteString(AppStorage.ConcatPaths([AppStoragePath, SearchName]), AsString);
            if StoreValueCrypted then
              AppStorage.DisablePropertyValueCrypt;
          end;
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

function TJvParameterListSelectList.GetAppStorage: TJvCustomAppStorage;
begin
  if Assigned(FParameterList) then
    Result := FParameterList.AppStorage
  else
    Result := nil;
end;

procedure TJvParameterListSelectList.SetAppStorage(Value: TJvCustomAppStorage);
begin
  if Assigned(FParameterList) then
    FParameterList.AppStorage := Value;
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
  SavePath := ParameterList.AppStoragePath;
  try
    ParameterList.AppStoragePath := GetSelectListPath(sloLoad, ACaption);
    if ParameterList.AppStoragePath <> '' then
    begin
      ParameterList.LoadData;
      ParameterList.SetDataToWinControls;
    end;
  finally
    ParameterList.AppStoragePath := SavePath;
  end;
end;

procedure TJvParameterListSelectList.SaveParameterList(ACaption: string = '');
var
  SavePath: string;
begin
  if not Assigned(ParameterList) then
    Exit;
  SavePath := ParameterList.AppStoragePath;
  try
    ParameterList.AppStoragePath := GetSelectListPath(sloStore, ACaption);
    if ParameterList.AppStoragePath <> '' then
    begin
      ParameterList.GetDataFromWinControls;
      ParameterList.StoreData;
    end;
  finally
    ParameterList.AppStoragePath := SavePath;
  end;
end;

end.

