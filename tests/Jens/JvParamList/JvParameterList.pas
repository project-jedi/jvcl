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


uses Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Forms, Controls,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF}
  Dialogs, ComCtrls,
  JvDynControlEngine, JvDynControlEngine_Interface,
  JvComponent, JvPanel, JvPropertyStore, JvAppStore, JvAppStoreSelectList;

type
  TJvParameterList = class
    ;
  TJvParameterListPropertyStore = class
    ;
  TJvParameterPropertyValues = class
    ;
  TJvParameterListSelectList = class ;

  tJvParameterListEnableDisableReason = class (TPersistent)
  private
    fRemoteParameterName : string;
    fValue : variant;
  protected
    procedure SetAsString(Value : string);
    function GetAsString : string;
    procedure SetAsDouble(Value : double);
    function GetAsDouble : double;
    procedure SetAsInteger(Value : integer);
    function GetAsInteger : integer;
    procedure SetAsBoolean(Value : boolean);
    function GetAsBoolean : boolean;
    procedure SetAsDate(Value : tDateTime);
    function GetAsDate : tDateTime;
    procedure SetAsVariant(Value : variant);
    function GetAsVariant : variant;
  public
    procedure Assign(Source : TPersistent); override;

    property AsString : string Read GetAsString Write SetAsString;
    property AsDouble : double Read GetAsDouble Write SetAsDouble;
    property AsInteger : integer Read GetAsInteger Write SetAsInteger;
    property AsBoolean : boolean Read GetAsBoolean Write SetAsBoolean;
    property AsDate : tDateTime Read GetAsDate Write SetAsDate;
    property AsVariant : variant Read GetAsVariant Write SetAsVariant;

    property RemoteParameterName : string Read fRemoteParameterName Write fRemoteParameterName;
  end;

  tJvParameterListEnableDisableReasonList = class (TStringList)
  public
    procedure Clear; override;
    procedure AddReasonVariant(RemoteParameterName : string; Value : variant);
    procedure AddReason(RemoteParameterName : string; Value : boolean); overload;
    procedure AddReason(RemoteParameterName : string; Value : integer); overload;
    procedure AddReason(RemoteParameterName : string; Value : double); overload;
    procedure AddReason(RemoteParameterName : string; Value : string); overload;
    procedure AddReason(RemoteParameterName : string; Value : tDateTime); overload;
  end;

  tJvParameterPropertyValue = class (TPersistent)
  private
    fPropertyName : string;
    fPropertyValue : variant;
  public
    property PropertyName : string Read fPropertyName Write fPropertyName;
    property PropertyValue : variant Read fPropertyValue Write fPropertyValue;
  end;

  TJvParameterPropertyValues = class (TStringList)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddValue(aName : string; aValue : variant);
  published
  end;

//     TNotifyEvent = procedure (Sender: TObject) ofobject;

  tJvBaseParameter = class (TComponent)
  private
    fCaption : string;
    fValue : variant;
    fWidth : integer;
    fHeight : integer;
    fSearchName : string;
    fRequired : boolean;
    fReadOnly : boolean;
    fReloadValuefromRegistry : boolean;
    fParentParameterName : string;
    fTabOrder : integer;
    fParameterList : TJvParameterList;
    fWinControl : TWinControl;
    fJvDynControl : IJvDynControl;
    fJvDynControlData : IJvDynControlData;
    fHint : string;
    fColor : TColor;
    fEnabled : boolean;
    fHelpContext : tHelpContext;
    fDisableReasons : tJvParameterListEnableDisableReasonList;
    fEnableReasons : tJvParameterListEnableDisableReasonList;
    fVisible : boolean;
  protected
    procedure SetAsString(Value : string); virtual;
    function GetAsString : string; virtual;
    procedure SetAsDouble(Value : double); virtual;
    function GetAsDouble : double; virtual;
    procedure SetAsInteger(Value : integer); virtual;
    function GetAsInteger : integer; virtual;
    procedure SetAsBoolean(Value : boolean); virtual;
    function GetAsBoolean : boolean; virtual;
    procedure SetAsDate(Value : tDateTime); virtual;
    function GetAsDate : tDateTime; virtual;
    procedure SetAsVariant(Value : variant); virtual;
    function GetAsVariant : variant; virtual;
    function GetParameterNameExt : string; virtual;
    function GetParameterNameBase : string;
    function GetParameterName : string;
    procedure SetWinControl(Value : TWinControl);
    function GetWinControl : TWinControl;
    property WinControl : TWinControl Read GetWinControl Write SetWinControl;
    property JvDynControl : IJvDynControl Read fJvDynControl;
    property JvDynControlData : IJvDynControlData Read fJvDynControlData;
    property Color : TColor Read fColor Write fColor;

    function GetWinControlData : variant; virtual;
    procedure SetWinControlData(Value : variant); virtual;

    procedure SetEnabled(Value : boolean); virtual;

    procedure HandleEnableDisable(Sender : TObject);

    property Value : variant Read fValue Write fValue;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    function GetDynControlEngine : tJvDynControlEngine;

  public
    constructor Create(AParameterList : TJvParameterList); virtual;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    function Validate(var fData : variant) : boolean; virtual;
    procedure CreateWinControlOnParent(ParameterParent : TWinControl); virtual; abstract;
    property WinControlData : variant Read GetWinControlData Write SetWinControlData;
    procedure GetData; virtual;
    procedure SetData; virtual;
    property ParameterList : TJvParameterList Read fParameterList Write fParameterList;
    property DynControlEngine : tJvDynControlEngine Read GetDynControlEngine;
  published
       {the next properties implements the possibilities to read and write the data }
    property AsString : string Read GetAsString Write SetAsString;
    property AsDouble : double Read GetAsDouble Write SetAsDouble;
    property AsInteger : integer Read GetAsInteger Write SetAsInteger;
    property AsBoolean : boolean Read GetAsBoolean Write SetAsBoolean;
    property AsDate : tDateTime Read GetAsDate Write SetAsDate;
    property AsVariant : variant Read GetAsVariant Write SetAsVariant;
       {this name is used to identify the parameter in the parameterlist,
        this value must be defined before inserting into the parameterlist }
    property SearchName : string Read fSearchName Write fSearchName;
       {should this value be saved in the registry by the parameterlist }
    property ReloadValuefromRegistry : boolean Read fReloadValuefromRegistry Write fReloadValuefromRegistry;
       {the searchname of the parentparameter. The parameter must be a
        descent of TJvPanelParameter or TTabControlParamter. If the
        parentparameter is a TJvTabControlParameter, than the ParentParameterName must be
        "searchname.tabname" of the TJvTabControlParameter}
    property ParentParameterName : string Read fParentParameterName Write fParentParameterName;
       {Is the value required, will be checked in the validate function}
    property Required : boolean Read fRequired Write fRequired;
    property ReadOnly : boolean Read fReadOnly Write fReadOnly;
    property Enabled : boolean Read fEnabled Write SetEnabled;
    property Visible : boolean Read fVisible Write fVisible;
       {the next properties find their expressions in the same properties of tWinControl }
    property Caption : string Read fCaption Write fCaption;
    property Width : integer Read fWidth Write fWidth;
    property Height : integer Read fHeight Write fHeight;
    property Hint : string Read fHint Write fHint;
    property HelpContext : tHelpContext Read fHelpContext Write fHelpContext;
    property TabOrder : integer Read fTabOrder Write fTabOrder;


    property DisableReasons : tJvParameterListEnableDisableReasonList Read fDisableReasons;
    property EnableReasons : tJvParameterListEnableDisableReasonList Read fEnableReasons;

  end;

  TJvParameterListMessages = class (TPersistent)
  private
    fCaption : string;
    fOkButton : string;
    fCancelButton : string;
    fHistoryLoadButton : string;
    fHistorySaveButton : string;
    fHistoryClearButton : string;
    fHistoryLoadCaption : string;
    fHistorySaveCaption : string;
    fHistoryClearCaption : string;
  public
    constructor Create;
  published
    property Caption : string Read fCaption Write fCaption;
    property OkButton : string Read fOkButton Write fOkButton;
    property CancelButton : string Read fCancelButton Write fCancelButton;
    property HistoryLoadButton : string Read fHistoryLoadButton Write fHistoryLoadButton;
    property HistorySaveButton : string Read fHistorySaveButton Write fHistorySaveButton;
    property HistoryClearButton : string Read fHistoryClearButton Write fHistoryClearButton;
    property HistoryLoadCaption : string Read fHistoryLoadCaption Write fHistoryLoadCaption;
    property HistorySaveCaption : string Read fHistorySaveCaption Write fHistorySaveCaption;
    property HistoryClearCaption : string Read fHistoryClearCaption Write fHistoryClearCaption;
  end;

  TJvParameterList = class (TJvComponent)
    procedure OnOkButtonClick(Sender : TObject);
    procedure OnCancelButtonClick(Sender : TObject);
  private
    fMessages : TJvParameterListMessages;
    fintParameterList : TStringList;
    FArrangeSettings : TJvArrangeSettings;
    fDynControlEngine : tJvDynControlEngine;
    fParameterDialog : tCustomForm;
    fWidth : integer;
    fHeight : integer;
    fAutoWidth : boolean;
    fAutoHeight : boolean;
    fMaxWidth : integer;
    fMaxHeight : integer;
    fOkButtonVisible : boolean;
    fCancelButtonVisible : boolean;
    fParameterListPropertyStore : TJvParameterListPropertyStore;
    fHistoryEnabled : boolean;
    fLastHistoryName : string;
    fParameterListSelectList : TJvParameterListSelectList;
    function AddObject(const S : string; AObject : TObject) : integer;
    procedure InsertObject(Index : integer; const S : string; AObject : TObject);
  protected
    ArrangePanel : TJvPanel;
    ScrollBox : TScrollBox;
    RightPanel : TJvPanel;
    procedure SetArrangeSettings(Value : TJvArrangeSettings);
    procedure SetPath(Value : string);
    function GetPath : string;
    function GetAppStore : TJvCustomAppStore;
    procedure SetAppStore(Value : TJvCustomAppStore);

    procedure SetDynControlEngine(Value : tJvDynControlEngine);

    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    function GetParentByName(MainParent : TWinControl; SearchName : string) : TWinControl;
    function GetCount : integer;


    procedure SetParameters(Index : integer; Value : tJvBaseParameter);
    function GetParameters(Index : integer) : tJvBaseParameter;

    function GetCurrentWidth : integer;
    function GetCurrentHeight : integer;

    procedure HistoryLoadClick(Sender : TObject);
    procedure HistorySaveClick(Sender : TObject);
    procedure HistoryClearClick(Sender : TObject);

    property intParameterList : TStringList Read fintParameterList;

    property ParameterDialog : tCustomForm Read fParameterDialog;
    property ParameterListSelectList : TJvParameterListSelectList Read fParameterListSelectList;


  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
       { Saves the data of all allowed parameters to the registry }
    procedure StoreData;
       { load the data of all allowed parameters from the registry }
    procedure LoadData;
       {Adds a new Parameter to the parameterlist }
    procedure AddParameter(aParameter : tJvBaseParameter);
       {returns the parameter identified by the Searchname}
    function ParameterByName(iSearchName : string) : tJvBaseParameter;
       {returns true id the parameter identified by the Searchname exists}
    function ExistsParameter(iSearchName : string) : boolean;
       {returns the parameter identified by index-position}
    function ParamByIndex(iIndex : integer) : tJvBaseParameter;
       {executes a dialog to enter all Parameter-Data,
        returns true when ok-button pressed}
    function ShowParameterDialog : boolean;

       { Creates the ParameterDialog }
    procedure CreateParameterDialog;

       {  count of parameters}
    property Count : integer Read getcount;

       {creates the components of all parameters on any WinControl}
    procedure CreateWinControlsOnParent(ParameterParent : TWinControl);
       {Destroy the WinControls of all parameters}
    procedure DestroyWinControls;

       {returns the current height of the created main-parameter-panel}
    property CurrentWidth : integer Read GetCurrentWidth;
       {returns the current height of the created main-parameter-panel}
    property CurrentHeight : integer Read GetCurrentHeight;

       { reads the data of all parameters from the WinControls}
    procedure GetDataFromWinControls;
    procedure SetDataToWinControls;
       { validates the data of all parameters without filling the data into
       the parameters }
    function ValidateDataAtWinControls : boolean;
       {deletes alll Parameters from the Parameterlist}
    procedure Clear;

       {this procedure checks the autoscroll-property of the internal
        scrollbox. This funtion should only be called, after the size of
        the parent-panel has changed}
    procedure CheckScrollboxAutoScroll;
       { Property to get access to the parameters }
    property Parameters[Index : integer] : tJvBaseParameter Read GetParameters Write SetParameters;

    property DynControlEngine : tJvDynControlEngine Read fDynControlEngine Write SetDynControlEngine;

  published
    property ArrangeSettings : TJvArrangeSettings Read FArrangeSettings Write SetArrangeSettings;
    property Messages : TJvParameterListMessages Read fMessages;
    property Path : string Read GetPath Write SetPath;
       {Width of the dialog. When width = 0, then the width will be calculated }
    property Width : integer Read fWidth Write fWidth;
       {Height of the dialog. When height = 0, then the Height will be calculated }
    property Height : integer Read fHeight Write fHeight;
       {Property to define that the dialog height should be calculated automaticly }
    property AutoWidth : boolean Read fAutoWidth Write fAutoWidth;
       {Property to define that the dialog height should be calculated automaticly }
    property AutoHeight : boolean Read fAutoHeight Write fAutoHeight;
       {Maximum ClientWidth of the Dialog}
    property MaxWidth : integer Read fMaxWidth Write fMaxWidth default 400;
       {Maximum ClientHeight of the Dialog}
    property MaxHeight : integer Read fMaxHeight Write fMaxHeight default 600;
    property OkButtonVisible : boolean Read fOkButtonVisible Write fOkButtonVisible;
    property CancelButtonVisible : boolean Read fCancelButtonVisible Write fCancelButtonVisible;
    property HistoryEnabled : boolean Read fHistoryEnabled Write fHistoryEnabled;
    property LastHistoryName : string Read fLastHistoryName Write fLastHistoryName;

    property AppStore : TJvCustomAppStore Read GetAppStore Write SetAppStore;
  end;

  TJvParameterListSelectList = class (tJvAppStoreSelectList)
  private
    fParameterList : TJvParameterList;
  protected
    function GetDynControlEngine : tJvDynControlEngine; override;
    function GetParameterList : TJvParameterList; virtual;
    procedure SetParameterList(Value : TJvParameterList); virtual;
    function GetAppStore : TJvCustomAppStore; override;
    procedure SetAppStore(Value : TJvCustomAppStore); override;
  public
 //        constructor create (aOwner : TComponent); override;
 //        destructor destroy; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure RestoreParameterList(aCaption : string = '');
    procedure SaveParameterList(aCaption : string = '');
  published
    property ParameterList : TJvParameterList Read GetParameterList Write SetParameterList;
  end;


  TJvParameterListPropertyStore = class (tJvCustomPropertyStore)
  private
    fParameterList : TJvParameterList;
  protected
    procedure LoadData; override;
    procedure StoreData; override;
  public
    property ParameterList : TJvParameterList Read fParameterList Write fParameterList;
  end;

{.$ENDIF}

implementation


{.$IFDEF COMPILER6_UP}

uses JvParameterList_Parameter;

resourcestring
  ErrParameterMustBeEntered = 'Parameter %s must be entered!';
  HistorySelectPath = 'History';


{*****************************************************************************}
{* tJvParameterListMessages
{*****************************************************************************}
constructor tJvParameterListMessages.Create;
begin
  inherited;
  Caption      := '';
  OkButton     := '&Ok';
  CancelButton := '&Cancel';
  HistoryLoadButton := '&Load';
  HistorySaveButton := '&Save';
  HistoryClearButton := 'Cl&ear';
  HistoryLoadCaption := 'Load Parameter Settings';
  HistorySaveCaption := 'Save Parameter Settings';
  HistoryClearCaption := 'Manage Parameter Settings';
end;

{*****************************************************************************}
{* tJvParameterListEnableDisableReason                                                           *}
{*****************************************************************************}

procedure tJvParameterListEnableDisableReason.SetAsString(Value : string);
begin
  fValue := Value;
end;   {*** Procedure tJvParameterListEnableDisableReason.SetAsString ***}

function tJvParameterListEnableDisableReason.GetAsString : string;
begin
  Result := fValue;
end;   {*** Function tJvParameterListEnableDisableReason.GetAsString ***}

procedure tJvParameterListEnableDisableReason.SetAsDouble(Value : double);
begin
  fValue := Value;
end;   {*** Procedure tJvParameterListEnableDisableReason.SetAsDouble ***}

function tJvParameterListEnableDisableReason.GetAsDouble : double;
begin
  Result := fValue;
end;   {*** Function tJvParameterListEnableDisableReason.GetAsDouble ***}

procedure tJvParameterListEnableDisableReason.SetAsInteger(Value : integer);
begin
  fValue := Value;
end;   {*** Procedure tJvParameterListEnableDisableReason.SetAsInteger ***}

function tJvParameterListEnableDisableReason.GetAsInteger : integer;
begin
  Result := fValue;
end;   {*** Function tJvParameterListEnableDisableReason.GetAsInteger ***}

procedure tJvParameterListEnableDisableReason.SetAsBoolean(Value : boolean);
begin
  if Value then
    fValue := 'TRUE'
  else
    fValue := 'FALSE';
end;   {*** Procedure tJvParameterListEnableDisableReason.SetAsBoolean ***}

function tJvParameterListEnableDisableReason.GetAsBoolean : boolean;
var
  s : string;
begin
  s      := fValue;
  Result := s = 'TRUE';
end;   {*** Function tJvParameterListEnableDisableReason.GetAsBoolean ***}

procedure tJvParameterListEnableDisableReason.SetAsDate(Value : tDateTime);
begin
  fValue := Value;
end;   {*** Procedure tJvParameterListEnableDisableReason.SetAsDate ***}

function tJvParameterListEnableDisableReason.GetAsDate : tDateTime;
begin
  Result := fValue;
end;   {*** Function tJvParameterListEnableDisableReason.GetAsDate ***}

procedure tJvParameterListEnableDisableReason.SetAsVariant(Value : variant);
begin
  fValue := Value;
end;   {*** Procedure tJvParameterListEnableDisableReason.SetAsVariant ***}

function tJvParameterListEnableDisableReason.GetAsVariant : variant;
begin
  Result := fValue;
end;   {*** Function tJvParameterListEnableDisableReason.GetAsVariant ***}

procedure tJvParameterListEnableDisableReason.Assign(Source : TPersistent);
begin
  inherited Assign(Source);
  AsVariant := tJvParameterListEnableDisableReason(Source).AsVariant;
end;   {*** Procedure tJvParameterListEnableDisableReason.Assign ***}


{*****************************************************************************}
{* tJvParameterListEnableDisableReasonList                                                  *}
{*****************************************************************************}

procedure tJvParameterListEnableDisableReasonList.Clear;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if Assigned(Objects[i]) then
      Objects[i].Free;
  inherited Clear;
end;

procedure tJvParameterListEnableDisableReasonList.AddReasonVariant(RemoteParameterName : string; Value : variant);
var
  Reason : tJvParameterListEnableDisableReason;
begin
  Reason := tJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsVariant := Value;
  AddObject(RemoteParameterName, Reason);
end;   {*** procedure tJvParameterListEnableDisableReasonList.AddReason ***}

procedure tJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName : string; Value : boolean);
var
  Reason : tJvParameterListEnableDisableReason;
begin
  Reason := tJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsBoolean := Value;
  AddObject(RemoteParameterName, Reason);
end;   {*** procedure tJvParameterListEnableDisableReasonList.AddReason ***}

procedure tJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName : string; Value : integer);
var
  Reason : tJvParameterListEnableDisableReason;
begin
  Reason := tJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsInteger := Value;
  AddObject(RemoteParameterName, Reason);
end;   {*** procedure tJvParameterListEnableDisableReasonList.AddReason ***}

procedure tJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName : string; Value : double);
var
  Reason : tJvParameterListEnableDisableReason;
begin
  Reason := tJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsDouble := Value;
  AddObject(RemoteParameterName, Reason);
end;   {*** procedure tJvParameterListEnableDisableReasonList.AddReason ***}

procedure tJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName : string; Value : string);
var
  Reason : tJvParameterListEnableDisableReason;
begin
  Reason := tJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsString := Value;
  AddObject(RemoteParameterName, Reason);
end;   {*** procedure tJvParameterListEnableDisableReasonList.AddReason ***}

procedure tJvParameterListEnableDisableReasonList.AddReason(RemoteParameterName : string; Value : tDateTime);
var
  Reason : tJvParameterListEnableDisableReason;
begin
  Reason := tJvParameterListEnableDisableReason.Create;
  Reason.RemoteParameterName := RemoteParameterName;
  Reason.AsDate := Value;
  AddObject(RemoteParameterName, Reason);
end;   {*** procedure tJvParameterListEnableDisableReasonList.AddReason ***}

 {*****************************************************************************}
 {* TJvParameterPropertyValues                                                *}
 {*****************************************************************************}

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
  i : integer;
begin
  for i := 0 to Count - 1 do
    if Assigned(Objects[i]) then
      Objects[i].Free;
end;

procedure TJvParameterPropertyValues.AddValue(aName : string; aValue : variant);
var
  Value : tJvParameterPropertyValue;
begin
  Value := tJvParameterPropertyValue.Create;
  Value.PropertyName := aName;
  Value.PropertyValue := aValue;
  AddObject(aName, Value)
end;


 {*****************************************************************************}
 {* tJvBaseParameter                                                           *}
 {*****************************************************************************}

constructor tJvBaseParameter.Create(AParameterList : TJvParameterList);
begin
  inherited Create(AParameterList);
  fReloadValuefromRegistry := true;
  fTabOrder := -1;
  fParameterList := AParameterList;
  fWinControl := nil;
  fJvDynControl := nil;
  fJvDynControlData := nil;
  Color    := clBtnFace;
  fEnabled := true;
  fVisible := true;
  fEnableReasons := tJvParameterListEnableDisableReasonList.Create;
  fDisableReasons := tJvParameterListEnableDisableReasonList.Create;
end;   {*** Constructor tJvBaseParameter.Create ***}

destructor tJvBaseParameter.Destroy;
begin
  FreeAndNil(fEnableReasons);
  FreeAndNil(fDisableReasons);
  inherited Destroy;
end;   {*** Constructor tJvBaseParameter.Create ***}

procedure tJvBaseParameter.SetAsString(Value : string);
begin
  fValue := Value;
end;   {*** Procedure tJvBaseParameter.SetAsString ***}

function tJvBaseParameter.GetAsString : string;
begin
  if VarIsNull(fValue) then
    Result := ''
  else
    Result := fValue;
end;   {*** Function tJvBaseParameter.GetAsString ***}

procedure tJvBaseParameter.SetAsDouble(Value : double);
begin
  fValue := Value;
end;   {*** Procedure tJvBaseParameter.SetAsDouble ***}

function tJvBaseParameter.GetAsDouble : double;
begin
  if AsString = '' then
    Result := 0
  else
    Result := fValue;
end;   {*** Function tJvBaseParameter.GetAsDouble ***}

procedure tJvBaseParameter.SetAsInteger(Value : integer);
begin
  fValue := Value;
end;   {*** Procedure tJvBaseParameter.SetAsInteger ***}

function tJvBaseParameter.GetAsInteger : integer;
begin
  if VarIsNull(fValue) then
    Result := 0
  else
    Result := fValue;
end;   {*** Function tJvBaseParameter.GetAsInteger ***}

procedure tJvBaseParameter.SetAsBoolean(Value : boolean);
begin
  if Value then
    fValue := 'TRUE'
  else
    fValue := 'FALSE';
end;   {*** Procedure tJvBaseParameter.SetAsBoolean ***}

function tJvBaseParameter.GetAsBoolean : boolean;
var
  s : string;
begin
  if VarIsNull(fValue) then
    Result := false
  else
  begin
    s      := fValue;
    Result := Uppercase(s) = 'TRUE';
  end;
end;   {*** Function tJvBaseParameter.GetAsBoolean ***}

procedure tJvBaseParameter.SetAsDate(Value : tDateTime);
begin
  fValue := VarFromDateTime(Value);
end;   {*** Procedure tJvBaseParameter.SetAsDate ***}

function tJvBaseParameter.GetAsDate : tDateTime;
begin
  if VarIsNull(fValue) then
    Result := 0
  else
    Result := VarToDateTime(fValue);
end;   {*** Function tJvBaseParameter.GetAsDate ***}

procedure tJvBaseParameter.SetAsVariant(Value : variant);
begin
  fValue := Value;
end;   {*** Procedure tJvBaseParameter.SetAsVariant ***}

function tJvBaseParameter.GetAsVariant : variant;
begin
  Result := fValue;
end;   {*** Function tJvBaseParameter.GetAsVariant ***}

procedure tJvBaseParameter.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = fWinControl) and (Operation = opRemove) then
  begin
    fWinControl   := nil;
    fJvDynControl := nil;
    fJvDynControlData := nil;
  end;
end;   {*** procedure tBaseOraToolForm.Notification ***}


function tJvBaseParameter.GetWinControlData : variant;
begin
  Result := NULL;
  if Assigned(JvDynControlData) then
    Result := JvDynControlData.Value;
end;

procedure tJvBaseParameter.SetWinControlData(Value : variant);
begin
  if Assigned(JvDynControlData) then
    JvDynControlData.Value := Value;
end;

function tJvBaseParameter.GetDynControlEngine : tJvDynControlEngine;
begin
  Result := nil;
  if Assigned(ParameterList) then
    Result := ParameterList.DynControlEngine;
end;

type
  tHackWinControl = class (tWinControl)
  public
    property OnExit;
  end;

procedure tJvBaseParameter.SetWinControl(Value : TWinControl);

begin
  fJvDynControl := nil;
  fWinControl   := Value;
  if not Assigned(Value) then
    Exit;
  Supports(fWinControl, IJvDynControl, fJvDynControl);
  Supports(fWinControl, IJvDynControlData, fJvDynControlData);

  JvDynControl.ControlSetCaption(Caption);
  if Assigned(JvDynControlData) then
    JvDynControlData.ControlSetReadOnly(ReadOnly);
  WinControl.Visible := Visible;
  WinControl.Enabled := Enabled;
  WinControl.Hint    := Hint;
  WinControl.HelpContext := HelpContext;
  JvDynControl.ControlSetOnExit(HandleEnableDisable);
end;

function tJvBaseParameter.GetWinControl : TWinControl;
begin
  Result := fWinControl
end;

procedure tJvBaseParameter.SetEnabled(Value : boolean);
begin
  fEnabled := Value;
  if Assigned(WinControl) then
    WinControl.Enabled := Value;
end;


procedure tJvBaseParameter.GetData;
begin
  fValue := NULL;
  if Assigned(WinControl) then
    fValue := WinControlData;
end;

procedure tJvBaseParameter.SetData;
begin
  if Assigned(WinControl) then
    WinControlData := fValue;
end;

procedure tJvBaseParameter.Assign(Source : TPersistent);
begin
  inherited Assign(Source);
  AsVariant  := tJvBaseParameter(Source).AsVariant;
  Caption    := tJvBaseParameter(Source).Caption;
  SearchName := tJvBaseParameter(Source).SearchName;
  Width      := tJvBaseParameter(Source).Width;
  Height     := tJvBaseParameter(Source).Height;
  Required   := tJvBaseParameter(Source).Required;
  ParentParameterName := tJvBaseParameter(Source).ParentParameterName;
  ReloadValueFromRegistry := tJvBaseParameter(Source).ReloadValueFromRegistry;
  TabOrder   := tJvBaseParameter(Source).TabOrder;
  fParameterList := tJvBaseParameter(Source).ParameterList;
  Color      := tJvBaseParameter(Source).Color;
  ReadOnly   := tJvBaseParameter(Source).ReadOnly;
  Enabled    := tJvBaseParameter(Source).Enabled;
  fEnableReasons.Assign(tJvBaseParameter(Source).fEnableReasons);
  fDisableReasons.Assign(tJvBaseParameter(Source).fDisableReasons);
end;   {*** Procedure tJvBaseParameter.Assign ***}

function tJvBaseParameter.Validate(var fData : variant) : boolean;
begin
  if not Required or not Enabled then
    Result := true
  else
    Result := not VarIsNull(fData);
  if not Result then
    MessageDlg(Format(ErrParameterMustBeEntered, [Caption]), mtError, [mbOK], 0);
end;   {*** Function tJvBaseParameter.Validate ***}

function tJvBaseParameter.GetParameterNameExt : string;
begin
  Result := '';
end;   {*** function tJvBaseParameter.GetParameterNameExt ***}

function tJvBaseParameter.GetParameterNameBase : string;
begin
  Result := 'ParameterItem' + SearchName;
end;   {*** function tJvBaseParameter.GetParameterNameExt ***}

function tJvBaseParameter.GetParameterName : string;
begin
  Result := GetParameterNameBase + GetParameterNameExt;
end;   {*** function tJvBaseParameter.GetParameterName ***}

procedure tJvBaseParameter.HandleEnableDisable(Sender : TObject);
var
  iEnable :   integer;
  Reason :    tJvParameterListEnableDisableReason;
  i, j :      integer;
  Parameter : tJvBaseParameter;
  HandleParameter : tJvBaseParameter;
  Data :      variant;
begin
  if not (Sender is tWinControl) then
    Exit;
  if not Assigned(ParameterList) then
    Exit;
  HandleParameter := nil;
  for i := 0 to ParameterList.Count - 1 do
    if Assigned(ParameterList.ParambyIndex(i).WinControl) then
      if Sender = ParameterList.ParambyIndex(i).WinControl then
      begin
        HandleParameter := ParameterList.ParambyIndex(i);
        break;
      end;  {*** IF Sender = ParameterList.ParambyIndex(i).WinControl ***}
  if not Assigned(HandleParameter) then
    Exit;
  Data := HandleParameter.GetWinControlData;
  if VarIsNull(Data) then
    Exit;
  for i := 0 to ParameterList.Count - 1 do
  begin
    Parameter := ParameterList.ParamByIndex(i);
    if not Assigned(Parameter) then
      Continue;
    iEnable := 0;
    if Parameter.EnableReasons.Count > 0 then
    begin
      iEnable := -1;
      for j := 0 to Parameter.EnableReasons.Count - 1 do
      begin
        Reason := tJvParameterListEnableDisableReason(Parameter.EnableReasons.Objects[j]);
        if not Assigned(Reason) then
          Continue;
        if Reason.RemoteParameterName <> HandleParameter.SearchName then
          Continue;
        if VarIsNull(Reason.AsVariant) then
          Continue;
        if iEnable = 0 then
          iEnable := -1;
        if (Reason.AsVariant = Data) then
          iEnable := 1;
      end;   {*** FOR i := 0 TO EnableReasons.Count-1 DO **}
    end;   {*** IF Parameter.EnableReasons.Count > 0 THEN ***}
    if Parameter.DisableReasons.Count > 0 then
    begin
      for j := 0 to Parameter.DisableReasons.Count - 1 do
      begin
        Reason := tJvParameterListEnableDisableReason(Parameter.DisableReasons.Objects[j]);
        if not Assigned(Reason) then
          Continue;
        if Reason.RemoteParameterName <> HandleParameter.SearchName then
          Continue;
        if VarIsNull(Reason.AsVariant) then
          Continue;
        if iEnable = 0 then
          iEnable := 1;
        if (Reason.AsVariant = Data) then
          iEnable := -1;
      end;   {*** FOR i := 0 TO DisableReasons.Count-1 DO **}
    end;     {*** IF Parameter.DisableReasons.Count > 0 THEN ***}
    case iEnable of
      -1 : Parameter.Enabled := false;
      1 : Parameter.Enabled  := true;
    end;  {*** CASE iEnable OF ***}
  end;   {*** FOR i := 0 TO ParameterList.Count-1 DO **}
end;   {*** procedure tJvBaseParameter.HandleEnableDisable ***}


 {*****************************************************************************}
 {* TJvParameterList                                                            *}
 {*****************************************************************************}

procedure TJvParameterList.AddParameter(aParameter : tJvBaseParameter);
begin
  AddObject(aParameter.SearchName, aParameter);
end;   {*** procedure TJvParameterList.AddParam ***}

function TJvParameterList.ExistsParameter(iSearchName : string) : boolean;
begin
  Result := Assigned(ParameterByName(iSearchName));
end;   {*** function TJvParameterList.ParameterByName ***}

function TJvParameterList.ParameterByName(iSearchName : string) : tJvBaseParameter;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Uppercase(Parameters[i].SearchName) = Uppercase(iSearchName) then
    begin
      Result := (Parameters[i]);
      break;
    end;
end;   {*** function TJvParameterList.ParameterByName ***}

function TJvParameterList.ParamByIndex(iIndex : integer) : tJvBaseParameter;
begin
  Result := (Parameters[iIndex]);
end;   {*** function TJvParameterList.ParameterByName ***}

procedure TJvParameterList.Assign(Source : TPersistent);
begin
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
  fintParameterList.Assign(TJvParameterList(Source).fintParameterList);
  HistoryEnabled := TJvParameterList(Source).HistoryEnabled;
  Path := TJvParameterList(Source).Path;
end;   {*** Procedure TJvParameterList.SetAsDate ***}


procedure TJvParameterList.SetPath(Value : string);
begin
  fParameterListPropertyStore.Path    := Value;
  fParameterListSelectList.SelectPath := Value + '\' + HistorySelectPath;
end;   {*** procedure TJvParameterList.SetPath ***}

function TJvParameterList.GetPath : string;
begin
  Result := fParameterListPropertyStore.Path;
end;   {*** function TJvParameterList.GetPath ***}


function TJvParameterList.GetAppStore : TJvCustomAppStore;
begin
  Result := fParameterListPropertyStore.AppStore;
end;

procedure TJvParameterList.SetAppStore(Value : TJvCustomAppStore);
begin
  fParameterListPropertyStore.AppStore := Value;
end;

constructor TJvParameterList.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fMessages := TJvParameterListMessages.Create;
  fParameterListPropertyStore := TJvParameterListPropertyStore.Create(nil);
  fParameterListPropertyStore.Parameterlist := Self;
  fintParameterList := TStringList.Create;
  fDynControlEngine := DefaultDynControlEngine;
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
  fMaxWidth    := 600;
  fMaxHeight   := 400;
  fOkbuttonVisible := true;
  fCancelButtonVisible := true;
  fHistoryEnabled := false;
  fLastHistoryName := '';
  fParameterListSelectList := TJvParameterListSelectList.Create(self);
  fParameterListSelectList.ParameterList := Self;
end;   {*** constructor TJvParameterList.create; ***}

destructor TJvParameterList.Destroy;
begin
  DestroyWinControls;
  FreeAndNil(fParameterListSelectList);
  if Assigned(fintParameterList) then
    FreeAndNil(fintParameterList);
  if Assigned(fParameterListPropertyStore) then
    FreeAndNil(fParameterListPropertyStore);
  FreeAndNil(FArrangeSettings);
  FreeAndNil(fMessages);
  inherited Destroy;
end;  {*** destructor TJvParameterList.destroy; ***}

procedure TJvParameterList.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = ScrollBox) then
      Scrollbox := nil;
    if (AComponent = RightPanel) then
      RightPanel := nil;
    if (AComponent = ArrangePanel) then
      ArrangePanel := nil;
    if (AComponent = fParameterListPropertyStore) then
      fParameterListPropertyStore := nil;
  end;
end;   {*** procedure TJvParameterList.Notification ***}

procedure TJvParameterList.SetDynControlEngine(Value : tJvDynControlEngine);
begin
  fDynControlEngine := Value;
end;

procedure TJvParameterList.StoreData;
begin
  if Path = '' then
    Exit;
  fParameterListPropertyStore.StoreData;
end;   {*** procedure TJvParameterList.StoreData; ***}

procedure TJvParameterList.LoadData;
begin
  if Path = '' then
    Exit;
  fParameterListPropertyStore.LoadData;
end;   {*** procedure TJvParameterList.LoadData ***}

procedure TJvParameterList.OnOkButtonClick(Sender : TObject);
begin
  if not ValidateDataAtWinControls then
    Exit;
  ParameterDialog.ModalResult := mrOk;
end;

procedure TJvParameterList.OnCancelButtonClick(Sender : TObject);
begin
  ParameterDialog.ModalResult := mrCancel;
end;

type
  tHackPanel = class (tCustomControl)
  public
    property canvas;
  end;


{ Creates the ParameterDialog }
procedure TJvParameterList.CreateParameterDialog;
var
  MainPanel, BottomPanel, HistoryPanel, ButtonPanel : tWinControl;
  OkButton, CancelButton : tWinControl;
  LoadButton, SaveButton, ClearButton : tWinControl;
  ButtonLeft : integer;
  ITmpPanel :  IJVDynControlPanel;
begin
  if Assigned(fParameterDialog) then
    FreeAndNil(fParameterDialog);

  fParameterDialog := DynControlEngine.CreateForm('', '');

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
  if not Supports(BottomPanel, IJVDynControlPanel, ITmpPanel) then
    raise EIntfCastError.Create('SIntfCastError');
  with ITmpPanel do
    ControlSetBorder(bvNone, bvRaised, 1, bsNone, 0);

  MainPanel := DynControlEngine.CreatePanelControl(Self, ParameterDialog, 'MainPanel', '', alClient);
  if not Supports(MainPanel, IJVDynControlPanel, ITmpPanel) then
    raise EIntfCastError.Create('SIntfCastError');
  with ITmpPanel do
    ControlSetBorder(bvNone, bvRaised, 1, bsNone, 3);

  ButtonPanel := DynControlEngine.CreatePanelControl(Self, BottomPanel, 'BottonPanel', '', alRight);
  if not Supports(ButtonPanel, IJVDynControlPanel, ITmpPanel) then
    raise EIntfCastError.Create('SIntfCastError');
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
    if not Supports(HistoryPanel, IJVDynControlPanel, ITmpPanel) then
      raise EIntfCastError.Create('SIntfCastError');
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
      Width  := thackPanel(HistoryPanel).Canvas.TextWidth(Messages.HistoryLoadButton) + 5;
      ButtonLeft := Left + Width + 5;
    end;
    SaveButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'SaveButton', Messages.HistorySaveButton, '', HistorySaveClick, false, false);
    with SaveButton do
    begin
      Left   := ButtonLeft;
      Top    := 5;
      Height := 20;
      Width  := thackPanel(HistoryPanel).Canvas.TextWidth(Messages.HistorySaveButton) + 5;
      ButtonLeft := Left + Width + 5;
    end;
    ClearButton := DynControlEngine.CreateButton(Self, HistoryPanel, 'ClearButton', Messages.HistoryClearButton, '', HistoryClearClick, false, false);
    with ClearButton do
    begin
      Left   := ButtonLeft;
      Top    := 5;
      Height := 20;
      Width  := thackPanel(HistoryPanel).Canvas.TextWidth(Messages.HistoryClearButton) + 5;
      ButtonLeft := Left + Width + 5;
    end;
    HistoryPanel.Width := ButtonLeft;
  end;   {*** IF HistoryEnabled THEN ***}

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
  CheckScrollboxAutoScroll;

end;


function TJvParameterList.ShowParameterDialog : boolean;
begin
  if Count = 0 then
    Exception.Create('TJvParameterList.ShowParameterDialog - No Parameters defined');
  CreateParameterDialog;
  try
    SetDataToWinControls;
    ParameterDialog.ShowModal;
    Result := ParameterDialog.ModalResult = mrOk;
    if Result then
      GetDataFromWinControls;
  finally
    FreeAndNil(fParameterDialog);
  end;
end;   {*** Function TJvParameterList.ShowParameterDialog ***}

function TJvParameterList.GetParentByName(MainParent : TWinControl; SearchName : string) : TWinControl;
var
  Parameter : tJvBaseParameter;
  i, j :      integer;
begin
  Result := MainParent;
  if (SearchName = '') or not Assigned(MainParent) then
    Exit;
  for i := 0 to Count - 1 do
    if Parameters[i].Visible then
 //      if Parameters[i] is TJvTabControlParameter then
 //        for j := 0 to TJvTabControlParameter(Parameters[i]).Tabs.Count - 1 do
//          if Uppercase(Parameters[i].SearchName + '.' + TJvTabControlParameter(Parameters[i]).Tabs[j]) = Uppercase(SearchName) then
//          begin
//            Result := TWinControl(TJvTabControlParameter(Parameters[i]).TabWinControls.Objects[j]);
//            break;
//          end   {*** IF Uppercase(tJvBaseParameter(Objects[i]).SearchName) = Uppercase(iSearchName) THEN ***}
//          else
    else if Uppercase(Parameters[i].SearchName) = Uppercase(SearchName) then
    begin
      Parameter := (Parameters[i]);
      if (Parameter is TJvArrangeParameter) then
      begin
        Result := TWinControl(Parameter.WinControl);
        break;
      end;  {*** IF (Parameter IS TJvArrangeParameter) THEN ***}
    end;   {*** IF Uppercase(tJvBaseParameter(Objects[i]).SearchName) = Uppercase(iSearchName) THEN ***}
end;   {*** function TJvParameterList.GetParentByName ***}


procedure TJvParameterList.HistoryLoadClick(Sender : TObject);
begin
  ParameterListSelectList.RestoreParameterList(Messages.HistoryLoadCaption);
end;

procedure TJvParameterList.HistorySaveClick(Sender : TObject);
begin
  ParameterListSelectList.SaveParameterList(Messages.HistorySaveCaption);
  ;
end;

procedure TJvParameterList.HistoryClearClick(Sender : TObject);
begin
  ParameterListSelectList.ManageSelectList(Messages.HistoryClearCaption);
end;


procedure TJvParameterList.CreateWinControlsOnParent(ParameterParent : TWinControl);
var
  i : integer;
begin
  if Assigned(Scrollbox) then
    Scrollbox.Free;
  if Assigned(ArrangePanel) then
    ArrangePanel.Free;
  Scrollbox := TScrollBox.Create(Self);
  ScrollBox.Parent := ParameterParent;
  with ScrollBox do
  begin
    AutoScroll := false;
    BorderStyle := bsNone;
    Align := alClient;
  end;   {*** WITH ScrollBox DO ***}
  RightPanel := TJvPanel.Create(Self);
  RightPanel.Parent := ScrollBox;
  with RightPanel do
  begin
    //Align := alNone;
    Align   := alRight;
    BorderStyle := bsNone;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Left    := 0;
    Top     := 0;
    Width   := 20;
    Visible := false;
  end;   {*** WITH MainArrangePanel DO ***}
  ArrangePanel      := TJvPanel.Create(Self);
  ArrangePanel.Parent := ScrollBox;
  ArrangePanel.Name := 'MainArrangePanel';
  with ArrangePanel do
  begin
    //Align := alNone;
    Align := alTop;
    BorderStyle := bsNone;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Caption := '';
    Left := 0;
    Top  := 0;
  end;   {*** WITH MainArrangePanel DO ***}
  ArrangePanel.ArrangeSettings := ArrangeSettings;
  try
    ArrangePanel.DisableArrange;
    for i := 0 to Count - 1 do
      if Parameters[i].Visible then
      begin
        Parameters[i].CreateWinControlOnParent(
          GetParentByName(ArrangePanel, Parameters[i].ParentParameterName));
        Parameters[i].WinControlData := Parameters[i].AsVariant;
      end;   {*** FOR i := 0 TO Count-1 DO ***}
    for i := 0 to Count - 1 do
      if Parameters[i].Visible then
        if Assigned(Parameters[i].WinControl) then
          if Assigned(tHackWincontrol(Parameters[i].WinControl).OnExit) then
            tHackWincontrol(Parameters[i].WinControl).OnExit(Parameters[i].WinControl);
  finally
    ArrangePanel.EnableArrange;
  end;
  ArrangePanel.ArrangeControls;
  CheckScrollboxAutoScroll;
end;   {*** procedure TJvParameterList.CreateWinControlsOnParent ***}


procedure TJvParameterList.CheckScrollboxAutoScroll;
begin
  if not Assigned(Scrollbox) then
    Exit;
  if not Assigned(ArrangePanel) then
    Exit;
  RightPanel.Visible   := false;
  ScrollBox.AutoScroll := false;
  if (ArrangePanel.Width > Scrollbox.Width) {OR
     (ArrangePanel.Width > MaxWidth) }then
  begin
    RightPanel.Visible   := true;
    ScrollBox.AutoScroll := true;
  end;
  if (ArrangePanel.Height > Scrollbox.Height) {OR
     (ArrangePanel.Height > MaxHeight) }then
    ScrollBox.AutoScroll := true;
end;

procedure TJvParameterList.DestroyWinControls;
var
  i : integer;
begin
  if Assigned(ArrangePanel) then
    FreeAndNil(ArrangePanel);
  if Assigned(ScrollBox) then
    FreeAndNil(scrollbox);
end;  {*** procedure TJvParameterList.DestroyWinControls; ***}

procedure TJvParameterList.GetDataFromWinControls;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if Parameters[i].Visible then
      Parameters[i].GetData;
end;   {*** procedure TJvParameterList.GetDataFromWinControls ***}

procedure TJvParameterList.SetDataToWinControls;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if Parameters[i].Visible then
      Parameters[i].SetData;
end;   {*** procedure TJvParameterList.GetDataFromWinControls ***}

function TJvParameterList.ValidateDataAtWinControls : boolean;
var
  i : integer;
  v : variant;
  b : boolean;
begin
  Result := false;
  for i := 0 to Count - 1 do
    if Parameters[i].Visible then
    begin
      v := parameters[i].WinControlData;
      b := Parameters[i].Validate(v);
      parameters[i].WinControlData := v;
      if not b then
        Exit;
    end;
  Result := true;
end;   {*** function TJvParameterList.ValidateDataAtWinControls ***}

function TJvParameterList.GetCount : integer;
begin
  Result := fintParameterList.Count;
end;   {*** function TJvParameterList.GetCount ***}

function TJvParameterList.AddObject(const S : string; AObject : TObject) : integer;
begin
  if not (AObject is tJvBaseParameter) then
    raise Exception.Create('TJvParameterList.AddObject : Wrong Object-Type');
  if tJvBaseParameter(AOBject).SearchName = '' then
    raise Exception.Create('TJvParameterList.AddObject : Searchname NOT defined');
  tJvBaseParameter(AObject).ParameterList := Self;
  Result := fintParameterList.AddObject(s, AObject);
end;  {*** function TJvParameterList.AddObject ***}

procedure TJvParameterList.InsertObject(Index : integer; const S : string; AObject : TObject);
begin
  if not (AObject is tJvBaseParameter) then
    raise Exception.Create('TJvParameterList.InsertObject : Wrong Object-Type');
  if tJvBaseParameter(AOBject).SearchName = '' then
    raise Exception.Create('TJvParameterList.InsertObject : Searchname NOT defined');
  tJvBaseParameter(AObject).ParameterList := Self;
  fintParameterList.InsertObject(Index, s, AObject);
end;  {*** procedure TJvParameterList.InsertObject ***}

procedure TJvParameterList.SetArrangeSettings(Value : TJvArrangeSettings);
begin
  fArrangeSettings.Assign(Value);
  if Assigned(ArrangePanel) then
    ArrangePanel.ArrangeSettings := ArrangeSettings;
end;  {*** procedure TJvParameterList.SetArrangeSettings ***}

procedure TJvParameterList.SetParameters(Index : integer; Value : tJvBaseParameter);
begin
  if (Index >= 0) and (Index < fintParameterList.Count) then
    fintParameterList.Objects[Index] := Value;
end;  {*** procedure TJvParameterList.SetParameters ***}



function TJvParameterList.GetParameters(Index : integer) : tJvBaseParameter;
begin
  if (Index >= 0) and (Index < fintParameterList.Count) then
    Result := tJvBaseParameter(fintParameterList.Objects[Index])
  else
    Result := nil;
end;  {*** function TJvParameterList.GetParameters ***}

function TJvParameterList.GetCurrentWidth : integer;
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
end;   {*** function TJvParameterList.GetCurrentWidth  ***}

function TJvParameterList.GetCurrentHeight : integer;
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
end;   {*** function TJvParameterList.GetCurrentHeight  ***}

procedure TJvParameterList.Clear;
begin
  fintParameterList.Clear;
end;   {*** procedure TJvParameterList.clear; ***}


 {*****************************************************************************}
 {* TJvParameterListPropertyStore                                                     *}
 {*****************************************************************************}

procedure TJvParameterListPropertyStore.LoadData;
var
  i : integer;
begin
  with Parameterlist do
    for i := 0 to ParameterList.Count - 1 do
      if not (Parameters[i] is tJvNoDataParameter) then
        with Parameters[i] do
          if ReloadValuefromRegistry then
            if Parameters[i] is tJvListParameter then
              with tJvListParameter(Parameters[i]) do
                ItemIndex := AppStore.ReadInteger(Path + '\' + SearchName, ItemIndex)
            else
              AsString := AppStore.ReadString(Path + '\' + SearchName, AsString);
end;   {*** procedure TJvParameterListPropertyStore.LoadData; ***}

procedure TJvParameterListPropertyStore.StoreData;
var
  i : integer;
begin
  with Parameterlist do
    for i := 0 to ParameterList.Count - 1 do
      if not (Parameters[i] is tJvNoDataParameter) then
        with Parameters[i] do
          if ReloadValuefromRegistry then
            if Parameters[i] is tJvListParameter then
              with tJvListParameter(Parameters[i]) do
                AppStore.WriteInteger(Path + '\' + SearchName, ItemIndex)
            else
              AppStore.WriteString(Path + '\' + SearchName, AsString);
end;   {*** procedure TJvParameterListPropertyStore.StoreData; ***}


 {*****************************************************************************}
 {* TJvParameterListPropertyStore                                                     *}
 {*****************************************************************************}

function tJvParameterListSelectList.GetDynControlEngine : tJvDynControlEngine;
begin
  Result := fParameterList.DynControlEngine;
end;

function tJvParameterListSelectList.GetParameterList : TJvParameterList;
begin
  Result := fParameterList;
end;

procedure tJvParameterListSelectList.SetParameterList(Value : TJvParameterList);
begin
  fParameterList := Value;
end;

function tJvParameterListSelectList.GetAppStore : TJvCustomAppStore;
begin
  if Assigned(fParameterList) then
    Result := fParameterList.AppStore;
end;

procedure tJvParameterListSelectList.SetAppStore(Value : TJvCustomAppStore);
begin
  if Assigned(fParameterList) then
    fParameterList.AppStore := Value;
end;

procedure tJvParameterListSelectList.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = fParameterList) then
    fParameterList := nil;
end;


procedure tJvParameterListSelectList.RestoreParameterList(aCaption : string = '');
var
  SavePath : string;
begin
  if not Assigned(ParameterList) then
    Exit;
  Savepath := ParameterList.Path;
  try
    ParameterList.Path := GetSelectPath(sloLoad, aCaption);
    if ParameterList.Path <> '' then
    begin
      ParameterList.LoadData;
      ParameterList.SetDataToWinControls;
    end;
  finally
    ParameterList.Path := SavePath;
  end;
end;

procedure tJvParameterListSelectList.SaveParameterList(aCaption : string = '');
var
  SavePath : string;
begin
  if not Assigned(ParameterList) then
    Exit;
  Savepath := ParameterList.Path;
  try
    ParameterList.Path := GetSelectPath(sloStore, aCaption);
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
