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

unit JvDynControlEngine;

interface

uses  Classes, Controls, Forms, StdCtrls, Graphics, Buttons,
  JvDynControlEngine_Interface;

type
  tJvDynControlType = (
    jctLabel,
    jctStaticText,
    jctPanel,
    jctScrollbox,
    jctEdit,
    jctCheckbox,
    jctComboBox,
    jctGroupBox,
    jctImage,
    jctRadioGroup,
    jctMemo,
    jctListBox,
    jctDateTimeEdit,
    jctDateEdit,
    jctTimeEdit,
    jctIntegerEdit,
    jctDoubleEdit,
    jctDirectoryEdit,
    jctFileNameEdit,
    jctButton,
    jctForm);

  tJvDynControlEngine = class (TPersistent)
  private
    fRegisteredControlTypes : array [tJvDynControlType] of TControlClass;
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    function GetPropCount(Instance: TPersistent): Integer;
  protected
    procedure SetPropertyValue(const aPersistent : TPersistent; const aPropertyName : string; const aValue : variant);
    function GetPropertyValue(const aPersistent : TPersistent; const aPropertyName : string) : variant;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CreateControl(aControlType : tJvDynControlType; aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tControl; virtual;
    function CreateControlClass(aControlClass : tControlClass; aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tControl; virtual;
    function CreateLabelControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string; aFocusControl : TWinControl) : tControl; virtual;
    function CreateStaticTextControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string) : tWinControl; virtual;
    function CreatePanelControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string; aAlign : TAlign) : tWinControl; virtual;
    function CreateScrollBoxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateEditControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateCheckboxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string) : tWinControl; virtual;
    function CreateComboBoxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aItems : TStringList) : tWinControl; virtual;
    function CreateGroupBoxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string) : tWinControl; virtual;
    function CreateImageControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateRadioGroupControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string; aItems : TStrings; aItemIndex : integer = 0) : tWinControl; virtual;
//          function CreatePageControlControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateMemoControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateListBoxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aItems : TStringList) : tWinControl; virtual;
    function CreateDateTimeControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateDateControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateTimeControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateIntegerControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateDoubleControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateDirectoryControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateFileNameControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl; virtual;
    function CreateButton(aOwner : TComponent; aParentControl : tWinControl; aButtonName : string; aCaption : string; aHint : string; aOnClick : TNotifyEvent; aDefault : boolean = false; aCancel : boolean = false) : TButton; virtual;
    function CreateForm(aCaption : string; aHint : string) : tCustomForm; virtual;

    procedure RegisterControl(const aDynControlType : tJvDynControlType; aControlClass : TControlClass); virtual;

    procedure SetControlCaption(aControl : IJvDynControl; Value : string);
    procedure SetControlTabOrder(aControl : IJvDynControl; Value : integer);

    procedure SetControlOnEnter(aControl : IJvDynControl; Value : TNotifyEvent);
    procedure SetControlOnExit(aControl : IJvDynControl; Value : TNotifyEvent);
    procedure SetControlOnClick(aControl : IJvDynControl; Value : TNotifyEvent);
  published
  end;

{$IFNDEF COMPILER6_UP}
function Supports(Instance: TObject; const Intf: TGUID): Boolean; overload;
function Supports(AClass: TClass; const Intf: TGUID): Boolean; overload;
{$ENDIF COMPILER6_UP}
function IntfCast(Instance: TObject; const Intf: TGUID): IUnknown;
procedure SetDefaultDynControlEngine(aEngine : tJvDynControlEngine);
function DefaultDynControlEngine : tJvDynControlEngine;

implementation

uses {$IFDEF COMPILER6_UP}
     Variants,
     {$ENDIF}
     TypInfo, SysUtils, JvTypes, JVDynControlEngine_VCL;

var
  fDefaultDynControlEngine : tJvDynControlEngine;

{$IFNDEF COMPILER6_UP}
function Supports(Instance: TObject; const Intf: TGUID): Boolean;
begin
  Result := Instance.GetInterfaceEntry(Intf) <> nil;
end;

function Supports(AClass: TClass; const Intf: TGUID): Boolean;
begin
  Result := AClass.GetInterfaceEntry(Intf) <> nil;
end;
{$ENDIF COMPILER6_UP}

function IntfCast(Instance: TObject; const Intf: TGUID): IUnknown;
begin
  if not Supports(Instance, Intf, Result) then
    raise EIntfCastError.Create('SIntfCastError');
end;

constructor tJvDynControlEngine.Create;
begin
  inherited Create;
end;

destructor tJvDynControlEngine.Destroy;
begin
  inherited Destroy;
end;

procedure tJvDynControlEngine.RegisterControl(const aDynControlType : tJvDynControlType; aControlClass : TControlClass);
var
  Valid : boolean;
begin
  fRegisteredControlTypes[aDynControlType] := nil;
  Valid := Supports(aControlClass, IJvDynControl);
  case aDynControlType of
    jctButton : Valid   := Valid and Supports(aControlClass, IJvDynControlButton);
    jctPanel : Valid    := Valid and Supports(aControlClass, IJvDynControlPanel);
    jctLabel : Valid    := Valid and Supports(aControlClass, IJvDynControlLabel);
    jctMemo : Valid     := Valid and Supports(aControlClass, IJvDynControlItems) and Supports(aControlClass, IJvDynControlData) and Supports(aControlClass, IJvDynControlMemo);
    jctRadioGroup,
    jctCombobox : Valid := Valid and Supports(aControlClass, IJvDynControlItems) and Supports(aControlClass, IJvDynControlData);
    jctEdit,
    jctIntegerEdit,
    jctDoubleEdit,
    jctFilenameEdit,
    jctDirectoryEdit,
    jctCheckBox,
    jctDateTimeEdit,
    jctDateEdit,
    jctTimeEdit : Valid := Valid and Supports(aControlClass, IJvDynControlData);
  end;
  if Valid then
    fRegisteredControlTypes[aDynControlType] := aControlClass
  else
    raise EJvclException.Create('tJvDynControlEngine.RegisterControl : Unsupported ControlClass');
end;

function tJvDynControlEngine.GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data   := GetTypeData(Instance.Classinfo);
  Result := Data^.PropCount;
end;


function tJvDynControlEngine.GetPropName(Instance: TPersistent; Index: Integer): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.ClassInfo);
  GetMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := PropInfo^.Name;
  finally
    FreeMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  end;
end;


procedure tJvDynControlEngine.SetPropertyValue(const aPersistent : TPersistent; const aPropertyName : string; const aValue : variant);
var Index : Integer;
    PropName : String;
    SubObj: TObject;
    p: Integer;
    SearchName: String;
    LastName: String;
begin
  SearchName := trim(aPropertyName);
  p := Pos('.', SearchName);
  IF p > 0 THEN
  begin
    LastName := trim(Copy (SearchName, p+1, Length(SearchName)-p));
    SearchName := trim(Copy (SearchName, 1, p-1));
  end
  else
    lastname := '';
  for Index := 0 to GetPropCount(aPersistent) - 1 do
  begin
    PropName := Uppercase(GetPropName(aPersistent, Index));
    if not (Uppercase(SearchName) = PropName) then
      continue;
    case PropType(aPersistent, PropName) of
      tkLString, tkWString, tkString:
        SetStrProp(aPersistent, PropName, VarToStr(aValue));
      tkEnumeration,
      tkSet,
      tkChar, tkInteger:
        SetOrdProp(aPersistent, PropName, aValue);
//      tkInt64:
//        SetInt64Prop(aPersistent, PropName, aValue);
      tkFloat:
        SetFloatProp(aPersistent, PropName, aValue);
      tkClass:
        begin
          SubObj := GetObjectProp(aPersistent, PropName);
          if SubObj is TStrings then
            TStrings(SubObj).Text := aValue
          else if (SubObj is TPersistent) AND (LastName <> '') then
            SetPropertyValue(tPersistent(SubObj), LastName, aValue);
        end;
    end;
  end;
end;

function tJvDynControlEngine.GetPropertyValue(const aPersistent : TPersistent; const aPropertyName : string) : variant;
var Index : Integer;
    PropName : String;
    SubObj: TObject;
    p: Integer;
    SearchName: String;
    LastName: String;
begin
  SearchName := trim(aPropertyName);
  p := Pos('.', SearchName);
  IF p > 0 THEN
  begin
    LastName := trim(Copy (SearchName, p+1, Length(SearchName)-p));
    SearchName := trim(Copy (SearchName, 1, p-1));
  end
  else
    lastname := '';
  for Index := 0 to GetPropCount(aPersistent) - 1 do
  begin
    PropName := Uppercase(GetPropName(aPersistent, Index));
    if not (Uppercase(SearchName) = PropName) then
      continue;
    case PropType(aPersistent, PropName) of
      tkLString, tkWString, tkString:
        Result := GetStrProp(aPersistent, PropName);
      tkEnumeration,
      tkSet,
      tkChar, tkInteger:
        Result := GetOrdProp(aPersistent, PropName);
      tkInt64:
        {$IFDEF COMPILER6_UP}
        Result := GetInt64Prop(aPersistent, PropName);
        {$ELSE}
        Result := Null;
        {$ENDIF}
      tkFloat:
        Result := GetFloatProp(aPersistent, PropName);
      tkClass:
        begin
          SubObj := GetObjectProp(aPersistent, PropName);
          if SubObj is TStrings then
            Result := TStrings(SubObj).Text
          else if (SubObj is TPersistent) AND (LastName <> '') then
            Result := GetPropertyValue(tPersistent(SubObj), LastName);
        end;
    end;
  end;
end;

function tJvDynControlEngine.CreateControl(aControlType : tJvDynControlType; aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tControl;
var
  Control : tControl;
begin
  if Assigned(fRegisteredControlTypes[aControlType]) then
    Result := CreateControlClass(fRegisteredControlTypes[aControlType], aOwner, aParentControl, aControlName)
  else if aControlType = jctForm then
  begin
    Result := tControl(TForm.Create(aOwner));
    if aControlName <> '' then
      Result.Name := aControlName;
  end
  else
    Result := nil;
  if Result = nil then
    raise EJvclException.Create('tJvDynControlEngine.CreateControl : No Registered ControlClass');
end;

function tJvDynControlEngine.CreateControlClass(aControlClass : tControlClass; aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := tControl(aControlClass.Create(aOwner));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.Create('SIntfCastError');
  with DynCtrl do
    ControlSetDefaultProperties;
  if Assigned(aParentControl) then
    Result.Parent := aParentControl;
  if aControlName <> '' then
    Result.Name := aControlName;
end;

function tJvDynControlEngine.CreateLabelControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string; aFocusControl : TWinControl) : tControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := CreateControl(jctLabel, aOwner, aParentControl, aControlName);
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.Create('SIntfCastError');
  with DynCtrl do
    ControlSetCaption(aCaption);
  with DynCtrl as IJvDynControlLabel do
    ControlSetFocusControl(aFocusControl);
end;

function tJvDynControlEngine.CreateStaticTextControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string) : tWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := tWinControl(CreateControl(jctStaticText, aOwner, aParentControl, aControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.Create('SIntfCastError');
  with DynCtrl do
    ControlSetCaption(aCaption);
end;


function tJvDynControlEngine.CreatePanelControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string; aAlign : TAlign) : tWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := tWinControl(CreateControl(jctPanel, aOwner, aParentControl, aControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.Create('SIntfCastError');
  with DynCtrl do
    ControlSetCaption(aCaption);
  Result.Align := aAlign;
end;

function tJvDynControlEngine.CreateScrollBoxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctScrollBox, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateEditControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctEdit, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateCheckboxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string) : tWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := tWinControl(CreateControl(jctCheckBox, aOwner, aParentControl, aControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.Create('SIntfCastError');
  with DynCtrl do
    ControlSetCaption(aCaption);
end;

function tJvDynControlEngine.CreateComboBoxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aItems : TStringList) : tWinControl;
var
  DynCtrl: IJvDynControl;
  DynCtrlItems: IJvDynControlItems;
begin
  Result := tWinControl(CreateControl(jctComboBox, aOwner, aParentControl, aControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.Create('SIntfCastError');
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.Create('SIntfCastError');
  DynCtrlItems.Items := aItems;  
end;

function tJvDynControlEngine.CreateGroupBoxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string) : tWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := tWinControl(CreateControl(jctGroupBox, aOwner, aParentControl, aControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.Create('SIntfCastError');
  with DynCtrl do
    ControlSetCaption(aCaption);
end;

function tJvDynControlEngine.CreateImageControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctImage, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateRadioGroupControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aCaption : string; aItems : TStrings; aItemIndex : integer = 0) : tWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := tWinControl(CreateControl(jctRadioGroup, aOwner, aParentControl, aControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.Create('SIntfCastError');
  with DynCtrl do
    ControlSetCaption(aCaption);
  with DynCtrl as iJvDynControlItems do
    Items := aItems;
  with DynCtrl as iJvDynControlData do
    Value := aItemIndex;
end;

//function tJvDynControlEngine.CreatePageControlControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
//begin
//  Result := tWinControl(CreateControl (jctPageControl, aOwner, aParentControl, aControlName));
//end;

function tJvDynControlEngine.CreateMemoControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctMemo, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateListBoxControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string; aItems : TStringList) : tWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := tWinControl(CreateControl(jctListBox, aOwner, aParentControl, aControlName));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.Create('SIntfCastError');
  with DynCtrl as iJvDynControlItems do
    Items := aItems;
end;

function tJvDynControlEngine.CreateDateTimeControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctDateTimeEdit, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateDateControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctDateEdit, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateTimeControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctTimeEdit, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateIntegerControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctIntegerEdit, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateDoubleControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctDoubleEdit, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateDirectoryControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctDirectoryEdit, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateFileNameControl(aOwner : TComponent; aParentControl : tWinControl; aControlName : string) : tWinControl;
begin
  Result := tWinControl(CreateControl(jctFileNameEdit, aOwner, aParentControl, aControlName));
end;

function tJvDynControlEngine.CreateButton(aOwner : TComponent; aParentControl : tWinControl; aButtonName : string; aCaption : string; aHint : string; aOnClick : TNotifyEvent; aDefault : boolean = false; aCancel : boolean = false) : TButton;
begin
  Result      := TButton(CreateControl(jctButton, aOwner, aParentControl, aButtonName));
  Result.Hint := aHint;
  Result.Caption := aCaption;
  Result.Default := aDefault;
  Result.Cancel := aCancel;
  Result.OnClick := aOnClick;
end;

function tJvDynControlEngine.CreateForm(aCaption : string; aHint : string) : tCustomForm;
begin
  Result      := tCustomForm(CreateControl(jctForm, Application, nil, ''));
  Result.Hint := aHint;
end;

procedure tJvDynControlEngine.SetControlCaption(aControl : IJvDynControl; Value : string);
begin
end;

procedure tJvDynControlEngine.SetControlTabOrder(aControl : IJvDynControl; Value : integer);
begin
end;

procedure tJvDynControlEngine.SetControlOnEnter(aControl : IJvDynControl; Value : TNotifyEvent);
begin
end;

procedure tJvDynControlEngine.SetControlOnExit(aControl : IJvDynControl; Value : TNotifyEvent);
begin
end;

procedure tJvDynControlEngine.SetControlOnClick(aControl : IJvDynControl; Value : TNotifyEvent);
begin
end;


procedure SetDefaultDynControlEngine(aEngine : tJvDynControlEngine);
begin
  if aEngine is tJvDynControlEngine then
    fDefaultDynControlEngine := aEngine;
end;

function DefaultDynControlEngine : tJvDynControlEngine;
begin
  Result := fDefaultDynControlEngine;
end;

end.
