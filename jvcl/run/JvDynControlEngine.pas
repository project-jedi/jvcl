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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDynControlEngine;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Controls, Forms, StdCtrls, Graphics,
  Variants,
  JvDynControlEngineIntf;

type
  TJvDynControlType = string;

const
  jctLabel = TJvDynControlType('Label');
  jctStaticText = TJvDynControlType('StaticText');
  jctPanel = TJvDynControlType('Panel');
  jctScrollBox = TJvDynControlType('ScrollBox');
  jctEdit = TJvDynControlType('Edit');
  jctCheckBox = TJvDynControlType('CheckBox');
  jctComboBox = TJvDynControlType('ComboBox');
  jctGroupBox = TJvDynControlType('GroupBox');
  jctImage = TJvDynControlType('Image');
  jctRadioGroup = TJvDynControlType('RadioGroup');
  jctRadioButton = TJvDynControlType('RadioButton');
  jctMemo = TJvDynControlType('Memo');
  jctRichEdit = TJvDynControlType('RichEdit');
  jctListBox = TJvDynControlType('ListBox');
  jctCheckListBox = TJvDynControlType('CheckListBox');
  jctCheckComboBox = TJvDynControlType('CheckComboBox');
  jctDateTimeEdit = TJvDynControlType('DateTimeEdit');
  jctDateEdit = TJvDynControlType('DateEdit');
  jctTimeEdit = TJvDynControlType('TimeEdit');
  jctCalculateEdit = TJvDynControlType('CalculateEdit');
  jctSpinEdit = TJvDynControlType('SpinEdit');
  jctDirectoryEdit = TJvDynControlType('DirectoryEdit');
  jctFileNameEdit = TJvDynControlType('FileNameEdit');
  jctButton = TJvDynControlType('Button');
  jctButtonEdit = TJvDynControlType('ButtonEdit');
  jctTreeView = TJvDynControlType('TreeView');
  jctForm = TJvDynControlType('Form');
  jctProgressBar = TJvDynControlType('Progressbar');
  jctPageControl = TJvDynControlType('Pagecontrol');
  jctTabControl = TJvDynControlType('Tabcontrol');
  jctRTTIInspector = TJvDynControlType('RTTIInspector');
  jctColorComboBox = TJvDynControlType('ColorComboBox');
  jctStringGrid = TJvDynControlType('StringGrid');

type
  TControlClass = class of TControl;

  TJvControlClassObject = class(TObject)
  private
    FControlClass: TControlClass;
  public
    property ControlClass: TControlClass read FControlClass write FControlClass;
  end;

  TJvAfterCreateControl = procedure(AControl: TControl) of object;

  TJvCustomDynControlEngine = class(TPersistent)
  private
    //FRegisteredControlTypes: array [TJvDynControlType] of TControlClass;
    FRegisteredControlTypes: TStringList;
    FRegisterControlsExecuted: Boolean;
    FAfterCreateControl: TJvAfterCreateControl;
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    function GetPropCount(Instance: TPersistent): Integer;
  protected
    procedure SetPropertyValue(const APersistent: TPersistent; const APropertyName: string; const AValue: Variant);
    function GetPropertyValue(const APersistent: TPersistent; const APropertyName: string): Variant;
    procedure AfterCreateControl(AControl: TControl); virtual;
    procedure NeedRegisterControls;
    procedure RegisterControls; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetRegisteredControlClass(AControlType: TJvDynControlType): TControlClass;

    function CreateControl(AControlType: TJvDynControlType; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string): TControl; virtual;
    function CreateControlClass(AControlClass: TControlClass; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string): TControl; virtual;
    function GetControlTextWidth(aControl: TControl; aFont: TFont; const aText:
        string): Integer;

    function IsControlTypeRegistered(const ADynControlType: TJvDynControlType): Boolean;

    function IsControlTypeValid(const ADynControlType: TJvDynControlType;
      AControlClass: TControlClass): Boolean; virtual;
    procedure RegisterControlType(const ADynControlType: TJvDynControlType;
      AControlClass: TControlClass); virtual;

    procedure SetControlCaption(AControl: IJvDynControl; const Value: string);
    procedure SetControlTabOrder(AControl: IJvDynControl; Value: Integer);

    procedure SetControlOnEnter(AControl: IJvDynControl; Value: TNotifyEvent);
    procedure SetControlOnExit(AControl: IJvDynControl; Value: TNotifyEvent);
    procedure SetControlOnClick(AControl: IJvDynControl; Value: TNotifyEvent);
  published
    property OnAfterCreateControl: TJvAfterCreateControl read FAfterCreateControl write FAfterCreateControl;
  end;

  TJvDynControlEngine = class(TJvCustomDynControlEngine)
  private
    FDistanceBetweenLabelAndControlHorz: Integer;
    FDistanceBetweenLabelAndControlVert: Integer;
  public
    constructor Create; override;
    function CreateButton(AOwner: TComponent; AParentControl: TWinControl;
      const AButtonName, ACaption, AHint: string;
      AOnClick: TNotifyEvent; ADefault: Boolean = False;
      ACancel: Boolean = False): TButton; virtual;
    function CreateButtonEditControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; AOnButtonClick: TNotifyEvent): TWinControl; virtual;
    function CreateCalculateControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateCheckboxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string): TWinControl; virtual;
    function CreateCheckComboBoxControl(AOwner: TComponent; AParentControl: TWinControl; const AControlName: string;
        AItems: TStrings; ADelimiter: string): TWinControl; virtual;
    function CreateCheckListBoxControl(AOwner: TComponent; AParentControl: TWinControl; const AControlName: string; AItems:
        TStrings): TWinControl; virtual;
    function CreateColorComboboxControl(AOwner: TComponent; AParentControl:
        TWinControl; const AControlName: string; ADefaultColor: TColor):
        TWinControl; virtual;
    function CreateComboBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; AItems: TStrings): TWinControl; virtual;
    function CreateDateControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateDateTimeControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateDirectoryControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateEditControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateFileNameControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateForm(const ACaption, AHint: string): TCustomForm; virtual;
    function CreateGroupBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string): TWinControl; virtual;
    function CreateImageControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateLabelControl(AOwner: TComponent; AParentControl: TWinControl;
        const AControlName, ACaption: string; AFocusControl: TWinControl = nil):
        TControl; virtual;
    function CreateLabelControlPanel(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string; AFocusControl: TWinControl;
      ALabelOnTop: Boolean = True; ALabelDefaultWidth: Integer = 0): TWinControl; virtual;
    function CreateListBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; AItems: TStrings): TWinControl; virtual;
    function CreateMemoControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreatePageControlControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; APages : TStrings): TWinControl; virtual;
    function CreatePanelControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string; AAlign: TAlign): TWinControl; virtual;
    function CreateProgressbarControl(AOwner: TComponent; AParentControl:
        TWinControl; const AControlName: string; AMin: Integer = 0; AMax: Integer =
        100; AStep: Integer = 1): TWinControl; virtual;
    function CreateRadioButton(AOwner: TComponent; AParentControl: TWinControl;
      const ARadioButtonName, ACaption: string): TWinControl; virtual;
    function CreateRadioGroupControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string; AItems: TStrings;
      AItemIndex: Integer = 0): TWinControl; virtual;
    function CreateRichEditControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateRTTIInspectorControl(AOwner: TComponent; AParentControl:
        TWinControl; const AControlName: string; AOnDisplayProperty:
        TJvDynControlInspectorControlOnDisplayPropertyEvent;
        AOnTranslatePropertyName:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent): TWinControl;
        virtual;
    function CreateScrollBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateSpinControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateStaticTextControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName, ACaption: string): TWinControl; virtual;
    function CreateStringGridControl(AOwner: TComponent; AParentControl: TWinControl; const AControlName: string;
        AColCount, ARowCount: Integer; AFixedCols: Integer = 1; AFixedRows: Integer = 1): TWinControl; virtual;
    function CreateTabControlControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ATabs : TStrings): TWinControl; virtual;
    function CreateTimeControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
    function CreateTreeViewControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string): TWinControl; virtual;
  published
    property DistanceBetweenLabelAndControlHorz: Integer read FDistanceBetweenLabelAndControlHorz write FDistanceBetweenLabelAndControlHorz default 4;
    property DistanceBetweenLabelAndControlVert: Integer read FDistanceBetweenLabelAndControlVert write FDistanceBetweenLabelAndControlVert default 1;
  end;

function IntfCast(Instance: TObject; const Intf: TGUID): IUnknown; overload;
procedure IntfCast(Instance: TObject; const IID: TGUID; out Intf); overload;

procedure SetDefaultDynControlEngine(AEngine: TJvDynControlEngine);
function DefaultDynControlEngine: TJvDynControlEngine;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  TypInfo,
  JvResources, JvTypes, JvComponent,
  JvJVCLUtils;

var
  GlobalDefaultDynControlEngine: TJvDynControlEngine = nil;

function IntfCast(Instance: TObject; const Intf: TGUID): IUnknown;
begin
  if not Supports(Instance, Intf, Result) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
end;

procedure IntfCast(Instance: TObject; const IID: TGUID; out Intf);
begin
  if not Supports(Instance, IID, Intf) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
end;

//=== { TJvCustomDynControlEngine } ==========================================

constructor TJvCustomDynControlEngine.Create;
begin
  inherited Create;
  FRegisteredControlTypes := TStringList.Create;
end;

destructor TJvCustomDynControlEngine.Destroy;
var
  Ind: Integer;
begin
  for Ind := 0 to FRegisteredControlTypes.Count - 1 do
    if Assigned(FRegisteredControlTypes.Objects[Ind]) then
      FRegisteredControlTypes.Objects[Ind].Free;
  FRegisteredControlTypes.Free;
  inherited Destroy;
end;

function TJvCustomDynControlEngine.IsControlTypeRegistered(const ADynControlType: TJvDynControlType): Boolean;
var
  Ind: Integer;
begin
  NeedRegisterControls;
  Ind := FRegisteredControlTypes.IndexOf(ADynControlType);
  if Ind >= 0 then
    Result := Assigned(FRegisteredControlTypes.Objects[Ind])
  else
    Result := False;
end;

function TJvCustomDynControlEngine.IsControlTypeValid(const ADynControlType: TJvDynControlType;
  AControlClass: TControlClass): Boolean;
var
  Valid: Boolean;
begin
  Valid := Supports(AControlClass, IJvDynControl);
  if ADynControlType = jctButton then
    Valid := Valid and Supports(AControlClass, IJvDynControlButton)
  else
  if ADynControlType = jctButtonEdit then
    Valid := Valid and Supports(AControlClass, IJvDynControlButton) and
      Supports(AControlClass, IJvDynControlData)
  else
  if ADynControlType = jctPanel then
    Valid := Valid and Supports(AControlClass, IJvDynControlPanel)
  else
  if ADynControlType = jctLabel then
    Valid := Valid and Supports(AControlClass, IJvDynControlLabel)
  else
  if ADynControlType = jctMemo then
    Valid := Valid and
      Supports(AControlClass, IJvDynControlItems) and
      Supports(AControlClass, IJvDynControlData) and
      Supports(AControlClass, IJvDynControlMemo)
  else
  if (ADynControlType = jctRadioGroup) or
    (ADynControlType = jctComboBox) then
    Valid := Valid and
      Supports(AControlClass, IJvDynControlItems) and
      Supports(AControlClass, IJvDynControlData)
  else
  if (ADynControlType = jctEdit) or
    (ADynControlType = jctCalculateEdit) or
    (ADynControlType = jctSpinEdit) or
    (ADynControlType = jctFileNameEdit) or
    (ADynControlType = jctDirectoryEdit) or
    (ADynControlType = jctCheckBox) or
    (ADynControlType = jctDateTimeEdit) or
    (ADynControlType = jctDateEdit) or
    (ADynControlType = jctTimeEdit) then
    Valid := Valid and Supports(AControlClass, IJvDynControlData);
  Result := Valid;
end;

procedure TJvCustomDynControlEngine.RegisterControlType(const ADynControlType: TJvDynControlType;
  AControlClass: TControlClass);
var
  Ind: Integer;
  ControlClassObject: TJvControlClassObject;
begin
  NeedRegisterControls;
  Ind := FRegisteredControlTypes.IndexOf(ADynControlType);
  if Ind >= 0 then
  begin
    ControlClassObject := TJvControlClassObject(FRegisteredControlTypes.Objects[Ind]);
    if Assigned(ControlClassObject) then
      ControlClassObject.Free;
    FRegisteredControlTypes.Delete(Ind);
  end;
  if IsControlTypeValid(ADynControlType, AControlClass) then
  begin
    ControlClassObject := TJvControlClassObject.Create;
    ControlClassObject.ControlClass := AControlClass;
    FRegisteredControlTypes.AddObject(ADynControlType, ControlClassObject);
  end
  else
    raise EJVCLException.CreateResFmt(@RsEUnsupportedControlClass, [ADynControlType]);
end;

function TJvCustomDynControlEngine.GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Instance.ClassInfo);
  Result := Data.PropCount;
end;

function TJvCustomDynControlEngine.GetPropName(Instance: TPersistent; Index: Integer): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.ClassInfo);
  GetMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo^.Name);
  finally
    FreeMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  end;
end;

procedure TJvCustomDynControlEngine.SetPropertyValue(const APersistent: TPersistent;
  const APropertyName: string; const AValue: Variant);
var
  Index: Integer;
  PropName: string;
  SubObj: TObject;
  P: Integer;
  SearchName: string;
  LastName: string;
begin
  SearchName := Trim(APropertyName);
  P := Pos('.', SearchName);
  if P > 0 then
  begin
    LastName := Trim(Copy(SearchName, P + 1, Length(SearchName) - P));
    SearchName := Trim(Copy(SearchName, 1, P - 1));
  end
  else
    LastName := '';
  for Index := 0 to GetPropCount(APersistent) - 1 do
  begin
    PropName := GetPropName(APersistent, Index);
    if CompareText(SearchName, PropName) = 0 then
    begin
      case PropType(APersistent, PropName) of
        {$IFDEF UNICODE} tkUString, {$ENDIF}
        tkLString, tkWString, tkString:
          SetStrProp(APersistent, PropName, VarToStr(AValue));
        tkEnumeration, tkSet, tkChar, tkInteger:
          SetOrdProp(APersistent, PropName, AValue);
//        tkInt64:
//          SetInt64Prop(APersistent, PropName, AValue);
        tkFloat:
          SetFloatProp(APersistent, PropName, AValue);
        tkClass:
          begin
            SubObj := GetObjectProp(APersistent, PropName);
            if SubObj is TStrings then
              TStrings(SubObj).Text := AValue
            else
            if (SubObj is TPersistent) and (LastName <> '') then
              SetPropertyValue(TPersistent(SubObj), LastName, AValue);
          end;
      end;
      Break; // property was found and there can't be a second property with the same name
    end;
  end;
end;

function TJvCustomDynControlEngine.GetPropertyValue(const APersistent: TPersistent;
  const APropertyName: string): Variant;
var
  Index: Integer;
  PropName: string;
  SubObj: TObject;
  P: Integer;
  SearchName: string;
  LastName: string;
begin
  Result := Null;
  SearchName := Trim(APropertyName);
  P := Pos('.', SearchName);
  if P > 0 then
  begin
    LastName := Trim(Copy(SearchName, P + 1, Length(SearchName) - P));
    SearchName := Trim(Copy(SearchName, 1, P - 1));
  end
  else
    LastName := '';
  for Index := 0 to GetPropCount(APersistent) - 1 do
  begin
    PropName := GetPropName(APersistent, Index);
    if CompareText(SearchName, PropName) = 0 then
    begin
      case PropType(APersistent, PropName) of
        {$IFDEF UNICODE} tkUString, {$ENDIF}
        tkLString, tkWString, tkString:
          Result := GetStrProp(APersistent, PropName);
        tkEnumeration, tkSet, tkChar, tkInteger:
          Result := GetOrdProp(APersistent, PropName);
        tkInt64:
          Result := GetInt64Prop(APersistent, PropName);
        tkFloat:
          Result := GetFloatProp(APersistent, PropName);
        tkClass:
          begin
            SubObj := GetObjectProp(APersistent, PropName);
            if SubObj is TStrings then
              Result := TStrings(SubObj).Text
            else
            if (SubObj is TPersistent) and (LastName <> '') then
              Result := GetPropertyValue(TPersistent(SubObj), LastName);
          end;
      end;
      Break; // property was found and there can't be a second property with the same name
    end;
  end;
end;

procedure TJvCustomDynControlEngine.AfterCreateControl(AControl: TControl);
begin
  if Assigned(FAfterCreateControl) then
    FAfterCreateControl(AControl);
end;

function TJvCustomDynControlEngine.GetRegisteredControlClass(AControlType: TJvDynControlType): TControlClass;
var
  Ind: Integer;
begin
  NeedRegisterControls;
  Result := nil;
  Ind := FRegisteredControlTypes.IndexOf(AControlType);
  if Ind >= 0 then
    if Assigned(FRegisteredControlTypes.Objects[Ind]) and
      (FRegisteredControlTypes.Objects[Ind] is TJvControlClassObject) then
      Result := TJvControlClassObject(FRegisteredControlTypes.Objects[Ind]).ControlClass;
end;

function TJvCustomDynControlEngine.CreateControl(AControlType: TJvDynControlType;
  AOwner: TComponent; AParentControl: TWinControl; AControlName: string): TControl;
begin
  NeedRegisterControls;
  if Assigned(GetRegisteredControlClass(AControlType)) then
    Result := CreateControlClass(GetRegisteredControlClass(AControlType), AOwner,
      AParentControl, AControlName)
  else
  if AControlType = jctForm then
  begin
    Result := TControl(TJvForm.CreateNew(AOwner));
    if AControlName <> '' then
      Result.Name := AControlName;
  end
  else
    Result := nil;
  if Result = nil then
    raise EJVCLException.CreateResFmt(@RsENoRegisteredControlClass, [AControlType]);
  AfterCreateControl(Result);
end;

function TJvCustomDynControlEngine.CreateControlClass(AControlClass: TControlClass;
  AOwner: TComponent; AParentControl: TWinControl; AControlName: string): TControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TControl(AControlClass.Create(AOwner));
  IntfCast(Result, IJvDynControl, DynCtrl);
  DynCtrl.ControlSetDefaultProperties;
  if Assigned(AParentControl) then
    Result.Parent := AParentControl;
  if AControlName <> '' then
    Result.Name := GenerateUniqueComponentName(AOwner, Result, AControlName);
end;

function TJvCustomDynControlEngine.GetControlTextWidth(aControl: TControl;
    aFont: TFont; const aText: string): Integer;
var
  Canvas: TControlCanvas;
begin
  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := aControl;
    Canvas.Font.Assign(aFont);
    Result := Canvas.TextWidth(aText);
  finally
    Canvas.free;
  end;
end;

procedure TJvCustomDynControlEngine.SetControlCaption(AControl: IJvDynControl; const Value: string);
begin
end;

procedure TJvCustomDynControlEngine.SetControlTabOrder(AControl: IJvDynControl; Value: Integer);
begin
end;

procedure TJvCustomDynControlEngine.SetControlOnEnter(AControl: IJvDynControl; Value: TNotifyEvent);
begin
end;

procedure TJvCustomDynControlEngine.SetControlOnExit(AControl: IJvDynControl; Value: TNotifyEvent);
begin
end;

procedure TJvCustomDynControlEngine.SetControlOnClick(AControl: IJvDynControl; Value: TNotifyEvent);
begin
end;

procedure TJvCustomDynControlEngine.NeedRegisterControls;
begin
  if not FRegisterControlsExecuted then
  begin
    FRegisterControlsExecuted := True;
    RegisterControls;
  end;
end;

procedure TJvCustomDynControlEngine.RegisterControls;
begin
  // no registration
end;

//=== { TJvDynControlEngine } ================================================

constructor TJvDynControlEngine.Create;
begin
  inherited Create;
  FDistanceBetweenLabelAndControlHorz := 4;
  FDistanceBetweenLabelAndControlVert := 1;
end;

function TJvDynControlEngine.CreateButton(AOwner: TComponent;
  AParentControl: TWinControl; const AButtonName, ACaption, AHint: string;
  AOnClick: TNotifyEvent; ADefault: Boolean = False; ACancel: Boolean = False): TButton;
begin
  Result := TButton(CreateControl(jctButton, AOwner, AParentControl, AButtonName));
  Result.Hint := AHint;
  Result.Caption := ACaption;
  Result.Default := ADefault;
  Result.Cancel := ACancel;
  Result.OnClick := AOnClick;
  Result.Width := GetControlTextWidth(Result, Result.Font, ACaption+'XXXX');
end;

function TJvDynControlEngine.CreateButtonEditControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string; AOnButtonClick: TNotifyEvent): TWinControl;
var
  DynCtrlButtonEdit: IJvDynControlButtonEdit;
begin
  Result := TWinControl(CreateControl(jctButtonEdit, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlButtonEdit, DynCtrlButtonEdit);
  DynCtrlButtonEdit.ControlSetOnButtonClick(AOnButtonClick);
end;

function TJvDynControlEngine.CreateCalculateControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctCalculateEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateCheckboxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string): TWinControl;
var
  DynCtrlCaption: IJvDynControlCaption;
  DynCtrlFont: IJvDynControlFont;
begin
  Result := TWinControl(CreateControl(jctCheckBox, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlCaption, DynCtrlCaption);
  DynCtrlCaption.ControlSetCaption(ACaption);
  if Supports(Result, IJvDynControlFont,DynCtrlFont) then
    Result.Width := GetControlTextWidth(Result, DynCtrlFont.ControlFont, ACaption+'XXXXXX');
end;

function TJvDynControlEngine.CreateCheckComboBoxControl(AOwner: TComponent; AParentControl: TWinControl; const
    AControlName: string; AItems: TStrings; ADelimiter: string): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
  DynControlCheckComboBox: IJvDynControlCheckComboBox;
begin
  Result := TWinControl(CreateControl(jctCheckComboBox, AOwner, AParentControl, AControlName));
  if Assigned(AItems) then
  begin
    IntfCast(Result, IJvDynControlItems, DynCtrlItems);
    DynCtrlItems.ControlSetItems(AItems);
    IntfCast(Result, IJvDynControlCheckComboBox, DynControlCheckComboBox);
    DynControlCheckComboBox.Delimiter := ADelimiter;
  end;
end;

function TJvDynControlEngine.CreateCheckListBoxControl(AOwner: TComponent; AParentControl: TWinControl; const
    AControlName: string; AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateControl(jctCheckListBox, AOwner, AParentControl, AControlName));
  if Assigned(AItems) then
  begin
    IntfCast(Result, IJvDynControlItems, DynCtrlItems);
    DynCtrlItems.ControlSetItems(AItems);
  end;
end;

function TJvDynControlEngine.CreateColorComboboxControl(AOwner: TComponent;
    AParentControl: TWinControl; const AControlName: string; ADefaultColor:
    TColor): TWinControl;
var
  DynControlColorComboBoxControl : IJvDynControlColorComboBoxControl;
begin
  Result := TWinControl(CreateControl(jctColorComboBox, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlColorComboBoxControl, DynControlColorComboBoxControl);
  DynControlColorComboBoxControl.ControlDefaultColor := ADefaultColor;
  DynControlColorComboBoxControl.ControlSelectedColor := ADefaultColor;
end;

function TJvDynControlEngine.CreateComboBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateControl(jctComboBox, AOwner, AParentControl, AControlName));
  if Assigned(AItems) then
  begin
    IntfCast(Result, IJvDynControlItems, DynCtrlItems);
    DynCtrlItems.ControlSetItems(AItems);
  end;
end;

function TJvDynControlEngine.CreateDateControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctDateEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateDateTimeControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctDateTimeEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateDirectoryControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctDirectoryEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateEditControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  Result := TWinControl(CreateControl(jctEdit, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlEdit, DynCtrlEdit);
end;

function TJvDynControlEngine.CreateFileNameControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctFileNameEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateForm(const ACaption, AHint: string): TCustomForm;
begin
  Result := TCustomForm(CreateControl(jctForm, Application, nil, ''));
  Result.Caption := ACaption;
  Result.Hint := AHint;
end;

function TJvDynControlEngine.CreateGroupBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string): TWinControl;
var
  DynCtrlCaption: IJvDynControlCaption;
begin
  Result := TWinControl(CreateControl(jctGroupBox, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlCaption, DynCtrlCaption);
  DynCtrlCaption.ControlSetCaption(ACaption);
end;

function TJvDynControlEngine.CreateImageControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctImage, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateLabelControl(AOwner: TComponent;
    AParentControl: TWinControl; const AControlName, ACaption: string;
    AFocusControl: TWinControl = nil): TControl;
var
  DynCtrlCaption: IJvDynControlCaption;
  DynCtrlLabel: IJvDynControlLabel;
begin
  Result := CreateControl(jctLabel, AOwner, AParentControl, AControlName);
  IntfCast(Result, IJvDynControlCaption, DynCtrlCaption);
  DynCtrlCaption.ControlSetCaption(ACaption);
  IntfCast(Result, IJvDynControlLabel, DynCtrlLabel);
  if Assigned(AFocusControl) then
    DynCtrlLabel.ControlSetFocusControl(AFocusControl);
end;

function TJvDynControlEngine.CreateLabelControlPanel(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string; AFocusControl: TWinControl;
  ALabelOnTop: Boolean = True; ALabelDefaultWidth: Integer = 0): TWinControl;
var
  Panel: TWinControl;
  LabelControl: TControl;
begin
  if not Assigned(AFocusControl) then
    raise EJVCLException.CreateRes(@RsENoFocusControl);
  Panel := CreatePanelControl(AOwner, AParentControl, '', '', alNone);
  LabelControl := CreateLabelControl(AOwner, Panel, '', ACaption, AFocusControl);
  AFocusControl.Parent := Panel;
  LabelControl.Top := 1;
  LabelControl.Left := 1;
  if ALabelOnTop then
  begin
    AFocusControl.Top := LabelControl.Height + DistanceBetweenLabelAndControlVert;
    AFocusControl.Left := 1;
    if LabelControl.Width > AFocusControl.Width then
      Panel.Width := LabelControl.Width
    else
      Panel.Width := AFocusControl.Width;
    Panel.Height := AFocusControl.Top + AFocusControl.Height;
  end
  else
  begin
    if ALabelDefaultWidth > 0 then
      LabelControl.Width := ALabelDefaultWidth;
    AFocusControl.Left := LabelControl.Width + DistanceBetweenLabelAndControlHorz;
    AFocusControl.Top := 1;
    if LabelControl.Height > AFocusControl.Height then
      Panel.Height := LabelControl.Height
    else
      Panel.Height := AFocusControl.Height;
    Panel.Width := AFocusControl.Width + AFocusControl.Left;
  end;
  Panel.Width := Panel.Width + 1;
  Panel.Height := Panel.Height + 1;
  Result := Panel;
end;

function TJvDynControlEngine.CreateListBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateControl(jctListBox, AOwner, AParentControl, AControlName));
  if Assigned(AItems) then
  begin
    IntfCast(Result, IJvDynControlItems, DynCtrlItems);
    DynCtrlItems.ControlSetItems(AItems);
  end;
end;

function TJvDynControlEngine.CreateMemoControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
var
  DynCtrlData: IJvDynControlData;
begin
  Result := TWinControl(CreateControl(jctMemo, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlData, DynCtrlData);
  DynCtrlData.ControlValue := '';
end;

function TJvDynControlEngine.CreatePageControlControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string;APages : TStrings): TWinControl;
var
  DynTabControl: IJvDynControlTabControl;
  i: Integer;
begin
  Result := TWinControl(CreateControl(jctPageControl, AOwner, AParentControl, AControlName));
  if Assigned(APages) and (APages.Count > 0) then
  begin
    IntfCast(Result, IJvDynControlTabControl, DynTabControl);
    for i := 0 to APages.Count - 1 do
      DynTabControl.ControlCreateTab(APages[i]);
  end;
end;

function TJvDynControlEngine.CreatePanelControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string;
  AAlign: TAlign): TWinControl;
var
  DynCtrlCaption: IJvDynControlCaption;
begin
  Result := TWinControl(CreateControl(jctPanel, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlCaption, DynCtrlCaption);
  DynCtrlCaption.ControlSetCaption(ACaption);
  Result.Align := AAlign;
end;

function TJvDynControlEngine.CreateProgressbarControl(AOwner: TComponent;
    AParentControl: TWinControl; const AControlName: string; AMin: Integer = 0;
    AMax: Integer = 100; AStep: Integer = 1): TWinControl;
var
  JvDynCtrlProgresBar: IJvDynControlProgressbar;
begin
  Result := TWinControl(CreateControl(jctProgressBar, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlProgressbar, JvDynCtrlProgresBar);
  JvDynCtrlProgresBar.ControlSetMin(AMin);
  JvDynCtrlProgresBar.ControlSetMax(AMax);
  JvDynCtrlProgresBar.ControlSetStep(AStep);
end;

function TJvDynControlEngine.CreateRadioButton(AOwner: TComponent; AParentControl: TWinControl;
  const ARadioButtonName, ACaption: string): TWinControl;
var
  DynCtrlCaption: IJvDynControlCaption;
begin
  Result := TWinControl(CreateControl(jctRadioButton, AOwner, AParentControl, ARadioButtonName));
  IntfCast(Result, IJvDynControlCaption, DynCtrlCaption);
  DynCtrlCaption.ControlSetCaption(ACaption);
end;

function TJvDynControlEngine.CreateRadioGroupControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string;
  AItems: TStrings; AItemIndex: Integer = 0): TWinControl;
var
  DynCtrlCaption: IJvDynControlCaption;
  DynCtrlItems: IJvDynControlItems;
  DynCtrlData: IJvDynControlData;
begin
  Result := TWinControl(CreateControl(jctRadioGroup, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlCaption, DynCtrlCaption);
  DynCtrlCaption.ControlSetCaption(ACaption);
  IntfCast(Result, IJvDynControlItems, DynCtrlItems);
  DynCtrlItems.ControlSetItems(AItems);
  IntfCast(Result, IJvDynControlData, DynCtrlData);
  DynCtrlData.ControlValue := AItemIndex;
end;

function TJvDynControlEngine.CreateRichEditControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctRichEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateRTTIInspectorControl(AOwner: TComponent;
    AParentControl: TWinControl; const AControlName: string;
    AOnDisplayProperty:
    TJvDynControlInspectorControlOnDisplayPropertyEvent;
    AOnTranslatePropertyName:
    TJvDynControlInspectorControlOnTranslatePropertyNameEvent): TWinControl;
var
  RTTIInspectorControl : IJvDynControlRTTIInspectorControl;
begin
  Result := TWinControl(CreateControl(jctRTTIInspector, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlRTTIInspectorControl, RTTIInspectorControl);
  RTTIInspectorControl.ControlOnDisplayProperty := AOnDisplayProperty;
  RTTIInspectorControl.ControlOnTranslatePropertyName := AOnTranslatePropertyName;
end;

function TJvDynControlEngine.CreateScrollBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctScrollBox, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateSpinControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctSpinEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateStaticTextControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName, ACaption: string): TWinControl;
var
  DynCtrlCaption: IJvDynControlCaption;
begin
  Result := TWinControl(CreateControl(jctStaticText, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlCaption, DynCtrlCaption);
  DynCtrlCaption.ControlSetCaption(ACaption);
end;

function TJvDynControlEngine.CreateStringGridControl(AOwner: TComponent; AParentControl: TWinControl; const
    AControlName: string; AColCount, ARowCount: Integer; AFixedCols: Integer = 1; AFixedRows: Integer = 1): TWinControl;
var
  DynCtrlStringGrid: IJvDynControlStringGrid;
begin
  Result := TWinControl(CreateControl(jctStringGrid, AOwner, AParentControl, AControlName));
  IntfCast(Result, IJvDynControlStringGrid, DynCtrlStringGrid);
  DynCtrlStringGrid.ControlColCount := AColCount;
  DynCtrlStringGrid.ControlFixedCols:= AFixedCols;
  DynCtrlStringGrid.ControlRowCount := ARowCount;
  DynCtrlStringGrid.ControlFixedRows:= AFixedRows;
end;

function TJvDynControlEngine.CreateTabControlControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string;ATabs : TStrings): TWinControl;
var
  DynTabControl: IJvDynControlTabControl;
  i: Integer;
begin
  Result := TWinControl(CreateControl(jctTabControl, AOwner, AParentControl, AControlName));
  if Assigned(ATabs) and (ATabs.Count > 0) then
  begin
    IntfCast(Result, IJvDynControlTabControl, DynTabControl);
    for i := 0 to ATabs.Count - 1 do
      DynTabControl.ControlCreateTab(ATabs[i]);
  end;
end;

function TJvDynControlEngine.CreateTimeControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctTimeEdit, AOwner, AParentControl, AControlName));
end;

function TJvDynControlEngine.CreateTreeViewControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string): TWinControl;
begin
  Result := TWinControl(CreateControl(jctTreeView, AOwner, AParentControl, AControlName));
end;


procedure SetDefaultDynControlEngine(AEngine: TJvDynControlEngine);
begin
  if AEngine is TJvDynControlEngine then
    GlobalDefaultDynControlEngine := AEngine;
end;

function DefaultDynControlEngine: TJvDynControlEngine;
begin
  Assert(Assigned(GlobalDefaultDynControlEngine),'JvDynControlEngine: DefaultDynControlEngine not defined');
  Result := GlobalDefaultDynControlEngine;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
