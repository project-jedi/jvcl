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

unit JvDynControlEngineDB;

{$I jvcl.inc}

interface

uses
  Classes, Controls, DB,
  JvDynControlEngine, JvDynControlEngineDBIntf;

type
  TJvDynControlEngineDB = class(TJvCustomDynControlEngine)
  private
    fDynControlEngine : TJvDynControlEngine;
  protected
    function IsControlTypeValid (const ADynControlType: TJvDynControlType;
      AControlClass: TControlClass) : Boolean; Override;
    function GetFieldControlType(AField : TField): TJvDynControlType; virtual;
    function GetDynControlEngine : TJvDynControlEngine;
    procedure SetDynControlEngine (ADynControlEngine : TJvDynControlEngine);
  public
    function CreateDBFieldControl(AField : TField; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string; ADatasource: TDatasource): TWinControl; virtual;

    function CreateDBControl(AControlType: TJvDynControlType; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string;
      ADatasource: TDatasource; const ADataField: string): TControl; virtual;

    function CreateDBTextControl(AOwner: TComponent;
      AParentControl: TWinControl; const AControlName : string;
      ADatasource: TDatasource; const ADataField: string; const ACaption: string): TWinControl;
    function CreateDBEditControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl; virtual;
    function CreateDBCheckboxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField, ACaption: string): TWinControl; virtual;
    function CreateDBComboBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string; AItems: TStrings): TWinControl; virtual;
    function CreateDBImageControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl; virtual;
    function CreateDBRadioGroupControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField, ACaption: string; AItems: TStrings): TWinControl; virtual;
    function CreateDBMemoControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl; virtual;
    function CreateDBListBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string; AItems: TStrings): TWinControl; virtual;
    function CreateDBDateTimeControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl; virtual;
    function CreateDBDateControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl; virtual;
    function CreateDBTimeControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl; virtual;
    function CreateDBCalculateControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl; virtual;
    function CreateDBSpinControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl; virtual;
    function CreateDBDirectoryControl(AOwner: TComponent;
      AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
    function CreateDBFileNameControl(AOwner: TComponent;
      AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
    function CreateDBGridControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource): TWinControl; virtual;
    function CreateDBNavigatorControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource): TWinControl; virtual;

    procedure CreateControlsFromDatasourceOnControl (ADatasource: TDatasource;
      AControl: TWinControl; AShowInvisibleFields : Boolean = False;
      ALabelOnTop : Boolean = True;
      ALabelDefaultWidth : Integer = 0; AFieldDefaultWidth : Integer = 0;
      AMaxFieldWidth : Integer = 300; AFieldSizeStep : Integer = 250);

    property DynControlEngine : TJvDynControlEngine read GetDynControlEngine write SetDynControlEngine;
  published
  end;

procedure SetDefaultDynControlEngineDB(AEngine: TJvDynControlEngineDB);
function DefaultDynControlEngineDB: TJvDynControlEngineDB;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils, TypInfo,
  JvResources, JvTypes, JvDynControlEngineIntf, JvDynControlEngineVCLDB;

var
  GlobalDefaultDynControlEngineDB: TJvDynControlEngineDB = nil;

procedure TJvDynControlEngineDB.SetDynControlEngine (ADynControlEngine : TJvDynControlEngine);
begin
  fDynControlEngine := ADynControlEngine;
end;

function TJvDynControlEngineDB.GetDynControlEngine : TJvDynControlEngine;
begin
  if Assigned (fDynControlEngine) then
    Result := fDynControlEngine
  else
    Result := DefaultDynControlEngine;
end;

function TJvDynControlEngineDB.IsControlTypeValid (const ADynControlType: TJvDynControlType;
      AControlClass: TControlClass) : Boolean;
var
  Valid: Boolean;
begin
  Valid := Inherited IsControlTypeValid (ADynControlType, AControlClass);
  case ADynControlType of
//    jctDBText:
//      Valid := Valid and Supports(AControlClass, IJvDynControlLabel);
    jctDBButtonEdit:
      Valid := Valid and Supports(AControlClass, IJvDynControlButton)
                     and Supports(AControlClass, IJvDynControlData);
    jctDBMemo:
      Valid := Valid and
        Supports(AControlClass, IJvDynControlItems) and
        Supports(AControlClass, IJvDynControlData) and
        Supports(AControlClass, IJvDynControlMemo);
    jctDBRadioGroup, jctDBComboBox:
      Valid := Valid and
        Supports(AControlClass, IJvDynControlItems) and
        Supports(AControlClass, IJvDynControlData);
    jctDBEdit, jctDBCalculateEdit, jctDBSpinEdit,
      jctDBCheckBox, jctDBDateTimeEdit, jctDBDateEdit, jctDBTimeEdit,
      jctDBDirectoryEdit, jctDBFileNameEdit :
      Valid := Valid and Supports(AControlClass, IJvDynControlData);
  end;
  if ADynControlType IN [
    jctDBEdit, jctDBText, jctDBListBox, jctDBComboBox, jctDBImage, jctDBRadioGroup,
    jctDBMemo, jctDBDateTimeEdit, jctDBDateEdit, jctDBTimeEdit,
    jctDBCalculateEdit, jctDBSpinEdit, jctDBDirectoryEdit, jctDBFileNameEdit, jctDBGrid] then
      Valid := Valid and Supports(AControlClass, IJvDynControlDataBase);
  Result := Valid;
end;

function TJvDynControlEngineDB.GetFieldControlType(AField : TField): TJvDynControlType;
begin
  if not Assigned(AField) then
    Raise Exception.Create ('TJvDynControlEngineDB.GetFieldControlType : AField must be assigned');
  Case AField.Datatype of
    ftMemo : Result := jctDBMemo;
    ftGraphic : Result := jctDBImage;
    ftString : Result := jctDBEdit;
    ftDate : Result := jctDBDateEdit;
    ftTime : Result := jctDBTimeEdit;
    ftDateTime : Result := jctDBDateTimeEdit;
    ftBoolean : Result := jctDBCheckBox;
  else
    Result := jctDBEdit;
  end;
end;

function TJvDynControlEngineDB.CreateDBFieldControl(AField : TField; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string; ADatasource: TDatasource): TWinControl;
begin
  Result:= TWinControl(CreateDBControl(GetFieldControlType(AField), AOwner, AParentControl, AControlName, ADatasource, AField.FieldName));
end;

function TJvDynControlEngineDB.CreateDBControl(AControlType: TJvDynControlType; AOwner: TComponent;
  AParentControl: TWinControl; AControlName: string;
  ADatasource: TDatasource; const ADataField: string): TControl;
var
  DynCtrl: IJvDynControlDatabase;
begin
  Result := CreateControl(AControlType, AOwner, AParentControl, AControlName);
  if not Supports(Result, IJvDynControlDatabase, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetDatasource (ADatasource);
  DynCtrl.ControlSetDatafield (ADataField);
end;


function TJvDynControlEngineDB.createDBTextControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName : string;
  ADatasource: TDatasource; const ADataField: string; const ACaption: string): TWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TWinControl(CreateDBControl(jctDBText, AOwner, AParentControl, AControlName, ADatasource, ADataField));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
end;

function TJvDynControlEngineDB.createDBEditControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  Result := TWinControl(CreateDBControl(jctDBEdit, AOwner, AParentControl, AControlName, ADatasource, ADataField));
  if not Supports(Result, IJvDynControlEdit, DynCtrlEdit) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
end;

function TJvDynControlEngineDB.createDBCheckboxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField, ACaption: string): TWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TWinControl(CreateDBControl(jctDBCheckBox, AOwner, AParentControl, AControlName, ADatasource, ADataField));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
end;

function TJvDynControlEngineDB.createDBComboBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string; AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateDBControl(jctDBComboBox, AOwner, AParentControl, AControlName, ADatasource, ADataField));
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
end;

function TJvDynControlEngineDB.createDBImageControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBImage, AOwner, AParentControl, AControlName, ADatasource, ADataField));
end;

function TJvDynControlEngineDB.createDBRadioGroupControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField, ACaption: string;
  AItems: TStrings): TWinControl;
var
  DynCtrl: IJvDynControl;
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateDBControl(jctDBRadioGroup, AOwner, AParentControl, AControlName, ADatasource, ADataField));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
end;

function TJvDynControlEngineDB.createDBMemoControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBMemo, AOwner, AParentControl, AControlName, ADatasource, ADataField));
end;

function TJvDynControlEngineDB.createDBListBoxControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string; AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateDBControl(jctDBListBox, AOwner, AParentControl, AControlName, ADatasource, ADataField));
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
end;

function TJvDynControlEngineDB.createDBDateTimeControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBDateTimeEdit, AOwner, AParentControl, AControlName, ADatasource, ADataField));
end;

function TJvDynControlEngineDB.createDBDateControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBDateEdit, AOwner, AParentControl, AControlName, ADatasource, ADataField));
end;

function TJvDynControlEngineDB.createDBTimeControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBTimeEdit, AOwner, AParentControl, AControlName, ADatasource, ADataField));
end;

function TJvDynControlEngineDB.createDBCalculateControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBCalculateEdit, AOwner, AParentControl, AControlName, ADatasource, ADataField));
end;

function TJvDynControlEngineDB.createDBSpinControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBSpinEdit, AOwner, AParentControl, AControlName, ADatasource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBDirectoryControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBDirectoryEdit, AOwner, AParentControl, AControlName, ADatasource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBFileNameControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADatasource: TDatasource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBFileNameEdit, AOwner, AParentControl, AControlName, ADatasource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBGridControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBGrid, AOwner, AParentControl, AControlName, ADatasource, ''));
end;

function TJvDynControlEngineDB.CreateDBNavigatorControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADatasource: TDatasource): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBNavigator, AOwner, AParentControl, AControlName, ADatasource, ''));
end;

type
  tAccessCustomControl = Class(TCustomControl);

procedure TJvDynControlEngineDB.CreateControlsFromDatasourceOnControl (ADatasource: TDatasource;
      AControl: TWinControl; AShowInvisibleFields : Boolean = False; ALabelOnTop : Boolean = True; ALabelDefaultWidth : Integer = 0;
      AFieldDefaultWidth : Integer = 0; AMaxFieldWidth : Integer = 300; AFieldSizeStep : Integer = 250);
var
  i : Integer;
  Control: TWinControl;
  LabelControl: TWinControl;
begin
  If not Assigned(ADataSource) or
     not Assigned(ADatasource.Dataset) or
     not Assigned(AControl) then
    Raise Exception.Create('TJvDynControlEngineDB.CreateControlsFromDatasourceOnControl : ADatasource, ADatasource.Dataset and AControl must be assigned');
  if not ADatasource.Dataset.Active then
    Raise Exception.Create('TJvDynControlEngineDB.CreateControlsFromDatasourceOnControl : ADatasource.Dataset must be active');
  for i := 0 to ADatasource.Dataset.Fieldcount -1 do
    if ADatasource.Dataset.Fields[i].visible or AShowInvisibleFields then
    begin
      Control := CreateDBFieldControl(ADatasource.Dataset.Fields[i], AControl, AControl, '', ADatasource);
      if AFieldDefaultWidth > 0 then
        Control.Width := AFieldDefaultWidth
      else
      begin
        if ADatasource.Dataset.Fields[i].Size > 0 then
          Control.Width := tAccessCustomControl(AControl).Canvas.TextWidth(' ')*ADatasource.Dataset.Fields[i].Size;
        if (AMaxFieldWidth > 0) and (Control.Width > AMaxFieldWidth) then
          Control.Width := AMaxFieldWidth;
      end;
      LabelControl := GetDynControlEngine.CreateLabelControlPanel(
          AControl, AControl, '', '&'+ADatasource.Dataset.Fields[i].DisplayLabel,
          Control, ALabelOnTop, ALabelDefaultWidth);
      if (AFieldSizeStep > 0) then
        if ((LabelControl.Width Mod AFieldSizeStep) <> 0) then
          LabelControl.Width := ((LabelControl.Width Div AFieldSizeStep)+1)* AFieldSizeStep;
    end;
end;

procedure SetDefaultDynControlEngineDB(AEngine: TJvDynControlEngineDB);
begin
  if AEngine is TJvDynControlEngineDB then
    GlobalDefaultDynControlEngineDB := AEngine;
end;

function DefaultDynControlEngineDB: TJvDynControlEngineDB;
begin
  Result := GlobalDefaultDynControlEngineDB;
end;


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

