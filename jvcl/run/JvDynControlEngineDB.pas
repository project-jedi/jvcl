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
    FDynControlEngine: TJvDynControlEngine;
  protected
    function GetFieldControlType(AField: TField): TJvDynControlType; virtual;
    function GetDynControlEngine: TJvDynControlEngine;
    procedure SetDynControlEngine (ADynControlEngine: TJvDynControlEngine);
  public
    function IsControlTypeValid(const ADynControlType: TJvDynControlType;
      AControlClass: TControlClass): Boolean; override;
    function CreateDBFieldControl(AField: TField; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string; ADataSource: TDataSource): TWinControl; virtual;
    function CreateDBControl(AControlType: TJvDynControlType; AOwner: TComponent;
      AParentControl: TWinControl; AControlName: string;
      ADataSource: TDataSource; const ADataField: string): TControl; virtual;
    function CreateDBTextControl(AOwner: TComponent;
      AParentControl: TWinControl; const AControlName: string;
      ADataSource: TDataSource; const ADataField: string; const ACaption: string): TWinControl;
    function CreateDBEditControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl; virtual;
    function CreateDBCheckboxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource;
      const ADataField, ACaption: string): TWinControl; virtual;
    function CreateDBComboBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string;
      AItems: TStrings): TWinControl; virtual;
    function CreateDBImageControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl; virtual;
    function CreateDBRadioGroupControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField, ACaption: string;
      AItems: TStrings): TWinControl; virtual;
    function CreateDBMemoControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl; virtual;
    function CreateDBListBoxControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string;
      AItems: TStrings): TWinControl; virtual;
    function CreateDBDateTimeControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl; virtual;
    function CreateDBDateControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl; virtual;
    function CreateDBTimeControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl; virtual;
    function CreateDBCalculateControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl; virtual;
    function CreateDBSpinControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl; virtual;
    function CreateDBDirectoryControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl;
    function CreateDBFileNameControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl;
    function CreateDBGridControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource): TWinControl; virtual;
    function CreateDBNavigatorControl(AOwner: TComponent; AParentControl: TWinControl;
      const AControlName: string; ADataSource: TDataSource): TWinControl; virtual;
    procedure CreateControlsFromDatasourceOnControl (ADataSource: TDataSource;
      AControl: TWinControl; AShowInvisibleFields: Boolean = False; ALabelOnTop: Boolean = True;
      ALabelDefaultWidth: Integer = 0; AFieldDefaultWidth: Integer = 0;
      AMaxFieldWidth: Integer = 300; AFieldSizeStep: Integer = 250);
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write SetDynControlEngine;
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

procedure TJvDynControlEngineDB.SetDynControlEngine (ADynControlEngine: TJvDynControlEngine);
begin
  FDynControlEngine := ADynControlEngine;
end;

function TJvDynControlEngineDB.GetDynControlEngine: TJvDynControlEngine;
begin
  if Assigned(FDynControlEngine) then
    Result := FDynControlEngine
  else
    Result := DefaultDynControlEngine;
end;

function TJvDynControlEngineDB.IsControlTypeValid(const ADynControlType: TJvDynControlType;
  AControlClass: TControlClass): Boolean;
var
  Valid: Boolean;
begin
  Valid := inherited IsControlTypeValid(ADynControlType, AControlClass);
  case ADynControlType of
//    jctDBText:
//      Valid := Valid and Supports(AControlClass, IJvDynControlLabel);
    jctDBButtonEdit:
      Valid := Valid and
        Supports(AControlClass, IJvDynControlButton) and
        Supports(AControlClass, IJvDynControlData);
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
  if ADynControlType in [jctDBEdit, jctDBText, jctDBListBox, jctDBComboBox, jctDBImage,
    jctDBRadioGroup, jctDBMemo, jctDBDateTimeEdit, jctDBDateEdit, jctDBTimeEdit,
    jctDBCalculateEdit, jctDBSpinEdit, jctDBDirectoryEdit, jctDBFileNameEdit, jctDBGrid] then
    Valid := Valid and Supports(AControlClass, IJvDynControlDataBase);
  Result := Valid;
end;

function TJvDynControlEngineDB.GetFieldControlType(AField: TField): TJvDynControlType;
begin
  if not Assigned(AField) then
    raise EJVCLException.CreateRes(@RsEUnassignedField);
  case AField.Datatype of
    ftMemo:
      Result := jctDBMemo;
    ftGraphic:
      Result := jctDBImage;
    ftString:
      Result := jctDBEdit;
    ftDate:
      Result := jctDBDateEdit;
    ftTime:
      Result := jctDBTimeEdit;
    ftDateTime:
      Result := jctDBDateTimeEdit;
    ftBoolean:
      Result := jctDBCheckBox;
  else
    Result := jctDBEdit;
  end;
end;

function TJvDynControlEngineDB.CreateDBFieldControl(AField: TField; AOwner: TComponent;
  AParentControl: TWinControl; AControlName: string; ADataSource: TDataSource): TWinControl;
begin
  Result:= TWinControl(CreateDBControl(GetFieldControlType(AField), AOwner, AParentControl,
    AControlName, ADataSource, AField.FieldName));
end;

function TJvDynControlEngineDB.CreateDBControl(AControlType: TJvDynControlType;
  AOwner: TComponent; AParentControl: TWinControl; AControlName: string;
  ADataSource: TDataSource; const ADataField: string): TControl;
var
  DynCtrl: IJvDynControlDatabase;
begin
  Result := CreateControl(AControlType, AOwner, AParentControl, AControlName);
  if not Supports(Result, IJvDynControlDatabase, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetDatasource(ADataSource);
  DynCtrl.ControlSetDatafield(ADataField);
end;


function TJvDynControlEngineDB.CreateDBTextControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string;
  ADataSource: TDataSource; const ADataField: string; const ACaption: string): TWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TWinControl(CreateDBControl(jctDBText, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
end;

function TJvDynControlEngineDB.CreateDBEditControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl;
var
  DynCtrlEdit: IJvDynControlEdit;
begin
  Result := TWinControl(CreateDBControl(jctDBEdit, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
  if not Supports(Result, IJvDynControlEdit, DynCtrlEdit) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
end;

function TJvDynControlEngineDB.CreateDBCheckboxControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string; ADataSource: TDataSource; const ADataField, ACaption: string): TWinControl;
var
  DynCtrl: IJvDynControl;
begin
  Result := TWinControl(CreateDBControl(jctDBCheckBox, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
end;

function TJvDynControlEngineDB.CreateDBComboBoxControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string; ADataSource: TDataSource; const ADataField: string;
  AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateDBControl(jctDBComboBox, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
end;

function TJvDynControlEngineDB.CreateDBImageControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBImage, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBRadioGroupControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource;
  const ADataField, ACaption: string; AItems: TStrings): TWinControl;
var
  DynCtrl: IJvDynControl;
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateDBControl(jctDBRadioGroup, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
  if not Supports(Result, IJvDynControl, DynCtrl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrl.ControlSetCaption(ACaption);
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
end;

function TJvDynControlEngineDB.CreateDBMemoControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string; ADataSource: TDataSource; const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBMemo, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBListBoxControl(AOwner: TComponent; AParentControl: TWinControl;
  const AControlName: string; ADataSource: TDataSource; const ADataField: string;
  AItems: TStrings): TWinControl;
var
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateDBControl(jctDBListBox, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
  if not Supports(Result, IJvDynControlItems, DynCtrlItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  DynCtrlItems.ControlSetItems(AItems);
end;

function TJvDynControlEngineDB.CreateDBDateTimeControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource;
  const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBDateTimeEdit, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBDateControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource;
  const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBDateEdit, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBTimeControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource;
  const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBTimeEdit, AOwner, AParentControl,
  AControlName, ADataSource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBCalculateControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource;
  const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBCalculateEdit, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBSpinControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource;
  const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBSpinEdit, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBDirectoryControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource;
  const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBDirectoryEdit, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBFileNameControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource;
  const ADataField: string): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBFileNameEdit, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
end;

function TJvDynControlEngineDB.CreateDBGridControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBGrid, AOwner, AParentControl,
    AControlName, ADataSource, ''));
end;

function TJvDynControlEngineDB.CreateDBNavigatorControl(AOwner: TComponent;
  AParentControl: TWinControl; const AControlName: string; ADataSource: TDataSource): TWinControl;
begin
  Result := TWinControl(CreateDBControl(jctDBNavigator, AOwner, AParentControl,
    AControlName, ADataSource, ''));
end;

type
  TAccessCustomControl = class(TCustomControl);

procedure TJvDynControlEngineDB.CreateControlsFromDatasourceOnControl(ADataSource: TDataSource;
  AControl: TWinControl; AShowInvisibleFields: Boolean = False; ALabelOnTop: Boolean = True;
  ALabelDefaultWidth: Integer = 0; AFieldDefaultWidth: Integer = 0; AMaxFieldWidth: Integer = 300;
  AFieldSizeStep: Integer = 250);
var
  I: Integer;
  Control: TWinControl;
  LabelControl: TWinControl;
begin
  if not Assigned(ADataSource) or not Assigned(ADataSource.DataSet) or not Assigned(AControl) then
    raise EJVCLException.CreateRes(@RsEUnassignedMultiple);
  if not ADataSource.DataSet.Active then
    raise EJVCLException.CreateRes(@RsEUnassignedDataSet);
  for I := 0 to ADataSource.DataSet.FieldCount -1 do
    if ADataSource.DataSet.Fields[I].Visible or AShowInvisibleFields then
    begin
      Control := CreateDBFieldControl(ADataSource.DataSet.Fields[I], AControl, AControl, '', ADataSource);
      if AFieldDefaultWidth > 0 then
        Control.Width := AFieldDefaultWidth
      else
      begin
        if ADataSource.DataSet.Fields[I].Size > 0 then
          Control.Width :=
            TAccessCustomControl(AControl).Canvas.TextWidth(' ') * ADataSource.DataSet.Fields[I].Size;
        if (AMaxFieldWidth > 0) and (Control.Width > AMaxFieldWidth) then
          Control.Width := AMaxFieldWidth;
      end;
      LabelControl := GetDynControlEngine.CreateLabelControlPanel(AControl, AControl,
        '', '&' + ADataSource.DataSet.Fields[I].DisplayLabel, Control, ALabelOnTop, ALabelDefaultWidth);
      if (AFieldSizeStep > 0) then
        if ((LabelControl.Width mod AFieldSizeStep) <> 0) then
          LabelControl.Width := ((LabelControl.Width div AFieldSizeStep) + 1) * AFieldSizeStep;
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

