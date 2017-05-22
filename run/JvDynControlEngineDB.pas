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

unit JvDynControlEngineDB;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, Controls, DB,
  JvDynControlEngine, JvDynControlEngineDBIntf;

const
  jctDBEdit = TJvDynControlType('DBEdit');
  jctDBText = TJvDynControlType('DBText');
  jctDBListBox = TJvDynControlType('DBListBox');
  jctDBCheckBox = TJvDynControlType('DBCheckBox');
  jctDBComboBox = TJvDynControlType('DBComboBox');
  jctDBImage = TJvDynControlType('DBImage');
  jctDBRadioGroup = TJvDynControlType('DBRadioGroup');
  jctDBMemo = TJvDynControlType('DBMemo');
  jctDBDateTimeEdit = TJvDynControlType('DBDateTimeEdit');
  jctDBDateEdit = TJvDynControlType('DBDateEdit');
  jctDBTimeEdit = TJvDynControlType('DBTimeEdit');
  jctDBCalculateEdit = TJvDynControlType('DBCalculateEdit');
  jctDBSpinEdit = TJvDynControlType('DBSpinEdit');
  jctDBDirectoryEdit = TJvDynControlType('DBDirectoryEdit');
  jctDBFileNameEdit = TJvDynControlType('DBFileNameEdit');
  jctDBGrid = TJvDynControlType('DBGrid');
  jctDBButtonEdit = TJvDynControlType('DBButtonEdit');
  jctDBNavigator = TJvDynControlType('DBNavigator');

type
  TJvCreateDBFieldsOnControlOptions = class(TPersistent)
  private
    FShowInvisibleFields: Boolean ;
    FLabelOnTop: Boolean ;
    FLabelDefaultWidth: Integer ;
    FFieldDefaultWidth: Integer ;
    FFieldMinWidth: Integer ;
    FFieldMaxWidth: Integer ;
    FFieldWidthStep: Integer;
    FUseFieldSizeForWidth: Boolean;
    FUseParentColorForReadOnly: Boolean;
  protected
    procedure SetFieldWidthStep(Value: Integer);
  public
    constructor Create;
    procedure Assign (Source: TPersistent); override;
  published
    property ShowInvisibleFields: Boolean read FShowInvisibleFields write FShowInvisibleFields default False;
    property LabelOnTop: Boolean read FLabelOnTop write FLabelOnTop default True;
    property LabelDefaultWidth: Integer read FLabelDefaultWidth write FLabelDefaultWidth default 0;
    property FieldDefaultWidth: Integer read FFieldDefaultWidth write FFieldDefaultWidth default 0;
    property FieldMinWidth: Integer read FFieldMinWidth write FFieldMinWidth default 20;
    property FieldMaxWidth: Integer read FFieldMaxWidth write FFieldMaxWidth default 300;
    property FieldWidthStep: Integer read FFieldWidthStep write SetFieldWidthStep default 50;
    property UseFieldSizeForWidth: Boolean read FUseFieldSizeForWidth write FUseFieldSizeForWidth default True;
    property UseParentColorForReadOnly: Boolean read FUseParentColorForReadOnly write FUseParentColorForReadOnly default True;
  end;

  TJvDynControlEngineDB = class(TJvCustomDynControlEngine)
  private
    FDynControlEngine: TJvDynControlEngine;
  protected
    function GetDynControlEngine: TJvDynControlEngine;
    procedure SetDynControlEngine(ADynControlEngine: TJvDynControlEngine);
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
    function CreateControlsFromDataSourceOnControl(ADataSource: TDataSource;
      AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions): Boolean; virtual;
    function CreateControlsFromDataComponentOnControl(ADataComponent: TComponent;
      AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions): Boolean; virtual;
    function GetDataSourceFromDataComponent(ADataComponent: TComponent): TDataSource; virtual;
    function GetFieldControlType(AField: TField): TJvDynControlType; virtual;
    function SupportsDataComponent(ADataComponent: TComponent): Boolean;
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write SetDynControlEngine;
  end;

procedure SetDefaultDynControlEngineDB(AEngine: TJvDynControlEngineDB);
function DefaultDynControlEngineDB: TJvDynControlEngineDB;

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
  Variants, SysUtils, TypInfo,
  JvResources, JvTypes, JvDynControlEngineIntf;

var
  GlobalDefaultDynControlEngineDB: TJvDynControlEngineDB = nil;

//=== { TJvCreateDBFieldsOnControlOptions } ==================================

constructor TJvCreateDBFieldsOnControlOptions.Create;
begin
  inherited Create;
  FShowInvisibleFields := False;
  FLabelOnTop := True;
  FLabelDefaultWidth := 0;
  FFieldDefaultWidth := 0;
  FFieldMinWidth := 20;
  FFieldMaxWidth := 300;
  FFieldWidthStep := 50;
  FUseFieldSizeForWidth := True;
  FUseParentColorForReadOnly := True;
end;

procedure TJvCreateDBFieldsOnControlOptions.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TJvCreateDBFieldsOnControlOptions) then
  begin
    ShowInvisibleFields := TJvCreateDBFieldsOnControlOptions(Source).ShowInvisibleFields;
    LabelOnTop := TJvCreateDBFieldsOnControlOptions(Source).LabelOnTop;
    LabelDefaultWidth := TJvCreateDBFieldsOnControlOptions(Source).LabelDefaultWidth;
    FieldDefaultWidth := TJvCreateDBFieldsOnControlOptions(Source).FieldDefaultWidth;
    FieldMinWidth := TJvCreateDBFieldsOnControlOptions(Source).FieldMinWidth;
    FieldMaxWidth := TJvCreateDBFieldsOnControlOptions(Source).FieldMaxWidth;
    FieldWidthStep := TJvCreateDBFieldsOnControlOptions(Source).FieldWidthStep;
    UseFieldSizeForWidth  := TJvCreateDBFieldsOnControlOptions(Source).UseFieldSizeForWidth;
    UseParentColorForReadOnly := TJvCreateDBFieldsOnControlOptions(Source).UseParentColorForReadOnly;
  end
  else
    inherited Assign(Source);
end;

procedure TJvCreateDBFieldsOnControlOptions.SetFieldWidthStep(Value: Integer);
begin
  if Value < 1 then
    FFieldWidthStep := 1
  else
    FFieldWidthStep := Value;
end;

//=== { TJvDynControlEngineDB } ==============================================

procedure TJvDynControlEngineDB.SetDynControlEngine(ADynControlEngine: TJvDynControlEngine);
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
//  if ADynControlType = jctDBText then
//      Valid := Valid and Supports(AControlClass, IJvDynControlLabel)
//  else
  if ADynControlType = jctDBButtonEdit then
    Valid := Valid and
      Supports(AControlClass, IJvDynControlButton) and
      Supports(AControlClass, IJvDynControlData)
  else
  if ADynControlType = jctDBMemo then
    Valid := Valid and
      Supports(AControlClass, IJvDynControlItems) and
      Supports(AControlClass, IJvDynControlData) and
      Supports(AControlClass, IJvDynControlMemo)
  else
  if (ADynControlType = jctDBRadioGroup) or
    (ADynControlType = jctDBComboBox) then
    Valid := Valid and
      Supports(AControlClass, IJvDynControlItems) and
      Supports(AControlClass, IJvDynControlData)
  else
  if (ADynControlType = jctDBEdit) or
    (ADynControlType = jctDBCalculateEdit) or
    (ADynControlType = jctDBSpinEdit) or
    (ADynControlType = jctDBCheckBox) or
    (ADynControlType = jctDBDateTimeEdit) or
    (ADynControlType = jctDBDateEdit) or
    (ADynControlType = jctDBTimeEdit) or
    (ADynControlType = jctDBDirectoryEdit) or
    (ADynControlType = jctDBFileNameEdit) then
    Valid := Valid and Supports(AControlClass, IJvDynControlData);
  if (ADynControlType = jctDBEdit) or
    (ADynControlType = jctDBCalculateEdit) or
    (ADynControlType = jctDBSpinEdit) or
    (ADynControlType = jctDBCheckBox) or
    (ADynControlType = jctDBDateTimeEdit) or
    (ADynControlType = jctDBDateEdit) or
    (ADynControlType = jctDBTimeEdit) or
    (ADynControlType = jctDBDirectoryEdit) or
    (ADynControlType = jctDBFileNameEdit) or
    (ADynControlType = jctDBText) or
    (ADynControlType = jctDBListBox) or
    (ADynControlType = jctDBImage) or
    (ADynControlType = jctDBRadioGroup) or
    (ADynControlType = jctDBMemo) or
    (ADynControlType = jctDBGrid) then
    Valid := Valid and Supports(AControlClass, IJvDynControlDataBase);
  Result := Valid;
end;

function TJvDynControlEngineDB.GetFieldControlType(AField: TField): TJvDynControlType;
begin
  if not Assigned(AField) then
    raise EJVCLException.CreateRes(@RsEUnassignedField);
  case AField.DataType of
    ftOraClob, ftMemo, ftFmtMemo{$IFDEF COMPILER10_UP}, ftWideMemo{$ENDIF COMPILER10_UP}:
      Result := jctDBMemo;
    ftGraphic:
      Result := jctDBImage;
    ftString:
      Result := jctDBEdit;
    ftDate:
      Result := jctDBDateEdit;
    ftTime:
      Result := jctDBTimeEdit;
    ftDateTime, ftTimestamp 
      {$IFDEF COMPILER10_UP}, ftOraTimestamp{$ENDIF COMPILER10_UP}:
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
  Result := TWinControl(CreateDBControl(GetFieldControlType(AField), AOwner, AParentControl,
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
  DynCtrl: IJvDynControlCaption;
begin
  Result := TWinControl(CreateDBControl(jctDBText, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
  if not Supports(Result, IJvDynControlCaption, DynCtrl) then
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
  DynCtrl: IJvDynControlCaption;
begin
  Result := TWinControl(CreateDBControl(jctDBCheckBox, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
  if not Supports(Result, IJvDynControlCaption, DynCtrl) then
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
  DynCtrl: IJvDynControlCaption;
  DynCtrlItems: IJvDynControlItems;
begin
  Result := TWinControl(CreateDBControl(jctDBRadioGroup, AOwner, AParentControl,
    AControlName, ADataSource, ADataField));
  if not Supports(Result, IJvDynControlCaption, DynCtrl) then
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

function TJvDynControlEngineDB.CreateControlsFromDataSourceOnControl(ADataSource: TDataSource;
  AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions): Boolean;
var
  I: Integer;
  Control: TWinControl;
  LabelControl: TWinControl;
  CreateOptions: TJvCreateDBFieldsOnControlOptions;
begin
  //Result := False;
  if not Assigned(ADataSource) or not Assigned(ADataSource.DataSet) or not Assigned(AControl) then
    raise EJVCLException.CreateRes(@RsEUnassignedMultiple);
  if not ADataSource.DataSet.Active then
    raise EJVCLException.CreateRes(@RsEUnassignedDataSet);
  if not Assigned(AOptions) then
    CreateOptions := TJvCreateDBFieldsOnControlOptions.Create
  else
    CreateOptions := AOptions;
  try
    for I := 0 to ADataSource.DataSet.FieldCount - 1 do
      if ADataSource.DataSet.Fields[I].Visible or CreateOptions.ShowInvisibleFields then
      begin
        Control := CreateDBFieldControl(ADataSource.DataSet.Fields[I], AControl, AControl, '', ADataSource);
        if CreateOptions.FieldDefaultWidth > 0 then
          Control.Width := CreateOptions.FieldDefaultWidth
        else
        begin
          if CreateOptions.UseFieldSizeForWidth then
            if ADataSource.DataSet.Fields[I].Size > 0 then
              Control.Width :=
                TAccessCustomControl(AControl).Canvas.TextWidth('X') * ADataSource.DataSet.Fields[I].Size
            else
              if (GetFieldControlType(ADataSource.DataSet.Fields[I])= jctDBMemo) and (CreateOptions.FieldMaxWidth > 0) then
                Control.Width := CreateOptions.FieldMaxWidth
              else
          else
            if ADataSource.DataSet.Fields[I].DisplayWidth > 0 then
              Control.Width :=
                TAccessCustomControl(AControl).Canvas.TextWidth('X') * ADataSource.DataSet.Fields[I].DisplayWidth;
          if (CreateOptions.FieldMaxWidth > 0) and (Control.Width > CreateOptions.FieldMaxWidth) then
            Control.Width := CreateOptions.FieldMaxWidth
          else
          if (CreateOptions.FieldMinWidth > 0) and (Control.Width < CreateOptions.FieldMinWidth) then
            Control.Width := CreateOptions.FieldMinWidth
        end;

        if CreateOptions.UseParentColorForReadOnly then
          // Use ParentColor when the field is ReadOnly
          if not ADataSource.DataSet.CanModify or ADataSource.DataSet.Fields[I].ReadOnly then
            if isPublishedProp(Control, 'ParentColor') then
              SetOrdProp(Control, 'ParentColor', Ord(True));
        LabelControl := GetDynControlEngine.CreateLabelControlPanel(AControl, AControl,
          '', '&' + ADataSource.DataSet.Fields[I].DisplayLabel, Control, CreateOptions.LabelOnTop, CreateOptions.LabelDefaultWidth);
        if CreateOptions.FieldWidthStep > 0 then
          if (LabelControl.Width mod CreateOptions.FieldWidthStep) <> 0 then
            LabelControl.Width := ((LabelControl.Width div CreateOptions.FieldWidthStep) + 1) * CreateOptions.FieldWidthStep;
      end;
  finally
    if not Assigned(AOptions) then
      CreateOptions.Free;
  end;
  Result := True;
end;

function TJvDynControlEngineDB.GetDataSourceFromDataComponent(ADataComponent: TComponent): TDataSource;
begin
  if ADatacomponent is TDataSource then
    Result := TDataSource(ADataComponent)
  else
    Result := nil;
end;

function TJvDynControlEngineDB.SupportsDataComponent(ADataComponent: TComponent): Boolean;
begin
  Result := Assigned(ADataComponent) and Assigned(GetDataSourceFromDataComponent(ADataComponent));
end;

function TJvDynControlEngineDB.CreateControlsFromDataComponentOnControl(ADataComponent: TComponent;
  AControl: TWinControl; AOptions: TJvCreateDBFieldsOnControlOptions): Boolean;
var
  DS: TDataSource;
begin
  DS := GetDataSourceFromDataComponent(ADataComponent);
  if Assigned(DS) then
    Result := CreateControlsFromDataSourceOnControl(DS, AControl, AOptions)
  else
    Result := False;
end;

procedure SetDefaultDynControlEngineDB(AEngine: TJvDynControlEngineDB);
begin
  if AEngine is TJvDynControlEngineDB then
    GlobalDefaultDynControlEngineDB := AEngine;
end;

function DefaultDynControlEngineDB: TJvDynControlEngineDB;
begin
  Assert(Assigned(GlobalDefaultDynControlEngineDB),'JvDynControlEngineDB: DefaultDynControlEngineDB not definded');
  Result := GlobalDefaultDynControlEngineDB;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
