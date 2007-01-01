{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActionsEngine.Pas, released on 2004-12-30.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBActionsEngine;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, ImgList, Graphics,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows, QImgList, QGraphics,
  {$ENDIF UNIX}
  Forms, Controls, Classes, DB,
  DBGrids, JvPanel, JvDynControlEngineDB, JvDynControlEngineDBTools;

type
  TJvDatabaseActionBaseEngine = class(TComponent)
  private
    FDatacomponent: TComponent;
    procedure SetDatacomponent(const Value: TComponent); virtual;
  public
    function Supports(ADataComponent: TComponent): Boolean; virtual;
    property Datacomponent: TComponent read FDatacomponent write SetDatacomponent;
  end;

  TJvDatabaseActionBaseDatasetEngine = class(TJvDatabaseActionBaseEngine)
  private
    FDataset: TDataset;
    procedure SetDatacomponent(const Value: TComponent); override;
  public
    function GetSQL: string; virtual;
    procedure SetSQL(const Value: string); virtual;
    function Supports(ADataComponent: TComponent): Boolean; override;
    function SupportsGetSQL: Boolean; virtual;
    function SupportsSetSQL: Boolean; virtual;
    property Dataset: TDataset read FDataset;
    property SQL: string read GetSQL write SetSQL;
  end;

  TJvShowSingleRecordWindowOptions = class(TPersistent)
  private
    FDialogCaption: string;
    FPostButtonCaption: string;
    FCancelButtonCaption: string;
    FCloseButtonCaption: string;
    FBorderStyle: TFormBorderStyle;
    FPosition: TPosition;
    FTop: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FArrangeConstraints: TSizeConstraints;
    FArrangeSettings: TJvArrangeSettings;
    FFieldCreateOptions: TJvCreateDBFieldsOnControlOptions;
  protected
    procedure SetArrangeSettings(Value: TJvArrangeSettings);
    procedure SetArrangeConstraints(Value: TSizeConstraints);
    procedure SetFieldCreateOptions(Value: TJvCreateDBFieldsOnControlOptions);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetOptionsToDialog(ADialog: TJvDynControlDataSourceEditDialog);
  published
    property DialogCaption: string read FDialogCaption write FDialogCaption;
    property PostButtonCaption: string read FPostButtonCaption write FPostButtonCaption;
    property CancelButtonCaption: string read FCancelButtonCaption write FCancelButtonCaption;
    property CloseButtonCaption: string read FCloseButtonCaption write FCloseButtonCaption;
    property BorderStyle: TFormBorderStyle read FBorderStyle write FBorderStyle default bsDialog;
    property Position: TPosition read FPosition write FPosition default poScreenCenter;
    property Top: Integer read FTop write FTop default 0;
    property Left: Integer read FLeft write FLeft default 0;
    property Width: Integer read FWidth write FWidth default 640;
    property Height: Integer read FHeight write FHeight default 480;
    property ArrangeConstraints: TSizeConstraints read FArrangeConstraints write SetArrangeConstraints;
    property ArrangeSettings: TJvArrangeSettings read FArrangeSettings write SetArrangeSettings;
    property FieldCreateOptions: TJvCreateDBFieldsOnControlOptions read FFieldCreateOptions
      write SetFieldCreateOptions;
  end;
                           
  TJvDatabaseActionBaseControlEngine = class(TJvDatabaseActionBaseEngine)
  private
    FDataset: TDataset;
    FDataSource: TDataSource;
    function GetSelectedField: TField; virtual;
  protected
    function GetDataSource(ADataComponent: TComponent): TDataSource; virtual;
    function GetDataSet(ADataComponent: TComponent): TDataSet; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDatacomponent(const Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function Supports(ADataComponent: TComponent): Boolean; override;
    function IsActive: Boolean; virtual;
    function HasData: Boolean; virtual;
    function FieldCount: Integer; virtual;
    function RecordCount: Integer; virtual;
    function RecNo: Integer; virtual;
    function CanInsert: Boolean; virtual;
    function CanUpdate: Boolean; virtual;
    function CanDelete: Boolean; virtual;
    function EOF: Boolean; virtual;
    function Bof: Boolean; virtual;
    function CanRefresh: Boolean; virtual;
    function CanNavigate: Boolean; virtual;
    procedure DisableControls; virtual;
    procedure EnableControls; virtual;
    function ControlsDisabled: Boolean; virtual;
    function EditModeActive: Boolean; virtual;
    function FieldById(const FieldId: Integer): TField; virtual;
    function FieldByName(const FieldName: string): TField; virtual;
    procedure FillFieldList(var AFieldList: TStrings; const AOnlyVisible: Boolean); virtual;
    procedure First; virtual;
    function GotoSelectedRow(const ASelectedRow: Integer): Boolean; virtual;
    function IsFieldVisible(const AFieldName: string): Boolean; virtual;
    function IsFieldReadOnly(const AFieldName: string): Boolean; virtual;
    procedure Last; virtual;
    procedure MoveBy(Distance: Integer); virtual;
    function SelectedRowsCount: Integer; virtual;
    procedure ShowSingleRecordWindow(AOptions: TJvShowSingleRecordWindowOptions); virtual;
    property Dataset: TDataset read FDataset;
    property DataSource: TDataSource read FDataSource;
    property SelectedField: TField read GetSelectedField;
  end;

  TJvDatabaseActionBaseEngineClass = class of TJvDatabaseActionBaseEngine;

  TJvDatabaseActionDBGridControlEngine = class(TJvDatabaseActionBaseControlEngine)
  private
    FCustomDBGrid: TCustomDBGrid;
  protected
    function GetCustomDBGrid(ADataComponent: TComponent): TCustomDBGrid; virtual;
    function GetDataSource(ADataComponent: TComponent): TDataSource; override;
    procedure OnCreateDataControls(ADynControlEngineDB: TJvDynControlEngineDB;
      AParentControl: TWinControl; AFieldCreateOptions: TJvCreateDBFieldsOnControlOptions);
    procedure SetDatacomponent(const Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GotoSelectedRow(const ASelectedRow: Integer): Boolean; override;
    function GetSelectedField: TField; override;
    function SelectedRowsCount: Integer; override;
    function Supports(ADataComponent: TComponent): Boolean; override;
    procedure ShowSingleRecordWindow(AOptions: TJvShowSingleRecordWindowOptions); override;
    property CustomDBGrid: TCustomDBGrid read FCustomDBGrid;
  end;

  TJvDatabaseActionEngineList = class(TList)
  public
    destructor Destroy; override;
    procedure RegisterEngine(AEngineClass: TJvDatabaseActionBaseEngineClass);
    function GetControlEngine(AComponent: TComponent): TJvDatabaseActionBaseControlEngine;
    function GetDatasetEngine(AComponent: TComponent): TJvDatabaseActionBaseDatasetEngine;
    function Supports(AComponent: TComponent): Boolean;
    function SupportsDataset(AComponent: TComponent): Boolean;
  end;

procedure RegisterActionEngine(AEngineClass: TJvDatabaseActionBaseEngineClass);

function RegisteredDatabaseActionEngineList: TJvDatabaseActionEngineList;

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
  SysUtils, Grids, TypInfo,
  {$IFDEF HAS_UNIT_STRUTILS}
  StrUtils,
  {$ENDIF HAS_UNIT_STRUTILS}
  JvResources, JvParameterList, JvParameterListParameter, JvDSADialogs,
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  Dialogs;

var
  IntRegisteredActionEngineList: TJvDatabaseActionEngineList;

//=== { TJvDatabaseActionBaseControlEngine } =================================

constructor TJvDatabaseActionBaseControlEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataset := nil;
  FDataSource := nil;
  FDatacomponent := nil;
end;

function TJvDatabaseActionBaseControlEngine.GetDataSource(ADataComponent: TComponent): TDataSource;
begin
  if Assigned(ADataComponent) and (ADataComponent is TDataSource) then
    Result := TDataSource(ADataComponent)
  else
    Result := nil;
end;

function TJvDatabaseActionBaseControlEngine.GetDataSet(ADataComponent: TComponent): TDataSet;
begin
  if Assigned(GetDataSource(ADataComponent)) then
    Result := GetDataSource(ADataComponent).DataSet
  else
    Result := nil;
end;

function TJvDatabaseActionBaseControlEngine.Supports(ADataComponent: TComponent): Boolean;
begin
  Result := Assigned(ADataComponent) and (ADataComponent is TDataSource);
end;

function TJvDatabaseActionBaseControlEngine.IsActive: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.Active
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.HasData: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.RecordCount > 0
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.FieldCount: Integer;
begin
  if Assigned(DataSet) then
    Result := DataSet.FieldCount
  else
    Result := -1;
end;

function TJvDatabaseActionBaseControlEngine.RecordCount: Integer;
begin
  if Assigned(DataSet) then
    Result := DataSet.RecordCount
  else
    Result := -1;
end;

function TJvDatabaseActionBaseControlEngine.RecNo: Integer;
begin
  if Assigned(DataSet) then
    Result := DataSet.RecNo
  else
    Result := -1;
end;

function TJvDatabaseActionBaseControlEngine.CanInsert: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.CanModify
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.CanUpdate: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.CanModify
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.CanDelete: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.CanModify
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.EOF: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.EOF
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.Bof: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.Bof
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.CanRefresh: Boolean;
begin
  Result := Assigned(DataSet);
end;

function TJvDatabaseActionBaseControlEngine.CanNavigate: Boolean;
begin
  Result := Assigned(DataSet);
end;

procedure TJvDatabaseActionBaseControlEngine.DisableControls;
begin
  if Assigned(DataSet) then
    DataSet.DisableControls;
end;

procedure TJvDatabaseActionBaseControlEngine.EnableControls;
begin
  if Assigned(DataSet) then
    DataSet.EnableControls;
end;

function TJvDatabaseActionBaseControlEngine.ControlsDisabled: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.ControlsDisabled
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.EditModeActive: Boolean;
begin
  if Assigned(DataSet) then
    Result := DataSet.State in [dsInsert, dsEdit]
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.FieldById(const FieldId: Integer): TField;
begin
  if Assigned(Dataset) then
    Result := Dataset.Fields[FieldId]
  else
    Result := nil;
end;

function TJvDatabaseActionBaseControlEngine.FieldByName(const FieldName: string): TField;
begin
  if Assigned(Dataset) then
    Result := Dataset.FieldByName(FieldName)
  else
    Result := nil;
end;

procedure TJvDatabaseActionBaseControlEngine.FillFieldList(var AFieldList: TStrings;
  const AOnlyVisible: Boolean);
var
  I: Integer;
begin
  AFieldList.Clear;
  if Assigned(Dataset) then
  begin
    for I := 0 to DataSet.Fields.Count - 1 do
      if not AOnlyVisible or IsFieldVisible(DataSet.Fields[I].FieldName) then
        AFieldList.Add(DataSet.Fields[I].FieldName);
  end;
end;

procedure TJvDatabaseActionBaseControlEngine.First;
begin
  if Assigned(DataSet) then
    DataSet.First;
end;

function TJvDatabaseActionBaseControlEngine.GotoSelectedRow(const ASelectedRow: Integer): Boolean;
begin
  Result := False;
end;

function TJvDatabaseActionBaseControlEngine.IsFieldVisible(const AFieldName: string): Boolean;
var
  Field: TField;
begin
  Field := FieldByName(AFieldName);
  if Assigned(Field) then
    Result := Field.Visible
  else
    Result := False;
end;

function TJvDatabaseActionBaseControlEngine.IsFieldReadOnly(const AFieldName: string): Boolean;
var
  Field: TField;
begin
  Field := FieldByName(AFieldName);
  if Assigned(Field) then
    Result := Field.ReadOnly
  else
    Result := False;
end;

procedure TJvDatabaseActionBaseControlEngine.Last;
begin
  if Assigned(DataSet) then
    DataSet.Last;
end;

procedure TJvDatabaseActionBaseControlEngine.MoveBy(Distance: Integer);
begin
  if Assigned(DataSet) then
    DataSet.MoveBy(Distance);
end;

procedure TJvDatabaseActionBaseControlEngine.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDataComponent) then
    DataComponent := nil;
end;

function TJvDatabaseActionBaseControlEngine.GetSelectedField: TField;
begin
  Result := nil;
end;

function TJvDatabaseActionBaseControlEngine.SelectedRowsCount: Integer;
begin
  Result := 0;
end;

procedure TJvDatabaseActionBaseControlEngine.SetDatacomponent(const Value: TComponent);
begin
  inherited SetDatacomponent (Value);
  FDatasource := GetDataSource(Value);
  FDataset := GetDataSet(Value);
end;

procedure TJvDatabaseActionBaseControlEngine.ShowSingleRecordWindow(AOptions: TJvShowSingleRecordWindowOptions);
var
  Dialog: TJvDynControlDataSourceEditDialog;
begin
  Dialog := TJvDynControlDataSourceEditDialog.Create;
  try
    AOptions.SetOptionsToDialog(Dialog);
    if Dialog.DynControlEngineDB.SupportsDataComponent(DataComponent) then
      Dialog.DataComponent := DataComponent
    else
      Dialog.DataComponent := DataSource;
    Dialog.ShowDialog;
  finally
    Dialog.Free;
  end;
end;

//=== { TJvDatabaseActionDBGridControlEngine } ===============================

function TJvDatabaseActionDBGridControlEngine.GetDataSource(ADataComponent: TComponent): TDataSource;
begin
  if Assigned(ADataComponent) and (ADataComponent is TCustomDBGrid) then
    Result := TCustomDBGrid(ADataComponent).DataSource
  else
    Result := nil;
end;

type
  TAccessCustomDBGrid = class(TCustomDBGrid);
  TAccessCustomControl = class(TCustomControl);

constructor TJvDatabaseActionDBGridControlEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomDBGrid := nil;
end;

function TJvDatabaseActionDBGridControlEngine.GetCustomDBGrid(ADataComponent: TComponent): TCustomDBGrid;
begin
  if Assigned(ADataComponent) and (ADataComponent is TCustomDBGrid) then
    Result := TCustomDBGrid(ADataComponent)
  else
    Result := nil;
end;

function TJvDatabaseActionDBGridControlEngine.GotoSelectedRow(const ASelectedRow: Integer): Boolean;
begin
  if (ASelectedRow >= 0) and (ASelectedRow < SelectedRowsCount) and
    Assigned(Dataset) and Dataset.Active then
  begin
    Dataset.GotoBookmark(Pointer(TAccessCustomDBGrid(CustomDBGrid).SelectedRows[ASelectedRow]));
    Result := True;
  end
  else
    Result := False;
end;

procedure TJvDatabaseActionDBGridControlEngine.OnCreateDataControls(ADynControlEngineDB: TJvDynControlEngineDB;
  AParentControl: TWinControl; AFieldCreateOptions: TJvCreateDBFieldsOnControlOptions);
var
  I: Integer;
  ds: TDataSource;
  Field: TField;
  LabelControl: TControl;
  Control: TWinControl;
  Column: TColumn;
begin
  if Assigned(CustomDBGrid) then
  begin
    ds := DataSource;
    with AFieldCreateOptions do
      for I := 0 to TAccessCustomDBGrid(CustomDBGrid).ColCount - 2 do
      begin
        Column := TAccessCustomDBGrid(CustomDBGrid).Columns[I];
        if Column.Visible or ShowInvisibleFields then
        begin
          Field := Column.Field;
          Control := ADynControlEngineDB.CreateDBFieldControl(Field, AParentControl, AParentControl, '', ds);
          Control.Enabled := not IsFieldReadOnly(Field.FieldName);
          if FieldDefaultWidth > 0 then
            Control.Width := FieldDefaultWidth
          else
          begin
            if UseFieldSizeForWidth then
              if Field.Size > 0 then
                Control.Width :=
                  TAccessCustomControl(AParentControl).Canvas.TextWidth('X') * Field.Size
              else
              begin
                if (ADynControlEngineDB.GetFieldControlType(ds.DataSet.Fields[I])= jctDBMemo) and
                 (FieldMaxWidth > 0) then
                  Control.Width := FieldMaxWidth;
              end
            else
              if Field.DisplayWidth > 0 then
                Control.Width :=
                  TAccessCustomControl(AParentControl).Canvas.TextWidth('X') * Field.DisplayWidth;
            if (FieldMaxWidth > 0) and (Control.Width > FieldMaxWidth) then
              Control.Width := FieldMaxWidth
            else
              if (FieldMinWidth > 0) and (Control.Width < FieldMinWidth) then
                Control.Width := FieldMinWidth;
          end;
          if UseParentColorForReadOnly then
            if (Assigned(ds.DataSet) and not ds.DataSet.CanModify) or Field.ReadOnly then
              if isPublishedProp(Control, 'ParentColor') then
                SetOrdProp(Control, 'ParentColor', Ord(True));
          LabelControl := ADynControlEngineDB.DynControlEngine.CreateLabelControlPanel(AParentControl,
            AParentControl, '', '&' + Column.Title.Caption, Control, True, 0);
          if FieldWidthStep > 0 then
            if (LabelControl.Width mod FieldWidthStep) <> 0 then
              LabelControl.Width := ((LabelControl.Width div FieldWidthStep) + 1) * FieldWidthStep;
        end;
      end;
  end;
end;

function TJvDatabaseActionDBGridControlEngine.GetSelectedField: TField;
begin
  if Assigned(CustomDBGrid) then
    Result := CustomDBGrid.SelectedField
  else
    Result := nil;
end;

function TJvDatabaseActionDBGridControlEngine.SelectedRowsCount: Integer;
begin
  if Assigned(CustomDBGrid) then
    Result := TAccessCustomDBGrid(CustomDBGrid).SelectedRows.Count
  else
    Result := 0;
end;

procedure TJvDatabaseActionDBGridControlEngine.SetDatacomponent(const Value: TComponent);
begin
  inherited SetDatacomponent(Value);
  FCustomDbGrid := GetCustomDBGrid(Value);
end;

function TJvDatabaseActionDBGridControlEngine.Supports(ADataComponent: TComponent): Boolean;
begin
  Result := Assigned(ADataComponent) and (ADataComponent is TCustomDBGrid);
end;

procedure TJvDatabaseActionDBGridControlEngine.ShowSingleRecordWindow(AOptions: TJvShowSingleRecordWindowOptions);
var
  Dialog: TJvDynControlDataSourceEditDialog;
begin
  Dialog := TJvDynControlDataSourceEditDialog.Create;
  try
    AOptions.SetOptionsToDialog(Dialog);
    if Dialog.DynControlEngineDB.SupportsDataComponent(DataComponent) then
      Dialog.DataComponent := DataComponent
    else
      Dialog.DataComponent := DataSource;
    Dialog.OnCreateDataControlsEvent := OnCreateDataControls;
    Dialog.ShowDialog;
  finally
    Dialog.Free;
  end;
end;

//=== { TJvDatabaseActionEngineList } ========================================

destructor TJvDatabaseActionEngineList.Destroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    TJvDatabaseActionBaseControlEngine(Items[I]).Free;
    Items[I] := nil;
    Delete(I);
  end;
  inherited Destroy;
end;

procedure TJvDatabaseActionEngineList.RegisterEngine(AEngineClass: TJvDatabaseActionBaseEngineClass);
begin
  Add(AEngineClass.Create(nil));
end;

function TJvDatabaseActionEngineList.GetControlEngine(AComponent: TComponent): TJvDatabaseActionBaseControlEngine;
var
  Ind: Integer;
begin
  Result := nil;
  for Ind := 0 to Count - 1 do
    if TObject(Items[Ind]) is TJvDatabaseActionBaseControlEngine then
      if TJvDatabaseActionBaseControlEngine(Items[Ind]).Supports(AComponent) then
      begin
        Result := TJvDatabaseActionBaseControlEngine(Items[Ind]);
        Break;
      end;
end;

function TJvDatabaseActionEngineList.GetDatasetEngine(AComponent: TComponent): TJvDatabaseActionBaseDatasetEngine;
var
  Ind: Integer;
begin
  Result := nil;
  for Ind := 0 to Count - 1 do
    if TObject(Items[Ind]) is TJvDatabaseActionBaseDatasetEngine then
      if TJvDatabaseActionBaseDatasetEngine(Items[Ind]).Supports(AComponent) then
      begin
        Result := TJvDatabaseActionBaseDatasetEngine(Items[Ind]);
        Break;
      end;
end;

function TJvDatabaseActionEngineList.Supports(AComponent: TComponent): Boolean;
begin
  Result := Assigned(GetControlEngine(AComponent));
end;

function TJvDatabaseActionEngineList.SupportsDataset(AComponent: TComponent): Boolean;
begin
  Result := Assigned(GetDatasetEngine(AComponent));
end;

//=== { TJvDatabaseActionBaseEngine } ========================================

procedure TJvDatabaseActionBaseEngine.SetDatacomponent(const Value: TComponent);
begin
  FDatacomponent := Value;
  if FDataComponent <> nil then
    FDataComponent.FreeNotification(Self);
end;

function TJvDatabaseActionBaseEngine.Supports(ADataComponent: TComponent):
    Boolean;
begin
  Result := False;
end;

//=== { TJvDatabaseActionBaseDatasetEngine } =================================

function TJvDatabaseActionBaseDatasetEngine.GetSQL: string;
begin
  Result := '';
end;

procedure TJvDatabaseActionBaseDatasetEngine.SetDatacomponent(const Value: TComponent);
begin
  inherited Setdatacomponent (Value);
  if Supports(Value) then
    FDataset := TDataset(Value)
  else
    FDataset := nil;
end;

procedure TJvDatabaseActionBaseDatasetEngine.SetSQL(const Value: string);
begin
  
end;

function TJvDatabaseActionBaseDatasetEngine.Supports(ADataComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TJvDatabaseActionBaseDatasetEngine.SupportsGetSQL: Boolean;
begin
  Result := False;
end;

function TJvDatabaseActionBaseDatasetEngine.SupportsSetSQL: Boolean;
begin
  Result := False;
end;

//=== { TJvShowSingleRecordWindowOptions } ===================================

constructor TJvShowSingleRecordWindowOptions.Create;
begin
  inherited Create;
  FDialogCaption := '';
  FPostButtonCaption := RsSRWPostButtonCaption;
  FCancelButtonCaption := RsSRWCancelButtonCaption;
  FCloseButtonCaption := RsSRWCloseButtonCaption;
  FBorderStyle := bsDialog;
  FTop := 0;
  FLeft := 0;
  FWidth := 640;
  FHeight := 480;
  FPosition := poScreenCenter;
  FArrangeSettings := TJvArrangeSettings.Create();
  with FArrangeSettings do
  begin
    AutoSize := asBoth;
    DistanceHorizontal := 3;
    DistanceVertical := 3;
    BorderLeft := 3;
    BorderTop := 3;
    WrapControls := True;
  end;
  FArrangeConstraints := TSizeConstraints.Create(nil);
  FArrangeConstraints.MaxHeight := 480;
  FArrangeConstraints.MaxWidth := 640;
  FFieldCreateOptions := TJvCreateDBFieldsOnControlOptions.Create;
end;

destructor TJvShowSingleRecordWindowOptions.Destroy;
begin
  FFieldCreateOptions.Free;
  FArrangeConstraints.Free;
  FArrangeSettings.Free;
  inherited Destroy;
end;

procedure TJvShowSingleRecordWindowOptions.SetArrangeSettings(Value: TJvArrangeSettings);
begin
  FArrangeSettings.Assign(Value);
end;

procedure TJvShowSingleRecordWindowOptions.SetArrangeConstraints(Value: TSizeConstraints);
begin
  FArrangeConstraints.Assign(Value);
end;

procedure TJvShowSingleRecordWindowOptions.SetFieldCreateOptions(Value: TJvCreateDBFieldsOnControlOptions);
begin
  FFieldCreateOptions.Assign(Value);
end;

procedure TJvShowSingleRecordWindowOptions.SetOptionsToDialog(ADialog: TJvDynControlDataSourceEditDialog);
begin
  if Assigned(ADialog) then
  begin
    ADialog.DialogCaption := DialogCaption;
    ADialog.PostButtonCaption := PostButtonCaption;
    ADialog.CancelButtonCaption := CancelButtonCaption;
    ADialog.CloseButtonCaption := CloseButtonCaption;
    ADialog.Position := Position;
    ADialog.BorderStyle := BorderStyle;
    ADialog.Top := Top;
    ADialog.Left := Left;
    ADialog.Width := Width;
    ADialog.Height := Height;
    ADialog.ArrangeConstraints := ArrangeConstraints;
    ADialog.ArrangeSettings := ArrangeSettings;
    ADialog.FieldCreateOptions := FieldCreateOptions;
  end;
end;

//=== Global =================================================================

function RegisteredDatabaseActionEngineList: TJvDatabaseActionEngineList;
begin
  Result := IntRegisteredActionEngineList;
end;

procedure RegisterActionEngine(AEngineClass: TJvDatabaseActionBaseEngineClass);
begin
  if Assigned(IntRegisteredActionEngineList) then
    IntRegisteredActionEngineList.RegisterEngine(AEngineClass);
end;

procedure CreateActionEngineList;
begin
  IntRegisteredActionEngineList := TJvDatabaseActionEngineList.Create;
end;

procedure DestroyActionEngineList;
begin
  IntRegisteredActionEngineList.Free;
  IntRegisteredActionEngineList := nil;
end;

procedure InitActionEngineList;
begin
  CreateActionEngineList;
  RegisterActionEngine(TJvDatabaseActionBaseControlEngine);
  RegisterActionEngine(TJvDatabaseActionDBGridControlEngine);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  InitActionEngineList;

finalization
  DestroyActionEngineList;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

