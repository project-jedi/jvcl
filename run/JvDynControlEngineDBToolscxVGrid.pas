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

unit JvDynControlEngineDBToolscxVGrid;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Controls, DB, Classes,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  cxDBVGrid, cxVGrid,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGridCustomTableView,
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  JvDynControlEngineDBTools,
  //JvPanel,
  JvDynControlEngineTools, JvDynControlEngine, JvDynControlEngineDB;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
type
  TJvDataSourceEditDialogCreateDataControlscxVGridEventClass = class(TObject)
  private
    FInternalDBVGrid: TcxDBVerticalGrid;
    FLayoutStyle: TcxvgLayoutStyle;
    procedure CreateDefaultFieldsOnVerticalGrid(aGrid: tcxDBVerticalGrid;
        aHiddenFieldNames: string = ''; aVisibleFieldNames: string = '*';
        aMinLengthForMemo: Integer = 50);
    function GetOptionsBehavior: TcxvgMultiRecordsOptionsBehavior;
    function GetOptionsData: TcxvgMultiRecordsOptionsData;
    function GetOptionsView: TcxvgMultiRecordsOptionsView;
    procedure SetOptionsBehavior(const Value: TcxvgMultiRecordsOptionsBehavior);
    procedure SetOptionsData(const Value: TcxvgMultiRecordsOptionsData);
    procedure SetOptionsView(const Value: TcxvgMultiRecordsOptionsView);
    {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
    procedure TransferGridItemToVertGrid(aGridItem: TcxCustomGridTableItem; aVGrid:
        TcxCustomVerticalGrid; var aPos: Integer; aCreateColumn: Boolean = true);
    procedure TransferGridViewToVertGrid(aView: tcxCustomGridTableView; aVGrid:
        TcxCustomVerticalGrid; TransferGridMode: Boolean);
    {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateDataControls(ADatacomponent : TComponent; ADynControlEngineDB:
        TJvDynControlEngineDB; AParentControl: TWinControl; AFieldCreateOptions:
        TJvCreateDBFieldsOnControlOptions);
    property LayoutStyle: TcxvgLayoutStyle read FLayoutStyle write FLayoutStyle;
    property OptionsBehavior: TcxvgMultiRecordsOptionsBehavior read
        GetOptionsBehavior write SetOptionsBehavior;
    property OptionsData: TcxvgMultiRecordsOptionsData read GetOptionsData write
        SetOptionsData;
    property OptionsView: TcxvgMultiRecordsOptionsView read GetOptionsView write
        SetOptionsView;

  end;

Var
  DefaultDataSourceEditDialogCreateDataControlscxVGridEventClass : TJvDataSourceEditDialogCreateDataControlscxVGridEventClass;
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}


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
  StdCtrls, SysUtils,
  JvDBActions, JvDynControlEngineIntf,
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  cxMemo, cxSpinEdit, cxCalc, cxCalendar,
{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  cxGrid, cxGridDBDataDefinitions, cxGridTableView, cxGridCustomView,
  cxGridBandedTableView,
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  JclStrings, JvPanel, JvJCLUtils;

type
  TAccessControl = class(TControl);

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}

constructor TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.Create;
begin
  inherited Create;
  FInternalDBVGrid := TcxDBVerticalGrid.Create(nil);
end;

destructor TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.Destroy;
begin
  FreeAndNil(FInternalDBVGrid);
  inherited Destroy;
end;

procedure
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.CreateDataControls(
    ADatacomponent : TComponent; ADynControlEngineDB: TJvDynControlEngineDB;
    AParentControl: TWinControl; AFieldCreateOptions:
    TJvCreateDBFieldsOnControlOptions);
var
  VGrid : TcxDBVerticalGrid;
begin
  if AParentControl is TJvPanel then
  begin
    TJvPanel (AParentControl).ArrangeSettings.AutoArrange := False;
    TJvPanel (AParentControl).ArrangeSettings.AutoSize := asNone;
    TJvPanel (AParentControl).Height:=0;
  end;
  VGrid := TcxDBVerticalGrid.Create (AParentControl.Parent);
  VGrid.Parent := AParentControl.Parent;
  VGrid.Align := alClient;
  VGrid.OptionsData := OptionsData;
  VGrid.OptionsBehavior := OptionsBehavior;
  VGrid.OptionsView := OptionsView;
  VGrid.LayoutStyle := LayoutStyle;
  VGrid.DataController.DataSource := ADynControlEngineDB.GetDataSourceFromDataComponent(ADatacomponent);
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
  if (aDataComponent is TcxGrid) and Assigned(tcxGrid(aDataComponent).ActiveView)
    and (tcxGrid(aDataComponent).ActiveView is tcxCustomGridTableView) then
    TransferGridViewToVertGrid(tcxCustomGridTableView(tcxGrid(aDataComponent).ActiveView), VGrid, False)
  else if (aDatacomponent is tcxCustomGridTableView) then
    TransferGridViewToVertGrid(tcxCustomGridTableView(aDatacomponent), VGrid, False)
  else
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
    CreateDefaultFieldsOnVerticalGrid (vGrid);
end;

procedure
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.CreateDefaultFieldsOnVerticalGrid(
    aGrid: tcxDBVerticalGrid; aHiddenFieldNames: string = '';
    aVisibleFieldNames: string = '*'; aMinLengthForMemo: Integer = 50);
var
  i, j: Integer;
  HiddenFields: tStringList;
  FieldNamesList: TStringList;
  Fieldfound: boolean;
  DBDatabinding: TcxDBVerticalGridItemDataBinding;
  CurrentRow: TcxDBEditorRow;
  Hiddenfound: Boolean;

  function GetFieldName(s: string): string;
  var
    i: Integer;
  begin
    s := Uppercase(s);
    if not CharInSet(s[1], ['A'..'Z', '_']) then
      s[1] := '_';
    for i := 2 to Length(s) do
      if not CharInSet(s[i], ['A'..'Z', '0'..'9', '_']) then
        s[i] := '_';
    Result := s;
  end;

begin
  if not Assigned(aGrid) then
    Exit;
  aGrid.BeginUpdate;
  HiddenFields := tStringList.Create;
  FieldNamesList := TStringList.Create;
  try
    HiddenFields.CommaText := Uppercase(aHiddenFieldNames);
    FieldNamesList.CommaText := Uppercase(aVisibleFieldNames);
    aGrid.DataController.Filter.Clear;
    aGrid.ClearRows;
    aGrid.DataController.GridMode := True;
    aGrid.DataController.CreateAllItems;
    for i := 0 to aGrid.Rows.Count - 1 do
    begin
      if (aGrid.Rows[i] is TcxDBEditorRow) then
      begin
        CurrentRow := TcxDBEditorRow(aGrid.Rows[i]);
        if not Assigned(CurrentRow) then
          Continue;
        DBDatabinding := CurrentRow.Properties.DataBinding;
        if not Assigned(DBDatabinding) or not Assigned(DBDatabinding.Field) then
          Continue;
        CurrentRow.Name := GetFieldName(aGrid.Name + ' Row ' + DBDatabinding.FieldName);
        Hiddenfound := False;
        for j := 0 to hiddenfields.Count - 1 do
          if (trim(HiddenFields[j]) <> '') and
            StrMatches(HiddenFields[j], Uppercase(DBDatabinding.FieldName), 1) then
          begin
            Hiddenfound := True;
            break;
          end;
        if Hiddenfound then
          CurrentRow.Visible := FALSE;
        Fieldfound := False;
        for j := 0 to FieldNamesList.Count - 1 do
          if (trim(FieldNamesList[j]) <> '') and
            StrMatches(FieldNamesList[j], Uppercase(DBDatabinding.FieldName), 1) then
          begin
            Fieldfound := True;
            break;
          end;
        if not Fieldfound then
          CurrentRow.Visible := FALSE;
        if DBDatabinding.Field.Datatype in [ftOraClob, ftMemo, ftFMTMemo{$IFDEF COMPILER10_UP}, ftWideMemo{$ENDIF COMPILER10_UP}] then
        begin
          CurrentRow.Properties.EditPropertiesClass := TcxMemoProperties;
          TcxMemoProperties(CurrentRow.Properties.EditProperties).Scrollbars := ssBoth;
        end
        else
          if DBDatabinding.Field.Datatype in [ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc{$IFDEF COMPILER12_UP},ftLongWord, ftShortint{$ENDIF COMPILER12_UP}] then
          begin
            CurrentRow.Properties.EditPropertiesClass := TcxSpinEditProperties;
            TcxSpinEditProperties(CurrentRow.Properties.EditProperties).ValueType := vtInt;
//            TcxSpinEditPropertiesAccess(CurrentRow.Properties.EditProperties).Buttons[0].Visible := False;
//            TcxSpinEditPropertiesAccess(CurrentRow.Properties.EditProperties).Buttons[1].Visible := False;
          end
          else
            if DBDatabinding.Field.Datatype in [ftFloat, ftCurrency, ftBCD, ftFMTBcd{$IFDEF COMPILER12_UP},ftExtended{$ENDIF COMPILER12_UP}] then
            begin
              CurrentRow.Properties.EditPropertiesClass := TcxCalcEditProperties;
            end
            else
              if DBDatabinding.Field.Datatype in [ftDate, ftTime, ftDateTime {$IFDEF COMPILER10_UP},ftOraTimestamp{$ENDIF COMPILER10_UP}] then
              begin
                CurrentRow.Properties.EditPropertiesClass := TcxDateEditProperties;
                TcxDateEditProperties(CurrentRow.Properties.EditProperties).InputKind := ikStandard;
                TcxDateEditProperties(CurrentRow.Properties.EditProperties).SaveTime := True;
                TcxDateEditProperties(CurrentRow.Properties.EditProperties).ShowTime := True;
              end
              else
                if (aMinLengthForMemo > 0) and
                  (DBDatabinding.Field.Datatype in [ftString, ftWideString]) and
                  (DBDatabinding.Field.Size > aMinLengthForMemo) then
                  CurrentRow.Properties.EditPropertiesClass := TcxMemoProperties;
      end
    end;

  finally
    HiddenFields.Free;
    aGrid.EndUpdate;
  end;
end;

function
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.GetOptionsBehavior:
    TcxvgMultiRecordsOptionsBehavior;
begin
  Result := FInternalDBVGrid.OptionsBehavior;
end;

function
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.GetOptionsData:
    TcxvgMultiRecordsOptionsData;
begin
  Result := FInternalDBVGrid.OptionsData;
end;

function
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.GetOptionsView:
    TcxvgMultiRecordsOptionsView;
begin
  Result := FInternalDBVGrid.OptionsView;
end;

procedure
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.SetOptionsBehavior(
    const Value: TcxvgMultiRecordsOptionsBehavior);
begin
  FInternalDBVGrid.OptionsBehavior.Assign (Value);
end;

procedure
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.SetOptionsData(
    const Value: TcxvgMultiRecordsOptionsData);
begin
  FInternalDBVGrid.OptionsData.Assign (Value);
end;

procedure
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.SetOptionsView(
    const Value: TcxvgMultiRecordsOptionsView);
begin
  FInternalDBVGrid.OptionsView.Assign (Value);
end;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXGRID}
procedure
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.TransferGridItemToVertGrid(
    aGridItem: TcxCustomGridTableItem; aVGrid: TcxCustomVerticalGrid; var aPos:
    Integer; aCreateColumn: Boolean = true);
var
  j: Integer;
  GridDataBinding: TcxGridItemDBDataBinding;
  EditorRow: TcxDBEditorRow;
begin
  if not (aGridItem is TcxGridColumn) or
    not (aGridItem.DataBinding is TcxGridItemDBDataBinding) then
    Exit;
  EditorRow := nil;
  GridDataBinding := TcxGridItemDBDataBinding(aGridItem.DataBinding);
  if aCreateColumn then
  begin
    EditorRow := TcxDBEditorRow(aVGrid.Add(TcxDBEditorRow));
    EditorRow.Properties.DataBinding.FieldName := GridDataBinding.FieldName;
    EditorRow.Properties.Caption := EditorRow.Properties.DataBinding.DefaultCaption;
  end
  else
    for j := 0 to aVGrid.Rows.Count - 1 do
    begin
      if not (aVGrid.Rows.Items[j] is TcxDBEditorRow) then
        Continue;
      EditorRow := TcxDBEditorRow(aVGrid.Rows.Items[j]);
      if not (EditorRow.Properties.DataBinding is TcxDBVerticalGridItemDataBinding) then
        continue;
      if GridDataBinding.FieldName = EditorRow.Properties.DataBinding.FieldName then
        Break;
      EditorRow := nil;
    end;
  if Assigned(EditorRow) then
  begin
    EditorRow.Properties.EditPropertiesClass := TcxGridColumn(aGridItem).PropertiesClass;
    EditorRow.Properties.EditProperties := TcxGridColumn(aGridItem).Properties;
    EditorRow.Visible := TcxGridColumn(aGridItem).ActuallyVisible or
      (TcxGridColumn(aGridItem).GroupIndex >= 0);
    EditorRow.Index := aPos;
    Inc(aPos);
  end;
end;

procedure
    TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.TransferGridViewToVertGrid(
    aView: tcxCustomGridTableView; aVGrid: TcxCustomVerticalGrid;
    TransferGridMode: Boolean);

var
  dbVGrid: TcxDBVerticalGrid;
  GridDataController: TcxGridDBDataController;
  i: Integer;
  Position: Integer;
  b: Integer;
begin
  if not Assigned(aView) then
    raise Exception.Create('TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.TransferGridViewToVertGrid : aView must be assigned');
  if not Assigned(aVGrid) then
    raise Exception.Create('TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.TransferGridViewToVertGrid : aVGrid must be assigned');
  if (csDestroying in aView.ComponentState) or
    (csDestroying in aVGrid.ComponentState) then
    exit;
  if avGrid is TcxDBVerticalGrid then
    dbVGrid := TcxDBVerticalGrid(avGrid)
  else
    raise Exception.Create('TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.TransferGridViewToVertGrid : aVGrid must TcxDBVerticalGrid');
  if tcxCustomGridView(aView).DataController is TcxGridDBDataController then
    GridDataController := TcxGridDBDataController(aView.DataController)
  else
    raise Exception.Create('TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.TransferGridViewToVertGrid : aView.DataController must TcxGridDBDataController');
  if aView.Control is TCxGrid then
    dbVGrid.LookAndFeel := TCxGrid(aView.Control).lookAndFeel;
  dbVGrid.BeginUpdate;
  try
    dbVGrid.DataController.Filter.Clear;
    dbVGrid.ClearRows;
    dbVGrid.DataController.Datasource := nil;
    if TransferGridMode then
      dbVGrid.dataController.GridMode :=
        GridDataController.DatamodeController.GridMode
    else
      dbVGrid.dataController.GridMode := True;
    dbVGrid.dataController.Datasource := GridDataController.Datasource;
    Position := 0;
    for i := 0 to aView.GroupedItemCount - 1 do
      TransferGridItemToVertGrid(aView.GroupedItems[i], dbVGrid, Position, True);
    if aView is TcxGridBandedTableView then
    begin
      for b := 0 to TcxGridBandedTableView(aView).Bands.VisibleCount - 1 do
        for i := 0 to aView.VisibleItemCount - 1 do
          if (aView.VisibleItems[i] is TcxGridBandedColumn) and
             (TcxGridBandedColumn (aView.VisibleItems[i]).Position.VisibleBandIndex = b) then
            TransferGridItemToVertGrid(aView.VisibleItems[i], dbVGrid, Position, True);
    end
    else
      for i := 0 to aView.VisibleItemCount - 1 do
        TransferGridItemToVertGrid(aView.VisibleItems[i], dbVGrid, Position, True);
  finally
    dbVGrid.EndUpdate;
  end;
end;
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXGRID}
{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}


initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  DefaultDataSourceEditDialogCreateDataControlscxVGridEventClass := TJvDataSourceEditDialogCreateDataControlscxVGridEventClass.Create;
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}


finalization
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  DefaultDataSourceEditDialogCreateDataControlscxVGridEventClass.Free;
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
