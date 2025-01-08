{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataSource.PAS, released on 08-07-2006.

The Initial Developer of the Original Code is Andreas Hausladen
[Andreas dott Hausladen att gmx dott com]
Portions created by Andreas Hausladen are Copyright (C) 2006 Andreas Hausladen.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDataSource;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, JvDataSourceIntf, DB, DBConsts;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDataSource = class(TDataSource, IJvDataSource, IJvDataSourceConnectorHandler)
  private
    FDataLink: TDataLink;
    FDataConnectors: TList;
    FUpdateLock: Integer;
    FUpdateLookBookmark: TBookmark;
    FOnFieldChanged: TDataChangeEvent;
    FOnEditingChanged: TNotifyEvent;
    FOnActiveChanged: TNotifyEvent;
    FOnCheckBrowseMode: TNotifyEvent;
    FOnLayoutChanged: TNotifyEvent;
    FOnDataSetChanged: TNotifyEvent;
    FOnDataSetScrolled: TNotifyEvent;
    FOnRecordChanged: TNotifyEvent;
    FEventsEnabled: Boolean;
    FNeedScroll: Boolean;
    FDataUpdated: Boolean;
    FDisableEventsOnLoading: Boolean;
    function GetDataConnector(Index: Integer): TJvDataConnector;
    function GetDataConnectorCount: Integer;
  protected
    procedure DataConnectorsFreeNotification;
    procedure AddDataConnector(DataConnector: TJvDataConnector);
    procedure RemoveDataConnector(DataConnector: TJvDataConnector);

    function DataSet: TDataSet;
    procedure Notify(Msg: Integer);

    procedure ActiveChanged; virtual;
    procedure FieldChanged(Field: TField); virtual;
    procedure RecordChanged; virtual;
    procedure LayoutChanged; virtual;
    procedure DataSetChanged; virtual;
    procedure DataSetScrolled; virtual;
    procedure EditingChanged; virtual;
    procedure CheckBrowseMode; virtual;
    procedure UpdateData; virtual;

    function AreEventsEnabled: Boolean;

    property DataConnectorCount: Integer read GetDataConnectorCount;
    property DataConnectors[Index: Integer]: TJvDataConnector read GetDataConnector; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { IJvDataSource implementation }
  protected
    { DataSource }
    procedure Edit; // DataSource.Edit
    function GetState: TJvDataSetState;
    function GetAutoEdit: Boolean;
    function GetEnabled: Boolean;

    { DataSet }
  public
    function GetDataLink: TObject;
    function GetDataSet: TObject;
    procedure DataSetEdit; // DataSet.Edit
    function CanModify: Boolean;
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;
    function Eof: Boolean;
    function Bof: Boolean;
    function RecordCount: Integer;
    function FieldByName(const FieldName: TDataFieldString): TObject;
    function FindField(const FieldName: TDataFieldString): TObject;
    procedure GetFieldNames(List: TStrings);
    function GetRecNo: Integer;
    procedure SetRecNo(Value: Integer);
    procedure Append;
    procedure Insert;
    procedure Post;
    procedure Cancel;
    procedure Delete;
    procedure Open;
    procedure Close;
    procedure MoveBy(Distance: Integer);
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TJvDBLocateOptions): Boolean;

    procedure BeginUpdate;
    procedure EndUpdate;

    property RecNo: Integer read GetRecNo write SetRecNo;

    { Fields }
    function GetFieldCount: Integer;
    function GetField(Index: Integer): TObject;
    function GetFieldName(Field: TObject): TDataFieldString;
    function GetFieldType(Field: TObject): TJvDBFieldType;
    function GetFieldSize(Field: TObject): Integer;
    function GetFieldDisplayLabel(Field: TObject): TDataFieldString;
    function GetFieldDisplayWidth(Field: TObject): Integer;
    procedure SetFieldDisplayWidth(Field: TObject; Value: Integer);
    function GetFieldEditMask(Field: TObject): TJvEditMask;
    function GetFieldReadOnly(Field: TObject): Boolean;
    function GetFieldVisible(Field: TObject): Boolean;
    function GetFieldRequired(Field: TObject): Boolean;
    procedure FieldClear(Field: TObject);
    function GetFieldIsNull(Field: TObject): Boolean;
    function GetFieldIsBlob(Field: TObject): Boolean;
    function GetFieldCanModify(Field: TObject): Boolean;

    function GetFieldOldValue(Field: TObject): Variant;
    function GetFieldValue(Field: TObject): Variant;
    procedure SetFieldValue(Field: TObject; const Value: Variant);
    function GetFieldString(Field: TObject): string;
    procedure SetFieldString(Field: TObject; const Value: string);
    function GetFieldWideString(Field: TObject): WideString;
    procedure SetFieldWideString(Field: TObject; const Value: WideString);
    function GetFieldInteger(Field: TObject): Integer;
    procedure SetFieldInteger(Field: TObject; const Value: Integer);
    function GetFieldFloat(Field: TObject): Double;
    procedure SetFieldFloat(Field: TObject; const Value: Double);
    function GetFieldDateTime(Field: TObject): TDateTime;
    procedure SetFieldDateTime(Field: TObject; const Value: TDateTime);
    function GetFieldBoolean(Field: TObject): Boolean;
    procedure SetFieldBoolean(Field: TObject; const Value: Boolean);

    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TObject read GetField;
    property FieldName[Field: TObject]: TDataFieldString read GetFieldName;
    property FieldType[Field: TObject]: TJvDBFieldType read GetFieldType;
    property FieldSize[Field: TObject]: Integer read GetFieldSize;
    property FieldDisplayLabel[Field: TObject]: TDataFieldString read GetFieldDisplayLabel;
    property FieldDisplayWidth[Field: TObject]: Integer read GetFieldDisplayWidth write SetFieldDisplayWidth;
    property FieldEditMask[Field: TObject]: TJvEditMask read GetFieldEditMask;
    property FieldReadOnly[Field: TObject]: Boolean read GetFieldReadOnly;
    property FieldVisible[Field: TObject]: Boolean read GetFieldVisible;
    property FieldRequired[Field: TObject]: Boolean read GetFieldRequired;
    property FieldIsNull[Field: TObject]: Boolean read GetFieldIsNull;
    property FieldIsBlob[Field: TObject]: Boolean read GetFieldIsBlob;
    property FieldCanModify[Field: TObject]: Boolean read GetFieldCanModify;

    property FieldOldValue[Field: TObject]: Variant read GetFieldOldValue;
    property FieldValue[Field: TObject]: Variant read GetFieldValue write SetFieldValue;
    property FieldString[Field: TObject]: string read GetFieldString write SetFieldString;
    property FieldWideString[Field: TObject]: WideString read GetFieldWideString write SetFieldWideString;
    property FieldInteger[Field: TObject]: Integer read GetFieldInteger write SetFieldInteger;
    property FieldFloat[Field: TObject]: Double read GetFieldFloat write SetFieldFloat;
    property FieldDataTime[Field: TObject]: TDateTime read GetFieldDateTime write SetFieldDateTime;
    property FieldBoolean[Field: TObject]: Boolean read GetFieldBoolean write SetFieldBoolean;
  published
    property EventsEnabled: Boolean read FEventsEnabled write FEventsEnabled default True;
    property DisableEventsOnLoading: Boolean read FDisableEventsOnLoading write FDisableEventsOnLoading default True;
    property OnActiveChanged: TNotifyEvent read FOnActiveChanged write FOnActiveChanged;
    property OnFieldChanged: TDataChangeEvent read FOnFieldChanged write FOnFieldChanged;
    property OnRecordChanged: TNotifyEvent read FOnRecordChanged write FOnRecordChanged;
    property OnLayoutChanged: TNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    property OnDataSetChanged: TNotifyEvent read FOnDataSetChanged write FOnDataSetChanged;
    property OnDataSetScrolled: TNotifyEvent read FOnDataSetScrolled write FOnDataSetScrolled;
    property OnEditingChanged: TNotifyEvent read FOnEditingChanged write FOnEditingChanged;
    property OnCheckBrowseMode: TNotifyEvent read FOnCheckBrowseMode write FOnCheckBrowseMode;
  end;

  TJvDataSourceDataLink = class(TDataLink)
  private
    FDataSource: TJvDataSource;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
    procedure LayoutChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure CheckBrowseMode; override;
  public
    constructor Create(ADataSource: TJvDataSource);
  end;

implementation

{ TJvDataSourceDataLink }

constructor TJvDataSourceDataLink.Create(ADataSource: TJvDataSource);
begin
  inherited Create;
  FDataSource := ADataSource
end;

procedure TJvDataSourceDataLink.LayoutChanged;
begin
  FDataSource.Notify(DC_LAYOUTCHANGED);
  FDataSource.LayoutChanged;
end;

procedure TJvDataSourceDataLink.ActiveChanged;
begin
  FDataSource.Notify(DC_ACTIVECHANGED);
  FDataSource.ActiveChanged;
end;

procedure TJvDataSourceDataLink.RecordChanged(Field: TField);
begin
  FDataSource.Notify(DC_RECORDCHANGED);
  if Field <> nil then
    FDataSource.FieldChanged(Field);
  FDataSource.RecordChanged;
end;

procedure TJvDataSourceDataLink.UpdateData;
begin
  FDataSource.Notify(DC_UPDATEDATA);
  FDataSource.UpdateData;
end;

procedure TJvDataSourceDataLink.DataSetChanged;
begin
  FDataSource.Notify(DC_RECORDCHANGED);
  FDataSource.DataSetChanged;
end;

procedure TJvDataSourceDataLink.DataSetScrolled(Distance: Integer);
begin
  FDataSource.Notify(DC_RECORDCHANGED); // that is what the inherited method would do
  FDataSource.Notify(DC_DATASETSCROLLED);
  FDataSource.DataSetScrolled;
end;

procedure TJvDataSourceDataLink.EditingChanged;
begin
  FDataSource.Notify(DC_EDITINGCHANGED);
  FDataSource.EditingChanged;
end;

procedure TJvDataSourceDataLink.CheckBrowseMode;
begin
  FDataSource.Notify(DC_CHECKBROWSEMODE);
  FDataSource.CheckBrowseMode;
end;

{ TJvDataSource }

constructor TJvDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TJvDataSourceDataLink.Create(Self);
  FDataConnectors := TList.Create;
  FDataLink.DataSource := Self;
  FEventsEnabled := True;
  FDisableEventsOnLoading := True;
end;

procedure TJvDataSource.DataConnectorsFreeNotification;
var
  I: Integer;
begin
  { Notify all DataConnectors by setting their DataSource property to NIL }
  I := FDataConnectors.Count - 1;
  while i >= 0 do
  begin
    DataConnectors[I].DataSource := nil;
    if I >= DataConnectorCount then
      I := DataConnectorCount - 1
    else
      Dec(I);
  end;
end;

destructor TJvDataSource.Destroy;
begin
  DataConnectorsFreeNotification;
  FDataLink.Free;
  FDataConnectors.Free;
  inherited Destroy;
end;

function TJvDataSource.GetDataConnector(Index: Integer): TJvDataConnector;
begin
  Result := TJvDataConnector(FDataConnectors[Index]);
end;

function TJvDataSource.GetDataConnectorCount: Integer;
begin
  Result := FDataConnectors.Count;
end;

function TJvDataSource.GetDataLink: TObject;
begin
  Result := FDataLink;
end;

procedure TJvDataSource.Notify(Msg: Integer);
var
  M: TJvDataConnectorMsg;
  I: Integer;
begin
  M.Msg := Msg;
  for I := 0 to DataConnectorCount - 1 do
    DataConnectors[I].Dispatch(M);
end;

procedure TJvDataSource.AddDataConnector(DataConnector: TJvDataConnector);
begin
  FDataConnectors.Add(DataConnector);
end;

procedure TJvDataSource.RemoveDataConnector(DataConnector: TJvDataConnector);
begin
  FDataConnectors.Remove(DataConnector);
end;

procedure TJvDataSource.Edit;
begin
  inherited Edit;
end;

function TJvDataSource.GetState: TJvDataSetState;
begin
  Result := TJvDataSetState(State);
end;

function TJvDataSource.GetAutoEdit: Boolean;
begin
  Result := AutoEdit;
end;

function TJvDataSource.GetEnabled: Boolean;
begin
  Result := Enabled;
end;

function TJvDataSource.DataSet: TDataSet;
begin
  Result := inherited DataSet;
  if not Assigned(Result) then
    raise EDatabaseError.CreateRes(@SDataSetClosed);
end;

function TJvDataSource.GetDataSet: TObject;
begin
  Result := inherited DataSet;
end;

procedure TJvDataSource.DataSetEdit;
begin
  DataSet.Edit;
end;

function TJvDataSource.CanModify: Boolean;
begin
  Result := Assigned(inherited DataSet) and DataSet.CanModify;
end;

procedure TJvDataSource.First;
begin
  DataSet.First;
end;

procedure TJvDataSource.Last;
begin
  DataSet.Last;
end;

procedure TJvDataSource.Next;
begin
  DataSet.Next;
end;

procedure TJvDataSource.Prior;
begin
  DataSet.Prior;
end;

function TJvDataSource.Eof: Boolean;
begin
  Result := DataSet.Eof;
end;

function TJvDataSource.Bof: Boolean;
begin
  Result := DataSet.Bof;
end;

function TJvDataSource.RecordCount: Integer;
begin
  Result := DataSet.RecordCount;
end;

function TJvDataSource.FieldByName(const FieldName: TDataFieldString): TObject;
begin
  Result := DataSet.FieldByName(FieldName);
end;

function TJvDataSource.FindField(const FieldName: TDataFieldString): TObject;
begin
  Result := DataSet.FindField(FieldName);
end;

procedure TJvDataSource.GetFieldNames(List: TStrings);
begin
  {$IFDEF COMPILER10_UP}
  {$WARN SYMBOL_DEPRECATED OFF}
  DataSet.GetFieldNames(List);
  {$WARN SYMBOL_DEPRECATED ON}
  {$ELSE}
  DataSet.GetFieldNames(List);
  {$ENDIF COMPILER10_UP}
end;

function TJvDataSource.GetRecNo: Integer;
begin
  Result := DataSet.RecNo;
end;

procedure TJvDataSource.SetRecNo(Value: Integer);
begin
  DataSet.RecNo := Value;
end;

procedure TJvDataSource.Append;
begin
  DataSet.Append;
end;

procedure TJvDataSource.Insert;
begin
  DataSet.Insert;
end;

procedure TJvDataSource.Post;
begin
  DataSet.Post;
end;

procedure TJvDataSource.Cancel;
begin
  DataSet.Cancel;
end;

procedure TJvDataSource.Delete;
begin
  DataSet.Delete;
end;

procedure TJvDataSource.Open;
begin
  DataSet.Open;
end;

procedure TJvDataSource.Close;
begin
  DataSet.Close;
end;

procedure TJvDataSource.MoveBy(Distance: Integer);
begin
  DataSet.MoveBy(Distance)
end;

function TJvDataSource.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TJvDBLocateOptions): Boolean;
begin
  Result := DataSet.Locate(KeyFields, KeyValues, TLocateOptions(Options));
end;

procedure TJvDataSource.BeginUpdate;
begin
  if FUpdateLock = 0 then
  begin
    DataSet.DisableControls;
    FUpdateLookBookmark := DataSet.GetBookmark;
  end;
  Inc(FUpdateLock);
end;

procedure TJvDataSource.EndUpdate;
begin
  Dec(FUpdateLock);
  if FUpdateLock = 0 then
  begin
    try
      try
        DataSet.GotoBookmark(FUpdateLookBookmark);
      except
      end;
      DataSet.FreeBookmark(FUpdateLookBookmark);
    finally
      DataSet.EnableControls;
    end;
  end;
end;

function TJvDataSource.GetFieldCount: Integer;
begin
  Result := DataSet.FieldCount;
end;

function TJvDataSource.GetField(Index: Integer): TObject;
begin
  Result := DataSet.Fields[Index];
end;

function TJvDataSource.GetFieldName(Field: TObject): TDataFieldString;
begin
  Result := TField(Field).FieldName;
end;

function TJvDataSource.GetFieldType(Field: TObject): TJvDBFieldType;
begin
  Result := TJvDBFieldType(TField(Field).DataType);
end;

function TJvDataSource.GetFieldSize(Field: TObject): Integer;
begin
  Result := TField(Field).Size;
end;

function TJvDataSource.GetFieldDisplayLabel(Field: TObject): TDataFieldString;
begin
  Result := TField(Field).DisplayLabel;
end;

function TJvDataSource.GetFieldDisplayWidth(Field: TObject): Integer;
begin
  Result := TField(Field).DisplayWidth;
end;

procedure TJvDataSource.SetFieldDisplayWidth(Field: TObject; Value: Integer);
begin
  TField(Field).DisplayWidth := Value;
end;

function TJvDataSource.GetFieldEditMask(Field: TObject): TJvEditMask;
begin
  Result := TField(Field).EditMask;
end;

function TJvDataSource.GetFieldReadOnly(Field: TObject): Boolean;
begin
  Result := TField(Field).ReadOnly;
end;

function TJvDataSource.GetFieldVisible(Field: TObject): Boolean;
begin
  Result := TField(Field).Visible;
end;

function TJvDataSource.GetFieldRequired(Field: TObject): Boolean;
begin
  Result := TField(Field).Required;
end;

procedure TJvDataSource.FieldClear(Field: TObject);
begin
  TField(Field).Clear;
end;

function TJvDataSource.GetFieldIsNull(Field: TObject): Boolean;
begin
  Result := TField(Field).IsNull
end;

function TJvDataSource.GetFieldIsBlob(Field: TObject): Boolean;
begin
  Result := TField(Field).IsBlob;
end;

function TJvDataSource.GetFieldCanModify(Field: TObject): Boolean;
begin
  Result := TField(Field).CanModify;
end;

function TJvDataSource.GetFieldOldValue(Field: TObject): Variant;
begin
  Result := TField(Field).OldValue;
end;

function TJvDataSource.GetFieldValue(Field: TObject): Variant;
begin
  Result := TField(Field).AsVariant;
end;

procedure TJvDataSource.SetFieldValue(Field: TObject; const Value: Variant);
begin
  TField(Field).AsVariant := Value;
end;

function TJvDataSource.GetFieldString(Field: TObject): string;
begin
  Result := TField(Field).AsString;
end;

procedure TJvDataSource.SetFieldString(Field: TObject; const Value: string);
begin
  TField(Field).AsString := Value;
end;

function TJvDataSource.GetFieldWideString(Field: TObject): WideString;
begin
  {$IFDEF COMPILER10_UP}
  Result := TField(Field).AsWideString;
  {$ELSE}
  Result := TField(Field).AsString;
  {$ENDIF COMPILER10_UP}
end;

procedure TJvDataSource.SetFieldWideString(Field: TObject; const Value: WideString);
begin
  {$IFDEF COMPILER10_UP}
  TField(Field).AsWideString := Value;
  {$ELSE}
  TField(Field).AsString := Value;
  {$ENDIF COMPILER10_UP}
end;

function TJvDataSource.GetFieldInteger(Field: TObject): Integer;
begin
  Result := TField(Field).AsInteger;
end;

procedure TJvDataSource.SetFieldInteger(Field: TObject; const Value: Integer);
begin
  TField(Field).AsInteger := Value;
end;

function TJvDataSource.GetFieldFloat(Field: TObject): Double;
begin
  Result := TField(Field).AsFloat;
end;

procedure TJvDataSource.SetFieldFloat(Field: TObject; const Value: Double);
begin
  TField(Field).AsFloat := Value;
end;

function TJvDataSource.GetFieldDateTime(Field: TObject): TDateTime;
begin
  Result := TField(Field).AsDateTime;
end;

procedure TJvDataSource.SetFieldDateTime(Field: TObject; const Value: TDateTime);
begin
  TField(Field).AsDateTime := Value;
end;

function TJvDataSource.GetFieldBoolean(Field: TObject): Boolean;
begin
  Result := TField(Field).AsBoolean;
end;

procedure TJvDataSource.SetFieldBoolean(Field: TObject; const Value: Boolean);
begin
  TField(Field).AsBoolean := Value;
end;

procedure TJvDataSource.ActiveChanged;
begin
  try
    if AreEventsEnabled and Assigned(FOnActiveChanged) then
      FOnActiveChanged(Self);
  finally
    if (inherited DataSet <> nil) and inherited DataSet.Active then
      DataSetScrolled;
  end;
end;

procedure TJvDataSource.CheckBrowseMode;
begin
  if FDataLink.DataSet <> nil then
  begin
    if not (FDataLink.DataSet.State in [dsEdit, dsInsert]) then
      FDataUpdated := False;
    FNeedScroll := True;
    if (FDataLink.DataSet.State = dsInsert) and FDataUpdated then
      FNeedScroll := False;
    if AreEventsEnabled and Assigned(FOnCheckBrowseMode) then
      FOnCheckBrowseMode(Self);
  end;
end;

procedure TJvDataSource.DataSetChanged;
var
  ScrollEventEnabled: Boolean;
begin
  ScrollEventEnabled := FNeedScroll;
  FNeedScroll := False;
  try
    if AreEventsEnabled and Assigned(FOnDataSetChanged) then
      FOnDataSetChanged(Self);
  finally
    if ScrollEventEnabled or 
       ((FDataLink.DataSet <> nil) and (FDataLink.DataSet.State = dsInsert)) then
      DataSetScrolled;
  end;
end;

procedure TJvDataSource.DataSetScrolled;
begin
  FNeedScroll := False;
  if AreEventsEnabled and Assigned(FOnDataSetScrolled) then
    FOnDataSetScrolled(Self);
end;

procedure TJvDataSource.EditingChanged;
begin
  if FDataLink.DataSet <> nil then
  begin
    if FDataUpdated or (FDataLink.DataSet.Active and (FDataLink.DataSet.RecNo <> -1)) then // DataSet.State is already updated
      FNeedScroll := False;
    if AreEventsEnabled and Assigned(FOnEditingChanged) then
      FOnEditingChanged(Self);
  end;
end;

procedure TJvDataSource.LayoutChanged;
begin
  if AreEventsEnabled and Assigned(FOnLayoutChanged) then
    FOnLayoutChanged(Self);
end;

procedure TJvDataSource.RecordChanged;
begin
  if AreEventsEnabled and Assigned(FOnRecordChanged) then
    FOnRecordChanged(Self);
end;

procedure TJvDataSource.FieldChanged(Field: TField);
begin
  if AreEventsEnabled and Assigned(FOnFieldChanged) then
    FOnFieldChanged(Self, Field);
end;

procedure TJvDataSource.UpdateData;
begin // event is handled by TDataSource
  FDataUpdated := True;
end;

function TJvDataSource.AreEventsEnabled: Boolean;
begin
  Result := EventsEnabled;
  if Result and DisableEventsOnLoading then
    Result := not (csLoading in ComponentState);
end;

end.
