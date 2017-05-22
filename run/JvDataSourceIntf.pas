{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataSourceIntf.PAS, released on 08-07-2006.

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

unit JvDataSourceIntf;

{$I jvcl.inc}

interface

uses
  {$IFNDEF DelphiPersonalEdition}
  DB,
  {$ENDIF !DelphiPersonalEdition}
  SysUtils, Classes, Contnrs;

const
  DC_ACTIVECHANGED = 100;
  DC_RECORDCHANGED = 101;
  DC_UPDATEDATA = 102;
  DC_LAYOUTCHANGED = 103;
  DC_DATASETCHANGED = 104;
  DC_DATASETSCROLLED = 105;
  DC_EDITINGCHANGED = 106;
  DC_CHECKBROWSEMODE = 107;

type
  // To avoid ambiguities in BCB when used with the Pro and above SKUs, the
  // DB related types are simply mapped to those from the DB unit.
  // Of course, in the case of a Personal edition, they have to be fully
  // declared.
  {$IFDEF DelphiPersonalEdition}
  TJvDataSetState = (dsInactive, dsBrowse, dsEdit, dsInsert, dsSetKey,
    dsCalcFields, dsFilter, dsNewValue, dsOldValue, dsCurValue, dsBlockRead,
    dsInternalCalc, dsOpening);
  {$ELSE}
  TJvDataSetState = TDataSetState;
  {$ENDIF DelphiPersonalEdition}

  {$IFDEF DelphiPersonalEdition}
  TJvDBFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, // 5..11
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, // 12..18
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, // 19..24
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
    ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval); // 38..41
  {$ELSE}
  TJvDBFieldType = TFieldType;
  {$ENDIF DelphiPersonalEdition}

  {$IFDEF DelphiPersonalEdition}
  TJvDBLocateOption = (loCaseInsensitive, loPartialKey);
  {$ELSE}
  TJvDBLocateOption = TLocateOption;
  {$ENDIF DelphiPersonalEdition}
  TJvDBLocateOptions = set of TJvDBLocateOption;

  {$IFDEF COMPILER10_UP}
  TDataFieldString = WideString;
  {$ELSE}
  TDataFieldString = string;
  {$ENDIF COMPILER10_UP}
  TJvEditMask = string;

  TJvDataConnectorMsg = record
    Msg: Integer;
  end;

  TJvDataConnector = class;

  IJvDataSourceConnectorHandler = interface
    ['{CCAB936A-6CB4-4047-95C8-7EBFC5DC4B9F}']
    procedure AddDataConnector(DataConnector: TJvDataConnector);
    procedure RemoveDataConnector(DataConnector: TJvDataConnector);
  end;

  IJvDataSource = interface
    ['{6F0ECE0E-0B77-4EC1-8E62-1C5ADE76D6B9}']

    { DataSource }
    procedure Edit; // DataSource.Edit
    function GetState: TJvDataSetState;
    function GetAutoEdit: Boolean;
    function GetEnabled: Boolean;

    property State: TJvDataSetState read GetState;
    property AutoEdit: Boolean read GetAutoEdit;
    property Enabled: Boolean read GetEnabled;

    { DataSet }
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
    property DataSet: TObject read GetDataSet;

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
  end;

  TJvDataConnectorField = class(TObject)
  private
    FDataSource: IJvDataSource;
    FField: TObject;
    FFieldName: TDataFieldString;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetDataType: TJvDBFieldType;
    function GetDisplayLabel: TDataFieldString;
    function GetDisplayWidth: Integer;
    function GetEditMask: TJvEditMask;
    function GetIsBlob: Boolean;
    function GetIsNull: Boolean;
    function GetOldValue: Variant;
    function GetReadOnly: Boolean;
    function GetRequired: Boolean;
    function GetSize: Integer;
    function GetValue: Variant;
    function GetVisible: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsInteger(const Value: Integer);
    procedure SetDisplayWidth(const Value: Integer);
    procedure SetValue(const Value: Variant);
    procedure SetDataSource(const Value: IJvDataSource);
    procedure SetFieldName(const Value: TDataFieldString);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsWideString: WideString;
    procedure SetAsWideString(const Value: WideString);
    function GetIsValid: Boolean;
    function GetCanModify: Boolean;
  protected
    procedure UpdateField(const ADataSource: IJvDataSource);
    property DataSource: IJvDataSource read FDataSource write SetDataSource;
  public
    procedure Clear;
    property IsValid: Boolean read GetIsValid; // False if Field = nil

    property Field: TObject read FField;
    property FieldName: TDataFieldString read FFieldName write SetFieldName;
    property DataType: TJvDBFieldType read GetDataType;
    property Size: Integer read GetSize;
    property DisplayLabel: TDataFieldString read GetDisplayLabel;
    property DisplayWidth: Integer read GetDisplayWidth write SetDisplayWidth;
    property EditMask: TJvEditMask read GetEditMask;
    property ReadOnly: Boolean read GetReadOnly;
    property Visible: Boolean read GetVisible;
    property Required: Boolean read GetRequired;
    property IsNull: Boolean read GetIsNull;
    property IsBlob: Boolean read GetIsBlob;
    property CanModify: Boolean read GetCanModify;

    property OldValue: Variant read GetOldValue;
    property Value: Variant read GetValue write SetValue;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
  end;

  TJvDataConnector = class(TPersistent)
  private
    FDataSource: IJvDataSource;
    FLockRecordChange: Integer;
    FModified: Boolean;
    FActive: Boolean;
    FMaster: TJvDataConnector;
    FFields: TObjectList;

    procedure DcRecordChanged(var Msg: TJvDataConnectorMsg); message DC_RECORDCHANGED;
    procedure DcActiveChanged(var Msg: TJvDataConnectorMsg); message DC_ACTIVECHANGED;
    procedure DcUpdateData(var Msg: TJvDataConnectorMsg); message DC_UPDATEDATA;
    procedure DcLayoutChanged(var Msg: TJvDataConnectorMsg); message DC_LAYOUTCHANGED;

    procedure SetDataSource(const Value: IJvDataSource);
    function GetDataSetConnected: Boolean;
    function GetField(Index: Integer): TJvDataConnectorField;
    function GetFieldCount: Integer;
    function GetFieldField(Field: TObject): TJvDataConnectorField;
  protected
    property Master: TJvDataConnector read FMaster write FMaster;

      { DataSourceConnected is invoked when a new DataSource is assigned. }
    procedure DataSourceConnected; virtual;
      { DataSourceDisconnected is invoked when the DataSource is destroyed or
        the DataSource is set to a new DataSource or NIL. }
    procedure DataSourceDisconnected; virtual;

    procedure ActiveChanged; virtual;
    procedure RecordChanged; virtual;
    procedure UpdateData; virtual;
    procedure LayoutChanged; virtual;

    procedure Notify(Msg: Integer); virtual;

    function CanEdit: Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDataLink: TObject;

    function FieldByName(const FieldName: TDataFieldString): TJvDataConnectorField;
    function FindField(const FieldName: TDataFieldString): TJvDataConnectorField;

    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TJvDataConnectorField read GetField;

    procedure Edit;
    procedure Reset;
    procedure UpdateRecord;
    procedure Modify;
    property DataSetConnected: Boolean read GetDataSetConnected;
    property Modified: Boolean read FModified;
    property Active: Boolean read FActive write FActive;
  published
    property DataSource: IJvDataSource read FDataSource write SetDataSource;
  end;

  TJvFieldDataConnector = class(TJvDataConnector)
  private
    FField: TJvDataConnectorField;
    procedure SetDataField(const Value: TDataFieldString);
    function GetDataField: TDataFieldString;
  protected
    procedure UpdateFields; virtual;
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSourceConnected; override;
    procedure DataSourceDisconnected; override;
    function CanEdit: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Field: TJvDataConnectorField read FField;
  published
    property DataField: TDataFieldString read GetDataField write SetDataField;
  end;

  TJvKeyFieldDataConnector = class(TJvFieldDataConnector)
  private
    FKey: TJvDataConnectorField;
    function GetKeyField: TDataFieldString;
    procedure SetKeyField(const Value: TDataFieldString);
  protected
    procedure UpdateFields; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Key: TJvDataConnectorField read FKey;
  published
    property KeyField: TDataFieldString read GetKeyField write SetKeyField;
  end;

  TJvLookupDataConnector = class(TJvKeyFieldDataConnector)
  private
    FList: TJvKeyFieldDataConnector;
    function GetListField: TDataFieldString;
    function GetListSource: IJvDataSource;
    procedure SetListField(const Value: TDataFieldString);
    procedure SetListSource(const Value: IJvDataSource);
    function GetListKeyField: TDataFieldString;
    procedure SetListKeyField(const Value: TDataFieldString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property List: TJvKeyFieldDataConnector read FList;
  published
    property ListField: TDataFieldString read GetListField write SetListField;
    property ListSource: IJvDataSource read GetListSource write SetListSource;
    property ListKeyField: TDataFieldString read GetListKeyField write SetListKeyField;
  end;

implementation

//=== { TJvDataConnector } ===================================================

constructor TJvDataConnector.Create;
begin
  inherited Create;
  FFields := TObjectList.Create;
end;

destructor TJvDataConnector.Destroy;
var
  Handler: IJvDataSourceConnectorHandler;
begin
  if Assigned(FDataSource) and Supports(FDataSource, IJvDataSourceConnectorHandler, Handler) then
    Handler.RemoveDataConnector(Self);
  FDataSource := nil;
  FFields.Free;
  inherited Destroy;
end;

function TJvDataConnector.GetDataLink: TObject;
begin
  if DataSource <> nil then
    Result := DataSource.GetDataLink
  else
    Result := nil;
end;

procedure TJvDataConnector.ActiveChanged;
begin
  Notify(DC_RECORDCHANGED);
end;

procedure TJvDataConnector.RecordChanged;
begin
end;

procedure TJvDataConnector.Reset;
begin
  if DataSetConnected and Active then
  begin
    Active := False;
    try
      Notify(DC_RECORDCHANGED);
    finally
      Active := True;
    end;
    FModified := False;
  end;
end;

procedure TJvDataConnector.UpdateData;
begin
end;

procedure TJvDataConnector.LayoutChanged;
begin
  Notify(DC_RECORDCHANGED);
end;

procedure TJvDataConnector.Modify;
begin
  if Active and (FLockRecordChange = 0) then
  begin
    Edit;
    FModified := True;
  end;
end;

procedure TJvDataConnector.Notify(Msg: Integer);
var
  M: TJvDataConnectorMsg;
begin
  M.Msg := Msg;
  Dispatch(M);
end;

procedure TJvDataConnector.Assign(Source: TPersistent);
begin
  if Source is TJvDataConnector then
    DataSource := TJvDataConnector(Source).DataSource
  else
    inherited Assign(Source);
end;

function TJvDataConnector.CanEdit: Boolean;
begin
  Result := Active and Assigned(DataSource);
end;

procedure TJvDataConnector.DataSourceConnected;
begin
  FModified := False;
  FActive := True;
  Notify(DC_ACTIVECHANGED);
end;

procedure TJvDataConnector.DataSourceDisconnected;
begin
  Notify(DC_ACTIVECHANGED);
  FActive := False;
  FModified := False;
  FFields.Clear;
end;

procedure TJvDataConnector.DcActiveChanged(var Msg: TJvDataConnectorMsg);
begin
  if FLockRecordChange = 0 then
  begin
    ActiveChanged;
    FModified := False;
  end;
  if Assigned(FMaster) then
    FMaster.Dispatch(Msg);
end;

procedure TJvDataConnector.DcLayoutChanged(var Msg: TJvDataConnectorMsg);
begin
  FFields.Clear;
  if FLockRecordChange = 0 then
  begin
    LayoutChanged;
    FModified := False;
  end;
  if Assigned(FMaster) then
    FMaster.Dispatch(Msg);
end;

procedure TJvDataConnector.DcRecordChanged(var Msg: TJvDataConnectorMsg);
begin
  if FLockRecordChange = 0 then
  begin
    Inc(FLockRecordChange);
    try
      RecordChanged;
    finally
      Dec(FLockRecordChange);
    end;
    FModified := False;
  end;

  if Assigned(FMaster) then
    FMaster.Dispatch(Msg);
end;

procedure TJvDataConnector.DcUpdateData(var Msg: TJvDataConnectorMsg);
begin
  if Active then
  begin
    Inc(FLockRecordChange);
    try
      UpdateData;
    finally
      Dec(FLockRecordChange);
    end;
    FModified := False;
  end;

  if Assigned(FMaster) then
    FMaster.Dispatch(Msg);
end;

procedure TJvDataConnector.Edit;
begin
  if CanEdit and (FLockRecordChange = 0) then
  begin
    Inc(FLockRecordChange);
    try
      DataSource.Edit;
    finally
      Dec(FLockRecordChange);
    end;
  end;
end;

function TJvDataConnector.FieldByName(const FieldName: TDataFieldString): TJvDataConnectorField;
begin
  Result := GetFieldField(DataSource.FieldByName(FieldName)); // raises exception if not found
end;

function TJvDataConnector.FindField(const FieldName: TDataFieldString): TJvDataConnectorField;
begin
  Result := nil;
  if DataSource <> nil then
    Result := GetFieldField(DataSource.FindField(FieldName));
end;

function TJvDataConnector.GetDataSetConnected: Boolean;
begin
  Result := Assigned(DataSource) and (DataSource.DataSet <> nil);
end;

function TJvDataConnector.GetField(Index: Integer): TJvDataConnectorField;
begin
  Result := GetFieldField(DataSource.Fields[Index]);
end;

function TJvDataConnector.GetFieldCount: Integer;
begin
  if DataSource <> nil then
    Result := DataSource.FieldCount
  else
    Result := 0;
end;

function TJvDataConnector.GetFieldField(Field: TObject): TJvDataConnectorField;
var
  I: Integer;
begin
  Result := nil;
  if Field <> nil then
  begin
    for I := 0 to FFields.Count - 1 do
    begin
      Result := TJvDataConnectorField(FFields[I]);
      if Result.Field = Field then
        Exit;
    end;
    Result := TJvDataConnectorField.Create;
    FFields.Add(Result);
    Result.DataSource := DataSource;
    Result.FieldName := DataSource.FieldName[Field];
  end;
end;

procedure TJvDataConnector.SetDataSource(const Value: IJvDataSource);
var
  Handler: IJvDataSourceConnectorHandler;
begin
  if Value <> FDataSource then
  begin
    if Assigned(FDataSource) then
    begin
      if Supports(FDataSource, IJvDataSourceConnectorHandler, Handler) then
        Handler.RemoveDataConnector(Self);
    end;
    try
      FDataSource := nil;
      DataSourceDisconnected;
    finally
      FDataSource := Value;
      if Assigned(FDataSource) then
      begin
        if Supports(FDataSource, IJvDataSourceConnectorHandler, Handler) then
        begin
          Handler.AddDataConnector(Self);
          DataSourceConnected;
        end
        else
          FDataSource := nil;
      end;
    end;
  end;
end;

procedure TJvDataConnector.UpdateRecord;
begin
  if DataSetConnected and Modified and Active then
  begin
    Inc(FLockRecordChange);
    try
      UpdateData;
    finally
      Dec(FLockRecordChange);
    end;
    FModified := False;
  end;
end;

//=== { TJvFieldDataConnector } ==============================================

constructor TJvFieldDataConnector.Create;
begin
  inherited Create;
  FField := TJvDataConnectorField.Create;
end;

procedure TJvFieldDataConnector.ActiveChanged;
begin
  UpdateFields;
  inherited ActiveChanged;
end;

procedure TJvFieldDataConnector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvFieldDataConnector then
    DataField := TJvFieldDataConnector(Source).DataField;
end;

function TJvFieldDataConnector.CanEdit: Boolean;
begin
  Result := inherited CanEdit and Field.CanModify;
end;

procedure TJvFieldDataConnector.DataSourceConnected;
begin
  UpdateFields;
  inherited DataSourceConnected;
end;

procedure TJvFieldDataConnector.DataSourceDisconnected;
begin
  UpdateFields;
  inherited DataSourceDisconnected;
end;

destructor TJvFieldDataConnector.Destroy;
begin
  FField.Free;
  inherited Destroy;
end;

function TJvFieldDataConnector.GetDataField: TDataFieldString;
begin
  Result := FField.FieldName;
end;

procedure TJvFieldDataConnector.LayoutChanged;
begin
  UpdateFields;
  inherited LayoutChanged;
end;

procedure TJvFieldDataConnector.SetDataField(const Value: TDataFieldString);
begin
  FField.FieldName := Value;
  Notify(DC_RECORDCHANGED);
end;

procedure TJvFieldDataConnector.UpdateFields;
begin
  FField.UpdateField(DataSource);
end;

//=== { TJvDataConnectorField } ==============================================

procedure TJvDataConnectorField.SetDataSource(const Value: IJvDataSource);
begin
  if Value <> FDataSource then
    UpdateField(Value);
end;

procedure TJvDataConnectorField.SetFieldName(const Value: TDataFieldString);
begin
  if Value <> FFieldName then
  begin
    FFieldName := Value;
    UpdateField(FDataSource);
  end;
end;

procedure TJvDataConnectorField.UpdateField(const ADataSource: IJvDataSource);
begin
  FDataSource := ADataSource;
  if Assigned(DataSource) and (DataSource.DataSet <> nil) then
    FField := DataSource.FindField(FFieldName)
  else
    FField := nil;
end;

procedure TJvDataConnectorField.Clear;
begin
  DataSource.FieldClear(Field);
end;

function TJvDataConnectorField.GetAsBoolean: Boolean;
begin
  Result := DataSource.FieldBoolean[Field];
end;

function TJvDataConnectorField.GetAsDateTime: TDateTime;
begin
  Result := DataSource.FieldDataTime[Field];
end;

function TJvDataConnectorField.GetAsFloat: Double;
begin
  Result := DataSource.FieldFloat[Field];
end;

function TJvDataConnectorField.GetAsInt64: Int64;
begin
  Result := StrToInt64Def(DataSource.FieldString[Field], 0);
end;

function TJvDataConnectorField.GetAsInteger: Integer;
begin
  Result := DataSource.FieldInteger[Field];
end;

function TJvDataConnectorField.GetAsString: string;
begin
  Result := DataSource.FieldString[Field];
end;

function TJvDataConnectorField.GetAsWideString: WideString;
begin
  Result := DataSource.FieldWideString[Field];
end;

function TJvDataConnectorField.GetCanModify: Boolean;
begin
  Result := IsValid and DataSource.FieldCanModify[Field];
end;

function TJvDataConnectorField.GetDataType: TJvDBFieldType;
begin
  Result := DataSource.FieldType[Field];
end;

function TJvDataConnectorField.GetDisplayLabel: TDataFieldString;
begin
  Result := DataSource.FieldDisplayLabel[Field];
end;

function TJvDataConnectorField.GetDisplayWidth: Integer;
begin
  Result := DataSource.FieldDisplayWidth[Field];
end;

function TJvDataConnectorField.GetEditMask: TJvEditMask;
begin
  Result := DataSource.FieldEditMask[Field];
end;

function TJvDataConnectorField.GetIsBlob: Boolean;
begin
  Result := DataSource.FieldIsBlob[Field];
end;

function TJvDataConnectorField.GetIsNull: Boolean;
begin
  Result := DataSource.FieldIsNull[Field];
end;

function TJvDataConnectorField.GetIsValid: Boolean;
begin
  if (DataSource <> nil) and (Field = nil) then
    UpdateField(DataSource);
  Result := (DataSource <> nil) and (Field <> nil);
end;

function TJvDataConnectorField.GetOldValue: Variant;
begin
  Result := DataSource.FieldOldValue[Field];
end;

function TJvDataConnectorField.GetReadOnly: Boolean;
begin
  Result := DataSource.FieldReadOnly[Field];
end;

function TJvDataConnectorField.GetRequired: Boolean;
begin
  Result := DataSource.FieldRequired[Field];
end;

function TJvDataConnectorField.GetSize: Integer;
begin
  Result := DataSource.FieldSize[Field];
end;

function TJvDataConnectorField.GetValue: Variant;
begin
  Result := DataSource.FieldValue[Field];
end;

function TJvDataConnectorField.GetVisible: Boolean;
begin
  Result := DataSource.FieldVisible[Field];
end;

procedure TJvDataConnectorField.SetAsBoolean(const Value: Boolean);
begin
  DataSource.FieldBoolean[Field] := Value;
end;

procedure TJvDataConnectorField.SetAsDateTime(const Value: TDateTime);
begin
  DataSource.FieldDataTime[Field] := Value;
end;

procedure TJvDataConnectorField.SetAsFloat(const Value: Double);
begin
  DataSource.FieldFloat[Field] := Value;
end;

procedure TJvDataConnectorField.SetAsInt64(const Value: Int64);
begin
  DataSource.FieldString[Field] := IntToStr(Value);
end;

procedure TJvDataConnectorField.SetAsInteger(const Value: Integer);
begin
  DataSource.FieldInteger[Field] := Value;
end;

procedure TJvDataConnectorField.SetAsString(const Value: string);
begin
  DataSource.FieldString[Field] := Value;
end;

procedure TJvDataConnectorField.SetAsWideString(const Value: WideString);
begin
  DataSource.FieldWideString[Field] := Value;
end;

procedure TJvDataConnectorField.SetDisplayWidth(const Value: Integer);
begin
  DataSource.FieldDisplayWidth[Field] := Value;
end;

procedure TJvDataConnectorField.SetValue(const Value: Variant);
begin
  DataSource.FieldValue[Field] := Value;
end;

//=== { TJvKeyFieldDataConnector } ===========================================

constructor TJvKeyFieldDataConnector.Create;
begin
  inherited Create;
  FKey := TJvDataConnectorField.Create;
end;

destructor TJvKeyFieldDataConnector.Destroy;
begin
  FKey.Free;
  inherited Destroy;
end;

procedure TJvKeyFieldDataConnector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvKeyFieldDataConnector then
    KeyField := TJvKeyFieldDataConnector(Source).KeyField;
end;

function TJvKeyFieldDataConnector.GetKeyField: TDataFieldString;
begin
  Result := FKey.FieldName;
end;

procedure TJvKeyFieldDataConnector.SetKeyField(const Value: TDataFieldString);
begin
  FKey.FieldName := Value;
end;


procedure TJvKeyFieldDataConnector.UpdateFields;
begin
  inherited UpdateFields;
  FKey.UpdateField(DataSource);
end;

//=== { TJvLookupDataConnector } =============================================

constructor TJvLookupDataConnector.Create;
begin
  inherited Create;
  FList := TJvKeyFieldDataConnector.Create;
  FList.Master := Self;
end;

destructor TJvLookupDataConnector.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TJvLookupDataConnector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvLookupDataConnector then
    FList.Assign(Source);
end;

function TJvLookupDataConnector.GetListField: TDataFieldString;
begin
  Result := FList.DataField;
end;

function TJvLookupDataConnector.GetListKeyField: TDataFieldString;
begin
  Result := FList.KeyField;
end;

function TJvLookupDataConnector.GetListSource: IJvDataSource;
begin
  Result := FList.DataSource;
end;

procedure TJvLookupDataConnector.SetListField(const Value: TDataFieldString);
begin
  FList.DataField := Value;
end;

procedure TJvLookupDataConnector.SetListKeyField(const Value: TDataFieldString);
begin
  FList.KeyField := Value;
end;

procedure TJvLookupDataConnector.SetListSource(const Value: IJvDataSource);
begin
  FList.DataSource := Value;
end;

end.
