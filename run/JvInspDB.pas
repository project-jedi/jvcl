{******************************************************************************

 Project JEDI Visible Component Library (J-VCL)

 The contents of this file are subject to the Mozilla Public License Version
 1.1 (the "License"); you may not use this file except in compliance with the
 License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 the specific language governing rights and limitations under the License.

 The Initial Developer of the Original Code is Marcel Bestebroer
  <marcelb@zeelandnet.nl>.
 Portions created by Marcel Bestebroer are Copyright (C) 2000 - 2002 mbeSoft.
 All Rights Reserved.

******************************************************************************

 JvInspector data layer to inspect TField instances.

 You may retrieve the latest version of this file at the Project JEDI home
 page, located at http://www.delphi-jedi.org

******************************************************************************}

{$I jvcl.inc}

unit JvInspDB;

interface

uses
  SysUtils, Classes, DB, TypInfo, 
  {$IFDEF VCL}
  DBCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QDBCtrls,
  {$ENDIF VisualCLX}
  JvInspector, JvFinalize;

type
  TJvInspectorDBData = class(TJvCustomInspectorData)
  private
    FDataLink: TFieldDataLink;
  protected
    procedure ActiveChange(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    procedure EditingChange(Sender: TObject); virtual;
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function GetDataSource: TDataSource; virtual;
    function GetField: TField; virtual;
    function GetFieldName: string; virtual;
    procedure InitDB(const ADataSource: TDataSource; const AFieldName: string); virtual;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetDataSource(const Value: TDataSource); virtual;
    procedure SetField(const Value: TField); virtual;
    procedure SetFieldName(const Value: string); virtual;
    property DataLink: TFieldDataLink read FDataLink;
  public
    class function New(const AParent: TJvCustomInspectorItem; const ADataSource: TDataSource;
      const AFieldName: string): TJvCustomInspectorItem; overload;
    class function New(const AParent: TJvCustomInspectorItem;
      const ADataSource: TDataSource): TJvInspectorItemInstances; overload;
    class function New(const AParent: TJvCustomInspectorItem; const ADataSource: TDataSource;
      const AFieldNames: array of string): TJvInspectorItemInstances; overload;
    destructor Destroy; override;
    class function FieldTypeMapping: TJvInspectorRegister;
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function ItemRegister: TJvInspectorRegister; override;
    procedure SetAsSet(const Buf); override;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Field: TField read GetField write SetField;
    property FieldName: string read GetFieldName write SetFieldName;
  end;

  TJvInspectorTFieldTypeRegItem = class(TJvCustomInspectorRegItem)
  private
    FFieldName: string;
    FFieldTable: string;
    FFieldType: TFieldType;
    FTypeInfo: PTypeInfo;
  public
    constructor Create(const AFieldName, AFieldTable: string; const AFieldType: TFieldType;
       ATypeInfo: PTypeInfo);
    function MatchValue(const ADataObj: TJvCustomInspectorData): Integer; override;
    function MatchPercent(const ADataObj: TJvCustomInspectorData): Integer; override;
    property FieldName: string read FFieldName;
    property FieldTable: string read FFieldTable;
    property FieldType: TFieldType read FFieldType;
    property TypeInfo: PTypeInfo read FTypeInfo;
  end;

function GetTableName(const AField: TField): string;
function GetFieldName(const AField: TField): string;

implementation

uses
  {$IFDEF VCL}
  Consts,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QConsts,
  {$ENDIF VisualCLX}
  JvResources;

const
  sUnitName = 'JvInspDB';

var
  GlobalDBReg: TJvInspectorRegister = nil;
  GlobalMapReg: TJvInspectorRegister = nil;

function GetTableName(const AField: TField): string;
begin
  if AField.Origin <> '' then
  begin
    Result := AField.Origin;
    Delete(Result, Pos('.', Result) + 1, Length(Result));
  end
  else
    Result := '';
end;

function GetFieldName(const AField: TField): string;
begin
  if AField.Origin <> '' then
  begin
    Result := AField.Origin;
    Delete(Result, 1, Pos('.', Result));
    if Result = '' then
      Result := AField.FieldName;
  end
  else
    Result := AField.FieldName;
end;

//=== TJvInspectorDBData =====================================================

destructor TJvInspectorDBData.Destroy;
begin
  inherited Destroy;
  DataLink.Free;
  FDataLink := nil;
end;

procedure TJvInspectorDBData.ActiveChange(Sender: TObject);
begin
  DoneEdits(True);
{  if Item.Editing then
    Item.DoneEdit(True);}
  Invalidate;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and DataSource.DataSet.Active then
    InitEdits;
{  if (DataSource <> nil) and (DataSource.DataSet <> nil) and DataSource.DataSet.Active and
      (Item.Inspector.FocusedItem <> nil) then
    Item.Inspector.FocusedItem.InitEdit;}
end;

procedure TJvInspectorDBData.DataChange(Sender: TObject);
begin
  if (DataLink <> nil) and (DataLink.Field <> nil) then
  begin
    RefreshEdits;
{    if Item.Editing then
    begin
      Item.DoneEdit(True);
      Item.InitEdit;
    end;}
    InvalidateData;
    Invalidate;
  end;
end;

procedure TJvInspectorDBData.EditingChange(Sender: TObject);
begin
  Invalidate;
end;

function TJvInspectorDBData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkFloat then
    Result := Field.AsFloat
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

function TJvInspectorDBData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  if TypeInfo.Kind = tkInt64 then
    Result := TLargeIntField(Field).AsLargeInt
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

function TJvInspectorDBData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

function TJvInspectorDBData.GetAsOrdinal: Int64;
begin
  CheckReadAccess;
  if Field is TBooleanField then
    Result := Ord(TBooleanField(Field).AsBoolean)
  else
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkSet, tkWChar, tkClass] then
  begin
    if GetTypeData(TypeInfo).OrdType = otULong then
      Result := Cardinal(Field.AsInteger)
    else
      Result := Field.AsInteger;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

function TJvInspectorDBData.GetAsString: string;
begin
  CheckReadAccess;
  if TypeInfo.Kind in [tkString, tkLString, tkWString] then
    Result := Field.AsString
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

function TJvInspectorDBData.GetDataSource: TDataSource;
begin
  if DataLink <> nil then
    Result := DataLink.DataSource
  else
    Result := nil;
end;

function TJvInspectorDBData.GetField: TField;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and (FieldName <> '') then
    Result := DataSource.DataSet.FindField(FieldName)
  else
    Result := nil;
end;

function TJvInspectorDBData.GetFieldName: string;
begin
  if DataLink <> nil then
    Result := DataLink.FieldName
  else
    Result := '';
end;

procedure TJvInspectorDBData.InitDB(const ADataSource: TDataSource; const AFieldName: string);
var
  MapItem: TJvCustomInspectorRegItem;
  ATypeInfo: PTypeInfo;
begin
  if DataLink = nil then
    FDataLink := TFieldDataLink.Create;
  DataLink.DataSource := ADataSource;
  DataLink.FieldName := AFieldName;
  DataLink.OnDataChange := DataChange;
  DataLink.OnActiveChange := ActiveChange;
  DataLink.OnEditingChange := EditingChange;
  MapItem := FieldTypeMapping.FindMatch(Self);
  if MapItem <> nil then
    ATypeInfo := TJvInspectorTFieldTypeRegItem(MapItem).TypeInfo
  else
    ATypeInfo := nil;
  if Field <> nil then
  begin
    Name := Field.DisplayName;
    TypeInfo := ATypeInfo;
  end
  else
  begin
    Name := AFieldName;
    TypeInfo := nil;
  end;
end;

function TJvInspectorDBData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorDBData) and (TJvInspectorDBData(Ref).Field = Field);
end;

procedure TJvInspectorDBData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkFloat then
  begin
    DataLink.Edit;
    Field.AsFloat := Value;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

procedure TJvInspectorDBData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  if TypeInfo.Kind = tkInt64 then
  begin
    DataLink.Edit;
    TLargeIntField(Field).AsLargeInt := Value;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

procedure TJvInspectorDBData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

procedure TJvInspectorDBData.SetAsOrdinal(const Value: Int64);
var
  MinValue: Int64;
  MaxValue: Int64;
begin
  CheckWriteAccess;
  if Field is TBooleanField then
  begin
    DataLink.Edit;
    TBooleanField(Field).AsBoolean := Value <> 0;
  end
  else
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkWChar] then
  begin
    case GetTypeData(TypeInfo).OrdType of
      otSByte:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          DataLink.Edit;
          Field.AsInteger := Shortint(Value)
        end;
      otUByte:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          DataLink.Edit;
          Field.AsInteger := Byte(Value)
        end;
      otSWord:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          DataLink.Edit;
          Field.AsInteger := Smallint(Value)
        end;
      otUWord:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          DataLink.Edit;
          Field.AsInteger := Word(Value)
        end;
      otSLong:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          DataLink.Edit;
          Field.AsInteger := Integer(Value)
        end;
      otULong:
        begin
          MinValue := Longword(GetTypeData(TypeInfo).MinValue);
          MaxValue := Longword(GetTypeData(TypeInfo).MaxValue);
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          DataLink.Edit;
          Field.AsInteger := Integer(Value)
        end;
    end;
  end
  else
  if TypeInfo.Kind = tkClass then
  begin
    DataLink.Edit;
    Field.AsInteger := Integer(Value);
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

procedure TJvInspectorDBData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  if TypeInfo.Kind in [tkString, tkLString, tkWString] then
  begin
    DataLink.Edit;
    Field.AsString := Value;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

procedure TJvInspectorDBData.SetDataSource(const Value: TDataSource);
var
  OrgFieldName: string;
begin
  if DataSource <> Value then
  begin
    OrgFieldName := FieldName;
    DataLink.DataSource := Value;
    if FieldName <> OrgFieldName then
      FieldName := OrgFieldName
    else
      Invalidate;
  end;
end;

procedure TJvInspectorDBData.SetField(const Value: TField);
begin
  if Field <> Value then
  begin
    TFieldDataLink(DataLink).FieldName := Value.FieldName;
    Invalidate;
  end;
end;

procedure TJvInspectorDBData.SetFieldName(const Value: string);
begin
  if FieldName <> Value then
  begin
    TFieldDataLink(DataLink).FieldName := Value;
    Invalidate;
  end;
end;

procedure RegisterDBTypes; forward;

class function TJvInspectorDBData.FieldTypeMapping: TJvInspectorRegister;
begin
  if GlobalMapReg = nil then
  begin
    GlobalMapReg := TJvInspectorRegister.Create(TJvCustomInspectorData);
    AddFinalizeObjectNil(sUnitName, TObject(GlobalMapReg));
    RegisterDBTypes; // register
  end;
  Result := GlobalMapReg;
end;

procedure TJvInspectorDBData.GetAsSet(var Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  TmpInt: Integer;
begin
  CheckReadAccess;
  if TypeInfo.Kind <> tkSet then
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
  CompType := GetTypeData(TypeInfo).CompType^;
  EnumMin := GetTypeData(CompType).MinValue;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  if ResBytes > 4 then
    ResBytes := 4;
  TmpInt := Field.AsInteger;
  Move(TmpInt, Buf, ResBytes);
end;

function TJvInspectorDBData.HasValue: Boolean;
begin
  Result := IsInitialized and (DataSource <> nil) and (DataSource.DataSet <> nil) and
    DataSource.DataSet.Active;
end;

function TJvInspectorDBData.IsAssigned: Boolean;
begin
  Result := IsInitialized and not Field.IsNull;
end;

function TJvInspectorDBData.IsInitialized: Boolean;
begin
  Result := (DataLink <> nil) and (DataSource <> nil) and (Field <> nil);
end;

class function TJvInspectorDBData.ItemRegister: TJvInspectorRegister;
begin
  if GlobalDBReg = nil then
  begin
    GlobalDBReg := TJvInspectorRegister.Create(TJvInspectorDBData);
    AddFinalizeObjectNil(sUnitName, TObject(GlobalDBReg));
  end;
  Result := GlobalDBReg;
end;

class function TJvInspectorDBData.New(const AParent: TJvCustomInspectorItem;
  const ADataSource: TDataSource; const AFieldName: string): TJvCustomInspectorItem;
var
  Data: TJvInspectorDBData;
begin
  Data := CreatePrim('', nil);
  Data.InitDB(ADataSource, AFieldName);
  Data := TJvInspectorDBData(RegisterInstance(Data));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

class function TJvInspectorDBData.New(const AParent: TJvCustomInspectorItem;
  const ADataSource: TDataSource): TJvInspectorItemInstances;
var
  DS: TDataSet;
  IArr: Integer;
  I: Integer;
  TmpItem: TJvCustomInspectorItem;
begin
  SetLength(Result, ADataSource.DataSet.FieldCount);
  DS := ADataSource.DataSet;
  IArr := 0;
  for I := 0 to DS.FieldCount - 1 do
  begin
    TmpItem := New(AParent, ADataSource, DS.Fields[I].FieldName);
    if TmpItem <> nil then
    begin
      Result[IArr] := TmpItem;
      Inc(IArr);
    end;
  end;
  SetLength(Result, IArr);
end;

class function TJvInspectorDBData.New(const AParent: TJvCustomInspectorItem;
  const ADataSource: TDataSource; const AFieldNames: array of string): TJvInspectorItemInstances;
var
  IArr: Integer;
  I: Integer;
  TmpItem: TJvCustomInspectorItem;
begin
  SetLength(Result, Length(AFieldNames));
  IArr := 0;
  for I := Low(AFieldNames) to High(AFieldNames) do
  begin
    TmpItem := New(AParent, ADataSource, AFieldNames[I]);
    if TmpItem <> nil then
    begin
      Result[IArr] := TmpItem;
      Inc(IArr);
    end;
  end;
end;

procedure TJvInspectorDBData.SetAsSet(const Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  TmpInt: Integer;
begin
  CheckWriteAccess;
  if TypeInfo.Kind <> tkSet then
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
  CompType := GetTypeData(TypeInfo).CompType^;
  EnumMin := GetTypeData(CompType).MinValue;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  if ResBytes > 4 then
    ResBytes := 4;
  TmpInt := 0;
  Move(Buf, TmpInt, ResBytes);
  DataLink.Edit;
  Field.AsInteger := TmpInt;
end;

//=== TJvInspectorTFieldTypeRegItem ==========================================

constructor TJvInspectorTFieldTypeRegItem.Create(const AFieldName, AFieldTable: string;
  const AFieldType: TFieldType;  ATypeInfo: PTypeInfo);
begin
  inherited Create(nil);
  FFieldName := AFieldName;
  FFieldTable := AFieldTable;
  FFieldType := AFieldType;
  FTypeInfo := ATypeInfo;
end;

function TJvInspectorTFieldTypeRegItem.MatchValue(const ADataObj: TJvCustomInspectorData): Integer;
var
  ThisField: TField;
  GoOn: Boolean;
  ThisTableName: string;
  ThisFieldName: string;
begin
  { Determine value as follows:

    Base value = 0
    * FieldType specified:
      * no match:               return 0
      * match:                  add 16
    * FieldName specified:
      * no match:               return 0
      * matches by mask:        add 4
      * exact match:            add 8
    * FieldTable specified:
      * no match:               return 0
      * matches by mask:        add 1
      * exact match:            add 2 }
  ThisField := (ADataObj as TJvInspectorDBData).Field;
  Result := 0;
  GoOn := ThisField <> nil;
  if GoOn then
  begin
    ThisTableName := GetTableName(ThisField);
    ThisFieldName := GetFieldName(ThisField);
    if FieldType <> ftUnknown then
    begin
      if FieldType = ThisField.DataType then
        Result := Result or 16
      else
        GoOn := False;
    end;
    if GoOn and (FieldName <> '') then
    begin
      if AnsiSameText(FieldName, ThisFieldName) then
        Result := Result or 8
      else
        GoOn := False;
    end;
    if GoOn and (FieldTable <> '') then
    begin
      if AnsiSameText(FieldTable, ThisTableName) then
        Result := Result or 2
      else
        GoOn := False;
    end;
  end;
  if not GoOn then
    Result := 0;
  if FieldType = ftUnknown then
    Result := 1;
end;

function TJvInspectorTFieldTypeRegItem.MatchPercent(const ADataObj: TJvCustomInspectorData): Integer;
begin
  if IsMatch(ADataObj) then
    ADataObj.TypeInfo := TypeInfo;
  if (FieldType = ftUnknown) and (ADataObj.TypeInfo = nil) then
    Result := 100 // terminate the search now
  else
    Result := 0; // Make sure the other items are searched as well
end;

procedure RegisterDBTypes;
begin
  with TJvInspectorDBData.FieldTypeMapping do
  begin
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftString, System.TypeInfo(string)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftSmallint, System.TypeInfo(Smallint)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftInteger, System.TypeInfo(Integer)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftWord, System.TypeInfo(Word)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftBoolean, System.TypeInfo(Boolean)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftFloat, System.TypeInfo(Double)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftCurrency, System.TypeInfo(Currency)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftBCD, System.TypeInfo(Currency)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftDate, System.TypeInfo(TDateTime)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftTime, System.TypeInfo(TDateTime)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftDateTime, System.TypeInfo(TDateTime)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftAutoInc, System.TypeInfo(Integer)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftMemo, System.TypeInfo(string)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftFmtMemo, System.TypeInfo(string)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftFixedChar, System.TypeInfo(string)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftWideString, System.TypeInfo(WideString)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftLargeint, System.TypeInfo(Int64)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftVariant, System.TypeInfo(Variant)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftInterface, System.TypeInfo(IUnknown)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftIDispatch, System.TypeInfo(IDispatch)));
    Add(TJvInspectorTFieldTypeRegItem.Create('', '', ftGUID, System.TypeInfo(string)));
  end;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

