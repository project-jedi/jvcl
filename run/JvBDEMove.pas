{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBMove.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  database batchmove

History:
  1.23 - added suport for table names with extensions;

 Note: All referenced fields MUST be Integer

 Example :
  Source = dbChildCompany
  Destination = dbCompany
  Tables = (
    Employee
    Children
  );
  References = (
    Children.Employee = Employee.Uni
  );
  TempTable = '_RATMP1_.DB';
  BeforePost = user defined unique generation procedure;

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBDEMove;

{$I jvcl.inc}

interface

uses
  Classes, DB, DBTables,
  JvComponent;

type
  TJvDBMove = class;
  TMoveAction = (maMove, maMap, maIgnore);

  TMoveEvent = procedure(Sender: TJvDBMove; Table: TTable; var Action: TMoveAction) of object;

  TJvDBMove = class(TJvComponent)
  private
    FSource: string;
    FDestination: string;
    FSTable: TTable;
    FDTable: TTable;
    FTempTable: string;
    FRTable: TTable; { temporary table }
    FTables: TStringList;
    FReferences: TStringList;
    FMappings: TStringList;
    FFieldRefs: TList;

    FProgress: Boolean;
    FRecordCount: Integer;
    FCurrentRecord: Integer;
    FErrorCount: Integer;
    FErrorBlobCount: Integer;
    FMaxPass: Integer;

    FOnMoveRecord: TMoveEvent;
    FOnPostError: TDataSetErrorEvent;

    procedure DoMove;
    function GetTables: TStrings;
    function GetReferences: TStrings;
    function GetMappings: TStrings;
    procedure SetTables(Value: TStrings);
    procedure SetReferences(Value: TStrings);
    procedure SetMappings(Value: TStrings);
    procedure CreateTmpTable;
    procedure CompileReferences;
    function Map(const TableName, FieldName: string): string;
    procedure CompatTables;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    property RecordCount: Integer read FRecordCount;
    property CurrentRecord: Integer read FCurrentRecord;
    property ErrorCount: Integer read FErrorCount;
    property ErrorBlobCount: Integer read FErrorBlobCount;
  published
    property Source: string read FSource write FSource;
    property Destination: string read FDestination write FDestination;
    property Tables: TStrings read GetTables write SetTables;
    property TempTable: string read FTempTable write FTempTable;
    property References: TStrings read GetReferences write SetReferences;
    property Mappings: TStrings read GetMappings write SetMappings;
    property OnMoveRecord: TMoveEvent read FOnMoveRecord write FOnMoveRecord;
    property OnPostError: TDataSetErrorEvent read FOnPostError write FOnPostError;
    property Progress: Boolean read FProgress write FProgress default False;
  end;

  EJvDBMoveError = class(EDatabaseError);

implementation

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils, BDE,
  JvBDEUtils, JvResources;

const
  cTable = 'Table';
  cField = 'Field';
  cOldValue = 'OldValue';
  cNewValue = 'NewValue';

type
  TFieldRef = class(TObject)
  private
    STableName: string;
    SFieldName: string;
    STableIndex: Integer;
    SFieldIndex: Integer;
    DTFieldIndex: Integer;
    MasterRef: Boolean;
    DTableName: string;
    DFieldName: string;
    DTableIndex: Integer;
    DFieldIndex: Integer;
  end;

function CmdString(S: string): Boolean;
begin
  S := Trim(S);
  Result := (S <> '') and (S[1] <> ';');
end;

constructor TJvDBMove.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTables := TStringList.Create;
  FReferences := TStringList.Create;
  FMappings := TStringList.Create;
  FFieldRefs := TList.Create;
  FTempTable := '_RATMP1_';
  FMaxPass := 1;
end;

destructor TJvDBMove.Destroy;
begin
  FTables.Free;
  FReferences.Free;
  FMappings.Free;
  FFieldRefs.Free;
  inherited Destroy;
end;

function TJvDBMove.GetTables: TStrings;
begin
  Result := FTables;
end;

procedure TJvDBMove.SetTables(Value: TStrings);
begin
  FTables.Assign(Value);
  CompatTables;
end;

procedure TJvDBMove.CompatTables;
var
  I: Integer;
begin
  { make compatible with previous version of TJvDBMove }
  for I := 0 to FTables.Count - 1 do
    if FTables[I] <> '' then
      FTables[I] := Trim(SubStr(FTables[I], 0, '='));
end;

function TJvDBMove.GetReferences: TStrings;
begin
  Result := FReferences;
end;

procedure TJvDBMove.SetReferences(Value: TStrings);
begin
  FReferences.Assign(Value);
end;

function TJvDBMove.GetMappings: TStrings;
begin
  Result := FMappings;
end;

procedure TJvDBMove.SetMappings(Value: TStrings);
begin
  FMappings.Assign(Value);
end;

function TJvDBMove.Map(const TableName, FieldName: string): string;
begin
  if FieldName = '' then
  begin
    Result := FMappings.Values[TableName];
    if Result = '' then
      Result := TableName;
  end
  else
  begin
    Result := SubStrEnd(FMappings.Values[ChangeFileExt(TableName, '') +
      '.' + FieldName], 0, '.');
    if Result = '' then
      Result := FieldName;
  end;
end;

procedure TJvDBMove.CreateTmpTable;
begin
  with FRTable do
  begin
    Active := False; { The Table component must not be active }
    { First, describe the type of table and give it a name }
    DatabaseName := FDestination;
    TableType := ttDefault;
    TableName := FTempTable;
    { Next, describe the fields in the table }
    with FieldDefs do
    begin
      Clear;
      Add(cTable, ftInteger, 0, True);
      Add(cField, ftInteger, 0, True);
      Add(cOldValue, ftInteger, 0, True);
      Add(cNewValue, ftInteger, 0, True);
    end;
    { Next, describe any indexes }
{    with IndexDefs do
    begin
      Clear;
      Add('', cTable + ';' + cField + ';' + cOldValue, [ixPrimary, ixUnique]);
    end;
   }{ Now that we have specified what we want, create the table }
    CreateTable;
  end;
end;

procedure TJvDBMove.Execute;

  procedure CalcRecords;
  var
    I: Integer;
  begin
    FRecordCount := 0;
    FCurrentRecord := 0;
    for I := 0 to FTables.Count - 1 do
      if CmdString(FTables[I]) then
      begin
        FSTable.Close;
        FSTable.TableName := FTables[I];
        FSTable.Open;
        Inc(FRecordCount, FSTable.RecordCount);
      end;
  end;

begin
  CompatTables;
  FSTable := TTable.Create(Self);
  FDTable := TTable.Create(Self);
  FRTable := TTable.Create(Self);
  try
    FSTable.DatabaseName := FSource;
    FDTable.DatabaseName := FDestination;
    FRecordCount := -1;
    if FProgress then
      CalcRecords;
    CreateTmpTable;
    try
      FRTable.Open;
      CompileReferences;
      FDTable.OnPostError := FOnPostError;
      DoMove;
    finally
      FRTable.Close;
      FRTable.DeleteTable;
    end;
  finally
    FSTable.Free;
    FDTable.Free;
    FRTable.Free;
  end;
end;

procedure TJvDBMove.CompileReferences;
var
  I, J: Integer;
  S: string;
  Master, Detail: string;
  FieldRef: TFieldRef;
begin
  FFieldRefs.Clear;
  for I := 0 to FReferences.Count - 1 do
  begin
    S := FReferences[I];
    if CmdString(S) then
    begin
      Detail := SubStr(S, 0, '=');
      Master := SubStr(S, 1, '=');
      if (Detail = '') or (Pos('.', Detail) = 0) or
        (Master = '') or (Pos('.', Master) = 0) then
        raise EJvDBMoveError.CreateRes(@RsEInvalidReferenceDescriptor);
      FieldRef := TFieldRef.Create;
      FieldRef.STableName := Trim(SubStr(Master, 0, '.'));
      FieldRef.SFieldName := Trim(SubStr(Master, 1, '.'));
      FieldRef.DTableName := Trim(SubStr(Detail, 0, '.'));
      FieldRef.DFieldName := Trim(SubStr(Detail, 1, '.'));
      FieldRef.STableIndex := -1;
      FieldRef.STableIndex := -1;
      FieldRef.SFieldIndex := -1;
      FieldRef.DFieldIndex := -1;
      FieldRef.DTFieldIndex := -1;
      FieldRef.MasterRef := True;
      for J := 0 to FFieldRefs.Count - 1 do
        with TFieldRef(FFieldRefs[J]) do
          if Cmp(STableName, FieldRef.STableName) and
            Cmp(SFieldName, FieldRef.SFieldName) then
          begin
            FieldRef.MasterRef := False;
            Break;
          end;
      FFieldRefs.Add(FieldRef);
    end;
  end;
end;

procedure TJvDBMove.DoMove;
type
  TRef = record
    IsRef: Boolean;
    Value: Integer;
    HasRef: Boolean;
  end;
var
  MasterFields: array [0..1023] of TRef; // Max_Columns
  HasMaster, HasDetail: Boolean;
  AllFixups: Boolean;
  I, TableIndex: Integer;
 // Er : Integer;

  procedure UpdateRefList(ATableIndex: Integer);
  var
    I, F: Integer;
  begin
    FillChar(MasterFields, SizeOf(MasterFields), 0);
    for I := 0 to FFieldRefs.Count - 1 do
      with TFieldRef(FFieldRefs[I]) do
      begin
        if Cmp(STableName, ChangeFileExt(FSTable.TableName, '')) then
        begin
          STableIndex := ATableIndex;
          for F := 0 to FSTable.FieldCount - 1 do
            if Cmp(SFieldName, FSTable.Fields[F].FieldName) then
            begin
              SFieldIndex := F;
              DTFieldIndex := FDTable.FieldByName(
                Map(FSTable.TableName, FSTable.Fields[SFieldIndex].FieldName)).Index;
              MasterFields[F].IsRef := True;
              HasMaster := True;
            end;
        end;
        if Cmp(Map(DTableName, ''), ChangeFileExt(FDTable.TableName, '')) then
        begin
          DTableIndex := ATableIndex;
          for F := 0 to FDTable.FieldCount - 1 do
            if Cmp(Map(DTableName, DFieldName), FDTable.Fields[F].FieldName) then
            begin
              DFieldIndex := F;
              MasterFields[F].HasRef := True;
              HasDetail := True;
            end;
        end;
      end;
  end;

  procedure AppendRef(TableIndex: Integer);
  var
    I: Integer;
  begin
    for I := 0 to FFieldRefs.Count - 1 do
      with TFieldRef(FFieldRefs[I]) do
        if MasterRef and (STableIndex = TableIndex) then
        try
          FRTable.AppendRecord([TableIndex + 1, SFieldIndex + 1,
            MasterFields[SFieldIndex].Value,
              FDTable.Fields[DTFieldIndex].AsVariant]);
        except;
        end;
  end;

  function FixupRef(TableIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to FFieldRefs.Count - 1 do
      with TFieldRef(FFieldRefs[I]) do
        if (DTableIndex = TableIndex) and
          (DFieldIndex <> -1) and
          (FDTable.Fields[DFieldIndex].AsVariant <> Null) then
        begin
         { FDTable.Fields[DFieldIndex].AsVariant :=
            FRTable.Lookup(cTable + ';' + cField + ';' + cOldValue, VarArrayOf([
              STableIndex + 1,
              SFieldIndex + 1,
              FDTable.Fields[DFieldIndex].AsVariant]),
              cNewValue); }
          if FRTable.Locate(cTable + ';' + cField + ';' + cOldValue,
            VarArrayOf([STableIndex + 1, SFieldIndex + 1,
              FDTable.Fields[DFieldIndex].AsVariant]), []) then
            FDTable.Fields[DFieldIndex].AsVariant := FRTable[cNewValue]
          else
          begin
           // record not found, may be in second pass
            AllFixups := False;
            Result := False;
            Inc(FErrorCount);
            Exit;
          end;
        end;
    Result := True;
  end;

  procedure MoveRecord(TableIndex: Integer);
  var
    F: Integer;
    Action: TMoveAction;

    procedure MoveField(FieldIndex: Integer);
    begin
      try
        FDTable.FieldByName(Map(FSTable.TableName,
          FSTable.Fields[FieldIndex].FieldName)).AsVariant :=
          FSTable.Fields[FieldIndex].AsVariant;
      except
        on E: EDBEngineError do
          if E.Errors[0].ErrorCode = DBIERR_BLOBMODIFIED then
          begin
            Inc(FErrorCount);
            Inc(FErrorBlobCount);
          end
          else
            raise;
      end;
    end;

  begin
    FDTable.Append;
    try
      for F := 0 to FSTable.FieldCount - 1 do
        if FDTable.FindField(Map(FSTable.TableName,
          FSTable.Fields[F].FieldName)) <> nil then
        begin
          MoveField(F);
          if MasterFields[F].IsRef then
            MasterFields[F].Value := FSTable.Fields[F].AsInteger;
        end;
      Action := maMove;
      if HasDetail and not FixupRef(TableIndex) then
        Action := maIgnore;
      if (Action = maMove) and Assigned(FOnMoveRecord) then
        FOnMoveRecord(Self, FDTable, Action);
      if HasMaster and (Action in [maMove, maMap]) then
        AppendRef(TableIndex);
      if Action = maMove then
      try
        FDTable.Post
      except
        on E: EAbort do
        begin
          FDTable.Cancel;
          Inc(FErrorCount);
        end;
      end
      else
        FDTable.Cancel;
    except
      on E: EAbort do
        raise
    else
      if FDTable.State = dsInsert then
        FDTable.Cancel;
     // raise;
    end;
  end;

  procedure MoveTable(TableIndex: Integer);
  begin
    FSTable.Close;
    FDTable.Close;
    FSTable.TableName := FTables[TableIndex];
    FDTable.TableName := Map(FTables[TableIndex], '');
    FSTable.Open;
    FDTable.Open;
    UpdateRefList(TableIndex);
    while not FSTable.Eof do
    begin
      try
        Inc(FCurrentRecord);
        MoveRecord(TableIndex);
      except
        //
        raise;
      end;
      FSTable.Next;
    end;
  end;

begin
  FCurrentRecord := 0;
  FErrorCount := 0;
  FErrorBlobCount := 0;
  for I := 0 to FTables.Count - 1 do
    if CmdString(FTables[I]) then
    begin
     { in Tables list one table can be appear more than once,
       but we must use one TableIndex for all appearance }
      TableIndex := FTables.IndexOf(FTables[I]);
     // if (TableIndex = I) or not AllFixups then
      begin
        AllFixups := True;
        MoveTable(TableIndex);
      end;
     { if TableIndex = I then
        Er := FErrorCount else
        FErrorCount := Er; }
    end;
end;

end.

