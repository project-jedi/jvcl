{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

component   : TJvDBMove
description : database batchmove

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

{
 history:
  1.23 - added suport for table names with extensions;


 Note: All referenced fields MUST be integer 

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
 }



unit JvDBMove;

interface

uses
  BDE, Windows, Messages, SysUtils, Classes, DB, DBTables
  {$IFDEF COMPILER6_UP}, Variants {$ENDIF}
  ;

type

  TJvDBMove = class;
  TMoveAction = (maMove, maMap, maIgnore);

  TMoveEvent = procedure(Sender : TJvDBMove; Table : TTable; var Action : TMoveAction) of object;

  TJvDBMove = class(TComponent)
  private
    SDatabase : string;
    DDatabase : string;
    STable : TTable;
    DTable : TTable;
    FTempTableName : string;
    RTable : TTable; { temporary table }
    FTables : TStrings;
    FReferences : TStrings;
    FMappings : TStrings;
    FieldRefs : TList;

    FProgress : boolean;
    FRecordCount : integer;
    FCurrentRecord : integer;
    FErrorCount : integer;
    FErrorBlobCount : integer;
    FMaxPass : integer;

    FOnMoveRecord : TMoveEvent;
    FOnPostError: TDataSetErrorEvent;

    procedure DoMove;
    procedure SetTables(Value : TStrings);
    procedure SetReferences(Value : TStrings);
    procedure SetMappings(Value : TStrings);
    procedure CreateTmpTable;
    procedure CompileReferences;
    function Map(const TableName, FieldName: string): string;
    procedure CompatTables;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    property RecordCount : integer read FRecordCount;
    property CurrentRecord : integer read FCurrentRecord;
    property ErrorCount : integer read FErrorCount;
    property ErrorBlobCount : integer read FErrorBlobCount;
  published
    property Source : string read SDatabase write SDatabase;
    property Destination : string read DDatabase write DDatabase;
    property Tables : TStrings read FTables write SetTables;
    property TempTable : string read FTempTableName write FTempTableName;
    property References : TStrings read FReferences write SetReferences;
    property Mappings: TStrings read FMappings write SetMappings;
    property OnMoveRecord : TMoveEvent read FOnMoveRecord write FOnMoveRecord;
    property OnPostError : TDataSetErrorEvent read FOnPostError write FOnPostError;
    property Progress : boolean read FProgress write FProgress default false;
  end;

  EJvDBMoveError  = class(EDatabaseError);

implementation

uses JvDBUtil;

type

  TFieldRef  = class
  private
    STableName : string;
    SFieldName : string;
    STableIndex : integer;
    SFieldIndex : integer;
    DTFieldIndex : integer;
    MasterRef : boolean;

    DTableName : string;
    DFieldName : string;
    DTableIndex : integer;
    DFieldIndex : integer;
  end;


function CmdString(S : string) : boolean;
begin
  S := Trim(S);
  Result := (S <> '') and (S[1] <> ';');
end;


constructor TJvDBMove.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FTables := TStringList.Create;
  FReferences := TStringList.Create;
  FMappings := TStringList.Create;
  FieldRefs := TList.Create;
  FTempTableName := '_RATMP1_';
  FMaxPass := 1;
end;

destructor TJvDBMove.Destroy;
begin
  FTables.Free;
  FReferences.Free;
  FMappings.Free;
  FieldRefs.Free;
  inherited Destroy;
end;

procedure TJvDBMove.SetTables(Value : TStrings);
begin
  FTables.Assign(Value);
  CompatTables;
end;

procedure TJvDBMove.CompatTables;
var
  i: integer;
begin
 { make compatible with previous version of TJvDBMove }
  for i := 0 to FTables.Count - 1 do    { Iterate }
    if FTables[i] <> '' then
      FTables[i] := Trim(SubStr(FTables[i], 0, '='));
end;    { CompatTables }

procedure TJvDBMove.SetReferences(Value : TStrings);
begin
  FReferences.Assign(Value);
end;

procedure TJvDBMove.SetMappings(Value : TStrings);
begin
  FMappings.Assign(Value);
end;

function TJvDBMove.Map(const TableName, FieldName: string): string;
begin
  if FieldName = '' then
  begin
    Result := FMappings.Values[TableName];
    if Result = '' then Result := TableName;
  end else
  begin
    Result := SubStrEnd(FMappings.Values[ChangeFileExt(TableName, '') +
      '.' + FieldName], 0, '.');
    if Result = '' then Result := FieldName;
  end;  
end;

procedure TJvDBMove.CreateTmpTable;
begin
  with RTable do
  begin
    Active := False;  { The Table component must not be active }
    { First, describe the type of table and give it a name }
    DatabaseName := DDatabase;
    TableType := ttDefault;
    TableName := FTempTableName;
    { Next, describe the fields in the table }
    with FieldDefs do
    begin
      Clear;
      Add('Table', ftInteger, 0, True);
      Add('Field', ftInteger, 0, True);
      Add('OldValue', ftInteger, 0, True);
      Add('NewValue', ftInteger, 0, True);
    end;
    { Next, describe any indexes }
{    with IndexDefs do
    begin
      Clear;
      Add('', 'Table;Field;OldValue', [ixPrimary, ixUnique]);
    end;
}    { Now that we have specified what we want, create the table }
    CreateTable;
  end;
end;

procedure TJvDBMove.Execute;

  procedure CalcRecords;
  var
    i : integer;
  begin
    FRecordCount := 0;
    FCurrentRecord := 0;
    for i := 0 to FTables.Count - 1 do
      if CmdString(FTables[i]) then
      begin
        STable.Close;
        STable.TableName := FTables[i];
        STable.Open;
        inc(FRecordCount, STable.RecordCount);
      end;
  end;

begin
  CompatTables;
  STable := TTable.Create(Self);
  DTable := TTable.Create(Self);
  RTable := TTable.Create(Self);
  try
    STable.DatabaseName := SDatabase;
    DTable.DatabaseName := DDatabase;
    FRecordCount := -1;
    if FProgress then CalcRecords;
    CreateTmpTable;
    try
      RTable.Open;
      CompileReferences;
      DTable.OnPostError := FOnPostError;
      DoMove;
    finally
      RTable.Close;
      RTable.DeleteTable;
    end;
  finally
    STable.Free;
    DTable.Free;
    RTable.Free;
  end;
end;

procedure TJvDBMove.CompileReferences;
var
  i, j : integer;
  S : string;
  Master, Detail : string;
  FieldRef : TFieldRef ;
begin
  FieldRefs.Clear;
  for i := 0 to FReferences.Count - 1 do
  begin
    S := FReferences[i];
    if CmdString(S) then
    begin
      Detail := SubStr(S, 0, '=');
      Master := SubStr(S, 1, '=');
      if (Detail = '') or (Pos('.', Detail) = 0) or
         (Master = '') or (Pos('.', Master) = 0) then
        raise EJvDBMoveError .Create('Invalid reference descriptor');
      FieldRef := TFieldRef .Create;
      FieldRef.STableName := Trim(SubStr(Master, 0, '.'));
      FieldRef.SFieldName := Trim(SubStr(Master, 1, '.'));
      FieldRef.DTableName := Trim(SubStr(Detail, 0, '.'));
      FieldRef.DFieldName := Trim(SubStr(Detail, 1, '.'));
      FieldRef.STableIndex := -1;
      FieldRef.STableIndex := -1;
      FieldRef.SFieldIndex := -1;
      FieldRef.DFieldIndex := -1;
      FieldRef.DTFieldIndex := -1;
      FieldRef.MasterRef := true;
      for j := 0 to FieldRefs.Count - 1 do
        with TFieldRef (FieldRefs[j]) do
          if Cmp(STableName, FieldRef.STableName) and
             Cmp(SFieldName, FieldRef.SFieldName) then
          begin
            FieldRef.MasterRef := false;
            break;
          end;
      FieldRefs.Add(FieldRef);
    end;
  end;
end;

procedure TJvDBMove.DoMove;

type
  TRef = record
    IsRef : boolean;
    Value : integer;
    HasRef : boolean;
  end;
var
  MasterFields : array[0..1023{Max_Columns}] of TRef;
  HasMaster, HasDetail : boolean;
  AllFixups : boolean;

  procedure UpdateRefList(ATableIndex : integer);
  var
    i, f : integer;
  begin
    FillChar(MasterFields, sizeof(MasterFields), 0);
    for i := 0 to FieldRefs.Count - 1 do
      with TFieldRef (FieldRefs[i]) do
      begin
        if Cmp(STableName, ChangeFileExt(STable.TableName, '')) then
        begin
          STableIndex := ATableIndex;
          for f := 0 to STable.FieldCount - 1 do
            if Cmp(SFieldName, STable.Fields[f].FieldName) then
            begin
              SFieldIndex := f;
              DTFieldIndex := DTable.FieldByName(
                Map(STable.TableName, STable.Fields[SFieldIndex].FieldName)).Index;
              MasterFields[f].IsRef := true;
              HasMaster := true;
            end;
        end;
        if Cmp(Map(DTableName, ''), ChangeFileExt(DTable.TableName,'')) then
        begin
          DTableIndex := ATableIndex;
          for f := 0 to DTable.FieldCount - 1 do
            if Cmp(Map(DTableName, DFieldName), DTable.Fields[f].FieldName) then
            begin
              DFieldIndex := f;
              MasterFields[f].HasRef := true;
              HasDetail := true;
            end;
        end;
      end;
  end;

  procedure AppendRef(TableIndex : integer);
  var
    i : integer;
  begin
    for i := 0 to FieldRefs.Count - 1 do
      with TFieldRef (FieldRefs[i]) do
        if MasterRef and (STableIndex = TableIndex) then
        try
          RTable.AppendRecord([TableIndex + 1, SFieldIndex + 1,
            MasterFields[SFieldIndex].Value,
            DTable.Fields[DTFieldIndex].AsVariant]);
        except;

        end;
  end;

  function FixupRef(TableIndex : integer) : boolean;
  var
    i : integer;
  begin
    for i := 0 to FieldRefs.Count - 1 do
      with TFieldRef (FieldRefs[i]) do
        if (DTableIndex = TableIndex) and
           (DFieldIndex <> -1) and
           (DTable.Fields[DFieldIndex].AsVariant <> Null) then
        begin
         { DTable.Fields[DFieldIndex].AsVariant :=
            RTable.Lookup('Table;Field;OldValue', VarArrayOf([
              STableIndex + 1,
              SFieldIndex + 1,
              DTable.Fields[DFieldIndex].AsVariant]),
              'NewValue'); }
          if RTable.Locate('Table;Field;OldValue', VarArrayOf([
              STableIndex + 1,
              SFieldIndex + 1,
              DTable.Fields[DFieldIndex].AsVariant]), []) then
            DTable.Fields[DFieldIndex].AsVariant := RTable['NewValue']
          else
          begin
           // record not found, may be in second pass
            AllFixups := false;
            Result := false;
            inc(FErrorCount);
            exit;
          end;
        end;
    Result := true;
  end;

  procedure MoveRecord(TableIndex : integer);

    procedure MoveField(FieldIndex : integer);
    begin
      try
        DTable.FieldByName(Map(STable.TableName,
          STable.Fields[FieldIndex].FieldName)).AsVariant :=
          STable.Fields[FieldIndex].AsVariant;
      except
        on E : EDBEngineError do
          if E.Errors[0].ErrorCode = DBIERR_BLOBMODIFIED then
          begin
            inc(FErrorCount);
            inc(FErrorBlobCount);
          end else
            raise
      end;
    end;

  var
    f : integer;
    Action : TMoveAction;
  begin
    DTable.Append;
    try
      for f := 0 to STable.FieldCount - 1 do
        if DTable.FindField(Map(STable.TableName, 
          STable.Fields[f].FieldName)) <> nil then
        begin
          MoveField(f);
          if MasterFields[f].IsRef then
            MasterFields[f].Value := STable.Fields[f].AsInteger;
        end;
      Action := maMove;
      if HasDetail and not FixupRef(TableIndex) then
        Action := maIgnore;
      if (Action = maMove) and Assigned(FOnMoveRecord) then
        FOnMoveRecord(Self, DTable, Action);
      if HasMaster and (Action in [maMove, maMap]) then
        AppendRef(TableIndex);
      if Action = maMove then
        try
          DTable.Post
        except
          on E : EAbort do
          begin
            DTable.Cancel;
            inc(FErrorCount);
          end;
        end
      else
        DTable.Cancel;
    except
      on E : EAbort do raise
      else
      if DTable.State = dsInsert then
        DTable.Cancel;
     // raise;
    end;
  end;

  procedure MoveTable(TableIndex : integer);
  begin
    STable.Close;
    DTable.Close;
    STable.TableName := FTables[TableIndex];
    DTable.TableName := Map(FTables[TableIndex], '');
    STable.Open;
    DTable.Open;
    UpdateRefList(TableIndex);
    while not STable.Eof do
    begin
      try
        inc(FCurrentRecord);
        MoveRecord(TableIndex);
      except
        //
        raise;
      end;
      STable.Next;
    end;
  end;

var
  i, TableIndex : integer;
 // Er : integer;
begin
  FCurrentRecord := 0;
  FErrorCount := 0;
  FErrorBlobCount := 0;
  for i := 0 to FTables.Count - 1 do
    if CmdString(FTables[i]) then     
    begin
     { in Tables list one table can be appear more than once,
       but we must use one TableIndex for all appearance }
      TableIndex := FTables.IndexOf(FTables[i]);
     // if (TableIndex = i) or not AllFixups then
      begin
        AllFixups := true;
        MoveTable(TableIndex);
      end;
     { if TableIndex = i then
        Er := FErrorCount else
        FErrorCount := Er; }
    end;
end;


end.


