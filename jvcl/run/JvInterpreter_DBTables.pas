{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_DBTables.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_DBTables;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  BDE, Classes, Db, DbTables;

{ EDBEngineError }

{ constructor Create(ErrorCode: DBIResult) }

procedure EDBEngineError_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(EDBEngineError.Create(Args.Values[0]));
end;

{ property Read ErrorCount: Integer }

procedure EDBEngineError_Read_ErrorCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := EDBEngineError(Args.Obj).ErrorCount;
end;

{ property Read Errors[Integer]: TDBError }

procedure EDBEngineError_Read_Errors(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(EDBEngineError(Args.Obj).Errors[Args.Values[0]]);
end;

{ ENoResultSet }

{ TSession }

{ constructor Create(AOwner: TComponent) }

procedure TSession_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TSession.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure AddAlias(const Name, Driver: string; List: TStrings); }

procedure TSession_AddAlias(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).AddAlias(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

{ procedure AddDriver(const Name: string; List: TStrings); }

procedure TSession_AddDriver(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).AddDriver(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{ procedure AddStandardAlias(const Name, Path, DefaultDriver: string); }

procedure TSession_AddStandardAlias(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).AddStandardAlias(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ property Read ConfigMode: TConfigMode }

procedure TSession_Read_ConfigMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TSession(Args.Obj).ConfigMode));
end;

{ property Write ConfigMode(Value: TConfigMode) }

procedure TSession_Write_ConfigMode(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).ConfigMode := TConfigMode(Byte(V2S(Value)));
end;

{ procedure AddPassword(const Password: string); }

procedure TSession_AddPassword(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).AddPassword(Args.Values[0]);
end;

{ procedure Close; }

procedure TSession_Close(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).Close;
end;

{ procedure CloseDatabase(Database: TDatabase); }

procedure TSession_CloseDatabase(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).CloseDatabase(V2O(Args.Values[0]) as TDatabase);
end;

{ procedure DeleteAlias(const Name: string); }

procedure TSession_DeleteAlias(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).DeleteAlias(Args.Values[0]);
end;

{ procedure DeleteDriver(const Name: string); }

procedure TSession_DeleteDriver(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).DeleteDriver(Args.Values[0]);
end;

{ procedure DropConnections; }

procedure TSession_DropConnections(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).DropConnections;
end;

{ function FindDatabase(const DatabaseName: string): TDatabase; }

procedure TSession_FindDatabase(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TSession(Args.Obj).FindDatabase(Args.Values[0]));
end;

{ procedure GetAliasNames(List: TStrings); }

procedure TSession_GetAliasNames(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).GetAliasNames(V2O(Args.Values[0]) as TStrings);
end;

{ procedure GetAliasParams(const AliasName: string; List: TStrings); }

procedure TSession_GetAliasParams(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).GetAliasParams(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{ function GetAliasDriverName(const AliasName: string): string; }

procedure TSession_GetAliasDriverName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).GetAliasDriverName(Args.Values[0]);
end;

{ procedure GetConfigParams(const Path, Section: string; List: TStrings); }

procedure TSession_GetConfigParams(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).GetConfigParams(Args.Values[0], Args.Values[1], V2O(Args.Values[2]) as TStrings);
end;

{ procedure GetDatabaseNames(List: TStrings); }

procedure TSession_GetDatabaseNames(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).GetDatabaseNames(V2O(Args.Values[0]) as TStrings);
end;

{ procedure GetDriverNames(List: TStrings); }

procedure TSession_GetDriverNames(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).GetDriverNames(V2O(Args.Values[0]) as TStrings);
end;

{ procedure GetDriverParams(const DriverName: string; List: TStrings); }

procedure TSession_GetDriverParams(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).GetDriverParams(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{ function GetPassword: Boolean; }

procedure TSession_GetPassword(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).GetPassword;
end;

{ procedure GetTableNames(const DatabaseName, Pattern: string; Extensions, SystemTables: Boolean; List: TStrings); }

procedure TSession_GetTableNames(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).GetTableNames(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3], V2O(Args.Values[4]) as
    TStrings);
end;

{ procedure GetStoredProcNames(const DatabaseName: string; List: TStrings); }

procedure TSession_GetStoredProcNames(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).GetStoredProcNames(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{ function IsAlias(const Name: string): Boolean; }

procedure TSession_IsAlias(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).IsAlias(Args.Values[0]);
end;

{ procedure ModifyAlias(Name: string; List: TStrings); }

procedure TSession_ModifyAlias(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).ModifyAlias(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{ procedure ModifyDriver(Name: string; List: TStrings); }

procedure TSession_ModifyDriver(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).ModifyDriver(Args.Values[0], V2O(Args.Values[1]) as TStrings);
end;

{ procedure Open; }

procedure TSession_Open(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).Open;
end;

{ function OpenDatabase(const DatabaseName: string): TDatabase; }

procedure TSession_OpenDatabase(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TSession(Args.Obj).OpenDatabase(Args.Values[0]));
end;

{ procedure RemoveAllPasswords; }

procedure TSession_RemoveAllPasswords(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).RemoveAllPasswords;
end;

{ procedure RemovePassword(const Password: string); }

procedure TSession_RemovePassword(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).RemovePassword(Args.Values[0]);
end;

{ procedure SaveConfigFile; }

procedure TSession_SaveConfigFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).SaveConfigFile;
end;

{ property Read DatabaseCount: Integer }

procedure TSession_Read_DatabaseCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).DatabaseCount;
end;

{ property Read Databases[Integer]: TDatabase }

procedure TSession_Read_Databases(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TSession(Args.Obj).Databases[Args.Values[0]]);
end;

{ property Read Handle: HDBISES }

procedure TSession_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TSession(Args.Obj).Handle);
end;

{ property Read Locale: TLocale }

procedure TSession_Read_Locale(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TSession(Args.Obj).Locale);
end;

{ property Read TraceFlags: TTraceFlags }

procedure TSession_Read_TraceFlags(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Word(TSession(Args.Obj).TraceFlags));
end;

{ property Write TraceFlags(Value: TTraceFlags) }

procedure TSession_Write_TraceFlags(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).TraceFlags := TTraceFlags(Word(V2S(Value)));
end;

{ property Read Active: Boolean }

procedure TSession_Read_Active(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).Active;
end;

{ property Write Active(Value: Boolean) }

procedure TSession_Write_Active(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).Active := Value;
end;

{ property Read AutoSessionName: Boolean }

procedure TSession_Read_AutoSessionName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).AutoSessionName;
end;

{ property Write AutoSessionName(Value: Boolean) }

procedure TSession_Write_AutoSessionName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).AutoSessionName := Value;
end;

{ property Read KeepConnections: Boolean }

procedure TSession_Read_KeepConnections(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).KeepConnections;
end;

{ property Write KeepConnections(Value: Boolean) }

procedure TSession_Write_KeepConnections(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).KeepConnections := Value;
end;

{ property Read NetFileDir: string }

procedure TSession_Read_NetFileDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).NetFileDir;
end;

{ property Write NetFileDir(Value: string) }

procedure TSession_Write_NetFileDir(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).NetFileDir := Value;
end;

{ property Read PrivateDir: string }

procedure TSession_Read_PrivateDir(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).PrivateDir;
end;

{ property Write PrivateDir(Value: string) }

procedure TSession_Write_PrivateDir(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).PrivateDir := Value;
end;

{ property Read SessionName: string }

procedure TSession_Read_SessionName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).SessionName;
end;

{ property Write SessionName(Value: string) }

procedure TSession_Write_SessionName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).SessionName := Value;
end;

{ property Read SQLHourGlass: Boolean }

procedure TSession_Read_SQLHourGlass(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSession(Args.Obj).SQLHourGlass;
end;

{ property Write SQLHourGlass(Value: Boolean) }

procedure TSession_Write_SQLHourGlass(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSession(Args.Obj).SQLHourGlass := Value;
end;

{ TDatabase }

{ constructor Create(AOwner: TComponent) }

procedure TDatabase_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDatabase.Create(V2O(Args.Values[0]) as TComponent));
end;

(*
{ procedure ApplyUpdates(const DataSets: array of TDBDataSet); }
procedure TDatabase_ApplyUpdates(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).ApplyUpdates(Args.Values[0]);
end;
*)

{ procedure Close; }

procedure TDatabase_Close(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).Close;
end;

{ procedure CloseDataSets; }

procedure TDatabase_CloseDataSets(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).CloseDataSets;
end;

{ procedure Commit; }

procedure TDatabase_Commit(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).Commit;
end;

{ procedure FlushSchemaCache(const TableName: string); }

procedure TDatabase_FlushSchemaCache(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).FlushSchemaCache(Args.Values[0]);
end;

{ procedure Open; }

procedure TDatabase_Open(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).Open;
end;

{ procedure Rollback; }

procedure TDatabase_Rollback(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).Rollback;
end;

{ procedure StartTransaction; }

procedure TDatabase_StartTransaction(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).StartTransaction;
end;

{ procedure ValidateName(const Name: string); }

procedure TDatabase_ValidateName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).ValidateName(Args.Values[0]);
end;

{ property Read DataSetCount: Integer }

procedure TDatabase_Read_DataSetCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).DataSetCount;
end;

{ property Read DataSets[Integer]: TDBDataSet }

procedure TDatabase_Read_DataSets(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDatabase(Args.Obj).DataSets[Args.Values[0]]);
end;

{ property Read Directory: string }

procedure TDatabase_Read_Directory(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).Directory;
end;

{ property Write Directory(Value: string) }

procedure TDatabase_Write_Directory(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).Directory := Value;
end;

{ property Read Handle: HDBIDB }

procedure TDatabase_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Longint(TDatabase(Args.Obj).Handle);
end;

{ property Write Handle(Value: HDBIDB) }

procedure TDatabase_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).Handle := HDBIDB(Longint(Value));
end;

{ property Read IsSQLBased: Boolean }

procedure TDatabase_Read_IsSQLBased(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).IsSQLBased;
end;

{ property Read InTransaction: Boolean }

procedure TDatabase_Read_InTransaction(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).InTransaction;
end;

{ property Read Locale: TLocale }

procedure TDatabase_Read_Locale(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TDatabase(Args.Obj).Locale);
end;

{ property Read Session: TSession }

procedure TDatabase_Read_Session(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDatabase(Args.Obj).Session);
end;

{ property Read Temporary: Boolean }

procedure TDatabase_Read_Temporary(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).Temporary;
end;

{ property Write Temporary(Value: Boolean) }

procedure TDatabase_Write_Temporary(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).Temporary := Value;
end;

{ property Read SessionAlias: Boolean }

procedure TDatabase_Read_SessionAlias(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).SessionAlias;
end;

{ property Read TraceFlags: TTraceFlags }

procedure TDatabase_Read_TraceFlags(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Word(TDatabase(Args.Obj).TraceFlags));
end;

{ property Write TraceFlags(Value: TTraceFlags) }

procedure TDatabase_Write_TraceFlags(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).TraceFlags := TTraceFlags(Word(V2S(Value)));
end;

{ property Read AliasName: string }

procedure TDatabase_Read_AliasName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).AliasName;
end;

{ property Write AliasName(Value: string) }

procedure TDatabase_Write_AliasName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).AliasName := Value;
end;

{ property Read Connected: Boolean }

procedure TDatabase_Read_Connected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).Connected;
end;

{ property Write Connected(Value: Boolean) }

procedure TDatabase_Write_Connected(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).Connected := Value;
end;

{ property Read DatabaseName: string }

procedure TDatabase_Read_DatabaseName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).DatabaseName;
end;

{ property Write DatabaseName(Value: string) }

procedure TDatabase_Write_DatabaseName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).DatabaseName := Value;
end;

{ property Read DriverName: string }

procedure TDatabase_Read_DriverName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).DriverName;
end;

{ property Write DriverName(Value: string) }

procedure TDatabase_Write_DriverName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).DriverName := Value;
end;

{ property Read HandleShared: Boolean }

procedure TDatabase_Read_HandleShared(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).HandleShared;
end;

{ property Write HandleShared(Value: Boolean) }

procedure TDatabase_Write_HandleShared(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).HandleShared := Value;
end;

{ property Read KeepConnection: Boolean }

procedure TDatabase_Read_KeepConnection(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).KeepConnection;
end;

{ property Write KeepConnection(Value: Boolean) }

procedure TDatabase_Write_KeepConnection(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).KeepConnection := Value;
end;

{ property Read LoginPrompt: Boolean }

procedure TDatabase_Read_LoginPrompt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).LoginPrompt;
end;

{ property Write LoginPrompt(Value: Boolean) }

procedure TDatabase_Write_LoginPrompt(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).LoginPrompt := Value;
end;

{ property Read Params: TStrings }

procedure TDatabase_Read_Params(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDatabase(Args.Obj).Params);
end;

{ property Write Params(Value: TStrings) }

procedure TDatabase_Write_Params(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).Params := V2O(Value) as TStrings;
end;

{ property Read SessionName: string }

procedure TDatabase_Read_SessionName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).SessionName;
end;

{ property Write SessionName(Value: string) }

procedure TDatabase_Write_SessionName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).SessionName := Value;
end;

{ property Read TransIsolation: TTransIsolation }

procedure TDatabase_Read_TransIsolation(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDatabase(Args.Obj).TransIsolation;
end;

{ property Write TransIsolation(Value: TTransIsolation) }

procedure TDatabase_Write_TransIsolation(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDatabase(Args.Obj).TransIsolation := Value;
end;

{ TBDEDataSet }

{ constructor Create(AOwner: TComponent) }

procedure TBDEDataSet_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBDEDataSet.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure ApplyUpdates; }

procedure TBDEDataSet_ApplyUpdates(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).ApplyUpdates;
end;

{ function BookmarkValid(Bookmark: TBookmark): Boolean; }

procedure TBDEDataSet_BookmarkValid(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).BookmarkValid(V2P(Args.Values[0]));
end;

{ procedure Cancel; }

procedure TBDEDataSet_Cancel(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).Cancel;
end;

{ procedure CancelUpdates; }

procedure TBDEDataSet_CancelUpdates(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).CancelUpdates;
end;

{ property Read CacheBlobs: Boolean }

procedure TBDEDataSet_Read_CacheBlobs(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).CacheBlobs;
end;

{ property Write CacheBlobs(Value: Boolean) }

procedure TBDEDataSet_Write_CacheBlobs(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).CacheBlobs := Value;
end;

{ function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; }

procedure TBDEDataSet_CompareBookmarks(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).CompareBookmarks(V2P(Args.Values[0]), V2P(Args.Values[1]));
end;

{ procedure CommitUpdates; }

procedure TBDEDataSet_CommitUpdates(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).CommitUpdates;
end;

{ function ConstraintsDisabled: Boolean; }

procedure TBDEDataSet_ConstraintsDisabled(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).ConstraintsDisabled;
end;

{ function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; }

procedure TBDEDataSet_CreateBlobStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBDEDataSet(Args.Obj).CreateBlobStream(V2O(Args.Values[0]) as TField, Args.Values[1]));
end;

{ procedure DisableConstraints; }

procedure TBDEDataSet_DisableConstraints(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).DisableConstraints;
end;

{ procedure EnableConstraints; }

procedure TBDEDataSet_EnableConstraints(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).EnableConstraints;
end;

{ procedure FetchAll; }

procedure TBDEDataSet_FetchAll(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).FetchAll;
end;

{ procedure FlushBuffers; }

procedure TBDEDataSet_FlushBuffers(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).FlushBuffers;
end;

{ function GetCurrentRecord(Buffer: PChar): Boolean; }

procedure TBDEDataSet_GetCurrentRecord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).GetCurrentRecord(PChar(string(Args.Values[0])));
end;

{ procedure GetIndexInfo; }

procedure TBDEDataSet_GetIndexInfo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).GetIndexInfo;
end;

{ function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; }

procedure TBDEDataSet_Locate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).Locate(Args.Values[0], Args.Values[1], TLocateOptions(Byte(V2S(Args.Values[2]))));
end;

{ function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; }

procedure TBDEDataSet_Lookup(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).Lookup(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ function IsSequenced: Boolean; }

procedure TBDEDataSet_IsSequenced(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).IsSequenced;
end;

{ procedure Post; }

procedure TBDEDataSet_Post(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).Post;
end;

{ procedure RevertRecord; }

procedure TBDEDataSet_RevertRecord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).RevertRecord;
end;

{ function UpdateStatus: TUpdateStatus; }

procedure TBDEDataSet_UpdateStatus(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).UpdateStatus;
end;

{ procedure Translate(Src, Dest: PChar; ToOem: Boolean); }

procedure TBDEDataSet_Translate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).Translate(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ property Read ExpIndex: Boolean }

procedure TBDEDataSet_Read_ExpIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).ExpIndex;
end;

{ property Read Handle: HDBICur }

procedure TBDEDataSet_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Longint(TBDEDataSet(Args.Obj).Handle);
end;

{ property Read KeySize: Word }

procedure TBDEDataSet_Read_KeySize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).KeySize;
end;

{ property Read Locale: TLocale }

procedure TBDEDataSet_Read_Locale(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TBDEDataSet(Args.Obj).Locale);
end;

{ property Read UpdateObject: TDataSetUpdateObject }

procedure TBDEDataSet_Read_UpdateObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBDEDataSet(Args.Obj).UpdateObject);
end;

{ property Write UpdateObject(Value: TDataSetUpdateObject) }

procedure TBDEDataSet_Write_UpdateObject(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).UpdateObject := V2O(Value) as TDataSetUpdateObject;
end;

{ property Read UpdatesPending: Boolean }

procedure TBDEDataSet_Read_UpdatesPending(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).UpdatesPending;
end;

{ property Read UpdateRecordTypes: TUpdateRecordTypes }

procedure TBDEDataSet_Read_UpdateRecordTypes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TBDEDataSet(Args.Obj).UpdateRecordTypes));
end;

{ property Write UpdateRecordTypes(Value: TUpdateRecordTypes) }

procedure TBDEDataSet_Write_UpdateRecordTypes(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).UpdateRecordTypes := TUpdateRecordTypes(Byte(V2S(Value)));
end;

{ property Read CachedUpdates: Boolean }

procedure TBDEDataSet_Read_CachedUpdates(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBDEDataSet(Args.Obj).CachedUpdates;
end;

{ property Write CachedUpdates(Value: Boolean) }

procedure TBDEDataSet_Write_CachedUpdates(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBDEDataSet(Args.Obj).CachedUpdates := Value;
end;

{ TDBDataSet }

{ function CheckOpen(Status: DBIResult): Boolean; }

procedure TDBDataSet_CheckOpen(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBDataSet(Args.Obj).CheckOpen(Args.Values[0]);
end;

{ procedure CloseDatabase(Database: TDatabase); }

procedure TDBDataSet_CloseDatabase(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBDataSet(Args.Obj).CloseDatabase(V2O(Args.Values[0]) as TDatabase);
end;

{ function OpenDatabase: TDatabase; }

procedure TDBDataSet_OpenDatabase(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBDataSet(Args.Obj).OpenDatabase);
end;

{ property Read Database: TDatabase }

procedure TDBDataSet_Read_Database(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBDataSet(Args.Obj).Database);
end;

{ property Read DBHandle: HDBIDB }

procedure TDBDataSet_Read_DBHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Longint(TDBDataSet(Args.Obj).DBHandle);
end;

{ property Read DBLocale: TLocale }

procedure TDBDataSet_Read_DBLocale(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TDBDataSet(Args.Obj).DBLocale);
end;

{ property Read DBSession: TSession }

procedure TDBDataSet_Read_DBSession(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBDataSet(Args.Obj).DBSession);
end;

{ property Read DatabaseName: string }

procedure TDBDataSet_Read_DatabaseName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBDataSet(Args.Obj).DatabaseName;
end;

{ property Write DatabaseName(Value: string) }

procedure TDBDataSet_Write_DatabaseName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBDataSet(Args.Obj).DatabaseName := Value;
end;

{ property Read SessionName: string }

procedure TDBDataSet_Read_SessionName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBDataSet(Args.Obj).SessionName;
end;

{ property Write SessionName(Value: string) }

procedure TDBDataSet_Write_SessionName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBDataSet(Args.Obj).SessionName := Value;
end;

{ TTable }

{ constructor Create(AOwner: TComponent) }

procedure TTable_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTable.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function BatchMove(ASource: TBDEDataSet; AMode: TBatchMode): Longint; }

procedure TTable_BatchMove(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).BatchMove(V2O(Args.Values[0]) as TBDEDataSet, Args.Values[1]);
end;

{ procedure AddIndex(const Name, Fields: string; Options: TIndexOptions); }

procedure TTable_AddIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).AddIndex(Args.Values[0], Args.Values[1], TIndexOptions(Byte(V2S(Args.Values[2]))));
end;

{ procedure ApplyRange; }

procedure TTable_ApplyRange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).ApplyRange;
end;

{ procedure CancelRange; }

procedure TTable_CancelRange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).CancelRange;
end;

{ procedure CloseIndexFile(const IndexFileName: string); }

procedure TTable_CloseIndexFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).CloseIndexFile(Args.Values[0]);
end;

{ procedure CreateTable; }

procedure TTable_CreateTable(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).CreateTable;
end;

{ procedure DeleteIndex(const Name: string); }

procedure TTable_DeleteIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).DeleteIndex(Args.Values[0]);
end;

{ procedure DeleteTable; }

procedure TTable_DeleteTable(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).DeleteTable;
end;

{ procedure EditKey; }

procedure TTable_EditKey(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).EditKey;
end;

{ procedure EditRangeEnd; }

procedure TTable_EditRangeEnd(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).EditRangeEnd;
end;

{ procedure EditRangeStart; }

procedure TTable_EditRangeStart(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).EditRangeStart;
end;

{ procedure EmptyTable; }

procedure TTable_EmptyTable(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).EmptyTable;
end;

{ function FindKey(const KeyValues: array of const): Boolean; }

procedure TTable_FindKey(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Args.OpenArray(0);
  Value := TTable(Args.Obj).FindKey(Slice(Args.OA^, Args.OAS));
end;

{ procedure FindNearest(const KeyValues: array of const); }

procedure TTable_FindNearest(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Args.OpenArray(0);
  TTable(Args.Obj).FindNearest(Slice(Args.OA^, Args.OAS));
end;

{ procedure GetIndexNames(List: TStrings); }

procedure TTable_GetIndexNames(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).GetIndexNames(V2O(Args.Values[0]) as TStrings);
end;

{ procedure GotoCurrent(Table: TTable); }

procedure TTable_GotoCurrent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).GotoCurrent(V2O(Args.Values[0]) as TTable);
end;

{ function GotoKey: Boolean; }

procedure TTable_GotoKey(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).GotoKey;
end;

{ procedure GotoNearest; }

procedure TTable_GotoNearest(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).GotoNearest;
end;

{ procedure LockTable(LockType: TLockType); }

procedure TTable_LockTable(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).LockTable(Args.Values[0]);
end;

{ procedure OpenIndexFile(const IndexName: string); }

procedure TTable_OpenIndexFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).OpenIndexFile(Args.Values[0]);
end;

{ procedure RenameTable(const NewTableName: string); }

procedure TTable_RenameTable(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).RenameTable(Args.Values[0]);
end;

{ procedure SetKey; }

procedure TTable_SetKey(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).SetKey;
end;

{ procedure SetRange(const StartValues, EndValues: array of const); }

procedure TTable_SetRange(var Value: Variant; Args: TJvInterpreterArgs);
var
  OA: TOpenArray;
  OAV: TValueArray;
  OAS: Integer;
begin
  Args.OpenArray(0);
  V2OA(Args.Values[1], OA, OAV, OAS);
  TTable(Args.Obj).SetRange(Slice(Args.OA^, Args.OAS), OA);
end;

{ procedure SetRangeEnd; }

procedure TTable_SetRangeEnd(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).SetRangeEnd;
end;

{ procedure SetRangeStart; }

procedure TTable_SetRangeStart(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).SetRangeStart;
end;

{ procedure UnlockTable(LockType: TLockType); }

procedure TTable_UnlockTable(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).UnlockTable(Args.Values[0]);
end;

{ property Read IndexDefs: TIndexDefs }

procedure TTable_Read_IndexDefs(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTable(Args.Obj).IndexDefs);
end;

{ property Read IndexFieldCount: Integer }

procedure TTable_Read_IndexFieldCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).IndexFieldCount;
end;

{ property Read IndexFields[Integer]: TField }

procedure TTable_Read_IndexFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTable(Args.Obj).IndexFields[Args.Values[0]]);
end;

{ property Write IndexFields[Integer]: TField }

procedure TTable_Write_IndexFields(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).IndexFields[Args.Values[0]] := V2O(Value) as TField;
end;

{ property Read KeyExclusive: Boolean }

procedure TTable_Read_KeyExclusive(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).KeyExclusive;
end;

{ property Write KeyExclusive(Value: Boolean) }

procedure TTable_Write_KeyExclusive(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).KeyExclusive := Value;
end;

{ property Read KeyFieldCount: Integer }

procedure TTable_Read_KeyFieldCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).KeyFieldCount;
end;

{ property Write KeyFieldCount(Value: Integer) }

procedure TTable_Write_KeyFieldCount(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).KeyFieldCount := Value;
end;

{ property Read TableLevel: Integer }

procedure TTable_Read_TableLevel(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).TableLevel;
end;

{ property Write TableLevel(Value: Integer) }

procedure TTable_Write_TableLevel(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).TableLevel := Value;
end;

{ property Read Exclusive: Boolean }

procedure TTable_Read_Exclusive(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).Exclusive;
end;

{ property Write Exclusive(Value: Boolean) }

procedure TTable_Write_Exclusive(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).Exclusive := Value;
end;

{ property Read IndexFieldNames: string }

procedure TTable_Read_IndexFieldNames(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).IndexFieldNames;
end;

{ property Write IndexFieldNames(Value: string) }

procedure TTable_Write_IndexFieldNames(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).IndexFieldNames := Value;
end;

{ property Read IndexFiles: TStrings }

procedure TTable_Read_IndexFiles(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTable(Args.Obj).IndexFiles);
end;

{ property Write IndexFiles(Value: TStrings) }

procedure TTable_Write_IndexFiles(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).IndexFiles := V2O(Value) as TStrings;
end;

{ property Read IndexName: string }

procedure TTable_Read_IndexName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).IndexName;
end;

{ property Write IndexName(Value: string) }

procedure TTable_Write_IndexName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).IndexName := Value;
end;

{ property Read MasterFields: string }

procedure TTable_Read_MasterFields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).MasterFields;
end;

{ property Write MasterFields(Value: string) }

procedure TTable_Write_MasterFields(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).MasterFields := Value;
end;

{ property Read MasterSource: TDataSource }

procedure TTable_Read_MasterSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTable(Args.Obj).MasterSource);
end;

{ property Write MasterSource(Value: TDataSource) }

procedure TTable_Write_MasterSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).MasterSource := V2O(Value) as TDataSource;
end;

{ property Read ReadOnly: Boolean }

procedure TTable_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TTable_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).ReadOnly := Value;
end;

{ property Read TableName: TFileName }

procedure TTable_Read_TableName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).TableName;
end;

{ property Write TableName(Value: TFileName) }

procedure TTable_Write_TableName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).TableName := Value;
end;

{ property Read TableType: TTableType }

procedure TTable_Read_TableType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTable(Args.Obj).TableType;
end;

{ property Write TableType(Value: TTableType) }

procedure TTable_Write_TableType(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTable(Args.Obj).TableType := Value;
end;

{ TBatchMove }

{ constructor Create(AOwner: TComponent) }

procedure TBatchMove_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBatchMove.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Execute; }

procedure TBatchMove_Execute(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).Execute;
end;

{ property Read ChangedCount: Longint }

procedure TBatchMove_Read_ChangedCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).ChangedCount;
end;

{ property Read KeyViolCount: Longint }

procedure TBatchMove_Read_KeyViolCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).KeyViolCount;
end;

{ property Read MovedCount: Longint }

procedure TBatchMove_Read_MovedCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).MovedCount;
end;

{ property Read ProblemCount: Longint }

procedure TBatchMove_Read_ProblemCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).ProblemCount;
end;

{ property Read AbortOnKeyViol: Boolean }

procedure TBatchMove_Read_AbortOnKeyViol(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).AbortOnKeyViol;
end;

{ property Write AbortOnKeyViol(Value: Boolean) }

procedure TBatchMove_Write_AbortOnKeyViol(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).AbortOnKeyViol := Value;
end;

{ property Read AbortOnProblem: Boolean }

procedure TBatchMove_Read_AbortOnProblem(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).AbortOnProblem;
end;

{ property Write AbortOnProblem(Value: Boolean) }

procedure TBatchMove_Write_AbortOnProblem(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).AbortOnProblem := Value;
end;

{ property Read CommitCount: Integer }

procedure TBatchMove_Read_CommitCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).CommitCount;
end;

{ property Write CommitCount(Value: Integer) }

procedure TBatchMove_Write_CommitCount(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).CommitCount := Value;
end;

{ property Read ChangedTableName: TFileName }

procedure TBatchMove_Read_ChangedTableName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).ChangedTableName;
end;

{ property Write ChangedTableName(Value: TFileName) }

procedure TBatchMove_Write_ChangedTableName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).ChangedTableName := Value;
end;

{ property Read Destination: TTable }

procedure TBatchMove_Read_Destination(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBatchMove(Args.Obj).Destination);
end;

{ property Write Destination(Value: TTable) }

procedure TBatchMove_Write_Destination(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).Destination := V2O(Value) as TTable;
end;

{ property Read KeyViolTableName: TFileName }

procedure TBatchMove_Read_KeyViolTableName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).KeyViolTableName;
end;

{ property Write KeyViolTableName(Value: TFileName) }

procedure TBatchMove_Write_KeyViolTableName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).KeyViolTableName := Value;
end;

{ property Read Mappings: TStrings }

procedure TBatchMove_Read_Mappings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBatchMove(Args.Obj).Mappings);
end;

{ property Write Mappings(Value: TStrings) }

procedure TBatchMove_Write_Mappings(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).Mappings := V2O(Value) as TStrings;
end;

{ property Read Mode: TBatchMode }

procedure TBatchMove_Read_Mode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).Mode;
end;

{ property Write Mode(Value: TBatchMode) }

procedure TBatchMove_Write_Mode(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).Mode := Value;
end;

{ property Read ProblemTableName: TFileName }

procedure TBatchMove_Read_ProblemTableName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).ProblemTableName;
end;

{ property Write ProblemTableName(Value: TFileName) }

procedure TBatchMove_Write_ProblemTableName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).ProblemTableName := Value;
end;

{ property Read RecordCount: Longint }

procedure TBatchMove_Read_RecordCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).RecordCount;
end;

{ property Write RecordCount(Value: Longint) }

procedure TBatchMove_Write_RecordCount(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).RecordCount := Value;
end;

{ property Read Source: TBDEDataSet }

procedure TBatchMove_Read_Source(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBatchMove(Args.Obj).Source);
end;

{ property Write Source(Value: TBDEDataSet) }

procedure TBatchMove_Write_Source(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).Source := V2O(Value) as TBDEDataSet;
end;

{ property Read Transliterate: Boolean }

procedure TBatchMove_Read_Transliterate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBatchMove(Args.Obj).Transliterate;
end;

{ property Write Transliterate(Value: Boolean) }

procedure TBatchMove_Write_Transliterate(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBatchMove(Args.Obj).Transliterate := Value;
end;

{ TParam }

{ constructor Create(AParamList: TParams; AParamType: TParamType) }

procedure TParam_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TParam.Create(V2O(Args.Values[0]) as TParams, Args.Values[1]));
end;

{ procedure Assign(Source: TPersistent); }

procedure TParam_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).Assign(V2O(Args.Values[0]) as TParam);
end;

{ procedure AssignField(Field: TField); }

procedure TParam_AssignField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AssignField(V2O(Args.Values[0]) as TField);
end;

{ procedure AssignFieldValue(Field: TField; const Value: Variant); }

procedure TParam_AssignFieldValue(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AssignFieldValue(V2O(Args.Values[0]) as TField, Args.Values[1]);
end;

{ procedure Clear; }

procedure TParam_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).Clear;
end;

{ procedure GetData(Buffer: Pointer); }

procedure TParam_GetData(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).GetData(V2P(Args.Values[0]));
end;

{ function GetDataSize: Integer; }

procedure TParam_GetDataSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).GetDataSize;
end;

{ procedure LoadFromFile(const FileName: string; BlobType: TBlobType); }

procedure TParam_LoadFromFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).LoadFromFile(Args.Values[0], Args.Values[1]);
end;

{ procedure LoadFromStream(Stream: TStream; BlobType: TBlobType); }

procedure TParam_LoadFromStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).LoadFromStream(V2O(Args.Values[0]) as TStream, Args.Values[1]);
end;

{ procedure SetBlobData(Buffer: Pointer; Size: Integer); }

procedure TParam_SetBlobData(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).SetBlobData(V2P(Args.Values[0]), Args.Values[1]);
end;

{ procedure SetData(Buffer: Pointer); }

procedure TParam_SetData(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).SetData(V2P(Args.Values[0]));
end;

{ property Read AsBCD: Currency }

procedure TParam_Read_AsBCD(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsBCD;
end;

{ property Write AsBCD(Value: Currency) }

procedure TParam_Write_AsBCD(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsBCD := Value;
end;

{ property Read AsBlob: TBlobData }

procedure TParam_Read_AsBlob(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsBlob;
end;

{ property Write AsBlob(Value: TBlobData) }

procedure TParam_Write_AsBlob(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsBlob := Value;
end;

{ property Read AsBoolean: Boolean }

procedure TParam_Read_AsBoolean(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsBoolean;
end;

{ property Write AsBoolean(Value: Boolean) }

procedure TParam_Write_AsBoolean(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsBoolean := Value;
end;

{ property Read AsCurrency: Double }

procedure TParam_Read_AsCurrency(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsCurrency;
end;

{ property Write AsCurrency(Value: Double) }

procedure TParam_Write_AsCurrency(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsCurrency := Value;
end;

{ property Read AsDate: TDateTime }

procedure TParam_Read_AsDate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsDate;
end;

{ property Write AsDate(Value: TDateTime) }

procedure TParam_Write_AsDate(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsDate := Value;
end;

{ property Read AsDateTime: TDateTime }

procedure TParam_Read_AsDateTime(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsDateTime;
end;

{ property Write AsDateTime(Value: TDateTime) }

procedure TParam_Write_AsDateTime(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsDateTime := Value;
end;

{ property Read AsFloat: Double }

procedure TParam_Read_AsFloat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsFloat;
end;

{ property Write AsFloat(Value: Double) }

procedure TParam_Write_AsFloat(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsFloat := Value;
end;

{ property Read AsInteger: LongInt }

procedure TParam_Read_AsInteger(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsInteger;
end;

{ property Write AsInteger(Value: LongInt) }

procedure TParam_Write_AsInteger(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsInteger := Value;
end;

{ property Read AsSmallInt: LongInt }

procedure TParam_Read_AsSmallInt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsSmallInt;
end;

{ property Write AsSmallInt(Value: LongInt) }

procedure TParam_Write_AsSmallInt(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsSmallInt := Value;
end;

{ property Read AsMemo: string }

procedure TParam_Read_AsMemo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsMemo;
end;

{ property Write AsMemo(Value: string) }

procedure TParam_Write_AsMemo(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsMemo := Value;
end;

{ property Read AsString: string }

procedure TParam_Read_AsString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsString;
end;

{ property Write AsString(Value: string) }

procedure TParam_Write_AsString(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsString := Value;
end;

{ property Read AsTime: TDateTime }

procedure TParam_Read_AsTime(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsTime;
end;

{ property Write AsTime(Value: TDateTime) }

procedure TParam_Write_AsTime(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsTime := Value;
end;

{ property Read AsWord: LongInt }

procedure TParam_Read_AsWord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).AsWord;
end;

{ property Write AsWord(Value: LongInt) }

procedure TParam_Write_AsWord(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).AsWord := Value;
end;

{ property Read Bound: Boolean }

procedure TParam_Read_Bound(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).Bound;
end;

{ property Write Bound(Value: Boolean) }

procedure TParam_Write_Bound(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).Bound := Value;
end;

{ property Read DataType: TFieldType }

procedure TParam_Read_DataType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).DataType;
end;

{ property Write DataType(Value: TFieldType) }

procedure TParam_Write_DataType(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).DataType := Value;
end;

{ property Read IsNull: Boolean }

procedure TParam_Read_IsNull(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).IsNull;
end;

{ property Read Name: string }

procedure TParam_Read_Name(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).Name;
end;

{ property Write Name(Value: string) }

procedure TParam_Write_Name(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).Name := Value;
end;

{ property Read ParamType: TParamType }

procedure TParam_Read_ParamType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).ParamType;
end;

{ property Write ParamType(Value: TParamType) }

procedure TParam_Write_ParamType(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).ParamType := Value;
end;

{ property Read Text: string }

procedure TParam_Read_Text(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).Text;
end;

{ property Write Text(Value: string) }

procedure TParam_Write_Text(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).Text := Value;
end;

{ property Read Value: Variant }

procedure TParam_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParam(Args.Obj).Value;
end;

{ property Write Value(Value: Variant) }

procedure TParam_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParam(Args.Obj).Value := Value;
end;

{ TParams }

{ constructor Create }

procedure TParams_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TParams.Create);
end;

{ procedure Assign(Source: TPersistent); }

procedure TParams_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParams(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ procedure AssignValues(Value: TParams); }

procedure TParams_AssignValues(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParams(Args.Obj).AssignValues(V2O(Args.Values[0]) as TParams);
end;

{ procedure AddParam(Value: TParam); }

procedure TParams_AddParam(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParams(Args.Obj).AddParam(V2O(Args.Values[0]) as TParam);
end;

{ procedure RemoveParam(Value: TParam); }

procedure TParams_RemoveParam(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParams(Args.Obj).RemoveParam(V2O(Args.Values[0]) as TParam);
end;

{ function CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType): TParam; }

procedure TParams_CreateParam(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TParams(Args.Obj).CreateParam(Args.Values[0], Args.Values[1], Args.Values[2]));
end;

{ function Count: Integer; }

procedure TParams_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParams(Args.Obj).Count;
end;

{ procedure Clear; }

procedure TParams_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParams(Args.Obj).Clear;
end;

{ procedure GetParamList(List: TList; const ParamNames: string); }

procedure TParams_GetParamList(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParams(Args.Obj).GetParamList(V2O(Args.Values[0]) as TList, Args.Values[1]);
end;

{ function IsEqual(Value: TParams): Boolean; }

procedure TParams_IsEqual(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParams(Args.Obj).IsEqual(V2O(Args.Values[0]) as TParams);
end;

{ function ParamByName(const Value: string): TParam; }

procedure TParams_ParamByName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TParams(Args.Obj).ParamByName(Args.Values[0]));
end;

{ property Read Items[Word]: TParam }

procedure TParams_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TParams(Args.Obj).Items[Args.Values[0]]);
end;

{ TStoredProc }

{ constructor Create(AOwner: TComponent) }

procedure TStoredProc_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStoredProc.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure CopyParams(Value: TParams); }

procedure TStoredProc_CopyParams(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).CopyParams(V2O(Args.Values[0]) as TParams);
end;

{ function DescriptionsAvailable: Boolean; }

procedure TStoredProc_DescriptionsAvailable(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStoredProc(Args.Obj).DescriptionsAvailable;
end;

{ procedure ExecProc; }

procedure TStoredProc_ExecProc(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).ExecProc;
end;

{ function ParamByName(const Value: string): TParam; }

procedure TStoredProc_ParamByName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStoredProc(Args.Obj).ParamByName(Args.Values[0]));
end;

{ procedure Prepare; }

procedure TStoredProc_Prepare(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).Prepare;
end;

{ procedure GetResults; }

procedure TStoredProc_GetResults(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).GetResults;
end;

{ procedure UnPrepare; }

procedure TStoredProc_UnPrepare(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).UnPrepare;
end;

{ property Read ParamCount: Word }

procedure TStoredProc_Read_ParamCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStoredProc(Args.Obj).ParamCount;
end;

{ property Read StmtHandle: HDBIStmt }

procedure TStoredProc_Read_StmtHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TStoredProc(Args.Obj).StmtHandle);
end;

{ property Read Prepared: Boolean }

procedure TStoredProc_Read_Prepared(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStoredProc(Args.Obj).Prepared;
end;

{ property Write Prepared(Value: Boolean) }

procedure TStoredProc_Write_Prepared(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).Prepared := Value;
end;

{ property Read StoredProcName: string }

procedure TStoredProc_Read_StoredProcName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStoredProc(Args.Obj).StoredProcName;
end;

{ property Write StoredProcName(Value: string) }

procedure TStoredProc_Write_StoredProcName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).StoredProcName := Value;
end;

{ property Read Overload: Word }

procedure TStoredProc_Read_Overload(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStoredProc(Args.Obj).Overload;
end;

{ property Write Overload(Value: Word) }

procedure TStoredProc_Write_Overload(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).Overload := Value;
end;

{ property Read Params: TParams }

procedure TStoredProc_Read_Params(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStoredProc(Args.Obj).Params);
end;

{ property Write Params(Value: TParams) }

procedure TStoredProc_Write_Params(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).Params := V2O(Value) as TParams;
end;

{ property Read ParamBindMode: TParamBindMode }

procedure TStoredProc_Read_ParamBindMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TStoredProc(Args.Obj).ParamBindMode;
end;

{ property Write ParamBindMode(Value: TParamBindMode) }

procedure TStoredProc_Write_ParamBindMode(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStoredProc(Args.Obj).ParamBindMode := Value;
end;

{ TQuery }

{ constructor Create(AOwner: TComponent) }

procedure TQuery_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuery.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure ExecSQL; }

procedure TQuery_ExecSQL(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).ExecSQL;
end;

{ function ParamByName(const Value: string): TParam; }

procedure TQuery_ParamByName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuery(Args.Obj).ParamByName(Args.Values[0]));
end;

{ procedure Prepare; }

procedure TQuery_Prepare(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).Prepare;
end;

{ procedure UnPrepare; }

procedure TQuery_UnPrepare(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).UnPrepare;
end;

{ property Read Prepared: Boolean }

procedure TQuery_Read_Prepared(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuery(Args.Obj).Prepared;
end;

{ property Write Prepared(Value: Boolean) }

procedure TQuery_Write_Prepared(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).Prepared := Value;
end;

{ property Read ParamCount: Word }

procedure TQuery_Read_ParamCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuery(Args.Obj).ParamCount;
end;

{ property Read Local: Boolean }

procedure TQuery_Read_Local(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuery(Args.Obj).Local;
end;

{ property Read StmtHandle: HDBIStmt }

procedure TQuery_Read_StmtHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TQuery(Args.Obj).StmtHandle);
end;

{ property Read Text: string }

procedure TQuery_Read_Text(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuery(Args.Obj).Text;
end;

{ property Read RowsAffected: Integer }

procedure TQuery_Read_RowsAffected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuery(Args.Obj).RowsAffected;
end;

{ property Read SQLBinary: PChar }

procedure TQuery_Read_SQLBinary(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := string(TQuery(Args.Obj).SQLBinary);
end;

{ property Write SQLBinary(Value: PChar) }

procedure TQuery_Write_SQLBinary(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).SQLBinary := PChar(string(Value));
end;

{ property Read Constrained: Boolean }

procedure TQuery_Read_Constrained(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuery(Args.Obj).Constrained;
end;

{ property Write Constrained(Value: Boolean) }

procedure TQuery_Write_Constrained(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).Constrained := Value;
end;

{ property Read DataSource: TDataSource }

procedure TQuery_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuery(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TQuery_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read ParamCheck: Boolean }

procedure TQuery_Read_ParamCheck(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuery(Args.Obj).ParamCheck;
end;

{ property Write ParamCheck(Value: Boolean) }

procedure TQuery_Write_ParamCheck(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).ParamCheck := Value;
end;

{ property Read RequestLive: Boolean }

procedure TQuery_Read_RequestLive(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuery(Args.Obj).RequestLive;
end;

{ property Write RequestLive(Value: Boolean) }

procedure TQuery_Write_RequestLive(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).RequestLive := Value;
end;

{ property Read SQL: TStrings }

procedure TQuery_Read_SQL(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuery(Args.Obj).SQL);
end;

{ property Write SQL(Value: TStrings) }

procedure TQuery_Write_SQL(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).SQL := V2O(Value) as TStrings;
end;

{ property Read Params: TParams }

procedure TQuery_Read_Params(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuery(Args.Obj).Params);
end;

{ property Write Params(Value: TParams) }

procedure TQuery_Write_Params(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).Params := V2O(Value) as TParams;
end;

{ property Read UniDirectional: Boolean }

procedure TQuery_Read_UniDirectional(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuery(Args.Obj).UniDirectional;
end;

{ property Write UniDirectional(Value: Boolean) }

procedure TQuery_Write_UniDirectional(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuery(Args.Obj).UniDirectional := Value;
end;

{ TUpdateSQL }

{ constructor Create(AOwner: TComponent) }

procedure TUpdateSQL_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TUpdateSQL.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Apply(UpdateKind: TUpdateKind); }

procedure TUpdateSQL_Apply(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TUpdateSQL(Args.Obj).Apply(Args.Values[0]);
end;

{ procedure ExecSQL(UpdateKind: TUpdateKind); }

procedure TUpdateSQL_ExecSQL(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TUpdateSQL(Args.Obj).ExecSQL(Args.Values[0]);
end;

{ procedure SetParams(UpdateKind: TUpdateKind); }

procedure TUpdateSQL_SetParams(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TUpdateSQL(Args.Obj).SetParams(Args.Values[0]);
end;

{ property Read Query[TUpdateKind]: TQuery }

procedure TUpdateSQL_Read_Query(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TUpdateSQL(Args.Obj).Query[Args.Values[0]]);
end;

{ property Read SQL[TUpdateKind]: TStrings }

procedure TUpdateSQL_Read_SQL(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TUpdateSQL(Args.Obj).SQL[Args.Values[0]]);
end;

{ property Write SQL[TUpdateKind]: TStrings }

procedure TUpdateSQL_Write_SQL(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TUpdateSQL(Args.Obj).SQL[Args.Values[0]] := V2O(Value) as TStrings;
end;

{ property Read ModifySQL: TStrings }

procedure TUpdateSQL_Read_ModifySQL(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TUpdateSQL(Args.Obj).ModifySQL);
end;

{ property Write ModifySQL(Value: TStrings) }

procedure TUpdateSQL_Write_ModifySQL(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TUpdateSQL(Args.Obj).ModifySQL := V2O(Value) as TStrings;
end;

{ property Read InsertSQL: TStrings }

procedure TUpdateSQL_Read_InsertSQL(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TUpdateSQL(Args.Obj).InsertSQL);
end;

{ property Write InsertSQL(Value: TStrings) }

procedure TUpdateSQL_Write_InsertSQL(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TUpdateSQL(Args.Obj).InsertSQL := V2O(Value) as TStrings;
end;

{ property Read DeleteSQL: TStrings }

procedure TUpdateSQL_Read_DeleteSQL(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TUpdateSQL(Args.Obj).DeleteSQL);
end;

{ property Write DeleteSQL(Value: TStrings) }

procedure TUpdateSQL_Write_DeleteSQL(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TUpdateSQL(Args.Obj).DeleteSQL := V2O(Value) as TStrings;
end;

{ TBlobStream }

{ constructor Create(Field: TBlobField; Mode: TBlobStreamMode) }

procedure TBlobStream_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBlobStream.Create(V2O(Args.Values[0]) as TBlobField, Args.Values[1]));
end;

{ function Read(var Buffer; Count: Longint): Longint; }

procedure TBlobStream_Read(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBlobStream(Args.Obj).Read(Args.Values[0], Args.Values[1]);
end;

{ function Write(const Buffer; Count: Longint): Longint; }

procedure TBlobStream_Write(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBlobStream(Args.Obj).Write(Args.Values[0], Args.Values[1]);
end;

{ function Seek(Offset: Longint; Origin: Word): Longint; }

procedure TBlobStream_Seek(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBlobStream(Args.Obj).Seek(Args.Values[0], Args.Values[1]);
end;

{ procedure Truncate; }

procedure TBlobStream_Truncate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBlobStream(Args.Obj).Truncate;
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cDbTables = 'DbTables';
begin
  with JvInterpreterAdapter do
  begin
    { EDBEngineError }
    AddClass(cDbTables, EDBEngineError, 'EDBEngineError');
    AddGet(EDBEngineError, 'Create', EDBEngineError_Create, 1, [varEmpty], varEmpty);
    AddGet(EDBEngineError, 'ErrorCount', EDBEngineError_Read_ErrorCount, 0, [varEmpty], varEmpty);
    AddGet(EDBEngineError, 'Errors', EDBEngineError_Read_Errors, 1, [varEmpty], varEmpty);
    { ENoResultSet }
    AddClass(cDbTables, ENoResultSet, 'ENoResultSet');
    { TConfigModes }
    AddConst(cDbTables, 'cfmVirtual', Ord(cfmVirtual));
    AddConst(cDbTables, 'cfmPersistent', Ord(cfmPersistent));
    AddConst(cDbTables, 'cfmSession', Ord(cfmSession));
    { TDatabaseEvent }
    AddConst(cDbTables, 'dbOpen', Ord(dbOpen));
    AddConst(cDbTables, 'dbClose', Ord(dbClose));
    AddConst(cDbTables, 'dbAdd', Ord(dbAdd));
    AddConst(cDbTables, 'dbRemove', Ord(dbRemove));
    AddConst(cDbTables, 'dbAddAlias', Ord(dbAddAlias));
    AddConst(cDbTables, 'dbDeleteAlias', Ord(dbDeleteAlias));
    AddConst(cDbTables, 'dbAddDriver', Ord(dbAddDriver));
    AddConst(cDbTables, 'dbDeleteDriver', Ord(dbDeleteDriver));
    { TTraceFlag }
    AddConst(cDbTables, 'tfQPrepare', Ord(tfQPrepare));
    AddConst(cDbTables, 'tfQExecute', Ord(tfQExecute));
    AddConst(cDbTables, 'tfError', Ord(tfError));
    AddConst(cDbTables, 'tfStmt', Ord(tfStmt));
    AddConst(cDbTables, 'tfConnect', Ord(tfConnect));
    AddConst(cDbTables, 'tfTransact', Ord(tfTransact));
    AddConst(cDbTables, 'tfBlob', Ord(tfBlob));
    AddConst(cDbTables, 'tfMisc', Ord(tfMisc));
    AddConst(cDbTables, 'tfVendor', Ord(tfVendor));
    AddConst(cDbTables, 'tfDataIn', Ord(tfDataIn));
    AddConst(cDbTables, 'tfDataOut', Ord(tfDataOut));
    { TSession }
    AddClass(cDbTables, TSession, 'TSession');
    AddGet(TSession, 'Create', TSession_Create, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'AddAlias', TSession_AddAlias, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TSession, 'AddDriver', TSession_AddDriver, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TSession, 'AddStandardAlias', TSession_AddStandardAlias, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TSession, 'ConfigMode', TSession_Read_ConfigMode, 0, [varEmpty], varEmpty);
    AddSet(TSession, 'ConfigMode', TSession_Write_ConfigMode, 0, [varEmpty]);
    AddGet(TSession, 'AddPassword', TSession_AddPassword, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'Close', TSession_Close, 0, [varEmpty], varEmpty);
    AddGet(TSession, 'CloseDatabase', TSession_CloseDatabase, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'DeleteAlias', TSession_DeleteAlias, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'DeleteDriver', TSession_DeleteDriver, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'DropConnections', TSession_DropConnections, 0, [varEmpty], varEmpty);
    AddGet(TSession, 'FindDatabase', TSession_FindDatabase, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'GetAliasNames', TSession_GetAliasNames, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'GetAliasParams', TSession_GetAliasParams, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TSession, 'GetAliasDriverName', TSession_GetAliasDriverName, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'GetConfigParams', TSession_GetConfigParams, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TSession, 'GetDatabaseNames', TSession_GetDatabaseNames, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'GetDriverNames', TSession_GetDriverNames, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'GetDriverParams', TSession_GetDriverParams, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TSession, 'GetPassword', TSession_GetPassword, 0, [varEmpty], varEmpty);
    AddGet(TSession, 'GetTableNames', TSession_GetTableNames, 5, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TSession, 'GetStoredProcNames', TSession_GetStoredProcNames, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TSession, 'IsAlias', TSession_IsAlias, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'ModifyAlias', TSession_ModifyAlias, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TSession, 'ModifyDriver', TSession_ModifyDriver, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TSession, 'Open', TSession_Open, 0, [varEmpty], varEmpty);
    AddGet(TSession, 'OpenDatabase', TSession_OpenDatabase, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'RemoveAllPasswords', TSession_RemoveAllPasswords, 0, [varEmpty], varEmpty);
    AddGet(TSession, 'RemovePassword', TSession_RemovePassword, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'SaveConfigFile', TSession_SaveConfigFile, 0, [varEmpty], varEmpty);
    AddGet(TSession, 'DatabaseCount', TSession_Read_DatabaseCount, 0, [varEmpty], varEmpty);
    AddGet(TSession, 'Databases', TSession_Read_Databases, 1, [varEmpty], varEmpty);
    AddGet(TSession, 'Handle', TSession_Read_Handle, 0, [varEmpty], varEmpty);
    AddGet(TSession, 'Locale', TSession_Read_Locale, 0, [varEmpty], varEmpty);
    AddGet(TSession, 'TraceFlags', TSession_Read_TraceFlags, 0, [varEmpty], varEmpty);
    AddSet(TSession, 'TraceFlags', TSession_Write_TraceFlags, 0, [varEmpty]);
    AddGet(TSession, 'Active', TSession_Read_Active, 0, [varEmpty], varEmpty);
    AddSet(TSession, 'Active', TSession_Write_Active, 0, [varEmpty]);
    AddGet(TSession, 'AutoSessionName', TSession_Read_AutoSessionName, 0, [varEmpty], varEmpty);
    AddSet(TSession, 'AutoSessionName', TSession_Write_AutoSessionName, 0, [varEmpty]);
    AddGet(TSession, 'KeepConnections', TSession_Read_KeepConnections, 0, [varEmpty], varEmpty);
    AddSet(TSession, 'KeepConnections', TSession_Write_KeepConnections, 0, [varEmpty]);
    AddGet(TSession, 'NetFileDir', TSession_Read_NetFileDir, 0, [varEmpty], varEmpty);
    AddSet(TSession, 'NetFileDir', TSession_Write_NetFileDir, 0, [varEmpty]);
    AddGet(TSession, 'PrivateDir', TSession_Read_PrivateDir, 0, [varEmpty], varEmpty);
    AddSet(TSession, 'PrivateDir', TSession_Write_PrivateDir, 0, [varEmpty]);
    AddGet(TSession, 'SessionName', TSession_Read_SessionName, 0, [varEmpty], varEmpty);
    AddSet(TSession, 'SessionName', TSession_Write_SessionName, 0, [varEmpty]);
    AddGet(TSession, 'SQLHourGlass', TSession_Read_SQLHourGlass, 0, [varEmpty], varEmpty);
    AddSet(TSession, 'SQLHourGlass', TSession_Write_SQLHourGlass, 0, [varEmpty]);
    { TTransIsolation }
    AddConst(cDbTables, 'tiDirtyRead', Ord(tiDirtyRead));
    AddConst(cDbTables, 'tiReadCommitted', Ord(tiReadCommitted));
    AddConst(cDbTables, 'tiRepeatableRead', Ord(tiRepeatableRead));
    { TDatabase }
    AddClass(cDbTables, TDatabase, 'TDatabase');
    AddGet(TDatabase, 'Create', TDatabase_Create, 1, [varEmpty], varEmpty);
    // AddGet(TDatabase, 'ApplyUpdates', TDatabase_ApplyUpdates, 1, [varEmpty], nil);
    AddGet(TDatabase, 'Close', TDatabase_Close, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'CloseDataSets', TDatabase_CloseDataSets, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'Commit', TDatabase_Commit, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'FlushSchemaCache', TDatabase_FlushSchemaCache, 1, [varEmpty], varEmpty);
    AddGet(TDatabase, 'Open', TDatabase_Open, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'Rollback', TDatabase_Rollback, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'StartTransaction', TDatabase_StartTransaction, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'ValidateName', TDatabase_ValidateName, 1, [varEmpty], varEmpty);
    AddGet(TDatabase, 'DataSetCount', TDatabase_Read_DataSetCount, 0, [varEmpty], varEmpty);
    AddIGet(TDatabase, 'DataSets', TDatabase_Read_DataSets, 1, [varEmpty], varEmpty);
    AddGet(TDatabase, 'Directory', TDatabase_Read_Directory, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'Directory', TDatabase_Write_Directory, 0, [varEmpty]);
    AddGet(TDatabase, 'Handle', TDatabase_Read_Handle, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'Handle', TDatabase_Write_Handle, 0, [varEmpty]);
    AddGet(TDatabase, 'IsSQLBased', TDatabase_Read_IsSQLBased, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'InTransaction', TDatabase_Read_InTransaction, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'Locale', TDatabase_Read_Locale, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'Session', TDatabase_Read_Session, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'Temporary', TDatabase_Read_Temporary, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'Temporary', TDatabase_Write_Temporary, 0, [varEmpty]);
    AddGet(TDatabase, 'SessionAlias', TDatabase_Read_SessionAlias, 0, [varEmpty], varEmpty);
    AddGet(TDatabase, 'TraceFlags', TDatabase_Read_TraceFlags, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'TraceFlags', TDatabase_Write_TraceFlags, 0, [varEmpty]);
    AddGet(TDatabase, 'AliasName', TDatabase_Read_AliasName, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'AliasName', TDatabase_Write_AliasName, 0, [varEmpty]);
    AddGet(TDatabase, 'Connected', TDatabase_Read_Connected, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'Connected', TDatabase_Write_Connected, 0, [varEmpty]);
    AddGet(TDatabase, 'DatabaseName', TDatabase_Read_DatabaseName, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'DatabaseName', TDatabase_Write_DatabaseName, 0, [varEmpty]);
    AddGet(TDatabase, 'DriverName', TDatabase_Read_DriverName, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'DriverName', TDatabase_Write_DriverName, 0, [varEmpty]);
    AddGet(TDatabase, 'HandleShared', TDatabase_Read_HandleShared, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'HandleShared', TDatabase_Write_HandleShared, 0, [varEmpty]);
    AddGet(TDatabase, 'KeepConnection', TDatabase_Read_KeepConnection, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'KeepConnection', TDatabase_Write_KeepConnection, 0, [varEmpty]);
    AddGet(TDatabase, 'LoginPrompt', TDatabase_Read_LoginPrompt, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'LoginPrompt', TDatabase_Write_LoginPrompt, 0, [varEmpty]);
    AddGet(TDatabase, 'Params', TDatabase_Read_Params, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'Params', TDatabase_Write_Params, 0, [varEmpty]);
    AddGet(TDatabase, 'SessionName', TDatabase_Read_SessionName, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'SessionName', TDatabase_Write_SessionName, 0, [varEmpty]);
    AddGet(TDatabase, 'TransIsolation', TDatabase_Read_TransIsolation, 0, [varEmpty], varEmpty);
    AddSet(TDatabase, 'TransIsolation', TDatabase_Write_TransIsolation, 0, [varEmpty]);
    { TRecNoStatus }
    AddConst(cDbTables, 'rnDbase', Ord(rnDbase));
    AddConst(cDbTables, 'rnParadox', Ord(rnParadox));
    AddConst(cDbTables, 'rnNotSupported', Ord(rnNotSupported));
    { TUpdateAction }
    AddConst(cDbTables, 'uaFail', Ord(uaFail));
    AddConst(cDbTables, 'uaAbort', Ord(uaAbort));
    AddConst(cDbTables, 'uaSkip', Ord(uaSkip));
    AddConst(cDbTables, 'uaRetry', Ord(uaRetry));
    AddConst(cDbTables, 'uaApplied', Ord(uaApplied));
    { TUpdateRecordTypes }
    AddConst(cDbTables, 'rtModified', Ord(rtModified));
    AddConst(cDbTables, 'rtInserted', Ord(rtInserted));
    AddConst(cDbTables, 'rtDeleted', Ord(rtDeleted));
    AddConst(cDbTables, 'rtUnmodified', Ord(rtUnmodified));
    { TDataSetUpdateObject }
    AddClass(cDbTables, TDataSetUpdateObject, 'TDataSetUpdateObject');
    { TKeyIndex }
    AddConst(cDbTables, 'kiLookup', Ord(kiLookup));
    AddConst(cDbTables, 'kiRangeStart', Ord(kiRangeStart));
    AddConst(cDbTables, 'kiRangeEnd', Ord(kiRangeEnd));
    AddConst(cDbTables, 'kiCurRangeStart', Ord(kiCurRangeStart));
    AddConst(cDbTables, 'kiCurRangeEnd', Ord(kiCurRangeEnd));
    AddConst(cDbTables, 'kiSave', Ord(kiSave));
    { TBDEDataSet }
    AddClass(cDbTables, TBDEDataSet, 'TBDEDataSet');
    AddGet(TBDEDataSet, 'Create', TBDEDataSet_Create, 1, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'ApplyUpdates', TBDEDataSet_ApplyUpdates, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'BookmarkValid', TBDEDataSet_BookmarkValid, 1, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'Cancel', TBDEDataSet_Cancel, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'CancelUpdates', TBDEDataSet_CancelUpdates, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'CacheBlobs', TBDEDataSet_Read_CacheBlobs, 0, [varEmpty], varEmpty);
    AddSet(TBDEDataSet, 'CacheBlobs', TBDEDataSet_Write_CacheBlobs, 0, [varEmpty]);
    AddGet(TBDEDataSet, 'CompareBookmarks', TBDEDataSet_CompareBookmarks, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'CommitUpdates', TBDEDataSet_CommitUpdates, 0, [varEmpty], varEmpty);
   // AddGet(TBDEDataSet, 'ConstraintCallBack', TBDEDataSet_ConstraintCallBack, 2, [varEmpty, varByRef], nil);
    AddGet(TBDEDataSet, 'ConstraintsDisabled', TBDEDataSet_ConstraintsDisabled, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'CreateBlobStream', TBDEDataSet_CreateBlobStream, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'DisableConstraints', TBDEDataSet_DisableConstraints, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'EnableConstraints', TBDEDataSet_EnableConstraints, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'FetchAll', TBDEDataSet_FetchAll, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'FlushBuffers', TBDEDataSet_FlushBuffers, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'GetCurrentRecord', TBDEDataSet_GetCurrentRecord, 1, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'GetIndexInfo', TBDEDataSet_GetIndexInfo, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'Locate', TBDEDataSet_Locate, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'Lookup', TBDEDataSet_Lookup, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'IsSequenced', TBDEDataSet_IsSequenced, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'Post', TBDEDataSet_Post, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'RevertRecord', TBDEDataSet_RevertRecord, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'UpdateStatus', TBDEDataSet_UpdateStatus, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'Translate', TBDEDataSet_Translate, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'ExpIndex', TBDEDataSet_Read_ExpIndex, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'Handle', TBDEDataSet_Read_Handle, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'KeySize', TBDEDataSet_Read_KeySize, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'Locale', TBDEDataSet_Read_Locale, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'UpdateObject', TBDEDataSet_Read_UpdateObject, 0, [varEmpty], varEmpty);
    AddSet(TBDEDataSet, 'UpdateObject', TBDEDataSet_Write_UpdateObject, 0, [varEmpty]);
    AddGet(TBDEDataSet, 'UpdatesPending', TBDEDataSet_Read_UpdatesPending, 0, [varEmpty], varEmpty);
    AddGet(TBDEDataSet, 'UpdateRecordTypes', TBDEDataSet_Read_UpdateRecordTypes, 0, [varEmpty], varEmpty);
    AddSet(TBDEDataSet, 'UpdateRecordTypes', TBDEDataSet_Write_UpdateRecordTypes, 0, [varEmpty]);
    AddGet(TBDEDataSet, 'CachedUpdates', TBDEDataSet_Read_CachedUpdates, 0, [varEmpty], varEmpty);
    AddSet(TBDEDataSet, 'CachedUpdates', TBDEDataSet_Write_CachedUpdates, 0, [varEmpty]);
    { TUpdateMode }
    AddConst(cDbTables, 'upWhereAll', Ord(upWhereAll));
    AddConst(cDbTables, 'upWhereChanged', Ord(upWhereChanged));
    AddConst(cDbTables, 'upWhereKeyOnly', Ord(upWhereKeyOnly));
    { TDBDataSet }
    AddClass(cDbTables, TDBDataSet, 'TDBDataSet');
    AddGet(TDBDataSet, 'CheckOpen', TDBDataSet_CheckOpen, 1, [varEmpty], varEmpty);
    AddGet(TDBDataSet, 'CloseDatabase', TDBDataSet_CloseDatabase, 1, [varEmpty], varEmpty);
    AddGet(TDBDataSet, 'OpenDatabase', TDBDataSet_OpenDatabase, 0, [varEmpty], varEmpty);
    AddGet(TDBDataSet, 'Database', TDBDataSet_Read_Database, 0, [varEmpty], varEmpty);
    AddGet(TDBDataSet, 'DBHandle', TDBDataSet_Read_DBHandle, 0, [varEmpty], varEmpty);
    AddGet(TDBDataSet, 'DBLocale', TDBDataSet_Read_DBLocale, 0, [varEmpty], varEmpty);
    AddGet(TDBDataSet, 'DBSession', TDBDataSet_Read_DBSession, 0, [varEmpty], varEmpty);
    AddGet(TDBDataSet, 'DatabaseName', TDBDataSet_Read_DatabaseName, 0, [varEmpty], varEmpty);
    AddSet(TDBDataSet, 'DatabaseName', TDBDataSet_Write_DatabaseName, 0, [varEmpty]);
    AddGet(TDBDataSet, 'SessionName', TDBDataSet_Read_SessionName, 0, [varEmpty], varEmpty);
    AddSet(TDBDataSet, 'SessionName', TDBDataSet_Write_SessionName, 0, [varEmpty]);
    { TBatchMode }
    AddConst(cDbTables, 'batAppend', Ord(batAppend));
    AddConst(cDbTables, 'batUpdate', Ord(batUpdate));
    AddConst(cDbTables, 'batAppendUpdate', Ord(batAppendUpdate));
    AddConst(cDbTables, 'batDelete', Ord(batDelete));
    AddConst(cDbTables, 'batCopy', Ord(batCopy));
    { TTableType }
    AddConst(cDbTables, 'ttDefault', Ord(ttDefault));
    AddConst(cDbTables, 'ttParadox', Ord(ttParadox));
    AddConst(cDbTables, 'ttDBase', Ord(ttDBase));
    AddConst(cDbTables, 'ttASCII', Ord(ttASCII));
    { TLockType }
    AddConst(cDbTables, 'ltReadLock', Ord(ltReadLock));
    AddConst(cDbTables, 'ltWriteLock', Ord(ltWriteLock));
    { TTable }
    AddClass(cDbTables, TTable, 'TTable');
    AddGet(TTable, 'Create', TTable_Create, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'BatchMove', TTable_BatchMove, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTable, 'AddIndex', TTable_AddIndex, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TTable, 'ApplyRange', TTable_ApplyRange, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'CancelRange', TTable_CancelRange, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'CloseIndexFile', TTable_CloseIndexFile, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'CreateTable', TTable_CreateTable, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'DeleteIndex', TTable_DeleteIndex, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'DeleteTable', TTable_DeleteTable, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'EditKey', TTable_EditKey, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'EditRangeEnd', TTable_EditRangeEnd, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'EditRangeStart', TTable_EditRangeStart, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'EmptyTable', TTable_EmptyTable, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'FindKey', TTable_FindKey, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'FindNearest', TTable_FindNearest, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'GetIndexNames', TTable_GetIndexNames, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'GotoCurrent', TTable_GotoCurrent, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'GotoKey', TTable_GotoKey, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'GotoNearest', TTable_GotoNearest, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'LockTable', TTable_LockTable, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'OpenIndexFile', TTable_OpenIndexFile, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'RenameTable', TTable_RenameTable, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'SetKey', TTable_SetKey, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'SetRange', TTable_SetRange, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTable, 'SetRangeEnd', TTable_SetRangeEnd, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'SetRangeStart', TTable_SetRangeStart, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'UnlockTable', TTable_UnlockTable, 1, [varEmpty], varEmpty);
    AddGet(TTable, 'IndexDefs', TTable_Read_IndexDefs, 0, [varEmpty], varEmpty);
    AddGet(TTable, 'IndexFieldCount', TTable_Read_IndexFieldCount, 0, [varEmpty], varEmpty);
    AddIGet(TTable, 'IndexFields', TTable_Read_IndexFields, 1, [varEmpty], varEmpty);
    AddISet(TTable, 'IndexFields', TTable_Write_IndexFields, 1, [varNull]);
    AddGet(TTable, 'KeyExclusive', TTable_Read_KeyExclusive, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'KeyExclusive', TTable_Write_KeyExclusive, 0, [varEmpty]);
    AddGet(TTable, 'KeyFieldCount', TTable_Read_KeyFieldCount, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'KeyFieldCount', TTable_Write_KeyFieldCount, 0, [varEmpty]);
    AddGet(TTable, 'TableLevel', TTable_Read_TableLevel, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'TableLevel', TTable_Write_TableLevel, 0, [varEmpty]);
    AddGet(TTable, 'Exclusive', TTable_Read_Exclusive, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'Exclusive', TTable_Write_Exclusive, 0, [varEmpty]);
    AddGet(TTable, 'IndexFieldNames', TTable_Read_IndexFieldNames, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'IndexFieldNames', TTable_Write_IndexFieldNames, 0, [varEmpty]);
    AddGet(TTable, 'IndexFiles', TTable_Read_IndexFiles, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'IndexFiles', TTable_Write_IndexFiles, 0, [varEmpty]);
    AddGet(TTable, 'IndexName', TTable_Read_IndexName, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'IndexName', TTable_Write_IndexName, 0, [varEmpty]);
    AddGet(TTable, 'MasterFields', TTable_Read_MasterFields, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'MasterFields', TTable_Write_MasterFields, 0, [varEmpty]);
    AddGet(TTable, 'MasterSource', TTable_Read_MasterSource, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'MasterSource', TTable_Write_MasterSource, 0, [varEmpty]);
    AddGet(TTable, 'ReadOnly', TTable_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'ReadOnly', TTable_Write_ReadOnly, 0, [varEmpty]);
    AddGet(TTable, 'TableName', TTable_Read_TableName, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'TableName', TTable_Write_TableName, 0, [varEmpty]);
    AddGet(TTable, 'TableType', TTable_Read_TableType, 0, [varEmpty], varEmpty);
    AddSet(TTable, 'TableType', TTable_Write_TableType, 0, [varEmpty]);
    { TBatchMove }
    AddClass(cDbTables, TBatchMove, 'TBatchMove');
    AddGet(TBatchMove, 'Create', TBatchMove_Create, 1, [varEmpty], varEmpty);
    AddGet(TBatchMove, 'Execute', TBatchMove_Execute, 0, [varEmpty], varEmpty);
    AddGet(TBatchMove, 'ChangedCount', TBatchMove_Read_ChangedCount, 0, [varEmpty], varEmpty);
    AddGet(TBatchMove, 'KeyViolCount', TBatchMove_Read_KeyViolCount, 0, [varEmpty], varEmpty);
    AddGet(TBatchMove, 'MovedCount', TBatchMove_Read_MovedCount, 0, [varEmpty], varEmpty);
    AddGet(TBatchMove, 'ProblemCount', TBatchMove_Read_ProblemCount, 0, [varEmpty], varEmpty);
    AddGet(TBatchMove, 'AbortOnKeyViol', TBatchMove_Read_AbortOnKeyViol, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'AbortOnKeyViol', TBatchMove_Write_AbortOnKeyViol, 0, [varEmpty]);
    AddGet(TBatchMove, 'AbortOnProblem', TBatchMove_Read_AbortOnProblem, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'AbortOnProblem', TBatchMove_Write_AbortOnProblem, 0, [varEmpty]);
    AddGet(TBatchMove, 'CommitCount', TBatchMove_Read_CommitCount, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'CommitCount', TBatchMove_Write_CommitCount, 0, [varEmpty]);
    AddGet(TBatchMove, 'ChangedTableName', TBatchMove_Read_ChangedTableName, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'ChangedTableName', TBatchMove_Write_ChangedTableName, 0, [varEmpty]);
    AddGet(TBatchMove, 'Destination', TBatchMove_Read_Destination, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'Destination', TBatchMove_Write_Destination, 0, [varEmpty]);
    AddGet(TBatchMove, 'KeyViolTableName', TBatchMove_Read_KeyViolTableName, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'KeyViolTableName', TBatchMove_Write_KeyViolTableName, 0, [varEmpty]);
    AddGet(TBatchMove, 'Mappings', TBatchMove_Read_Mappings, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'Mappings', TBatchMove_Write_Mappings, 0, [varEmpty]);
    AddGet(TBatchMove, 'Mode', TBatchMove_Read_Mode, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'Mode', TBatchMove_Write_Mode, 0, [varEmpty]);
    AddGet(TBatchMove, 'ProblemTableName', TBatchMove_Read_ProblemTableName, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'ProblemTableName', TBatchMove_Write_ProblemTableName, 0, [varEmpty]);
    AddGet(TBatchMove, 'RecordCount', TBatchMove_Read_RecordCount, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'RecordCount', TBatchMove_Write_RecordCount, 0, [varEmpty]);
    AddGet(TBatchMove, 'Source', TBatchMove_Read_Source, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'Source', TBatchMove_Write_Source, 0, [varEmpty]);
    AddGet(TBatchMove, 'Transliterate', TBatchMove_Read_Transliterate, 0, [varEmpty], varEmpty);
    AddSet(TBatchMove, 'Transliterate', TBatchMove_Write_Transliterate, 0, [varEmpty]);
    { TParamType }
    AddConst(cDbTables, 'ptUnknown', Ord(ptUnknown));
    AddConst(cDbTables, 'ptInput', Ord(ptInput));
    AddConst(cDbTables, 'ptOutput', Ord(ptOutput));
    AddConst(cDbTables, 'ptInputOutput', Ord(ptInputOutput));
    AddConst(cDbTables, 'ptResult', Ord(ptResult));
    { TParam }
    AddClass(cDbTables, TParam, 'TParam');
    AddGet(TParam, 'Create', TParam_Create, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TParam, 'Assign', TParam_Assign, 1, [varEmpty], varEmpty);
    AddGet(TParam, 'AssignField', TParam_AssignField, 1, [varEmpty], varEmpty);
    AddGet(TParam, 'AssignFieldValue', TParam_AssignFieldValue, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TParam, 'Clear', TParam_Clear, 0, [varEmpty], varEmpty);
    AddGet(TParam, 'GetData', TParam_GetData, 1, [varEmpty], varEmpty);
    AddGet(TParam, 'GetDataSize', TParam_GetDataSize, 0, [varEmpty], varEmpty);
    AddGet(TParam, 'LoadFromFile', TParam_LoadFromFile, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TParam, 'LoadFromStream', TParam_LoadFromStream, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TParam, 'SetBlobData', TParam_SetBlobData, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TParam, 'SetData', TParam_SetData, 1, [varEmpty], varEmpty);
    AddGet(TParam, 'AsBCD', TParam_Read_AsBCD, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsBCD', TParam_Write_AsBCD, 0, [varEmpty]);
    AddGet(TParam, 'AsBlob', TParam_Read_AsBlob, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsBlob', TParam_Write_AsBlob, 0, [varEmpty]);
    AddGet(TParam, 'AsBoolean', TParam_Read_AsBoolean, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsBoolean', TParam_Write_AsBoolean, 0, [varEmpty]);
    AddGet(TParam, 'AsCurrency', TParam_Read_AsCurrency, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsCurrency', TParam_Write_AsCurrency, 0, [varEmpty]);
    AddGet(TParam, 'AsDate', TParam_Read_AsDate, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsDate', TParam_Write_AsDate, 0, [varEmpty]);
    AddGet(TParam, 'AsDateTime', TParam_Read_AsDateTime, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsDateTime', TParam_Write_AsDateTime, 0, [varEmpty]);
    AddGet(TParam, 'AsFloat', TParam_Read_AsFloat, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsFloat', TParam_Write_AsFloat, 0, [varEmpty]);
    AddGet(TParam, 'AsInteger', TParam_Read_AsInteger, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsInteger', TParam_Write_AsInteger, 0, [varEmpty]);
    AddGet(TParam, 'AsSmallInt', TParam_Read_AsSmallInt, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsSmallInt', TParam_Write_AsSmallInt, 0, [varEmpty]);
    AddGet(TParam, 'AsMemo', TParam_Read_AsMemo, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsMemo', TParam_Write_AsMemo, 0, [varEmpty]);
    AddGet(TParam, 'AsString', TParam_Read_AsString, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsString', TParam_Write_AsString, 0, [varEmpty]);
    AddGet(TParam, 'AsTime', TParam_Read_AsTime, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsTime', TParam_Write_AsTime, 0, [varEmpty]);
    AddGet(TParam, 'AsWord', TParam_Read_AsWord, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'AsWord', TParam_Write_AsWord, 0, [varEmpty]);
    AddGet(TParam, 'Bound', TParam_Read_Bound, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'Bound', TParam_Write_Bound, 0, [varEmpty]);
    AddGet(TParam, 'DataType', TParam_Read_DataType, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'DataType', TParam_Write_DataType, 0, [varEmpty]);
    AddGet(TParam, 'IsNull', TParam_Read_IsNull, 0, [varEmpty], varEmpty);
    AddGet(TParam, 'Name', TParam_Read_Name, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'Name', TParam_Write_Name, 0, [varEmpty]);
    AddGet(TParam, 'ParamType', TParam_Read_ParamType, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'ParamType', TParam_Write_ParamType, 0, [varEmpty]);
    AddGet(TParam, 'Text', TParam_Read_Text, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'Text', TParam_Write_Text, 0, [varEmpty]);
    AddGet(TParam, 'Value', TParam_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TParam, 'Value', TParam_Write_Value, 0, [varEmpty]);
    { TParams }
    AddClass(cDbTables, TParams, 'TParams');
    AddGet(TParams, 'Create', TParams_Create, 0, [varEmpty], varEmpty);
    AddGet(TParams, 'Assign', TParams_Assign, 1, [varEmpty], varEmpty);
    AddGet(TParams, 'AssignValues', TParams_AssignValues, 1, [varEmpty], varEmpty);
    AddGet(TParams, 'AddParam', TParams_AddParam, 1, [varEmpty], varEmpty);
    AddGet(TParams, 'RemoveParam', TParams_RemoveParam, 1, [varEmpty], varEmpty);
    AddGet(TParams, 'CreateParam', TParams_CreateParam, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TParams, 'Count', TParams_Count, 0, [varEmpty], varEmpty);
    AddGet(TParams, 'Clear', TParams_Clear, 0, [varEmpty], varEmpty);
    AddGet(TParams, 'GetParamList', TParams_GetParamList, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TParams, 'IsEqual', TParams_IsEqual, 1, [varEmpty], varEmpty);
    AddGet(TParams, 'ParamByName', TParams_ParamByName, 1, [varEmpty], varEmpty);
    AddGet(TParams, 'Items', TParams_Read_Items, 1, [varEmpty], varEmpty);
    { TParamBindMode }
    AddConst(cDbTables, 'pbByName', Ord(pbByName));
    AddConst(cDbTables, 'pbByNumber', Ord(pbByNumber));
    { TStoredProc }
    AddClass(cDbTables, TStoredProc, 'TStoredProc');
    AddGet(TStoredProc, 'Create', TStoredProc_Create, 1, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'CopyParams', TStoredProc_CopyParams, 1, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'DescriptionsAvailable', TStoredProc_DescriptionsAvailable, 0, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'ExecProc', TStoredProc_ExecProc, 0, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'ParamByName', TStoredProc_ParamByName, 1, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'Prepare', TStoredProc_Prepare, 0, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'GetResults', TStoredProc_GetResults, 0, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'UnPrepare', TStoredProc_UnPrepare, 0, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'ParamCount', TStoredProc_Read_ParamCount, 0, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'StmtHandle', TStoredProc_Read_StmtHandle, 0, [varEmpty], varEmpty);
    AddGet(TStoredProc, 'Prepared', TStoredProc_Read_Prepared, 0, [varEmpty], varEmpty);
    AddSet(TStoredProc, 'Prepared', TStoredProc_Write_Prepared, 0, [varEmpty]);
    AddGet(TStoredProc, 'StoredProcName', TStoredProc_Read_StoredProcName, 0, [varEmpty], varEmpty);
    AddSet(TStoredProc, 'StoredProcName', TStoredProc_Write_StoredProcName, 0, [varEmpty]);
    AddGet(TStoredProc, 'Overload', TStoredProc_Read_Overload, 0, [varEmpty], varEmpty);
    AddSet(TStoredProc, 'Overload', TStoredProc_Write_Overload, 0, [varEmpty]);
    AddGet(TStoredProc, 'Params', TStoredProc_Read_Params, 0, [varEmpty], varEmpty);
    AddSet(TStoredProc, 'Params', TStoredProc_Write_Params, 0, [varEmpty]);
    AddGet(TStoredProc, 'ParamBindMode', TStoredProc_Read_ParamBindMode, 0, [varEmpty], varEmpty);
    AddSet(TStoredProc, 'ParamBindMode', TStoredProc_Write_ParamBindMode, 0, [varEmpty]);
    { TQuery }
    AddClass(cDbTables, TQuery, 'TQuery');
    AddGet(TQuery, 'Create', TQuery_Create, 1, [varEmpty], varEmpty);
    AddGet(TQuery, 'ExecSQL', TQuery_ExecSQL, 0, [varEmpty], varEmpty);
    AddGet(TQuery, 'ParamByName', TQuery_ParamByName, 1, [varEmpty], varEmpty);
    AddGet(TQuery, 'Prepare', TQuery_Prepare, 0, [varEmpty], varEmpty);
    AddGet(TQuery, 'UnPrepare', TQuery_UnPrepare, 0, [varEmpty], varEmpty);
    AddGet(TQuery, 'Prepared', TQuery_Read_Prepared, 0, [varEmpty], varEmpty);
    AddSet(TQuery, 'Prepared', TQuery_Write_Prepared, 0, [varEmpty]);
    AddGet(TQuery, 'ParamCount', TQuery_Read_ParamCount, 0, [varEmpty], varEmpty);
    AddGet(TQuery, 'Local', TQuery_Read_Local, 0, [varEmpty], varEmpty);
    AddGet(TQuery, 'StmtHandle', TQuery_Read_StmtHandle, 0, [varEmpty], varEmpty);
    AddGet(TQuery, 'Text', TQuery_Read_Text, 0, [varEmpty], varEmpty);
    AddGet(TQuery, 'RowsAffected', TQuery_Read_RowsAffected, 0, [varEmpty], varEmpty);
    AddGet(TQuery, 'SQLBinary', TQuery_Read_SQLBinary, 0, [varEmpty], varEmpty);
    AddSet(TQuery, 'SQLBinary', TQuery_Write_SQLBinary, 0, [varEmpty]);
    AddGet(TQuery, 'Constrained', TQuery_Read_Constrained, 0, [varEmpty], varEmpty);
    AddSet(TQuery, 'Constrained', TQuery_Write_Constrained, 0, [varEmpty]);
    AddGet(TQuery, 'DataSource', TQuery_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TQuery, 'DataSource', TQuery_Write_DataSource, 0, [varEmpty]);
    AddGet(TQuery, 'ParamCheck', TQuery_Read_ParamCheck, 0, [varEmpty], varEmpty);
    AddSet(TQuery, 'ParamCheck', TQuery_Write_ParamCheck, 0, [varEmpty]);
    AddGet(TQuery, 'RequestLive', TQuery_Read_RequestLive, 0, [varEmpty], varEmpty);
    AddSet(TQuery, 'RequestLive', TQuery_Write_RequestLive, 0, [varEmpty]);
    AddGet(TQuery, 'SQL', TQuery_Read_SQL, 0, [varEmpty], varEmpty);
    AddSet(TQuery, 'SQL', TQuery_Write_SQL, 0, [varEmpty]);
    AddGet(TQuery, 'Params', TQuery_Read_Params, 0, [varEmpty], varEmpty);
    AddSet(TQuery, 'Params', TQuery_Write_Params, 0, [varEmpty]);
    AddGet(TQuery, 'UniDirectional', TQuery_Read_UniDirectional, 0, [varEmpty], varEmpty);
    AddSet(TQuery, 'UniDirectional', TQuery_Write_UniDirectional, 0, [varEmpty]);
    { TUpdateSQL }
    AddClass(cDbTables, TUpdateSQL, 'TUpdateSQL');
    AddGet(TUpdateSQL, 'Create', TUpdateSQL_Create, 1, [varEmpty], varEmpty);
    AddGet(TUpdateSQL, 'Apply', TUpdateSQL_Apply, 1, [varEmpty], varEmpty);
    AddGet(TUpdateSQL, 'ExecSQL', TUpdateSQL_ExecSQL, 1, [varEmpty], varEmpty);
    AddGet(TUpdateSQL, 'SetParams', TUpdateSQL_SetParams, 1, [varEmpty], varEmpty);
    AddGet(TUpdateSQL, 'Query', TUpdateSQL_Read_Query, 1, [varEmpty], varEmpty);
    AddGet(TUpdateSQL, 'SQL', TUpdateSQL_Read_SQL, 1, [varEmpty], varEmpty);
    AddSet(TUpdateSQL, 'SQL', TUpdateSQL_Write_SQL, 1, [varNull]);
    AddGet(TUpdateSQL, 'ModifySQL', TUpdateSQL_Read_ModifySQL, 0, [varEmpty], varEmpty);
    AddSet(TUpdateSQL, 'ModifySQL', TUpdateSQL_Write_ModifySQL, 0, [varEmpty]);
    AddGet(TUpdateSQL, 'InsertSQL', TUpdateSQL_Read_InsertSQL, 0, [varEmpty], varEmpty);
    AddSet(TUpdateSQL, 'InsertSQL', TUpdateSQL_Write_InsertSQL, 0, [varEmpty]);
    AddGet(TUpdateSQL, 'DeleteSQL', TUpdateSQL_Read_DeleteSQL, 0, [varEmpty], varEmpty);
    AddSet(TUpdateSQL, 'DeleteSQL', TUpdateSQL_Write_DeleteSQL, 0, [varEmpty]);
    { TBlobStream }
    AddClass(cDbTables, TBlobStream, 'TBlobStream');
    AddGet(TBlobStream, 'Create', TBlobStream_Create, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TBlobStream, 'Read', TBlobStream_Read, 2, [varByRef, varEmpty], varEmpty);
    AddGet(TBlobStream, 'Write', TBlobStream_Write, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TBlobStream, 'Seek', TBlobStream_Seek, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TBlobStream, 'Truncate', TBlobStream_Truncate, 0, [varEmpty], varEmpty);
  end;
  RegisterClasses([TSession, TDatabase, TTable, TQuery, TStoredProc,
    TBatchMove, TParam, TParams, TUpdateSQL]);
end;

end.

