{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBUtil.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : db-aware routines

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDBUtil;

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF}
  Bde, SysUtils, Classes, Forms,
  DB, DBTables;

type

  EJvScriptError = class(Exception)
    ErrPos: Integer;
    constructor Create2(AMessage: string; AErrPos: Integer);
  end;

  TCommit = (ctNone, ctStep, ctAll);
  TJvProgressEvent = procedure(UserData: Integer; var Cancel: Boolean; Line: Integer) of object;

{ ExecuteSQLScript executes SQL script }

procedure ExecuteSQLScript(Base: TDataBase; const Script: string; const Commit: TCommit; OnProgress: TJvProgressEvent; const
  UserData: Integer);

{ GetQueryResult executes SQL Query and returns Result as Variant }

function GetQueryResult(const DatabaseName, SQL: string): Variant;

{ GetStoredProcResult executes SQL stored procedure and returns
  value of ResultName parameters as Variant }

function GetStoredProcResult(const ADatabaseName, AStoredProcName: string; AParams: array of Variant;
  const AResultName: string): Variant;

{ StrFieldDesc returns field description of given FLDDesc record }

function StrFieldDesc(Field: FLDDesc): string;

function Var2Type(V: Variant; const VarType: Integer): Variant;

procedure CopyRecord(DataSet: TDataSet);

{ AddReference create reference for paradox table,
  RefField and MasterField are field numbers (first field has number 1)
  Tables allready must have indices for this fields }

procedure AddReference(Tbl: TTable; RefName: string; RefField: Word;
  MasterTable: string; MasterField: Word; ModOp, DelOp: RINTQual);

{ AddMasterPassword extracted from "bde.hlp" file }
procedure AddMasterPassword(Table: TTable; pswd: string);

{ PackTable extracted from "bde.hlp" file }
procedure PackTable(Table: TTable);

procedure PackEncryptedTable(Table: TTable; pswd: string);

function EncodeQuotes(const S: string): string;

{*********************** from JvStrUtil unit ***********************}

function Cmp(const S1, S2: string): Boolean;

{ SubStr returns substring from string, S,
  separated with Separator string}

function SubStr(const S: string; const Index: Integer; const Separator: string): string;

{ SubStrEnd same to previous function but Index numerated
  from the end of string }

function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;

{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }
  
function ReplaceString(S: string; const OldPattern, NewPattern: string): string;

{ GetXYByPos is same to previous function, but
  returns X position in line too}

procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer);

{####################### from JvStrUtil unit #######################}

implementation

uses
  JvTypes;

{$IFNDEF COMPILER3_UP}

function AnsiStrIComp(S1, S2: PChar): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1, S2, -1) - 2;
end;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, MaxLen, S2, MaxLen) - 2;
end;

{$ENDIF COMPILER3_UP}

constructor EJvScriptError.Create2(AMessage: string; AErrPos: Integer);
begin
  inherited Create(AMessage);
  ErrPos := AErrPos;
end;

procedure ExecuteSQLScript(Base: TDataBase; const Script: string; const Commit: TCommit;
  OnProgress: TJvProgressEvent; const UserData: Integer);
var
  N: Integer;
  Term: Char;

  function NextQuery: string;
  var
    C: Char;
    Rem: Boolean;
  begin
    Result := '';
    Rem := False;
    while Length(Script) >= N do
    begin
      C := Script[N];
      Inc(N);
      if (C = Term) and not Rem then
        Exit;
      Result := Result + C;
      if (C = '/') and (Length(Script) >= N) and (Script[N] = '*') then
        Rem := True;
      if (C = '*') and (Length(Script) >= N) and (Script[N] = '/') and Rem then
        Rem := False;
    end;
    Result := '';
  end;

  function SetTerm(S: string): Boolean;
  var
    Rem: Boolean;
  begin
    Rem := False;
    while (Length(S) > 0) do
    begin
      if (S[1] in [' ', #13, #10]) then
        Delete(S, 1, 1)
      else
      if Rem then
        if (S[1] = '*') and (Length(S) > 1) and (S[2] = '/') then
        begin
          Delete(S, 1, 2);
          Rem := False;
        end
        else
          Delete(S, 1, 1)
      else
      if (S[1] = '/') and (Length(S) > 1) and (S[2] = '*') then
      begin
        Delete(S, 1, 2);
        Rem := True;
      end
      else
        Break;
    end;
    Result := AnsiStrLIComp(PChar(S), 'set term', 8) = 0;
    if Result then
    begin
      S := Trim(Copy(S, 9, 1024));
      if Length(S) = 1 then
        Term := S[1]
      else
        EDatabaseError.Create('Bad term');
      Exit;
    end;
    Result := AnsiStrLIComp(PChar(S), 'commit work', 11) = 0;
    if Result then
    begin
      Base.Commit;
      Base.StartTransaction;
      Exit;
    end;
  end;

var
  Q: string;
  ErrPos: Integer;
  NBeg: Integer;
  X, Y, N2: Integer;
  S1: string;
  Query: TQuery;
  Stop: Boolean;
begin
  if Commit in [ctStep, ctAll] then
    Base.StartTransaction;
  Query := TQuery.Create(Application);
  try
    Query.DatabaseName := Base.DatabaseName;
    Query.ParamCheck := False;
    N := 1;
    Term := ';';
    Stop := False;
    NBeg := 1;
    try
      Q := NextQuery;
      while Q <> '' do
      begin
        if not SetTerm(Q) then
        begin
          if Assigned(OnProgress) then
          begin
            S1 := Q;
            N2 := 0;
            while (Length(S1) > 0) and (S1[1] in [' ', #13, #10]) do
            begin
              Delete(S1, 1, 1);
              Inc(N2);
            end;
            GetXYByPos(Script, NBeg + N2, X, Y);
            if Assigned(OnProgress) then
              OnProgress(UserData, Stop, Y)
            else
              // (rom) i do not like this
              Application.ProcessMessages;
            if Stop then
              Abort;
          end;
          Query.SQL.Text := Q;
          Query.ExecSQL;
          if Commit = ctStep then
          begin
            Base.Commit;
            Base.StartTransaction;
          end;
          Query.Close;
        end;
        NBeg := N + 1;
        Q := NextQuery;
      end;
      if Commit in [ctStep, ctAll] then
        Base.Commit;
    except
      on E: Exception do
      begin
        if Commit in [ctStep, ctAll] then
          Base.Rollback;
        if E is EDatabaseError then
        begin
          ErrPos := NBeg;
          //..
          raise EJvScriptError.Create2(E.Message, ErrPos)
        end
        else
          raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

function GetQueryResult(const DatabaseName, SQL: string): Variant;
var
  Query: TQuery;
begin
  Query := TQuery.Create(Application);
  try
    Query.DatabaseName := DatabaseName;
    Query.ParamCheck := False;
    Query.SQL.Text := SQL;
    Query.Open;
    Result := Query.Fields[0].AsVariant;
  finally
    Query.Free;
  end;
end;

function GetStoredProcResult(const ADatabaseName, AStoredProcName: string; AParams: array of Variant;
  const AResultName: string): Variant;
var
  I: Integer;
begin
  with TStoredProc.Create(Application) do
    try
      DatabaseName := ADatabaseName;
      ParamBindMode := pbByNumber;
      StoredProcName := AStoredProcName;
      Prepare;
      for I := Low(AParams) to High(AParams) do
        Params[I].Value := AParams[I];
      ExecProc;
      Result := ParamByName(AResultName).Value;
    finally
      Free;
    end;
end;

function StrFieldDesc(Field: FLDDesc): string;

  function SUnits1: string;
  begin
    Result := IntToStr(Field.iUnits1);
  end;

  function SUnits2: string;
  begin
    if Field.iUnits2 < 0 then
      Result := IntToStr(-Field.iUnits2)
    else
      Result := IntToStr(Field.iUnits2);
  end;

begin
  with Field do
    case iFldType of
      fldUNKNOWN:
        Result := 'unknown';
      fldZSTRING:
       Result := 'string'; { Null terminated string }
      fldDATE:
        Result := 'date'; { Date (32 bit) }
      fldBLOB:
        Result := 'blob'; { Blob }
      fldBOOL:
        Result := 'boolean'; { Boolean  (16 bit) }
      fldINT16:
        Result := 'integer'; { 16 bit signed number }
      fldINT32:
        Result := 'long integer'; { 32 bit signed number }

      fldFLOAT:
        Result := 'float'; { 64 bit floating point }
      fldBCD:
        Result := 'BCD'; { BCD }
      fldBYTES:
        Result := 'bytes'; { Fixed number of bytes }
      fldTIME:
        Result := 'time'; { Time (32 bit) }
      fldTIMESTAMP:
        Result := 'timestamp'; { Time-stamp  (64 bit) }
      fldUINT16:
        Result := 'unsigned int'; { Unsigned 16 bit Integer }
      fldUINT32:
        Result := 'unsigned long int'; { Unsigned 32 bit Integer }

      fldFLOATIEEE:
        Result := 'float IEEE'; { 80-bit IEEE float }
      fldVARBYTES:
        Result := 'varbytes'; { Length prefixed var bytes }
      fldLOCKINFO:
        Result := 'lockinfo'; { Look for LOCKINFO typedef }
      {$IFDEF COMPILER3_UP}
      fldCURSOR:
        Result := 'Oracle cursor'; { For Oracle Cursor type }
      {$ENDIF COMPILER3_UP}

     { Paradox types (Physical) }
      fldPDXCHAR:
        Result := 'alpha(' + SUnits1 + ')'; { Alpha    (string) }
      fldPDXNUM:
        Result := 'numeric(' + SUnits1 + ', ' + SUnits2 + ')'; { Numeric }

      fldPDXMONEY:
        Result := 'money'; { Money }
      fldPDXDATE:
        Result := 'date'; { Date }
      fldPDXSHORT:
        Result := 'smallint'; { Short }
      fldPDXMEMO:
        Result := 'memo blob'; { Text Memo (blob) }
      fldPDXBINARYBLOB:
        Result := 'binary blob'; { Binary data (blob) }
      fldPDXFMTMEMO:
        Result := 'formatted blob'; { Formatted text  (blob) }
      fldPDXOLEBLOB:
        Result := 'OLE blob'; { OLE object (blob) }

      fldPDXGRAPHIC:
        Result := 'graphic blob'; { Graphics object (blob) }
      fldPDXLONG:
        Result := 'long integer'; { Long }
      fldPDXTIME:
        Result := 'time'; { Time }
      fldPDXDATETIME:
        Result := 'date time'; { Time Stamp }
      fldPDXBOOL:
        Result := 'boolean'; { Logical }
      fldPDXAUTOINC:
        Result := 'auto increment'; { Auto increment (long) }
      fldPDXBYTES:
        Result := 'bytes'; { Fixed number of bytes }

      fldPDXBCD:
        Result := 'BCD'; { BCD (32 digits) }

      { xBASE types (Physical) }
      fldDBCHAR:
        Result := 'character'; { Char string }
      fldDBNUM:
        Result := 'number'; { Number }
      fldDBMEMO:
        Result := 'memo blob'; { Memo (blob) }
      fldDBBOOL:
        Result := 'logical'; { Logical }
      fldDBDATE:
        Result := 'date'; { Date }
      fldDBFLOAT:
        Result := 'float'; { Float }

      fldDBLOCK:
        Result := 'LOCKINFO'; { Logical type is LOCKINFO }
      fldDBOLEBLOB:
        Result := 'OLE blob'; { OLE object    (blob) }
      fldDBBINARY:
        Result := 'binary blob'; { Binary data   (blob) }
      fldDBBYTES:
        Result := 'bytes'; { Only for TEMPORARY tables }
      {$IFDEF COMPILER3_UP}
      fldDBLONG:
        Result := 'long integer'; { Long (Integer) }
      fldDBDATETIME:
        Result := 'date time'; { Time Stamp }
      fldDBDOUBLE:
        Result := 'double'; { Double }

      fldDBAUTOINC:
        Result := 'auto increment'; { Auto increment (long) }
      {$ENDIF COMPILER3_UP}

     { InterBase types (Physical) }
      1026:
        Result := 'integer';
      1028:
        Result := 'numeric(' + SUnits1 + ', ' + SUnits2 + ')'; { Numeric }
      1029:
        Result := 'char(' + SUnits1 + ')';
      1031:
        Result := 'date'; { Date }
    else
      Result := 'unknown type';
    end;
end;

{************************ Variant conversion routines ************************}

function Var2Type(V: Variant; const VarType: Integer): Variant;
begin
  if V = Null then
  begin
    case VarType of
      varString, varOleStr:
        Result := '';
      varInteger, varSmallint, varByte:
        Result := 0;
      varBoolean:
        Result := False;
      varSingle, varDouble, varCurrency, varDate:
        Result := 0.0;
    else
      Result := VarAsType(V, VarType);
    end;
  end
  else
    Result := VarAsType(V, VarType);
end;

procedure CopyRecord(DataSet: TDataSet);
var
  I: Integer;
begin
  with DataSet, TStringList.Create do
  try
    for I := 0 to FieldCount - 1 do
      Add(Fields[I].AsString);
    DataSet.Append;
    for I := 0 to FieldCount - 1 do
      if Fields[I].IsNull then
        Fields[I].AsString := Strings[I];
  finally
    Free;
  end
end;

procedure AddReference(Tbl: TTable; RefName: string; RefField: Word;
  MasterTable: string; MasterField: Word; ModOp, DelOp: RINTQual);
var
  hDb: hDbiDb;
  TblDesc: CRTblDesc;
  RInt: pRINTDesc;
  Dir: string;
  OpType: CROpType;
begin
  SetLength(Dir, dbiMaxNameLen + 1);
  Check(DbiGetDirectory(Tbl.DBHandle, False, PChar(Dir)));
  SetLength(Dir, StrLen(PChar(Dir)));
  RInt := AllocMem(SizeOf(RINTDesc));
  try
    FillChar(TblDesc, SizeOf(CRTblDesc), #0);
    Tbl.DisableControls;
    Tbl.Close;
    Check(DbiOpenDatabase(nil, nil, dbiReadWrite, dbiOpenExcl, nil, 0, nil, nil, hDb));
    Check(DbiSetDirectory(hDb, PChar(Dir)));
    with RInt^ do
    begin
      StrPCopy(szRintName, RefName);
      StrPCopy(szTblName, MasterTable);
      eType := rintDEPENDENT;
      eModOp := ModOp;
      eDelOp := DelOp;
      iFldCount := 1;
      aiThisTabFld[0] := RefField;
      aiOthTabFld[0] := MasterField;
    end;
    TblDesc.iRintCount := 1;
    TblDesc.pRINTDesc := RInt;
    OpType := crADD;
    TblDesc.pecrRintOp := @OpType;
    StrPCopy(TblDesc.szTblName, Tbl.TableName);
    StrCopy(TblDesc.szTblType, szParadox);
    Check(DbiDoRestructure(hDb, 1, @TblDesc, nil, nil, nil, False));
  finally
    Check(DbiCloseDatabase(hDb));
    FreeMem(RInt, SizeOf(RINTDesc));
    Tbl.EnableControls;
    Tbl.Open;
  end;
end;

// Pack a Paradox or dBASE table
// The table must be opened execlusively before calling this function...

procedure PackTable(Table: TTable);
var
  Props: CURProps;
  hDb: hDBIDb;
  TableDesc: CRTblDesc;
begin
  // Make sure the table is open exclusively so we can get the db handle...
  if not Table.Active then
    raise EDatabaseError.Create('Table must be opened to pack');
  if not Table.Exclusive then
    raise EDatabaseError.Create('Table must be opened exclusively to pack');

  // Get the table properties to determine table type...
  Check(DbiGetCursorProps(Table.Handle, Props));

  // If the table is a Paradox table, you must call DbiDoRestructure...
  if Props.szTableType = szPARADOX then
  begin
    // Blank out the structure...
    FillChar(TableDesc, SizeOf(TableDesc), 0);
    // Get the database handle from the table's cursor handle...

    Check(DbiGetObjFromObj(hDBIObj(Table.Handle), objDATABASE, hDBIObj(hDb)));
    // Put the table name in the table descriptor...
    StrPCopy(TableDesc.szTblName, Table.TableName);
    // Put the table type in the table descriptor...
    StrPCopy(TableDesc.szTblType, Props.szTableType);
    // Set the Pack option in the table descriptor to True...
    TableDesc.bPack := True;
    // Close the table so the restructure can complete...
    Table.Close;
    // Call DbiDoRestructure...
    Check(DbiDoRestructure(hDb, 1, @TableDesc, nil, nil, nil, False));
  end
  else
  // If the table is a dBASE table, simply call DbiPackTable...
  if (Props.szTableType = szDBASE) then
    Check(DbiPackTable(Table.DBHandle, Table.Handle, nil, szDBASE, True))
  else
    // Pack only works on Paradox or dBASE; nothing else...
    raise EDatabaseError.Create('Table must be either of Paradox or dBASE type to pack');
  Table.Open;
end;

//Add a master password to a Paradox table.
//This procedure uses the following input:
//AddMasterPassword(Table1, 'MyNewPassword')

procedure AddMasterPassword(Table: TTable; pswd: string);
const
  RESTRUCTURE_TRUE = WordBool(1);
var
  TblDesc: CRTblDesc;
  hDb: hDBIDb;
begin
  { Make sure that the table is opened and is exclusive }
  if not Table.Active or not Table.Exclusive then
    raise EDatabaseError.Create('Table must be opened in exclusive mode to add passwords');
  { Initialize the table descriptor }
  FillChar(TblDesc, SizeOf(CRTblDesc), #0);
  with TblDesc do
  begin
    { Place the table name in descriptor }
    StrPCopy(szTblName, Table.TableName);
    { Place the table type in descriptor }
    StrCopy(szTblType, szPARADOX);
    { Master Password, Password }
    StrPCopy(szPassword, pswd);
    { Set bProtected to True }
    bProtected := RESTRUCTURE_TRUE;
  end;
  { Get the database handle from the cursor handle }
  Check(DbiGetObjFromObj(hDBIObj(Table.Handle), objDATABASE, hDBIObj(hDb)));
  { Close the table }
  Table.Close;

  { Add the master password to the Paradox table }
  Check(DbiDoRestructure(hDb, 1, @TblDesc, nil, nil, nil, False));
  { Add the new password to the session }
  Session.AddPassword(pswd);
  { Re-Open the table }
  Table.Open;
end;

// Pack a Paradox table with Password
// The table must be opened execlusively before calling this function...

procedure PackEncryptedTable(Table: TTable; pswd: string);
const
  RESTRUCTURE_TRUE = WordBool(1);
var
  Props: CURProps;
  hDb: hDBIDb;
  TableDesc: CRTblDesc;
begin
  // Make sure the table is open exclusively so we can get the db handle...
  if not Table.Active then
    raise EDatabaseError.Create('Table must be opened to pack');
  if not Table.Exclusive then
    raise EDatabaseError.Create('Table must be opened exclusively to pack');

  // Get the table properties to determine table type...
  Check(DbiGetCursorProps(Table.Handle, Props));

  // If the table is a Paradox table, you must call DbiDoRestructure...
  if Props.szTableType = szPARADOX then
  begin
    // Blank out the structure...
    FillChar(TableDesc, SizeOf(TableDesc), 0);
    // Get the database handle from the table's cursor handle...
    Check(DbiGetObjFromObj(hDBIObj(Table.Handle), objDATABASE, hDBIObj(hDb)));
    // Put the table name in the table descriptor...
    StrPCopy(TableDesc.szTblName, Table.TableName);
    // Put the table type in the table descriptor...
    StrPCopy(TableDesc.szTblType, Props.szTableType);
    // Set the Pack option in the table descriptor to True...
    TableDesc.bPack := True;
    { Master Password, Password }
    StrPCopy(TableDesc.szPassword, pswd);
    { Set bProtected to True }
    TableDesc.bProtected := RESTRUCTURE_TRUE;
    // Close the table so the restructure can complete...
    Table.Close;
    // Call DbiDoRestructure...
    Check(DbiDoRestructure(hDb, 1, @TableDesc, nil, nil, nil, False));
  end
  else
  // If the table is a dBASE table, simply call DbiPackTable...
  if Props.szTableType = szDBASE then
    Check(DbiPackTable(Table.DBHandle, Table.Handle, nil, szDBASE, True))
  else
    // Pack only works on Paradox or dBASE; nothing else...
    raise EDatabaseError.Create('Table must be either of Paradox or dBASE type to pack');
  Table.Open;
end;

function EncodeQuotes(const S: string): string;
begin
  Result := S;
  Result := ReplaceString(Result, CrLf, Cr);
  Result := ReplaceString(Result, Cr, '\#13');
  Result := ReplaceString(Result, '"', '\#34');
  Result := ReplaceString(Result, ',', '\#44');
end;

{*********************** from JvStrUtil unit ***********************}
{ SubStr returns substring from string, S,
  separated with Separator string}

function SubStr(const S: string; const Index: Integer; const Separator: string): string;
// {Вырезает подстроку. Подстроки разделяются символом Sep}
var
  I: Integer;
  pB, pE: PChar;
begin
  Result := '';
  if ((Index < 0) or ((Index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then
    Exit;
  pB := PChar(S);
  for I := 1 to Index do
  begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then
      Exit;
    pB := pB + Length(Separator);
    if pB[0] = #0 then
      Exit;
  end;
  pE := StrPos(pB + 1, PChar(Separator));
  if pE = nil then
    pE := PChar(S) + Length(S);
  if AnsiStrLIComp(pB, PChar(Separator), Length(Separator)) <> 0 then
    SetString(Result, pB, pE - pB);
end;

function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;
var
  MaxIndex: Integer;
  pB: PChar;
begin
// Not optimal implementation [translated]
  MaxIndex := 0;
  pB := StrPos(PChar(S), PChar(Separator));
  while pB <> nil do
  begin
    Inc(MaxIndex);
    pB := StrPos(pB + Length(Separator), PChar(Separator));
  end;
  Result := SubStr(S, MaxIndex - Index, Separator);
end;

function Cmp(const S1, S2: string): Boolean;
begin
  Result := AnsiStrIComp(PChar(S1), PChar(S2)) = 0;
end;

{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }

function ReplaceString(S: string; const OldPattern, NewPattern: string): string;
var
  LW: Integer;
  P: PChar;
  Sm: Integer;
begin
  LW := Length(OldPattern);
  P := StrPos(PChar(S), PChar(OldPattern));
  while P <> nil do
  begin
    Sm := P - PChar(S);
    S := Copy(S, 1, Sm) + NewPattern + Copy(S, Sm + LW + 1, Length(S));
    P := StrPos(PChar(S) + Sm + Length(NewPattern), PChar(OldPattern));
  end;
  Result := S;
end;

{ GetXYByPos is same to previous function, but
  returns X position in line too}

procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer);
var
  I, iB: Integer;
begin
  X := -1;
  Y := -1;
  iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then
  begin
    I := 1;
    Y := 0;
    while I <= Pos do
    begin
      if S[I] = #13 then
      begin
        Inc(Y);
        iB := I + 1
      end;
      Inc(I);
    end;
    X := Pos - iB;
  end;
end;
{####################### from JvStrUtil unit #######################}

end.

