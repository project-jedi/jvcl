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
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvDBUtil;

interface

uses
  Windows, Messages, Bde, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, DBTables, DBCtrls
  {$IFDEF COMPILER6_UP}, Variants {$ENDIF}
  ;


type

  EJvScriptError  = class(Exception)
    ErrPos : integer;
    constructor Create2(AMessage : string; AErrPos : integer);
  end;

  TCommit = (ctNone, ctStep, ctAll);
  TOnProgress = procedure(UserData : integer; var Cancel : boolean; Line : integer) of object;


  { ExecuteSQLScript executes SQL script }

  procedure ExecuteSQLScript(Base : TDataBase; const Script : string; const Commit : TCommit; OnProgress : TOnProgress; const UserData : integer);

  { GetQueryResult executes SQL Query and returns result as variant }

  function GetQueryResult(const DatabaseName, SQL : string) : variant;

  { GetStoredProcResult executes SQL stored procedure and returns
    value of ResultName parameters as variant }

  function GetStoredProcResult(const DatabaseName, StoredProcName : string; Params : array of variant; const ResultName : string) : variant;

  { StrFieldDesc returns field description of given FLDDesc record }

  function StrFieldDesc(Field: FLDDesc) : string;


  function Var2Type(V : Variant; const VarType : integer) : variant;


  procedure CopyRecord(DataSet : TDataSet);

 { AddReference create reference for paradox table,
   RefField and MasterField are field numbers (first field has number 1)
   Tables allready must have indices for this fields }

  procedure AddReference(Tbl : TTable; RefName : string; RefField : word;
    MasterTable : string; MasterField : word; ModOp, DelOp : RINTQual);

 { AddMasterPassword extracted from "bde.hlp" file }
  procedure AddMasterPassword(Table: TTable; pswd: string);

 { PackTable extracted from "bde.hlp" file }
  procedure PackTable(Table: TTable);

  procedure PackEncryptedTable(Table: TTable; pswd: string);

  function EncodeQuates(const S: string): string;


{*********************** from JvStrUtil unit ***********************}
  function Cmp(const S1, S2 : string) : boolean;

  { SubStr returns substring from string, S,
    separated with Separator string}
  function SubStr(const S : string; const index : integer; const Separator : string) : string;

  { SubStrEnd same to previous function but index numerated
    from the end of string }

  function SubStrEnd(const S : string; const index : integer; const Separator : string) : string;

  { ReplaceString searches for all substrings, OldPattern,
    in a string, S, and replaces them with NewPattern }
  function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;

  { GetXYByPos is same to previous function, but
    returns X position in line too}
  procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);
{####################### from JvStrUtil unit #######################}

  
implementation

{$IFNDEF COMPILER3_UP}
function AnsiStrIComp(S1, S2: PChar): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - 2;
end;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    S1, MaxLen, S2, MaxLen) - 2;
end;
{$ENDIF COMPILER3_UP}


constructor EJvScriptError .Create2(AMessage : string; AErrPos : integer);
begin
  inherited Create(AMessage);
  ErrPos := AErrPos;
end;

procedure ExecuteSQLScript(Base : TDataBase; const Script : string; const Commit : TCommit; OnProgress : TOnProgress; const UserData : integer);
var
  N : integer;
  Term : char;

  function NextQuery : string;
  var
    C : char;
    Rem : boolean;
  begin
    Result := '';
    Rem := false;
    while Length(Script) >= N do begin
      C := Script[N];
      inc(N);
      if (C = Term) and not Rem then exit;
      Result := Result + C;
      if (C = '/') and (Length(Script) >= N) and (Script[N] = '*') then
        Rem := true;
      if (C = '*') and (Length(Script) >= N) and (Script[N] = '/') and Rem then
        Rem := false;
    end;
    Result := '';
  end;
  function SetTerm(S : string) : boolean;
  var
    Rem : boolean;
  begin
    Rem := false;
    while (Length(S) > 0) do begin
      if (S[1] in [' ', #13, #10]) then Delete(S, 1, 1)
      else
      if Rem then
        if (S[1] = '*') and (Length(S) > 1) and (S[2] = '/') then begin
          Delete(S, 1, 2);
          Rem := false;
        end else
          Delete(S, 1, 1)
      else
      if (S[1] = '/') and (Length(S) > 1) and (S[2] = '*') then begin
        Delete(S, 1, 2);
        Rem := true;
      end
      else break;
    end;
    Result := ANSIStrLIComp(PChar(S), 'set term', 8) = 0;
    if Result then begin
      S := Trim(Copy(S, 9, 1024));
      if Length(S) = 1 then
        Term := S[1] else
        EDatabaseError.Create('Bad term');
      exit;
    end;
    Result := ANSIStrLIComp(PChar(S), 'commit work', 11) = 0;
    if Result then begin
      Base.Commit;
      Base.StartTransaction;
      exit;
    end;
  end;

var
  Q : string;
  ErrPos : integer;
  NBeg : integer;
  X, Y, N2 : integer;
  S1 : string;
  Query : TQuery;
  Stop : boolean;
begin
  if Commit in [ctStep, ctAll] then
    Base.StartTransaction;
  Query := TQuery.Create(Application);
  try
    Query.DatabaseName := Base.DatabaseName;
    Query.ParamCheck := false;
    N := 1; Term := ';'; Stop := false;
    NBeg := 1;
    try
      Q := NextQuery;
      while Q <> '' do
      begin
        if not SetTerm(Q) then
        begin
          if Assigned(OnProgress) then
          begin
            S1 := Q; N2 := 0;
            while (Length(S1) > 0) and (S1[1] in [' ', #13, #10]) do
            begin
              Delete(S1, 1, 1);
              inc(N2);
            end;
            GetXYByPos(Script, NBeg+N2, X, Y);
            if Assigned(OnProgress) then
              OnProgress(UserData, Stop, Y)
            else
              Application.ProcessMessages;
            if Stop then Abort;
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
        NBeg := N+1;
        Q := NextQuery;
      end;
      if Commit in [ctStep, ctAll] then
        Base.Commit;
    except
      on E : Exception do
      begin
        if Commit in [ctStep, ctAll] then
          Base.Rollback;
        if E is EDatabaseError then
        begin
          ErrPos := NBeg;
          //..
          raise EJvScriptError .Create2(E.Message, ErrPos)
        end else
          raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

function GetQueryResult(const DatabaseName, SQL : string) : variant;
var
  Query : TQuery;
begin
  Query := TQuery.Create(Application);
  try
    Query.DatabaseName := DatabaseName;
    Query.ParamCheck := false;
    Query.SQL.Text := SQL;
    Query.Open;
    Result := Query.Fields[0].AsVariant;
  finally
    Query.Free;
  end;
end;

function GetStoredProcResult(const DatabaseName, StoredProcName : string; Params : array of variant; const ResultName : string) : variant;
var
  StoredProc : TStoredProc;
  i : integer;
begin
  StoredProc := TStoredProc.Create(Application);
  try
    StoredProc.DatabaseName := DatabaseName;
    StoredProc.ParamBindMode := pbByNumber;
    StoredProc.StoredProcName := StoredProcName;
    StoredProc.Prepare;
    for i := Low(Params) to High(Params) do
      StoredProc.Params[i].Value := Params[i];
    StoredProc.ExecProc;
    Result := StoredProc.ParamByName(ResultName).Value;
  finally
    StoredProc.Free;
  end;
end;

function StrFieldDesc(Field: FLDDesc) : string;

  function sUnits1 : string;
  begin
    Result := IntToStr(Field.iUnits1);
  end;
  function sUnits2 : string;
  begin
    if Field.iUnits2 < 0 then
      Result := IntToStr(-Field.iUnits2) else
      Result := IntToStr(Field.iUnits2);
  end;

begin
  with Field do
    case iFldType  of
      fldUNKNOWN: result := 'unknown';
      fldZSTRING: result := 'string';               { Null terminated string }
      fldDATE: result := 'date';                    { Date     (32 bit) }
      fldBLOB: result := 'BLOb';                    { Blob }
      fldBOOL: result := 'boolean';                 { Boolean  (16 bit) }
      fldINT16: result := 'integer';                { 16 bit signed number }
      fldINT32: result := 'long integer';           { 32 bit signed number }

      fldFLOAT: result := 'float';                  { 64 bit floating point }
      fldBCD: result := 'BCD';                      { BCD }
      fldBYTES: result := 'bytes';                  { Fixed number of bytes }
      fldTIME: result := 'time';                    { Time        (32 bit) }
      fldTIMESTAMP: result := 'timestamp';          { Time-stamp  (64 bit) }
      fldUINT16: result := 'unsigned int';          { Unsigned 16 bit integer }
      fldUINT32: result := 'unsigned long int';     { Unsigned 32 bit integer }

      fldFLOATIEEE: result := 'float IEEE';         { 80-bit IEEE float }
      fldVARBYTES: result := 'varbytes';            { Length prefixed var bytes }
      fldLOCKINFO: result := 'lockinfo';            { Look for LOCKINFO typedef }
     {$IFDEF COMPILER3_UP}
      fldCURSOR: result := 'Oracle cursor';         { For Oracle Cursor type }
     {$ENDIF COMPILER3_UP}

     { Paradox types (Physical) }
      fldPDXCHAR: result := 'alpha('+sUnits1+')';       { Alpha    (string) }
      fldPDXNUM: result := 'numeric('+sUnits1+', '+sUnits2+')';               { Numeric }

      fldPDXMONEY: result := 'money';               { Money }
      fldPDXDATE: result := 'date';                 { Date }
      fldPDXSHORT: result := 'smallint';            { Short }
      fldPDXMEMO: result := 'Memo BLOb';            { Text Memo       (blob) }
      fldPDXBINARYBLOB: result := 'Binary BLOb';    { Binary data     (blob) }
      fldPDXFMTMEMO: result := 'formatted BLOb';    { Formatted text  (blob) }
      fldPDXOLEBLOB: result := 'OLE BLOb';          { OLE object      (blob) }

      fldPDXGRAPHIC: result := 'Graphic BLOb';      { Graphics object (blob) }
      fldPDXLONG: result := 'long integer';         { Long }
      fldPDXTIME: result := 'time';                 { Time }
      fldPDXDATETIME: result := 'date time';        { Time Stamp }
      fldPDXBOOL: result := 'boolean';              { Logical }
      fldPDXAUTOINC: result := 'auto increment';    { Auto increment (long) }
      fldPDXBYTES: result := 'bytes';               { Fixed number of bytes }

      fldPDXBCD: result := 'BCD';                   { BCD (32 digits) }

      { xBASE types (Physical) }
      fldDBCHAR: result := 'character';             { Char string }
      fldDBNUM: result := 'number';                 { Number }
      fldDBMEMO: result := 'Memo BLOb';             { Memo          (blob) }
      fldDBBOOL: result := 'logical';               { Logical }
      fldDBDATE: result := 'date';                  { Date }
      fldDBFLOAT: result := 'float';                { Float }

      fldDBLOCK: result := 'LOCKINFO';              { Logical type is LOCKINFO }
      fldDBOLEBLOB: result := 'OLE BLOb';           { OLE object    (blob) }
      fldDBBINARY: result := 'Binary BLOb';         { Binary data   (blob) }
      fldDBBYTES: result := 'bytes';                { Only for TEMPORARY tables }
     {$IFDEF COMPILER3_UP}
      fldDBLONG: result := 'long integer';          { Long (Integer) }
      fldDBDATETIME: result := 'date time';         { Time Stamp }
      fldDBDOUBLE: result := 'double';              { Double }

      fldDBAUTOINC: result := 'aut increment';      { Auto increment (long) }
     {$ENDIF COMPILER3_UP}

     { InterBase types (Physical) }
      1026 : result := 'integer';
      1028 : result := 'numeric('+sUnits1+', '+sUnits2+')';  { Numeric }
      1029 : result := 'char('+sUnits1+')';
      1031 : result := 'date';                               { Date      }
    else
      Result := 'unknown type';
    end;
end;

{************************ variant conversion routines ************************}

function Var2Type(V : Variant; const VarType : integer) : variant;
begin
  if V = null then
  begin
    case VarType of
      varString,
      varOleStr    : Result := '';
      varInteger,
      varSmallint,
      varByte      : Result := 0;
      varBoolean   : Result := false;
      varSingle,
      varDouble,
      varCurrency,
      varDate      : Result := 0.0;
      else Result := VarAsType(V, VarType);
    end;
  end else
    Result := VarAsType(V, VarType);
end;

procedure CopyRecord(DataSet : TDataSet);
var
  i : integer;
begin
  with DataSet, TStringList.Create do
  try
    for i := 0 to FieldCount -1 do
      Add(Fields[i].AsString);
    DataSet.Append;
    for i := 0 to FieldCount -1 do
      if Fields[i].IsNull then
        Fields[i].AsString := Strings[i];
  finally
    Free;
  end
end;

procedure AddReference(Tbl : TTable; RefName : string; RefField : word;
  MasterTable : string; MasterField : word; ModOp, DelOp : RINTQual);
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
  RInt := AllocMem(sizeof(RINTDesc));
  try
    FillChar(TblDesc, sizeof(CRTblDesc), #0);
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
    FreeMem(RInt, sizeof(RINTDesc));
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
  if (Props.szTableType = szPARADOX) then begin
    // Blank out the structure...
    FillChar(TableDesc, sizeof(TableDesc), 0);
    // Get the database handle from the table's cursor handle...

    Check(DbiGetObjFromObj(hDBIObj(Table.Handle), objDATABASE, hDBIObj(hDb)));
    // Put the table name in the table descriptor...
    StrPCopy(TableDesc.szTblName, Table.TableName);
    // Put the table type in the table descriptor...
    StrPCopy(TableDesc.szTblType, Props.szTableType);
    // Set the Pack option in the table descriptor to TRUE...
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
      // Pack only works on PAradox or dBASE; nothing else...
      raise EDatabaseError.Create('Table must be either of Paradox or dBASE ' +
        'type to pack');
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
    raise EDatabaseError.Create('Table must be opened in exclusive ' +
      'mode to add passwords');
  { Initialize the table descriptor }
  FillChar(TblDesc, SizeOf(CRTblDesc), #0);
  with TblDesc do begin

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
  if (Props.szTableType = szPARADOX) then begin
    // Blank out the structure...
    FillChar(TableDesc, sizeof(TableDesc), 0);
    // Get the database handle from the table's cursor handle...

    Check(DbiGetObjFromObj(hDBIObj(Table.Handle), objDATABASE, hDBIObj(hDb)));
    // Put the table name in the table descriptor...
    StrPCopy(TableDesc.szTblName, Table.TableName);
    // Put the table type in the table descriptor...
    StrPCopy(TableDesc.szTblType, Props.szTableType);
    // Set the Pack option in the table descriptor to TRUE...
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
    if (Props.szTableType = szDBASE) then
      Check(DbiPackTable(Table.DBHandle, Table.Handle, nil, szDBASE, True))
    else
      // Pack only works on PAradox or dBASE; nothing else...
      raise EDatabaseError.Create('Table must be either of Paradox or dBASE ' +
        'type to pack');
  Table.Open;
end;

function EncodeQuates(const S: string): string;
begin
  Result := S;
  Result := ReplaceString(Result, #13#10, #13);
  Result := ReplaceString(Result, #13, '\#13');
  Result := ReplaceString(Result, '"', '\#34');
  Result := ReplaceString(Result, ',', '\#44');
end;


{*********************** from JvStrUtil unit ***********************}
{ SubStr returns substring from string, S,
  separated with Separator string}
function SubStr(const S : string; const index : integer; const Separator : string) : string;
 {Вырезает подстроку. Подстроки разделяются символом Sep}
var
  i : integer;
  pB, pE : PChar;
begin
  Result := '';
  if ((index < 0) or ((index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then exit;
  pB := PChar(S);
  for i := 1 to index do begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then exit;
    pB := pB+Length(Separator);
    if pB[0] = #0 then exit;
  end;
  pE := StrPos(pB+1, PChar(Separator));
  if pE = nil then pE := PChar(S)+Length(S);
  if not (ANSIStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, pB, pE-pB);
end;

function SubStrEnd(const S : string; const index : integer; const Separator : string) : string;
 {то же что и SubStr, но подстроки нумеруются с конца}
var
  MaxIndex : integer;
  pB : PChar;
begin
 {неоптимальная реализация}
  MaxIndex := 0;
  pB := StrPos(PChar(S), PChar(Separator));
  while pB <> nil do begin
    inc(MaxIndex);
    pB := StrPos(pB+Length(Separator), PChar(Separator));
  end;
  Result := SubStr(S, MaxIndex - index, Separator);
end;

function Cmp(const S1, S2 : string) : boolean;
begin
  Result := ANSIStrIComp(PChar(S1), PChar(S2)) = 0;
end;

{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }
function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;
var
  LW : integer;
  P : PChar;
  Sm : integer;
begin
  LW := Length(OldPattern);
  P := StrPos(PChar(S), PChar(OldPattern));
  while P <> nil do begin
    Sm := P-PChar(S);
    S := Copy(S, 1, Sm)+NewPattern+Copy(S, Sm+LW+1, Length(S));
    P := StrPos(PChar(S)+Sm+Length(NewPattern), PChar(OldPattern));
  end;
  Result := S;
end;

{ GetXYByPos is same to previous function, but
  returns X position in line too}
procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);
var
  i, iB : integer;
begin
  X := -1; Y := -1; iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then begin
    i := 1;
    Y := 0;
    while (i <= Pos) do begin
      if S[i] = #13 then begin inc(Y); iB := i+1 end;
      inc(i);
    end;
    X := Pos - iB;
  end;
end;
{####################### from JvStrUtil unit #######################}

end.
