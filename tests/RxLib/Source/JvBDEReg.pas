{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBDEReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
{$I JVCL.INC}

unit JvBDEReg;


interface

uses Classes,
  {$IFDEF Delphi6_Up}
RTLConsts,  DesignIntf, DesignEditors,  VCLEditors,
{$ELSE}
  LibIntf, DsgnIntf,
{$ENDIF}
SysUtils, DB, DBTables;

{ Register data aware custom controls }

procedure Register;

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.D16}
{$ENDIF}

uses TypInfo, JvDBLists, JvLConst, JvDBQBE, JvDBFilter, JvDBIndex, JvDBPrgrss, 
  JvxLogin, JvDBSecur, JvQuery, JvVCLUtils, JvDbExcpt, JvDsgn,
  {$IFDEF DCS} JvSelDSFrm, {$ENDIF} {$IFDEF RX_MIDAS} JvRemLog, {$ENDIF}
  {$IFDEF Delphi3_Up} JvQBndDlg, {$ELSE} 
  {$IFNDEF WIN32} JvQBndDlg, {$ELSE} JvQBindDlg, {$ENDIF} {$ENDIF}
  Consts, LibHelp, JvMemTable;

{$IFDEF WIN32}

{ TJvSessionNameProperty }

type
  TJvSessionNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvSessionNameProperty.GetValueList(List: TStrings);
begin
  Sessions.GetSessionNames(List);
end;

{$ENDIF WIN32}

{ TJvDatabaseNameProperty }

type
  TJvDatabaseNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvDatabaseNameProperty.GetValueList(List: TStrings);
{$IFDEF WIN32}
var
  S: TSession;
{$ENDIF}
begin
{$IFDEF WIN32}
  if (GetComponent(0) is TDBDataSet) then
    (GetComponent(0) as TDBDataSet).DBSession.GetDatabaseNames(List)
  else if (GetComponent(0) is TJvSQLScript) then begin
    S := Sessions.FindSession((GetComponent(0) as TJvSQLScript).SessionName);
    if S = nil then S := Session;
    S.GetDatabaseNames(List);
  end;
{$ELSE}
  Session.GetDatabaseNames(List);
{$ENDIF}
end;

{ TJvTableNameProperty }
{ For TJvFieldList, TJvIndexList components }

type
  TJvTableNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvTableNameProperty.GetValueList(List: TStrings);
begin
{$IFDEF WIN32}
  (GetComponent(0) as TJvCustomTableItems).DBSession.GetTableNames((GetComponent(0)
    as TJvCustomTableItems).DatabaseName, '', True, False, List);
{$ELSE}
  Session.GetTableNames((GetComponent(0) as TJvCustomTableItems).DatabaseName,
    '', True, False, List);
{$ENDIF WIN32}
end;

{$IFNDEF Delphi4_Up}

{$IFNDEF VER90}
 {$IFNDEF VER93}
function EditQueryParams(DataSet: TDataSet; List: TParams): Boolean;
begin
  Result := JvQBndDlg.EditQueryParams(DataSet, List, hcDQuery);
end;
 {$ENDIF}
{$ENDIF}

{ TJvParamsProperty }

type
  TJvParamsProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

function TJvParamsProperty.GetValue: string;
var
  Params: TParams;
begin
  Params := TParams(Pointer(GetOrdValue));
  if Params.Count > 0 then
{$IFDEF WIN32}
    Result := Format('(%s)', [GetPropInfo.Name])
{$ELSE}
    Result := Format('(%s)', [GetPropInfo^.Name])
{$ENDIF}
  else
    Result := ResStr(srNone);
end;

function TJvParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog];
end;

procedure TJvParamsProperty.Edit;
var
  List, Params: TParams;
  Query: TDataSet;
  QueryCreated: Boolean;
  I: Integer;
begin
  QueryCreated := False;
  if GetComponent(0) is TDataSet then
    Query := GetComponent(0) as TDataSet
  else begin
    Query := TQuery.Create(GetComponent(0) as TComponent);
    QueryCreated := True;
  end;
  try
    Params := TParams(GetOrdProp(GetComponent(0), GetPropInfo));
    if QueryCreated then TQuery(Query).Params := Params;
    List := TParams.Create;
    try
      List.Assign(Params);
      if EditQueryParams(Query, List) {$IFDEF WIN32} and not
        List.IsEqual(Params) {$ENDIF} then
      begin
{$IFDEF WIN32}
        Modified;
{$ELSE}
        if Designer <> nil then Designer.Modified;
{$ENDIF}
        Query.Close;
        for I := 0 to PropCount - 1 do begin
          Params := TParams(GetOrdProp(GetComponent(I),
            TypInfo.GetPropInfo(GetComponent(I).ClassInfo,
{$IFDEF WIN32}
            GetPropInfo.Name)));
{$ELSE}
            GetPropInfo^.Name)));
{$ENDIF}
          Params.AssignValues(List);
        end;
      end;
    finally
      List.Free;
    end;
  finally
    if QueryCreated then Query.Free;
  end;
end;

{$ENDIF Delphi4_Up}

{ TJvUserTableNameProperty }
{ For TJvDBSecurity component }

type
  TJvUserTableNameProperty = class(TJvDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvUserTableNameProperty.GetValueList(List: TStrings);
var
  Security: TJvDBSecurity;
begin
  Security := GetComponent(0) as TJvDBSecurity;
  if Security.Database <> nil then begin
{$IFDEF WIN32}
    Security.Database.Session.GetTableNames(Security.Database.DatabaseName,
      '*.*', True, False, List);
{$ELSE}
    Session.GetTableNames(Security.Database.DatabaseName, '*.*',
      True, False, List);
{$ENDIF}
  end;
end;

{ TLoginNameFieldProperty }
{ For TJvDBSecurity component }

type
  TLoginNameFieldProperty = class(TJvDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;

procedure TLoginNameFieldProperty.GetValueList(List: TStrings);
var
  Security: TJvDBSecurity;
  Table: TTable;
begin
  Security := GetComponent(0) as TJvDBSecurity;
  if (Security.Database <> nil) and (Security.UsersTableName <> '') then begin
    Table := TTable.Create(Security);
    try
      Table.DatabaseName := Security.Database.DatabaseName;
      Table.TableName := Security.UsersTableName;
      Table.GetFieldNames(List);
    finally
      Table.Free;
    end;
  end;
end;

{$IFDEF DCS}

{ TJvMemoryTableEditor }

type
  TJvMemoryTableEditor = class(TJvMemDataSetEditor)
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; override;
  end;

function TJvMemoryTableEditor.CopyStructure(Source, Dest: TDataSet): Boolean;
begin
  Result := Dest is TJvMemoryTable;
  if Result then
    TJvMemoryTable(Dest).CopyStructure(Source);
end;

{$ENDIF DCS}

{ Designer registration }

procedure Register;
begin
{$IFDEF Delphi4_Up}
  { Database Components are excluded from the STD SKU }
  if GDAL = LongWord(-16) then Exit;
{$ENDIF}

{ Data aware components and controls }
  RegisterComponents({LoadStr(srRXDBAware)}'JvX DBAware', [TJvQuery, TJvSQLScript,
    TJvMemoryTable, TJvQBEQuery, TJvDBFilter, TJvDBIndexCombo, TJvDBProgress, 
    TJvDBSecurity]);
{$IFDEF RX_MIDAS}
{ MIDAS components }
  RegisterComponents({LoadStr(srRXDBAware)}'JvX DBAware', [TJvRemoteLogin]);
  RegisterNonActiveX([TJvRemoteLogin], axrComponentOnly);
{$ENDIF}
{ Database lists }
  RegisterComponents({LoadStr(srRXDBAware)}'JvX DBAware', [TJvBDEItems, TJvDatabaseItems,
    TJvTableItems]);
{$IFNDEF CBUILDER}
 {$IFDEF USE_OLD_DBLISTS}
  RegisterComponents({LoadStr(srRXDBAware)}'JvX DBAware', [TJvDatabaseList, TJvLangDrivList,
    TJvTableList, TJvStoredProcList, TJvFieldList, TJvIndexList]);
 {$ENDIF USE_OLD_DBLISTS}
{$ENDIF CBUILDER}

{$IFDEF Delphi3_Up}
  RegisterNonActiveX([TJvQuery, TJvSQLScript, TJvMemoryTable, TJvQBEQuery,
    TJvDBFilter, TJvDBIndexCombo, TJvDBProgress, TJvDBSecurity, TJvBDEItems,
    TJvDatabaseItems, TJvTableItems], axrComponentOnly);
{$ENDIF Delphi3_Up}

{ Property and component editors for data aware controls }

  RegisterPropertyEditor(TypeInfo(TFileName), TJvCustomTableItems, 'TableName',
    TJvTableNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TJvDBSecurity,
    'UsersTableName', TJvUserTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBSecurity,
    'LoginNameField', TLoginNameFieldProperty);

{$IFDEF DCS}
  RegisterComponentEditor(TJvMemoryTable, TJvMemoryTableEditor);
{$ENDIF}

{$IFNDEF Delphi4_Up}
  RegisterPropertyEditor(TypeInfo(TParams), TJvQBEQuery, 'Params',
    TJvParamsProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TJvQuery, 'Macros',
    TJvParamsProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TJvSQLScript, 'Params',
    TJvParamsProperty);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TJvSQLScript, 'DatabaseName',
    TJvDatabaseNameProperty);
{$IFDEF WIN32}
  RegisterPropertyEditor(TypeInfo(string), TJvCustomBDEItems, 'SessionName',
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvSQLScript, 'SessionName',
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBProgress, 'SessionName',
    TJvSessionNameProperty);
{$ELSE}
  DbErrorIntercept;
{$ENDIF WIN32}

end;

end.
