{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxBDEReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvxBDEReg;


interface

uses Classes, RTLConsts, DesignIntf, DesignEditors, VCLEditors, SysUtils, DB, DBTables;

{ Register data aware custom controls }

procedure Register;

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.D16}
{$ENDIF}

uses TypInfo, JvxDBLists, JvxLConst, JvxDBQBE, JvxDBFilter, JvxDBIndex, JvxDBPrgrss, 
  JvxLogin, JvxDBSecur, JvxQuery, JvxVCLUtils, JvxDbExcpt, JvxDsgn,
  {$IFDEF DCS} JvxSelDSFrm, {$ENDIF} {$IFDEF RX_MIDAS} JvxRemLog, {$ENDIF}
  {$IFDEF Delphi3_Up} JvxQBndDlg, {$ELSE} 
  {$IFNDEF WIN32} JvxQBndDlg, {$ELSE} JvxQBindDlg, {$ENDIF} {$ENDIF}
  Consts, LibHelp, JvxMemTable;

{$IFDEF WIN32}

{ TJvxSessionNameProperty }

type
  TJvxSessionNameProperty = class(TJvxDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvxSessionNameProperty.GetValueList(List: TStrings);
begin
  Sessions.GetSessionNames(List);
end;

{$ENDIF WIN32}

{ TJvxDatabaseNameProperty }

type
  TJvxDatabaseNameProperty = class(TJvxDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvxDatabaseNameProperty.GetValueList(List: TStrings);
{$IFDEF WIN32}
var
  S: TSession;
{$ENDIF}
begin
{$IFDEF WIN32}
  if (GetComponent(0) is TDBDataSet) then
    (GetComponent(0) as TDBDataSet).DBSession.GetDatabaseNames(List)
  else if (GetComponent(0) is TJvxSQLScript) then begin
    S := Sessions.FindSession((GetComponent(0) as TJvxSQLScript).SessionName);
    if S = nil then S := Session;
    S.GetDatabaseNames(List);
  end;
{$ELSE}
  Session.GetDatabaseNames(List);
{$ENDIF}
end;

{ TJvxTableNameProperty }
{ For TJvxFieldList, TJvxIndexList components }

type
  TJvxTableNameProperty = class(TJvxDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvxTableNameProperty.GetValueList(List: TStrings);
begin
{$IFDEF WIN32}
  (GetComponent(0) as TJvxCustomTableItems).DBSession.GetTableNames((GetComponent(0)
    as TJvxCustomTableItems).DatabaseName, '', True, False, List);
{$ELSE}
  Session.GetTableNames((GetComponent(0) as TJvxCustomTableItems).DatabaseName,
    '', True, False, List);
{$ENDIF WIN32}
end;

{$IFNDEF Delphi4_Up}

{$IFNDEF VER90}
 {$IFNDEF VER93}
function EditQueryParams(DataSet: TDataSet; List: TParams): Boolean;
begin
  Result := JvxQBndDlg.EditQueryParams(DataSet, List, hcDQuery);
end;
 {$ENDIF}
{$ENDIF}

{ TJvxParamsProperty }

type
  TJvxParamsProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

function TJvxParamsProperty.GetValue: string;
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

function TJvxParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog];
end;

procedure TJvxParamsProperty.Edit;
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

{ TJvxUserTableNameProperty }
{ For TJvxDBSecurity component }

type
  TJvxUserTableNameProperty = class(TJvxDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvxUserTableNameProperty.GetValueList(List: TStrings);
var
  Security: TJvxDBSecurity;
begin
  Security := GetComponent(0) as TJvxDBSecurity;
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
{ For TJvxDBSecurity component }

type
  TLoginNameFieldProperty = class(TJvxDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;

procedure TLoginNameFieldProperty.GetValueList(List: TStrings);
var
  Security: TJvxDBSecurity;
  Table: TTable;
begin
  Security := GetComponent(0) as TJvxDBSecurity;
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

{ TJvxMemoryTableEditor }

type
  TJvxMemoryTableEditor = class(TJvxMemDataSetEditor)
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; override;
  end;

function TJvxMemoryTableEditor.CopyStructure(Source, Dest: TDataSet): Boolean;
begin
  Result := Dest is TJvxMemoryTable;
  if Result then
    TJvxMemoryTable(Dest).CopyStructure(Source);
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
  RegisterComponents(LoadStr(srRXDBAware), [TJvxQuery, TJvxSQLScript,
    TJvxMemoryTable, TJvxQBEQuery, TJvxDBFilter, TJvxDBIndexCombo, TJvxDBProgress, 
    TJvxDBSecurity]);
{$IFDEF RX_MIDAS}
{ MIDAS components }
  RegisterComponents(LoadStr(srRXDBAware), [TJvxRemoteLogin]);
  RegisterNonActiveX([TJvxRemoteLogin], axrComponentOnly);
{$ENDIF}
{ Database lists }
  RegisterComponents(LoadStr(srRXDBAware), [TJvxBDEItems, TJvxDatabaseItems,
    TJvxTableItems]);
{$IFNDEF CBUILDER}
 {$IFDEF USE_OLD_DBLISTS}
  RegisterComponents(LoadStr(srRXDBAware), [TJvxDatabaseList, TJvxLangDrivList,
    TJvxTableList, TJvxStoredProcList, TJvxFieldList, TJvxIndexList]);
 {$ENDIF USE_OLD_DBLISTS}
{$ENDIF CBUILDER}

{$IFDEF Delphi3_Up}
  RegisterNonActiveX([TJvxQuery, TJvxSQLScript, TJvxMemoryTable, TJvxQBEQuery,
    TJvxDBFilter, TJvxDBIndexCombo, TJvxDBProgress, TJvxDBSecurity, TJvxBDEItems,
    TJvxDatabaseItems, TJvxTableItems], axrComponentOnly);
{$ENDIF Delphi3_Up}

{ Property and component editors for data aware controls }

  RegisterPropertyEditor(TypeInfo(TFileName), TJvxCustomTableItems, 'TableName',
    TJvxTableNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TJvxDBSecurity,
    'UsersTableName', TJvxUserTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxDBSecurity,
    'LoginNameField', TLoginNameFieldProperty);

{$IFDEF DCS}
  RegisterComponentEditor(TJvxMemoryTable, TJvxMemoryTableEditor);
{$ENDIF}

{$IFNDEF Delphi4_Up}
  RegisterPropertyEditor(TypeInfo(TParams), TJvxQBEQuery, 'Params',
    TJvxParamsProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TJvxQuery, 'Macros',
    TJvxParamsProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TJvxSQLScript, 'Params',
    TJvxParamsProperty);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TJvxSQLScript, 'DatabaseName',
    TJvxDatabaseNameProperty);
{$IFDEF WIN32}
  RegisterPropertyEditor(TypeInfo(string), TJvxCustomBDEItems, 'SessionName',
    TJvxSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxSQLScript, 'SessionName',
    TJvxSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxDBProgress, 'SessionName',
    TJvxSessionNameProperty);
{$ELSE}
  DbErrorIntercept;
{$ENDIF WIN32}

end;

end.
