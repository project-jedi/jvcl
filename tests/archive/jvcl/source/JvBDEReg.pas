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

{$I JVCL.INC}

unit JvBDEReg;

interface

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF}
  SysUtils, DB, DBTables;

{ Register data aware custom controls }

procedure Register;

implementation

uses
  TypInfo,
  JvDBLists, JvDBQBE, JvDBFilter, JvDBIndex, JvDBPrgrss,
  JvDBSecur, JvQuery, JvDsgn,
  {$IFNDEF DelphiPersonalEdition}
  JvSelDSFrm,
  {$ENDIF}
  {$IFDEF Jv_MIDAS}
  JvRemLog,
  {$ENDIF}
  JvQBndDlg,
  Consts, LibHelp, JvMemTable, JvxDConst;

//=== TJvSessionNameProperty =================================================

type
  TJvSessionNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvSessionNameProperty.GetValueList(List: TStrings);
begin
  Sessions.GetSessionNames(List);
end;

//=== TJvDatabaseNameProperty ================================================

type
  TJvDatabaseNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvDatabaseNameProperty.GetValueList(List: TStrings);
var
  S: TSession;
begin
  if (GetComponent(0) is TDBDataSet) then
    (GetComponent(0) as TDBDataSet).DBSession.GetDatabaseNames(List)
  else
  if (GetComponent(0) is TJvSQLScript) then
  begin
    S := Sessions.FindSession((GetComponent(0) as TJvSQLScript).SessionName);
    if S = nil then
      S := Session;
    S.GetDatabaseNames(List);
  end;
end;

//=== TJvTableNameProperty ===================================================

{ For TJvFieldList, TJvIndexList components }

type
  TJvTableNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TJvTableNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TJvCustomTableItems).DBSession.GetTableNames((GetComponent(0)
    as TJvCustomTableItems).DatabaseName, '', True, False, List);
end;


//=== TJvUserTableNameProperty ===============================================

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
  if Security.Database <> nil then
  begin
    Security.Database.Session.GetTableNames(Security.Database.DatabaseName,
      '*.*', True, False, List);
  end;
end;

//=== TLoginNameFieldProperty ================================================

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
  if (Security.Database <> nil) and (Security.UsersTableName <> '') then
  begin
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

{$IFNDEF DelphiPersonalEdition}

//=== TJvMemoryTableEditor ===================================================

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

{$ENDIF DelphiPersonalEdition}

{ Designer registration }

procedure Register;
const
  cParams = 'Params';
  cSessionName = 'SessionName';
begin
  { Database Components are excluded from the STD SKU }
  if GDAL = LongWord(-16) then
    Exit;

  { Data aware components and controls }
  RegisterComponents(srJvDataAccessPalette, [TJvQuery, TJvSQLScript,
    TJvMemoryTable, TJvQBEQuery, TJvDBFilter,
      TJvDBSecurity]);

  RegisterComponents(srJvDataControlsPalette, [TJvDBIndexCombo, TJvDBProgress]);
  {$IFDEF Jv_MIDAS}
  { MIDAS components }
  RegisterComponents(srJvDataAccessPalette, [TJvRemoteLogin]);
  RegisterNonActiveX([TJvRemoteLogin], axrComponentOnly);
  {$ENDIF}
  { Database lists }
  RegisterComponents(srJvDataAccessPalette, [TJvBDEItems, TJvDatabaseItems,
    TJvTableItems]);
  {$IFNDEF CBUILDER}
  {$IFDEF USE_OLD_DBLISTS}
  RegisterComponents(srJvDataAccessPalette, [TJvDatabaseList, TJvLangDrivList,
    TJvTableList, TJvStoredProcList, TJvFieldList, TJvIndexList]);
  {$ENDIF USE_OLD_DBLISTS}
  {$ENDIF CBUILDER}

  RegisterNonActiveX([TJvQuery, TJvSQLScript, TJvMemoryTable, TJvQBEQuery,
    TJvDBFilter, TJvDBIndexCombo, TJvDBProgress, TJvDBSecurity, TJvBDEItems,
      TJvDatabaseItems, TJvTableItems], axrComponentOnly);

  { Property and component editors for data aware controls }

  RegisterPropertyEditor(TypeInfo(TFileName), TJvCustomTableItems, 'TableName',
    TJvTableNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TJvDBSecurity,
    'UsersTableName', TJvUserTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBSecurity,
    'LoginNameField', TLoginNameFieldProperty);
  
  {$IFNDEF DelphiPersonalEdition}
  RegisterComponentEditor(TJvMemoryTable, TJvMemoryTableEditor);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TJvSQLScript, 'DatabaseName',
    TJvDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomBDEItems, cSessionName,
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvSQLScript, cSessionName,
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBProgress, cSessionName,
    TJvSessionNameProperty);
end;

end.

