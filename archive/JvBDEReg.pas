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
  JvDBSecur, JvQuery, 
  {$IFNDEF DelphiPersonalEdition}
  JvSelDSFrm, JvDBEditors, JvBDEEditors, JvDBSecurityEditor, JvMemTableEditor, 
  {$ENDIF}
  {$IFDEF Jv_MIDAS}
  JvRemLog,
  {$ENDIF}
  JvQBndDlg,
  Consts, LibHelp, JvMemTable, JvxDConst;

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
  {$IFNDEF BCB}
  {$IFDEF USE_OLD_DBLISTS}
  RegisterComponents(srJvDataAccessPalette, [TJvDatabaseList, TJvLangDrivList,
    TJvTableList, TJvStoredProcList, TJvFieldList, TJvIndexList]);
  {$ENDIF USE_OLD_DBLISTS}
  {$ENDIF BCB}

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

