{$I JVCL.INC}

unit JvBDEReg;


interface

procedure Register;

implementation
uses
  SysUtils, Classes, DesignIntf, JvBDEEditors, 
  JvBdeUtils, JvBDEFilter, JvBDEIndex, JvBDELists, JvBDEMove,
  JvBDEProgress, JvBDEQBE, JvBDESecurity, JvBDEMemTable,
  JvBDEQuery, JvBDESQLScript,
  JvBDEMemTableEditor, JvBDESecurityEditor;


{.$R ..\resources\JvBDEReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv BDE Controls',[
    TJvDBFilter, TJvDBIndexCombo, TJvDBListDataSet, TJvDatabaseItems,
    TJvTableItems, TJvDatabaseList, TJvLangDrivList, TJvTableList, TJvStoredProcList, TJvFieldList,
    TJvIndexList, TJvDBMove, TJvDBProgress, TJvQBEQuery, TJvDBSecurity,
    TJvBDEMemoryTable, TJvQuery, TJvBDESQLScript  
    ]);

  RegisterPropertyEditor(TypeInfo(TFileName), TJvCustomTableItems, 'TableName',
    TJvTableNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TJvDBSecurity,
    'UsersTableName', TJvUserTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBSecurity,
    'LoginNameField', TLoginNameFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBDESQLScript, 'DatabaseName',
    TJvDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomBDEItems, 'SessionName',
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBDESQLScript, 'SessionName',
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBProgress, 'SessionName',
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove,
    'Source', TJvDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove,
    'Destination', TJvDatabaseNameProperty);

  RegisterComponentEditor(TJvBDEMemoryTable, TJvBDEMemoryTableEditor);

end;

end.

