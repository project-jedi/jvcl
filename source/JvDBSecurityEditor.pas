{$I JVCL.INC}
unit JvDBSecurityEditor;

interface
uses
  Classes,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF}
  SysUtils, DB, DBTables, JvDBEditors, JvDBSecur;


type
  { For TJvDBSecurity component }
  TJvUserTableNameProperty = class(TJvDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;

  TLoginNameFieldProperty = class(TJvDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;


implementation

//=== TJvUserTableNameProperty ===============================================

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

end.
