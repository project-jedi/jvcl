{$I JVCL.INC}

unit JvBDEEditors;

interface
uses
  Classes, JvDBEditors;

type
  TJvDatabaseNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TJvSessionNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  { For TJvFieldList, TJvIndexList components }
  TJvTableNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;


implementation
uses
  DB, DBTables;
  
//=== TJvDatabaseNameProperty ================================================

procedure TJvDatabaseNameProperty.GetValueList(List: TStrings);
begin
  if (GetComponent(0) is TDBDataSet) then
    (GetComponent(0) as TDBDataSet).DBSession.GetDatabaseNames(List)
  else if Session <> nil then
    Session.GetDatabaseNames(List);
end;

//=== TJvSessionNameProperty =================================================

procedure TJvSessionNameProperty.GetValueList(List: TStrings);
begin
  Sessions.GetSessionNames(List);
end;

//=== TJvTableNameProperty ===================================================

procedure TJvTableNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TDBDataSet).DBSession.GetTableNames((GetComponent(0)
    as TDBDataSet).DatabaseName, '', True, False, List);
end;

end.
