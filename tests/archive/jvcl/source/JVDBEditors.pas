{$I JVCL.INC}

unit JVDBEditors;

interface
uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF COMPILER6_UP}

type
  {**************** from Delphi2\Lib\DBReg.pas }
  TJvDBStringProperty  = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvDataFieldProperty = class(TJvDBStringProperty)
  public
    function GetDataSourcePropName: string; virtual;
    procedure GetValueList(List: TStrings); override;
  end;

  TJvListFieldProperty = class(TJvDataFieldProperty)
  public
    function GetDataSourcePropName: string; override;
  end;

  { For TJvDBLookupList, TJvDBLookupCombo components }
  TJvLookupSourceProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
    function GetDataSourcePropName: string; virtual;
  end;


implementation
uses
  DB, TypInfo;

//=== TJvDBStringProperty ====================================================

function TJvDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TJvDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

//=== TJvDataFieldProperty ===================================================

function TJvDataFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TJvDataFieldProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := TComponent(GetComponent(0));
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
  end;
end;

{ TJvListFieldProperty }

//=== TJvListFieldProperty ===================================================

function TJvListFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'ListSource';
end;

//=== TJvLookupSourceProperty =======================================================

function TJvLookupSourceProperty.GetDataSourcePropName: string;
begin
  Result := 'LookupSource';
end;

procedure TJvLookupSourceProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := TComponent(GetComponent(0));
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
  end;
end;

end.
