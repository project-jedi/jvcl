{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDBReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Register db-aware components

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvaDBReg;

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

  TJvDatabaseNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

{################ from Delphi2\Lib\DBReg.pas }


procedure Register;

implementation

uses
  TypInfo, DB, DBTables,
  JvDBTreeView, JvDBLookupTreeView, JvDBMove, JvSQLS, JvxDConst;

{$R ..\resources\radb.dcr}

procedure Register;
const
  cKeyField = 'KeyField';
  cListField = 'ListField';
  cMasterField = 'MasterField';
  cDetailField = 'DetailField';
  cIconField = 'IconField';
  cItemField = 'ItemField';
  cStartMasterValue = 'StartMasterValue';
  {$IFDEF COMPILER6_UP}
  {$IFDEF COMPLIB_VCL}
  cDatabase = 'Database';
  cUseFilter = 'UseFilter';
  {$ENDIF}
  {$ENDIF}
begin
  {RADBCt unit}
  RegisterComponents(srJvBDEPalette, [TJvaSQLScript]);
  { JvDBMove unit }
  RegisterComponents(srJvBDEPalette, [TJvDBMove]);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, 'Source', TJvDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, 'Destination', TJvDatabaseNameProperty);

  {JvDBTreeView unit}
  RegisterComponents(srJvDataControlsPalette, [TJvDBTreeView]);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cItemField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cMasterField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cDetailField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cIconField, TJvDataFieldProperty);

  {JvDBLookupTreeView unit}
  RegisterComponents(srJvDataControlsPalette, [TJvDBLookupTreeView, TJvDBLookupTreeViewCombo]);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cKeyField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cListField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cMasterField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cDetailField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cIconField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cKeyField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cListField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cMasterField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cDetailField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cIconField, TJvListFieldProperty);

  {$IFDEF COMPILER6_UP}
  {$IFDEF COMPLIB_VCL}
  RegisterPropertiesInCategory(cDatabase, TJvDBTreeView,
    [cItemField, cMasterField, cDetailField, cIconField, cStartMasterValue, cUseFilter]);
  RegisterPropertiesInCategory(cDatabase, TJvDBLookupTreeView,
    [cMasterField, cDetailField, cIconField, cStartMasterValue, cKeyField, cListField, cUseFilter]);
  RegisterPropertiesInCategory(cDatabase, TJvDBLookupTreeViewCombo,
    [cMasterField, cDetailField, cIconField, cStartMasterValue, cKeyField, cListField, cUseFilter]);
  {$ENDIF}
  {$ENDIF}
end;

//=== TJvListFieldProperty ===================================================

function TJvListFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'ListSource';
end;

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

//=== TJvDatabaseNameProperty ================================================

procedure TJvDatabaseNameProperty.GetValueList(List: TStrings);
begin
  Session.GetDatabaseNames(List);
end;

end.

