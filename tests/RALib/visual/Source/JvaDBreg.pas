{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDBReg.pas, released on 2002-07-04.

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

uses Classes,
  {$IFDEF COMPILER6_UP}
   DesignIntf, DesignEditors, VCLEditors
  {$ELSE}
   DsgnIntf
  {$ENDIF COMPILER6_UP}
  ;

type

 {**************** from Delphi2\Lib\DBReg.pas }
  TJvDBStringProperty  = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvDataFieldProperty  = class(TJvDBStringProperty )
  public
    function GetDataSourcePropName: string; virtual;
    procedure GetValueList(List: TStrings); override;
  end;

  TJvListFieldProperty  = class(TJvDataFieldProperty )
  public
    function GetDataSourcePropName: string; override;
  end;

  TJvDatabaseNameProperty  = class(TJvDBStringProperty )
  public
    procedure GetValueList(List: TStrings); override;
  end;

{################ from Delphi2\Lib\DBReg.pas }


procedure Register;

implementation

uses TypInfo, DB, DBTables,
  JvDBUtil, JvDBTreeView, JvDBLookupTreeView, JvDBMove, JvSQLS;

{$R radb.dcr}

const
{$IFDEF MSWINDOWS}
  RALibTabName = 'JVCL-RA';
{$ENDIF}
{$IFDEF LINUX}
  RALibTabName = 'JVCL-RA';
{$ENDIF}

procedure Register;
begin
 {RADBCt unit}
  RegisterComponents(RALibTabName, [TJvaSQLScript]);
 {JvDBTreeView unit}
  RegisterComponents(RALibTabName, [TJvDBTreeView]);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, 'ItemField', TJvDataFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, 'MasterField', TJvDataFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, 'DetailField', TJvDataFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, 'IconField', TJvDataFieldProperty );

 {JvDBLookupTreeView unit}
  RegisterComponents(RALibTabName, [TJvDBLookupTreeView, TJvDBLookupTreeViewCombo]);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, 'KeyField', TJvListFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, 'ListField', TJvListFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, 'MasterField', TJvListFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, 'DetailField', TJvListFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, 'IconField', TJvListFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, 'KeyField', TJvListFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, 'ListField', TJvListFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, 'MasterField', TJvListFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, 'DetailField', TJvListFieldProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, 'IconField', TJvListFieldProperty );

 { JvDBMove unit }
  RegisterComponents(RALibTabName, [TJvDBMove]);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, 'Source', TJvDatabaseNameProperty );
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, 'Destination', TJvDatabaseNameProperty );

 {$IFDEF COMPILER6_UP}
 {$IFDEF COMPLIB_VCL}
  RegisterPropertiesInCategory('Database', TJvDBTreeView,
    ['ItemField', 'MasterField', 'DetailField', 'IconField', 'StartMasterValue', 'UseFilter']);
  RegisterPropertiesInCategory('Database', TJvDBLookupTreeView,
    ['MasterField', 'DetailField', 'IconField', 'StartMasterValue', 'KeyField', 'ListField', 'UseFilter']);
  RegisterPropertiesInCategory('Database', TJvDBLookupTreeViewCombo,
    ['MasterField', 'DetailField', 'IconField', 'StartMasterValue', 'KeyField', 'ListField', 'UseFilter']);
 {$ENDIF}
 {$ENDIF}

end;

{**************** from Delphi2\Lib\DBReg.pas }
function TJvListFieldProperty .GetDataSourcePropName: string;
begin
  Result := 'ListSource';
end;

function TJvDBStringProperty .GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TJvDBStringProperty .GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;


function TJvDataFieldProperty .GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TJvDataFieldProperty .GetValueList(List: TStrings);
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

procedure TJvDatabaseNameProperty .GetValueList(List: TStrings);
begin
  Session.GetDatabaseNames(List);
end;
{################ from Delphi2\Lib\DBReg.pas }

end.

