{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCSVBaseProp.PAS, released on 2002-11-04.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCsvBaseEditor;

{$I jvcl.inc}

interface

uses
  Classes,
  DesignIntf, DesignEditors, VCLEditors,
  JvCSVBaseControls;

type
  TJvCSVFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TJvCSVFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


implementation

uses
  Windows, Forms, Dialogs;

//=== { TJvCSVFileNameProperty } =============================================

procedure TJvCSVFileNameProperty.Edit;
begin
  with TOpenDialog.Create(Application) do
    try
      FileName := GetValue;
      if Execute then
        SetValue(FileName);
    finally
     Free;
    end;
end;

function TJvCSVFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

//=== { TJvCSVFieldProperty } ================================================

function TJvCSVFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TJvCSVFieldProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  DbEdit: TJvCSVEdit;
  DbCombo: TJvCSVComboBox;
  Ck: TJvCSVCheckBox;
  Compo: TPersistent;
begin
  try
    Compo := GetComponent(0);
    if Compo.ClassName = 'TJvCSVEdit' then
    begin
      DbEdit := TJvCSVEdit(GetComponent(0));
      if Assigned(DbEdit.CSVDataBase) then
        for I := 0 to DbEdit.CSVDataBase.CSVFieldNames.Count - 1 do
          Proc(DbEdit.CSVDataBase.CSVFieldNames[I]);
    end
    else
    if Compo.ClassName = 'TJvCSVComboBox' then
    begin
      DbCombo := TJvCSVComboBox(GetComponent(0));
      if Assigned(DbCombo.CSVDataBase) then
        for I := 0 to DbCombo.CSVDataBase.CSVFieldNames.Count - 1 do
          Proc(DbCombo.CSVDataBase.CSVFieldNames[I]);
    end
    else
    if Compo.ClassName = 'TJvCSVCheckBox' then
    begin
      Ck := TJvCSVCheckBox(GetComponent(0));
      if Assigned(Ck.CSVDataBase) then
        for I := 0 to Ck.CSVDataBase.CSVFieldNames.Count - 1 do
          Proc(Ck.CSVDataBase.CSVFieldNames[I]);
    end;
  except
  end;
end;


end.
