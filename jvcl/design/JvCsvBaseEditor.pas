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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCsvBaseEditor;

{$I jvcl.inc}

interface

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvCSVBaseControls;

type
  TCSVFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TCSVFieldProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  Forms, Dialogs;

//=== { TCSVFileNameProperty } ===============================================

procedure TCSVFileNameProperty.Edit;
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

function TCSVFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

//=== { TCSVFieldProperty } ==================================================

function TCSVFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TCSVFieldProperty.GetValues(Proc: TGetStrProc);
var
  I, C: Integer;
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
      begin
        C := DbEdit.CSVDataBase.CSVFieldNames.Count;
        if C > 0 then
          for I := 0 to C - 1 do
            Proc(DbEdit.CSVDataBase.CSVFieldNames[I]);
      end;
    end
    else
    if Compo.ClassName = 'TJvCSVComboBox' then
    begin
      DbCombo := TJvCSVComboBox(GetComponent(0));
      if Assigned(DbCombo.CSVDataBase) then
      begin
        C := DbCombo.CSVDataBase.CSVFieldNames.Count;
        if C > 0 then
          for I := 0 to C - 1 do
            Proc(DbCombo.CSVDataBase.CSVFieldNames[I]);
      end;
    end
    else
    if Compo.ClassName = 'TJvCSVCheckBox' then
    begin
      Ck := TJvCSVCheckBox(GetComponent(0));
      if Assigned(Ck.CSVDataBase) then
      begin
        C := Ck.CSVDataBase.CSVFieldNames.Count;
        if C > 0 then
          for I := 0 to C - 1 do
            Proc(Ck.CSVDataBase.CSVFieldNames[I]);
      end;
    end;
  except
  end;
end;

end.

