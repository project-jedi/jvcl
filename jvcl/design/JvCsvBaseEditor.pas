{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCSVBaseProp.PAS, released on 2002-11-04.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2002-11-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvCSVBaseEditor;

interface
uses JvCSVBaseControls, Classes,
{$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors
{$ELSE}
  DsgnIntf
{$ENDIF COMPILER6_UP}

  ;
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
uses Forms, Dialogs;

procedure TCSVFileNameProperty.Edit;
var
  dlg: TOpenDialog;
begin
  dlg := TOpendialog.Create(application);
  dlg.FileName := getValue;
  if dlg.Execute then
    SetValue(dlg.filename);
  dlg.free;
end;

function TCSVFileNameProperty.GetAttributes: TPropertyattributes;
begin
  result := [padialog];
end;

function TCSVFieldProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paValueList];
end;

procedure TCSVFieldProperty.GetValues(Proc: TGetStrProc);
var
  i, c: integer;
  dbedit: TJvCSVEdit;
  dbcombo: TJvCSVComboBox;
  ck: TJvCSVCheckBox;
  AComponent: TPersistent;
begin
  try
    Acomponent := getcomponent(0);
    if Acomponent.ClassName = 'TJvCSVEdit' then
    begin
      dbedit := TJvCSVEdit(getcomponent(0));
      if assigned(dbedit.CSVDataBase) then
      begin
        c := dbEdit.CSVDataBase.CSVFieldNames.count;
        if c > 0 then
          for i := 0 to c - 1 do
            Proc(dbEdit.CSVDataBase.CSVFieldNames[i]);
      end;
    end
    else if Acomponent.ClassName = 'TJvCSVComboBox' then
    begin
      dbcombo := TJvCSVComboBox(getcomponent(0));
      if assigned(dbcombo.CSVDataBase) then
      begin
        c := dbcombo.CSVDataBase.CSVFieldNames.count;
        if c > 0 then
          for i := 0 to c - 1 do
            Proc(dbcombo.CSVDataBase.CSVFieldNames[i]);
      end;
    end
    else if Acomponent.ClassName = 'TJvCSVCheckBox' then
    begin
      ck := TJvCSVCheckBox(getcomponent(0));
      if assigned(ck.CSVDataBase) then
      begin
        c := ck.CSVDataBase.CSVFieldNames.count;
        if c > 0 then
          for i := 0 to c - 1 do
            Proc(ck.CSVDataBase.CSVFieldNames[i]);
      end;
    end;
  except
  end;
end;

end.

