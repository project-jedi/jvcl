unit JvCsvDataDsgn;

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDsgn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s):  Warren Postma (warrenpstma@hotmail.com)

Last Modified: 2003-04-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : TJvCsvDataSet data access component. Design time unit.

Known Issues:
-----------------------------------------------------------------------------}

{
  Design Time unit
  (contains property editors for TCsvDataSource component)
  Written by Warren Postma.
  Donated to Delphi JEDI Project.

}
interface

uses Windows,
     Messages,
     Db,
     SysUtils,
     Classes,
     Forms,
     Dialogs,
     Graphics,
     DesignIntf,
     DesignEditors,
     JvCsvData;



type

  { A string property editor makes setting up the CSV Field definitions less painful }
  TJvCsvDefStrProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { A right clicky on the CSVDataSet will allow you to select the CSV field definitions editor. }
{  TCSVDataSetComponentEditor = class(TComponentEditor)
    function GetVerbCount : Integer; override;
    function GetVerb (Index : Integer) : string; override;
    procedure ExecuteVerb(Index : Integer); override;
  end;
 }

  TJvFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


{ VCL Register }
procedure Register;

implementation

{ CsvDataDefStrDsgn= String Editor at design time for CSVDefs }


uses JvCsvDataDefStrDsgn; {,DSDESIGN}

function TJvCsvDefStrProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function DoCsvDefDialog(OldValue:String):String;
var
  Dialog:TJvCsvDefStrDialog;
  dlgResult:Integer;
  WindowList: Pointer;
begin
    WindowList := DisableTaskWindows(0);
    try
      dlgResult := idCancel;
      Dialog :=  TJvCsvDefStrDialog.Create(nil); // no owner!
      Dialog.SetCsvStr(OldValue);
      dlgResult := Dialog.ShowModal;
      if dlgResult = idOk then
              result := Dialog.GetCsvStr
      else
              result := OldValue;
    finally
      Dialog.Free;
      EnableTaskWindows(WindowList);
    end;
end;

procedure TJvCsvDefStrProperty.Edit;
var
  s1,s2:String;
  Component:TJvCsvCustomInMemoryDataSet;
begin

    Component := GetComponent(0) as TJvCsvCustomInMemoryDataSet;
    
    s1 := GetValue();
    if (s1 = '') then
        s1 := Component.GetCsvHeader; // todo! read first line of CSV file!
    s2 := DoCsvDefDialog(s1);        

    //if s1<>s2 then begin // on change of csv value.
    SetValue(s2);
    //end
end;






{ TJvFilenameProperty }
procedure TJvFilenameProperty.Edit;
var
  csvFileOpen: TOpenDialog;
begin
  csvFileOpen := TOpenDialog.Create(Application);
  csvFileOpen.Title := 'JvCsvDataSet - Select CSV File to Open';
  csvFileOpen.Filename := GetValue;
  csvFileOpen.Filter := '*.csv';
  csvFileOpen.Options := csvFileOpen.Options + [ofPathMustExist];
  try
    if csvFileOpen.Execute then SetValue(csvFileOpen.Filename);
  finally
    csvFileOpen.Free;
  end;
end;

function TJvFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;



// VCL Registration
procedure Register;
begin
{ Property Editors }
 RegisterPropertyEditor(TypeInfo(string), TJvCSVDataSet, 'CsvFieldDef', TJvCsvDefStrProperty);
// RegisterPropertyEditor(TypeInfo(string), TJvCSVDataSet, 'TableName', TFilenameProperty);
 RegisterPropertyEditor(TypeInfo(string), TJvCSVDataSet, 'Filename', TJvFileNameProperty);

 { Component Editor - Verbs for the Right-Clicky-on-ye-component thing
   Requires a working DSDESIGN.pas source that will compile. }
// RegisterComponentEditor(TCSVDataSet, TCSVDataSetComponentEditor);

{ Component }
  RegisterComponents( 'Jv Data Access', [TJvCSVDataSet]); // {'Data Access'}

end;

end.
