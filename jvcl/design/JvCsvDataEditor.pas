{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDsgn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):  Warren Postma (warrenpstma att hotmail dott com)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  TJvCsvDataSet data access component. Design time unit.
  (contains property editors for TCsvDataSource component)

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvCsvDataEditor;

interface

uses
  SysUtils, Classes, Db,
  Windows, Messages, Forms, Dialogs, Graphics,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvCsvData;

type
  { A string property editor makes setting up the CSV Field definitions less painful }
  TJvCsvDefStrProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { A right click on the CSVDataSet will allow you to select the CSV field definitions editor. }
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

procedure Register;

implementation

uses
  JvCsvDataForm, JvConsts, JvDsgnConsts;

{ CsvDataDefStrDsgn= String Editor at design time for CSVDefs }

//=== { TJvCsvDefStrProperty } ===============================================

function TJvCsvDefStrProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function DoCsvDefDialog(OldValue: string): string;
var
  Dialog: TJvCsvDefStrDialog;
  dlgResult: Integer;
  WindowList: Pointer;
begin
  {$IFDEF MSWINDOWS}
  WindowList := DisableTaskWindows(0);
  {$ENDIF MSWINDOWS}
  Dialog := TJvCsvDefStrDialog.Create(nil); // no owner!
//  dlgResult := idCancel;
  try
    Dialog.SetCsvStr(OldValue);
    dlgResult := Dialog.ShowModal;
    if dlgResult = idOk then
      Result := Dialog.GetCsvStr
    else
      Result := OldValue;
  finally
    Dialog.Free;
    {$IFDEF MSWINDOWS}
    EnableTaskWindows(WindowList);
    {$ENDIF MSWINDOWS}
  end;
end;

procedure TJvCsvDefStrProperty.Edit;
var
  S1, S2: string;
  Component: TJvCustomCsvDataSet;
begin
  Component := GetComponent(0) as TJvCustomCsvDataSet;

  S1 := GetValue;
  if S1 = '' then
    S1 := Component.GetCsvHeader; // TODO! read first line of CSV file!
  S2 := DoCsvDefDialog(S1);

  //if S1 <> S2 then begin // on change of csv value.
  SetValue(S2);
  //end
end;

{
function TCSVDataSetComponentEditor.GetVerbCount: Integer;
begin
 Result := 2;  //The number of item in popup-menu
end;

function TCSVDataSetComponentEditor.GetVerb (Index : Integer) : string;
begin
  case Index of
    0:  Result := 'Edit CSV Field Definitions...'; // Label displayed for each
    1:  Result := 'Edit VCL Field Definitions...';
  else
    Result := '';
  end;
end;

procedure TCSVDataSetComponentEditor.ExecuteVerb(Index : Integer);
var
  CsvOwner: TCSVDataSet;
  s2: string;
begin
 case Index of
   0 : begin
//Execution for each
           try
             CsvOwner := Component as TCSVDataSet;
             // D.Caption := Component.Owner.Name +'.'+ Component.Name;
             s2 := DoCsvDefDialog( CsvOwner.CsvFieldDef );
             if CsvOwner.CsvFieldDef<>s2 then begin
                CsvOwner.CsvFieldDef := s2;
                Designer.Modified;
             end;
           finally
             // Any cleanup goes here.
           end;
      end;
   1 : begin
        // Requires DSDESIGN:
            ShowFieldsEditor(Designer, TDataSet(Component), GetDSDesignerClass);
          // Instead we have to pretend that this fields editor does not exist!?

      end;

  end;
end;
}

//=== { TJvFilenameProperty } ================================================

procedure TJvFilenameProperty.Edit;
begin
  with TOpenDialog.Create(Application) do
  begin
    Title := RsJvCsvDataSetSelectCSVFileToOpen;
    FileName := GetValue;
    Filter := RsCsvFilter;
    Options := Options + [ofPathMustExist];
    try
      if Execute then
        SetValue(FileName);
    finally
      Free;
    end;
  end;
end;

function TJvFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure Register;
const
 cCsvFieldDef = 'CsvFieldDef';
 //cTableName = 'TableName';
 cFileName = 'FileName';
begin
  RegisterPropertyEditor(TypeInfo(string), TJvCSVDataSet, cCsvFieldDef, TJvCsvDefStrProperty);
// RegisterPropertyEditor(TypeInfo(string), TJvCSVDataSet, cTableName, TFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCSVDataSet, cFileName, TJvFileNameProperty);

 { Component Editor - Verbs for the Right-Clicky-on-ye-component thing
   Requires a working DSDESIGN.pas source that will compile. }
// RegisterComponentEditor(TCSVDataSet, TCSVDataSetComponentEditor);

  RegisterComponents(RsPaletteDBNonVisual, [TJvCSVDataSet]);
end;

end.

