{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{
 NOTE: this demo requires that the Borland sample data is installed and that the
 DBDEMOS folder is available
}

unit JvInspectorDBDemoMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBTables, Grids, DBGrids, JvInspector, JvInspDB, StdCtrls,
  JvComponent, JvExControls;

type
  TJvInspectorDBDemoMainForm = class(TForm)
    JvInspectorBorlandPainter1: TJvInspectorBorlandPainter;
    JvInspector1: TJvInspector;
    dbInspector: TDatabase;
    Table1: TTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    btnOpenCloseDS: TButton;
    btnClose: TButton;
    Table1EmpNo: TIntegerField;
    Table1LastName: TStringField;
    Table1FirstName: TStringField;
    Table1PhoneExt: TStringField;
    Table1HireDate: TDateTimeField;
    Table1Salary: TFloatField;
    procedure FormShow(Sender: TObject);
    procedure btnOpenCloseDSClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  end;

var
  JvInspectorDBDemoMainForm: TJvInspectorDBDemoMainForm;

implementation

{$R *.dfm}

procedure TJvInspectorDBDemoMainForm.FormShow(Sender: TObject);
begin
  TJvInspectorDBData.New(JvInspector1.Root, DataSource1, ['LastName', 'FirstName', 'Salary']);
end;

procedure TJvInspectorDBDemoMainForm.btnOpenCloseDSClick(Sender: TObject);
begin
  Table1.Active := not Table1.Active;
  if not Table1.Active then
    btnOpenCloseDS.Caption := 'Open table'
  else
    btnOpenCloseDS.Caption := 'Close table'
end;

procedure TJvInspectorDBDemoMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
