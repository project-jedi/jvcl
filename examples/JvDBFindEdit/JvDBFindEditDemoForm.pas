{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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
unit JvDBFindEditDemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, ExtCtrls, DBCtrls, DB, JvDBFindEdit,
  DBTables, Mask, JvExMask, JvMaskEdit, JvToolEdit;

type
  TJvDBFindEditDemoFrm = class(TForm)
    DataSource1: TDataSource;
    Table1: TTable;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Reset: TButton;
    yulFindEdit1: TJvDBFindEdit;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  JvDBFindEditDemoFrm: TJvDBFindEditDemoFrm;

implementation

{$R *.dfm}

procedure TJvDBFindEditDemoFrm.CheckBox1Click(Sender: TObject);
begin
  yulFindEdit1.IgnoreCase := not (CheckBox1.Checked);
  yulFindEdit1.ResetFilter;
end;

procedure TJvDBFindEditDemoFrm.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    yulFindEdit1.FindMode := fmAnyPos
  else
    yulFindEdit1.FindMode := fmFirstPos;
  yulFindEdit1.ResetFilter;
end;

procedure TJvDBFindEditDemoFrm.FormShow(Sender: TObject);
var
  i: Integer;
begin
  Table1.Open;
  for i := 0 to table1.FieldDefs.Count - 1 do
    ComboBox1.Items.Add(table1.FieldDefs.Items[i].Name);

  if ComboBox1.Items.Count > 0 then
    ComboBox1.ItemIndex := 0;

  if length(combobox1.Items[ComboBox1.ItemIndex]) > 0 then
    yulFindEdit1.DataField := combobox1.Items[ComboBox1.ItemIndex]
  else
    Showmessage('No field selected');
end;

procedure TJvDBFindEditDemoFrm.ComboBox1Change(Sender: TObject);
begin
  if length(combobox1.Items[ComboBox1.ItemIndex]) > 0 then
  begin
    yulFindEdit1.DataField := combobox1.Items[ComboBox1.ItemIndex];
    if table1.FieldList.FieldByName(yulFindEdit1.DataField) is TDateField then
      yulFindEdit1.EditMask := '!99/99/9999;1;_'
    else
      yulFindEdit1.EditMask := '';
  end
  else
    ShowMessage('No field selected');
  yulFindEdit1.Text := '';
end;

procedure TJvDBFindEditDemoFrm.ResetClick(Sender: TObject);
begin
  yulFindEdit1.ResetFilter;
end;

procedure TJvDBFindEditDemoFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Table1.Close;
end;

end.

