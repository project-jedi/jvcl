{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:  Lionel Reynaud

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

{$I jvcl.inc}

unit JvDBSearchDemoMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF COMPILER6_UP} Variants, {$ENDIF} Dialogs, Grids, StdCtrls, JvExStdCtrls,
  JvEdit, JvCombobox, ExtCtrls, DB, DBGrids, JvExDBGrids, JvDBGrid, DBTables,
  JvDBSearchEdit, JvDBSearchComboBox;

type
  TJvDBSearchDemoMainFrm = class(TForm)
    Table1: TTable;
    JvDBGrid1: TJvDBGrid;
    DataSource1: TDataSource;
    pnl1: TPanel;
    JvDBSearchComboBox1: TJvDBSearchComboBox;
    JvDBSearchEdit1: TJvDBSearchEdit;
    btnConnect: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
    chkClearOnEnter: TCheckBox;
    procedure btnConnectClick(Sender: TObject);
    procedure chkClearOnEnterClick(Sender: TObject);
  end;

var
  JvDBSearchDemoMainFrm: TJvDBSearchDemoMainFrm;

implementation

{$R *.dfm}

procedure TJvDBSearchDemoMainFrm.btnConnectClick(Sender: TObject);
begin
  Table1.Active := btnConnect.Tag = 0;
  if Table1.Active then
  begin
    btnConnect.Tag := 1;
    btnConnect.Caption := 'Disconnect';
  end
  else
  begin
    btnConnect.Tag := 0;
    btnConnect.Caption := 'Connect';
  end;
end;

procedure TJvDBSearchDemoMainFrm.chkClearOnEnterClick(Sender: TObject);
begin
  JvDBSearchEdit1.ClearOnEnter := chkClearOnEnter.Checked;
end;

end.
