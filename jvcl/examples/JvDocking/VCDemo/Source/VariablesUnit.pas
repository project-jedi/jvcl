{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit VariablesUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Tabs, JvDockControlForm,
  JvComponent;

type
  TVariablesForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ComboBox1: TComboBox;
    lbDockClient1: TJvDockClient;
    Panel2: TPanel;
    ListView1: TListView;
    TabSet1: TTabSet;
    procedure ListView1Resize(Sender: TObject);
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VariablesForm: TVariablesForm;

implementation

uses Main;

{$R *.dfm}

procedure TVariablesForm.ListView1Resize(Sender: TObject);
begin
  ListView1.Columns[1].Width := ListView1.Width - ListView1.Columns[0].Width;
end;

procedure TVariablesForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.Variables_ToolButton.Down := False;
  MainForm.Variable1.Checked := False;
  MainForm.Variables_PopupItem.Checked := False;
end;

procedure TVariablesForm.Panel1Resize(Sender: TObject);
begin
  ComboBox1.Width := Panel1.Width - ComboBox1.Left;
end;

procedure TVariablesForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.Variables_ToolButton.Down := True;
  MainForm.Variable1.Checked := True;
  MainForm.Variables_PopupItem.Checked := True;
end;

end.
