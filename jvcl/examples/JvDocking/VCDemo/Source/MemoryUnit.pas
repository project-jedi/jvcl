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
unit MemoryUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, JvDockControlForm, JvComponent;

type
  TMemoryForm = class(TForm)
    Panel1: TPanel;
    Edit1: TEdit;
    Label1: TLabel;
    lbDockClient1: TJvDockClient;
    Memo1: TMemo;
    procedure Panel1Resize(Sender: TObject);
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MemoryForm: TMemoryForm;

implementation

uses Main;

{$R *.dfm}

procedure TMemoryForm.Panel1Resize(Sender: TObject);
begin
  Edit1.Width := Panel1.Width - Edit1.Left;
end;

procedure TMemoryForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.Memory_ToolButton.Down := False;
  MainForm.Memory1.Checked := False;
  MainForm.Memory_PopupItem.Checked := False;
end;

procedure TMemoryForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.Memory_ToolButton.Down := True;
  MainForm.Memory1.Checked := True;
  MainForm.Memory_PopupItem.Checked := True;
end;

end.
