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
unit CallStackUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvDockControlForm, JvComponent;

type
  TCallStackForm = class(TForm)
    Memo1: TMemo;
    lbDockClient1: TJvDockClient;
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CallStackForm: TCallStackForm;

implementation

uses Main;

{$R *.dfm}

procedure TCallStackForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.CallStack_ToolButton.Down := False;
  MainForm.CallStack1.Checked := False;
  MainForm.CallStack_PopupItem.Checked := False;
end;

procedure TCallStackForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.CallStack_ToolButton.Down := True;
  MainForm.CallStack1.Checked := True;
  MainForm.CallStack_PopupItem.Checked := True;
end;

end.
