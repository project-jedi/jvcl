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
unit WatchUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, Tabs, JvDockControlForm, Menus,
  ComCtrls, JvComponent;

type
  TWatchForm = class(TForm)
    Panel2: TPanel;
    TabSet1: TTabSet;
    lbDockClient1: TJvDockClient;
    Shape1: TShape;
    ListView1: TListView;
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure ListView1Resize(Sender: TObject);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WatchForm: TWatchForm;

implementation

uses Main;

{$R *.dfm}

procedure TWatchForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.Watch_ToolButton.Down := False;
  MainForm.Watch1.Checked := False;
  MainForm.Watch_PopupItem.Checked := False;
end;

procedure TWatchForm.ListView1Resize(Sender: TObject);
begin
  ListView1.Columns[1].Width := ListView1.Width - ListView1.Columns[0].Width;
end;

procedure TWatchForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.Watch_ToolButton.Down := True;
  MainForm.Watch1.Checked := True;
  MainForm.Watch_PopupItem.Checked := True;
end;

end.
