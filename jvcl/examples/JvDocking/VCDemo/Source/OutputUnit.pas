unit OutputUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, JvDockControlForm, Tabs, StdCtrls, ComCtrls, ExtCtrls,
  JvComponent;

type
  TOutputForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    TabSet1: TTabSet;
    ScrollBar1: TScrollBar;
    Memo1: TMemo;
    procedure lbDockClient1FormHide(Sender: TObject);
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
    procedure Panel2CanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OutputForm: TOutputForm;

implementation

uses Main;

{$R *.dfm}

procedure TOutputForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.Output_ToolButton.Down := False;
  MainForm.Output1.Checked := False;
  MainForm.Output_PopupItem.Checked := False;
end;

procedure TOutputForm.Panel2CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if Panel2.Width = NewWidth then Exit;
  if Panel2.Width > 0 then
  begin
    if (TabSet1.Width > 0) and (TabSet1.Width < Panel2.Width) then
      TabSet1.Width := Round(TabSet1.Width * NewWidth / Panel2.Width)
    else
      TabSet1.Width := Panel2.Width div 2;
  end;
  Splitter1.Left := TabSet1.Width;
end;

procedure TOutputForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.Output_ToolButton.Down := True;
  MainForm.Output1.Checked := True;
  MainForm.Output_PopupItem.Checked := True;
end;

end.
