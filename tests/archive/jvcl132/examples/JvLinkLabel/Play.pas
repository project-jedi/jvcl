{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Play.pas, released 2002-01-05.

The Initial Developer of the Original Code is David Polberger <dpol@swipnet.se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-01-05;
Current Version: 1.00

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
  Please see the accompanying documentation.
-----------------------------------------------------------------------------}

unit Play;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, JvLinkLabel;

type
  TfrmPlay = class(TForm)
    LinkLabel: TJvLinkLabel;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Panel2: TPanel;
    Bevel1: TBevel;
    Panel3: TPanel;
    btnRefresh: TButton;
    TreeView: TTreeView;
    Edit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses
  InfoStrings, JvLinkLabelDebug;

procedure TfrmPlay.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  LinkLabel.Caption := Lorem;
  Edit.Text := Lorem;
  btnRefreshClick(Self);
end;

procedure TfrmPlay.btnRefreshClick(Sender: TObject);
begin
  TreeView.Items.BeginUpdate;

  try
    TDebugLinkLabelTools.NodeTreeToTreeNodes(LinkLabel, TreeView.Items);
    TreeView.FullExpand;
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TfrmPlay.Panel2Resize(Sender: TObject);
begin
  Edit.Width := (Sender as TControl).Width - 16;
end;

procedure TfrmPlay.EditChange(Sender: TObject);
begin
  LinkLabel.Caption := (Sender as TEdit).Text;
end;

end.
