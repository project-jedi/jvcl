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
unit WorkSpaceUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, JvDockControlForm, ComCtrls, ImgList, ExtCtrls, JvComponent;

type
  TWorkSpaceForm = class(TForm)
    lbDockClient1: TJvDockClient;
    WorkSpace_ImageList: TImageList;
    PageControl1: TPageControl;
    ClassView_TabSheet: TTabSheet;
    ClassView_TreeView: TTreeView;
    ResourceView_TabSheet: TTabSheet;
    ResourceView_TreeView: TTreeView;
    FileView_TabSheet: TTabSheet;
    FileView_TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure lbDockClient1FormHide(Sender: TObject);
    procedure PageControl1Resize(Sender: TObject);
    procedure ClassView_TreeViewCollapsed(Sender: TObject;
      Node: TTreeNode);
    procedure ClassView_TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure lbDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WorkSpaceForm: TWorkSpaceForm;

implementation

uses Main;

{$R *.dfm}

procedure TWorkSpaceForm.FormCreate(Sender: TObject);

{  procedure EachForAt(TreeNodes: TTreeNodes);
  var i: Integer;
  begin
    for i := 0 to TreeNodes.Count - 1 do
    begin
      EachForAt(TreeNodes.Item[i]);
//      TreeNodes
    end;
  end;}

begin
//
//  EachForAt(ClassView_TreeView.Items);
//  ClassView_TreeView.select
end;

procedure TWorkSpaceForm.lbDockClient1FormHide(Sender: TObject);
begin
  MainForm.WorkSpace_ToolButton.Down := False;
  MainForm.WorkSpace1.Checked := False;
  MainForm.WorkSpace_PopupItem.Checked := False;
end;

procedure TWorkSpaceForm.PageControl1Resize(Sender: TObject);
begin
//  if PageControl1.Width > 320 then
//    PageControl1.TabWidth := 0
//  else if PageControl1.Width > 3 then
//    PageControl1.TabWidth := PageControl1.Width div 3 - 1;
end;

procedure TWorkSpaceForm.ClassView_TreeViewCollapsed(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.ImageIndex = 2 then
  begin
    Node.ImageIndex := 24;
    Node.SelectedIndex := 24;
  end;
end;

procedure TWorkSpaceForm.ClassView_TreeViewExpanded(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.ImageIndex = 24 then
  begin
    Node.ImageIndex := 2;
    Node.SelectedIndex := 2;
  end;
end;

procedure TWorkSpaceForm.lbDockClient1FormShow(Sender: TObject);
begin
  MainForm.WorkSpace_ToolButton.Down := True;
  MainForm.WorkSpace1.Checked := True;
  MainForm.WorkSpace_PopupItem.Checked := True;
end;

end.
