unit WorkSpaceUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, JvDockControlForm, ComCtrls, ImgList, ExtCtrls;

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
