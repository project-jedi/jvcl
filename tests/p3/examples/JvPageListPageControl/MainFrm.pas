unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvPageListPCDemo, JvPageListTreeView, StdCtrls,
  ExtCtrls, ImgList, JvDriveCtrls, JvComCtrls;

type
  TForm1 = class(TForm)
    JvPageListTreeView1: TJvPageListTreeView;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    ImageList1: TImageList;
    procedure CheckBox1Click(Sender: TObject);
    procedure JvPageListTreeView1DragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure JvPageListTreeView1DragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure JvPageListTreeView1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
  private
    { Private declarations }
    FDragNode:TTreeNode;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
//  JvPageListPageControl1.HideAllTabs := CheckBox1.Checked;
end;

procedure TForm1.JvPageListTreeView1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var N:TTreeNode;
begin
  N := JvPageListTreeView1.GetNodeAt(X,Y);
  Accept := (Source = Sender) and (N <> FDragNode);
end;

procedure TForm1.JvPageListTreeView1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var N:TTreeNode;
begin
  N := JvPageListTreeView1.GetNodeAt(X,Y);
  if (FDragNode <> nil) and (N <> FDragNode) then
    FDragNode.MoveTo(N,naAddChild);
end;

procedure TForm1.JvPageListTreeView1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragNode := JvPageListTreeView1.Selected;
end;

end.
