unit CheckTVDemoFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, JvComCtrls, JvCheckTV, ImgList, ComCtrls, StdCtrls, ExtCtrls;

type
  TfrmCheckTVDemo = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    ilStandard: TImageList;
    ilChecks: TImageList;
    Label1: TLabel;
    edText: TEdit;
    btnAdd: TButton;
    cbNodeType: TComboBox;
    Label2: TLabel;
    btnAddChild: TButton;
    chkChecked: TCheckBox;
    Label3: TLabel;
    edImageIndex: TEdit;
    udImageIndex: TUpDown;
    popTree: TPopupMenu;
    Nodetype1: TMenuItem;
    mnuNormal: TMenuItem;
    mnuCheckBox: TMenuItem;
    mnuRadioItem: TMenuItem;
    mnuChecked: TMenuItem;
    mnuDelete: TMenuItem;
    chkFlat: TCheckBox;
    ilFlatChecks: TImageList;
    btnRandom: TButton;
    cbStyle: TComboBox;
    StatusBar1: TStatusBar;
    Label4: TLabel;
    cbCascadeLevels: TComboBox;
    Label5: TLabel;
    N1: TMenuItem;
    Clear1: TMenuItem;
    Label6: TLabel;
    cbCascadeOptions: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnAddChildClick(Sender: TObject);
    procedure cbNodeTypeChange(Sender: TObject);
    procedure mnuNormalClick(Sender: TObject);
    procedure mnuCheckBoxClick(Sender: TObject);
    procedure mnuRadioItemClick(Sender: TObject);
    procedure mnuCheckedClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure chkFlatClick(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
    procedure cbStyleChange(Sender: TObject);
    procedure cbCascadeLevelsChange(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure cbCascadeOptionsChange(Sender: TObject);
  private
    { Private declarations }
    procedure SetupNode(Node: TTreeNode);
    procedure DoTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DoToggling(Sender:TObject;Node:TTreeNode;var AllowChange:boolean);
    procedure DoToggled(Sender:TObject;Node:TTreeNode);
  public
    { Public declarations }
    tv: TJvCheckTreeView;
  end;

var
  frmCheckTVDemo: TfrmCheckTVDemo;

implementation

{$R *.dfm}

procedure TfrmCheckTVDemo.FormCreate(Sender: TObject);
begin
  tv := TJvCheckTreeview.Create(self);
  tv.Align := alLeft;
  tv.Width := 200;
  tv.Parent := self;
  tv.Images := ilStandard;
  tv.StateImages := ilChecks;
  tv.HideSelection := false;
  tv.PopupMenu := popTree;

  tv.OnContextPopup := DoTreeContextPopup;
  tv.OnToggling := DoToggling;
  tv.OnToggled  := DoToggled;

  udImageIndex.Max := ilStandard.Count - 1;
  cbNodeType.ItemIndex := 1; // checkboxes
  cbNodeTypeChange(Sender);

  cbStyle.ItemIndex := 2; //cbsJVCL
  cbStyleChange(Sender);

  cbCascadeLevels.ItemIndex := 0;
  cbCascadeLevelsChange(Sender);

  cbCascadeOptions.ItemIndex := 2; // cascade all
  cbCascadeOptionsChange(Sender);
end;

procedure TfrmCheckTVDemo.SetupNode(Node:TTreeNode);
begin
  Node.ImageIndex := udImageIndex.Position;
  Node.SelectedIndex := Node.ImageIndex;
  if udImageIndex.Position <> -1 then
  begin
    if udImageIndex.Position < udImageIndex.Max then
      udImageIndex.Position := udImageIndex.Position + 1
    else
      udImageIndex.Position := 0;
  end;
  case cbNodeType.ItemIndex of
    1: // check box
      begin
        tv.CheckBox[Node] := true;
        tv.Checked[Node] := chkChecked.Checked;
      end;
    2:  // radio item
      begin
        tv.RadioItem[Node] := true;
        tv.Checked[Node] := chkChecked.Checked;
      end;
  end;
  Node.MakeVisible;
  tv.Selected := Node;
end;

procedure TfrmCheckTVDemo.btnAddClick(Sender: TObject);
begin
  SetupNode(tv.Items.Add(tv.Selected, edText.Text));
  edText.SetFocus;
end;

procedure TfrmCheckTVDemo.btnAddChildClick(Sender: TObject);
begin
  SetupNode(tv.Items.AddChild(tv.Selected, edText.Text));
  edText.SetFocus;
end;

procedure TfrmCheckTVDemo.cbNodeTypeChange(Sender: TObject);
begin
  chkChecked.Enabled := cbNodeType.ItemIndex > 0;
end;

procedure TfrmCheckTVDemo.DoTreeContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var N:TTreeNode;
begin
  N := tv.GetNodeAt(MousePos.X,MousePos.Y);
  if N <> nil then
  begin
    popTree.Tag := integer(N);
    mnuChecked.Enabled := true;
    mnuChecked.Checked := tv.Checked[N];
    if tv.CheckBox[N] then
      mnuCheckBox.Checked := true
    else if tv.RadioItem[N] then
      mnuRadioItem.Checked := true
    else
    begin
      mnuChecked.Enabled := false;
      mnuNormal.Checked := true;
      mnuChecked.Checked := false;
    end;
  end
  else                        
  begin
    popTree.Tag := 0;
    Handled := true;
  end;
end;

procedure TfrmCheckTVDemo.mnuNormalClick(Sender: TObject);
var N:TTreeNode;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
  begin
    tv.CheckBox[N] := false;
    tv.RadioItem[N] := false;
  end;
end;

procedure TfrmCheckTVDemo.mnuCheckBoxClick(Sender: TObject);
var N:TTreeNode;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
    tv.CheckBox[N] := true;
end;

procedure TfrmCheckTVDemo.mnuRadioItemClick(Sender: TObject);
var N:TTreeNode;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
    tv.RadioItem[N] := true;
end;

procedure TfrmCheckTVDemo.mnuCheckedClick(Sender: TObject);
var N:TTreeNode;M:TMenuItem;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
  begin
    M := Sender as TMenuItem;
    M.Checked := not M.Checked;
    tv.Checked[N] := M.Checked;
  end;
end;

procedure TfrmCheckTVDemo.mnuDeleteClick(Sender: TObject);
var N:TTreeNode;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
    tv.Items.Delete(N);
end;

procedure TfrmCheckTVDemo.chkFlatClick(Sender: TObject);
begin
  if chkFlat.Checked then
    tv.StateImages := ilFlatChecks
  else
    tv.StateImages := ilChecks;
end;

procedure TfrmCheckTVDemo.btnRandomClick(Sender: TObject);
var i,aIndex:integer;N:TTreeNode;
begin
  Randomize;
  tv.Items.Clear;
  for i := 0 to 200 do
  begin
    if tv.Items.Count =  0 then
      N := tv.Items.AddChild(nil,edText.Text)
    else
    begin
      aIndex := Random(tv.Items.Count-1);
      if AIndex < tv.Items.Count div 2 then
        N := tv.Items.AddChild(nil,edText.Text)
      else
        N := tv.Items.AddChild(tv.Items[aIndex],edText.Text)
    end;
    N.ImageIndex := Random(ilStandard.Count-1);
    N.SelectedIndex := N.ImageIndex;
    case Random(8) of
    2..3: // check box
      begin
        tv.CheckBox[N] := true;
        tv.Checked[N]  := boolean(Random(2));
      end;
    5..7:  // radio item
      begin
        tv.RadioItem[N] := true;
        tv.Checked[N]  := boolean(Random(5));
      end;
    end;
  end;
  tv.FullExpand;
end;

procedure TfrmCheckTVDemo.cbStyleChange(Sender: TObject);
begin
  tv.CheckBoxOptions.Style := TJvTVCheckBoxStyle(cbStyle.ItemIndex);
  case tv.CheckBoxOptions.Style of
    cbsNative,cbsNone:
      tv.StateImages := nil;
    cbsJVCL:
      if chkFlat.Checked then
        tv.StateImages := ilFlatChecks
      else
        tv.StateImages := ilChecks;
  end;
end;

const
  cOnOff:array[boolean] of PChar = ('off','on');

procedure TfrmCheckTVDemo.DoToggled(Sender: TObject; Node: TTreeNode);
begin
  StatusBar1.Panels[0].Text := Format('Node %s toggled %s',[Node.Text,cOnOff[tv.Checked[Node]]]);
end;

procedure TfrmCheckTVDemo.DoToggling(Sender: TObject; Node: TTreeNode;
  var AllowChange: boolean);
begin
  StatusBar1.Panels[0].Text := Format('Node %s about to be toggled %s',[Node.Text,cOnOff[not tv.Checked[Node]]]);
end;

procedure TfrmCheckTVDemo.cbCascadeLevelsChange(Sender: TObject);
begin
  tv.CheckBoxOptions.CascadeLevels := cbCascadeLevels.ItemIndex - 1;
end;

procedure TfrmCheckTVDemo.Clear1Click(Sender: TObject);
begin
  tv.Items.Clear;
end;

procedure TfrmCheckTVDemo.cbCascadeOptionsChange(Sender: TObject);
var F:TJvTVCascadeOptions;
begin
  F := [];
  case cbCascadeOptions.ItemIndex of
    0: F := [poOnCheck];
    1: F := [poOnUnCheck];
    2: F := [poOnCheck,poOnUnCheck];
  end;
  tv.CheckBoxOptions.CascadeOptions := F;
end;

end.

