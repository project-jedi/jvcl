{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit CheckTVDemoFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList, ComCtrls, StdCtrls, ExtCtrls,
  JvCheckTreeView, JvExComCtrls, JvComCtrls;

type
  TfrmCheckTVDemo = class(TForm)
    ilStandard: TImageList;
    ilChecks: TImageList;
    popTree: TPopupMenu;
    Nodetype1: TMenuItem;
    mnuNormal: TMenuItem;
    mnuCheckBox: TMenuItem;
    mnuRadioItem: TMenuItem;
    mnuChecked: TMenuItem;
    mnuDelete: TMenuItem;
    ilFlatChecks: TImageList;
    StatusBar1: TStatusBar;
    N1: TMenuItem;
    Clear1: TMenuItem;
    Label1: TLabel;
    edText: TEdit;
    Label2: TLabel;
    cbNodeType: TComboBox;
    Label3: TLabel;
    edImageIndex: TEdit;
    udImageIndex: TUpDown;
    cbStyle: TComboBox;
    chkChecked: TCheckBox;
    chkFlat: TCheckBox;
    Label5: TLabel;
    cbCascadeLevels: TComboBox;
    Label6: TLabel;
    cbCascadeOptions: TComboBox;
    btnRandom: TButton;
    btnAddChild: TButton;
    btnAdd: TButton;
    Label4: TLabel;
    JvCheckTreeView1: TJvCheckTreeView;
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
    procedure JvCheckTreeView1ContextPopup(Sender: TObject;
      MousePos: TPoint; var Handled: Boolean);
    procedure JvCheckTreeView1Toggling(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure JvCheckTreeView1Toggled(Sender: TObject; Node: TTreeNode);
  public
    procedure SetupNode(Node: TTreeNode);
  end;

var
  frmCheckTVDemo: TfrmCheckTVDemo;

implementation

{$R *.dfm}

procedure TfrmCheckTVDemo.FormCreate(Sender: TObject);
begin
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

procedure TfrmCheckTVDemo.SetupNode(Node: TTreeNode);
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
        JvCheckTreeView1.CheckBox[Node] := True;
        JvCheckTreeView1.Checked[Node] := chkChecked.Checked;
      end;
    2: // radio item
      begin
        JvCheckTreeView1.RadioItem[Node] := True;
        JvCheckTreeView1.Checked[Node] := chkChecked.Checked;
      end;
  end;
  Node.MakeVisible;
  JvCheckTreeView1.Selected := Node;
end;

procedure TfrmCheckTVDemo.btnAddClick(Sender: TObject);
begin
  SetupNode(JvCheckTreeView1.Items.Add(JvCheckTreeView1.Selected, edText.Text));
  edText.SetFocus;
end;

procedure TfrmCheckTVDemo.btnAddChildClick(Sender: TObject);
begin
  SetupNode(JvCheckTreeView1.Items.AddChild(JvCheckTreeView1.Selected, edText.Text));
  edText.SetFocus;
end;

procedure TfrmCheckTVDemo.cbNodeTypeChange(Sender: TObject);
begin
  chkChecked.Enabled := cbNodeType.ItemIndex > 0;
end;

procedure TfrmCheckTVDemo.mnuNormalClick(Sender: TObject);
var
  N: TTreeNode;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
  begin
    JvCheckTreeView1.CheckBox[N] := False;
    JvCheckTreeView1.RadioItem[N] := False;
  end;
end;

procedure TfrmCheckTVDemo.mnuCheckBoxClick(Sender: TObject);
var
  N: TTreeNode;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
    JvCheckTreeView1.CheckBox[N] := True;
end;

procedure TfrmCheckTVDemo.mnuRadioItemClick(Sender: TObject);
var
  N: TTreeNode;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
    JvCheckTreeView1.RadioItem[N] := True;
end;

procedure TfrmCheckTVDemo.mnuCheckedClick(Sender: TObject);
var
  N: TTreeNode;
  M: TMenuItem;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
  begin
    M := Sender as TMenuItem;
    M.Checked := not M.Checked;
    JvCheckTreeView1.Checked[N] := M.Checked;
  end;
end;

procedure TfrmCheckTVDemo.mnuDeleteClick(Sender: TObject);
var
  N: TTreeNode;
begin
  N := TTreeNode(popTree.Tag);
  if N <> nil then
    JvCheckTreeView1.Items.Delete(N);
end;

procedure TfrmCheckTVDemo.chkFlatClick(Sender: TObject);
begin
  if chkFlat.Checked then
    JvCheckTreeView1.StateImages := ilFlatChecks
  else
    JvCheckTreeView1.StateImages := ilChecks;
end;

procedure TfrmCheckTVDemo.btnRandomClick(Sender: TObject);
var
  i, aIndex: integer;
  N: TTreeNode;
begin
  Randomize;
  JvCheckTreeView1.Items.BeginUpdate;
  JvCheckTreeView1.Items.Clear;
  for i := 0 to 200 do
  begin
    if JvCheckTreeView1.Items.Count = 0 then
      N := JvCheckTreeView1.Items.AddChild(nil, edText.Text)
    else
    begin
      aIndex := Random(JvCheckTreeView1.Items.Count - 1);
      if AIndex < JvCheckTreeView1.Items.Count div 2 then
        N := JvCheckTreeView1.Items.AddChild(nil, edText.Text)
      else
        N := JvCheckTreeView1.Items.AddChild(JvCheckTreeView1.Items[aIndex], edText.Text)
    end;
    N.ImageIndex := Random(ilStandard.Count - 1);
    N.SelectedIndex := N.ImageIndex;
    case Random(8) of
      2..3: // check box
        begin
          JvCheckTreeView1.CheckBox[N] := True;
          JvCheckTreeView1.Checked[N] := Random(2) <> 0;
        end;
      5..7: // radio item
        begin
          JvCheckTreeView1.RadioItem[N] := True;
          JvCheckTreeView1.Checked[N] := Random(5) <> 0;
        end;
    end;
  end;
  JvCheckTreeView1.FullExpand;
  JvCheckTreeView1.Items.EndUpdate;
end;

procedure TfrmCheckTVDemo.cbStyleChange(Sender: TObject);
begin
  JvCheckTreeView1.CheckBoxOptions.Style := TJvTVCheckBoxStyle(cbStyle.ItemIndex);
  case JvCheckTreeView1.CheckBoxOptions.Style of
    cbsNative, cbsNone:
      JvCheckTreeView1.StateImages := nil;
    cbsJVCL:
      if chkFlat.Checked then
        JvCheckTreeView1.StateImages := ilFlatChecks
      else
        JvCheckTreeView1.StateImages := ilChecks;
  end;
end;

const
  cOnOff: array [Boolean] of PChar = ('off', 'on');

procedure TfrmCheckTVDemo.cbCascadeLevelsChange(Sender: TObject);
begin
  JvCheckTreeView1.CheckBoxOptions.CascadeLevels := cbCascadeLevels.ItemIndex - 1;
end;

procedure TfrmCheckTVDemo.Clear1Click(Sender: TObject);
begin
  JvCheckTreeView1.Items.Clear;
end;

procedure TfrmCheckTVDemo.cbCascadeOptionsChange(Sender: TObject);
var
  F: TJvTVCascadeOptions;
begin
  F := [];
  case cbCascadeOptions.ItemIndex of
    0: F := [poOnCheck];
    1: F := [poOnUnCheck];
    2: F := [poOnCheck, poOnUnCheck];
  end;
  JvCheckTreeView1.CheckBoxOptions.CascadeOptions := F;
end;

procedure TfrmCheckTVDemo.JvCheckTreeView1ContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  N: TTreeNode;
begin
  N := JvCheckTreeView1.GetNodeAt(MousePos.X, MousePos.Y);
  if N <> nil then
  begin
    popTree.Tag := integer(N);
    mnuChecked.Enabled := True;
    mnuChecked.Checked := JvCheckTreeView1.Checked[N];
    if JvCheckTreeView1.CheckBox[N] then
      mnuCheckBox.Checked := True
    else
    if JvCheckTreeView1.RadioItem[N] then
      mnuRadioItem.Checked := True
    else
    begin
      mnuChecked.Enabled := False;
      mnuNormal.Checked := True;
      mnuChecked.Checked := False;
    end;
  end
  else
  begin
    popTree.Tag := 0;
    Handled := True;
  end;
end;

procedure TfrmCheckTVDemo.JvCheckTreeView1Toggling(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  StatusBar1.Panels[0].Text := Format('Node %s about to be toggled %s', [Node.Text, cOnOff[not JvCheckTreeView1.Checked[Node]]]);
end;

procedure TfrmCheckTVDemo.JvCheckTreeView1Toggled(Sender: TObject;
  Node: TTreeNode);
begin
  StatusBar1.Panels[0].Text := Format('Node %s toggled %s', [Node.Text, cOnOff[JvCheckTreeView1.Checked[Node]]]);
end;

end.

