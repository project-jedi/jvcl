{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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
{$I jvcl.inc}
unit main;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, JvUIB, JVUIBMetaData, StdCtrls, ComCtrls, Menus, ExtCtrls,
  JvComponent;

type
  TMainForm = class(TForm)
    DataBase: TJvUIBDataBase;
    Transaction: TJvUIBTransaction;
    TreeView: TTreeView;
    Memo: TMemo;
    MainMenu: TMainMenu;
    mFile: TMenuItem;
    Open: TMenuItem;
    SaveToFile1: TMenuItem;
    LoadFromFile1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Splitter: TSplitter;
    procedure OpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure SaveToFile1Click(Sender: TObject);
    procedure LoadFromFile1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ShowNodes(node: TMetaNode; Parent: TTreeNode);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  MetaData: TMetaDataBase;
implementation

uses Math;

{$R *.dfm}

procedure TMainForm.ShowNodes(node: TMetaNode; Parent: TTreeNode);
var
  i, j: Integer;
  ClassNode: TTreeNode;
begin
  Parent := TreeView.Items.AddChild(Parent, node.Name);
  Parent.Data := Node;
  for i := 0 to node.NodeCount - 1 do
    if node.Nodes[i].Childs.Count > 0 then
    begin
      ClassNode := TreeView.Items.AddChild(Parent, node.Nodes[i].ClassID.NodeClass
      + format(': %s(%d)', [node.Nodes[i].ClassID.ClassName, node.Nodes[i].Childs.Count]));
      for j := 0 to node.Nodes[i].Childs.Count - 1 do
        ShowNodes(TMetaNode(node.Nodes[i].Childs[j]), ClassNode);
    end;
end;

procedure TMainForm.OpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    DataBase.Connected := false;
    TreeView.Items.Clear;
    DataBase.DatabaseName := OpenDialog.FileName;
    MetaData.LoadFromDatabase(Transaction);
  end;

  ShowNodes(MetaData, nil);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MetaData := TMetaDataBase.Create(nil, 0);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MetaData.Free;
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  If (node.Data <> nil) then
    memo.Lines.Text := TMetaNode(Node.Data).AsDDL else
    memo.Lines.Text := '';
end;

procedure TMainForm.SaveToFile1Click(Sender: TObject);
var FileStream: TFileStream;
begin
  if SaveDialog.Execute then
  begin
    FileStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
    try
      MetaData.SaveToStream(FileStream);
    finally
      FileStream.Free;
    end;
  end;
end;

procedure TMainForm.LoadFromFile1Click(Sender: TObject);
var FileStream: TFileStream;
begin
  OpenDialog.FileName := SaveDialog.FileName;
  if OpenDialog.Execute then
  begin
    TreeView.Items.Clear;
    FileStream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    try
      MetaData.Free;
      MetaData := TMetaDataBase.CreateFromStream(nil, -1, FileStream);
      ShowNodes(MetaData, nil);
    finally
      FileStream.Free;
    end;
  end;
end;

end.


