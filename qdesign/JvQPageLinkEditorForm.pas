{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageLinkEditorForm.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
Changes:
2002-10-22:
  changed the way a parent/child PageIndex is assigned so that it matches the
  actual component using this editor
  
$Id$
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQPageLinkEditorForm;

interface

uses
  Classes, SysUtils,  
  QForms, QControls, QStdCtrls, QExtCtrls, QComCtrls,
  QActnList, QMenus,  
  DesignEditors, Variants, DesignIntf, 
  JvQPageList, JvQPageListTreeView, JvQComponent;

type
  { a property editor for the PageLinks property of TJvCustomPageListTreeView}
  TJvPageLinksProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TfrmJvTreeViewLinksEditor = class(TJvForm)
    tvItems: TTreeView;
    btnLink: TButton;
    lbPages: TListBox;
    acMain: TActionList;
    acLink: TAction;
    Label1: TLabel;
    Label2: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    pnlLeft: TPanel;
    pnlMid: TPanel;
    pnlRight: TPanel;
    popTree: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure acLinkExecute(Sender: TObject);
    procedure acMainUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure FormResize(Sender: TObject);
  private
    FTreeView:TCustomTreeView;
    function CreatePopUpItem(Index: Integer): TMenuItem;
    procedure DoPopClick(Sender: TObject);
    procedure AssignComponents(TreeView: TCustomTreeView; const PageList: IPageList);
    procedure AssignToComponents(TreeView: TCustomTreeView; const PageList: IPageList);
  public
    class function Edit(TreeView: TCustomTreeView; const PageList: IPageList): Boolean;
  end;

procedure ShowPageLinkEditor(TreeView: TJvCustomPageListTreeView);

implementation

uses
  JvQDsgnConsts;



{$R *.xfm}


type
  THackTreeView = class(TJvCustomPageListTreeView);

procedure ShowPageLinkEditor(TreeView: TJvCustomPageListTreeView);
begin
  if TfrmJvTreeViewLinksEditor.Edit(TreeView,  TreeView.PageList ) and
    (THackTreeView(TreeView).Items.Count > 0) then
    THackTreeView(TreeView).Items.GetFirstNode.Expand(False);
end;

//=== TJvPageLinksProperty ===================================================

procedure TJvPageLinksProperty.Edit;
begin
  ShowPageLinkEditor(GetComponent(0) as TJvCustomPageListTreeView);
end;

function TJvPageLinksProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

//=== TfrmJvTreeViewLinksEditor ==============================================

function GetStrippedText(const AText: string): string;
var
  I: Integer;
begin
  Result := AText;
  for I := Length(AText) downto 1 do
    if AText[I] = '(' then
    begin
      Result := Copy(AText, 1, I - 2);
      Break;
    end;
end;

function GetNewTreeText(const AText: string; Index: Integer): string;
begin
  Result := Format('%s (%d)', [GetStrippedText(AText), Index])
end;

procedure TfrmJvTreeViewLinksEditor.DoPopClick(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    lbPages.ItemIndex := Tag;
    if acLink.Enabled then
      acLink.Execute;
  end;
end;

function TfrmJvTreeViewLinksEditor.CreatePopUpItem(Index: Integer): TMenuItem;
begin
  Result := TMenuItem.Create(popTree);
  Result.Caption := Format(RsCreateLinkToPaged, [Index]);
  Result.Tag := Index;
  if Index < 10 then
    Result.ShortCut := ShortCut(Ord('0') + Index, [ssCtrl])
  else
  if (Index >= 10) and (Index < 36) then
    Result.ShortCut := ShortCut(Ord('A') + Index - 10, [ssCtrl]);
  Result.OnClick := DoPopClick;
end;

procedure TfrmJvTreeViewLinksEditor.AssignComponents(TreeView: TCustomTreeView;
  const PageList: IPageList);
var
  N1: TJvPageIndexNode;
  N2: TTreeNode;
  I: Integer;
begin
  tvItems.Items.Clear;
  FTreeView := TreeView;
  if TreeView <> nil then
  begin
    tvItems.Items.Assign(THackTreeView(TreeView).Items);
    tvItems.ShowButtons := True; // THackTreeView(TreeView).ShowButtons;
  end;
  if TreeView is TJvCustomPageListTreeView then
  begin
    N1 := THackTreeView(TreeView).Items.GetFirstNode as TJvPageIndexNode;
    N2 := tvItems.Items.GetFirstNode;
    while Assigned(N1) and Assigned(N2) do
    begin
      N2.Data := Pointer(N1.PageIndex);
      N2.Text := Format('%s (%d)', [N1.Text, N1.PageIndex]);
      N1 := TJvPageIndexNode(N1.GetNext);
      N2 := N2.GetNext;
    end;
  end;

  lbPages.Items.Clear;
  popTree.Items.Clear;
  if PageList <> nil then
    for I := 0 to PageList.GetPageCount - 1 do
    begin
      lbPages.Items.Add(Format('%s (%d)', [PageList.GetPageCaption(I), I]));
      popTree.Items.Add(CreatePopUpItem(I));
    end;

  if tvItems.Items.Count > 0 then
  begin
    tvItems.Items[0].Selected := True;
    tvItems.Items[0].Expand(False);
  end;
end;

procedure TfrmJvTreeViewLinksEditor.AssignToComponents(TreeView: TCustomTreeView;
  const PageList: IPageList);
var
  N1: TJvPageIndexNode;
  N2: TTreeNode;
begin
  if TreeView <> nil then
    THackTreeView(TreeView).Items.Assign(tvItems.Items);
  if TreeView is TJvCustomPageListTreeView then
  begin
    N1 := THackTreeView(TreeView).Items.GetFirstNode as TJvPageIndexNode;
    N2 := tvItems.Items.GetFirstNode;
    while Assigned(N1) and Assigned(N2) do
    begin
      N1.PageIndex := Integer(N2.Data);
      N1.Text := GetStrippedText(N2.Text);
      N1 := TJvPageIndexNode(N1.GetNext);
      N2 := N2.GetNext;
    end;
  end;
end;

procedure TfrmJvTreeViewLinksEditor.tvItemsChange(Sender: TObject;
  Node: TTreeNode);
begin
  if Node <> nil then
    if Node is TJvPageIndexNode then
      lbPages.ItemIndex := TJvPageIndexNode(Node).PageIndex
    else
      lbPages.ItemIndex := Integer(Node.Data);
end;

procedure TfrmJvTreeViewLinksEditor.acLinkExecute(Sender: TObject);
var
  N: TTreeNode;
begin
  N := tvItems.Selected;
  N.Data := Pointer(lbPages.ItemIndex);
  if FTreeView is TJvCustomSettingsTreeView then
  begin
    // make the editor behave like the component
    if (N.Parent <> nil) and (N.Parent.GetFirstChild = N) then
    begin
      N.Parent.Data := Pointer(lbPages.ItemIndex);
      N.Parent.Text := GetNewTreeText(N.Parent.Text, lbPages.ItemIndex);
    end
    else
    if N.HasChildren then
    begin
      N.GetFirstChild.Data := Pointer(lbPages.ItemIndex);
      N.GetFirstChild.Text := GetNewTreeText(N.GetFirstChild.Text, lbPages.ItemIndex);
    end;
  end;
  if N is TJvPageIndexNode then
    TJvPageIndexNode(N).PageIndex := lbPages.ItemIndex;
  N.Text := GetNewTreeText(N.Text, lbPages.ItemIndex);
end;

procedure TfrmJvTreeViewLinksEditor.acMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acLink.Enabled := (tvItems.Selected <> nil) and (lbPages.ItemIndex > -1);
end;

procedure TfrmJvTreeViewLinksEditor.FormResize(Sender: TObject);
begin
  pnlLeft.Width := (ClientWidth - pnlMid.Width) div 2;
end;

class function TfrmJvTreeViewLinksEditor.Edit(TreeView: TCustomTreeView;
  const PageList: IPageList): Boolean;
begin
  // keep in mind that Self is a class here not an object
  with Self.Create(Application) do
  try
    AssignComponents(TreeView, PageList);
    Result := ShowModal = mrOk;
    if Result then
      AssignToComponents(TreeView, PageList);
  finally
    Free;
  end;
end;

end.

