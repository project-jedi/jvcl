{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTreeItemsEditorForm.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

Changes:
2002-10-22:
  Drawing of State images differs from normal images since they are 1-based:
  the 0-th item for the state imagelist isn't drawn to alert the user to this fact
-----------------------------------------------------------------------------}
// $Id$

unit JvTreeItemsEditorForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  DesignEditors, DesignIntf,
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, ComCtrls, Menus, StdActns;

type
  TJvTreeItemsProperty = class(TClassProperty)
    function GetAttributes: TPropertyAttributes; override;
  public
    procedure Edit; override;
  end;

  TJvTreeViewEditor = class(TComponentEditor)
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TJvPageTreeViewEditor = class(TJvTreeViewEditor)
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TfrmTreeViewItems = class(TForm)
    Panel1: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    tvItems: TTreeView;
    Splitter1: TSplitter;
    Panel2: TPanel;
    btnNew: TButton;
    btnNewSub: TButton;
    btnDelete: TButton;
    acItems: TActionList;
    acNewItem: TAction;
    acNewSubItem: TAction;
    acDelete: TAction;
    Bevel1: TBevel;
    gbProperties: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    cbImage: TComboBox;
    cbSelected: TComboBox;
    cbState: TComboBox;
    edNodeText: TEdit;
    PopupMenu1: TPopupMenu;
    acNodeMoveLeft: TAction;
    acNodeMoveRight: TAction;
    acNodeMoveUp: TAction;
    acNodeMoveDown: TAction;
    Movedown1: TMenuItem;
    Moveup1: TMenuItem;
    N1: TMenuItem;
    Moveleft1: TMenuItem;
    Moveright1: TMenuItem;
    acLoadFromFile: TAction;
    acSaveToFile: TAction;
    LoadFromFile1: TMenuItem;
    SaveToFile1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    CheckBoxEnabled: TCheckBox;
    lblExpandedImageIndex: TLabel;
    cbExpandedImageIndex: TComboBox;
    procedure edNodeTextChange(Sender: TObject);
    procedure acNewItemExecute(Sender: TObject);
    procedure acNewSubItemExecute(Sender: TObject);
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure acFileOpenAccept(Sender: TObject);
    procedure acFileSaveAsAccept(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acItemsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure cbImageIndexDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbStateDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure acNodeMoveUpExecute(Sender: TObject);
    procedure acNodeMoveDownExecute(Sender: TObject);
    procedure acNodeMoveRightExecute(Sender: TObject);
    procedure acNodeMoveLeftExecute(Sender: TObject);
    procedure tvItemsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tvItemsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure acLoadFromFileExecute(Sender: TObject);
    procedure acSaveToFileExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDragNode: TTreeNode;
    FTreeView: TCustomTreeView;
    FNodeDisabledList: TList;
    procedure RecreateTreeView(ATreeViewClass: TComponentClass);
    function InternEdit(ATreeView: TCustomTreeView): Boolean;
    procedure BuildNodeDisabledList;
    procedure ApplyNodeDisabledList;
  public
    class function Edit(ATreeView: TCustomTreeView): Boolean;
  end;

  TGroupBox = class(StdCtrls.TGroupBox)
  private
    FPropagateEnabled: Boolean;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property PropagateEnabled: Boolean read FPropagateEnabled write FPropagateEnabled default True;
  end;

implementation

uses
  ImgList,
  JvPageListTreeView, JvPageLinkEditorForm, JvDsgnConsts, JclBase;

{$R *.dfm}

type
  THackTreeView = class(TCustomTreeView);

//=== { TGroupBox } ==========================================================

constructor TGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PropagateEnabled := True;
end;

procedure TGroupBox.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if PropagateEnabled then
    Broadcast(Msg);
end;

//=== { TJvTreeItemsProperty } ===============================================

procedure TJvTreeItemsProperty.Edit;
begin
  if TfrmTreeViewItems.Edit(TCustomTreeView(GetComponent(0))) then
    Modified;
end;

function TJvTreeItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

//=== { TJvTreeViewEditor } ==================================================

procedure TJvTreeViewEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TJvTreeViewEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
  begin
    if TfrmTreeViewItems.Edit(TCustomTreeView(Component)) then
      if Designer <> nil then
        Designer.Modified;
  end
  else
    inherited ExecuteVerb(Index);
end;

function TJvTreeViewEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := RsItemsEditorEllipsis
  else
    Result := '';
end;

function TJvTreeViewEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== { TJvPageTreeViewEditor } ==============================================

procedure TJvPageTreeViewEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 1 then
  begin
    if ShowPageLinkEditor(TJvCustomPageListTreeView(Component)) then
      if Designer <> nil then
        Designer.Modified;
  end
  else
    inherited ExecuteVerb(Index);
end;

function TJvPageTreeViewEditor.GetVerb(Index: Integer): string;
begin
  if Index = 1 then
    Result := RsLinksEditorEllipsis
  else
    Result := inherited GetVerb(Index);
end;

function TJvPageTreeViewEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

//=== { TfrmTreeViewItems } ==================================================

procedure TfrmTreeViewItems.edNodeTextChange(Sender: TObject);
begin
  if tvItems.Selected <> nil then
  begin
    tvItems.Selected.Text := edNodeText.Text;
    tvItems.Selected.ImageIndex :=
      StrToIntDef(cbImage.Text, tvItems.Selected.ImageIndex);
    tvItems.Selected.SelectedIndex :=
      StrToIntDef(cbSelected.Text, tvItems.Selected.SelectedIndex);
    tvItems.Selected.StateIndex :=
      StrToIntDef(cbState.Text, tvItems.Selected.StateIndex);
    {$IFDEF RTL200_UP} // Delphi 2009+
    tvItems.Selected.ExpandedImageIndex :=
      StrToIntDef(cbExpandedImageIndex.Text, tvItems.Selected.ExpandedImageIndex);
    if FNodeDisabledList.IndexOf(tvItems.Selected) = -1 then
    begin
      if not CheckBoxEnabled.Checked then
        FNodeDisabledList.Add(tvItems.Selected);
    end
    else if CheckBoxEnabled.Checked then
      FNodeDisabledList.Remove(tvItems.Selected);
    {$ENDIF RTL200_UP}
  end;
end;

procedure TfrmTreeViewItems.acNewItemExecute(Sender: TObject);
begin
  with tvItems.Items.Add(tvItems.Selected, '') do
  begin
    MakeVisible;
    Selected := True;
  end;
  gbProperties.Enabled := True;
  edNodeText.SetFocus;
end;

procedure TfrmTreeViewItems.acNewSubItemExecute(Sender: TObject);
begin
  with tvItems.Items.AddChild(tvItems.Selected, '') do
  begin
    MakeVisible;
    Selected := True;
  end;
  gbProperties.Enabled := True;
  edNodeText.SetFocus;
end;

function DoStringCompare(SL: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrToIntDef(SL[Index2], Index2) - StrToIntDef(SL[Index1], Index1);
end;

procedure TfrmTreeViewItems.tvItemsChange(Sender: TObject; Node: TTreeNode);

  function AddCB(CB: TComboBox; AItemIndex: Integer): Integer;
  var
    S: string;
    SL: TStringList;
  begin
    S := IntToStr(AItemIndex);
    Result := CB.Items.IndexOf(S);
    if Result < 0 then
    begin
      SL := TStringList.Create;
      try
        SL.Assign(CB.Items);
        SL.Add(S);
        SL.CustomSort(DoStringCompare);
        CB.Items.Assign(SL);
        Result := SL.IndexOf(S);
      finally
        SL.Free;
      end;
    end;
  end;

begin
  if Node <> nil then
  begin
    edNodeText.OnChange := nil;
    edNodeText.Text := Node.Text;
    cbImage.ItemIndex := AddCB(cbImage, Node.ImageIndex);
    cbSelected.ItemIndex := AddCB(cbSelected, Node.SelectedIndex);
    cbState.ItemIndex := AddCB(cbState, Node.StateIndex);
    {$IFDEF RTL200_UP} // Delphi 2009+
    cbExpandedImageIndex.ItemIndex := AddCB(cbExpandedImageIndex, Node.ExpandedImageIndex);
    CheckBoxEnabled.Checked := FNodeDisabledList.IndexOf(Node) = -1;
    {$ENDIF RTL200_UP}
    edNodeText.OnChange := edNodeTextChange;
  end;
  gbProperties.Enabled := tvItems.Selected <> nil;
end;

procedure TfrmTreeViewItems.acFileOpenAccept(Sender: TObject);
begin
//  tvItems.LoadFromFile(acFileOpen.Dialog.FileName);
  tvItems.FullExpand;
end;

procedure TfrmTreeViewItems.acFileSaveAsAccept(Sender: TObject);
begin
//  tvItems.SaveToFile(acFileSaveAs.Dialog.FileName);
end;

procedure TfrmTreeViewItems.acDeleteExecute(Sender: TObject);
var
  N: TTreeNode;
begin
  edNodeText.OnChange := nil;
  tvItems.OnChange := nil;
  try
    N := tvItems.Selected.GetPrev;
    if N = nil then
      N := tvItems.Selected.GetNext;
    FNodeDisabledList.Remove(tvItems.Selected);
    tvItems.Selected.Delete;
    edNodeText.Text := '';
    if N <> nil then
    begin
      N.MakeVisible;
      N.Selected := True;
    end;
  finally
    edNodeText.OnChange := edNodeTextChange;
    tvItems.OnChange := tvItemsChange;
  end;
end;

procedure TfrmTreeViewItems.acItemsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  bSel: Boolean;
  nSel: TTreeNode;
begin
  bSel := tvItems.Selected <> nil;
  nSel := tvItems.Selected;
  acNewSubItem.Enabled := bSel;
  acDelete.Enabled := bSel;
  acNodeMoveLeft.Enabled := bSel and (nSel.Parent <> nil);
  acNodeMoveRight.Enabled := bSel and (nSel.getPrevSibling <> nil);
  acNodeMoveUp.Enabled := bSel and ((nSel.getPrevSibling <> nil) or (nSel.Parent <> nil));
  acNodeMoveDown.Enabled := bSel and ((nSel.getNextSibling <> nil) or (nSel.Parent <> nil));
end;

procedure TfrmTreeViewItems.RecreateTreeView(ATreeViewClass: TComponentClass);
var
  R: TRect;
begin
  // Replace tvItems with the same treeview class so that Assign() calls copy all data
  // Needed for JvPageListTreeView
  R := tvItems.BoundsRect;
  tvItems.Free;
  tvItems := TTreeView(TComponentClass(ATreeViewClass).Create(Self) as TCustomTreeView);
  tvItems.BoundsRect := R;
  tvItems.Align := alLeft;
  tvItems.Parent := Self;
  tvItems.DragMode := dmAutomatic;
  tvItems.HideSelection := False;
  tvItems.Indent := 19;
  tvItems.PopupMenu := PopupMenu1;
  tvItems.ReadOnly := True;
  tvItems.TabOrder := 1;
  tvItems.OnChange := tvItemsChange;
  tvItems.OnDragDrop := tvItemsDragDrop;
  tvItems.OnDragOver := tvItemsDragOver;
  tvItems.OnStartDrag := tvItemsStartDrag;

  Splitter1.Left := tvItems.Left + tvItems.Width;
end;

class function TfrmTreeViewItems.Edit(ATreeView: TCustomTreeView): Boolean;
var
  F: TfrmTreeViewItems;
begin
  // keep in mind that Self is class here not object
  F := Self.Create(Application);
  try
    Result := F.InternEdit(ATreeView);
  finally
    F.Free;
  end;
end;

function TfrmTreeViewItems.InternEdit(ATreeView: TCustomTreeView): Boolean;
const
  cNegItem = '-1';
var
  IL: TCustomImageList;
  I: Integer;
begin
  FTreeView := ATreeView;
  FNodeDisabledList := TList.Create;
  try
    RecreateTreeView(TComponentClass(FTreeView.ClassType));
    tvItems.Items.Assign(THackTreeView(FTreeView).Items);

    {$IFNDEF RTL200_UP} // Delphi 2009+
    // Hide not supported controls
    cbExpandedImageIndex.Visible := False;
    lblExpandedImageIndex.Visible := False;
    CheckBoxEnabled.Visible := False;
    {$ENDIF ~RTL200_UP}

    IL := THackTreeView(FTreeView).Images;
    if IL <> nil then
    begin
      cbImage.Style := csOwnerDrawFixed;
      cbSelected.Style := csOwnerDrawFixed;
      cbImage.ItemHeight := IL.Height;
      cbSelected.ItemHeight := IL.Height;
      cbImage.Items.Add(cNegItem);
      cbSelected.Items.Add(cNegItem);
      for I := 0 to IL.Count - 1 do
      begin
        cbImage.Items.Add(IntToStr(I));
        cbSelected.Items.Add(IntToStr(I));
      end;
      cbImage.Tag := SizeInt(IL);
      cbSelected.Tag := SizeInt(IL);
      {$IFDEF RTL200_UP} // Delphi 2009+
      cbExpandedImageIndex.Style := csOwnerDrawFixed;
      cbExpandedImageIndex.ItemHeight := IL.Height;
      cbExpandedImageIndex.Tag := SizeInt(IL);
      {$ENDIF RTL200_UP}
    end;
    IL := THackTreeView(FTreeView).StateImages;
    if IL <> nil then
    begin
      cbState.Style := csOwnerDrawFixed;
      cbState.ItemHeight := IL.Height;
      cbState.Items.Add(cNegItem);
      for I := 0 to IL.Count - 1 do
        cbState.Items.Add(IntToStr(I));
      cbState.Tag := SizeInt(IL);
    end;
    cbImage.ItemIndex := 0;
    cbSelected.ItemIndex := 0;
    cbState.ItemIndex := 0;
    {$IFDEF RTL200_UP} // Delphi 2009+
    cbExpandedImageIndex.ItemIndex := 0;
    CheckBoxEnabled.Checked := True;
    {$ENDIF RTL200_UP}
    tvItems.FullExpand;
    if tvItems.Items.Count > 0 then
      tvItems.Items.GetFirstNode.MakeVisible;
    Result := ShowModal = mrOk;
    if Result then
    begin
      ApplyNodeDisabledList;
      THackTreeView(FTreeView).Items.Assign(tvItems.Items);
    end;
  finally
    FNodeDisabledList.Free;
  end;
end;

procedure TfrmTreeViewItems.BuildNodeDisabledList;
{$IFDEF RTL200_UP} // Delphi 2009+
var
  Node: TTreeNode;
{$ENDIF RTL200_UP}
begin
  {$IFDEF RTL200_UP} // Delphi 2009+
  Node := tvItems.Items.GetFirstNode;
  while Node <> nil do
  begin
    if not Node.Enabled then
    begin
      FNodeDisabledList.Add(Node);
      Node.Enabled := True;
    end;
    Node := Node.GetNext;
  end;
  {$ENDIF RTL200_UP}
end;

procedure TfrmTreeViewItems.ApplyNodeDisabledList;
{$IFDEF RTL200_UP} // Delphi 2009+
var
  I: Integer;
{$ENDIF RTL200_UP}
begin
  {$IFDEF RTL200_UP} // Delphi 2009+
  for I := 0 to FNodeDisabledList.Count - 1 do
    TTreeNode(FNodeDisabledList[I]).Enabled := False;
  {$ENDIF RTL200_UP}
end;

procedure TfrmTreeViewItems.cbImageIndexDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  DrawOffset, DrawIndex: Integer;
  IL: TImageList;
  CB: TComboBox;
begin
  CB := TComboBox(Control);
  IL := TImageList(CB.Tag);
  DrawIndex := Index - 1;
  DrawOffset := 2;
  CB.Canvas.FillRect(Rect);
  if IL <> nil then
  begin
    IL.Draw(CB.Canvas, Rect.Left + 2, Rect.Top, DrawIndex);
    DrawOffset := IL.Width + 2;
  end;
  Rect.Left := Rect.Left + DrawOffset;
  DrawText(CB.Canvas.Handle, PChar(Format('%d', [DrawIndex])), -1, Rect,
    DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
end;

procedure TfrmTreeViewItems.cbStateDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  DrawOffset, DrawIndex: Integer;
  IL: TImageList;
  CB: TComboBox;
begin
  CB := TComboBox(Control);
  IL := TImageList(CB.Tag);
  DrawIndex := Index - 1;
  DrawOffset := 2;
  CB.Canvas.FillRect(Rect);
  // state images are *one*-based, so we don't draw the 0'th image
  // to remind the user about this fact...
  if IL <> nil then
  begin
    if DrawIndex > 0 then
      IL.Draw(CB.Canvas, Rect.Left + 2, Rect.Top, DrawIndex);
    DrawOffset := IL.Width + 2;
  end;
  Rect.Left := Rect.Left + DrawOffset;
  DrawText(CB.Canvas.Handle, PChar(Format('%d', [DrawIndex])), -1, Rect,
    DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
end;

procedure TfrmTreeViewItems.acNodeMoveUpExecute(Sender: TObject);
begin
  with tvItems.Selected do
  begin
    if getPrevSibling <> nil then
      MoveTo(getPrevSibling, naInsert)
    else
    if Parent <> nil then
      MoveTo(Parent, naInsert);
  end;
  tvItems.FullExpand;
end;

procedure TfrmTreeViewItems.acNodeMoveDownExecute(Sender: TObject);
var N:TTreeNode;
begin
  with tvItems.Selected do
  begin
    if getNextSibling <> nil then
      getNextSibling.MoveTo(tvItems.Selected, naInsert)
    else
    if Parent <> nil then
    begin
      N := Parent;
      MoveTo(Parent, naInsert);
      N.MoveTo(tvItems.Selected, naInsert);
    end;
  end;
  tvItems.FullExpand;
end;

procedure TfrmTreeViewItems.acNodeMoveRightExecute(Sender: TObject);
begin
  with tvItems.Selected do
    MoveTo(getPrevSibling, naAddChild);
  tvItems.FullExpand;
end;

procedure TfrmTreeViewItems.acNodeMoveLeftExecute(Sender: TObject);
var N:TTreeNode;
begin
  with tvItems.Selected do
  begin
    N := Parent;
    MoveTo(Parent, naInsert);
    N.MoveTo(tvItems.Selected, naInsert);
  end;
  tvItems.FullExpand;
end;

procedure TfrmTreeViewItems.tvItemsStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragNode := tvItems.Selected;
end;

procedure TfrmTreeViewItems.tvItemsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  N: TTreeNode;
begin
  N := tvItems.GetNodeAt(X,Y);
  Accept := (Source = Sender) and (FDragNode <> nil) and (N <> FDragNode);
  if (N <> nil) and N.HasChildren and not N.Expanded then
    N.Expand(False);
end;

procedure TfrmTreeViewItems.tvItemsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  N: TTreeNode;
begin
  if FDragNode = nil then
    Exit;
  N := tvItems.GetNodeAt(X,Y);
  if N = nil then
    FDragNode.MoveTo(tvItems.Items.GetFirstNode, naAdd)
  else
    FDragNode.MoveTo(N, naAddChildFirst);
  FDragNode := nil;
  tvItems.FullExpand;
end;

procedure TfrmTreeViewItems.acLoadFromFileExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    tvItems.LoadFromFile(OpenDialog1.Filename);
end;

procedure TfrmTreeViewItems.acSaveToFileExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
    tvItems.SaveToFile(SaveDialog1.Filename);
end;

procedure TfrmTreeViewItems.FormShow(Sender: TObject);
begin
  // Build the disabled list in OnShow because ShowModal will recreate the
  // treeview control what makes the references in the list invalid.
  BuildNodeDisabledList;
end;

end.
