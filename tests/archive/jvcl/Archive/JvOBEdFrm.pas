{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOBEdFrm.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ Form used by the TCustomOutlookBar components property editors }

unit JvOBEdFrm;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, JvOLBar, ActnList, Menus;

type
  TfrmOLBarEditFrm = class(TForm)
    GroupBox1: TGroupBox;
    tvOLBar: TTreeView;
    btnNewPage: TButton;
    btnNewButton: TButton;
    btnDelete: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    acPages: TActionList;
    acNewPage: TAction;
    acNewButton: TAction;
    acDelete: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    acApply: TAction;
    cbImages: TComboBox;
    Label1: TLabel;
    popPages: TPopupMenu;
    NewPage1: TMenuItem;
    NewButton1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    edCaption: TEdit;
    Label2: TLabel;
    procedure tvOLBarChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure tvOLBarChange(Sender: TObject; Node: TTreeNode);
    procedure cbImagesMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure cbImagesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure acPagesUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acNewPageExecute(Sender: TObject);
    procedure edCaptionChange(Sender: TObject);
    procedure cbImagesChange(Sender: TObject);
    procedure tvOLBarKeyPress(Sender: TObject; var Key: Char);
    procedure edCaptionKeyPress(Sender: TObject; var Key: Char);
    procedure acNewButtonExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acApplyExecute(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure tvOLBarCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvOLBarAddition(Sender: TObject; Node: TTreeNode);
    procedure tvOLBarDeletion(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    FOLBar: TJvCustomOutlookBar;
    FModified: boolean;
    procedure SetOLBar(const Value: TJvCustomOutlookBar);
    procedure FillImageCombo;
  public
    { Public declarations }
    class function Edit(AOLBar: TJvCustomOutlookBar): boolean;
    property OLBar: TJvCustomOutlookBar read FOLBar write SetOLBar;
  end;


implementation

{$R *.dfm}

{ TfrmOLBarEditFrm }
type
  THOB = class(TJvCustomOutlookBar);

class function TfrmOLBarEditFrm.Edit(AOLBar: TJvCustomOutlookBar): boolean;
var
  frmOLBarEditFrm: TfrmOLBarEditFrm;
begin
  frmOLBarEditFrm := self.Create(Application);
  with frmOLBarEditFrm do
  try
    OLBar := AOLBar;
    Result := ShowModal = mrOK;
  finally
    Free;
  end;
end;

procedure TfrmOLBarEditFrm.FillImageCombo;
var i: integer;
begin
  cbImages.Items.Clear;
  if Assigned(tvOLBar.Images) then
  begin
    cbImages.Items.AddObject('(none)', nil);
    for i := 1 to tvOLBar.Images.Count do
      cbImages.Items.AddObject(Format('%u', [i - 1]), TObject(i))
  end
  else
    for i := -1 to 10 do
      cbImages.Items.Add(Format('%d', [i]));
  cbImages.ItemIndex := 0;
end;

procedure TfrmOLBarEditFrm.SetOLBar(const Value: TJvCustomOutlookBar);
var i, j: integer; FH: THOB; selNode, N, N2: TTreeNode;
begin
  FOLBar := Value;
  SelNode := nil;
  if not Assigned(FOLBar) then Exit;
  FH := THOB(FOlBar);
  tvOLBar.Items.BeginUpdate;
  try
    tvOLBar.Items.Clear;
    if FH.ButtonSize = olbsLarge then
      tvOLBar.Images := FH.LargeImages
    else
      tvOLBar.Images := FH.SmallImages;
    if Assigned(tvOLBar.Images) then
    begin
      cbImages.Style := csOwnerDrawVariable;
//      cbImages.Height := tvOLBar.Images.Height;
    end
    else
      cbImages.Style := csDropDown;
    FillImageCombo;
    for i := 0 to FH.Pages.Count - 1 do
    begin
      N := tvOLBar.Items.AddChild(nil, FH.Pages[i].Caption);
      if FH.ActivePageIndex = i then
        selNode := N;
      N.ImageIndex := -1;
      N.SelectedIndex := -1;
      for j := 0 to FH.Pages[i].Buttons.Count - 1 do
      begin
        N2 := tvOLBar.Items.AddChild(N, FH.Pages[i].Buttons[j].Caption);
        N2.ImageIndex := FH.Pages[i].Buttons[j].ImageIndex;
        N2.SelectedIndex := N2.ImageIndex;
      end;
    end;
    if tvOLBar.Items.Count > 0 then
    begin
      tvOLBar.FullExpand;
      if SelNode = nil then
        tvOLBar.Selected := tvOLBar.Items[0]
      else
        tvOLBar.Selected := SelNode;
      tvOLBar.Selected.MakeVisible;
      tvOLBar.Selected.Focused := true;
    end;
  finally
    tvOLBar.Items.EndUpdate;
  end;
  FModified := false;
end;

procedure TfrmOLBarEditFrm.tvOLBarChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
var i: integer;
begin
  // save settings
  if tvOLBar.Selected <> nil then
  begin
    if (tvOLBar.Images = nil) then
      i := StrToIntDef(cbImages.Text, tvOLBar.Selected.ImageIndex)
    else if cbImages.ItemIndex > -1 then
      i := cbImages.ItemIndex - 1
    else
      i := -1; // StrToIntDef(cbImages.Text,tvOLBar.Selected.ImageIndex);
    tvOLBar.Selected.ImageIndex := i;
    tvOLBar.Selected.SelectedIndex := i;
    tvOLBar.Selected.Text := edCaption.Text;
  end;
end;

procedure TfrmOLBarEditFrm.tvOLBarChange(Sender: TObject; Node: TTreeNode);
begin
  // load settings
  if Node <> nil then
  begin
    edCaption.Text := Node.Text;
    if Node.Parent <> nil then
    begin
      if Node.ImageIndex > -1 then
      begin
        if tvOLBar.Images <> nil then
          cbImages.ItemIndex := Node.ImageIndex + 1
        else
          cbImages.Text := IntToStr(Node.ImageIndex);
      end
      else
        cbImages.ItemIndex := 0
    end
    else
      cbImages.ItemIndex := 0;
  end;
end;

procedure TfrmOLBarEditFrm.cbImagesMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  if Assigned(tvOLBar.Images) then
    Height := tvOLBar.Images.Height + 2;
end;

procedure TfrmOLBarEditFrm.cbImagesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var R: TRect; i: integer;
begin
  with cbImages do
  begin
    R := Rect;
    Canvas.FillRect(R);
    Rect.Right := Rect.Left + tvOLBar.Images.Width + 4;
    R.Left := Rect.Right;
    i := integer(Items.Objects[Index]);
    if i > 0 then
      tvOLBar.Images.Draw(Canvas, Rect.Left, Rect.Top, i - 1);
    DrawText(Canvas.Handle, PChar(Items[Index]), -1, R, DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TfrmOLBarEditFrm.acPagesUpdate(Action: TBasicAction;
  var Handled: Boolean);
var N: TTreeNode;
begin
  N := tvOLBar.Selected;
  cbImages.Enabled := (N <> nil) and (N.Parent <> nil);
  if not cbImages.Enabled then
    cbImages.ItemIndex := -1;
  acApply.Enabled := FModified;
  acNewButton.Enabled := (N <> nil);
  acDelete.Enabled := (N <> nil);

  acMoveUp.Enabled := (N <> nil) and (N.Parent <> nil) and (N.getPrevSibling <> nil);
  acMoveDown.Enabled := (N <> nil) and (N.Parent <> nil) and (N.getNextSibling <> nil);
end;

procedure TfrmOLBarEditFrm.acNewPageExecute(Sender: TObject);
var N: TTreeNode;
begin
  N := tvOLBar.Items.Add(nil, '');
  N.ImageIndex := -1;
  N.SelectedIndex := -1;
  N.MakeVisible;
  N.Selected := true;
  N.Focused := true;
  tvOlbar.SetFocus;
  FModified := true;
//  edCaption.SetFocus;
end;

procedure TfrmOLBarEditFrm.edCaptionChange(Sender: TObject);
begin
  if tvOLBar.Selected <> nil then
  begin
    tvOLBar.Selected.Text := edCaption.Text;
    FModified := true;
  end;
end;

procedure TfrmOLBarEditFrm.cbImagesChange(Sender: TObject);
var i: integer;
begin
  if (tvOLBar.Selected <> nil) and cbImages.Enabled then
  begin
    if cbImages.Style = csDropDown then
      i := StrToIntDef(cbImages.Text, tvOLBar.Selected.ImageIndex)
    else if cbImages.ItemIndex > -1 then
      i := cbImages.ItemIndex - 1
    else
      i := -1;
    FModified := true;
    tvOLBar.Selected.ImageIndex := i;
    tvOLBar.Selected.SelectedIndex := i;
    tvOLBar.Refresh;
  end;
end;

procedure TfrmOLBarEditFrm.tvOLBarKeyPress(Sender: TObject; var Key: Char);
begin
  if Key > #31 then
    edCaption.Text := {edCaption.Text + } Key;
  Key := #0;
  edCaption.SetFocus;
  edCaption.SelStart := Length(edCaption.Text);
  edCaption.SelLength := 0;
end;

procedure TfrmOLBarEditFrm.edCaptionKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    tvOLBar.SetFocus;
    Key := #0;
  end;
end;

procedure TfrmOLBarEditFrm.acNewButtonExecute(Sender: TObject);
var N: TTreeNode;
begin
  N := tvOLBar.Selected;
  if N.Parent <> nil then
    N := N.Parent;
  N := tvOLBar.Items.AddChild(N, '');
  N.MakeVisible;
  N.Selected := true;
  N.Focused := true;
  tvOlbar.SetFocus;
  FModified := true;
//  edCaption.SetFocus;
end;

procedure TfrmOLBarEditFrm.acDeleteExecute(Sender: TObject);
var N: TTreeNode;
begin
  if tvOLBar.Selected <> nil then
  begin
    N := tvOLBar.Selected.getPrev;
    if not Assigned(N) then
      N := tvOLBar.Selected.getNext;
    tvOLBar.Selected.Delete;
    FModified := true;
    if Assigned(N) then
    begin
      N.MakeVisible;
      N.Selected := true;
      N.Focused := true;
      tvOlbar.SetFocus;
    end
  end;
end;

procedure TfrmOLBarEditFrm.acApplyExecute(Sender: TObject);
var FH: THOB; N, N2: TTreeNode; P: TJvOutlookBarPage; B: TJvOutlookBarButton;
begin
  FH := THOB(FOLBar);
  N := tvOLBar.Items.GetFirstNode;
  FH.Pages.BeginUpdate;
  try
    FH.Pages.Clear;
    while Assigned(N) do
    begin
      P := FH.Pages.Add;
      P.Caption := N.Text;
      N2 := N.getFirstChild;
      while Assigned(N2) do
      begin
        B := P.Buttons.Add;
        B.Caption := N2.Text;
        B.ImageIndex := N2.ImageIndex;
        N2 := N2.getNextSibling;
      end;
      N := N.getNextSibling;
    end;
  finally
    FH.Pages.EndUpdate;
  end;
  FModified := false;
end;

procedure TfrmOLBarEditFrm.btnOKClick(Sender: TObject);
begin
  acApply.Execute;
end;

procedure TfrmOLBarEditFrm.acMoveUpExecute(Sender: TObject);
var N: TTreeNode;
begin
  N := tvOLBar.Selected;
  N.MoveTo(N.getPrevSibling, naInsert);
  N.MakeVisible;
end;

procedure TfrmOLBarEditFrm.acMoveDownExecute(Sender: TObject);
var N, N2: TTreeNode;
begin
  N := tvOLBar.Selected;
  N2 := N.getNextSibling.getNextSibling;
  if N2 = nil then
    N.MoveTo(N.getNextSibling, naAdd)
  else
    N.MoveTo(N2, naInsert);
  N.MakeVisible;
end;

procedure TfrmOLBarEditFrm.tvOLBarCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse := false;
end;

procedure TfrmOLBarEditFrm.tvOLBarAddition(Sender: TObject;
  Node: TTreeNode);
begin
  acApply.Enabled := true;
end;

procedure TfrmOLBarEditFrm.tvOLBarDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  acApply.Enabled := true;
end;

end.

