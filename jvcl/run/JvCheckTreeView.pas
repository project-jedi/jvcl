{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckTreeView.PAS, released on 2003-06-22.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCheckTreeView;

{$I jvcl.inc}
{$I vclonly.inc} // <- JvComCtrls

interface

uses
  Windows, Classes, ComCtrls,
  JvComCtrls, JvExComCtrls;

type
  TJvTVCheckBoxStyle = (cbsNone, cbsNative, cbsJVCL);
  TJvTVCascadeOption = (poOnCheck, poOnUnCheck);
  TJvTVCascadeOptions = set of TJvTVCascadeOption;

  TJvTreeViewCheckBoxOptions = class(TPersistent)
  private
    FTreeView: TJvTreeView;
    FImageIndices: array[0..3] of Integer;
    FStyle: TJvTVCheckBoxStyle;
    FCascadeLevels: Integer;
    FCascadeOptions: TJvTVCascadeOptions;
    function GetImageIndex(const Index: Integer): Integer;
    procedure SetImageIndex(const Index, Value: Integer);
    procedure ChangeImage(OldIndex, NewIndex: Integer);
    procedure SetStyle(const Value: TJvTVCheckBoxStyle);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    property TreeView: TJvTreeView read FTreeView;
  published
    // Style determines what type of checkboxes/radioitems are displayed in the treeview. Style can have one of the following values:
    // cbsNone   - no checkboxes or radiobuttons are displayed. Works like a normal treeview
    // cbsNative - use MS implementation of checkboxes. With this option you can only display
    //             checkboxes and not radioitems. You can't set up your own images using the StateImages/StateIndex properties
    //             of the treeview since this is overriden by the MS implementation
    // cbsJVCL  - use the custom JVCL style. With this option you can display any type of images
    //            by setting up your own StateImages ImageList and change the index properties below
    //            (see CheckBoxUncheckedIndex etc)
    property Style: TJvTVCheckBoxStyle read FStyle write SetStyle;
    // CascadeLevels controls how many levels down a check or uncheck of a checkbox is propagated
    // If CascadeLevels is -1, checks and unchecks are cascaded to all children recursively regardless of depth.
    // If CascadeLevels is 0 (the default), no propagation takes place. If CascadeLevels > 0, the check/uncheck is
    // propagated that number of levels (i.e if CascadeLevels = 2, checks will propagate 2 levels below
    // the currently selected node)
    // Note that this only works if Style = cbsJVCL!
    property CascadeLevels: Integer read FCascadeLevels write FCascadeLevels default 0;
    // CascadeOptions determines how propagation of checks/unchecks are performed. CascadeOptions is a
    // set that can contain a combination of the following values:
    // cbOnCheck - the checkbox state is propagated when the node is checked
    // cbOnUnCheck - the checkbox state is propagated when the node is unchecked
    // If both values are set, the checkbox state is always propagated (unless CascadeLevels = 0, of course)
    // Setting this property to an empty set is equivalent to setting CascadeLevels := 0, i.e no propagation
    property CascadeOptions: TJvTVCascadeOptions read FCascadeOptions write FCascadeOptions
      default [poOnCheck, poOnUnCheck];

    // Use the properties below in combination with an imagelist assigned to the
    // Treeviews StateImages property to control what images are displayed for the various checkbox and radioitems states
    // The actual images used are of no significance. Rather, it is the index of the property that controls what happens when a node is
    // checked or unchecked: if the node has its StateIndex set to CheckBoxUncheckedIndex or CheckBoxCheckedIndex, it will be treated as
    // a checkbox, if the node has its StateIndex set to RadioUncheckedIndex or RadioCheckedIndex, it will be treated as a radioitem
    // Checkboxes are toggled on and off, possibly with propagation
    // RadioItems are only toggled on when "checked" and there is no propagation but all other radioitems on the same level will
    // automatically be toggled off. Note that if you don't set a specific radioitem on a level as checked, they will all be unhecked
    // until the user checks one of them
    // NB! the first used index in a StateImages imagelist is 1, not 0! The 0'th item is ignored by the underlying treeview, so
    // you will have to assign a dummy image as the first to make the imagelist work for you

    // CheckBoxUncheckedIndex is the index for the image in StateImages used for the unchecked checkbox state
    property CheckBoxUncheckedIndex: Integer index 0 read GetImageIndex write SetImageIndex default 1;
    // CheckBoxCheckedIndex is the index for the image in StateImages used for the checked checkbox state
    property CheckBoxCheckedIndex: Integer index 1 read GetImageIndex write SetImageIndex default 2;
    // RadioUncheckedIndex is the index for the image in StateImages used for the unchecked radioitem state
    property RadioUncheckedIndex: Integer index 2 read GetImageIndex write SetImageIndex default 3;
    // RadioCheckedIndex is the index for the image in StateImages used for the checked radioitem state
    property RadioCheckedIndex: Integer index 3 read GetImageIndex write SetImageIndex default 4;
  end;

  TJvCheckTreeView = class(TJvTreeView)
  private
    FCheckBoxOptions: TJvTreeViewCheckBoxOptions;
    FOnToggled: TTVChangedEvent;
    FOnToggling: TTVChangingEvent;
    function GetCheckBox(Node: TTreeNode): Boolean;
    function GetChecked(Node: TTreeNode): Boolean;
    function GetRadioItem(Node: TTreeNode): Boolean;
    procedure SetCheckBox(Node: TTreeNode; const Value: Boolean);
    procedure SetChecked(Node: TTreeNode; const Value: Boolean);
    procedure SetRadioItem(Node: TTreeNode; const Value: Boolean);
    procedure SetCheckBoxOptions(const Value: TJvTreeViewCheckBoxOptions);
    procedure InternalSetChecked(Node: TTreeNode; const Value: Boolean; Levels: Integer);
  protected
    procedure ToggleNode(Node: TTreeNode); virtual;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoToggled(Node: TTreeNode); dynamic;
    function DoToggling(Node: TTreeNode): Boolean; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // get / set whether Node is checked
    property Checked[Node: TTreeNode]: Boolean read GetChecked write SetChecked;
    // get / set whether Node is a checkbox
    property CheckBox[Node: TTreeNode]: Boolean read GetCheckBox write SetCheckBox;
    // get / set whether Node is a radioitem
    property RadioItem[Node: TTreeNode]: Boolean read GetRadioItem write SetRadioItem;
  published
    // CheckBoxOptions controls the behavior of the checbox/radioitems in the treeview
    property CheckBoxOptions: TJvTreeViewCheckBoxOptions read FCheckBoxOptions write SetCheckBoxOptions;
    // called just before a node is to be toggled
    // NB! If you have activated propagation, this event will be called for *all* nodes affected by the propagation
    property OnToggling: TTVChangingEvent read FOnToggling write FOnToggling;
    // called just after a node has been toggled
    // NB! If you have activated propagation, this event will be called for *all* nodes affected by the propagation
    property OnToggled: TTVChangedEvent read FOnToggled write FOnToggled;
  end;

implementation

procedure ToggleTreeViewCheckBoxes(Node: TTreeNode;
  AUnChecked, AChecked, ARadioUnchecked, ARadioChecked: Integer);
var
  Tmp: TTreeNode;
begin
  if Assigned(Node) then
  begin
    if Node.StateIndex = AUnChecked then
      Node.StateIndex := AChecked
    else
    if Node.StateIndex = AChecked then
      Node.StateIndex := AUnChecked
    else
    if Node.StateIndex = ARadioUnchecked then
    begin
      Tmp := Node.Parent;
      if not Assigned(Tmp) then
        Tmp := TTreeView(Node.TreeView).Items.GetFirstNode
      else
        Tmp := Tmp.getFirstChild;
      while Assigned(Tmp) do
      begin
        if Tmp.StateIndex in [ARadioUnchecked, ARadioChecked] then
          Tmp.StateIndex := ARadioUnchecked;
        Tmp := Tmp.getNextSibling;
      end;
      Node.StateIndex := ARadioChecked;
    end;
  end;
end;

//=== { TJvTreeViewCheckBoxOptions } =========================================

constructor TJvTreeViewCheckBoxOptions.Create;
var
  I: Integer;
begin
  inherited Create;
  for I := Low(FImageIndices) to High(FImageIndices) do
    FImageIndices[I] := I+1;
  FCascadeLevels := 0;
  FCascadeOptions := [poOnCheck, poOnUnCheck]
end;

procedure TJvTreeViewCheckBoxOptions.Assign(Source: TPersistent);
begin
  if (Source <> Self) and (Source is TJvTreeViewCheckBoxOptions) then
  begin
    Style := TJvTreeViewCheckBoxOptions(Source).Style;
    CascadeLevels := TJvTreeViewCheckBoxOptions(Source).CascadeLevels;
    CascadeOptions := TJvTreeViewCheckBoxOptions(Source).CascadeOptions;
    CheckBoxUncheckedIndex := TJvTreeViewCheckBoxOptions(Source).CheckBoxUncheckedIndex;
    CheckBoxCheckedIndex := TJvTreeViewCheckBoxOptions(Source).CheckBoxCheckedIndex;
    RadioUncheckedIndex := TJvTreeViewCheckBoxOptions(Source).RadioUncheckedIndex;
    RadioCheckedIndex := TJvTreeViewCheckBoxOptions(Source).RadioCheckedIndex;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTreeViewCheckBoxOptions.ChangeImage(OldIndex, NewIndex: Integer);
var
  N: TTreeNode;
begin
  if Assigned(FTreeView) then
  begin
    FTreeView.Items.BeginUpdate;
    try
      N := FTreeView.Items.GetFirstNode;
      while Assigned(N) do
      begin
        if N.StateIndex = OldIndex then
          N.StateIndex := NewIndex;
        N := N.GetNext;
      end;
    finally
      FTreeView.Items.EndUpdate;
    end;
  end;
end;

function TJvTreeViewCheckBoxOptions.GetImageIndex(const Index: Integer): Integer;
begin
  if (Index >= 0) and (Index <= High(FImageIndices)) then
    Result := FImageIndices[Index]
  else
    Result := 0;
end;

procedure TJvTreeViewCheckBoxOptions.SetImageIndex(const Index, Value: Integer);
begin
  if (Index >= 0) and (Index <= High(FImageIndices)) and (FImageIndices[Index] <> Value) then
  begin
    ChangeImage(FImageIndices[Index], Value);
    FImageIndices[Index] := Value;
  end;
end;

procedure TJvTreeViewCheckBoxOptions.SetStyle(const Value: TJvTVCheckBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    FTreeView.Checkboxes := FStyle = cbsNative;
  end;
end;

//=== { TJvCheckTreeView } ===================================================

constructor TJvCheckTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckBoxOptions := TJvTreeViewCheckBoxOptions.Create;
  FCheckBoxOptions.FTreeView := Self;
end;

destructor TJvCheckTreeView.Destroy;
begin
  FCheckBoxOptions.Free;
  inherited Destroy;
end;

procedure TJvCheckTreeView.Click;
var
  P: TPoint;
begin
  if CheckBoxOptions.Style = cbsJVCL then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if htOnStateIcon in GetHitTestInfoAt(P.X, P.Y) then
      InternalSetChecked(Selected, not Checked[Selected], CheckBoxOptions.CascadeLevels);
  end;
  inherited Click;
end;

procedure TJvCheckTreeView.DoToggled(Node: TTreeNode);
begin
  if Assigned(FOnToggled) then
    FOnToggled(Self, Node);
end;

function TJvCheckTreeView.DoToggling(Node: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnToggling) then
    FOnToggling(Self, Node, Result);
end;

function TJvCheckTreeView.GetCheckBox(Node: TTreeNode): Boolean;
begin
  with CheckBoxOptions do
    Result := (Node <> nil) and (Node.StateIndex in [CheckBoxUncheckedIndex, CheckBoxCheckedIndex]);
end;

function TJvCheckTreeView.GetChecked(Node: TTreeNode): Boolean;
begin
  with CheckBoxOptions do
    Result := (Node <> nil) and (Node.StateIndex in [RadioCheckedIndex, CheckBoxCheckedIndex]);
end;

function TJvCheckTreeView.GetRadioItem(Node: TTreeNode): Boolean;
begin
  with CheckBoxOptions do
    Result := (Node <> nil) and (Node.StateIndex in [RadioCheckedIndex, RadioUncheckedIndex]);
end;

procedure TJvCheckTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (CheckBoxOptions.Style = cbsJVCL) and (Key = VK_SPACE) and Assigned(Selected) then
    InternalSetChecked(Selected, not Checked[Selected], CheckBoxOptions.CascadeLevels);
end;

procedure TJvCheckTreeView.SetCheckBox(Node: TTreeNode; const Value: Boolean);
begin
  with CheckBoxOptions do
    if (Node <> nil) and (Style = cbsJVCL) then
    begin
      if Value then
      begin
        if Checked[Node] then
          Node.StateIndex := CheckBoxCheckedIndex
        else
          Node.StateIndex := CheckBoxUncheckedIndex;
      end
      else
        Node.StateIndex := 0;
    end;
end;

procedure TJvCheckTreeView.SetCheckBoxOptions(const Value: TJvTreeViewCheckBoxOptions);
begin
  FCheckBoxOptions.Assign(Value);
end;

procedure TJvCheckTreeView.InternalSetChecked(Node: TTreeNode; const Value: Boolean; Levels: Integer);
var
  Tmp: TTreeNode;
begin
  if Checked[Node] <> Value then
    ToggleNode(Node);
  if (Levels <> 0) and CheckBox[Node] and
    ((Value and (poOnCheck in CheckBoxOptions.CascadeOptions)) or (not Value and (poOnUnCheck in
    CheckBoxOptions.CascadeOptions))) then
  begin
    Tmp := Node.getFirstChild;
    while Tmp <> nil do
    begin
      if CheckBox[Tmp] then
        InternalSetChecked(Tmp, Value, Levels - Ord(Levels > 0));
      Tmp := Tmp.getNextSibling;
    end;
  end;
end;

procedure TJvCheckTreeView.SetChecked(Node: TTreeNode; const Value: Boolean);
begin
  with CheckBoxOptions do
    if Style = cbsJVCL then
      InternalSetChecked(Node, Value, CheckBoxOptions.CascadeLevels)
    else
      inherited Checked[Node] := Value;
end;

procedure TJvCheckTreeView.SetRadioItem(Node: TTreeNode; const Value: Boolean);
var
  B: Boolean;
begin
  with CheckBoxOptions do
    if (Node <> nil) and (Style = cbsJVCL) then
    begin
      if Value then
      begin
        B := Checked[Node];
        Node.StateIndex := RadioUncheckedIndex;
        // make sure to toggle the others on or off
        if B then
          ToggleNode(Node);
      end
      else
        Node.StateIndex := 0;
    end;
end;

procedure TJvCheckTreeView.ToggleNode(Node: TTreeNode);
begin
  if DoToggling(Node) then
  begin
    with CheckBoxOptions do
      ToggleTreeViewCheckBoxes(Node,
        CheckBoxUncheckedIndex, CheckBoxCheckedIndex, RadioUncheckedIndex, RadioCheckedIndex);
    DoToggled(Node);
  end;
end;

end.

