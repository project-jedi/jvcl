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

Contributor(s): Olivier Sannier

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCheckTreeView;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Controls, ComCtrls,
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
    property Style: TJvTVCheckBoxStyle read FStyle write SetStyle default cbsNone;
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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvCheckTreeView = class(TJvTreeView)
  private
    FCheckBoxOptions: TJvTreeViewCheckBoxOptions;
    FOnToggled: TTVChangedEvent;
    FOnToggling: TTVChangingEvent;
    FNextItemRect: TRect;

    function GetCheckBox(Node: TTreeNode): Boolean;
    function GetChecked(Node: TTreeNode): Boolean;
    function GetRadioItem(Node: TTreeNode): Boolean;
    procedure SetCheckBox(Node: TTreeNode; const Value: Boolean);
    procedure SetChecked(Node: TTreeNode; const Value: Boolean);
    procedure SetRadioItem(Node: TTreeNode; const Value: Boolean);
    procedure SetCheckBoxOptions(const Value: TJvTreeViewCheckBoxOptions);
    procedure InternalSetChecked(Node: TTreeNode; const Value: Boolean; Levels: Integer);
  protected
    procedure TreeNodeCheckedChange(Sender: TObject); override;
    function ToggleNode(Node: TTreeNode) : Boolean; virtual;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoToggled(Node: TTreeNode); dynamic;
    function DoToggling(Node: TTreeNode): Boolean; dynamic;
    function CreateNode: TTreeNode; override;
    procedure SetCheckBoxes(const Value: Boolean); override;
    function IsJVCLCheckBoxes: Boolean;

    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    function GetCheckedFromState(Node: TTreeNode): Boolean; override;
    procedure SetCheckedInState(Node: TTreeNode; Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetAllCheckStates(CheckState: Boolean);
    procedure CheckAll;
    procedure UncheckAll;

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

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  CommCtrl, SysUtils, Types,
  JvConsts;

procedure ToggleTreeViewCheckBoxes(Node: TTreeNode;
  AUnChecked, AChecked, ARadioUnchecked, ARadioChecked: Integer);
var
  Tmp: TTreeNode;
begin
  if Assigned(Node) then
  begin
    if Node.StateIndex = -1 then
      Node.StateIndex := AUnchecked;

    if Node.StateIndex = AUnChecked then
    begin
      Node.StateIndex := AChecked;
      (Node as TJvTreeNode).Checked := True;
    end
    else
    if Node.StateIndex = AChecked then
    begin
      Node.StateIndex := AUnChecked;
      (Node as TJvTreeNode).Checked := False;
    end
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
      (Node as TJvTreeNode).Checked := True;
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
    FTreeView.Checkboxes := FStyle <> cbsNone;
  end;
end;

//=== { TJvCheckTreeView } ===================================================

procedure TJvCheckTreeView.CNNotify(var Msg: TWMNotify);
var
  pnmtvA: PNMTREEVIEWA;
  pnmtvW: PNMTREEVIEWW;
begin
  inherited;

  case Msg.NMHdr.code of
    TVN_SELCHANGINGA:
      begin
        pnmtvA := PNMTREEVIEWA(Msg.NMHdr);
        TreeView_GetItemRect(Handle, pnmtvA.itemNew.hItem, FNextItemRect, False);
      end;
    TVN_SELCHANGINGW:
      begin
        pnmtvW := PNMTREEVIEWW(Msg.NMHdr);
        TreeView_GetItemRect(Handle, pnmtvW.itemNew.hItem, FNextItemRect, False);
      end;
  end;
end;

constructor TJvCheckTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckBoxOptions := TJvTreeViewCheckBoxOptions.Create;
  FCheckBoxOptions.FTreeView := Self;
end;

function TJvCheckTreeView.CreateNode: TTreeNode;
begin
  Result := inherited CreateNode;
  if IsJVCLCheckBoxes then
    Result.StateIndex := CheckBoxOptions.CheckBoxUncheckedIndex;
end;

destructor TJvCheckTreeView.Destroy;
begin
  FCheckBoxOptions.Free;
  inherited Destroy;
end;

procedure TJvCheckTreeView.Click;
var
  P: TPoint;
  ItemHandle: HTREEITEM;
  ItemRect: TRect;
begin
  if IsJVCLCheckBoxes and (csClicked in ControlState) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);

    // Mantis 4316: An almost out of view item might have been moved when its
    // checkbox has been clicked. When this code executes, the item has already
    // been moved and as such the cursor is outside of it. But when the
    // selection was about to be changed, we stored the position of the item
    // at that time, and with this we can adjust the cursor position. This way
    // the adjusted position lies within the cursor mark and GetHitTestInfoAt
    // returns the expected value to trigger InternalSetChecked
    ItemHandle := TreeView_GetSelection(Handle);
    TreeView_GetItemRect(Handle, ItemHandle, ItemRect, False);

    P.X := P.X - (FNextItemRect.Left - ItemRect.Left);
    P.Y := P.Y - (FNextItemRect.Top - ItemRect.Top);

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

function TJvCheckTreeView.IsJVCLCheckBoxes: Boolean;
begin
  Result := CheckBoxes and (CheckBoxOptions.Style = cbsJVCL);
end;

function TJvCheckTreeView.GetCheckBox(Node: TTreeNode): Boolean;
begin
  with CheckBoxOptions do
    Result := (Node <> nil) and (Node.StateIndex in [CheckBoxUncheckedIndex, CheckBoxCheckedIndex]);
end;

function TJvCheckTreeView.GetChecked(Node: TTreeNode): Boolean;
begin
  with CheckBoxOptions do
    if IsJVCLCheckBoxes then
      Result := (Node <> nil) and (Node.StateIndex in [RadioCheckedIndex, CheckBoxCheckedIndex])
    else
      Result := inherited Checked[Node];
end;

function TJvCheckTreeView.GetCheckedFromState(Node: TTreeNode): Boolean;
var
  Item: TTVItem;
begin
  with Item do
  begin
    mask := TVIF_STATE;
    hItem := Node.ItemId;
    if TreeView_GetItem(Handle, Item) then
      Result := (((Item.state and TVIS_STATEIMAGEMASK) or TVIS_CHECKED) = TVIS_CHECKED) or
                (((Item.state and TVIS_STATEIMAGEMASK) or TVIS_CHECKED shl 1) = TVIS_CHECKED shl 1)
    else
      Result := False;
  end;
end;

function TJvCheckTreeView.GetRadioItem(Node: TTreeNode): Boolean;
begin
  with CheckBoxOptions do
    Result := (Node <> nil) and (Node.StateIndex in [RadioCheckedIndex, RadioUncheckedIndex]);
end;

procedure TJvCheckTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Assigned(Selected) and IsJVCLCheckBoxes and
    (Key = VK_SPACE) and (Shift * KeyboardShiftStates = []) then
  begin
    InternalSetChecked(Selected, not Checked[Selected], CheckBoxOptions.CascadeLevels);
    Key := 0; // Otherwise the checkmark will be toggled back
  end;
end;

procedure TJvCheckTreeView.SetCheckBox(Node: TTreeNode; const Value: Boolean);
begin
  if (Node <> nil) and IsJVCLCheckBoxes then
    if Value then
    begin
      if Checked[Node] then
        Node.StateIndex := CheckBoxOptions.CheckBoxCheckedIndex
      else
        Node.StateIndex := CheckBoxOptions.CheckBoxUncheckedIndex;
    end
    else
      Node.StateIndex := 0;
end;

procedure TJvCheckTreeView.SetCheckBoxes(const Value: Boolean);
var
  I: Integer;
begin
  if Value <> CheckBoxes then
  begin
    if Value and (CheckBoxOptions.Style = cbsNone) then
      CheckBoxOptions.FStyle := cbsNative; // Don't call the setter, this prevents a recursion

    inherited SetCheckBoxes(Value);

    if CheckBoxes then
    begin
      // When dealing with checkboxes, the StateIndex is used to represent
      // what an item is (radio/checkbox) and its state. If left to -1, this
      // will prevent the rest of the code here from working properly.
      // Hence we take steps to ensure that every item with a state at -1 is
      // an unchecked checkbox
      for I := 0 to Items.Count - 1 do
        if Items[I].StateIndex = -1 then
          Items[I].StateIndex := CheckBoxOptions.CheckBoxUncheckedIndex;
    end
    else
    begin
      // Hide all checkboxes
      for I := 0 to Items.Count - 1 do
        if Items[I].StateIndex <> -1 then
          Items[I].StateIndex := -1; // hide all checkboxes
    end;
  end;
end;

procedure TJvCheckTreeView.SetCheckBoxOptions(const Value: TJvTreeViewCheckBoxOptions);
begin
  FCheckBoxOptions.Assign(Value);
end;

procedure TJvCheckTreeView.InternalSetChecked(Node: TTreeNode; const Value: Boolean; Levels: Integer);
var
  Tmp: TTreeNode;
  Toggled: Boolean;
begin
  Toggled := False;
  if Checked[Node] <> Value then
    Toggled := ToggleNode(Node);
  // Only cascade if the node has been toggled.
  if Toggled and (Levels <> 0) and CheckBox[Node] and
    ((Value and (poOnCheck in CheckBoxOptions.CascadeOptions)) or
    (not Value and (poOnUnCheck in CheckBoxOptions.CascadeOptions))) then
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
  // Mantis 3608: We call inherited to be sure that the visual state is
  // updated according to the correct value.
  // Then if the style is JVCL, we work internally to update the StateIndex
  // of the node that is being modified.
  inherited Checked[Node] := Value;

  if IsJVCLCheckBoxes then
    InternalSetChecked(Node, Value, CheckBoxOptions.CascadeLevels)
end;

procedure TJvCheckTreeView.SetCheckedInState(Node: TTreeNode; Value: Boolean);
begin
  if CheckBoxes then
  begin
    if IsJVCLCheckBoxes then
    begin
      if Node.StateIndex in [CheckBoxOptions.RadioCheckedIndex, CheckBoxOptions.RadioUncheckedIndex] then
      begin
        if Value then
          Node.StateIndex := CheckBoxOptions.RadioCheckedIndex
        else
          Node.StateIndex := CheckBoxOptions.RadioUncheckedIndex;
      end
      else if Node.StateIndex in [CheckBoxOptions.CheckBoxCheckedIndex, CheckBoxOptions.CheckBoxUncheckedIndex] then
      begin
        if Value then
          Node.StateIndex := CheckBoxOptions.CheckBoxCheckedIndex
        else
          Node.StateIndex := CheckBoxOptions.CheckBoxUncheckedIndex;
      end
      else
        // Neither CheckBox or RadioButton is visible, so do nothing. Otherwise we would show a CheckBox
        // where no was before. The caller has to call "CheckBox[Node] := True" to show the CheckBox.
    end
    else
      inherited SetCheckedInState(Node, Value);
  end;
end;

procedure TJvCheckTreeView.SetRadioItem(Node: TTreeNode; const Value: Boolean);
var
  B: Boolean;
begin
  if (Node <> nil) and IsJVCLCheckBoxes then
  begin
    if Value then
    begin
      B := Checked[Node];
      Node.StateIndex := CheckBoxOptions.RadioUncheckedIndex;
      // make sure to toggle the others on or off
      if B then
        ToggleNode(Node);
    end
    else
      Node.StateIndex := 0;
  end;
end;

function TJvCheckTreeView.ToggleNode(Node: TTreeNode) : Boolean;
begin
  Result := False;
  if DoToggling(Node) then
  begin
    with CheckBoxOptions do
      ToggleTreeViewCheckBoxes(Node,
        CheckBoxUncheckedIndex, CheckBoxCheckedIndex, RadioUncheckedIndex, RadioCheckedIndex);
    DoToggled(Node);
    Result := True;
  end;
end;

procedure TJvCheckTreeView.TreeNodeCheckedChange(Sender: TObject);
var
  Node: TJvTreeNode;
begin
  inherited TreeNodeCheckedChange(Sender);

  if IsJVCLCheckBoxes then
  begin
    Node := Sender as TJvTreeNode;
    InternalSetChecked(Node, Node.Checked, CheckBoxOptions.CascadeLevels)
  end;
end;

procedure TJvCheckTreeView.WMLButtonDown(var Message: TWMLButtonDown);
var
  Node: TTreeNode;
  Item: TTVItem;
begin
  inherited;

  // Mantis 5629
  // For some reason yet to be fully understood, the VCL (or maybe the underlying windows API)
  // changes the Item.state value back to a value valid for checkboxes and not for
  // radio buttons if one continues to click on a radio button if it is already checked.
  // To fix this, we inspect the node under the mouse and if it's a radio item
  // that has its state image outside of the valid values, we restore the valid value.
  // This is not the most beautiful code in the world, but until someone understands
  // the root cause of this, this will suffice.
  //
  // --> Clues for someone willing to look:
  // The change happens when the inherited handler exists of the WM_NOTIFY management code
  // in TWinControl.MainWndProc. During the whole execution of that procedure, the
  // item.state value is valid, and as soon as one exits and comes back here, it is changed
  // Must be that a message is waiting to be processed and it gets so by calling any
  // TreeView function, but it's nearly impossible to track as it does not get passed
  // to us if we setup a TVM_GETITEM message handler...   
  Node := GetNodeAt(Message.XPos, Message.YPos);
  if RadioItem[Node] then
  begin
    Item.hItem := Node.ItemId;
    Item.mask := TVIF_STATE;
    Item.stateMask := TVIS_STATEIMAGEMASK;
    TreeView_GetItem(Handle, Item);

    if ((Item.state and TVIS_STATEIMAGEMASK) <> UINT(IndexToStateImageMask(CheckBoxOptions.RadioUncheckedIndex))) and
       ((Item.state and TVIS_STATEIMAGEMASK) <> UINT(IndexToStateImageMask(CheckBoxOptions.RadioCheckedIndex))) then
    begin
      Item.mask := TVIF_STATE or TVIF_HANDLE;
      Item.stateMask := TVIS_STATEIMAGEMASK;
      Item.hItem := Node.ItemId;
      Item.state := IndexToStateImageMask(Node.StateIndex);
      TreeView_SetItem(Handle, Item);
    end;
  end;
end;

procedure TJvCheckTreeView.SetAllCheckStates(CheckState: Boolean);
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    for I := 0 to Items.Count - 1 do
      TJvTreeNode(Items[i]).Checked := CheckState;
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvCheckTreeView.CheckAll;
begin
  SetAllCheckStates(True);
end;

procedure TJvCheckTreeView.UncheckAll;
begin
  SetAllCheckStates(False);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
