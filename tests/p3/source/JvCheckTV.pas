
unit JvCheckTV;

interface
uses
  Windows, Classes, SysUtils, ComCtrls, CommCtrl, JvComCtrls;

type
  TJvTVCheckBoxStyle = (cbsNone,cbsNative,cbsJVCL);
  TJvTVCascadeOption = (poOnCheck,poOnUnCheck);
  TJvTVCascadeOptions = set of TJvTVCascadeOption;

  TJvTreeViewCheckBoxOptions = class(TPersistent)
  private
    FTreeView: TJvTreeView;
    FImageIndices: array[0..3] of integer;
    FStyle: TJvTVCheckBoxStyle;
    FCascadeLevels: integer;
    FCascadeOptions: TJvTVCascadeOptions;
    function GetImageIndex(const Index: Integer): integer;
    procedure SetImageIndex(const Index, Value: integer);
    procedure ChangeImage(OldIndex, NewIndex: integer);
    procedure SetStyle(const Value: TJvTVCheckBoxStyle);
  public
    constructor Create;
    property TreeView:TJvTreeView read FTreeView;
  published
    // Style determines what type of checkboxes/radioitems are displayed in the treeview. Style can have one of the following values:
    // cbsNone   - no checkboxes or radiobuttons are displayed. Works like a normal treeview
    // cbsNative - use MS implementation of checkboxes. With this option you can only display
    //             checkboxes and not radioitems. You can't set up your own images using the StateImages/StateIndex properties
    //             of the treeview since this is overriden by the MS implementation
    // cbsJVCL  - use the custom JVCL style. With this option you can display any type of images
    //            by setting up your own StateImages ImageList and change the index properties below
    //            (see CheckBoxUnCheckedIndex etc)
    property Style:TJvTVCheckBoxStyle read FStyle write SetStyle;
    // CascadeLevels controls how many levels down a check or uncheck of a checkbox is propagated
    // If CascadeLevels is -1, checks and unchecks are cascaded to all children recursively regardless of depth.
    // If CascadeLevels is 0 (the default), no propagation takes place. If CascadeLevels > 0, the check/uncheck is
    // propagated that number of levels (i.e if CascadeLevels = 2, checks will propagate 2 levels below
    // the currently selected node)
    // Note that this only works if Style = cbsJVCL!
    property CascadeLevels:integer read FCascadeLevels write FCascadeLevels default 0;
    // CascadeOptions determines how propagation of checks/unchecks are performed. CascadeOptions is a
    // set that can contain a combination of the following values:
    // cbOnCheck - the checkbox state is propagated when the node is checked
    // cbOnUnCheck - the checkbox state is propagated when the node is unchecked
    // If both values are set, the checkbox state is always propagated (unless CascadeLevels = 0, of course)
    // Setting this property to an empty set is equivalent to setting CascadeLevels := 0, i.e no propagation
    property CascadeOptions:TJvTVCascadeOptions read FCascadeOptions write FCascadeOptions default [poOnCheck,poOnUnCheck];

    // Use the properties below in combination with an imagelist assigned to the
    // Treeviews StateImages property to control what images are displayed for the various checkbox and radioitems states
    // The actual images used are of no significance. Rather, it is the index of the property that controls what happens when a node is
    // checked or unchecked: if the node has its StateIndex set to CheckBoxUnCheckedIndex or CheckBoxCheckedIndex, it will be treated as
    // a checkbox, if the node has its StateIndex set to RadioUncheckedIndex or RadioCheckedIndex, it will be treated as a radioitem
    // Checkboxes are toggled on and off, possibly with propagation
    // RadioItems are only toggled on when "checked" and there is no propagation but all other radioitems on the same level will
    // automatically be toggled off. Note that if you don't set a specific radioitem on a level as checked, they will all be unhecked
    // until the user checks one of them
    // NB! the first used index in a StateImages imagelist is 1, not 0! The 0'th item is ignored by the underlying treeview, so
    // you will have to assign a dummy image as the first to make the imagelist work for you

    // CheckBoxUnCheckedIndex is the index for the image in StateImages used for the unchecked checkbox state
    property CheckBoxUnCheckedIndex: integer index 0 read GetImageIndex write SetImageIndex default 1;
    // CheckBoxCheckedIndex is the index for the image in StateImages used for the checked checkbox state
    property CheckBoxCheckedIndex: integer index 1 read GetImageIndex write SetImageIndex default 2;
    // RadioUncheckedIndex is the index for the image in StateImages used for the unchecked radioitem state
    property RadioUncheckedIndex: integer index 2 read GetImageIndex write SetImageIndex default 3;
    // RadioCheckedIndex is the index for the image in StateImages used for the checked radioitem state
    property RadioCheckedIndex: integer index 3 read GetImageIndex write SetImageIndex default 4;
  end;

  TJvCheckTreeView = class(TJvTreeView)
  private
    FCheckBoxOptions: TJvTreeViewCheckBoxOptions;
    FOnToggled: TTvChangedEvent;
    FOnToggling: TTvChangingEvent;
    function GetCheckBox(Node: TTreeNode): boolean;
    function GetChecked(Node: TTreeNode): boolean;
    function GetRadioItem(Node: TTreeNode): boolean;
    procedure SetCheckBox(Node: TTreeNode; const Value: boolean);
    procedure SetChecked(Node: TTreeNode; const Value: boolean);
    procedure SetRadioItem(Node: TTreeNode; const Value: boolean);
    procedure SetCheckBoxOptions(const Value: TJvTreeViewCheckBoxOptions);
    procedure InternalSetChecked(Node: TTreeNode; const Value: boolean; Levels: integer);
  protected
    procedure ToggleNode(Node: TTreeNode); virtual;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoToggled(Node:TTreeNode);dynamic;
    function DoToggling(Node:TTreeNode):boolean;dynamic;
  public

    // get / set whether Node is checked
    property Checked[Node: TTreeNode]: boolean read GetChecked write SetChecked;
    // get / set whether Node is a checkbox
    property CheckBox[Node: TTreeNode]: boolean read GetCheckBox write SetCheckBox;
    // get / set whether Node is a radioitem
    property RadioItem[Node: TTreeNode]: boolean read GetRadioItem write SetRadioItem;

    constructor Create(AOwner: TComponent); override;
  published
    // CheckBoxOptions controls the behavior of the checbox/radioitems in the treeview
    property CheckBoxOptions: TJvTreeViewCheckBoxOptions read FCheckBoxOptions write SetCheckBoxOptions;
    // called just before a node is to be toggled
    // NB! If you have activated propagation, this event will be called for *all* nodes affected by the propagation
    property OnToggling:TTvChangingEvent read FOnToggling write FOnToggling;
    // called just after a node has been toggled
    // NB! If you have activated propagation, this event will be called for *all* nodes affected by the propagation
    property OnToggled:TTvChangedEvent read FOnToggled write FOnToggled;
  end;

implementation

procedure ToggleTreeViewCheckBoxes(Node: TTreeNode; cUnChecked, cChecked, cRadioUnchecked, cRadioChecked: integer);
var
  tmp: TTreeNode;
begin
  if Assigned(Node) then
  begin
    if Node.StateIndex = cUnChecked then
      Node.StateIndex := cChecked
    else if Node.StateIndex = cChecked then
      Node.StateIndex := cUnChecked
    else if Node.StateIndex = cRadioUnChecked then
    begin
      tmp := Node.Parent;
      if not Assigned(tmp) then
        tmp := TTreeView(Node.TreeView).Items.getFirstNode
      else
        tmp := tmp.getFirstChild;
      while Assigned(tmp) do
      begin
        if (tmp.StateIndex in [cRadioUnChecked, cRadioChecked]) then
           tmp.StateIndex := cRadioUnChecked;
        tmp := tmp.getNextSibling;
      end;
      Node.StateIndex := cRadioChecked;
    end; // if StateIndex = cRadioUnChecked
  end; // if Assigned(Node)
end;

{ TJvTreeViewCheckBoxOptions }

procedure TJvTreeViewCheckBoxOptions.ChangeImage(OldIndex, NewIndex: integer);
var
  N: TTreeNode;
begin
  if Assigned(FTreeView) then
  begin
    N := FTreeView.Items.GetFirstNode;
    while Assigned(N) do
    begin
      if N.StateIndex = OldIndex then
        N.StateIndex := NewIndex;
      N := N.GetNext;
    end;
  end;
end;

constructor TJvTreeViewCheckBoxOptions.Create;
begin
  inherited;
  FImageIndices[0] := 1;
  FImageIndices[1] := 2;
  FImageIndices[2] := 3;
  FImageIndices[3] := 4;
  FCascadeLevels := 0;
  FCascadeOptions := [poOnCheck,poOnUnCheck]
end;

function TJvTreeViewCheckBoxOptions.GetImageIndex(const Index: Integer): integer;
begin
  if (Index >= 0) and (Index <= High(FImageIndices)) then
    Result := FImageIndices[Index]
  else
    Result := 0;
end;

procedure TJvTreeViewCheckBoxOptions.SetImageIndex(const Index, Value: integer);
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

{ TJvCheckTreeView }

procedure TJvCheckTreeView.Click;
var
  P: TPoint;
begin
  if CheckBoxOptions.Style = cbsJVCL then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if (htOnStateIcon in GetHitTestInfoAt(P.X, P.Y)) then
      InternalSetChecked(Selected,not Checked[Selected], CheckBoxOptions.CascadeLevels);
  end;
  inherited;
end;

constructor TJvCheckTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FCheckBoxOptions := TJvTreeViewCheckBoxOptions.Create;
  FCheckBoxOptions.FTreeView := self;
end;

procedure TJvCheckTreeView.DoToggled(Node: TTreeNode);
begin
  if Assigned(FOnToggled) then
    FOnToggled(self,Node);
end;

function TJvCheckTreeView.DoToggling(Node: TTreeNode): boolean;
begin
  Result := true;
  if Assigned(FOnToggling) then
    FOnToggling(self,Node,Result);
end;

function TJvCheckTreeView.GetCheckBox(Node: TTreeNode): boolean;
begin
  with CheckBoxOptions do
    Result := (Node <> nil) and (Node.StateIndex in [CheckBoxUncheckedIndex, CheckBoxCheckedIndex]);
end;

function TJvCheckTreeView.GetChecked(Node: TTreeNode): boolean;
begin
  with CheckBoxOptions do
    Result := (Node <> nil) and (Node.StateIndex in [RadioCheckedIndex, CheckBoxCheckedIndex]);
end;

function TJvCheckTreeView.GetRadioItem(Node: TTreeNode): boolean;
begin
  with CheckBoxOptions do
    Result := (Node <> nil) and (Node.StateIndex in [RadioCheckedIndex, RadioUnCheckedIndex]);
end;

procedure TJvCheckTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (CheckBoxOptions.Style = cbsJVCL) and (Key = VK_SPACE) and Assigned(Selected) then
    InternalSetChecked(Selected,not Checked[Selected],CheckBoxOptions.CascadeLevels);
end;

procedure TJvCheckTreeView.SetCheckBox(Node: TTreeNode; const Value: boolean);
begin
  with CheckBoxOptions do
    if (Node <> nil) and (Style = cbsJVCL) then
    begin
      if Value then
      begin
        if Checked[Node] then
          Node.StateIndex := CheckBoxCheckedIndex
        else
          Node.StateIndex := CheckBoxUnCheckedIndex;
      end
      else
        Node.StateIndex := 0;
    end;
end;

procedure TJvCheckTreeView.SetCheckBoxOptions(const Value: TJvTreeViewCheckBoxOptions);
begin
  // FCheckBoxOptions := Value;
end;

procedure TJvCheckTreeView.InternalSetChecked(Node: TTreeNode;const Value: boolean;Levels:integer);
var tmp:TTreeNode;
begin
  if (Checked[Node] <> Value) then
    ToggleNode(Node);
  if (Levels <> 0) and CheckBox[Node]
    and ((Value and (poOnCheck in CheckBoxOptions.CascadeOptions)) or (not Value and (poOnUnCheck in CheckBoxOptions.CascadeOptions))) then
  begin
    tmp := Node.getFirstChild;
    while tmp <> nil do
    begin
      if CheckBox[tmp] then
        InternalSetChecked(tmp,Value,Levels-Ord(Levels > 0));
      tmp := tmp.getNextSibling;
    end;
  end;
end;

procedure TJvCheckTreeView.SetChecked(Node: TTreeNode;
  const Value: boolean);
begin
  with CheckBoxOptions do
    if Style = cbsJVCL then
      InternalSetChecked(Node,Value,CheckBoxOptions.CascadeLevels)
    else
      inherited Checked[Node] := Value;
end;

procedure TJvCheckTreeView.SetRadioItem(Node: TTreeNode; const Value: boolean);
var
  b: boolean;
begin
  with CheckBoxOptions do
    if (Node <> nil) and (Style = cbsJVCL) then
    begin
      if Value then
      begin
        b := Checked[Node];
        Node.StateIndex := RadioUnCheckedIndex;
        // make sure to toggle the others on or off
        if b then
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
        CheckBoxUnCheckedIndex, CheckBoxCheckedIndex, RadioUnCheckedIndex, RadioCheckedIndex);
    DoToggled(Node);
  end;
end;

end.

