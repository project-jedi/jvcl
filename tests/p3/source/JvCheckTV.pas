unit JvCheckTV;

interface
uses
  Windows, Classes, SysUtils, ComCtrls, CommCtrl, JvComCtrls;

type
  TJvTVCheckBoxStyle = (cbsNone,cbsNative,cbsJVCL);
  TJvTreeViewCheckBoxOptions = class(TPersistent)
  private
    FTreeView: TJvTreeView;
    FImageIndices: array[0..3] of integer;
    FStyle: TJvTVCheckBoxStyle;
//    FActive: boolean;
    function GetImageIndex(const Index: Integer): integer;
    procedure SetImageIndex(const Index, Value: integer);
    procedure ChangeImage(OldIndex, NewIndex: integer);
    procedure SetStyle(const Value: TJvTVCheckBoxStyle);
  public
    constructor Create;
    property TreeView:TJvTreeView read FTreeView;
  published
    // NB! The Active property is introduced so you can use this treeview as a normal treeview
    // with the StateImages. Setting  it to false only disables the *handling* of the check/uncheck
    // events. It does nothing with the actual images in the StateImages list or the
    // nodes StateImageIndex property
//    property Active: boolean read FActive write FActive default false;
    property Style:TJvTVCheckBoxStyle read FStyle write SetStyle;
    // image indices for StateImages imagelist (NB! first index is 1, not 0!)
    property CheckBoxUnCheckedIndex: integer index 0 read GetImageIndex write SetImageIndex default 1;
    property CheckBoxCheckedIndex: integer index 1 read GetImageIndex write SetImageIndex default 2;
    property RadioUncheckedIndex: integer index 2 read GetImageIndex write SetImageIndex default 3;
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
  protected
    procedure ToggleNode(Node: TTreeNode); virtual;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoToggled(Node:TTreeNode);dynamic;
    function DoToggling(Node:TTreeNode):boolean;dynamic;
  public

    // get / set whether the node is checked
    property Checked[Node: TTreeNode]: boolean read GetChecked write SetChecked;
    // get / set whether the node is a checbox
    property CheckBox[Node: TTreeNode]: boolean read GetCheckBox write SetCheckBox;
    // get / set whether the node is a radioitem
    property RadioItem[Node: TTreeNode]: boolean read GetRadioItem write SetRadioItem;

    constructor Create(AOwner: TComponent); override;
  published
    property CheckBoxOptions: TJvTreeViewCheckBoxOptions read FCheckBoxOptions write SetCheckBoxOptions;
    property OnToggling:TTvChangingEvent read FOnToggling write FOnToggling;
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
    else
      if Node.StateIndex = cChecked then
        Node.StateIndex := cUnChecked
      else
        if Node.StateIndex = cRadioUnChecked then
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

procedure TJvTreeViewCheckBoxOptions.SetStyle(
  const Value: TJvTVCheckBoxStyle);
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
      ToggleNode(Selected);
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
    ToggleNode(Selected);
end;

procedure TJvCheckTreeView.SetCheckBox(Node: TTreeNode;
  const Value: boolean);
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

procedure TJvCheckTreeView.SetChecked(Node: TTreeNode;
  const Value: boolean);
begin
  with CheckBoxOptions do
    if Style = cbsJVCL then
    begin
      if (Checked[Node] <> Value) then
        ToggleNode(Node);
    end
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

