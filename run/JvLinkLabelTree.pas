{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Tree.pas, released 2002-01-06.

The Initial Developer of the Original Code is David Polberger <dpol att swipnet dott se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): Cetkovsky

Current Version: 2.00

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Please see the accompanying documentation.
Description:
  Tree.pas provides the tree data structure used elsewhere, TNodeTree, as well
  as supporting classes.

  Note: Documentation for this unit can be found in Doc\Source.txt and
        Doc\Readme.txt!
-----------------------------------------------------------------------------}
// $Id$

unit JvLinkLabelTree;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils, Windows, Graphics,
  JvLinkLabelTools, JvTypes;

type
  ENodeError = class(EJVCLException);

  { Object hierarchy:

    TNode
    |  TParentNode
    |  |  TAreaNode
    |  |  |  TStyleNode
    |  |  |  TColorNode
    |  |  |  TLinkNode
    |  |  |  TDynamicNode
    |  |  |  TRootNode
    |  TStringNode
    |  TActionNode
    |  TUnknownNode
  }

  TNodeClass = class of TNode;
  TNodeType = (ntNode, ntParentNode, ntAreaNode, ntStyleNode, ntColorNode,      // Bianconi
    ntLinkNode, ntDynamicNode, ntRootNode, ntStringNode, ntActionNode, ntUnknownNode);
  TParentNode = class;
  TRootNode = class;

  TNode = class(TObject)
  private
    FParent: TParentNode;
    FRootNode: TRootNode;
  public
    // Bianconi #2
    constructor Create;
    destructor Destroy; override;
    // End of Bianconi #2
    function GetNodeType: TNodeType;
    property Parent: TParentNode read FParent write FParent;
    property Root: TRootNode read FRootNode write FRootNode;
  end;

  INodeEnumerator = interface
    function GetNext: TNode;
    function HasNext: Boolean;
    procedure Reset;
  end;

  TNodeList = class;
  TParentNode = class(TNode)
  private
    FChildren: TNodeList;
    FOwnsChildren: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChild(const ANode: TNode; const ARoot: TRootNode);
    procedure DestroyChildren;
    function IndexOfChild(const Node: TNode): Integer;
    function GetTopLevelNodeEnumerator(const NodeClass: TNodeClass): INodeEnumerator;
    function GetFirstNodeOfClass(NodeClass: TNodeClass): TNode;
    function GetSpecificNodeOfClass(Index: Integer; NodeClass: TNodeClass): TNode;
    property Children: TNodeList read FChildren;
    property OwnsChildren: Boolean read FOwnsChildren write FOwnsChildren;
  end;

  IRectEnumerator = interface;

  TAreaNode = class(TParentNode)
  private
    FStartingPoint: TPoint;
    FStyles: TFontStyles;
    FColor: TColor;
    function GetText: string;
  protected
    function GetStyles: TFontStyles; virtual;
    function GetColor: TColor; virtual;
  public
    constructor Create;
    function GetRectEnumerator: IRectEnumerator;
    function IsPointInNode(const P: TPoint): Boolean;
    function IsPointInNodeClass(const P: TPoint; NodeClass: TNodeClass): Boolean; virtual;
    function GetNodeAtPointOfClass(const P: TPoint; NodeClass: TNodeClass): TNode;
    property StartingPoint: TPoint read FStartingPoint write FStartingPoint;
    property Styles: TFontStyles read GetStyles write FStyles;
    property Color: TColor read GetColor write FColor;
    property Text: string read GetText;
  end;

  TStyleNode = class(TAreaNode)
  private
    FStyle: TFontStyle;
  public
    constructor Create(const Style: TFontStyle);
    property Style: TFontStyle read FStyle write FStyle;
  end;

  // Bianconi
  TColorNode = class(TAreaNode)
  private
    FColor: TColor;
  public
    constructor Create(const AColor: TColor);
    property Color: TColor read FColor write FColor;
  end;
  // End of Bianconi

  TLinkState = (lsNormal, lsClicked, lsHot);
  TLinkNode = class(TAreaNode)
  private
    FState: TLinkState;
    FNumber: Integer;
    //Cetkovsky -->
    FParam: string;
    //<-- Cetkovsky
  protected
    function GetColor: TColor; override;

    //Cetkovsky -->
    function GetParam: string; virtual;
    procedure SetParam(Value: string); virtual;
    //<-- Cetkovsky
  public
    //Cetkovsky -->
    constructor Create(const AParam: string);
    //<-- Cetkovsky
    //constructor Create;
    class procedure ResetCount;
    property State: TLinkState read FState write FState;
    property Number: Integer read FNumber;

    //Cetkovsky -->
    property Param: string read GetParam write SetParam;
    //<-- Cetkovsky
  end;

  TDynamicNode = class(TAreaNode)
  private
    FNumber: Integer;
  public
    constructor Create;
    class procedure ResetCount;
    property Number: Integer read FNumber;
  end;

  TRectArray = array of TRect;

  TRootNode = class(TAreaNode)
  private
    FRectArray: TRectArray;
    procedure AddRect(const Rect: TRect);
  public
    procedure RetrieveRectsOfTLinkNodeChildren;
    function IsPointInNodeClass(const P: TPoint; NodeClass: TNodeClass): Boolean; override;
  end;

  TSpaceInfo = record
    LastWordEndsWithSpace: Boolean;
    SpaceWidth: Integer;
  end;

  TWordInfo = record
    SpaceInfo: TSpaceInfo;
    Width: Integer;
  end;

  TWordInfoArray = array of TWordInfo;

  TStringNode = class(TNode)
  private
    FText: string;
    FRectArray: TRectArray;
    FWordInfoArray: TWordInfoArray;
    FFirstWordWidthRetrieved: Boolean;
  protected
    //Cetkovsky -->
    class function ConvertEntities(Text: string): string;
    //<-- Cetkovsky
//    function ConvertEntities(Text: string): string;
  public
    constructor Create(const Text: string);
    procedure AddRect(const Rect: TRect);
    procedure ClearRects;
    procedure AddWordInfo(SpaceInfo: TSpaceInfo; Width: Integer);
    procedure ClearWordInfo;
    function IsWordInfoInArray(const Pos: Integer): Boolean;
    function GetWordInfo(const Pos: Integer): TWordInfo;
    function IsPointInNode(const P: TPoint): Boolean;
    property Text: string read FText write FText;
    property RectArray: TRectArray read FRectArray;
    property FirstWordWidthRetrieved: Boolean read FFirstWordWidthRetrieved write FFirstWordWidthRetrieved;
  end;

  TActionType = (atLineBreak, atParagraphBreak);

  TActionNode = class(TNode)
  private
    FAction: TActionType;
  public
    constructor Create(const Action: TActionType);
    property Action: TActionType read FAction write FAction;
  end;

  TUnknownNode = class(TNode)
  private
    FTag: string;
  public
    constructor Create(const Tag: string);
    property Tag: string read FTag;
  end;

  TNodeList = class(TList)
  private
    function Get(Index: Integer): TNode;
    procedure Put(Index: Integer; const Value: TNode);
  public
    function Add(Item: TNode): Integer;
    procedure Insert(Index: Integer; Item: TNode);
    function Remove(Item: TNode): Integer;
    function IndexOf(Item: TNode): Integer;
    property Items[Index: Integer]: TNode read Get write Put; default;
  end;

  TNodeTree = class(TObject)
  private
    FRoot: TRootNode;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTopLevelNodeEnumerator(const NodeClass: TNodeClass): INodeEnumerator;
    function IsPointInTree(const P: TPoint): Boolean;
    function IsPointInNodeClass(const P: TPoint; NodeClass: TNodeClass): Boolean;
    function GetNodeAtPointOfClass(const P: TPoint; NodeClass: TNodeClass): TNode;
    function GetSpecificNodeOfClass(Index: Integer; NodeClass: TNodeClass): TNode;
    procedure Clear;
    property Root: TRootNode read FRoot;
  end;

  IRectEnumerator = interface
    function GetNext: TRect;
    function HasNext: Boolean;
    procedure Reset;
  end;

const
  clNormalLink = TColor($400 or $80000000);
  clClickedLink = TColor($401 or $80000000);
  clHotLink = TColor($402 or $80000000);

procedure ResetNodeCount;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvConsts, JvResources;

type
  TRectList = class(TOwnerPointerList)
  private
    function Get(Index: Integer): PRect;
  public
    procedure AddRect(const Rect: TRect);
    property Items[Index: Integer]: PRect read Get; default;
  end;

  TRectEnumerator = class(TInterfacedObject, IRectEnumerator)
  private
    FList: TRectList;
    FIndex: Integer;
  public
    constructor Create(const List: TRectList);
    destructor Destroy; override;
    function GetNext: TRect;
    function HasNext: Boolean;
    procedure Reset;
  end;

  TTopLevelNodeEnumerator = class(TInterfacedObject, INodeEnumerator)
  private
    FRoot: TParentNode;
    FNodeClass: TNodeClass;
    FList: TNodeList;
    FIndex: Integer;
    procedure BuildList;
  public
    constructor Create(const Root: TParentNode; NodeClass: TNodeClass);
    destructor Destroy; override;
    function GetNext: TNode;
    function HasNext: Boolean;
    procedure Reset;
  end;

var
  LinkNodeCount: Integer = 0;
  DynamicNodeCount: Integer = 0;

procedure ResetNodeCount;
begin
  TLinkNode.ResetCount;
  TDynamicNode.ResetCount;
end;

//=== { TNodeTree } ==========================================================

constructor TNodeTree.Create;
begin
  inherited Create;
  FRoot := TRootNode.Create;
  FRoot.Styles := [];
  FRoot.Color := clWindowText;
end;

destructor TNodeTree.Destroy;
begin
  Clear;
  FRoot.Free;
  inherited Destroy;
end;

procedure TNodeTree.Clear;
begin
  FRoot.DestroyChildren;
end;

function TNodeTree.GetNodeAtPointOfClass(const P: TPoint; NodeClass: TNodeClass): TNode;
begin
  Result := FRoot.GetNodeAtPointOfClass(P, NodeClass);
end;

function TNodeTree.GetSpecificNodeOfClass(Index: Integer;
  NodeClass: TNodeClass): TNode;
begin
  Result := FRoot.GetSpecificNodeOfClass(Index, NodeClass);
end;

function TNodeTree.GetTopLevelNodeEnumerator(
  const NodeClass: TNodeClass): INodeEnumerator;
begin
  Result := FRoot.GetTopLevelNodeEnumerator(NodeClass);
end;

function TNodeTree.IsPointInNodeClass(const P: TPoint;
  NodeClass: TNodeClass): Boolean;
begin
  Result := FRoot.IsPointInNodeClass(P, NodeClass);
end;

function TNodeTree.IsPointInTree(const P: TPoint): Boolean;
begin
  Result := FRoot.IsPointInNode(P);
end;

//=== { TParentNode } ========================================================

constructor TParentNode.Create;
begin
  inherited Create;
  FChildren := TNodeList.Create;
  FOwnsChildren := True;
end;

destructor TParentNode.Destroy;
begin
  DestroyChildren;
  FChildren.Free;
  inherited Destroy;
end;

procedure TParentNode.AddChild(const ANode: TNode; const ARoot: TRootNode);
begin
  FChildren.Add(ANode);
  ANode.Parent := Self;
  ANode.Root   := ARoot;
end;

procedure TParentNode.DestroyChildren;
var
  I: Integer;
begin
  if FOwnsChildren then
    for I := FChildren.Count - 1 downto 0 do
    begin
      FChildren[I].Free;
      FChildren.Delete(I);
    end;
end;

function TParentNode.GetFirstNodeOfClass(NodeClass: TNodeClass): TNode;

  function RecurseTree(CurrentRoot: TParentNode): TNode;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to CurrentRoot.Children.Count - 1 do
    begin
      if CurrentRoot.Children[I] is NodeClass then
      begin
        Result := CurrentRoot.FChildren[I];
        Break;
      end
      else
      if CurrentRoot.Children[I] is TParentNode then
        Result := RecurseTree(TParentNode(CurrentRoot.Children[I]));
    end;
  end;

begin
  Result := RecurseTree(Self);
end;

function TParentNode.GetSpecificNodeOfClass(Index: Integer;
  NodeClass: TNodeClass): TNode;
var
  NodeEnum: INodeEnumerator;
  CurrentNode: TNode;
  CurrentIndex: Integer;
begin
  Result := nil;
  CurrentIndex := 0;
  NodeEnum := Self.GetTopLevelNodeEnumerator(NodeClass);
  while NodeEnum.HasNext do
  begin
    CurrentNode := NodeEnum.GetNext;
    if CurrentIndex = Index then
      Result := CurrentNode;
    Inc(CurrentIndex);
  end;
end;

function TParentNode.GetTopLevelNodeEnumerator(const NodeClass: TNodeClass): INodeEnumerator;
begin
  Result := TTopLevelNodeEnumerator.Create(Self, NodeClass);
end;

function TParentNode.IndexOfChild(const Node: TNode): Integer;
begin
  Result := FChildren.IndexOf(Node);
end;

//=== { TNodeList } ==========================================================

function TNodeList.Add(Item: TNode): Integer;
begin
  Result := inherited Add(Item);
end;

function TNodeList.Get(Index: Integer): TNode;
begin
  Result := inherited Get(Index);
end;

function TNodeList.IndexOf(Item: TNode): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TNodeList.Insert(Index: Integer; Item: TNode);
begin
  inherited Insert(Index, Item);
end;

procedure TNodeList.Put(Index: Integer; const Value: TNode);
begin
  inherited Put(Index, Value);
end;

function TNodeList.Remove(Item: TNode): Integer;
begin
  Result := inherited Remove(Item);
end;

//=== { TStringNode } ========================================================

constructor TStringNode.Create(const Text: string);
begin
  inherited Create;
  FText := ConvertEntities(Text);
end;

procedure TStringNode.AddRect(const Rect: TRect);
begin
  SetLength(FRectArray, Length(FRectArray) + 1);
  FRectArray[High(FRectArray)] := Rect;
end;

procedure TStringNode.AddWordInfo(SpaceInfo: TSpaceInfo; Width: Integer);
begin
  SetLength(FWordInfoArray, Length(FWordInfoArray) + 1);
  FWordInfoArray[High(FWordInfoArray)].Width := Width;
  FWordInfoArray[High(FWordInfoArray)].SpaceInfo := SpaceInfo;
end;

procedure TStringNode.ClearRects;
begin
  FRectArray := nil;
end;

procedure TStringNode.ClearWordInfo;
begin
  FWordInfoArray := nil;
end;

//Cetkovsky -->
class function TStringNode.ConvertEntities(Text: string): string;
//<-- Cetkovsky
type
  TEntity = record
    Entity: PChar;
    Str: PChar;
  end;
const
  NumberOfEntities = 2;
  Entities: array [0..NumberOfEntities - 1] of TEntity =
    ((Entity: '&lt;'; Str: '<'),
     (Entity: '&gt;'; Str: '>'));
var
  I: Integer;
begin
  { Our support for entities is very limited. Right now, we only use it as a way
    to write the "<" and ">" characters, which would've been impossible without
    the use of entities. To implement full support, akin to XHTML, we would need
    to revise this simple implementation, which only handles simple string
    replacement (the renderer is oblivious to entities). For our uses, however,
    it's sufficient. }
  for I := Low(Entities) to High(Entities) do
    with Entities[I] do
      TStringTools.Replace(Entity, Str, Text);
  Result := Text;
end;

function TStringNode.GetWordInfo(const Pos: Integer): TWordInfo;
begin
  if IsWordInfoInArray(Pos) then
    Result := FWordInfoArray[Pos]
  else
    raise ENodeError.CreateRes(@RsEWordInfoIndexOutOfBounds);
end;

function TStringNode.IsPointInNode(const P: TPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(FRectArray) to High(FRectArray) do
  begin
    Result := TGraphicTools.IsPointInRect(FRectArray[I], P);
    if Result then
      Break;
  end;
end;

function TStringNode.IsWordInfoInArray(const Pos: Integer): Boolean;
begin
  Result := Pos <= High(FWordInfoArray);
end;

//=== { TStyleNode } =========================================================

constructor TStyleNode.Create(const Style: TFontStyle);
begin
  inherited Create;
  FStyle := Style;
end;

// Bianconi
//=== { TColorNode } =========================================================

constructor TColorNode.Create(const AColor: TColor);
begin
  inherited Create;
  if AColor <> clNone then
    FColor := AColor
  else
    FColor := inherited GetColor;
end;
// End of Bianconi

//=== { TUnknownNode } =======================================================

constructor TUnknownNode.Create(const Tag: string);
begin
  inherited Create;
  FTag := Tag;
end;

//=== { TActionNode } ========================================================

constructor TActionNode.Create(const Action: TActionType);
begin
  inherited Create;
  FAction := Action;
end;

//=== { TAreaNode } ==========================================================

constructor TAreaNode.Create;
begin
  inherited Create;
  FStartingPoint := Point(0, 0);
end;

function TAreaNode.GetColor: TColor;
begin
  Result := FColor;
end;

function TAreaNode.GetNodeAtPointOfClass(const P: TPoint; NodeClass: TNodeClass): TNode;
var
  NodeEnum: INodeEnumerator;
  CurrentNode: TAreaNode;
begin
  Result := nil;
  NodeEnum := Self.GetTopLevelNodeEnumerator(TAreaNode);
  while NodeEnum.HasNext do
  begin
    CurrentNode := NodeEnum.GetNext as TAreaNode;
    if CurrentNode.IsPointInNode(P) then
      if CurrentNode is NodeClass then
      begin
        Result := CurrentNode;
        Break;
      end
      else
        Result := CurrentNode.GetNodeAtPointOfClass(P, NodeClass);
  end;
end;

function TAreaNode.GetRectEnumerator: IRectEnumerator;
var
  NodeEnum: INodeEnumerator;
  CurrentNode: TStringNode;
  FList: TRectList;
  I: Integer;
begin
  FList := TRectList.Create;

  try
    { Retrieve a top-level enumerator which we use to get pointers to all
      TStringNodes we own (we write Self.Get... to make explicit the fact that
      we only get pointers to TStringNodes we own). }
    NodeEnum := Self.GetTopLevelNodeEnumerator(TStringNode);
    while NodeEnum.HasNext do
    begin
      CurrentNode := NodeEnum.GetNext as TStringNode;
      for I := Low(CurrentNode.RectArray) to High(CurrentNode.RectArray) do
        FList.AddRect(CurrentNode.RectArray[I]);
    end;

    // FList will be destroyed by TRectEnumerator's destructor
    Result := TRectEnumerator.Create(FList);
  except
    FList.Free;
    raise;
  end;
end;

function TAreaNode.GetStyles: TFontStyles;
begin
  Result := FStyles;
end;

function TAreaNode.GetText: string;
var
  NodeEnum: INodeEnumerator;
begin
  Result := '';
  NodeEnum := Self.GetTopLevelNodeEnumerator(TStringNode);
  while NodeEnum.HasNext do
    Result := Result + (NodeEnum.GetNext as TStringNode).Text;
end;

function TAreaNode.IsPointInNode(const P: TPoint): Boolean;
var
  NodeEnum: INodeEnumerator;
  CurrentNode: TStringNode;
begin
  Result := False;
  NodeEnum := Self.GetTopLevelNodeEnumerator(TStringNode);
  while NodeEnum.HasNext do
  begin
    CurrentNode := NodeEnum.GetNext as TStringNode;
    Result := CurrentNode.IsPointInNode(P);
    if Result then
      Break;
  end;
end;

function TAreaNode.IsPointInNodeClass(const P: TPoint;
  NodeClass: TNodeClass): Boolean;
var
  NodeEnum: INodeEnumerator;
  CurrentNode: TNode;
begin
  Result := False;
  NodeEnum := Self.GetTopLevelNodeEnumerator(NodeClass);
  while NodeEnum.HasNext do
  begin
    CurrentNode := NodeEnum.GetNext;
    if (CurrentNode is TAreaNode) then
    begin
      Result := TAreaNode(CurrentNode).IsPointInNode(P);
      if Result then
        Break;
    end;
  end;
end;

//=== { TNode } ==============================================================

// Bianconi #2
constructor TNode.Create;
begin
  inherited Create;
  FParent := nil;
  FRootNode := nil;
end;

destructor TNode.Destroy;
begin
  FParent := nil;
  FRootNode := nil;
  inherited Destroy;
end;
// End of Bianconi #2

function TNode.GetNodeType: TNodeType;
var
  NodeClass: TClass;
const
  NodeClasses: array [TNodeType] of TClass =
    (TNode, TParentNode, TAreaNode, TStyleNode, TColorNode,     // Bianconi
     TLinkNode, TDynamicNode, TRootNode, TStringNode, TActionNode, TUnknownNode);
begin
  { We get the dynamic type using TObject.ClassType, which returns a pointer to
    the class' virtual memory table, instead of testing using the "is" reserved
    word. We do this as any node is a TNode (thanks to polymorphism); we would
    have to test in reverse order, as if we tested for TNode first everything
    would appear to be a TNode. This could get messy when we add more TNode
    descendants later. }
  NodeClass := Self.ClassType;

  for Result := Low(TNodeType) to High(TNodeType) do
    if NodeClasses[Result] = NodeClass then
      Exit;

  raise ENodeError.CreateRes(@RsETNodeGetNodeTypeUnknownClass);
end;

//=== { TTopLevelNodeEnumerator } ============================================

constructor TTopLevelNodeEnumerator.Create(const Root: TParentNode;
  NodeClass: TNodeClass);
begin
  inherited Create;
  FRoot := Root;
  FNodeClass := NodeClass;
  FIndex := 0;

  FList := TNodeList.Create;
  BuildList;
end;

destructor TTopLevelNodeEnumerator.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TTopLevelNodeEnumerator.BuildList;

  procedure RecurseTree(CurrentRoot: TParentNode);
  var
    I: Integer;
  begin
    for I := 0 to CurrentRoot.Children.Count - 1 do
    begin
      { If we find a child that is of the requested type, add it to the list.
        Don't continue to recurse, as we're not interested in this node's
        children (after all, we're a top level enumerator!). }
      if CurrentRoot.Children[I] is FNodeClass then
        FList.Add(CurrentRoot.FChildren[I])
      else
      if CurrentRoot.Children[I] is TParentNode then
        RecurseTree(TParentNode(CurrentRoot.Children[I]));
    end;
  end;

begin
  FList.Clear;
  RecurseTree(FRoot);
end;

function TTopLevelNodeEnumerator.GetNext: TNode;
begin
  if HasNext then
  begin
    Result := FList[FIndex];
    Inc(FIndex);
  end
  else
    raise ENodeError.CreateRes(@RsENoMoreNodesToReturn);
end;

function TTopLevelNodeEnumerator.HasNext: Boolean;
begin
  Result := FIndex < FList.Count;
end;

procedure TTopLevelNodeEnumerator.Reset;
begin
  FIndex := 0;
end;

//=== { TRectEnumerator } ====================================================

constructor TRectEnumerator.Create(const List: TRectList);
begin
  inherited Create;
  FList := List;
  FIndex := 0;
end;

destructor TRectEnumerator.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TRectEnumerator.GetNext: TRect;
begin
  if HasNext then
  begin
    Result := FList[FIndex]^;
    Inc(FIndex);
  end
  else
    raise ENodeError.CreateRes(@RsENoMoreRecordsToReturn);
end;

function TRectEnumerator.HasNext: Boolean;
begin
  Result := FIndex < FList.Count;
end;

procedure TRectEnumerator.Reset;
begin
  FIndex := 0;
end;

//=== { TRectList } ==========================================================

procedure TRectList.AddRect(const Rect: TRect);
var
  NewRecord: PRect;
begin
  New(NewRecord);
  try
    NewRecord^.Left := Rect.Left;
    NewRecord^.Top := Rect.Top;
    NewRecord^.Right := Rect.Right;
    NewRecord^.Bottom := Rect.Bottom;
    FList.Add(NewRecord);
  except
    Dispose(NewRecord);
    raise;
  end;
end;

function TRectList.Get(Index: Integer): PRect;
begin
  Result := FList[Index];
end;

//=== { TLinkNode } ==========================================================

//Cetkovsky -->
constructor TLinkNode.Create(const AParam: string);
//<-- Cetkovsky
begin
  inherited Create;
  FNumber := LinkNodeCount;
  Inc(LinkNodeCount);
  //Cetkovsky -->
  FParam := AParam;
  //<-- Cetkovsky
end;

function TLinkNode.GetColor: TColor;
begin
  case State of
    lsNormal:
      Result := clNormalLink;
    lsClicked:
      Result := clClickedLink;
    lsHot:
      Result := clHotLink;
  else
    Result := inherited GetColor; // To get rid of a compiler warning
  end;
end;

class procedure TLinkNode.ResetCount;
begin
  LinkNodeCount := 0;
end;

//Cetkovsky -->
function TLinkNode.GetParam: string;
begin
  Result := FParam;
end;

procedure TLinkNode.SetParam(Value: string);
begin
  FParam := Value;
end;
//<-- Cetkovsky

//=== { TRootNode } ==========================================================

procedure TRootNode.AddRect(const Rect: TRect);
begin
  SetLength(FRectArray, Length(FRectArray) + 1);
  FRectArray[High(FRectArray)] := Rect;
end;

function TRootNode.IsPointInNodeClass(const P: TPoint; NodeClass: TNodeClass): Boolean;
var
  I: Integer;
begin
  { In the root, we cache the locations of all our TLinkNode children, not only
    our most immediate children but all of them, even if they have a parent
    other than the root node. We do this to improve performance, as this routine
    might be queried every time the mouse is moved. On a PII-400 MHz computer,
    TJvLinkLabel alone might consume 20% CPU power without this optimization when
    we move the mouse pointer as fast as we can, which is not acceptable. With
    this optimization, we consume only about a third as much CPU power. }
  if (NodeClass = TLinkNode) and (Length(FRectArray) <> 0) then
  begin
    Result := False;
    for I := Low(FRectArray) to High(FRectArray) do
      if TGraphicTools.IsPointInRect(FRectArray[I], P) then
      begin
        Result := True;
        Break;
      end;
  end
  else
    Result := inherited IsPointInNodeClass(P, NodeClass);
end;

procedure TRootNode.RetrieveRectsOfTLinkNodeChildren;
var
  NodeEnum: INodeEnumerator;
  RectEnum: IRectEnumerator;
begin
  FRectArray := nil;
  NodeEnum := Self.GetTopLevelNodeEnumerator(TLinkNode);
  while NodeEnum.HasNext do
  begin
    RectEnum := (NodeEnum.GetNext as TLinkNode).GetRectEnumerator;
    while RectEnum.HasNext do
      AddRect(RectEnum.GetNext);
  end;
end;

//=== { TDynamicNode } =======================================================

constructor TDynamicNode.Create;
begin
  inherited Create;
  FNumber := DynamicNodeCount;
  Inc(DynamicNodeCount);
end;

class procedure TDynamicNode.ResetCount;
begin
  DynamicNodeCount := 0;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
