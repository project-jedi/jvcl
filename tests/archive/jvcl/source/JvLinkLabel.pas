{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLinkLabel.pas, released 2002-01-06.

The Initial Developer of the Original Code is David Polberger <dpol@swipnet.se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-01-06;
Current Version: 2.00

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Please see the accompanying documentation.
Description:
  LinkLabel.pas contains the main component, TJvLinkLabel, a rich-text label.
  It makes use of the renderer and parser stored in Renderer.pas and Parser.pas,
  respectively.

  Note: Documentation for this unit can be found in Doc\Source.txt and
        Doc\Readme.txt!
-----------------------------------------------------------------------------}

unit JvLinkLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvLinkLabelParser, JvLinkLabelRenderer, JvLinkLabelTree, JVCLVer;

type
  ELinkLabelError = class(Exception);

  TJvCustomLinkLabel = class;
  TLinkClickEvent = procedure(Sender: TObject; LinkNumber: Integer;
    LinkText: string) of object;
  TDynamicTagInitEvent = procedure(Sender: TObject; out Source: string;
    Number: Integer) of object;

  TJvCustomLinkLabel = class(TGraphicControl, IDynamicNodeHandler)
  private
    FCaption: TCaption;
    FText: TStringList;
    FRenderer: IRenderer;
    FActiveLinkNode: TLinkNode;
    FHotLinks: Boolean;
    FRect: TRect;
    FAutoHeight: Boolean;
    FMarginWidth: Integer;
    FMarginHeight: Integer;
    FOriginalCursor: TCursor;
    FOnCaptionChanged: TNotifyEvent;
    FOnLinkClick: TLinkClickEvent;
    FOnDynamicTagInit: TDynamicTagInitEvent;
    FParser: IParser;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetTransparent(const Value: Boolean);
    function GetLinkColor: TColor;
    function GetLinkStyle: TFontStyles;
    procedure SetLinkColor(const Value: TColor);
    procedure SetLinkStyle(const Value: TFontStyles);
    procedure SynchronizeRootAndFont;
    function GetLinkColorClicked: TColor;
    procedure SetLinkColorClicked(const Value: TColor);
    function GetLinkColorHot: TColor;
    procedure SetLinkColorHot(const Value: TColor);
    procedure ActivateLinkNodeAtPos(const P: TPoint; State: TLinkState);
    procedure DeactivateActiveLinkNode;
    procedure HandleDynamicNode(out Source: string; const Node: TDynamicNode);
    procedure SetCaption(const Value: TCaption);
    function GetTransparent: Boolean;
    function IsActiveLinkNodeClicked: Boolean;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetMarginHeight(const Value: Integer);
    procedure SetMarginWidth(const Value: Integer);
    procedure SetText(const Value: TStringList);

  protected
    FNodeTree: TNodeTree;
    procedure Paint; override;
    function CreateParser: IParser; virtual;
    function CreateRenderer: IRenderer; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Resize; override;
    procedure DoCaptionChanged; virtual;
    procedure DoLinkClicked(LinkNumber: Integer; LinkText: string); virtual;
    procedure DoDynamicTagInit(out Source: string; Number: Integer); virtual;
    property Parser: IParser read FParser;
    property Renderer: IRenderer read FRenderer;

    property OnDynamicTagInit: TDynamicTagInitEvent read FOnDynamicTagInit write FOnDynamicTagInit;
    property OnCaptionChanged: TNotifyEvent read FOnCaptionChanged write FOnCaptionChanged;
    property OnLinkClick: TLinkClickEvent read FOnLinkClick write FOnLinkClick;

    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Caption: TCaption read FCaption write SetCaption;
    property Text: TStringList read FText write SetText;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property LinkColor: TColor read GetLinkColor write SetLinkColor;
    property LinkColorClicked: TColor read GetLinkColorClicked write SetLinkColorClicked;
    property LinkColorHot: TColor read GetLinkColorHot write SetLinkColorHot;
    property LinkStyle: TFontStyles read GetLinkStyle write SetLinkStyle;
    property HotLinks: Boolean read FHotLinks write FHotLinks;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property MarginWidth: Integer read FMarginWidth write SetMarginWidth;
    property MarginHeight: Integer read FMarginHeight write SetMarginHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure UpdateDynamicTag(Number: Integer; const Source: string);
    function GetDynamicTagContents(Number: Integer): string;
  end;

  TJvLinkLabel = class(TJvCustomLinkLabel)
    property OnDynamicTagInit;
    property OnCaptionChanged;
    property OnLinkClick;

    property AboutJVCL;
    property Caption;
    property Text;
    property Transparent;
    property LinkColor;
    property LinkColorClicked;
    property LinkColorHot;
    property LinkStyle;
    property HotLinks;
    property AutoHeight;
    property MarginWidth;
    property MarginHeight;

    property Align;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
  end;

implementation

{$R *.res}

uses
  JvLinkLabelTools;

const
  crNewLinkHand = 1;

  { TJvLinkLabel }

procedure TJvCustomLinkLabel.ActivateLinkNodeAtPos(const P: TPoint; State: TLinkState);
var
  NodeAtPoint: TLinkNode;

  function IsNewNode: Boolean;
  begin
    { We must only redraw the TLinkNode if it either isn't the same as the
      currently active TLinkNode (FActiveLinkNode), or if we're trying to change
      the state (that is, alter the color). }
    Result := (FActiveLinkNode <> NodeAtPoint);
    if not Result and Assigned(FActiveLinkNode) then
      Result := FActiveLinkNode.State <> State;
  end;

begin
  if FNodeTree.IsPointInNodeClass(P, TLinkNode) then
  begin
    NodeAtPoint := FNodeTree.GetNodeAtPointOfClass(P, TLinkNode) as TLinkNode;
    if Assigned(NodeAtPoint) and IsNewNode then
    begin
      DeactivateActiveLinkNode;
      NodeAtPoint.State := State;
      FActiveLinkNode := NodeAtPoint;
      FRenderer.RenderNode(Canvas, FRect, NodeAtPoint);
    end;
  end;
end;

procedure TJvCustomLinkLabel.CMFontChanged(var Message: TMessage);

  procedure ClearWordInfo;
  var
    Enum: INodeEnumerator;
  begin
    Enum := FNodeTree.GetTopLevelNodeEnumerator(TStringNode);
    while Enum.HasNext do
      (Enum.GetNext as TStringNode).ClearWordInfo;
  end;

begin
  inherited;
  SynchronizeRootAndFont;
  ClearWordInfo;
  Invalidate;
end;

procedure TJvCustomLinkLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotLinks and not IsActiveLinkNodeClicked then
    DeactivateActiveLinkNode;
end;

procedure TJvCustomLinkLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

constructor TJvCustomLinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText := TStringList.Create;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  Width := 160;
  Height := 17;
  FNodeTree := TNodeTree.Create;
  FAutoHeight := True;

  // Give descendant components an opportunity to replace the default classes
  FParser := CreateParser;
  FParser.SetDynamicNodeHandler(Self);
  FRenderer := CreateRenderer;

  SetLinkColor(clBlue);
  SetLinkColorClicked(clRed);
  SetLinkColorHot(clPurple);
  SetLinkStyle([fsUnderline]);

  Screen.Cursors[crNewLinkHand] := LoadCursor(HInstance, 'LINKHAND');
end;

function TJvCustomLinkLabel.CreateParser: IParser;
begin
  { Descendant components wishing to use another parser (implementing the
    IParser interface) should override this routine and provide their own. A
    pointer to this object should be returned.

    function TMyLinkLabel.CreateParser: IParser;
    begin
      Result := TMyParser.Create;
    end; }
  Result := TDefaultParser.Create;
end;

function TJvCustomLinkLabel.CreateRenderer: IRenderer;
begin
  // Please refer to the comment in TJvCustomLinkLabel.CreateParser above.
  Result := TDefaultRenderer.Create;
end;

procedure TJvCustomLinkLabel.DeactivateActiveLinkNode;
begin
  if Assigned(FActiveLinkNode) then
  try
    FActiveLinkNode.State := lsNormal;
    FRenderer.RenderNode(Canvas, FRect, FActiveLinkNode);
  finally
    FActiveLinkNode := nil;
  end;
end;

destructor TJvCustomLinkLabel.Destroy;
begin
  FNodeTree.Free;
  FText.Free;
  inherited Destroy;
end;

procedure TJvCustomLinkLabel.DoCaptionChanged;
begin
  if Assigned(FOnCaptionChanged) then
    FOnCaptionChanged(Self);
end;

procedure TJvCustomLinkLabel.DoDynamicTagInit(out Source: string;
  Number: Integer);
begin
  if Assigned(FOnDynamicTagInit) then
    FOnDynamicTagInit(Self, Source, Number);
end;

procedure TJvCustomLinkLabel.DoLinkClicked(LinkNumber: Integer; LinkText: string);
begin
  if Assigned(FOnLinkClick) then
    FOnLinkClick(Self, LinkNumber, LinkText);
end;

function TJvCustomLinkLabel.GetDynamicTagContents(Number: Integer): string;
var
  Node: TAreaNode;
begin
  { Note that the output of this method is not serialized, that is, it will be
    plain text, with no tags present. In other words, simply the contents of
    the TStringNodes owned by the sought TDynamicNode. }
  Node := FNodeTree.GetSpecificNodeOfClass(Number, TDynamicNode) as TAreaNode;
  if Assigned(Node) then
    Result := Node.Text
  else
    raise ELinkLabelError.Create('Unable to locate specified node');
end;

function TJvCustomLinkLabel.GetLinkColor: TColor;
begin
  Result := FRenderer.LinkColor;
end;

function TJvCustomLinkLabel.GetLinkColorClicked: TColor;
begin
  Result := FRenderer.LinkColorClicked;
end;

function TJvCustomLinkLabel.GetLinkColorHot: TColor;
begin
  Result := FRenderer.LinkColorHot;
end;

function TJvCustomLinkLabel.GetLinkStyle: TFontStyles;
begin
  Result := FRenderer.LinkStyle;
end;

function TJvCustomLinkLabel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TJvCustomLinkLabel.HandleDynamicNode(out Source: string;
  const Node: TDynamicNode);
begin
  if Assigned(Node) then
    DoDynamicTagInit(Source, Node.Number);
end;

function TJvCustomLinkLabel.IsActiveLinkNodeClicked: Boolean;
begin
  Result := Assigned(FActiveLinkNode);
  if Result then
    Result := FActiveLinkNode.State = lsClicked;
end;

procedure TJvCustomLinkLabel.Loaded;
begin
  inherited;
  FOriginalCursor := Cursor;
  Resize;
end;

procedure TJvCustomLinkLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  ActivateLinkNodeAtPos(Point(X, Y), lsClicked);
end;

procedure TJvCustomLinkLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FNodeTree.IsPointInNodeClass(Point(X, Y), TLinkNode) then
  begin
    Cursor := crNewLinkHand;
    if FHotLinks and not IsActiveLinkNodeClicked then
      ActivateLinkNodeAtPos(Point(X, Y), lsHot);
  end
  else
  begin
    Cursor := FOriginalCursor;
    if FHotLinks and not IsActiveLinkNodeClicked then
      DeactivateActiveLinkNode;
  end;
end;

procedure TJvCustomLinkLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  NodeAtPoint: TLinkNode;
begin
  inherited;
  if FNodeTree.IsPointInNodeClass(Point(X, Y), TLinkNode) then
  begin
    NodeAtPoint := FNodeTree.GetNodeAtPointOfClass(Point(X, Y), TLinkNode) as TLinkNode;
    if Assigned(NodeAtPoint) then
      DoLinkClicked(NodeAtPoint.Number, NodeAtPoint.Text);
  end;

  DeactivateActiveLinkNode;
end;

procedure TJvCustomLinkLabel.Paint;
begin
  inherited;
  if Assigned(FNodeTree) then
  begin
    with Canvas do
    begin
      if not Transparent then
      begin
        Brush.Color := Color;
        Brush.Style := bsSolid;
        FillRect(ClientRect);
      end;

      Brush.Style := bsClear;
    end;

    Canvas.Font := Font;
    FRenderer.RenderTree(Canvas, FRect, FNodeTree);

    if FAutoHeight and (Align in [alNone, alTop, alBottom]) then
      ClientHeight := FRenderer.GetTextHeight + FMarginHeight;
  end;
end;

procedure TJvCustomLinkLabel.Resize;
begin
  inherited;
  FRect := Rect(ClientRect.Left + FMarginWidth, ClientRect.Top + FMarginHeight,
    ClientRect.Right - FMarginWidth, ClientRect.Bottom);
end;

procedure TJvCustomLinkLabel.SetAutoHeight(const Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLinkLabel.SetCaption(const Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    FText.Clear;
    FText.Add(FCaption);

    FActiveLinkNode := nil; // We're about to free the tree containing the node it's pointing to
    FNodeTree.Free;
    ResetNodeCount;
    FNodeTree := FParser.Parse(Value);
    SynchronizeRootAndFont;
    Invalidate;
    DoCaptionChanged;
  end;
end;

procedure TJvCustomLinkLabel.SetLinkColor(const Value: TColor);
begin
  if Value <> GetLinkColor then
  begin
    FRenderer.LinkColor := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLinkLabel.SetLinkColorClicked(const Value: TColor);
begin
  if Value <> GetLinkColorClicked then
    FRenderer.LinkColorClicked := Value;
end;

procedure TJvCustomLinkLabel.SetLinkColorHot(const Value: TColor);
begin
  FRenderer.LinkColorHot := Value;
end;

procedure TJvCustomLinkLabel.SetLinkStyle(const Value: TFontStyles);
begin
  if Value <> GetLinkStyle then
  begin
    FRenderer.LinkStyle := Value;
    Invalidate;
  end;
end;

procedure TJvCustomLinkLabel.SetMarginHeight(const Value: Integer);
begin
  FMarginHeight := Value;
  Resize;
  Invalidate;
end;

procedure TJvCustomLinkLabel.SetMarginWidth(const Value: Integer);
begin
  FMarginWidth := Value;
  Resize;
  Invalidate;
end;

procedure TJvCustomLinkLabel.SetText(const Value: TStringList);
begin
  FText.Assign(Value);
  SetCaption(TStringTools.RemoveCRLF(FText.Text));
end;

procedure TJvCustomLinkLabel.SetTransparent(const Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TJvCustomLinkLabel.SynchronizeRootAndFont;
begin
  if Assigned(FNodeTree) then
    with FNodeTree.Root do
    begin
      Styles := Font.Style;
      Color := Font.Color;
    end;
end;

procedure TJvCustomLinkLabel.UpdateDynamicTag(Number: Integer;
  const Source: string);
var
  NodeEnum: INodeEnumerator;
  Parser: IParser;
  CurrentNode: TDynamicNode;
begin
  NodeEnum := FNodeTree.GetTopLevelNodeEnumerator(TDynamicNode);
  while NodeEnum.HasNext do
  begin
    CurrentNode := NodeEnum.GetNext as TDynamicNode;
    if CurrentNode.Number = Number then
    begin
      Parser := CreateParser;
      CurrentNode.DestroyChildren;
      Parser.AddSourceTreeToDynamicNode(CurrentNode, Source);
      Paint;
      Exit;
    end;
  end;

  raise ELinkLabelError.Create('TJvCustomLinkLabel.UpdateDynamicTag: Tag not found');
end;

end.
