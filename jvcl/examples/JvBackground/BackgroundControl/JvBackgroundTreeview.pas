{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBackgroundTreeview.PAS, released on 2004-04-26.

The Initial Developer of the Original Code is Robert Rossmair [Robert dott Rossmair att t-online dott de]
Portions created by Robert Rossmair are Copyright (C) 2003 Robert Rossmair.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Example of control that implements background image support
-----------------------------------------------------------------------------}
// $Id$

unit JvBackgroundTreeview;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  CommCtrl, ComCtrls,
  JvBackgrounds;

type
  TJvBackgroundTVButtonKind = (tvbRectangle, tvbRhombus, tvbCircle);

  TJvBackgroundTreeView = class(TCustomTreeView)
  private
    FBackground: TJvControlBackground;
    FSelectedColor: TColor;
    FHorzOffset: Integer;
    FButtonSize: Integer;
    FButtonKind: TJvBackgroundTVButtonKind;
    function DefButtonSize: Integer;
    procedure DottedLine(X1, Y1, X2, Y2: Integer);
    function DrawBranch(Node: TTreeNode): TRect;
    procedure SetBackground(Value: TJvControlBackground);
    procedure SetButtonKind(Value: TJvBackgroundTVButtonKind);
    procedure SetButtonSize(Value: Integer);
    procedure SetSelectedColor(Value: TColor);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    function CanCollapse(Node: TTreeNode): Boolean; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    //procedure Change(Node: TTreeNode); override;
    //function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; override;
    procedure DoCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    {function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; override;}
    //function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
    function MaxButtonSize: Integer; dynamic;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Background: TJvControlBackground read FBackground write SetBackground;
    property ButtonKind: TJvBackgroundTVButtonKind read FButtonKind write SetButtonKind;
    property ButtonSize: Integer read FButtonSize write SetButtonSize;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor
      default clHighlightText;
    property Align;
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    { Items must be published after OnGetImageIndex and OnGetSelectedIndex }
    property Items;
  end;


implementation

constructor TJvBackgroundTreeView.Create(AOwner: TComponent);
begin
  FBackground := TJvControlBackground.Create(Self);
  inherited Create(AOwner);
  FButtonSize := DefButtonSize;
  FSelectedColor := clHighlightText;
end;

destructor TJvBackgroundTreeView.Destroy;
begin
  FBackground.Free;
  inherited;
end;

function TJvBackgroundTreeView.DefButtonSize: Integer;
begin
  Result := Abs(Font.Size);
  if Result < 8 then Result := 8;
  Result := 4 + (Result-7) div 6;
end;

procedure LineProc(X, Y: Integer; TV: TJvBackgroundTreeView); stdcall;
begin
  with TV.Canvas do
    if Odd((X xor TV.FHorzOffset) xor Y) then Pixels[X, Y] := Pen.Color;
end;

procedure TJvBackgroundTreeView.DottedLine(X1, Y1, X2, Y2: Integer);
begin
  LineDDA(X1, Y1, X2, Y2, @LineProc, Integer(Self));
end;

function TJvBackgroundTreeView.DrawBranch(Node: TTreeNode): TRect;
var
  R: TRect absolute Result;
  cx, cy: Integer;
  Yt, Yb: Integer;
begin
  Canvas.Pen.Color := clBtnShadow;
  R := Node.DisplayRect(True);
  FHorzOffset := R.Left xor (Node.Level and Indent);
  Dec(R.Left, Indent);
  cx := R.Left + Indent div 2;
  cy := (R.Top + R.Bottom) div 2;
  with Canvas do
  begin
    if Node.HasChildren then
    begin
      Yt := cy-FButtonSize;
      Yb := cy+FButtonSize;
      case FButtonKind of
        tvbRectangle:
          Rectangle(cx-FButtonSize, Yt, cx+FButtonSize+1, Yb+1);
        tvbRhombus:
          begin
      	    Dec(Yt);
            Inc(Yb);
            Polygon([Point(cx-FButtonSize-1, cy), Point(cx, Yt), Point(cx+FButtonSize+1, cy), Point(cx, Yb)]);
          end;
      	tvbCircle:
          Ellipse(cx-FButtonSize, cy-FButtonSize, cx+FButtonSize+1, cy+FButtonSize+1);
      end;
      //draw the horizontal indicator.
      PenPos := Point(cx-FButtonSize+2, cy);
      LineTo(cx+FButtonSize-1, cy);
      //draw the vertical indicator if the node is collapsed
      if not Node.Expanded then
      begin
        PenPos := Point(cx, cy-FButtonSize+2);
        LineTo(cx, cy+FButtonSize-1);
      end;
      DottedLine(cx+FButtonSize, cy, R.Left + Indent-2, cy)
    end else
    begin
      Yt := cy;
      Yb := cy;
      DottedLine(cx, cy, R.Left + Indent-2, cy);
    end;
    if Node.GetPrev <> nil then
      //draw half vertical line, top portion.
      DottedLine(cx, Yt, cx, R.Top-1);

    if ((Node.GetNextVisible <> nil) and (Node.GetNextVisible.Level = Node.Level))
    or (Node.GetNextSibling <> nil) then
    //draw bottom portion of half vertical line.
      DottedLine(cx, Yb, cx, R.Bottom+1);
        //now connect vertical lines of higher level nodes.
    Node := Node.Parent;
    while Node <> nil do
    begin
      cx := cx - Indent;
      if Node.GetNextSibling <> nil then
        DottedLine(cx, R.Top, cx, R.Bottom);
      Node := Node.Parent;
    end;
  end;
  Inc(R.Left, Indent {+ FButtonSize});
end;

procedure TJvBackgroundTreeView.DoCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  with Sender.Canvas do
  begin
    Brush.Style := bsClear;
    if cdsSelected in State
      then Font.Color := FSelectedColor;
    with DrawBranch(Node) do
      TextOut(Left+2, Top+1, Node.Text);
  end;
  DefaultDraw := False;
end;
{
function TJvBackgroundTreeView.CustomDrawItem(
  Node: TTreeNode;
  State: TCustomDrawState;
  Stage: TCustomDrawStage;
  var PaintImages: Boolean): Boolean;
begin
  Result := inherited CustomDrawItem(Node, State, Stage, PaintImages);
  if Result and (Stage = cdPrePaint) then
  with Canvas do
  begin
    Brush.Style := bsClear;
    if cdsSelected in State
      then Font.Color := clRed;
    with DrawBranch(Node) do
      TextOut(Left+2, Top+1, Node.Text);
    Result := False;
  end;
end;
}

function TJvBackgroundTreeView.CanExpand(Node: TTreeNode): Boolean;
var
  R: TRect;
begin
  Result := inherited CanExpand(Node);
  if Result and not Node.Expanded then
  begin
    R := ClientRect;
    R.Top := Node.DisplayRect(False).Bottom;
    InvalidateRect(Handle, @R, False);
  end;
end;

function TJvBackgroundTreeView.CanCollapse(Node: TTreeNode): Boolean;
var
  R: TRect;
begin
  Result := inherited CanCollapse(Node);
  if Result and Node.Expanded then
  begin
    R := ClientRect;
    R.Top := Node.DisplayRect(False).Bottom;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TJvBackgroundTreeView.SetBackground(Value: TJvControlBackground);
begin
end;

procedure TJvBackgroundTreeView.SetButtonKind(Value: TJvBackgroundTVButtonKind);
begin
  if FButtonKind <> Value then
  begin
    FButtonKind := Value;
    Invalidate;
  end;
end;

procedure TJvBackgroundTreeView.SetButtonSize(Value: Integer);
begin
  if Value < 3 then Value := 3 else
  if Value > MaxButtonSize then Value := MaxButtonSize;
  if FButtonSize <> Value then
  begin
    FButtonSize := Value;
    Invalidate;
  end;
end;

procedure TJvBackgroundTreeView.SetSelectedColor(Value: TColor);
begin
  if Value <> FSelectedColor then
  begin
    FSelectedColor := Value;
    Invalidate;
  end;
end;

function TJvBackgroundTreeView.MaxButtonSize: Integer;
begin
  Result := Abs(Font.Size) * 3 div 5;
end;

procedure TJvBackgroundTreeView.WndProc(var Message: TMessage);
begin
  if FBackground.HookBeforeMessage(Message) then Exit;
  inherited WndProc(Message);
  FBackground.HookAfterMessage(Message);
end;

procedure TJvBackgroundTreeView.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ButtonSize := DefButtonSize;
end;

procedure TJvBackgroundTreeView.CNNotify(var Message: TWMNotify);
begin
  if not Assigned(OnCustomDrawItem) then
  begin
    OnCustomDrawItem := DoCustomDrawItem;
    inherited;
    OnCustomDrawItem := nil;
  end
  else inherited;
end;

end.
