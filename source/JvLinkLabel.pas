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

// Bianconi
// Known Bugs

unit JvLinkLabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvLinkLabelParser, JvLinkLabelRenderer, JvLinkLabelTree, JVCLVer, JvTypes;

type
  ELinkLabelError = class(EJVCLException);

  TLinkClickEvent = procedure(Sender: TObject; LinkNumber: Integer;
    LinkText: string) of object;
  TDynamicTagInitEvent = procedure(Sender: TObject; out Source: string;
    Number: Integer) of object;

  TJvCustomLinkLabel = class(TGraphicControl, IDynamicNodeHandler)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FCaption: TCaption;
    FText: TStrings;
    FRenderer: IRenderer;
    FActiveLinkNode: TLinkNode;
    FHotLinks: Boolean;
//    FRect: TRect;
    FAutoHeight: Boolean;
    FMarginWidth: Integer;
    FMarginHeight: Integer;
    FOriginalCursor: TCursor;
    FOnCaptionChanged: TNotifyEvent;
    FOnLinkClick: TLinkClickEvent;
    FOnDynamicTagInit: TDynamicTagInitEvent;
    FParser: IParser;
    FLayout : TTextLayout;  // Bianconi
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
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetMarginHeight(const Value: Integer);
    procedure SetMarginWidth(const Value: Integer);
    procedure SetText(const Value: TStrings);
    procedure SetLayout(AValue : TTextLayout);     // Bianconi
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
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Caption: TCaption read FCaption write SetCaption;
    property Text: TStrings read FText write SetText;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Layout : TTextLayout read FLayout write SetLayout;                // Bianconi
    property LinkColor: TColor read GetLinkColor write SetLinkColor;
    property LinkColorClicked: TColor read GetLinkColorClicked write SetLinkColorClicked;
    property LinkColorHot: TColor read GetLinkColorHot write SetLinkColorHot;
    property LinkStyle: TFontStyles read GetLinkStyle write SetLinkStyle;
    property HotLinks: Boolean read FHotLinks write FHotLinks;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property MarginWidth: Integer read FMarginWidth write SetMarginWidth;
    property MarginHeight: Integer read FMarginHeight write SetMarginHeight;
    property OnDynamicTagInit: TDynamicTagInitEvent read FOnDynamicTagInit write FOnDynamicTagInit;
    property OnCaptionChanged: TNotifyEvent read FOnCaptionChanged write FOnCaptionChanged;
    property OnLinkClick: TLinkClickEvent read FOnLinkClick write FOnLinkClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure UpdateDynamicTag(Number: Integer; const Source: string);
    function GetDynamicTagContents(Number: Integer): string;
  end;

  TJvLinkLabel = class(TJvCustomLinkLabel)
  // (rom) published was missing here
  published
    property AboutJVCL;
    property Caption;
    property Text;
    property Anchors;
    property Transparent;
    property Layout;                   // Bianconi
    property LinkColor;
    property LinkColorClicked;
    property LinkColorHot;
    property LinkStyle;
    property HotLinks;
    property AutoHeight;
    property MarginWidth;
    property MarginHeight;

    property OnDynamicTagInit;
    property OnCaptionChanged;
    property OnLinkClick;

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

const
  crNewLinkHand = 1;

procedure TJvCustomLinkLabel.ActivateLinkNodeAtPos(const P: TPoint; State: TLinkState);
var
  NodeAtPoint: TLinkNode;
  ptP : TPoint;  // Bianconi #2
  TmpRect : TRect;

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
  // Bianconi #2
  // Changes Control's canvas point to relative coordinates
  ptP := Point(P.X - FNodeTree.Root.StartingPoint.X,P.Y - FNodeTree.Root.StartingPoint.Y);

  if FNodeTree.IsPointInNodeClass(ptP, TLinkNode) then
  begin
    NodeAtPoint := FNodeTree.GetNodeAtPointOfClass(ptP, TLinkNode) as TLinkNode;
    if Assigned(NodeAtPoint) and IsNewNode then
    begin
      DeactivateActiveLinkNode;
      NodeAtPoint.State := State;
      FActiveLinkNode := NodeAtPoint;
      TmpRect := ClientRect;
      InflateRect(TmpRect,-FMarginWidth,-FMarginHeight);
      FRenderer.RenderNode(Canvas, TmpRect, NodeAtPoint);
    end;
  end;
end;

procedure TJvCustomLinkLabel.DeactivateActiveLinkNode;
var
  TmpRect : TRect;
begin
  if Assigned(FActiveLinkNode) then
  try
    FActiveLinkNode.State := lsNormal;
    TmpRect := ClientRect;
    InflateRect(TmpRect,-FMarginWidth,-FMarginHeight);
    FRenderer.RenderNode(Canvas, TmpRect, FActiveLinkNode);
  finally
    FActiveLinkNode := nil;
  end;
end;

procedure TJvCustomLinkLabel.CMFontChanged(var Msg: TMessage);

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

procedure TJvCustomLinkLabel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FHotLinks and not IsActiveLinkNodeClicked then
    DeactivateActiveLinkNode;
end;

procedure TJvCustomLinkLabel.CMTextChanged(var Msg: TMessage);
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

  FLayout := tlTop;              // Bianconi

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
  inherited Loaded;
  FOriginalCursor := Cursor;
  Resize;
end;

procedure TJvCustomLinkLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  ActivateLinkNodeAtPos(Point(X, Y), lsClicked);
end;

procedure TJvCustomLinkLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ptP : TPoint;  // Bianconi #2
begin
  inherited MouseMove(Shift, X, Y);

  // Bianconi #2
  ptP := Point(X - FNodeTree.Root.StartingPoint.X,Y - FNodeTree.Root.StartingPoint.Y);

  if FNodeTree.IsPointInNodeClass(ptP, TLinkNode) then
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

procedure TJvCustomLinkLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NodeAtPoint: TLinkNode;
  ptP : TPoint;  // Bianconi #2
begin
  inherited MouseUp(Button, Shift, X, Y);

  // Bianconi #2
  ptP := Point(X - FNodeTree.Root.StartingPoint.X,Y - FNodeTree.Root.StartingPoint.Y);

  if FNodeTree.IsPointInNodeClass(ptP, TLinkNode) then
  begin
    NodeAtPoint := FNodeTree.GetNodeAtPointOfClass(ptP, TLinkNode) as TLinkNode;
    if Assigned(NodeAtPoint) then
      DoLinkClicked(NodeAtPoint.Number, NodeAtPoint.Text);
  end;

  DeactivateActiveLinkNode;
end;

// Bianconi
procedure TJvCustomLinkLabel.Paint;
var
  TmpBmp : TBitMap;
  TmpRect : TRect;
begin
  inherited Paint;
  TmpBmp := nil;
  if Assigned(FNodeTree) then
  begin
    try
      Canvas.Font := Font;
      if not Transparent then
      begin
        // repaint canvas
        Canvas.Brush.Color := Color;
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(ClientRect);
      end;
      Canvas.Brush.Style := bsClear;

      TmpBmp := TBitMap.Create;
      TmpRect := ClientRect;
      TmpBmp.Height := TmpRect.Bottom - (FMarginHeight shl 1) + 1;  // TmpRect.Top = 0, ignore it
      TmpBmp.Width  := TmpRect.Right - (FMarginWidth shl 1) + 1;    // TmpRect.left = 0, ignore it
      TmpBmp.Canvas.Font.Assign(Canvas.Font);
      TmpBmp.Canvas.Pen.Assign(Canvas.Pen);
      // Initialize background of canvas
      // You should choose a color diferent from your links colors to made it visible
      TmpBmp.Canvas.Brush.Color := Color;
      TmpBmp.Canvas.Brush.Style := bsSolid;
      TmpBmp.Canvas.FillRect(Rect(0,0,TmpBmp.Width - 1,TmpBmp.Height - 1));

      TmpBmp.Transparent := True;
      TmpBmp.TransparentMode := tmAuto;

      // Set new start point
      // The new start point is relative to temporary canvas, Left & Top Corner
      FNodeTree.Root.StartingPoint := Point(0,0);  // Bianconi #2
      FRenderer.RenderTree(TmpBmp.Canvas, Rect(0,0,TmpBmp.Width - 1,TmpBmp.Height - 1), FNodeTree);

      //  Set new height e don't draw in this pass.
      //  Wait for next paint event.
      //  Allow correctly layout position and improve some performance
      if FAutoHeight and
         (Align in [alNone, alTop, alBottom]) and
         (ClientHeight <> (FRenderer.GetTextHeight + (FMarginHeight shl 1)) ) then
      begin
        ClientHeight := FRenderer.GetTextHeight + (FMarginHeight shl 1);
      end
      else
      begin
        TmpRect := ClientRect;
        InflateRect(TmpRect,-FMarginWidth,-FMarginHeight);

        case FLayout of
          tlTop    :
            begin
              // Nothing to do
            end; // tlTop
          tlCenter :
            begin
              TmpRect.Top := TmpRect.Top +
                             (TmpRect.Bottom - TmpRect.Top - FRenderer.GetTextHeight) div 2;
              if( TmpRect.Top < FMarginHeight ) then
              begin
                TmpRect.Top := FMarginHeight;
              end;
            end; // tlCenter
          tlBottom :
            begin
              TmpRect.Top := TmpRect.Bottom - FRenderer.GetTextHeight;
              if( TmpRect.Top < FMarginHeight ) then
              begin
                TmpRect.Top := FMarginHeight;
              end;
            end; // tlBottom
        end;
        // Adjust Root start point relative to control's canvas.
        FNodeTree.Root.StartingPoint := Point(TmpRect.Left, TmpRect.Top);  // Bianconi #2
        Canvas.Draw(TmpRect.Left,TmpRect.Top,TmpBmp);
      end;
    finally
      if( Assigned(TmpBmp) ) then TmpBmp.Free;
    end;
  end;
end;
// End of Bianconi

procedure TJvCustomLinkLabel.Resize;
begin
  inherited Resize;
// Bianconi -> ClientRect.Bottom - FMarginHeight
// MarginHeight is applied on Top and Bottom values, exactly as MargimWidth
//  FRect := Rect(ClientRect.Left + FMarginWidth, ClientRect.Top + FMarginHeight,
//    ClientRect.Right - FMarginWidth, ClientRect.Bottom - FMarginHeight);
// end of Bianconi
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

procedure TJvCustomLinkLabel.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
  SetCaption(FText.Text);
end;

// Bianconi
procedure TJvCustomLinkLabel.SetLayout(AValue : TTextLayout);
begin
  if( FLayout <> AValue ) then
  begin
    FLayout := AValue;
    Invalidate;
  end;
end;
// end of Bianconi

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

