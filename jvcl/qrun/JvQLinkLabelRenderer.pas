{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Renderer.pas, released 2002-01-06.

The Initial Developer of the Original Code is David Polberger <dpol@swipnet.se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): ______________________________________.

Current Version: 2.00

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Please see the accompanying documentation.
Description:
  Renderer.pas provides both the IRenderer interface, as well as a class
  providing a default implementation. A class implementing the IRenderer
  interface is supposed to render the output of the supplied TNodeTree to the
  screen.

  Note: Documentation for this unit can be found in Doc\Source.txt and
        Doc\Readme.txt!
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQLinkLabelRenderer;

interface

uses
  Classes, SysUtils,
  
  
  QGraphics, Types,
  
  JvQLinkLabelTree, JvQLinkLabelTextHandler, JvQTypes;

type
  ERendererError = class(EJVCLException);

  IRenderer = interface
    function GetLinkColor: TColor;
    procedure SetLinkColor(const Value: TColor);
    function GetLinkColorClicked: TColor;
    procedure SetLinkColorClicked(const Value: TColor);
    function GetLinkColorHot: TColor;
    procedure SetLinkColorHot(const Value: TColor);
    function GetLinkStyle: TFontStyles;
    procedure SetLinkStyle(const Value: TFontStyles);

    procedure RenderTree(const Canvas: TCanvas; Rect: TRect; const Tree: TNodeTree);
    procedure RenderNode(const Canvas: TCanvas; Rect: TRect;
      const Node: TAreaNode);
    function GetTextHeight: Integer;
    property LinkColor: TColor read GetLinkColor write SetLinkColor;
    property LinkColorClicked: TColor read GetLinkColorClicked write SetLinkColorClicked;
    property LinkColorHot: TColor read GetLinkColorHot write SetLinkColorHot;
    property LinkStyle: TFontStyles read GetLinkStyle write SetLinkStyle;
  end;

  TDefaultRenderer = class(TInterfacedObject, IRenderer, IStartingPosObserver)
  private
    FLinkColor: TColor;
    FLinkColorClicked: TColor;
    FLinkColorHot: TColor;
    FLinkStyle: TFontStyles;
    FTextHandler: ITextHandler;
    FTextHeight: Integer;
    function GetLinkColor: TColor;
    procedure SetLinkColor(const Value: TColor);
    function GetLinkColorClicked: TColor;
    procedure SetLinkColorClicked(const Value: TColor);
    function GetLinkColorHot: TColor;
    procedure SetLinkColorHot(const Value: TColor);
    function GetLinkStyle: TFontStyles;
    procedure SetLinkStyle(const Value: TFontStyles);
  protected
    procedure DoRenderNode(const Node: TAreaNode; Styles: TFontStyles;
      Color: TColor); virtual;
    procedure StartingPosUpdated(PosX, PosY: Integer; const Node: TAreaNode);
    function TranslateColor(const Color: TColor): TColor; virtual;
  public
    procedure RenderTree(const Canvas: TCanvas; Rect: TRect; const Tree: TNodeTree);
    procedure RenderNode(const Canvas: TCanvas; Rect: TRect;
      const Node: TAreaNode);
    function GetTextHeight: Integer;
    property LinkColor: TColor read GetLinkColor write SetLinkColor;
    property LinkColorClicked: TColor read GetLinkColorClicked write SetLinkColorClicked;
    property LinkColorHot: TColor read GetLinkColorHot write SetLinkColorHot;
    property LinkStyle: TFontStyles read GetLinkStyle write SetLinkStyle;
  end;

implementation

procedure TDefaultRenderer.DoRenderNode(const Node: TAreaNode;
  Styles: TFontStyles; Color: TColor);
var
  I: Integer;
  ChildNode: TNode;
  NewStyles: TFontStyles;
  NewColor: TColor;
begin
  if FTextHandler.IsPosCurrent then
    Node.StartingPoint := Point(FTextHandler.GetPosX, FTextHandler.GetPosY)
  else
    FTextHandler.AddStartingPosObserver(Self, Node);

  Node.Styles := Styles;
  Node.Color := TranslateColor(Color);

  for I := 0 to Node.Children.Count - 1 do
  begin
    ChildNode := Node.Children[I];
    NewColor := TranslateColor(Color);
    NewStyles := Styles;

    case ChildNode.GetNodeType of
      ntStringNode:
        FTextHandler.TextOut(ChildNode as TStringNode, NewStyles, NewColor);
      ntActionNode:
        case (ChildNode as TActionNode).Action of
          atLineBreak:
            FTextHandler.DoLineBreak;
          atParagraphBreak:
            FTextHandler.DoParagraphBreak;
        end;
      ntStyleNode:
        NewStyles := Styles + [(ChildNode as TStyleNode).Style];
      // Bianconi
      ntColorNode:
        NewColor := (ChildNode as TColorNode).Color;
      // End of Bianconi
      ntLinkNode:
        begin
          NewStyles := Styles + LinkStyle;
          NewColor := LinkColor;
        end;
    end;

    if ChildNode is TAreaNode then
      DoRenderNode(TAreaNode(ChildNode), NewStyles, NewColor);
  end;
end;

function TDefaultRenderer.GetLinkColorClicked: TColor;
begin
  Result := FLinkColorClicked;
end;

function TDefaultRenderer.GetLinkColor: TColor;
begin
  Result := FLinkColor;
end;

function TDefaultRenderer.GetLinkStyle: TFontStyles;
begin
  Result := FLinkStyle;
end;

procedure TDefaultRenderer.RenderNode(const Canvas: TCanvas; Rect: TRect;
  const Node: TAreaNode);
begin
  Canvas.Start;
  FTextHandler := TTextHandler.Create(Rect,
    Node.StartingPoint.X, Node.StartingPoint.Y, Canvas);
  // End of Bianconi #2
  try
    DoRenderNode(Node, Node.Styles, Node.Color);
    FTextHandler.EmptyBuffer;
    FTextHeight := FTextHandler.GetTextHeight;
  finally
    FTextHandler := nil;
  end;
  Canvas.Stop;
end;

procedure TDefaultRenderer.RenderTree(const Canvas: TCanvas; Rect: TRect;
  const Tree: TNodeTree);
begin
  Tree.Root.StartingPoint := Point(Rect.Left, Rect.Top);
  RenderNode(Canvas, Rect, Tree.Root);
  Tree.Root.RetrieveRectsOfTLinkNodeChildren;
end;

procedure TDefaultRenderer.SetLinkColorClicked(const Value: TColor);
begin
  FLinkColorClicked := Value;
end;

procedure TDefaultRenderer.SetLinkColor(const Value: TColor);
begin
  FLinkColor := Value;
end;

procedure TDefaultRenderer.SetLinkStyle(const Value: TFontStyles);
begin
  FLinkStyle := Value;
end;

function TDefaultRenderer.GetLinkColorHot: TColor;
begin
  Result := FLinkColorHot;
end;

procedure TDefaultRenderer.SetLinkColorHot(const Value: TColor);
begin
  FLinkColorHot := Value;
end;

function TDefaultRenderer.TranslateColor(const Color: TColor): TColor;
begin
  case Color of
    clNormalLink:
      Result := FLinkColor;
    clClickedLink:
      Result := FLinkColorClicked;
    clHotLink:
      Result := FLinkColorHot;
  else
    Result := Color;
  end;
end;

procedure TDefaultRenderer.StartingPosUpdated(PosX, PosY: Integer;
  const Node: TAreaNode);
begin
  Node.StartingPoint := Point(PosX, PosY);
end;

function TDefaultRenderer.GetTextHeight: Integer;
begin
  Result := FTextHeight;
end;

end.
