{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Parser.pas, released 2002-01-06.

The Initial Developer of the Original Code is David Polberger <dpol att swipnet dott se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): ______________________________________.

Current Version: 2.00

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Please see the accompanying documentation.
Description:
  Parser.pas provides both the IParser interface, as well as a class providing
  a default implementation. A class implementing IParser is supposed to parse
  a string, and return a tree representation represented by a TNodeTree.

  Note: Documentation for this unit can be found in Doc\Source.txt and
        Doc\Readme.txt!
-----------------------------------------------------------------------------}
// $Id$

unit JvQLinkLabelParser;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils,  
  QGraphics, 
  JvQLinkLabelTree, JvQLinkLabelTools;

type
  IDynamicNodeHandler = interface
    procedure HandleDynamicNode(out Source: string; const Node: TDynamicNode);
  end;

  IParser = interface
    function Parse(const Text: string): TNodeTree; overload;
    function Parse(const List: TStringList): TNodeTree; overload;
    procedure SetDynamicNodeHandler(Handler: IDynamicNodeHandler);
    procedure AddSourceTreeToDynamicNode(const Node: TDynamicNode;
      const Source: string);
  end;

  IElementEnumerator = interface;

  TDefaultParser = class(TInterfacedObject, IParser)
  private
    FEnum: IElementEnumerator;
    FDynamicNodeHandler: IDynamicNodeHandler;
    procedure ParseNode(const Node: TParentNode);
  protected
    function GetNodeFromTag(const Tag: string): TNode; virtual;
    procedure HandleDynamicTag(const Node: TDynamicNode);
  public
    procedure SetElementEnumerator(NewEnum: IElementEnumerator);
    function Parse(const Text: string): TNodeTree; overload;
    function Parse(const List: TStringList): TNodeTree; overload;
    procedure SetDynamicNodeHandler(Handler: IDynamicNodeHandler);
    procedure AddSourceTreeToDynamicNode(const Node: TDynamicNode;
      const Source: string);
  end;

  TElementKind = (ekBeginTag, ekEndTag, ekString);
  TElement = record
    Kind: TElementKind;
    Text: string;
  end;

  IElementEnumerator = interface
    function PopNextElement: TElement;
    function PeekNextElement: TElement;
    function IsEndReached: Boolean;
  end;

implementation

uses
  JvQResources;

//=== { TElementEnumerator } =================================================

type
  TElementEnumerator = class(TInterfacedObject, IElementEnumerator)
  private
    FText: string;
    FPosInText: Integer;
    FOldPosInText: Integer; // Used to see whether we should use our cached copy
    FCachedElement: TElement;
    FNewPosInText: Integer;
    function GetNextElement(const IncrementPos: Boolean): TElement;
  public
    constructor Create(const Text: string);
    function PopNextElement: TElement;
    function PeekNextElement: TElement;
    function IsEndReached: Boolean;
  end;

const
  OpenTag = '<';
  CloseTag = '>';
  EndMarker = '/';

constructor TElementEnumerator.Create(const Text: string);
begin
  inherited Create;
  FPosInText := 1;
  FOldPosInText := -1;
  FText := Text;
end;

function TElementEnumerator.GetNextElement(const IncrementPos: Boolean): TElement;

  function GetElementKind: TElementKind;
  var
    TempString: string;
  begin
    TempString := Copy(FText, FPosInText, 2);

    if Copy(TempString, 1, 2) = OpenTag + EndMarker then // "</..."
      Result := ekEndTag
    else
    if Copy(TempString, 1, 1) = OpenTag then // "<..."
      Result := ekBeginTag
    else
      Result := ekString;
  end;

  function GetElementText(const Kind: TElementKind): string;
  var
    StartPos: Integer;
    EndPos: Integer;
    Padding: Integer;

    procedure FindNewTagPos(const I: Integer);
    begin
      Inc(StartPos, I); // To get in front of the "<" or "</" character(s)
      EndPos := StartPos;
      while (EndPos < Length(FText)) and (FText[EndPos] <> CloseTag) do
        Inc(EndPos);
      Inc(EndPos);
      Padding := 1;
    end;

  begin
    StartPos := FPosInText;
    EndPos := FPosInText;
    Padding := 0;

    case Kind of
      ekBeginTag:
        FindNewTagPos(1);
      ekEndTag:
        FindNewTagPos(2);
      ekString:
        while (EndPos <= Length(FText)) and (FText[EndPos] <> OpenTag) do
          Inc(EndPos);
    end;

    Result := Copy(FText, StartPos, (EndPos - StartPos - Padding));
    FNewPosInText := EndPos;
  end;

begin
  if IsEndReached then
    raise EParserError.CreateRes(@RsENoMoreElementsToReturn);

  if FOldPosInText = FPosInText then // Use cached element
    Result := FCachedElement
  else
  begin
    FOldPosInText := FPosInText;

    Result.Kind := GetElementKind;
    Result.Text := GetElementText(Result.Kind);

    FCachedElement := Result;
  end;

  if IncrementPos then
    FPosInText := FNewPosInText;
end;

function TElementEnumerator.IsEndReached: Boolean;
begin
  Result := FPosInText > Length(FText);
end;

function TElementEnumerator.PeekNextElement: TElement;
begin
  Result := GetNextElement(False);
end;

function TElementEnumerator.PopNextElement: TElement;
begin
  Result := GetNextElement(True);
end;

//=== { TDefaultParser } =====================================================

procedure TDefaultParser.AddSourceTreeToDynamicNode(const Node: TDynamicNode;
  const Source: string);
var
  Parser: TDefaultParser;
  Tree: TNodeTree;
  I: Integer;
begin
  Tree := nil;
  try
    Parser := TDefaultParser.Create;
    try
      Tree := Parser.Parse(Source);
    finally
      Parser.Free;
    end;

    Tree.Root.OwnsChildren := False;
    for I := 0 to Tree.Root.Children.Count - 1 do
      Node.AddChild(Tree.Root.Children[I], Tree.Root);
  finally
    Tree.Free;
  end;
end;

function TDefaultParser.GetNodeFromTag(const Tag: string): TNode;
type
  TTag = (ttBold, ttItalic, ttUnderline, ttColor,
    ttLink, ttLineBreak, ttParagraphBreak, ttDynamic);
var
  CurrentTag: TTag;
  UnknownTag: Boolean;

  // Bianconi
  function GetColorFromTag: TColor;
  var
    sVar: string;
  begin
    Result := clNone;
    sVar := Copy(Tag, Pos('=', Tag) + 1, Length(Tag));
    try
      Result := StringToColor(sVar);
    except // Only to avoid raise an exception on invalid color
    end;
  end;
  // End of Bianconi

  function GetTagFromString: TTag;
  const
    TagStrings: array [TTag] of PChar =
     ('B',
      'I',
      'U',
      'COLOR=', // Bianconi
      'LINK',
      'BR',
      'P',
      'DYNAMIC');
    DontCare = 0;
  begin
    UnknownTag := False;
    // Bianconi
    for Result := Low(TTag) to High(TTag) do
      if (AnsiUpperCase(Tag) = TagStrings[Result]) or
        (Copy(AnsiUpperCase(Tag), 1, Length(TagStrings[Result])) = 'COLOR=') then
        Exit;
    //End of Bianconi
    Result := TTag(DontCare); // To get rid of a compiler warning
    UnknownTag := True;
  end;

begin
  { Descendant parsers should override this routine, call inherited and add
    support for proprietary tags (using custom node objects, which descend from
    TNode). Note that appropriate modifications need to be made to the renderer
    as well, either by creating a new class which implements the IRenderer
    interface, or by extending the TDefaultRenderer class. See this class for
    more information. }
  CurrentTag := GetTagFromString;

  if not UnknownTag then
    case CurrentTag of
      ttBold:
        Result := TStyleNode.Create(fsBold);
      ttItalic:
        Result := TStyleNode.Create(fsItalic);
      ttUnderline:
        Result := TStyleNode.Create(fsUnderline);
      // Bianconi
      ttColor:
        Result := TColorNode.Create(GetColorFromTag);
      // End of Bianconi
      ttLink:
        Result := TLinkNode.Create;
      ttLineBreak:
        Result := TActionNode.Create(atLineBreak);
      ttParagraphBreak:
        Result := TActionNode.Create(atParagraphBreak);
      ttDynamic:
        Result := TDynamicNode.Create;
    else
      Result := TUnknownNode.Create(Tag);
    end
  else
    Result := TUnknownNode.Create(Tag);

  if CurrentTag = ttDynamic then
    HandleDynamicTag(Result as TDynamicNode);
end;

procedure TDefaultParser.HandleDynamicTag(const Node: TDynamicNode);
var
  Source: string;
begin
  if Assigned(FDynamicNodeHandler) then
  begin
    FDynamicNodeHandler.HandleDynamicNode(Source, Node);
    if Source <> '' then
      AddSourceTreeToDynamicNode(Node, Source);
  end;
end;

function TDefaultParser.Parse(const List: TStringList): TNodeTree;
begin
  Result := Parse(List.Text);
end;

function TDefaultParser.Parse(const Text: string): TNodeTree;
begin
  Result := TNodeTree.Create;
  FEnum := TElementEnumerator.Create(TStringTools.RemoveCRLF(Text));
  try
    ParseNode(Result.Root);
  finally
    FEnum := nil;
  end;
end;

procedure TDefaultParser.ParseNode(const Node: TParentNode);
var
  Element: TElement;
  NewNode: TNode;

  function EndReached: Boolean;
  begin
    Result := FEnum.IsEndReached or (FEnum.PeekNextElement.Kind = ekEndTag);
  end;

  function IsNodeContainer(const Node: TNode; const Element: TElement): Boolean;
  begin
    { Returns whether the given node is can contain other elements and thus
      descends from TParentNode. Descendants from this class begin with <?> and
      end with </?> (for example, <B> and </B>. Nodes that descend from
      TActionNode shouldn't be terminated with </?> (for example, <P>). Note
      that TDynamicNode is special; while it descends from TParentNode, it never
      contains children at parse-time, thus we shouldn't wait for a redundant
      </DYNAMIC>. Instead, its contents are supplied before it's rendered by
      compiled program code. }
    Result := (Element.Kind = ekBeginTag) and
      (Node is TParentNode) and not (Node is TDynamicNode);
  end;

begin
  while not EndReached do
  begin
    Element := FEnum.PopNextElement;

    case Element.Kind of
      ekString:
        NewNode := TStringNode.Create(Element.Text);
      ekBeginTag:
        NewNode := GetNodeFromTag(Element.Text);
    else
      raise EParserError.CreateRes(@RsEUnsupportedState);
    end;

    if (Node.GetNodeType = ntRootNode) then
      Node.AddChild(NewNode, TRootNode(Node))
    else
      Node.AddChild(NewNode, Node.Root);

    if IsNodeContainer(NewNode, Element) then
      ParseNode(NewNode as TParentNode);
  end;

  { When we have reached the end of a tag (</LINK> for example) we don't enter
    the main body. We have called FEnum.PeekElement and have determined (in
    EndReached in this routine) that the next element to be returned by FEnum.
    PopElement will be an end-tag. Thus, we exit this routine and return either
    to another copy of ParseNode (if we've been called recursively) or to Parse.

    However, if we only check the next element to be returned using PeekElement,
    it won't be popped off our "stack", which is what we do here. If we hadn't
    popped it here, EndReached would've returned True in all other incarnations
    of this routine in the call stack; thus, one single end-tag would've caused
    the whole parse process to stop. This is obviously not what we want. }
  if not FEnum.IsEndReached then
    FEnum.PopNextElement;
end;

procedure TDefaultParser.SetDynamicNodeHandler(
  Handler: IDynamicNodeHandler);
begin
  FDynamicNodeHandler := Handler;
end;

procedure TDefaultParser.SetElementEnumerator(NewEnum: IElementEnumerator);
begin
  if Assigned(NewEnum) then
    FEnum := NewEnum;
end;

end.

