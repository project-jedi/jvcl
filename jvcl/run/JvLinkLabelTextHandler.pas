{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: TextHandler.pas, released 2002-01-06.

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
  This unit, and its supporting types and classes, exist for the sole purpose of
  supporting words broken into different nodes in the tree. These strings are
  rendered correctly, but they are unfortunattreated as different words when
  the time comes to do word-wrapping. This means that one substring of a word
  gets placed on one row, and the others on different rows. Consider this word:

  <B>Te</B>st

  In this case, "Te" would be placed on the first row while "st" would be placed
  on the second row, if we were short on space. The first TJvLinkLabel engine
  did not support this at all, while the second engine supported it most of the
  time, although using a hack.

  One could argue that few would ever want to have something like the above
  rendered, but all current browsers support it, and it _is_ used on the
  Internet to achieve special formatting. The syntax clearly supports words
  with characters styled differently; if the engine didn't support this, it
  would be a technical shortcoming.

  Note: Documentation for this unit can be found in Doc\Source.txt and
        Doc\Readme.txt!
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvLinkLabelTextHandler;

interface

uses
  Classes, SysUtils,
  {$IFDEF VCL}
  Graphics, Windows,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, Types,
  {$ENDIF VisualCLX}
  JvLinkLabelTree, JvLinkLabelTools, JvTypes;

type
  ETextHandlerError = class(EJVCLException);

  IStartingPosObserver = interface
    procedure StartingPosUpdated(PosX, PosY: Integer; const Node: TAreaNode);
  end;

  ITextHandler = interface
    function GetPosX: Integer;
    function GetPosY: Integer;
    function GetLineHeight: Integer;
    function GetCanvas: TCanvas;

    procedure TextOut(Node: TStringNode; Style: TFontStyles; Color: TColor);
    procedure DoParagraphBreak;
    procedure DoLineBreak;
    procedure EmptyBuffer;
    function GetTextHeight: Integer;
    function IsPosCurrent: Boolean;
    procedure AddStartingPosObserver(Observer: IStartingPosObserver;
      Node: TAreaNode);

    property PosX: Integer read GetPosX;
    property PosY: Integer read GetPosY;
    property LineHeight: Integer read GetLineHeight;
    property Canvas: TCanvas read GetCanvas;
  end;

  TTextElementList = class;
  TNodeObserverList = class;
  TTextHandler = class(TInterfacedObject, ITextHandler)
  private
    FPosX: Integer;
    FPosY: Integer;
    FList: TTextElementList;
    FRect: TRect;
    FCanvas: TCanvas;
    FLineHeight: Integer;
    FObservers: TNodeObserverList;
    function GetPosX: Integer;
    function GetPosY: Integer;
    function GetLineHeight: Integer;
    function GetCanvas: TCanvas;
  public
    constructor Create(const Rect: TRect; InitialX, InitialY: Integer;
      const Canvas: TCanvas);
    destructor Destroy; override;
    procedure TextOut(Node: TStringNode; Style: TFontStyles; Color: TColor);
    procedure DoParagraphBreak;
    procedure DoLineBreak;
    procedure EmptyBuffer;
    function GetTextHeight: Integer;
    function IsPosCurrent: Boolean;
    procedure AddStartingPosObserver(Observer: IStartingPosObserver;
      Node: TAreaNode);
  end;

  TParentTextElement = class(TObject)
  end;

  TStringElement = class(TParentTextElement)
  private
    FNode: TStringNode;
    FStyle: TFontStyles;
    FColor: TColor;
  public
    constructor Create(const Node: TStringNode; Style: TFontStyles; Color: TColor);
    function BeginsWithSpace: Boolean;
    function EndsWithSpace: Boolean;
    property Node: TStringNode read FNode;
    property Style: TFontStyles read FStyle;
    property Color: TColor read FColor;
  end;

  TActionElement = class(TParentTextElement)
  private
    FActionType: TActionType;
  public
    constructor Create(ActionType: TActionType);
    property ActionType: TActionType read FActionType;
  end;

  TTextElementList = class(TObject)
  private
    FList: TList;
    function Get(Index: Integer): TParentTextElement;
    function GetCount: Integer;
  protected
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddStringElement(const Node: TStringNode; Style: TFontStyles; Color: TColor);
    procedure AddParagraphBreak;
    procedure AddLineBreak;
    property Items[Index: Integer]: TParentTextElement read Get; default;
    property Count: Integer read GetCount;
  end;

  PNodeObserver = ^TNodeObserver;
  TNodeObserver = record
    Observer: IStartingPosObserver;
    ParentNode: TAreaNode;
    FirstStringNode: TStringNode;
  end;

  TNodeObserverList = class(TOwnerPointerList)
  private
    function Get(Index: Integer): PNodeObserver;
    procedure Put(Index: Integer; const Value: PNodeObserver);
  public
    procedure AddObserver(Observer: IStartingPosObserver; ParentNode: TAreaNode;
      FirstStringNode: TStringNode);
    procedure RemoveObserver(Item: PNodeObserver);
    function IndexOfStringNode(Node: TStringNode): Integer;
    property Items[Index: Integer]: PNodeObserver read Get write Put; default;
  end;

implementation

uses
  JvResources;

//=== TWordEnumerator ========================================================

const
  Space = ' ';

type
  IWordEnumerator = interface
    procedure SetText(const Text: string);
    function GetText: string;
    function GetCount: Integer;

    function PopNext: string;
    function PeekNext: string;
    function HasNext: Boolean;
    procedure Reset;
    property Text: string read GetText write SetText;
    property Count: Integer read GetCount;
  end;

  TWordEnumerator = class(TInterfacedObject, IWordEnumerator)
  private
    FPos: Integer;
    FText: string;
    FCount: Integer;
    procedure SetText(const Text: string);
    function GetText: string;
    function GetNext(const IncrementPos: Boolean): string;
    function GetCount: Integer;
  public
    constructor Create(const Text: string);
    function PopNext: string;
    function PeekNext: string;
    function HasNext: Boolean;
    procedure Reset;
  end;

constructor TWordEnumerator.Create(const Text: string);
begin
  inherited Create;
  Reset;
  SetText(Text);
end;

function TWordEnumerator.GetCount: Integer;
begin
  Result := FCount;
end;

function TWordEnumerator.GetNext(const IncrementPos: Boolean): string;
var
  StartPos: Integer;
  EndPos: Integer;
begin
  if not HasNext then
    raise ETextHandlerError.CreateRes(@RsENoMoreWords);

  StartPos := FPos;
  EndPos := FPos;
  while (EndPos <= Length(FText)) and (FText[EndPos] <> Space) do
    Inc(EndPos);
  Inc(EndPos);
  Result := Copy(FText, StartPos, EndPos - StartPos);

  if IncrementPos then
  begin
    FPos := EndPos;
    Inc(FCount);
  end;
end;

function TWordEnumerator.GetText: string;
begin
  Result := FText;
end;

function TWordEnumerator.HasNext: Boolean;
begin
  Result := FPos <= Length(FText);
end;

function TWordEnumerator.PeekNext: string;
begin
  Result := GetNext(False);
end;

function TWordEnumerator.PopNext: string;
begin
  Result := GetNext(True);
end;

procedure TWordEnumerator.Reset;
begin
  FPos := 1;
  FCount := 0;
end;

procedure TWordEnumerator.SetText(const Text: string);
begin
  FText := Text;
  FCount := 0;
end;

//=== TTextElementList =======================================================

constructor TTextElementList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TTextElementList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TTextElementList.AddLineBreak;
begin
  FList.Add(TActionElement.Create(atLineBreak));
end;

procedure TTextElementList.AddParagraphBreak;
begin
  FList.Add(TActionElement.Create(atParagraphBreak));
end;

procedure TTextElementList.AddStringElement(const Node: TStringNode;
  Style: TFontStyles; Color: TColor);
begin
  FList.Add(TStringElement.Create(Node, Style, Color));
end;

procedure TTextElementList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    Get(I).Free;
  FList.Clear;
end;

function TTextElementList.Get(Index: Integer): TParentTextElement;
begin
  Result := FList[Index];
end;

function TTextElementList.GetCount: Integer;
begin
  Result := FList.Count;
end;

//=== TTextHandler ===========================================================

constructor TTextHandler.Create(const Rect: TRect; InitialX, InitialY: Integer;
  const Canvas: TCanvas);
var
  TempFontStyle: TFontStyles;
const
  // (rom) i have seen other letter combinations elsewhere
  // Bianconi #2
  // MaximumHeightString = 'fg';
  MaximumHeightString = 'Yy';
begin
  inherited Create;
  FRect := Rect;
  FPosX := InitialX;
  FPosY := InitialY;
  FCanvas := Canvas;

  { TextHeight returns slightly different values depending on whether fsBold is
    in Canvas.Font.Style. This is not acceptable, as it's important that
    FLineHeight stays constant between TTextHandler instances. Thus we set
    Canvas.Font.Style to [] before calculating the line height. }
  TempFontStyle := Canvas.Font.Style;
  Canvas.Font.Style := [];
  try
    FLineHeight := Canvas.TextHeight(MaximumHeightString);
  finally
    Canvas.Font.Style := TempFontStyle;
  end;

  FList := TTextElementList.Create;
  FObservers := TNodeObserverList.Create;
end;

destructor TTextHandler.Destroy;
begin
  FObservers.Free;
  FList.Free;
  inherited Destroy;
end;

procedure TTextHandler.AddStartingPosObserver(
  Observer: IStartingPosObserver; Node: TAreaNode);
begin
  FObservers.AddObserver(Observer, Node,
    Node.GetFirstNodeOfClass(TStringNode) as TStringNode);
end;

procedure TTextHandler.DoLineBreak;
begin
  FList.AddLineBreak;
  EmptyBuffer;
end;

procedure TTextHandler.DoParagraphBreak;
begin
  FList.AddParagraphBreak;
  EmptyBuffer;
end;

function TTextHandler.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TTextHandler.GetLineHeight: Integer;
begin
  Result := FLineHeight;
end;

function TTextHandler.GetPosX: Integer;
begin
  Result := FPosX;
end;

function TTextHandler.GetPosY: Integer;
begin
  Result := FPosY;
end;

function TTextHandler.GetTextHeight: Integer;
begin
  Result := FPosY + FLineHeight;
end;

function TTextHandler.IsPosCurrent: Boolean;
begin
  Result := FList.Count = 0;
end;

procedure TTextHandler.EmptyBuffer;
var
  I: Integer;
  Element: TStringElement;
  Enum: IWordEnumerator;
  Buffer: string;
  NextWord: string;
  NextWordWidth: Integer;
  Width: Integer;
  SpaceInfo: TSpaceInfo;

  function GetWidth(out SpaceInfo: TSpaceInfo): Integer;
  var
    J: Integer;
    PrivateEnum: IWordEnumerator;
    WordElement: string;
    CurrentElement: TStringElement;
  begin
    { If the width of the first word has already been included in the count,
      don't count it again; thus, return 0. }
    if Element.Node.FirstWordWidthRetrieved and (Enum.Count = 1) then
      Result := 0
    else
      Result := FCanvas.TextWidth(NextWord);

    { Update record with default information; might be overwritten later if
      we're dealing with quite special markup. }
    with SpaceInfo do
    begin
      LastWordEndsWithSpace := TStringTools.EndsWith(NextWord);
      SpaceWidth := FCanvas.TextWidth(Space);
    end;

    if (not Enum.HasNext) and not (Element.Node.FirstWordWidthRetrieved and (Enum.Count = 1)) then
    begin
      J := I + 1;

      while (J < FList.Count) and
        (FList[J - 1] is TStringElement) and
        (FList[J] is TStringElement) and
        (not TStringElement(FList[J - 1]).EndsWithSpace) and
        (not TStringElement(FList[J]).BeginsWithSpace) do // Part of the same word
      begin
        CurrentElement := TStringElement(FList[J]);
        PrivateEnum := TWordEnumerator.Create(CurrentElement.Node.Text);

        FCanvas.Font.Style := CurrentElement.Style;
        WordElement := PrivateEnum.PopNext;
        Inc(Result, FCanvas.TextWidth(WordElement));
        CurrentElement.Node.FirstWordWidthRetrieved := True;

        // Update record
        if J = FList.Count - 1 then
          with SpaceInfo do
          begin
            LastWordEndsWithSpace := TStringTools.EndsWith(WordElement);
            SpaceWidth := FCanvas.TextWidth(Space);
          end;

        // We're only ínterested in the first word; let's break if there are more
        if PrivateEnum.HasNext then
          Break;
        Inc(J);
      end;

      // Restore canvas
      FCanvas.Font.Style := Element.Style;
    end;
  end;

  function GetWidthWithoutLastSpace: Integer;
  begin
    if SpaceInfo.LastWordEndsWithSpace then
      Result := Width - SpaceInfo.SpaceWidth
    else
      Result := Width;
  end;

  function IsFirstWordOfSource: Boolean;
  begin
    { If we are processing the first word of the source, we don't want to word
      wrap; we'd simply leave an empty line at the top. }
    Result := ((FPosX = FRect.Left) and (FPosY = FRect.Top) and (Enum.Count = 1));
  end;

  function IsInWord: Boolean;
  begin
    Result := Element.Node.FirstWordWidthRetrieved and (Enum.Count = 1);
  end;

  procedure NotifyObservers;
  var
    Index: Integer;
  begin
    { Notify observers that we are processing the node they are interested in.
      Note that more than one observer may be interested in monitoring the same
      node; TDynamicNode is a good example. }
    Index := FObservers.IndexOfStringNode(Element.Node);

    while Index <> -1 do
    begin
      with FObservers[Index]^ do
        if Assigned(Observer) then
          Observer.StartingPosUpdated(FPosX, FPosY, ParentNode);
      FObservers.RemoveObserver(FObservers[Index]);
      Index := FObservers.IndexOfStringNode(Element.Node);
    end;
  end;

  function GetCurrentRect: TRect;
  begin
    Result := Rect( FPosX,
                    FPosY,
                    FPosX + FCanvas.TextWidth(Buffer),
                    FPosY + FLineHeight);
  end;

begin
  for I := 0 to FList.Count - 1 do
    if FList[I] is TActionElement then
    begin
      // Bianconi #2
      FPosX := 0;
      // End of Bianconi #2
      case TActionElement(FList[I]).ActionType of
        atLineBreak:
          Inc(FPosY, FLineHeight);
        atParagraphBreak:
          Inc(FPosY, FLineHeight * 2);
      end;
    end
    else
    if FList[I] is TStringElement then
      with FCanvas do
      begin
        Element := TStringElement(FList[I]);
        NotifyObservers;

        Font.Style := Element.Style;
        Font.Color := Element.Color;

        Enum := TWordEnumerator.Create(Element.Node.Text);
        Buffer := '';
        Width := 0;
        Element.Node.ClearRects;

        while Enum.HasNext do
        begin
          NextWord := Enum.PopNext;

          { We cache information about each individual word to speed rendering;
            this way, we don't have to recalculate this information every time
            this routine is called (basically every time the tree needs to be
            repainted). We also do this as we otherwise wouldn't get correct
            output when rendering nodes individually (for example, we frequently
            rerender TLinkNodes with a different color). We only break after every
            complete word, and one node might not contain complete words. GetWidth
            makes use of information from other nodes succeeding the current one
            if necessary; this explains why it's important to only store
            information gathered when rendering the complete tree, that is, the
            first time we render anything at all. }
          if Element.Node.IsWordInfoInArray(Enum.Count - 1) then
          begin
            NextWordWidth := Element.Node.GetWordInfo(Enum.Count - 1).Width;
            SpaceInfo := Element.Node.GetWordInfo(Enum.Count - 1).SpaceInfo;
          end
          else
          begin
            NextWordWidth := GetWidth(SpaceInfo);
            Element.Node.AddWordInfo(SpaceInfo, NextWordWidth);
          end;

          Inc(Width, NextWordWidth);

          // Bianconi #2
          // Original Code -> ... FPosX + GetWidthWithoutLastSpace ...
          if( ( (FPosX + Element.Node.Root.StartingPoint.X + GetWidthWithoutLastSpace) >= FRect.Right) and
              not (NextWord = Space) and   // Never wrap because of lone space elements
              not IsFirstWordOfSource and  // Don't wrap if we have yet to output anything
              not IsInWord ) then          // We can't wrap if we're in the middle of rendering a word
          begin // Word wrap
            { Output contents of buffer, empty it and start on a new line, thus
              resetting FPosX and incrementing FPosY. }

            TextOut( FPosX + Element.Node.Root.StartingPoint.X,
                     FPosY + Element.Node.Root.StartingPoint.Y,
                     TrimRight(Buffer));
            Element.Node.AddRect(GetCurrentRect);
            Buffer := '';
            // Bianconi #2
            // FPosX := FRect.Left;
            FPosX := 0;
            // End of Bianconi #2
            Width := NextWordWidth;
            Inc(FPosY, FLineHeight);
          end
          else
          if (Element.Node.FirstWordWidthRetrieved) and (Enum.HasNext) and
            (Enum.Count = 1) then
            Inc(Width, TextWidth(NextWord));

          Buffer := Buffer + NextWord;
        end;  // while Enum.HasNext

        TextOut( FPosX + Element.Node.Root.StartingPoint.X,
                 FPosY + Element.Node.Root.StartingPoint.Y,
                 Buffer);
        Element.Node.AddRect(GetCurrentRect);
        Inc(FPosX, TextWidth(Buffer));
      end
    else
      raise ETextHandlerError.CreateRes(@RsEUnsupported);

  FList.Clear;
end;

procedure TTextHandler.TextOut(Node: TStringNode; Style: TFontStyles;
  Color: TColor);
begin
  { Consider these strings:
    "This is a <B>test</B>"
      We first store the string and its attributes in our list. As it ends with
      a space character, we know it's safe to empty our buffer (thus rendering
      the results to the screen). When we encounter "test", we don't know for
      sure whether it'll be followed by a new word or a new substring ("run"?).
      We have to wait until someone tells us that we've reached the end of the
      string by calling our public EmptyBuffer method.
    "This is a<B> test</B>"
      As usual, we store the first node element ("This is a"). As it doesn't end
      with a space, it could be followed by another character. However, when we
      encounter " test", we know that it was indeed a separate word. We
      immediately call EmptyBuffer before parsing the new string.
    "<B>Te</B><I>s</I>ting stuff "
      Here's an instance of the general problem this class was designed to
      solve. We first store "Te" and its attributes, as it might only be a part
      of a word. Indeed, in this case we're right. When we get to "s", we store
      this in a second entry in the list. "ting" is then stored in a third
      entry after which we discover that the last character is a space, meaning
      that we've assembled an entire word. Thus we empty our buffer. }

  if Copy(Node.Text, 1, 1) = Space then
    EmptyBuffer;

  FList.AddStringElement(Node, Style, Color);

  if Copy(Node.Text, Length(Node.Text), 1) = Space then
    EmptyBuffer;
end;

//=== TNodeObserverList ======================================================

procedure TNodeObserverList.AddObserver(Observer: IStartingPosObserver;
  ParentNode: TAreaNode; FirstStringNode: TStringNode);
var
  NewRecord: PNodeObserver;
begin
  New(NewRecord);

  try
    NewRecord^.Observer := Observer;
    NewRecord^.ParentNode := ParentNode;
    NewRecord^.FirstStringNode := FirstStringNode;

    FList.Add(NewRecord);
  except
    Dispose(NewRecord);
    raise;
  end;
end;

function TNodeObserverList.Get(Index: Integer): PNodeObserver;
begin
  Result := FList[Index];
end;

function TNodeObserverList.IndexOfStringNode(Node: TStringNode): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FList.Count - 1 do
    if Get(I)^.FirstStringNode = Node then
    begin
      Result := I;
      Break;
    end;
end;

procedure TNodeObserverList.Put(Index: Integer;
  const Value: PNodeObserver);
begin
  FList[Index] := Value;
end;

procedure TNodeObserverList.RemoveObserver(Item: PNodeObserver);
begin
  FList.Remove(Item);
  Dispose(Item);
end;

//=== TNodeObserverList ======================================================

constructor TStringElement.Create(const Node: TStringNode;
  Style: TFontStyles; Color: TColor);
begin
  inherited Create;
  FNode := Node;
  FStyle := Style;
  FColor := Color;
end;

function TStringElement.BeginsWithSpace: Boolean;
begin
  Result := TStringTools.BeginsWith(FNode.Text);
end;

function TStringElement.EndsWithSpace: Boolean;
begin
  Result := TStringTools.EndsWith(FNode.Text);
end;

//=== TActionElement =========================================================

constructor TActionElement.Create(ActionType: TActionType);
begin
  inherited Create;
  FActionType := ActionType;
end;

end.
