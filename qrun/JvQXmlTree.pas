{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXMLTree.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQXmlTree;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, 
  Variants, 
  JvQStrings;

type
  TJvXMLValueType = (xvtString, xvtCDATA);
  TJvXMLFilterOperator = (xfoNOP, xfoEQ, xfoIEQ, xfoNE, xfoINE, xfoGE,
    xfoIGE, xfoLE, xfoILE, xfoGT, xfoIGT, xfoLT, xfoILT);

  TJvXMLTree = class;

  TJvXMLFilterAtom = class(TObject)
  private
    FValue: string;
    FName: string;
    FOperator: TJvXMLFilterOperator;
    FAttributeFilter: Boolean;
  public
    property Name: string read FName write FName;
    property Operator: TJvXMLFilterOperator read FOperator write FOperator;
    property Value: string read FValue write FValue;
    property AttributeFilter: Boolean read FAttributeFilter write FAttributeFilter;
  end;

  TJvXMLFilter = class(TObject)
  private
    FName: string;
    FFilters: TList;
    procedure Initialize(FilterStr: string);
  public
    constructor Create(FilterStr: string);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Filters: TList read FFilters write FFilters;
  end;

  TJvXMLNode = class;

  TJvXMLAttribute = class(TObject)
  private
    FName: string;
    FValue: Variant;
    FParent: TJvXMLNode;
  public
    constructor Create(AParent: TJvXMLNode; const AName: string; AValue: Variant);
    function Document: string;
    property Name: string read FName write FName;
    property Value: Variant read FValue write FValue;
    property Parent: TJvXMLNode read FParent write FParent;
  end;

  TJvXMLNode = class(TObject)
  private
    FName: string;
    FValue: Variant;
    FNodes: TList;
    FAttributes: TList;
    FParentNode: TJvXMLNode;
    FValueType: TJvXMLValueType;
  public
    constructor Create(const AName: string; AValue: Variant; AParent: TJvXMLNode);
    destructor Destroy; override;
    // added 29-July-2000
    function GetNamePathNode(const APath: string): TJvXMLNode;
    procedure DeleteNamePathNode(const APath: string);
    function ForceNamePathNode(const APath: string): TJvXMLNode;
    function GetNamePathNodeAttribute(const APath, AName: string): TJvXMLAttribute;
    procedure DeleteNamePathNodeAttribute(const APath, AName: string);
    function ForceNamePathNodeAttribute(const APath, AName: string; AValue: Variant): TJvXMLAttribute;
    function AddNode(const AName: string; AValue: Variant): TJvXMLNode;
    function AddNodeEx(const AName: string; AValue: Variant): TJvXMLNode;
    procedure DeleteNode(Index: Integer);
    procedure ClearNodes;
    function AddAttribute(const AName: string; AValue: Variant): TJvXMLAttribute;
    function GetAttributeValue(const AName: string): Variant;
    procedure DeleteAttribute(Index: Integer);
    procedure ClearAttributes;
    function Document(ALevel: Integer): string;
    function GetNodePath: string;
    function GetNamedNode(const AName: string): TJvXMLNode;
    function SelectSingleNode(const APattern: string): TJvXMLNode;
    procedure SelectNodes(APattern: string; AList: TList);
    function TransformNode(AStyleSheet: TJvXMLNode): string;
    function Process(ALevel: Integer; ANode: TJvXMLNode): string;
    function FindNamedNode(const AName: string): TJvXMLNode;
    procedure FindNamedNodes(const AName: string; AList: TList);
    procedure GetAllNodes(AList: TList);
    function GetNamedAttribute(const AName: string): TJvXMLAttribute;
    procedure FindNamedAttributes(const AName: string; AList: TList);
    function MatchFilter(AObjFilter: TJvXMLFilter): Boolean;
    procedure MatchPattern(const APattern: string; AList: TList);
    procedure GetNodeNames(AList: TStrings);
    procedure GetAttributeNames(AList: TStrings);
    function GetNameSpace: string;
    function HasChildNodes: Boolean;
    function CloneNode: TJvXMLNode;
    function FirstChild: TJvXMLNode;
    function LastChild: TJvXMLNode;
    function PreviousSibling: TJvXMLNode;
    function NextSibling: TJvXMLNode;
    function MoveAddNode(Dest: TJvXMLNode): TJvXMLNode;
    function MoveInsertNode(Dest: TJvXMLNode): TJvXMLNode;
    function RemoveChildNode(ANode: TJvXMLNode): TJvXMLNode;
    property Name: string read FName write FName;
    property Value: Variant read FValue write FValue;
    property ValueType: TJvXMLValueType read FValueType write FValueType;
    property Nodes: TList read FNodes write FNodes;
    property ParentNode: TJvXMLNode read FParentNode write FParentNode;
    property Attributes: TList read FAttributes write FAttributes;
  end;

  TJvXMLTree = class(TJvXMLNode)
  private
    FLines: TStringList;
    FNodeCount: Integer;
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    function GetText: string;
    procedure SetText(const Value: string);
  public
    constructor Create(const AName: string; AValue: Variant; AParent: TJvXMLNode);
    destructor Destroy; override;
    procedure ParseXML;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    function AsText: string;
    property Lines: TStrings read GetLines write SetLines;
    property NodeCount: Integer read FNodeCount;
    property Text: string read GetText write SetText;
  end;

procedure PreProcessXML(AList: TStrings);

implementation

uses
  JvQConsts;

procedure PreProcessXML(AList: TStrings);
var
  OList: TStringList;
  S, xTag, xText, xData: string;
  P1, P2, C: Integer;
  //Level: Integer;

  function Clean(const AText: string): string;
  begin
    Result := StringReplace(AText, sLineBreak, ' ', [rfReplaceAll]);
    Result := StringReplace(Result, Tab, ' ', [rfReplaceAll]);
    Result := Trim(Result);
  end;

  function CleanCDATA(const AText: string): string;
  begin
    Result := StringReplace(AText, sLineBreak, '\n ', [rfReplaceAll]);
    Result := StringReplace(Result, Tab, '\t ', [rfReplaceAll]);
  end;

begin
  OList := TStringList.Create;
  try
    S := AList.Text;
    xText := '';
    xTag := '';
    P1 := 1;
    C := Length(S);
    //Level := 0;
    repeat
      P2 := PosStr('<', S, P1);
      if P2 > 0 then
      begin
        xText := Trim(Copy(S, P1, P2 - P1));
        if xText <> '' then
          OList.Add('TX:' + Clean(xText));
        P1 := P2;
        // check for CDATA
        if UpperCase(Copy(S, P1, 9)) = '<![CDATA[' then
        begin
          P2 := PosStr(']]>', S, P1);
          xData := Copy(S, P1 + 9, P2 - P1 - 9);
          OList.Add('CD:' + CleanCDATA(xData));
          P1 := P2 + 2;
        end
        else
        begin
          P2 := PosStr('>', S, P1);
          if P2 > 0 then
          begin
            xTag := Copy(S, P1 + 1, P2 - P1 - 1);
            P1 := P2;
            if xTag[1] = '/' then
            begin
              Delete(xTag, 1, 1);
              OList.Add('CT:' + Clean(xTag));
              //Dec(Level);
            end
            else
            if xTag[Length(xTag)] = '/' then
              OList.Add('ET:' + Clean(xTag))
            else
            begin
              //Inc(Level);
              OList.Add('OT:' + Clean(xTag));
            end;
          end;
        end;
      end
      else
      begin
        xText := Trim(Copy(S, P1, Length(S)));
        if xText <> '' then
        begin
          OList.Add('TX:' + Clean(xText));
        end;
        P1 := C;
      end;
      Inc(P1);
    until P1 > C;
    AList.Assign(OList);
  finally
    OList.Free;
  end;
end;

//=== { TJvXMLNode } =========================================================

constructor TJvXMLNode.Create(const AName: string; AValue: Variant; AParent: TJvXMLNode);
begin
  inherited Create;
  FNodes := TList.Create;
  FName := AName;
  FValue := AValue;
  FValueType := xvtString;
  FParentNode := AParent;
  FAttributes := TList.Create;
end;

destructor TJvXMLNode.Destroy;
begin
  ClearNodes;
  FNodes.Free;
  ClearAttributes;
  FAttributes.Free;
  inherited Destroy;
end;

function TJvXMLNode.AddAttribute(const AName: string; AValue: Variant): TJvXMLAttribute;
begin
  Result := TJvXMLAttribute.Create(Self, AName, AValue);
  Attributes.Add(Result);
end;

function TJvXMLNode.AddNode(const AName: string; AValue: Variant): TJvXMLNode;
begin
  Result := TJvXMLNode.Create(AName, AValue, Self);
  Nodes.Add(Result);
end;

// adds node and parses any attributes;

function TJvXMLNode.AddNodeEx(const AName: string; AValue: Variant): TJvXMLNode;
var
  S, SN, SV: string;
  C, P1, P2: Integer;
begin
  Result := TJvXMLNode.Create(AName, AValue, Self);
  Self.Nodes.Add(Result);
  C := Length(AName);
  //first parse Name
  P1 := PosStr(' ', AName, 1);
  if P1 = 0 then
    Exit;
  S := Copy(AName, 1, P1 - 1);
  Result.Name := S;
  repeat
    // find '='
    P2 := PosStr('=', AName, P1);
    if P2 = 0 then
      Break;
    SN := Trim(Copy(AName, P1, P2 - P1));
    P1 := P2;
    // find begin of value
    P1 := PosStr('"', AName, P1);
    if P1 = 0 then
      Break;
    P2 := PosStr('"', AName, P1 + 1);
    if P2 = 0 then
      Exit;
    SV := Copy(AName, P1 + 1, P2 - P1 - 1);
    Result.AddAttribute(SN, SV);
    P1 := P2 + 1;
  until P1 > C;
end;

function TJvXMLNode.GetNamedAttribute(const AName: string): TJvXMLAttribute;
var
  I: Integer;
  N: TJvXMLAttribute;
begin
  Result := nil;
  for I := 0 to Attributes.Count - 1 do
  begin
    N := TJvXMLAttribute(Attributes[I]);
    if N.Name = AName then
    begin
      Result := N;
      Break;
    end;
  end;
end;

procedure TJvXMLNode.ClearAttributes;
var
  I: Integer;
begin
  if Attributes.Count <> 0 then
  begin
    for I := 0 to Attributes.Count - 1 do
      TJvXMLAttribute(Attributes[I]).Free;
    Attributes.Clear;
  end;
end;

procedure TJvXMLNode.ClearNodes;
var
  I: Integer;
begin
  for I := 0 to Nodes.Count - 1 do
    TJvXMLNode(Nodes[I]).Free;
  Nodes.Clear;
end;

procedure TJvXMLNode.DeleteAttribute(Index: Integer);
begin
  if (Attributes.Count > 0) and (Index < Attributes.Count) then
  begin
    TJvXMLAttribute(Attributes[Index]).Free;
    Attributes.Delete(Index);
  end;
end;

procedure TJvXMLNode.DeleteNode(Index: Integer);
begin
  if (Nodes.Count > 0) and (Index < Nodes.Count) then
  begin
    TJvXMLNode(Nodes[Index]).Free;
    Nodes.Delete(Index);
  end;
end;

function TJvXMLNode.Document(ALevel: Integer): string;
var
  I: Integer;
  Indent: string;

  function ExpandCDATA(const AValue: string): string;
  begin
    Result := StringReplace(AValue, '\n ', sLineBreak, [rfReplaceAll]);
    Result := StringReplace(Result, '\t ', Tab, [rfReplaceAll]);
  end;

begin
  if ALevel > 0 then
    Indent := StringOfChar(' ', ALevel * 2)
  else
    Indent := '';
  Result := Indent + '<' + Name;
  if Attributes.Count > 0 then
    for I := 0 to Attributes.Count - 1 do
      Result := Result + TJvXMLAttribute(Attributes[I]).Document;
  if (Nodes.Count = 0) and (Value = '') then
  begin
    Result := Result + ' />' + sLineBreak;
    Exit;
  end
  else
    Result := Result + '>' + sLineBreak;
  if Value <> '' then
  begin
    if ValueType = xvtString then
      Result := Result + Indent + '  ' + Value + sLineBreak
    else
    if ValueType = xvtCDATA then
      Result := Result + Indent + '  ' + '<![CDATA[' + ExpandCDATA(Value) + ']]>' + sLineBreak;
  end;
  if Nodes.Count <> 0 then
    for I := 0 to Nodes.Count - 1 do
      Result := Result + TJvXMLNode(Nodes[I]).Document(ALevel + 1);
  Result := Result + Indent + '</' + Name + '>' + sLineBreak;
end;

// duplicates a node recursively

function TJvXMLNode.CloneNode: TJvXMLNode;
var
  I: Integer;
  N: TJvXMLNode;
begin
  Result := TJvXMLNode.Create(Name, Value, nil);
  Result.Name := Name;
  Result.Value := Value;
  if Attributes.Count > 0 then
    for I := 0 to Attributes.Count - 1 do
      Result.AddAttribute(TJvXMLAttribute(Attributes[I]).Name, TJvXMLAttribute(Attributes[I]).Value);
  if Nodes.Count > 0 then
    for I := 0 to Nodes.Count - 1 do
    begin
      N := TJvXMLNode(Nodes[I]).CloneNode;
      Result.Nodes.Add(N);
    end;
end;

function TJvXMLNode.GetNamedNode(const AName: string): TJvXMLNode;
var
  I: Integer;
  N: TJvXMLNode;
begin
  Result := nil;
  for I := 0 to Nodes.Count - 1 do
  begin
    N := TJvXMLNode(Nodes[I]);
    if N.Name = AName then
    begin
      Result := N;
      Exit;
    end;
  end;
end;

function TJvXMLNode.FirstChild: TJvXMLNode;
begin
  if Nodes.Count > 0 then
    Result := TJvXMLNode(Nodes[0])
  else
    Result := nil;
end;

function TJvXMLNode.LastChild: TJvXMLNode;
begin
  if Nodes.Count > 0 then
    Result := TJvXMLNode(Nodes[Nodes.Count - 1])
  else
    Result := nil;
end;

function TJvXMLNode.NextSibling: TJvXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  if ParentNode = nil then
    Exit;
  Index := ParentNode.Nodes.IndexOf(Self);
  if Index = -1 then
    Exit;
  if Index < ParentNode.Nodes.Count - 1 then
    Result := TJvXMLNode(ParentNode.Nodes[Index + 1]);
end;

function TJvXMLNode.PreviousSibling: TJvXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  if ParentNode = nil then
    Exit;
  Index := ParentNode.Nodes.IndexOf(Self);
  if Index = -1 then
    Exit;
  if Index > 0 then
    Result := TJvXMLNode(ParentNode.Nodes[Index - 1]);
end;
// moves a node to a new location

function TJvXMLNode.MoveInsertNode(Dest: TJvXMLNode): TJvXMLNode;
var
  Index1, Index2: Integer;
begin
  Result := nil;
  if Dest.ParentNode = nil then
    Exit; // can not move to root
  Index1 := Self.ParentNode.Nodes.IndexOf(Self);
  if Index1 = -1 then
    Exit;
  Index2 := Dest.ParentNode.Nodes.IndexOf(Dest);
  if Index2 = -1 then
    Exit;
  Dest.ParentNode.Nodes.Insert(Index2, Self);
  Self.ParentNode.Nodes.Delete(Index1);
  Self.ParentNode := Dest.ParentNode;
  Result := Self;
end;

function TJvXMLNode.MoveAddNode(Dest: TJvXMLNode): TJvXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  if Dest = nil then
    Exit; // can not move to root
  Index := Self.ParentNode.Nodes.IndexOf(Self);
  if Index = -1 then
    Exit;
  Dest.Nodes.Add(Self);
  Self.ParentNode.Nodes.Delete(Index);
  Self.ParentNode := Dest;
  Result := Self;
end;

// removes and Frees the childnode recursively.
// returns Self when done, or nil in case of error

function TJvXMLNode.RemoveChildNode(ANode: TJvXMLNode): TJvXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  Index := Nodes.IndexOf(ANode);
  if Index = -1 then
    Exit;
  Nodes.Delete(Index);
  ANode.Free;
  Result := Self;
end;

function TJvXMLNode.HasChildNodes: Boolean;
begin
  Result := Nodes.Count > 0;
end;

procedure TJvXMLNode.GetAttributeNames(AList: TStrings);
var
  I: Integer;
begin
  AList.Clear;
  for I := 0 to Attributes.Count - 1 do
    AList.Add(TJvXMLAttribute(Attributes[I]).Name);
end;

procedure TJvXMLNode.GetNodeNames(AList: TStrings);
var
  I, C: Integer;
begin
  AList.Clear;
  C := Nodes.Count;
  for I := 0 to C - 1 do
    AList.Add(TJvXMLNode(Nodes[I]).Name);
end;

function TJvXMLNode.GetNodePath: string;
var
  N: TJvXMLNode;
begin
  N := Self;
  Result := Name;
  while N.ParentNode <> nil do
  begin
    N := N.ParentNode;
    Result := N.Name + '/' + Result;
  end;
end;

// search recursively for a named node

function TJvXMLNode.FindNamedNode(const AName: string): TJvXMLNode;
var
  I: Integer;
  N: TJvXMLNode;
begin
  Result := nil;
  for I := 0 to Nodes.Count - 1 do
  begin
    N := TJvXMLNode(Nodes[I]);
    if N.Name = AName then
    begin
      Result := N;
      Break;
    end
    else
    begin // Recurse
      Result := N.FindNamedNode(AName);
      if Result <> nil then
        Break;
    end;
  end;
end;

// add all found named Nodes to AList

procedure TJvXMLNode.FindNamedNodes(const AName: string; AList: TList);
var
  I: Integer;
  N: TJvXMLNode;
begin
  for I := 0 to Nodes.Count - 1 do
  begin
    N := TJvXMLNode(Nodes[I]);
    if N.Name = AName then
      AList.Add(N);
    // Recurse
    N.FindNamedNodes(AName, AList);
  end;
end;

// add recursively all Nodes to AList
// the list only contains pointers to the Nodes
// typecast to use, e.g. N:=TJvXMLNode(AList[0]);

procedure TJvXMLNode.GetAllNodes(AList: TList);
var
  I: Integer;
  N: TJvXMLNode;
begin
  for I := 0 to Nodes.Count - 1 do
  begin
    N := TJvXMLNode(Nodes[I]);
    AList.Add(N);
    // Recurse
    N.GetAllNodes(AList);
  end;
end;

// add recursively all Nodes with matching named attribute to AList
// the list only contains pointers to the Nodes
// typecast to use, e.g. N:=TJvXMLNode(AList[0]);

procedure TJvXMLNode.FindNamedAttributes(const AName: string; AList: TList);
var
  I: Integer;
begin
  for I := 0 to Attributes.Count - 1 do
    if TJvXMLAttribute(Attributes[I]).Name = AName then
    begin
      AList.Add(Self);
      Break;
    end;
  for I := 0 to Nodes.Count - 1 do
    TJvXMLNode(Nodes[I]).FindNamedAttributes(AName, AList);
end;

{
this procedure adds the node to AList when it matches the pattern
this will be the key procedure for XSL implementation
only basic matching is provided in the first release
path operators
 /  child path
 // recursive descent
 .  curren context or node
 @  attribute
 *  wildcar
some examples
 /  the root node only
 book/author  <author> elements that are children of <book> elements
 // the root node and all Nodes below
 //*  all element Nodes below the root node
 book//author  <author> elements that are descendants of <book> elements
 .//author  <author elements that are descendants of the current element
 *  non-root elements, irrespective of the element Name
 book/*  elements that are children of <book> elements
 book//* elements that are descendants of <book> elements
 book/*/author  <author> elements that are grandchildren of <book> elements
 book/@print_date print_date attributes that are attached to <book> elements
 */@print_date print_date atrtributes that are attached to any elements

Index can be used to specify a particular node within a matching set
 /booklist/book[0]  First <book> node in root <booklist> element
 /booklist/book[2]  Third <book> node in root <booklist> element
 /booklist/book[end()] Last <book> node in root <booklist> element
}

procedure TJvXMLNode.MatchPattern(const APattern: string; AList: TList);
begin
  // to be implemented
end;

{select a node based on path info
 e.g. booklist/book/category will find the first
 <category> that is a child of <book> that is a child of <booklist>
 }

function TJvXMLNode.SelectSingleNode(const APattern: string): TJvXMLNode;
var
  NPattern, LFilter: string;
  P, I: Integer;
  N: TJvXMLNode;
  ObjFilter: TJvXMLFilter;
begin
  Result := nil;
  if Nodes.Count = 0 then
    Exit;
  ObjFilter := nil;
  try
    P := Pos('/', APattern);
    if P = 0 then
    begin
      ObjFilter := TJvXMLFilter.Create(APattern);
      for I := 0 to Nodes.Count - 1 do
      begin
        N := TJvXMLNode(Nodes[I]);
        if N.MatchFilter(ObjFilter) then
        begin
          Result := N;
          Exit;
        end;
      end;
      // not found;
    end
    else
    begin
      LFilter := Copy(APattern, 1, P - 1);
      NPattern := Copy(APattern, P + 1, Length(APattern));
      ObjFilter := TJvXMLFilter.Create(LFilter);
      for I := 0 to Nodes.Count - 1 do
      begin
        N := TJvXMLNode(Nodes[I]);
        if N.MatchFilter(ObjFilter) then
        begin
          Result := N.SelectSingleNode(NPattern);
          if Result <> nil then
            Exit;
        end;
      end;
    end;
  finally
    ObjFilter.Free;
  end;
end;

// filter contains Name + any filters between []

function TJvXMLNode.MatchFilter(AObjFilter: TJvXMLFilter): Boolean;
var
  I, J: Integer;
  A: TJvXMLAttribute;
  N: TJvXMLNode;
  AttName: string;
  Atom: TJvXMLFilterAtom;
  AttResult: Boolean;

  function EvalAtom(const AValue: string): Boolean;
  begin
    Result := False;
    case Atom.Operator of
      xfoNOP:
        Result := True;
      xfoEQ:
        Result := AValue = Atom.Value;
      xfoIEQ:
        Result := AnsiCompareText(AValue, Atom.Value) = 0;
      xfoNE:
        Result := AValue <> Atom.Value;
      xfoINE:
        Result := AnsiCompareText(AValue, Atom.Value) <> 0;
      xfoGT:
        try
          Result := StrToFloat(AValue) > StrToFloat(Atom.Value);
        except
        end;
      xfoIGT:
        Result := AnsiCompareText(AValue, Atom.Value) > 0;
      xfoLT:
        try
          Result := StrToFloat(AValue) < StrToFloat(Atom.Value);
        except
        end;
      xfoILT:
        Result := AnsiCompareText(AValue, Atom.Value) < 0;
      xfoGE:
        try
          Result := StrToFloat(AValue) >= StrToFloat(Atom.Value);
        except
        end;
      xfoIGE:
        Result := AnsiCompareText(AValue, Atom.Value) >= 0;
      xfoLE:
        try
          Result := StrToFloat(AValue) <= StrToFloat(Atom.Value);
        except
        end;
      xfoILE:
        Result := AnsiCompareText(AValue, Atom.Value) <= 0;
    end;
  end;

begin
  Result := False;
  AttResult := False;
  if AObjFilter.Filters.Count = 0 then
  begin // just filter on Name
    Result := AObjFilter.Name = Name;
    Exit;
  end;
  for I := 0 to AObjFilter.Filters.Count - 1 do
  begin
    Atom := TJvXMLFilterAtom(AObjFilter.Filters[I]);
    if Atom.AttributeFilter then
    begin
      AttName := Atom.Name;
      if AttName = '*' then
      begin // match any attribute
        if Attributes.Count = 0 then
          Exit;
        for J := 0 to Attributes.Count - 1 do
        begin
          A := TJvXMLAttribute(Attributes[J]);
          AttResult := EvalAtom(A.Value);
          if AttResult then
            Break;
        end;
        if not AttResult then
          Exit;
      end
      else
      begin
        A := GetNamedAttribute(AttName);
        if (A = nil) or not EvalAtom(A.Value) then
          Exit;
      end;
    end
    else
    begin
      AttName := Atom.Name;
      N := GetNamedNode(AttName);
      if (N = nil) or not EvalAtom(N.Value) then
        Exit;
    end;
  end;
  Result := True;
end;

procedure TJvXMLNode.SelectNodes(APattern: string; AList: TList);
var
  NPattern: string;
  P, I: Integer;
  N: TJvXMLNode;
  LFilter: string;
  ObjFilter: TJvXMLFilter;
  Recurse: Boolean;
begin
  if Nodes.Count = 0 then
    Exit;
  if Copy(APattern, 1, 2) = '//' then
  begin //recursive
    Delete(APattern, 1, 2);
    Recurse := True;
  end
  else
    Recurse := False;
  P := Pos('/', APattern);
  if P = 0 then
  begin
    LFilter := APattern;
    ObjFilter := TJvXMLFilter.Create(LFilter);
    for I := 0 to Nodes.Count - 1 do
    begin
      N := TJvXMLNode(Nodes[I]);
      if N.MatchFilter(ObjFilter) then
        AList.Add(N)
      else
      if Recurse then
        N.SelectNodes('//' + APattern, AList);
    end;
    ObjFilter.Free;
  end
  else
  begin
    LFilter := Copy(APattern, 1, P - 1);
    if Copy(APattern, P, 2) = '//' then
      NPattern := Copy(APattern, P, Length(APattern))
    else
      NPattern := Copy(APattern, P + 1, Length(APattern));
    ObjFilter := TJvXMLFilter.Create(LFilter);
    for I := 0 to Nodes.Count - 1 do
    begin
      N := TJvXMLNode(Nodes[I]);
      if N.MatchFilter(ObjFilter) then
        N.SelectNodes(NPattern, AList)
      else
      if Recurse then
        N.SelectNodes('//' + APattern, AList);
    end;
    ObjFilter.Free;
  end;
end;

// the XSL implementation
// although this function returns a string, the string itself can be parsed to Create a DOM

function TJvXMLNode.TransformNode(AStyleSheet: TJvXMLNode): string;
begin
  // to be implemented;
  Result := AStyleSheet.Process(0, Self);
end;

// used in conjunction with the TransformNode function.
// basically works like the Document function except for Nodes with processing instructions

function TJvXMLNode.Process(ALevel: Integer; ANode: TJvXMLNode): string;
var
  I: Integer;
  Indent: string;

  function ExpandCDATA(const AValue: string): string;
  begin
    Result := StringReplace(AValue, '\n ', sLineBreak, [rfReplaceAll]);
    Result := StringReplace(Result, '\t ', Tab, [rfReplaceAll]);
  end;

begin
  if ParentNode = nil then
  begin
    for I := 0 to Nodes.Count - 1 do
      Result := Result + TJvXMLNode(Nodes[I]).Process(ALevel + 1, ANode);
    Exit;
  end;
  if ALevel > 0 then
    Indent := StringOfChar(' ', ALevel * 2)
  else
    Indent := '';
  Result := Indent + '<' + Name;
  for I := 0 to Attributes.Count - 1 do
    Result := Result + TJvXMLAttribute(Attributes[I]).Document;
  if (Nodes.Count = 0) and (Value = '') then
  begin
    Result := Result + ' />' + sLineBreak;
    Exit;
  end
  else
    Result := Result + '>' + sLineBreak;
  if Value <> '' then
  begin
    if ValueType = xvtString then
      Result := Result + Indent + '  ' + Value + sLineBreak
    else
    if ValueType = xvtCDATA then
      Result := Result + Indent + '  ' + '<![CDATA[' + ExpandCDATA(Value) + ']]>' + sLineBreak;
  end;
  for I := 0 to Nodes.Count - 1 do
    Result := Result + TJvXMLNode(Nodes[I]).Process(ALevel + 1, ANode);
  Result := Result + Indent + '</' + Name + '>' + sLineBreak;
end;

function TJvXMLNode.GetNameSpace: string;
var
  P: Integer;
begin
  P := Pos(':', FName);
  if P > 0 then
    Result := Copy(FName, 1, P - 1)
  else
    Result := '';
end;

// find the node with a path like customers/regional/jansoft

function TJvXMLNode.GetNamePathNode(const APath: string): TJvXMLNode;
var
  AName, NewPath, SIndex: string;
  I, P, Index, IndexC: Integer;
  N: TJvXMLNode;
begin
  Result := nil;
  if Nodes.Count = 0 then
    Exit;
  if APath = '' then
  begin
    Result := Self;
    Exit;
  end;
  P := PosStr('/', APath, 1);
  if P = 0 then
  begin
    AName := APath;
    NewPath := '';
  end
  else
  begin
    AName := Copy(APath, 1, P - 1);
    NewPath := Copy(APath, P + 1, Length(APath));
  end;
  // now check for any Index []
  P := PosStr('[', AName, 1);
  Index := 0; // search first by default
  IndexC := 0;
  if P > 0 then
  begin
    SIndex := Copy(AName, P + 1, Length(AName) - P - 1);
    AName := Copy(AName, 1, P - 1);
    if SIndex = 'end' then
      Index := -1
    else
      try
        Index := StrToInt(SIndex);
        if Index >= Nodes.Count then
          Exit;
      except
        Exit;
      end;
  end;
  if Index = -1 then // search end from end
    for I := Nodes.Count - 1 downto 0 do
    begin
      N := TJvXMLNode(Nodes[I]);
      if N.Name = AName then
        if NewPath = '' then
        begin
          Result := N;
          Exit;
        end
        else
        begin
          Result := N.GetNamePathNode(NewPath);
          Exit;
        end;
    end
  else // search from beginning indexed
    for I := 0 to Nodes.Count - 1 do
    begin
      N := TJvXMLNode(Nodes[I]);
      if N.Name = AName then
        if Index = IndexC then
        begin
          if NewPath = '' then
          begin
            Result := N;
            Exit;
          end
          else
          begin
            Result := N.GetNamePathNode(NewPath);
            Exit;
          end;
        end
        else
          Inc(IndexC);
    end;
end;

function TJvXMLNode.ForceNamePathNode(const APath: string): TJvXMLNode;
var
  AName, NewPath: string;
  I, P: Integer;
  N: TJvXMLNode;
  DoAppend: Boolean;
begin
  //  Result:=nil;
  P := PosStr('/', APath, 1);
  if P = 0 then
  begin
    AName := APath;
    NewPath := '';
  end
  else
  begin
    AName := Copy(APath, 1, P - 1);
    NewPath := Copy(APath, P + 1, Length(APath));
  end;
  P := PosStr('+', AName, 1);
  if P > 0 then
    Delete(AName, P, 1);
  DoAppend := P > 0;
  if not DoAppend then
    for I := 0 to Nodes.Count - 1 do
    begin
      N := TJvXMLNode(Nodes[I]);
      if N.Name = AName then
        if NewPath = '' then
        begin
          Result := N;
          Exit;
        end
        else
        begin
          Result := N.ForceNamePathNode(NewPath);
          Exit;
        end;
    end;
  // we dont have it , so force it;
  N := TJvXMLNode.Create(AName, '', Self);
  Nodes.Add(N);
  if NewPath = '' then
    Result := N
  else
    Result := N.ForceNamePathNode(NewPath);
end;

function TJvXMLNode.ForceNamePathNodeAttribute(const APath, AName: string;
  AValue: Variant): TJvXMLAttribute;
var
  N: TJvXMLNode;
  A: TJvXMLAttribute;
begin
  Result := nil;
  N := ForceNamePathNode(APath);
  if N = nil then
    Exit;
  A := N.GetNamedAttribute(AName);
  if A <> nil then
  begin
    A.Value := AValue;
    Result := A;
  end
  else
    Result := N.AddAttribute(AName, AValue);
end;

function TJvXMLNode.GetNamePathNodeAttribute(const APath, AName: string): TJvXMLAttribute;
var
  N: TJvXMLNode;
begin
  Result := nil;
  N := GetNamePathNode(APath);
  if N = nil then
    Exit;
  Result := N.GetNamedAttribute(AName);
end;

procedure TJvXMLNode.DeleteNamePathNode(const APath: string);
var
  N, PN: TJvXMLNode;
  I: Integer;
begin
  if APath = '' then
    Exit;
  N := GetNamePathNode(APath);
  if N = nil then
    Exit;
  PN := N.ParentNode;
  for I := 0 to PN.Nodes.Count - 1 do
    if TJvXMLNode(PN.Nodes[I]) = N then
    begin
      PN.DeleteNode(I);
      Exit;
    end;
end;

procedure TJvXMLNode.DeleteNamePathNodeAttribute(const APath, AName: string);
var
  A: TJvXMLAttribute;
  PN: TJvXMLNode;
  I: Integer;
begin
  A := GetNamePathNodeAttribute(APath, AName);
  if A = nil then
    Exit;
  PN := A.Parent;
  for I := 0 to PN.Attributes.Count - 1 do
    if TJvXMLAttribute(PN.Attributes[I]) = A then
    begin
      PN.DeleteAttribute(I);
      Exit;
    end;
end;

function TJvXMLNode.GetAttributeValue(const AName: string): Variant;
var
  I: Integer;
  A: TJvXMLAttribute;
begin
  Result := Null;
  for I := 0 to Attributes.Count - 1 do
  begin
    A := TJvXMLAttribute(Attributes[I]);
    if A.Name = AName then
    begin
      Result := A.Value;
      Exit;
    end;
  end;
end;

//=== { TJvXMLTree } =========================================================

constructor TJvXMLTree.Create(const AName: string; AValue: Variant; AParent: TJvXMLNode);
begin
  inherited Create(AName, AValue, AParent);
  FLines := TStringList.Create;
end;

destructor TJvXMLTree.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

function TJvXMLTree.AsText: string;
var
  I: Integer;
begin
  if Nodes.Count = 0 then
    Exit;
  Result := '<' + Name;
  if Attributes.Count > 0 then
    for I := 0 to Attributes.Count - 1 do
      Result := Result + TJvXMLAttribute(Attributes[I]).Document;
  Result := Result + '>' + sLineBreak;
  for I := 0 to Nodes.Count - 1 do
    Result := Result + TJvXMLNode(Nodes[I]).Document(1);
  Result := Result + '</' + Name + '>' + sLineBreak;
end;

procedure TJvXMLTree.SaveToFile(const FileName: string);
begin
  Lines.Text := Text;
  Lines.SaveToFile(FileName);
end;

function TJvXMLTree.GetLines: TStrings;
begin
  Result := FLines;
end;

procedure TJvXMLTree.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TJvXMLTree.LoadFromStream(Stream: TStream);
begin
  ClearNodes;
  ClearAttributes;
  Lines.LoadFromStream(Stream);
  PreProcessXML(Lines);
  ParseXML;
end;

procedure TJvXMLTree.SaveToStream(Stream: TStream);
begin
  Lines.Text := AsText;
  Lines.SaveToStream(Stream);
end;

function TJvXMLTree.GetText: string;
var
  I: Integer;
begin
  //  Result:='<'+Name;
  //  if Attributes.Count>0 then
  //  for I:=0 to Attributes.Count-1 do
  //    Result:=Result+TJvXMLAttribute(Attributes[I]).Document;
  //  Result:=Result+'>'+sLineBreak;
  Result := '';
  for I := 0 to Nodes.Count - 1 do
    Result := Result + TJvXMLNode(Nodes[I]).Document(0);
  //  Result:=Result+'</'+Name+'>'+sLineBreak;
end;

procedure TJvXMLTree.SetText(const Value: string);
begin
  ClearNodes;
  ClearAttributes;
  Lines.Text := Value;
  PreProcessXML(Lines);
  ParseXML;
end;

//=== { TJvXMLAttribute } ====================================================

constructor TJvXMLAttribute.Create(AParent: TJvXMLNode; const AName: string; AValue: Variant);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
  FParent := AParent;
end;

function TJvXMLAttribute.Document: string;
var
  S: string;
begin
  S := Value;
  Result := ' ' + Name + '="' + S + '"';
end;

//=== { TJvXMLTree } =========================================================

procedure TJvXMLTree.ParseXML;
var
  I, C: Integer;
  S, Token, AName: string;
  N: TJvXMLNode;
begin
  I := 0;
  FNodeCount := 0;
  ClearNodes;
  ClearAttributes;
  Name := 'root';
  N := Self;
  C := Lines.Count - 1;
  repeat
    S := Lines[I];
    Token := Copy(S, 1, 3);
    AName := Copy(S, 4, Length(S));
    if Token = 'OT:' then
    begin
      N := N.AddNodeEx(AName, '');
      Inc(FNodeCount);
    end
    else
    if Token = 'CT:' then
      N := N.ParentNode
    else
    if Token = 'ET:' then
      N.AddNodeEx(AName, '')
    else
    if Token = 'TX:' then
    begin
      N.Value := AName;
      N.ValueType := xvtString;
    end
    else
    if Token = 'CD:' then
    begin
      N.Value := AName;
      N.ValueType := xvtCDATA;
    end;
    Inc(I);
  until I > C;
end;

procedure TJvXMLTree.LoadFromFile(const FileName: string);
begin
  ClearNodes;
  ClearAttributes;
  Lines.LoadFromFile(FileName);
  PreProcessXML(Lines);
  ParseXML;
end;

//=== { TJvXMLFilter } =======================================================

constructor TJvXMLFilter.Create(FilterStr: string);
begin
  inherited Create;
  Filters := TList.Create;
  Initialize(FilterStr);
end;

destructor TJvXMLFilter.Destroy;
var
  I: Integer;
begin
  for I := 0 to Filters.Count - 1 do
    TJvXMLFilterAtom(Filters[I]).Free;
  Filters.Free;
  inherited Destroy;
end;

procedure TJvXMLFilter.Initialize(FilterStr: string);
var
  LFilter: string;
  P1, P2: Integer;
  AttName, AttValue: string;
  AttOperator: TJvXMLFilterOperator;
  Atom: TJvXMLFilterAtom;
  //    A: TJvXMLAttribute;
  //    N: TJvXMLNode;

  function TrimQuotes(const S: string): string;
  var
    L: Integer;
  begin
    Result := Trim(S);
    if S = '' then
      Exit;
    if (S[1] = '"') or (S[1] = '''') then
      Delete(Result, 1, 1);
    if S = '' then
      Exit;
    L := Length(Result);
    if (Result[L] = '"') or (Result[L] = '''') then
      Delete(Result, L, 1);
  end;

  function SplitNameValue(const S: string): Boolean;
  var
    PP: Integer;
  begin
    // (rom) inefficient implementation
    //      Result:=False;
    PP := PosStr(' $ne$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoNE;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 6, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $ine$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoINE;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 7, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $ge$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoGE;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 6, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $ige$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoIGE;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 7, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $gt$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoGT;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 6, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $igt$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoIGT;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 7, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $le$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoLE;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 6, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $ile$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoILE;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 7, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $lt$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoLT;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 6, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $ilt$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoILT;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 7, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $eq$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoEQ;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 6, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' $ieq$ ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoIEQ;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 7, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    PP := PosStr(' = ', S, 1);
    if PP > 0 then
    begin
      AttOperator := xfoEQ;
      AttName := Trim(Copy(S, 1, PP - 1));
      AttValue := TrimQuotes(Copy(S, PP + 3, Length(S)));
      Result := (AttName <> '') and (AttValue <> '');
      Exit;
    end;
    AttOperator := xfoNOP;
    AttName := S;
    AttValue := '';
    Result := True;
    Exit;
  end;

begin
  P1 := PosStr('[', FilterStr, 1);
  if P1 = 0 then
  begin // just a Name filter on Name
    Name := FilterStr;
    Exit;
  end
  else
  begin
    Name := Copy(FilterStr, 1, P1 - 1);
    Delete(FilterStr, 1, P1 - 1);
  end;
  repeat
    FilterStr := Trim(FilterStr);
    P1 := PosStr('[', FilterStr, 1);
    if P1 = 0 then
      Exit;
    P2 := PosStr(']', FilterStr, P1 + 1);
    if P2 = 0 then
      Exit;
    LFilter := Copy(FilterStr, P1 + 1, P2 - P1 - 1);
    Delete(FilterStr, 1, P2);
    if LFilter = '' then
      Exit;
    // check for attribute filter
    if LFilter[1] = '@' then
    begin
      if not SplitNameValue(Copy(LFilter, 2, Length(LFilter))) then
        Exit;
      Atom := TJvXMLFilterAtom.Create;
      Atom.Name := AttName;
      Atom.Operator := AttOperator;
      Atom.Value := AttValue;
      Atom.AttributeFilter := True;
      Filters.Add(Atom);
    end
    else
    begin // childfilter
      if not SplitNameValue(LFilter) then
        Exit;
      Atom := TJvXMLFilterAtom.Create;
      Atom.Name := AttName;
      Atom.Operator := AttOperator;
      Atom.Value := AttValue;
      Atom.AttributeFilter := False;
      Filters.Add(Atom);
    end;
  until FilterStr = '';
end;

end.

