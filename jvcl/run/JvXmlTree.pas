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

{$I jvcl.inc}

unit JvXmlTree;

interface

uses
  SysUtils, Classes,
  {$IFDEF DELPHI6_UP}
  Variants,
  {$ENDIF DELPHI6_UP}
  JvStrings;

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
    procedure SetName(const Value: string);
    procedure SetOperator(const Value: TJvXMLFilterOperator);
    procedure SetValue(const Value: string);
    procedure SetAttributeFilter(const Value: Boolean);
  public
    property Name: string read FName write SetName;
    property Operator: TJvXMLFilterOperator read FOperator write SetOperator;
    property Value: string read FValue write SetValue;
    property AttributeFilter: Boolean read FAttributeFilter write SetAttributeFilter;
  end;

  TJvXMLFilter = class(TObject)
  private
    FName: string;
    FFilters: TList;
    procedure SetName(const Value: string);
    procedure SetFilters(const Value: TList);
  public
    constructor Create(FilterStr: string);
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property Filters: TList read FFilters write SetFilters;
  end;

  TJvXMLNode = class;

  TJvXMLAttribute = class(TObject)
  private
    FName: string;
    FValue: Variant;
    FParent: TJvXMLNode;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: Variant);
    procedure SetParent(const Value: TJvXMLNode);
  public
    constructor Create(AParent: TJvXMLNode; AName: string; AValue: Variant);
    function Document: string;
    property Name: string read FName write SetName;
    property Value: Variant read FValue write SetValue;
    property Parent: TJvXMLNode read FParent write SetParent;
  end;

  TJvXMLNode = class(TObject)
  private
    FName: string;
    FValue: Variant;
    FNodes: TList;
    FAttributes: TList;
    FParentNode: TJvXMLNode;
    FValueType: TJvXMLValueType;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: Variant);
    procedure SetNodes(const Value: TList);
    procedure SetAttributes(const Value: TList);
    procedure SetParentNode(const Value: TJvXMLNode);
    procedure SetValueType(const Value: TJvXMLValueType);
  public
    constructor Create(AName: string; AValue: Variant; AParent: TJvXMLNode);
    destructor Destroy; override;
    // added 29-July-2000
    function GetNamePathNode(APath: string): TJvXMLNode;
    procedure DeleteNamePathNode(APath: string);
    function ForceNamePathNode(APath: string): TJvXMLNode;
    function GetNamePathNodeAttribute(APath, AName: string): TJvXMLAttribute;
    procedure DeleteNamePathNodeAttribute(APath, AName: string);
    function ForceNamePathNodeAttribute(APath, AName: string; AValue: Variant): TJvXMLAttribute;
    function AddNode(AName: string; AValue: Variant): TJvXMLNode;
    function AddNodeEx(AName: string; AValue: Variant): TJvXMLNode;
    procedure DeleteNode(Index: Integer);
    procedure ClearNodes;
    function AddAttribute(AName: string; AValue: Variant): TJvXMLAttribute;
    function GetAttributeValue(AName: string): Variant;
    procedure DeleteAttribute(Index: Integer);
    procedure ClearAttributes;
    function Document(aLevel: Integer): string;
    function getNodePath: string;
    function getNamedNode(AName: string): TJvXMLNode;
    function SelectSingleNode(pattern: string): TJvXMLNode;
    procedure selectNodes(pattern: string; aList: TList);
    function transformNode(stylesheet: TJvXMLNode): string;
    function process(aLevel: Integer; node: TJvXMLNode): string;
    function findNamedNode(AName: string): TJvXMLNode;
    procedure findNamedNodes(AName: string; aList: TList);
    procedure getAllNodes(aList: TList);
    function getNamedAttribute(AName: string): TJvXMLAttribute;
    procedure findNamedAttributes(AName: string; aList: TList);
    function matchFilter(objFilter: TJvXMLFilter): Boolean;
    procedure matchPattern(aPattern: string; aList: TList);
    procedure getNodeNames(aList: TStringList);
    procedure getAttributeNames(aList: TStringList);
    function getNameSpace: string;
    function hasChildNodes: Boolean;
    function cloneNode: TJvXMLNode;
    function firstChild: TJvXMLNode;
    function lastChild: TJvXMLNode;
    function previousSibling: TJvXMLNode;
    function nextSibling: TJvXMLNode;
    function moveAddNode(Dest: TJvXMLNode): TJvXMLNode;
    function moveInsertNode(Dest: TJvXMLNode): TJvXMLNode;
    function removeChildNode(aNode: TJvXMLNode): TJvXMLNode;
    property Name: string read FName write SetName;
    property Value: Variant read FValue write SetValue;
    property ValueType: TJvXMLValueType read FValueType write SetValueType;
    property Nodes: TList read FNodes write SetNodes;
    property parentNode: TJvXMLNode read FParentNode write SetParentNode;
    property Attributes: TList read FAttributes write SetAttributes;
  end;

  TJvXMLTree = class(TJvXMLNode)
  private
    FLines: TStringList;
    FNodeCount: Integer;
    procedure SetLines(const Value: TStringList);
    function getText: string;
    procedure setText(const Value: string);
  public
    constructor Create(AName: string; AValue: Variant; AParent: TJvXMLNode);
    destructor Destroy; override;
    procedure ParseXML;
    procedure LoadFromFile(fn: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(aFile: string);
    procedure SaveToStream(Stream: TStream);
    function asText: string;
    property Lines: TStringList read FLines write SetLines;
    property NodeCount: Integer read FNodeCount;
    property Text: string read getText write setText;
  end;

procedure PreProcessXML(aList: TStringList);

implementation

uses
  {$IFDEF BCB}
  {$IFNDEF BCB5}
  Variants,
  {$ENDIF BCB5}
  {$ENDIF BCB}
  JvConsts;

procedure PreProcessXML(aList: TStringList);
var
  oList: TStringList;
  s, xTag, xText, xData: string;
  p1, p2, c: Integer;
  aLevel: Integer;

  function clean(aText: string): string;
  begin
    Result := stringreplace(aText, sLineBreak, ' ', [rfreplaceall]);
    Result := stringreplace(Result, tab, ' ', [rfreplaceall]);
    Result := Trim(Result);
  end;

  function cleanCDATA(aText: string): string;
  begin
    Result := stringreplace(aText, sLineBreak, '\n ', [rfreplaceall]);
    Result := stringreplace(Result, tab, '\t ', [rfreplaceall]);
  end;

  function spc: string;
  begin
    if alevel < 1 then
      Result := ''
    else
      Result := stringofchar(' ', 2 * aLevel);
  end;
begin
  oList := TStringList.Create;
  s := aList.text;
  xText := '';
  xTag := '';
  p1 := 1;
  c := Length(s);
  aLevel := 0;
  repeat
    p2 := PosStr('<', s, p1);
    if p2 > 0 then
    begin
      xText := Trim(Copy(s, p1, p2 - p1));
      if xText <> '' then
      begin
        oList.Append('TX:' + clean(xText));
      end;
      p1 := p2;
      // check for CDATA
      if uppercase(Copy(s, p1, 9)) = '<![CDATA[' then
      begin
        p2 := posstr(']]>', s, p1);
        xData := Copy(s, p1 + 9, p2 - p1 - 9);
        oList.Append('CD:' + cleanCDATA(xData));
        p1 := p2 + 2;
      end
      else
      begin
        p2 := posstr('>', s, p1);
        if p2 > 0 then
        begin
          xTag := Copy(s, p1 + 1, p2 - p1 - 1);
          p1 := p2;
          if xTag[1] = '/' then
          begin
            delete(xTag, 1, 1);
            oList.Append('CT:' + clean(xTag));
            dec(aLevel);
          end
          else
          if xtag[Length(xTag)] = '/' then
          begin
            oList.Append('ET:' + clean(xTag));
          end
          else
          begin
            inc(aLevel);
            oList.Append('OT:' + clean(xTag));
          end
        end
      end
    end
    else
    begin
      xText := Trim(Copy(s, p1, Length(s)));
      if xText <> '' then
      begin
        oList.Append('TX:' + clean(xText));
      end;
      p1 := c;
    end;
    inc(p1);
  until p1 > c;
  alist.assign(oList);
  oList.Free;
end;

{ TJvXMLNode }

function TJvXMLNode.AddAttribute(AName: string;
  AValue: Variant): TJvXMLAttribute;
var
  n: TJvXMLAttribute;
begin
  n := TJvXMLAttribute.Create(Self, AName, AValue);
  Attributes.Add(n);
  Result := n;
end;

function TJvXMLNode.AddNode(AName: string; AValue: Variant): TJvXMLNode;
var
  n: TJvXMLNode;
begin
  n := TJvXMLNode.Create(AName, AValue, Self);
  Self.Nodes.Add(n);
  Result := n
end;

// adds node and parses any attributes;

function TJvXMLNode.AddNodeEx(AName: string; AValue: Variant): TJvXMLNode;
var
  n: TJvXMLNode;
  s, sn, sv: string;
  c, p1, p2: Integer;
begin
  n := TJvXMLNode.Create(AName, AValue, Self);
  Self.Nodes.Add(n);
  Result := n;
  c := Length(AName);
  //first parse name
  p1 := posstr(' ', AName, 1);
  if p1 = 0 then Exit;
  s := Copy(AName, 1, p1 - 1);
  n.Name := s;
  repeat
    // find '='
    p2 := posStr('=', AName, p1);
    if p2 = 0 then Exit;
    sn := Trim(Copy(AName, p1, p2 - p1));
    p1 := p2;
    // find begin of value
    p1 := posStr('"', AName, p1);
    if p1 = 0 then Exit;
    p2 := posStr('"', AName, p1 + 1);
    if p2 = 0 then Exit;
    sv := Copy(AName, p1 + 1, p2 - p1 - 1);
    n.AddAttribute(sn, sv);
    p1 := p2 + 1;
  until p1 > c;
end;

function TJvXMLNode.getNamedAttribute(AName: string): TJvXMLAttribute;
var
  i: Integer;
  n: TJvXMLAttribute;
begin
  Result := nil;
  if Attributes.Count = 0 then Exit;
  for i := 0 to Attributes.count - 1 do
  begin
    n := TJvXMLAttribute(Attributes[i]);
    if n.name = AName then
    begin
      Result := n;
      Exit;
    end;
  end;
end;

procedure TJvXMLNode.ClearAttributes;
var
  i: Integer;
begin
  if Attributes.count <> 0 then
  begin
    for i := 0 to Attributes.count - 1 do
      TJvXMLAttribute(Attributes[i]).Free;
    Attributes.clear;
  end;
end;

procedure TJvXMLNode.ClearNodes;
var
  i: Integer;
begin
  i := nodes.count;
  if i <> 0 then
  begin
    for i := 0 to nodes.count - 1 do
      TJvXMLNode(Nodes[i]).Free;
    nodes.clear;
  end;
end;

constructor TJvXMLNode.Create(AName: string; AValue: Variant; AParent: TJvXMLNode);
begin
  FNodes := TList.Create;
  FName := AName;
  FValue := AValue;
  FValueType := xvtString;
  FParentNode := AParent;
  FAttributes := TList.Create;
end;

procedure TJvXMLNode.DeleteAttribute(Index: Integer);
begin
  if (attributes.count > 0) and (Index < attributes.count) then
  begin
    TJvXMLAttribute(Attributes[Index]).Free;
    Attributes.Delete(Index);
  end;
end;

procedure TJvXMLNode.DeleteNode(Index: Integer);
begin
  if (nodes.count > 0) and (Index < nodes.count) then
  begin
    TJvXMLNode(Nodes[Index]).Free;
    nodes.Delete(Index);
  end;
end;

destructor TJvXMLNode.Destroy;
begin
  ClearNodes;
  FNodes.Free;
  ClearAttributes;
  FAttributes.Free;
  inherited;
end;

function TJvXMLNode.Document(aLevel: Integer): string;
var
  i: Integer;
  spc: string;

  function ExpandCDATA(AValue: string): string;
  begin
    Result := stringreplace(AValue, '\n ', sLineBreak, [rfreplaceall]);
    Result := stringreplace(Result, '\t ', tab, [rfreplaceall]);
  end;
begin
  if aLevel > 0 then
    spc := StringOfChar(' ', aLevel * 2)
  else
    spc := '';
  Result := spc + '<' + Name;
  if Attributes.Count > 0 then
    for i := 0 to Attributes.count - 1 do
      Result := Result + TJvXMLAttribute(Attributes[i]).Document;
  if (nodes.count = 0) and (value = '') then
  begin
    Result := Result + ' />' + sLineBreak;
    Exit;
  end
  else
    Result := Result + '>' + sLineBreak;
  if Value <> '' then
  begin
    if ValueType = xvtString then
      Result := Result + spc + '  ' + Value + sLineBreak
    else
    if ValueType = xvtCDATA then
    begin
      Result := Result + spc + '  ' + '<![CDATA[' + ExpandCDATA(value) + ']]>' + sLineBreak;
    end
  end;
  if nodes.count <> 0 then
    for i := 0 to nodes.count - 1 do
      Result := Result + TJvXMLNode(nodes[i]).Document(aLevel + 1);
  Result := Result + spc + '</' + Name + '>' + sLineBreak;
end;

// duplicates a node recursively

function TJvXMLNode.cloneNode: TJvXMLNode;
var
  i: Integer;
  n: TJvXMLNode;
begin
  Result := TJvXMLNode.Create(name, value, nil);
  Result.name := name;
  Result.value := value;
  if Attributes.count > 0 then
  begin
    for i := 0 to Attributes.count - 1 do
    begin
      Result.AddAttribute(TJvXMLAttribute(Attributes[i]).name, TJvXMLAttribute(Attributes[i]).value);
    end;
  end;
  if nodes.count > 0 then
  begin
    for i := 0 to nodes.count - 1 do
    begin
      n := TJvXMLNode(nodes[i]).cloneNode;
      Result.Nodes.Add(n);
    end;
  end;
end;

function TJvXMLNode.getNamedNode(AName: string): TJvXMLNode;
var
  i: Integer;
  n: TJvXMLNode;
begin
  Result := nil;
  if Nodes.Count = 0 then Exit;
  for i := 0 to Nodes.count - 1 do
  begin
    n := TJvXMLNode(nodes[i]);
    if n.name = AName then
    begin
      Result := n;
      Exit;
    end;
  end;
end;

procedure TJvXMLNode.SetAttributes(const Value: TList);
begin
  FAttributes := Value;
end;

procedure TJvXMLNode.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TJvXMLNode.SetNodes(const Value: TList);
begin
  FNodes := Value;
end;

procedure TJvXMLNode.SetParentNode(const Value: TJvXMLNode);
begin
  FParentNode := Value;
end;

procedure TJvXMLNode.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

function TJvXMLNode.firstChild: TJvXMLNode;
begin
  if Nodes.Count > 0 then
    Result := TJvXMLNode(nodes[0])
  else
    Result := nil;
end;

function TJvXMLNode.lastChild: TJvXMLNode;
begin
  if nodes.count > 0 then
    Result := TJvXMLNode(nodes[nodes.count - 1])
  else
    Result := nil;
end;

function TJvXMLNode.nextSibling: TJvXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  if ParentNode = nil then Exit;
  Index := ParentNode.Nodes.IndexOf(Self);
  if Index = -1 then Exit;
  if Index < ParentNode.nodes.Count - 1 then
    Result := TJvXMLNode(ParentNode.nodes[Index + 1]);
end;

function TJvXMLNode.previousSibling: TJvXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  if ParentNode = nil then Exit;
  Index := ParentNode.Nodes.IndexOf(Self);
  if Index = -1 then Exit;
  if Index > 0 then
    Result := TJvXMLNode(ParentNode.nodes[Index - 1]);
end;
// moves a node to a new location

function TJvXMLNode.moveInsertNode(Dest: TJvXMLNode): TJvXMLNode;
var
  index1, index2: Integer;
begin
  Result := nil;
  if Dest.parentNode = nil then Exit; // can not move to root
  index1 := Self.parentNode.Nodes.IndexOf(Self);
  if index1 = -1 then Exit;
  index2 := dest.parentNode.Nodes.IndexOf(dest);
  if index2 = -1 then Exit;
  dest.parentNode.Nodes.Insert(index2, Self);
  Self.parentNode.nodes.Delete(index1);
  Self.parentNode := dest.parentnode;
  Result := Self;
end;

function TJvXMLNode.moveAddNode(Dest: TJvXMLNode): TJvXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  if Dest = nil then Exit; // can not move to root
  Index := Self.parentNode.Nodes.IndexOf(Self);
  if Index = -1 then Exit;
  dest.Nodes.Add(Self);
  Self.parentNode.nodes.Delete(Index);
  Self.parentNode := dest;
  Result := Self;
end;

// removes and Frees the childnode recursively.
// returns Self when done, or nil in case of error

function TJvXMLNode.removeChildNode(aNode: TJvXMLNode): TJvXMLNode;
var
  Index: Integer;
begin
  Result := nil;
  Index := nodes.IndexOf(aNode);
  if Index = -1 then Exit;
  nodes.Delete(Index);
  aNode.Free;
  Result := Self;
end;

function TJvXMLNode.hasChildNodes: Boolean;
begin
  Result := nodes.count > 0;
end;

procedure TJvXMLNode.getAttributeNames(aList: TStringList);
var
  i, c: Integer;
begin
  aList.Clear;
  c := Attributes.count;
  if c = 0 then Exit;
  for i := 0 to c - 1 do
    aList.append(TJvXMLAttribute(Attributes[i]).name);
end;

procedure TJvXMLNode.getNodeNames(aList: TStringList);
var
  i, c: Integer;
begin
  aList.Clear;
  c := Nodes.count;
  if c = 0 then Exit;
  for i := 0 to c - 1 do
    aList.append(TJvXMLNode(Nodes[i]).name);
end;

function TJvXMLNode.getNodePath: string;
var
  n: TJvXMLNode;
begin
  n := Self;
  Result := name;
  while n.parentNode <> nil do
  begin
    n := n.parentNode;
    Result := n.name + '/' + Result;
  end;
end;

// search recursively for a named node

function TJvXMLNode.findNamedNode(AName: string): TJvXMLNode;
var
  i: Integer;
  n: TJvXMLNode;
begin
  Result := nil;
  if Nodes.Count = 0 then Exit;
  for i := 0 to Nodes.count - 1 do
  begin
    n := TJvXMLNode(nodes[i]);
    if n.name = AName then
    begin
      Result := n;
      Exit;
    end
    else
    begin // recurse
      Result := n.findNamedNode(AName);
      if Result <> nil then Exit;
    end;
  end;
end;

// add all found named nodes to aList

procedure TJvXMLNode.findNamedNodes(AName: string; aList: TList);
var
  i: Integer;
  n: TJvXMLNode;
begin
  if Nodes.Count = 0 then Exit;
  for i := 0 to Nodes.count - 1 do
  begin
    n := TJvXMLNode(nodes[i]);
    if n.name = AName then
      alist.Add(n);
    // recurse
    n.findNamedNodes(AName, aList);
  end;
end;

// add recursively all nodes to aList
// the list only contains pointers to the nodes
// typecast to use, e.g. n:=TJvXMLNode(aList[0]);

procedure TJvXMLNode.getAllNodes(aList: TList);
var
  i: Integer;
  n: TJvXMLNode;
begin
  if Nodes.Count = 0 then Exit;
  for i := 0 to Nodes.count - 1 do
  begin
    n := TJvXMLNode(nodes[i]);
    alist.Add(n);
    // recurse
    n.getAllNodes(aList);
  end;
end;

// add recursively all nodes with matching named attribute to aList
// the list only contains pointers to the nodes
// typecast to use, e.g. n:=TJvXMLNode(aList[0]);

procedure TJvXMLNode.findNamedAttributes(AName: string; aList: TList);
var
  i, c: Integer;
  n: TJvXMLNode;
begin
  c := Attributes.count;
  if c > 0 then
    for i := 0 to c - 1 do
    begin
      if TJvXMLAttribute(Attributes[i]).name = AName then
      begin
        aList.Add(Self);
        break;
      end;
    end;
  if Nodes.Count = 0 then Exit;
  for i := 0 to Nodes.count - 1 do
  begin
    n := TJvXMLNode(nodes[i]);
    n.findNamedAttributes(AName, aList);
  end;
end;

{
this procedure adds the node to aList when it matches the pattern
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
 // the root node and all nodes below
 //*  all element nodes below the root node
 book//author  <author> elements that are descendants of <book> elements
 .//author  <author elements that are descendants of the current element
 *  non-root elements, irrespective of the element name
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

procedure TJvXMLNode.matchPattern(aPattern: string; aList: TList);
begin
  // to be implemented
end;

procedure TJvXMLNode.SetValueType(const Value: TJvXMLValueType);
begin
  FValueType := Value;
end;

{select a node based on path info
 e.g. booklist/book/category will find the first
 <category> that is a child of <book> that is a child of <booklist>
 }

function TJvXMLNode.SelectSingleNode(pattern: string): TJvXMLNode;
var
  npattern, aFilter: string;
  p, i, c: Integer;
  n: TJvXMLNode;
  objFilter: TJvXMLFilter;
begin
  Result := nil;
  c := nodes.count;
  if c = 0 then Exit;
  p := pos('/', pattern);
  if p = 0 then
  begin
    objFilter := TJvXMLFilter.Create(pattern);
    for i := 0 to c - 1 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.matchFilter(objFilter) then
      begin
        Result := n;
        objFilter.Free;
        Exit;
      end;
    end;
    objFilter.Free;
    Exit; // not found;
  end
  else
  begin
    aFilter := Copy(pattern, 1, p - 1);
    nPattern := Copy(pattern, p + 1, Length(pattern));
    objFilter := TJvXMLFilter.Create(aFilter);
    for i := 0 to c - 1 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.matchFilter(objFilter) then
      begin
        Result := n.SelectSingleNode(npattern);
        if Result <> nil then
        begin
          objFilter.Free;
          Exit
        end;
      end;
    end;
    objFilter.Free;
  end;
end;

// filter contains name + any filters between []

function TJvXMLNode.matchFilter(objFilter: TJvXMLFilter): Boolean;
var
  i, j: Integer;
  a: TJvXMLAttribute;
  n: TJvXMLNode;
  attName: string;
  atom: TJvXMLFilterAtom;
  attResult: Boolean;

  function evalAtom(AValue: string): Boolean;
  begin
    Result := false;
    case atom.Operator of
      xfoNOP: Result := true;
      xfoEQ: Result := AValue = atom.Value;
      xfoIEQ: Result := comparetext(AValue, atom.value) = 0;
      xfoNE: Result := avalue <> atom.value;
      xfoINE: Result := comparetext(AValue, atom.value) <> 0;
      xfoGT:
        try
          Result := Strtofloat(avalue) > strtofloat(atom.value);
        except
        end;
      xfoIGT: Result := comparetext(AValue, atom.value) > 0;
      xfoLT:
        try
          Result := Strtofloat(avalue) < strtofloat(atom.value);
        except
        end;
      xfoILT: Result := comparetext(AValue, atom.value) < 0;
      xfoGE:
        try
          Result := Strtofloat(avalue) >= strtofloat(atom.value);
        except
        end;
      xfoIGE: Result := comparetext(AValue, atom.value) >= 0;
      xfoLE:
        try
          Result := Strtofloat(avalue) <= strtofloat(atom.value);
        except
        end;
      xfoILE: Result := comparetext(AValue, atom.value) <= 0;
    end;

  end;
begin
  Result := false;
  AttResult := false;
  if objFilter.Filters.Count = 0 then
  begin // just filter on name
    Result := objFilter.Name = name;
    Exit;
  end;
  for i := 0 to objFilter.Filters.count - 1 do
  begin
    atom := TJvXMLFilterAtom(objFilter.Filters[i]);
    if atom.AttributeFilter then
    begin
      attName := atom.Name;
      if attName = '*' then
      begin // match any attribute
        if Attributes.Count = 0 then Exit;
        for j := 0 to Attributes.count - 1 do
        begin
          a := TJvXMLAttribute(Attributes[j]);
          attResult := evalAtom(a.value);
          if AttResult then break;
        end;
        if not AttResult then Exit;
      end
      else
      begin
        a := GetNamedAttribute(attName);
        if a = nil then Exit;
        if not evalAtom(a.value) then Exit;
      end;
    end
    else
    begin
      attName := atom.Name;
      n := GetNamedNode(attName);
      if n = nil then Exit;
      if not evalAtom(n.value) then Exit;
    end;
  end;
  Result := true;
end;

procedure TJvXMLNode.SelectNodes(pattern: string; aList: TList);
var
  npattern: string;
  p, i, c: Integer;
  n: TJvXMLNode;
  aFilter: string;
  objFilter: TJvXMLFilter;
  recurse: Boolean;
begin
  c := nodes.count;
  if c = 0 then Exit;
  if Copy(pattern, 1, 2) = '//' then
  begin //recursive
    delete(pattern, 1, 2);
    recurse := true;
  end
  else
    recurse := false;
  p := pos('/', pattern);
  if p = 0 then
  begin
    aFilter := pattern;
    objFilter := TJvXMLFilter.Create(aFilter);
    for i := 0 to c - 1 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.matchFilter(objFilter) then
        aList.Add(n)
      else
      begin
        if recurse then
          n.SelectNodes('//' + pattern, aList);
      end;
    end;
    objFilter.Free;
  end
  else
  begin
    aFilter := Copy(pattern, 1, p - 1);
    if Copy(pattern, p, 2) = '//' then
      npattern := Copy(pattern, p, Length(pattern))
    else
      npattern := Copy(pattern, p + 1, Length(pattern));
    objFilter := TJvXMLFilter.Create(aFilter);
    for i := 0 to c - 1 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.matchFilter(objFilter) then
        n.SelectNodes(npattern, aList)
      else
      begin
        if recurse then
          n.selectNodes('//' + pattern, aList);
      end;
    end;
    objFilter.Free;
  end;
end;

// the XSL implementation
// although this function returns a string, the string itself can be parsed to Create a DOM

function TJvXMLNode.transformNode(stylesheet: TJvXMLNode): string;
begin
  // to be implemented;
  Result := stylesheet.process(0, Self);
end;

// used in conjunction with the transformNode function.
// basically works like the Document function except for nodes with processing instructions

function TJvXMLNode.process(aLevel: Integer; node: TJvXMLNode): string;
var
  i: Integer;
  spc: string;

  function ExpandCDATA(AValue: string): string;
  begin
    Result := stringreplace(AValue, '\n ', sLineBreak, [rfreplaceall]);
    Result := stringreplace(Result, '\t ', tab, [rfreplaceall]);
  end;
begin
  if parentNode = nil then
  begin
    if nodes.count <> 0 then
      for i := 0 to nodes.count - 1 do
        Result := Result + TJvXMLNode(nodes[i]).process(aLevel + 1, node);
    Exit;
  end;
  if aLevel > 0 then
    spc := StringOfChar(' ', aLevel * 2)
  else
    spc := '';
  Result := spc + '<' + Name;
  if Attributes.Count > 0 then
    for i := 0 to Attributes.count - 1 do
      Result := Result + TJvXMLAttribute(Attributes[i]).Document;
  if (nodes.count = 0) and (value = '') then
  begin
    Result := Result + ' />' + sLineBreak;
    Exit;
  end
  else
    Result := Result + '>' + sLineBreak;
  if Value <> '' then
  begin
    if ValueType = xvtString then
      Result := Result + spc + '  ' + Value + sLineBreak
    else
    if ValueType = xvtCDATA then
    begin
      Result := Result + spc + '  ' + '<![CDATA[' + ExpandCDATA(value) + ']]>' + sLineBreak;
    end
  end;
  if nodes.count <> 0 then
    for i := 0 to nodes.count - 1 do
      Result := Result + TJvXMLNode(nodes[i]).process(aLevel + 1, node);
  Result := Result + spc + '</' + Name + '>' + sLineBreak;
end;

function TJvXMLNode.getNameSpace: string;
var
  p: Integer;
begin
  p := pos(':', FName);
  if p > 0 then
    Result := Copy(FName, 1, p - 1)
  else
    Result := '';
end;

// find the node with a path like customers/regional/jansoft

function TJvXMLNode.GetNamePathNode(APath: string): TJvXMLNode;
var
  AName, newpath, sindex: string;
  c, i, p, Index, indexc: Integer;
  n: TJvXMLNode;
begin
  Result := nil;
  c := nodes.Count;
  if c = 0 then Exit;
  if APath = '' then
  begin
    Result := Self;
    Exit;
  end;
  p := posstr('/', APath, 1);
  if p = 0 then
  begin
    AName := APath;
    newpath := '';
  end
  else
  begin
    AName := Copy(APath, 1, p - 1);
    newPath := Copy(APath, p + 1, Length(APath));
  end;
  // now check for any Index []
  p := posstr('[', aname, 1);
  Index := 0; // search first by default
  indexc := 0;
  if p > 0 then
  begin
    sindex := Copy(AName, p + 1, Length(AName) - p - 1);
    AName := Copy(AName, 1, p - 1);
    if sindex = 'end' then
      Index := -1
    else
    try
      Index := strtoint(sindex);
      if Index >= c then Exit;
    except
      Exit;
    end
  end;
  if Index = -1 then // search end from end
    for i := c - 1 downto 0 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.Name = aname then
      begin
        if newpath = '' then
        begin
          Result := n;
          Exit;
        end
        else
        begin
          Result := n.GetNamePathNode(newPath);
          Exit;
        end;
      end;
    end
  else // search from beginning indexed
    for i := 0 to c - 1 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.Name = aname then
      begin
        if Index = indexc then
        begin
          if newpath = '' then
          begin
            Result := n;
            Exit;
          end
          else
          begin
            Result := n.GetNamePathNode(newPath);
            Exit;
          end;
        end
        else
          inc(indexc);
      end;
    end;
end;

function TJvXMLNode.ForceNamePathNode(APath: string): TJvXMLNode;
var
  AName, newpath: string;
  c, i, p: Integer;
  n: TJvXMLNode;
  doappend: Boolean;
begin
  //  Result:=nil;
  p := posstr('/', APath, 1);
  if p = 0 then
  begin
    AName := APath;
    newpath := '';
  end
  else
  begin
    AName := Copy(APath, 1, p - 1);
    newPath := Copy(APath, p + 1, Length(APath));
  end;
  p := posstr('+', AName, 1);
  if p > 0 then delete(AName, p, 1);
  doappend := p > 0;
  c := nodes.Count;
  if (not doappend) and (c > 0) then
  begin
    for i := 0 to c - 1 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.Name = aname then
      begin
        if newpath = '' then
        begin
          Result := n;
          Exit;
        end
        else
        begin
          Result := n.ForceNamePathNode(newPath);
          Exit;
        end;
      end;
    end;
  end;
  // we dont have it , so force it;
  n := TJvXMLNode.Create(AName, '', Self);
  nodes.Add(n);
  if newpath = '' then
    Result := n
  else
    Result := n.ForceNamePathNode(newpath);
end;

function TJvXMLNode.ForceNamePathNodeAttribute(APath, AName: string;
  AValue: Variant): TJvXMLAttribute;
var
  n: TJvXMLNode;
  a: TJvXMLAttribute;
begin
  Result := nil;
  n := ForceNamePathNode(APath);
  if n = nil then Exit;
  a := n.GetNamedAttribute(AName);
  if a <> nil then
  begin
    a.Value := AValue;
    Result := a;
  end
  else
  begin
    Result := n.addAttribute(aname, avalue);
  end;
end;

function TJvXMLNode.GetNamePathNodeAttribute(APath, AName: string): TJvXMLAttribute;
var
  n: TJvXMLNode;
begin
  Result := nil;
  n := GetNamePathNode(APath);
  if n = nil then Exit;
  Result := n.GetNamedAttribute(AName);
end;

procedure TJvXMLNode.DeleteNamePathNode(APath: string);
var
  n, pn: TJvXMLNode;
  i: Integer;
begin
  if APath = '' then Exit;
  n := GetNamePathNode(APath);
  if n = nil then Exit;
  pn := n.parentNode;
  for i := 0 to pn.nodes.count - 1 do
  begin
    if TJvXMLNode(pn.nodes[i]) = n then
    begin
      pn.DeleteNode(i);
      Exit;
    end;
  end;
end;

procedure TJvXMLNode.DeleteNamePathNodeAttribute(APath, AName: string);
var
  a: TJvXMLAttribute;
  pn: TJvXMLNode;
  i: Integer;
begin
  a := GetNamePathNodeAttribute(APath, AName);
  if a = nil then Exit;
  pn := a.Parent;
  for i := 0 to pn.attributes.count - 1 do
  begin
    if TJvXMLAttribute(pn.attributes[i]) = a then
    begin
      pn.DeleteAttribute(i);
      Exit;
    end;
  end;
end;

function TJvXMLNode.GetAttributeValue(AName: string): Variant;
var
  i, c: Integer;
  a: TJvXMLAttribute;
begin
  Result := Null;
  c := attributes.count;
  if c = 0 then Exit;
  for i := 0 to c - 1 do
  begin
    a := TJvXMLAttribute(attributes[i]);
    if a.Name = AName then
    begin
      Result := a.Value;
      Exit;
    end;
  end;
end;

{ TJvXMLTree }

constructor TJvXMLTree.Create(AName: string; AValue: Variant; AParent: TJvXMLNode);
begin
  inherited Create(AName, AValue, AParent);
  FLines := TStringList.Create;
end;

destructor TJvXMLTree.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

function TJvXMLTree.asText: string;
var
  i, c: Integer;
begin
  c := Nodes.Count;
  if c = 0 then Exit;
  Result := '<' + Name;
  if Attributes.Count > 0 then
    for i := 0 to Attributes.count - 1 do
      Result := Result + TJvXMLAttribute(Attributes[i]).Document;
  Result := Result + '>' + sLineBreak;
  for i := 0 to c - 1 do
    Result := Result + TJvXMLNode(nodes[i]).Document(1);
  Result := Result + '</' + Name + '>' + sLineBreak;
end;

procedure TJvXMLTree.SaveToFile(aFile: string);
begin
  Lines.text := Text;
  Lines.SaveToFile(aFile)
end;

procedure TJvXMLTree.SetLines(const Value: TStringList);
begin
  FLines.assign(Value);
end;

procedure TJvXMLTree.LoadFromStream(Stream: TStream);
begin
  ClearNodes;
  ClearAttributes;
  Lines.LoadFromStream(Stream);
  PreProcessXML(FLines);
  ParseXML;
end;

procedure TJvXMLTree.SaveToStream(Stream: TStream);
begin
  Lines.text := asText;
  Lines.SaveToStream(Stream);
end;

function TJvXMLTree.getText: string;
var
  i, c: Integer;
begin
  c := Nodes.Count;
  if c = 0 then Exit;
  //  Result:='<'+Name;
  //  if Attributes.Count>0 then
  //  for i:=0 to Attributes.count-1 do
  //    Result:=Result+TJvXMLAttribute(Attributes[i]).Document;
  //  Result:=Result+'>'+sLineBreak;
  Result := '';
  for i := 0 to c - 1 do
    Result := Result + TJvXMLNode(nodes[i]).Document(0);
  //  Result:=Result+'</'+Name+'>'+sLineBreak;
end;

procedure TJvXMLTree.setText(const Value: string);
begin
  ClearNodes;
  ClearAttributes;
  Lines.text := Value;
  PreProcessXML(FLines);
  ParseXML;
end;

{ TJvXMLAttribute }

constructor TJvXMLAttribute.Create(AParent: TJvXMLNode; AName: string; AValue: Variant);
begin
  FName := AName;
  FValue := AValue;
  FParent := AParent;
end;

function TJvXMLAttribute.Document: string;
var
  s: string;
begin
  s := Value;
  Result := ' ' + Name + '="' + s + '"';
end;

procedure TJvXMLAttribute.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TJvXMLAttribute.SetParent(const Value: TJvXMLNode);
begin
  FParent := Value;
end;

procedure TJvXMLAttribute.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TJvXMLTree }

procedure TJvXMLTree.ParseXML;
var
  i, c: Integer;
  s, token, AName: string;
  n: TJvXMLNode;
begin
  i := 0;
  FNodeCount := 0;
  ClearNodes;
  ClearAttributes;
  Name := 'root';
  n := Self;
  c := Lines.Count - 1;
  repeat
    s := Lines[i];
    token := Copy(s, 1, 3);
    AName := Copy(s, 4, Length(s));
    if token = 'OT:' then
    begin
      n := n.AddNodeEx(AName, '');
      inc(FNodeCount);
    end
    else
    if token = 'CT:' then
    begin
      n := n.ParentNode;
    end
    else
    if token = 'ET:' then
    begin
      n.AddNodeEx(AName, '');
    end
    else
    if token = 'TX:' then
    begin
      n.Value := AName;
      n.ValueType := xvtString;
    end
    else
    if token = 'CD:' then
    begin
      n.value := AName;
      n.ValueType := xvtCDATA;
    end;
    inc(i);
  until i > c;
end;

procedure TJvXMLTree.LoadFromFile(fn: string);
begin
  ClearNodes;
  ClearAttributes;
  Lines.LoadFromFile(fn);
  PreProcessXML(FLines);
  ParseXML;
end;

{ TJvXMLFilter }

constructor TJvXMLFilter.Create(FilterStr: string);
var
  theFilter: string;
  p1, p2: Integer;
  attName, attValue: string;
  attOperator: TJvXMLFilterOperator;
  atom: TJvXMLFilterAtom;
  //    a:TJvXMLAttribute;
  //    n:TJvXMLNode;

  function trimquotes(s: string): string;
  var
    cc: Integer;
  begin
    Result := Trim(s);
    if s = '' then Exit;
    if (s[1] = '"') or (s[1] = '''') then delete(Result, 1, 1);
    if s = '' then Exit;
    cc := Length(Result);
    if (Result[cc] = '"') or (Result[cc] = '''') then delete(Result, cc, 1);
  end;

  function splitNameValue(s: string): Boolean;
  var
    pp: Integer;
  begin
    //      Result:=false;
    pp := posStr(' $ne$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoNE;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 6, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $ine$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoINE;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 7, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $ge$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoGE;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 6, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $ige$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoIGE;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 7, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $gt$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoGT;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 6, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $igt$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoIGT;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 7, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $le$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoLE;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 6, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $ile$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoILE;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 7, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $lt$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoLT;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 6, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $ilt$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoILT;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 7, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $eq$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoEQ;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 6, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' $ieq$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoIEQ;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 7, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    pp := posStr(' = ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoEQ;
      attName := Trim(Copy(s, 1, pp - 1));
      attvalue := trimquotes(Copy(s, pp + 3, Length(s)));
      Result := (attName <> '') and (attValue <> '');
      Exit;
    end;
    attOperator := xfoNOP;
    attName := s;
    attValue := '';
    Result := true;
    Exit;
  end;

begin
  Filters := TList.Create;
  p1 := posStr('[', FilterStr, 1);
  if p1 = 0 then
  begin // just a name filter on name
    name := FilterStr;
    Exit;
  end
  else
  begin
    Name := Copy(FilterStr, 1, p1 - 1);
    delete(FilterStr, 1, p1 - 1);
  end;
  repeat
    FilterStr := Trim(FilterStr);
    p1 := posStr('[', FilterStr, 1);
    if p1 = 0 then Exit;
    p2 := posStr(']', FilterStr, p1 + 1);
    if p2 = 0 then Exit;
    theFilter := Copy(FilterStr, p1 + 1, p2 - p1 - 1);
    delete(FilterStr, 1, p2);
    if theFilter = '' then Exit;
    // check for attribute filter
    if theFilter[1] = '@' then
    begin
      if not splitNameValue(Copy(theFilter, 2, Length(theFilter))) then Exit;
      atom := TJvXMLFilterAtom.Create;
      atom.Name := attName;
      atom.Operator := attOperator;
      atom.Value := attValue;
      atom.AttributeFilter := true;
      Filters.Add(atom);
    end
    else
    begin // childfilter
      if not splitNameValue(theFilter) then Exit;
      atom := TJvXMLFilterAtom.Create;
      atom.Name := attName;
      atom.Operator := attOperator;
      atom.Value := attValue;
      atom.AttributeFilter := false;
      Filters.Add(atom);
    end;
  until FilterStr = '';
end;

destructor TJvXMLFilter.Destroy;
var
  i: Integer;
begin
  if Filters.Count > 0 then
    for i := 0 to Filters.count - 1 do
      TJvXMLFilterAtom(Filters[i]).Free;
  filters.Free;
  inherited Destroy;
end;

procedure TJvXMLFilter.SetFilters(const Value: TList);
begin
  FFilters := Value;
end;

procedure TJvXMLFilter.SetName(const Value: string);
begin
  FName := Value;
end;

{ TJvXMLFilterAtom }

procedure TJvXMLFilterAtom.SetAttributeFilter(const Value: Boolean);
begin
  FAttributeFilter := Value;
end;

procedure TJvXMLFilterAtom.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TJvXMLFilterAtom.SetOperator(
  const Value: TJvXMLFilterOperator);
begin
  FOperator := Value;
end;

procedure TJvXMLFilterAtom.SetValue(const Value: string);
begin
  FValue := Value;
end;

end.



