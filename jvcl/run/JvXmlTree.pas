{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXMLTree.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
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
  TJvXMLFilterOperator = (xfoNOP, xfoEQ, xfoIEQ, xfoNE, xfoINE, xfoGE, xfoIGE, xfoLE, xfoILE, xfoGT, xfoIGT, xfoLT, xfoILT);

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
    Fparent: TJvXMLNode;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: Variant);
    procedure Setparent(const Value: TJvXMLNode);
  public
    constructor Create(aParent: TJvXMLNode; aName: string; aValue: Variant);
    function document: string;
    property Name: string read FName write SetName;
    property Value: Variant read FValue write SetValue;
    property parent: TJvXMLNode read Fparent write Setparent;
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
    constructor Create(aName: string; aValue: Variant; aParent: TJvXMLNode);
    destructor Destroy; override;
    // added 29-July-2000
    function getNamePathNode(aPath: string): TJvXMLNode;
    procedure deleteNamePathNode(aPath: string);
    function ForceNamePathNode(aPath: string): TJvXMLNode;
    function GetNamePathNodeAttribute(aPath, aName: string): TJvXMLAttribute;
    procedure deleteNamePathNodeAttribute(aPath, aName: string);
    function ForceNamePathNodeAttribute(aPath, aName: string; aValue: Variant): TJvXMLAttribute;
    function AddNode(aName: string; aValue: Variant): TJvXMLNode;
    function AddNodeEx(aName: string; aValue: Variant): TJvXMLNode;
    procedure DeleteNode(index: integer);
    procedure ClearNodes;
    function AddAttribute(aName: string; aValue: Variant): TJvXMLAttribute;
    function GetAttributeValue(aName: string): Variant;
    procedure DeleteAttribute(index: integer);
    procedure ClearAttributes;
    function document(aLevel: integer): string;
    function getNodePath: string;
    function getNamedNode(aName: string): TJvXMLNode;
    function SelectSingleNode(pattern: string): TJvXMLNode;
    procedure selectNodes(pattern: string; aList: TList);
    function transformNode(stylesheet: TJvXMLNode): string;
    function process(aLevel: integer; node: TJvXMLNode): string;
    function findNamedNode(aName: string): TJvXMLNode;
    procedure findNamedNodes(aName: string; aList: TList);
    procedure getAllNodes(aList: TList);
    function getNamedAttribute(aName: string): TJvXMLAttribute;
    procedure findNamedAttributes(aName: string; aList: TList);
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
    FNodeCount: integer;
    procedure SetLines(const Value: TStringList);
    function getText: string;
    procedure setText(const Value: string);
  public
    constructor Create(aName: string; aValue: Variant; aParent: TJvXMLNode);
    destructor Destroy; override;
    procedure ParseXML;
    procedure LoadFromFile(fn: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(aFile: string);
    procedure SaveToStream(Stream: TStream);
    function asText: string;
    property Lines: TStringList read FLines write SetLines;
    property NodeCount: integer read FNodeCount;
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
  p1, p2, c: integer;
  aLevel: integer;

  function clean(aText: string): string;
  begin
    result := stringreplace(aText, sLineBreak, ' ', [rfreplaceall]);
    result := stringreplace(result, tab, ' ', [rfreplaceall]);
    result := trim(result);
  end;

  function cleanCDATA(aText: string): string;
  begin
    result := stringreplace(aText, sLineBreak, '\n ', [rfreplaceall]);
    result := stringreplace(result, tab, '\t ', [rfreplaceall]);
  end;

  function spc: string;
  begin
    if alevel < 1 then
      result := ''
    else
      result := stringofchar(' ', 2 * aLevel);
  end;
begin
  oList := TStringList.Create;
  s := aList.text;
  xText := '';
  xTag := '';
  p1 := 1;
  c := length(s);
  aLevel := 0;
  repeat
    p2 := PosStr('<', s, p1);
    if p2 > 0 then
    begin
      xText := trim(copy(s, p1, p2 - p1));
      if xText <> '' then
      begin
        oList.Append('TX:' + clean(xText));
      end;
      p1 := p2;
      // check for CDATA
      if uppercase(copy(s, p1, 9)) = '<![CDATA[' then
      begin
        p2 := posstr(']]>', s, p1);
        xData := copy(s, p1 + 9, p2 - p1 - 9);
        oList.Append('CD:' + cleanCDATA(xData));
        p1 := p2 + 2;
      end
      else
      begin
        p2 := posstr('>', s, p1);
        if p2 > 0 then
        begin
          xTag := copy(s, p1 + 1, p2 - p1 - 1);
          p1 := p2;
          if xTag[1] = '/' then
          begin
            delete(xTag, 1, 1);
            oList.Append('CT:' + clean(xTag));
            dec(aLevel);
          end
          else if xtag[length(xTag)] = '/' then
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
      xText := trim(copy(s, p1, length(s)));
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

function TJvXMLNode.AddAttribute(aName: string;
  aValue: Variant): TJvXMLAttribute;
var
  n: TJvXMLAttribute;
begin
  n := TJvXMLAttribute.Create(self, aName, aValue);
  Attributes.Add(n);
  result := n;
end;

function TJvXMLNode.AddNode(aName: string; aValue: Variant): TJvXMLNode;
var
  n: TJvXMLNode;
begin
  n := TJvXMLNode.Create(aName, aValue, self);
  self.Nodes.Add(n);
  result := n
end;

// adds node and parses any attributes;

function TJvXMLNode.AddNodeEx(aName: string; aValue: Variant): TJvXMLNode;
var
  n: TJvXMLNode;
  s, sn, sv: string;
  c, p1, p2: integer;
begin
  n := TJvXMLNode.Create(aName, aValue, self);
  self.Nodes.Add(n);
  result := n;
  c := length(aName);
  //first parse name
  p1 := posstr(' ', aName, 1);
  if p1 = 0 then exit;
  s := copy(aName, 1, p1 - 1);
  n.Name := s;
  repeat
    // find '='
    p2 := posStr('=', aName, p1);
    if p2 = 0 then exit;
    sn := trim(copy(aName, p1, p2 - p1));
    p1 := p2;
    // find begin of value
    p1 := posStr('"', aName, p1);
    if p1 = 0 then exit;
    p2 := posStr('"', aName, p1 + 1);
    if p2 = 0 then exit;
    sv := copy(aName, p1 + 1, p2 - p1 - 1);
    n.AddAttribute(sn, sv);
    p1 := p2 + 1;
  until p1 > c;
end;

function TJvXMLNode.getNamedAttribute(aName: string): TJvXMLAttribute;
var
  i: integer;
  n: TJvXMLAttribute;
begin
  result := nil;
  if Attributes.Count = 0 then exit;
  for i := 0 to Attributes.count - 1 do
  begin
    n := TJvXMLAttribute(Attributes[i]);
    if n.name = aName then
    begin
      result := n;
      exit;
    end;
  end;
end;

procedure TJvXMLNode.ClearAttributes;
var
  i: integer;
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
  i: integer;
begin
  i := nodes.count;
  if i <> 0 then
  begin
    for i := 0 to nodes.count - 1 do
      TJvXMLNode(Nodes[i]).Free;
    nodes.clear;
  end;
end;

constructor TJvXMLNode.Create(aName: string; aValue: Variant; aParent: TJvXMLNode);
begin
  FNodes := TList.Create;
  FName := aName;
  FValue := aValue;
  FValueType := xvtString;
  FParentNode := aParent;
  FAttributes := TList.Create;
end;

procedure TJvXMLNode.DeleteAttribute(index: integer);
begin
  if (attributes.count > 0) and (index < attributes.count) then
  begin
    TJvXMLAttribute(Attributes[index]).Free;
    Attributes.Delete(index);
  end;
end;

procedure TJvXMLNode.DeleteNode(index: integer);
begin
  if (nodes.count > 0) and (index < nodes.count) then
  begin
    TJvXMLNode(Nodes[index]).Free;
    nodes.Delete(index);
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

function TJvXMLNode.document(aLevel: integer): string;
var
  i: integer;
  spc: string;

  function ExpandCDATA(aValue: string): string;
  begin
    result := stringreplace(aValue, '\n ', sLineBreak, [rfreplaceall]);
    result := stringreplace(result, '\t ', tab, [rfreplaceall]);
  end;
begin
  if aLevel > 0 then
    spc := StringOfChar(' ', aLevel * 2)
  else
    spc := '';
  result := spc + '<' + Name;
  if Attributes.Count > 0 then
    for i := 0 to Attributes.count - 1 do
      result := result + TJvXMLAttribute(Attributes[i]).document;
  if (nodes.count = 0) and (value = '') then
  begin
    result := result + ' />' + sLineBreak;
    exit;
  end
  else
    result := result + '>' + sLineBreak;
  if Value <> '' then
  begin
    if ValueType = xvtString then
      result := result + spc + '  ' + Value + sLineBreak
    else if ValueType = xvtCDATA then
    begin
      result := result + spc + '  ' + '<![CDATA[' + ExpandCDATA(value) + ']]>' + sLineBreak;
    end
  end;
  if nodes.count <> 0 then
    for i := 0 to nodes.count - 1 do
      result := result + TJvXMLNode(nodes[i]).document(aLevel + 1);
  result := result + spc + '</' + Name + '>' + sLineBreak;
end;

// duplicates a node recursively

function TJvXMLNode.cloneNode: TJvXMLNode;
var
  i: integer;
  n: TJvXMLNode;
begin
  result := TJvXMLNode.Create(name, value, nil);
  result.name := name;
  result.value := value;
  if Attributes.count > 0 then
  begin
    for i := 0 to Attributes.count - 1 do
    begin
      result.AddAttribute(TJvXMLAttribute(Attributes[i]).name, TJvXMLAttribute(Attributes[i]).value);
    end;
  end;
  if nodes.count > 0 then
  begin
    for i := 0 to nodes.count - 1 do
    begin
      n := TJvXMLNode(nodes[i]).cloneNode;
      result.Nodes.Add(n);
    end;
  end;
end;

function TJvXMLNode.getNamedNode(aName: string): TJvXMLNode;
var
  i: integer;
  n: TJvXMLNode;
begin
  result := nil;
  if Nodes.Count = 0 then exit;
  for i := 0 to Nodes.count - 1 do
  begin
    n := TJvXMLNode(nodes[i]);
    if n.name = aName then
    begin
      result := n;
      exit;
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
    result := TJvXMLNode(nodes[0])
  else
    result := nil;
end;

function TJvXMLNode.lastChild: TJvXMLNode;
begin
  if nodes.count > 0 then
    result := TJvXMLNode(nodes[nodes.count - 1])
  else
    result := nil;
end;

function TJvXMLNode.nextSibling: TJvXMLNode;
var
  index: integer;
begin
  result := nil;
  if ParentNode = nil then exit;
  index := ParentNode.Nodes.IndexOf(self);
  if index = -1 then exit;
  if index < ParentNode.nodes.Count - 1 then
    result := TJvXMLNode(ParentNode.nodes[index + 1]);
end;

function TJvXMLNode.previousSibling: TJvXMLNode;
var
  index: integer;
begin
  result := nil;
  if ParentNode = nil then exit;
  index := ParentNode.Nodes.IndexOf(self);
  if index = -1 then exit;
  if index > 0 then
    result := TJvXMLNode(ParentNode.nodes[index - 1]);
end;
// moves a node to a new location

function TJvXMLNode.moveInsertNode(Dest: TJvXMLNode): TJvXMLNode;
var
  index1, index2: integer;
begin
  result := nil;
  if Dest.parentNode = nil then exit; // can not move to root
  index1 := self.parentNode.Nodes.IndexOf(self);
  if index1 = -1 then exit;
  index2 := dest.parentNode.Nodes.IndexOf(dest);
  if index2 = -1 then exit;
  dest.parentNode.Nodes.Insert(index2, self);
  self.parentNode.nodes.Delete(index1);
  self.parentNode := dest.parentnode;
  result := self;
end;

function TJvXMLNode.moveAddNode(Dest: TJvXMLNode): TJvXMLNode;
var
  index: integer;
begin
  result := nil;
  if Dest = nil then exit; // can not move to root
  index := self.parentNode.Nodes.IndexOf(self);
  if index = -1 then exit;
  dest.Nodes.Add(self);
  self.parentNode.nodes.Delete(index);
  self.parentNode := dest;
  result := self;
end;

// removes and Frees the childnode recursively.
// returns self when done, or nil in case of error

function TJvXMLNode.removeChildNode(aNode: TJvXMLNode): TJvXMLNode;
var
  index: integer;
begin
  result := nil;
  index := nodes.IndexOf(aNode);
  if index = -1 then exit;
  nodes.Delete(index);
  aNode.Free;
  result := self;
end;

function TJvXMLNode.hasChildNodes: Boolean;
begin
  result := nodes.count > 0;
end;

procedure TJvXMLNode.getAttributeNames(aList: TStringList);
var
  i, c: integer;
begin
  aList.Clear;
  c := Attributes.count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
    aList.append(TJvXMLAttribute(Attributes[i]).name);
end;

procedure TJvXMLNode.getNodeNames(aList: TStringList);
var
  i, c: integer;
begin
  aList.Clear;
  c := Nodes.count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
    aList.append(TJvXMLNode(Nodes[i]).name);
end;

function TJvXMLNode.getNodePath: string;
var
  n: TJvXMLNode;
begin
  n := self;
  result := name;
  while n.parentNode <> nil do
  begin
    n := n.parentNode;
    result := n.name + '/' + result;
  end;
end;

// search recursively for a named node

function TJvXMLNode.findNamedNode(aName: string): TJvXMLNode;
var
  i: integer;
  n: TJvXMLNode;
begin
  result := nil;
  if Nodes.Count = 0 then exit;
  for i := 0 to Nodes.count - 1 do
  begin
    n := TJvXMLNode(nodes[i]);
    if n.name = aName then
    begin
      result := n;
      exit;
    end
    else
    begin // recurse
      result := n.findNamedNode(aName);
      if result <> nil then exit;
    end;
  end;
end;

// add all found named nodes to aList

procedure TJvXMLNode.findNamedNodes(aName: string; aList: TList);
var
  i: integer;
  n: TJvXMLNode;
begin
  if Nodes.Count = 0 then exit;
  for i := 0 to Nodes.count - 1 do
  begin
    n := TJvXMLNode(nodes[i]);
    if n.name = aName then
      alist.Add(n);
    // recurse
    n.findNamedNodes(aName, aList);
  end;
end;

// add recursively all nodes to aList
// the list only contains pointers to the nodes
// typecast to use, e.g. n:=TJvXMLNode(aList[0]);

procedure TJvXMLNode.getAllNodes(aList: TList);
var
  i: integer;
  n: TJvXMLNode;
begin
  if Nodes.Count = 0 then exit;
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

procedure TJvXMLNode.findNamedAttributes(aName: string; aList: TList);
var
  i, c: integer;
  n: TJvXMLNode;
begin
  c := Attributes.count;
  if c > 0 then
    for i := 0 to c - 1 do
    begin
      if TJvXMLAttribute(Attributes[i]).name = aName then
      begin
        aList.Add(self);
        break;
      end;
    end;
  if Nodes.Count = 0 then exit;
  for i := 0 to Nodes.count - 1 do
  begin
    n := TJvXMLNode(nodes[i]);
    n.findNamedAttributes(aName, aList);
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

index can be used to specify a particular node within a matching set
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
  p, i, c: integer;
  n: TJvXMLNode;
  objFilter: TJvXMLFilter;
begin
  result := nil;
  c := nodes.count;
  if c = 0 then exit;
  p := pos('/', pattern);
  if p = 0 then
  begin
    objFilter := TJvXMLFilter.Create(pattern);
    for i := 0 to c - 1 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.matchFilter(objFilter) then
      begin
        result := n;
        objFilter.Free;
        exit;
      end;
    end;
    objFilter.Free;
    exit; // not found;
  end
  else
  begin
    aFilter := copy(pattern, 1, p - 1);
    nPattern := copy(pattern, p + 1, length(pattern));
    objFilter := TJvXMLFilter.Create(aFilter);
    for i := 0 to c - 1 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.matchFilter(objFilter) then
      begin
        result := n.SelectSingleNode(npattern);
        if result <> nil then
        begin
          objFilter.Free;
          exit
        end;
      end;
    end;
    objFilter.Free;
  end;
end;

// filter contains name + any filters between []

function TJvXMLNode.matchFilter(objFilter: TJvXMLFilter): Boolean;
var
  i, j: integer;
  a: TJvXMLAttribute;
  n: TJvXMLNode;
  attName: string;
  atom: TJvXMLFilterAtom;
  attResult: Boolean;

  function evalAtom(aValue: string): Boolean;
  begin
    result := false;
    case atom.Operator of
      xfoNOP: result := true;
      xfoEQ: result := aValue = atom.Value;
      xfoIEQ: result := comparetext(aValue, atom.value) = 0;
      xfoNE: result := avalue <> atom.value;
      xfoINE: result := comparetext(aValue, atom.value) <> 0;
      xfoGT:
        try
          result := Strtofloat(avalue) > strtofloat(atom.value);
        except
        end;
      xfoIGT: result := comparetext(aValue, atom.value) > 0;
      xfoLT:
        try
          result := Strtofloat(avalue) < strtofloat(atom.value);
        except
        end;
      xfoILT: result := comparetext(aValue, atom.value) < 0;
      xfoGE:
        try
          result := Strtofloat(avalue) >= strtofloat(atom.value);
        except
        end;
      xfoIGE: result := comparetext(aValue, atom.value) >= 0;
      xfoLE:
        try
          result := Strtofloat(avalue) <= strtofloat(atom.value);
        except
        end;
      xfoILE: result := comparetext(aValue, atom.value) <= 0;
    end;

  end;
begin
  Result := false;
  AttResult := false;
  if objFilter.Filters.Count = 0 then
  begin // just filter on name
    result := objFilter.Name = name;
    exit;
  end;
  for i := 0 to objFilter.Filters.count - 1 do
  begin
    atom := TJvXMLFilterAtom(objFilter.Filters[i]);
    if atom.AttributeFilter then
    begin
      attName := atom.Name;
      if attName = '*' then
      begin // match any attribute
        if Attributes.Count = 0 then exit;
        for j := 0 to Attributes.count - 1 do
        begin
          a := TJvXMLAttribute(Attributes[j]);
          attResult := evalAtom(a.value);
          if AttResult then break;
        end;
        if not AttResult then exit;
      end
      else
      begin
        a := GetNamedAttribute(attName);
        if a = nil then exit;
        if not evalAtom(a.value) then exit;
      end;
    end
    else
    begin
      attName := atom.Name;
      n := GetNamedNode(attName);
      if n = nil then exit;
      if not evalAtom(n.value) then exit;
    end;
  end;
  result := true;
end;

procedure TJvXMLNode.SelectNodes(pattern: string; aList: TList);
var
  npattern: string;
  p, i, c: integer;
  n: TJvXMLNode;
  aFilter: string;
  objFilter: TJvXMLFilter;
  recurse: Boolean;
begin
  c := nodes.count;
  if c = 0 then exit;
  if copy(pattern, 1, 2) = '//' then
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
    aFilter := copy(pattern, 1, p - 1);
    if copy(pattern, p, 2) = '//' then
      npattern := copy(pattern, p, length(pattern))
    else
      npattern := copy(pattern, p + 1, length(pattern));
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
  result := stylesheet.process(0, self);
end;

// used in conjunction with the transformNode function.
// basically works like the document function except for nodes with processing instructions

function TJvXMLNode.process(aLevel: integer; node: TJvXMLNode): string;
var
  i: integer;
  spc: string;

  function ExpandCDATA(aValue: string): string;
  begin
    result := stringreplace(aValue, '\n ', sLineBreak, [rfreplaceall]);
    result := stringreplace(result, '\t ', tab, [rfreplaceall]);
  end;
begin
  if parentNode = nil then
  begin
    if nodes.count <> 0 then
      for i := 0 to nodes.count - 1 do
        result := result + TJvXMLNode(nodes[i]).process(aLevel + 1, node);
    exit;
  end;
  if aLevel > 0 then
    spc := StringOfChar(' ', aLevel * 2)
  else
    spc := '';
  result := spc + '<' + Name;
  if Attributes.Count > 0 then
    for i := 0 to Attributes.count - 1 do
      result := result + TJvXMLAttribute(Attributes[i]).document;
  if (nodes.count = 0) and (value = '') then
  begin
    result := result + ' />' + sLineBreak;
    exit;
  end
  else
    result := result + '>' + sLineBreak;
  if Value <> '' then
  begin
    if ValueType = xvtString then
      result := result + spc + '  ' + Value + sLineBreak
    else if ValueType = xvtCDATA then
    begin
      result := result + spc + '  ' + '<![CDATA[' + ExpandCDATA(value) + ']]>' + sLineBreak;
    end
  end;
  if nodes.count <> 0 then
    for i := 0 to nodes.count - 1 do
      result := result + TJvXMLNode(nodes[i]).process(aLevel + 1, node);
  result := result + spc + '</' + Name + '>' + sLineBreak;
end;

function TJvXMLNode.getNameSpace: string;
var
  p: integer;
begin
  p := pos(':', FName);
  if p > 0 then
    result := copy(FName, 1, p - 1)
  else
    result := '';
end;

// find the node with a path like customers/regional/jansoft

function TJvXMLNode.getNamePathNode(aPath: string): TJvXMLNode;
var
  aName, newpath, sindex: string;
  c, i, p, index, indexc: integer;
  n: TJvXMLNode;
begin
  result := nil;
  c := nodes.Count;
  if c = 0 then exit;
  if apath = '' then
  begin
    result := self;
    exit;
  end;
  p := posstr('/', apath, 1);
  if p = 0 then
  begin
    aName := apath;
    newpath := '';
  end
  else
  begin
    aName := copy(apath, 1, p - 1);
    newPath := copy(apath, p + 1, length(apath));
  end;
  // now check for any index []
  p := posstr('[', aname, 1);
  index := 0; // search first by default
  indexc := 0;
  if p > 0 then
  begin
    sindex := copy(aName, p + 1, length(aName) - p - 1);
    aName := copy(aName, 1, p - 1);
    if sindex = 'end' then
      index := -1
    else
    try
      index := strtoint(sindex);
      if index >= c then exit;
    except
      exit;
    end
  end;
  if index = -1 then // search end from end
    for i := c - 1 downto 0 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.Name = aname then
      begin
        if newpath = '' then
        begin
          result := n;
          exit;
        end
        else
        begin
          result := n.getNamePathNode(newPath);
          exit;
        end;
      end;
    end
  else // search from beginning indexed
    for i := 0 to c - 1 do
    begin
      n := TJvXMLNode(nodes[i]);
      if n.Name = aname then
      begin
        if index = indexc then
        begin
          if newpath = '' then
          begin
            result := n;
            exit;
          end
          else
          begin
            result := n.getNamePathNode(newPath);
            exit;
          end;
        end
        else
          inc(indexc);
      end;
    end;
end;

function TJvXMLNode.ForceNamePathNode(aPath: string): TJvXMLNode;
var
  aName, newpath: string;
  c, i, p: integer;
  n: TJvXMLNode;
  doappend: Boolean;
begin
  //  result:=nil;
  p := posstr('/', apath, 1);
  if p = 0 then
  begin
    aName := apath;
    newpath := '';
  end
  else
  begin
    aName := copy(apath, 1, p - 1);
    newPath := copy(apath, p + 1, length(apath));
  end;
  p := posstr('+', aName, 1);
  if p > 0 then delete(aName, p, 1);
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
          result := n;
          exit;
        end
        else
        begin
          result := n.ForceNamePathNode(newPath);
          exit;
        end;
      end;
    end;
  end;
  // we dont have it , so force it;
  n := TJvXMLNode.Create(aName, '', self);
  nodes.Add(n);
  if newpath = '' then
    result := n
  else
    result := n.ForceNamePathNode(newpath);
end;

function TJvXMLNode.ForceNamePathNodeAttribute(aPath, aName: string;
  aValue: Variant): TJvXMLAttribute;
var
  n: TJvXMLNode;
  a: TJvXMLAttribute;
begin
  result := nil;
  n := ForceNamePathNode(aPath);
  if n = nil then exit;
  a := n.GetNamedAttribute(aName);
  if a <> nil then
  begin
    a.Value := aValue;
    result := a;
  end
  else
  begin
    result := n.addAttribute(aname, avalue);
  end;
end;

function TJvXMLNode.GetNamePathNodeAttribute(aPath,
  aName: string): TJvXMLAttribute;
var
  n: TJvXMLNode;
begin
  result := nil;
  n := GetNamePathNode(aPath);
  if n = nil then exit;
  result := n.GetNamedAttribute(aName);
end;

procedure TJvXMLNode.deleteNamePathNode(aPath: string);
var
  n, pn: TJvXMLNode;
  i: integer;
begin
  if apath = '' then exit;
  n := getNamePathNode(aPath);
  if n = nil then exit;
  pn := n.parentNode;
  for i := 0 to pn.nodes.count - 1 do
  begin
    if TJvXMLNode(pn.nodes[i]) = n then
    begin
      pn.DeleteNode(i);
      exit;
    end;
  end;
end;

procedure TJvXMLNode.deleteNamePathNodeAttribute(aPath, aName: string);
var
  a: TJvXMLAttribute;
  pn: TJvXMLNode;
  i: integer;
begin
  a := GetNamePathNodeAttribute(aPath, aName);
  if a = nil then exit;
  pn := a.parent;
  for i := 0 to pn.attributes.count - 1 do
  begin
    if TJvXMLAttribute(pn.attributes[i]) = a then
    begin
      pn.DeleteAttribute(i);
      exit;
    end;
  end;
end;

function TJvXMLNode.GetAttributeValue(aName: string): Variant;
var
  i, c: integer;
  a: TJvXMLAttribute;
begin
  result := Null;
  c := attributes.count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
  begin
    a := TJvXMLAttribute(attributes[i]);
    if a.Name = aName then
    begin
      result := a.Value;
      exit;
    end;
  end;
end;

{ TJvXMLTree }

constructor TJvXMLTree.Create(aName: string; aValue: Variant; aParent: TJvXMLNode);
begin
  inherited Create(aName, aValue, aParent);
  FLines := TStringList.Create;
end;

destructor TJvXMLTree.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

function TJvXMLTree.asText: string;
var
  i, c: integer;
begin
  c := Nodes.Count;
  if c = 0 then exit;
  result := '<' + Name;
  if Attributes.Count > 0 then
    for i := 0 to Attributes.count - 1 do
      result := result + TJvXMLAttribute(Attributes[i]).document;
  result := result + '>' + sLineBreak;
  for i := 0 to c - 1 do
    result := result + TJvXMLNode(nodes[i]).document(1);
  result := result + '</' + Name + '>' + sLineBreak;
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
  i, c: integer;
begin
  c := Nodes.Count;
  if c = 0 then exit;
  //  result:='<'+Name;
  //  if Attributes.Count>0 then
  //  for i:=0 to Attributes.count-1 do
  //    result:=result+TJvXMLAttribute(Attributes[i]).document;
  //  result:=result+'>'+sLineBreak;
  result := '';
  for i := 0 to c - 1 do
    result := result + TJvXMLNode(nodes[i]).document(0);
  //  result:=result+'</'+Name+'>'+sLineBreak;
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

constructor TJvXMLAttribute.Create(aParent: TJvXMLNode; aName: string; aValue: Variant);
begin
  FName := aName;
  FValue := aValue;
  FParent := aParent;
end;

function TJvXMLAttribute.document: string;
var
  s: string;
begin
  s := Value;
  result := ' ' + Name + '="' + s + '"';
end;

procedure TJvXMLAttribute.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TJvXMLAttribute.Setparent(const Value: TJvXMLNode);
begin
  Fparent := Value;
end;

procedure TJvXMLAttribute.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TJvXMLTree }

procedure TJvXMLTree.ParseXML;
var
  i, c: integer;
  s, token, aName: string;
  n: TJvXMLNode;
begin
  i := 0;
  FNodeCount := 0;
  ClearNodes;
  ClearAttributes;
  Name := 'root';
  n := self;
  c := Lines.Count - 1;
  repeat
    s := Lines[i];
    token := copy(s, 1, 3);
    aName := copy(s, 4, length(s));
    if token = 'OT:' then
    begin
      n := n.AddNodeEx(aName, '');
      inc(FNodeCount);
    end
    else if token = 'CT:' then
    begin
      n := n.ParentNode;
    end
    else if token = 'ET:' then
    begin
      n.AddNodeEx(aName, '');
    end
    else if token = 'TX:' then
    begin
      n.Value := aName;
      n.ValueType := xvtString;
    end
    else if token = 'CD:' then
    begin
      n.value := aName;
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
  p1, p2: integer;
  attName, attValue: string;
  attOperator: TJvXMLFilterOperator;
  atom: TJvXMLFilterAtom;
  //    a:TJvXMLAttribute;
  //    n:TJvXMLNode;

  function trimquotes(s: string): string;
  var
    cc: integer;
  begin
    result := trim(s);
    if s = '' then exit;
    if (s[1] = '"') or (s[1] = '''') then delete(result, 1, 1);
    if s = '' then exit;
    cc := length(result);
    if (result[cc] = '"') or (result[cc] = '''') then delete(result, cc, 1);
  end;

  function splitNameValue(s: string): Boolean;
  var
    pp: integer;
  begin
    //      result:=false;
    pp := posStr(' $ne$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoNE;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 6, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $ine$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoINE;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 7, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $ge$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoGE;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 6, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $ige$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoIGE;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 7, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $gt$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoGT;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 6, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $igt$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoIGT;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 7, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $le$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoLE;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 6, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $ile$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoILE;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 7, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $lt$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoLT;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 6, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $ilt$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoILT;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 7, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $eq$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoEQ;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 6, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' $ieq$ ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoIEQ;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 7, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    pp := posStr(' = ', s, 1);
    if pp > 0 then
    begin
      attOperator := xfoEQ;
      attName := trim(copy(s, 1, pp - 1));
      attvalue := trimquotes(copy(s, pp + 3, length(s)));
      result := (attName <> '') and (attValue <> '');
      exit;
    end;
    attOperator := xfoNOP;
    attName := s;
    attValue := '';
    result := true;
    exit;
  end;

begin
  Filters := TList.Create;
  p1 := posStr('[', FilterStr, 1);
  if p1 = 0 then
  begin // just a name filter on name
    name := FilterStr;
    exit;
  end
  else
  begin
    Name := copy(FilterStr, 1, p1 - 1);
    delete(FilterStr, 1, p1 - 1);
  end;
  repeat
    FilterStr := trim(FilterStr);
    p1 := posStr('[', FilterStr, 1);
    if p1 = 0 then exit;
    p2 := posStr(']', FilterStr, p1 + 1);
    if p2 = 0 then exit;
    theFilter := copy(FilterStr, p1 + 1, p2 - p1 - 1);
    delete(FilterStr, 1, p2);
    if theFilter = '' then exit;
    // check for attribute filter
    if theFilter[1] = '@' then
    begin
      if not splitNameValue(copy(theFilter, 2, length(theFilter))) then exit;
      atom := TJvXMLFilterAtom.Create;
      atom.Name := attName;
      atom.Operator := attOperator;
      atom.Value := attValue;
      atom.AttributeFilter := true;
      Filters.Add(atom);
    end
    else
    begin // childfilter
      if not splitNameValue(theFilter) then exit;
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
  i: integer;
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



