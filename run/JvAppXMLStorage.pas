{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppXMLStorage.pas, released on 2003-12-06.

The Initial Developer of the Original Code is Olivier Sannier
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier
All Rights Reserved.

Contributor(s): none to date

Last Modified: 2004-01-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvAppXMLStorage;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF LINUX}
  SysUtils, Classes, IniFiles,
  JvAppStorage, JvSimpleXml;

type
  TJvAppXMLStorage = class(TJvCustomAppStorage)
  protected
    FFileName: TFileName;
    FXml: TJvSimpleXml;
    FCurrentNode: TJvSimpleXmlElem;
    function GetFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
    function GetRootNodeName: string;
    procedure SetRootNodeName(const Value: string);
    // Returns the last node in path, if it exists.
    // Returns nil in all other cases
    // If StartNode is nil, then FCurrentNode is used as a
    // starting point for Path
    function GetNodeFromPath(Path: string; StartNode: TJvSimpleXmlElem = nil): TJvSimpleXmlElem;
    // Reads the \ separated Key string and sets FCurrentNode to
    // the last one, having created all the required XML nodes
    // including the last one
    procedure CreateAndSetNode(Key: string);
    procedure EnumFolders(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;
    procedure EnumValues(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;
    function IsFolderInt(Path: string; ListIsValue: Boolean = True): Boolean; override;
    procedure SplitKeyPath(const Path: string; out Key, ValueName: string); override;
    function PathExistsInt(const Path: string): boolean; override;
    function ValueStoredInt(const Path: string): Boolean; override;
    procedure DeleteValueInt(const Path: string); override;
    procedure DeleteSubTreeInt(const Path: string); override;
    function DoReadBoolean(const Path: string; Default: Boolean): Boolean; override;
    procedure DoWriteBoolean(const Path: string; Value: Boolean); override;
    function DoReadInteger(const Path: string; Default: Integer): Integer; override;
    procedure DoWriteInteger(const Path: string; Value: Integer); override;
    function DoReadFloat(const Path: string; Default: Extended): Extended; override;
    procedure DoWriteFloat(const Path: string; Value: Extended); override;
    function DoReadString(const Path: string; Default: string): string; override;
    procedure DoWriteString(const Path: string; Value: string); override;
    function DoReadBinary(const Path: string; var Buf; BufSize: Integer): Integer; override;
    procedure DoWriteBinary(const Path: string; const Buf; BufSize: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Flush;
    procedure Reload;
    property Xml: TJvSimpleXml read FXml;
  published
    property FileName: TFileName read GetFileName write SetFileName;
    property RootNodeName: string read GetRootNodeName write SetRootNodeName;
  end;

implementation

uses
  TypInfo,
  JclStrings,
  JvTypes, JvConsts, JvResources;

const
  cNullDigit = '0';
  cCount = 'Count';
  cEmptyPath = 'EmptyPath';

function BinStrToBuf(Value: string; var Buf; BufSize: Integer): Integer;
var
  P: PChar;
begin
  if Odd(Length(Value)) then
    Value := cNullDigit + Value;
  if (Length(Value) div 2) < BufSize then
    BufSize := Length(Value) div 2;
  Result := 0;
  P := PChar(Value);
  while (BufSize > 0) do
  begin
    PChar(Buf)[Result] := Chr(StrToInt('$' + P[0] + P[1]));
    Inc(Result);
    Dec(BufSize);
    Inc(P, 2);
  end;
end;

function BufToBinStr(const Buf; BufSize: Integer): string;
var
  P: PChar;
  S: string;
begin
  SetLength(Result, BufSize * 2);
  P := PChar(Result);
  Inc(P, (BufSize - 1) * 2); // Point to end of string ^
  while BufSize > 0 do
  begin
    S := IntToHex(Ord(PChar(Buf)[BufSize]), 2);
    P[0] := S[1];
    P[1] := S[2];
    Dec(P, 2);
    Dec(BufSize);
  end;
end;

//=== TJvAppXMLStorage =======================================================

constructor TJvAppXMLStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXml := TJvSimpleXml.Create(nil);
  RootNodeName := 'Configuration';
  FCurrentNode := FXml.Root;
end;

destructor TJvAppXMLStorage.Destroy;
begin
  Flush;
  FXml.Free;
  inherited Destroy;
end;

procedure TJvAppXMLStorage.SetRootNodeName(const Value: string);
begin
  if Value = '' then
    raise EPropertyError.Create(RsENodeCannotBeEmpty)
  else
  begin
    StringReplace(Value, ' ', '_', [rfReplaceAll]);
    FXml.Root.Name := Value;
  end;
end;

procedure TJvAppXMLStorage.SplitKeyPath(const Path: string; out Key, ValueName: string);
begin
  inherited SplitKeyPath(Path, Key, ValueName);
  if Key = '' then
    Key := Path;
end;

function TJvAppXMLStorage.ValueStoredInt(const Path: string): Boolean;
var
  Section: string;
  Key: string;
  Node: TJvSimpleXmlElem;
begin
  SplitKeyPath(Path, Section, Key);
  Result := False;
  Node := GetNodeFromPath(Section);
  if Assigned(Node) then
    Result := Assigned(Node.Items.ItemNamed[Key]);
end;

procedure TJvAppXMLStorage.DeleteValueInt(const Path: string);
var
  Node: TJvSimpleXmlElem;
  Section: string;
  Key: string;
begin
  if ValueStored(Path) then
  begin
    SplitKeyPath(Path, Section, Key);
    Node := GetNodeFromPath(Section);
    if Assigned(Node) then
      Node.Items.Delete(Key);
  end;
end;

procedure TJvAppXMLStorage.DeleteSubTreeInt(const Path: string);
var
  TopNode: string;
  Node: TJvSimpleXmlElem;
  Parent: TJvSimpleXmlElem;
  Name: string;
begin
  TopNode := GetAbsPath(Path);
  if TopNode = '' then
    TopNode := Path;
  Node := GetNodeFromPath(TopNode);
  if Assigned(Node) then
  begin
    Name := Node.Name;
    Parent := Node.Parent;
    if Assigned(Parent) then
      Parent.Items.Delete(Name);
  end;
end;

function TJvAppXMLStorage.DoReadInteger(const Path: string; Default: Integer): Integer;
var
  ParentPath: string;
  ValueName: string;
  Node: TJvSimpleXmlElem;
begin
  SplitKeyPath(Path, ParentPath, ValueName);

  Node := GetNodeFromPath(ParentPath);

  if Assigned(Node) and Assigned(Node.Items.ItemNamed[ValueName]) then
  begin
    try
      Result := Node.Items.ItemNamed[ValueName].IntValue;
    except
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
    end;
  end
  else
  if StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
    raise EJVCLException.CreateFmt(RsEPathDoesntExists, [Path]);
end;

procedure TJvAppXMLStorage.DoWriteInteger(const Path: string; Value: Integer);
var
  ParentPath: string;
  ValueName: string;
begin
  SplitKeyPath(Path, ParentPath, ValueName);
  CreateAndSetNode(ParentPath);
  FXml.Options := [sxoAutoCreate, sxoAutoIndent];
  FCurrentNode.Items.ItemNamed[ValueName].IntValue := Value;
  FXml.Options := [sxoAutoIndent];
end;

function TJvAppXMLStorage.DoReadFloat(const Path: string; Default: Extended): Extended;
var
  ParentPath: string;
  ValueName: string;
  StrValue: string;
  Node: TJvSimpleXmlElem;
begin
  SplitKeyPath(Path, ParentPath, ValueName);

  Node := GetNodeFromPath(ParentPath);

  if Assigned(Node) and Assigned(Node.Items.ItemNamed[ValueName]) then
  begin
    try
      StrValue := Node.Items.ItemNamed[ValueName].Value;
      Result := StrToFloat(StrValue);
    except
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
    end;
  end
  else
  if StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
    raise EJVCLException.CreateFmt(RsEPathDoesntExists, [Path]);
end;

procedure TJvAppXMLStorage.DoWriteFloat(const Path: string; Value: Extended);
var
  ParentPath: string;
  ValueName: string;
begin
  SplitKeyPath(Path, ParentPath, ValueName);
  CreateAndSetNode(ParentPath);
  FXml.Options := [sxoAutoCreate, sxoAutoIndent];
  FCurrentNode.Items.ItemNamed[ValueName].Value := FloatToStr(Value);
  FXml.Options := [sxoAutoIndent];
end;

function TJvAppXMLStorage.DoReadString(const Path: string; Default: string): string;
var
  ParentPath: string;
  ValueName: string;
  Node: TJvSimpleXmlElem;
begin
  SplitKeyPath(Path, ParentPath, ValueName);

  Node := GetNodeFromPath(ParentPath);

  if Assigned(Node) and Assigned(Node.Items.ItemNamed[ValueName]) then
  begin
    try
      Result := Node.Items.ItemNamed[ValueName].Value;
    except
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
    end;
  end
  else
  if StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
    raise EJVCLException.CreateFmt(RsEPathDoesntExists, [Path]);
end;

procedure TJvAppXMLStorage.DoWriteString(const Path: string; Value: string);
var
  ParentPath: string;
  ValueName: string;
begin
  SplitKeyPath(Path, ParentPath, ValueName);
  CreateAndSetNode(ParentPath);
  FXml.Options := [sxoAutoCreate, sxoAutoIndent];
  FCurrentNode.Items.ItemNamed[ValueName].Value := Value;
  FXml.Options := [sxoAutoIndent];
end;

function TJvAppXMLStorage.DoReadBinary(const Path: string; var Buf; BufSize: Integer): Integer;
var
  Value: string;
begin
  Value := DoReadString(Path, '');
  Result := BinStrToBuf(Value, Buf, BufSize);
end;

procedure TJvAppXMLStorage.DoWriteBinary(const Path: string; const Buf; BufSize: Integer);
begin
  DoWriteString(Path, BufToBinStr(Buf, BufSize));
end;

procedure TJvAppXMLStorage.Flush;
begin
  if FFileName <> '' then
    FXml.SaveToFile(FFileName);
end;

function TJvAppXMLStorage.GetFileName: TFileName;
begin
  Result := FFileName;
end;

procedure TJvAppXMLStorage.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  Reload;
end;

procedure TJvAppXMLStorage.EnumFolders(const Path: string;
  const Strings: TStrings; const ReportListAsValue: Boolean);
var
  RefPath: string;
  I: Integer;
  Node: TJvSimpleXmlElem;
begin
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := cEmptyPath;

  Node := GetNodeFromPath(RefPath, FXml.Root);

  if Node <> nil then
  begin
    Strings.Clear;
    for I := 0 to Node.Items.Count - 1 do
      Strings.Add(Node.Items[I].Name);
  end
  else
    raise EJVCLException.CreateFmt(RsEPathDoesntExists, [RefPath]);
end;

procedure TJvAppXMLStorage.EnumValues(const Path: string;
  const Strings: TStrings; const ReportListAsValue: Boolean);
var
  PathIsList: Boolean;
  RefPath: string;
  I: Integer;
  Node: TJvSimpleXmlElem;
  Name: string;
begin
  PathIsList := ReportListAsValue and ListStored(Path);
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := cEmptyPath;

  Node := GetNodeFromPath(RefPath, FXml.Root);

  if Node <> nil then
  begin
    Strings.Clear;
    for I := 0 to Node.Items.Count - 1 do
    begin
      Name := Node.Items[I].Name;
      if (not PathIsList or (not AnsiSameText(cCount, Name) and
        not NameIsListItem(Name))) then
        Strings.Add(Name);
    end;
  end
  else
    raise EJVCLException.CreateFmt(RsEPathDoesntExists, [RefPath]);
end;

function TJvAppXMLStorage.IsFolderInt(Path: string;
  ListIsValue: Boolean): Boolean;
var
  RefPath: string;
  ValueNames: TStrings;
  I: Integer;
  Node: TJvSimpleXmlElem;
  Name: string;
begin
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := cEmptyPath;

  Node := GetNodeFromPath(RefPath);
  Result := False;
  if Assigned(Node) and ListIsValue and
    Assigned(Node.Items.ItemNamed[cCount]) then
  begin
    ValueNames := TStringList.Create;
    try
      I := 0;
      repeat
        Name := Node.Items[I].Name;
        Result := not AnsiSameText(cCount, Name) and
          not NameIsListItem(Name);
        Inc(I);
      until (I = Node.Items.Count) or Result;
    finally
      ValueNames.Free;
    end;
  end;
end;

function TJvAppXMLStorage.GetRootNodeName: string;
begin
  Result := FXml.Root.Name;
end;

procedure TJvAppXMLStorage.Reload;
begin
  if FileExists(FFileName) then
    FXml.LoadFromFile(FFileName);
end;

procedure TJvAppXMLStorage.CreateAndSetNode(Key: string);
begin
  FXml.Options := [sxoAutoCreate, sxoAutoIndent];
  FCurrentNode := GetNodeFromPath(Key, FXml.Root);
  FXml.Options := [sxoAutoIndent];
end;

function TJvAppXMLStorage.GetNodeFromPath(Path: string; StartNode: TJvSimpleXmlElem = nil): TJvSimpleXmlElem;
var
  NodeList: TStringList;
  I: Integer;
  Node: TJvSimpleXmlElem;
begin
  NodeList := TStringList.Create;
  if StartNode <> nil then
    Node := StartNode
  else
    Node := FCurrentNode;

  try
    try
      StrToStrings(Path, '\', NodeList, false);
      for I := 0 to NodeList.Count - 1 do
      begin
        if Assigned(Node.Items.ItemNamed[NodeList[i]]) then
          Node := Node.Items.ItemNamed[NodeList[i]]
        else
        begin
          Result := nil;
          Exit;
        end;
      end;
    finally
      NodeList.Free;
    end;
  except
    Node := nil;
  end;
  Result := Node;
end;

function TJvAppXMLStorage.PathExistsInt(const Path: string): boolean;
var
  SubKey: string;
  ValueName: string;
  Node: TJvSimpleXmlElem;
begin
  SplitKeyPath(Path, SubKey, ValueName);
  Result := False;
  Node := GetNodeFromPath(Path, FCurrentNode);
  if Assigned(Node) then
    Result := Assigned(Node.Items.ItemNamed[ValueName]);
end;

function TJvAppXMLStorage.DoReadBoolean(const Path: string;
  Default: Boolean): Boolean;
var
  ParentPath: string;
  ValueName: string;
  Node: TJvSimpleXmlElem;
begin
  SplitKeyPath(Path, ParentPath, ValueName);

  Node := GetNodeFromPath(ParentPath);

  if Assigned(Node) and Assigned(Node.Items.ItemNamed[ValueName]) then
  begin
    try
      Result := Node.Items.ItemNamed[ValueName].BoolValue;
    except
      if StorageOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
    end;
  end
  else
  if StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
    raise EJVCLException.CreateFmt(RsEPathDoesntExists, [Path]);
end;

procedure TJvAppXMLStorage.DoWriteBoolean(const Path: string;
  Value: Boolean);
var
  ParentPath: string;
  ValueName: string;
begin
  SplitKeyPath(Path, ParentPath, ValueName);
  CreateAndSetNode(ParentPath);
  FXml.Options := [sxoAutoCreate, sxoAutoIndent];
  FCurrentNode.Items.ItemNamed[ValueName].BoolValue := Value;
  FXml.Options := [sxoAutoIndent];
end;

end.

