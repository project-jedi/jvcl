{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppXmlStore.pas, released on 2003-12-06.

The Initial Developer of the Original Code is Olivier Sannier
Portions created by Marcel Bestebroer are Copyright (C) 2003 Olivier Sannier
All Rights Reserved.

Contributor(s): none to date

Last Modified: 2003-12-06

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAppXmlStore;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF LINUX}
  SysUtils, Classes, IniFiles,
  JvAppStore,
  JvSimpleXml;

type
  TJvAppXmlStore = class(TJvCustomAppStore)
  private
    FFileName : TFilename;
    function GetFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
    function GetRootNodeName: string;
  protected
    FXml: TJvSimpleXml;
    procedure SetRootNodeName(const Value: string);
    function ValueExists(const NodeName, Attribute : string): Boolean; virtual;
    function ReadValue(const NodeName, Attribute : string): string; virtual;
    procedure WriteValue(const NodeName, Attribute , Value: string); virtual;
    procedure RemoveValue(const NodeName, Attribute : string); virtual;
    procedure EnumFolders(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;
    procedure EnumValues(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); override;
    function IsFolderInt(Path: string; ListIsValue: Boolean = True): Boolean; override;
    procedure SplitKeyPath(const Path: string; out Key, ValueName: string); override;
    function ValueStoredInt(const Path: string): Boolean; override;
    procedure DeleteValueInt(const Path: string); override;
    procedure DeleteSubTreeInt(const Path: string); override;
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

    property Xml : TJvSimpleXml read FXml;
  published
    property FileName: TFileName read GetFileName write SetFileName;
    property RootNodeName : string read GetRootNodeName write SetRootNodeName;
  end;

implementation

uses
  JvTypes, JvConsts, TypInfo;

const
  cNullDigit = '0';
  cCount = 'Count';

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

{ TJvAppXmlStore }

procedure TJvAppXmlStore.SetRootNodeName(const Value: string);
begin
  if Value = '' then
    raise EPropertyError.Create(SNodeCannotBeEmpty)
  else
  begin
    StringReplace(Value, ' ', '_', [rfReplaceAll]);
    FXml.Root.Name := Value;
  end;
end;

procedure TJvAppXmlStore.SplitKeyPath(const Path: string; out Key, ValueName: string);
begin
  inherited SplitKeyPath(Path, Key, ValueName);
  if Key = '' then
    Key := Path;
end;

function TJvAppXmlStore.ValueStoredInt(const Path: string): Boolean;
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  Result := ValueExists(Section, Key);
end;

procedure TJvAppXmlStore.DeleteValueInt(const Path: string);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  RemoveValue(Section, Key);
end;

procedure TJvAppXmlStore.DeleteSubTreeInt(const Path: string);
var
  TopSection: string;
begin
  TopSection := GetAbsPath(Path);
  if TopSection = '' then
    TopSection := Path;
  raise EJVCLAppStoreError.Create(SDelSubTreeNotImplemented);
end;

function TJvAppXmlStore.DoReadInteger(const Path: string; Default: Integer): Integer;
var
  Section: string;
  Key: string;
  Value: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
  begin
    Value := ReadValue(Section, Key);
    if Value = '' then
      Value := cNullDigit;
    Result := StrToInt(Value);
  end
  else
    Result := Default;
end;

procedure TJvAppXmlStore.DoWriteInteger(const Path: string; Value: Integer);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, IntToStr(Value));
end;

function TJvAppXmlStore.DoReadFloat(const Path: string; Default: Extended): Extended;
var
  Section: string;
  Key: string;
  Value: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
  begin
    Value := ReadValue(Section, Key);
    if Value = '' then
      Value := cNullDigit;
    Result := StrToFloat(Value);
  end
  else
    Result := Default;
end;

procedure TJvAppXmlStore.DoWriteFloat(const Path: string; Value: Extended);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, FloatToStr(Value));
end;

function TJvAppXmlStore.DoReadString(const Path: string; Default: string): string;
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
    Result := ReadValue(Section, Key)
  else
    Result := Default;
end;

procedure TJvAppXmlStore.DoWriteString(const Path: string; Value: string);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, Value);
end;

function TJvAppXmlStore.DoReadBinary(const Path: string; var Buf; BufSize: Integer): Integer;
var
  Section: string;
  Key: string;
  Value: string;
begin
  SplitKeyPath(Path, Section, Key);
  if ValueExists(Section, Key) then
  begin
    Value := ReadValue(Section, Key);
    Result := BinStrToBuf(Value, Buf, BufSize);
  end
  else
    Result := 0;
end;

procedure TJvAppXmlStore.DoWriteBinary(const Path: string; const Buf; BufSize: Integer);
var
  Section: string;
  Key: string;
begin
  SplitKeyPath(Path, Section, Key);
  WriteValue(Section, Key, BufToBinStr(Buf, BufSize));
end;

function TJvAppXmlStore.ReadValue(const NodeName,
  Attribute: string): string;
begin
  Result := FXml.Root.Items.ItemNamed[NodeName].Items.ItemNamed[Attribute].Value;
end;

procedure TJvAppXmlStore.RemoveValue(const NodeName, Attribute: string);
begin
  FXml.Root.Items.ItemNamed[NodeName].Items.Delete(Attribute);
end;

function TJvAppXmlStore.ValueExists(const NodeName,
  Attribute: string): Boolean;
var
  Node : TJvSimpleXmlElem;
begin
  Node := FXml.Root.Items.ItemNamed[NodeName];
  Result := Assigned(Node) and
            Assigned(Node.Items.ItemNamed[Attribute]);
end;

procedure TJvAppXmlStore.WriteValue(const NodeName, Attribute,
  Value: string);
var
  Node : TJvSimpleXmlElem;
  Attr : TJvSimpleXmlElem;
begin
  Node := FXml.Root.Items.ItemNamed[NodeName];
  if not Assigned(Node) then
    Node := FXml.Root.Items.Add(NodeName);

  Attr := Node.Items.ItemNamed[Attribute];
  if not Assigned(Attr) then
    Node.Items.Add(Attribute,Value)
  else
    Attr.Value := Value;
end;

constructor TJvAppXmlStore.Create(AOwner: TComponent);
begin
  inherited;
  FXml := TJvSimpleXml.Create(nil);
  RootNodeName := 'Configuration';
end;

destructor TJvAppXmlStore.Destroy;
begin
  Flush;
  FXml.Free;
  inherited;
end;

procedure TJvAppXmlStore.Flush;
begin
  FXml.SaveToFile(FFileName);
end;

function TJvAppXmlStore.GetFileName: TFileName;
begin
  Result := FFileName;
end;

procedure TJvAppXmlStore.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  if FileExists(FFileName) then
    FXml.LoadFromFile(FFileName);
end;

procedure TJvAppXmlStore.EnumFolders(const Path: string;
  const Strings: TStrings; const ReportListAsValue: Boolean);
var
  RefPath: string;
  I: Integer;
begin
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := 'EmptyPath';

  Strings.Clear;
  for I := 0 to FXml.Root.Items.Count - 1 do
  begin
    Strings.Add(FXml.Root.Items[I].Name);
  end;

  I := Strings.Count - 1;
  while I >= 0 do
  begin
    if (RefPath <> '') and ((Copy(Strings[I], 1, Length(RefPath) + 1) <> RefPath + '\') or
      (Pos('\', Copy(Strings[I], 2 + Length(RefPath), Length(Strings[I]) - Length(RefPath))) > 0)) then
      Strings.Delete(I)
    else
    if ReportListAsValue and ValueExists(Strings[I], cCount) then
      Strings.Delete(I)
    else
    if RefPath <> '' then
      Strings[I] := Copy(Strings[I], 1 + Length(RefPath), Length(Strings[I]) - Length(RefPath));
    Dec(I);
  end;
end;

procedure TJvAppXmlStore.EnumValues(const Path: string;
  const Strings: TStrings; const ReportListAsValue: Boolean);
var
  PathIsList: Boolean;
  RefPath: string;
  I: Integer;
begin
  PathIsList := ReportListAsValue and ListStored(Path);
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := 'EmptyPath';

  Strings.Clear;
  for I := 0 to FXml.Root.Items.ItemNamed[RefPath].Items.Count - 1 do
  begin
    Strings.Add(FXml.Root.Items.ItemNamed[RefPath].Items[I].Name);
  end;

  for I := Strings.Count - 1 downto 0 do
  begin
    if PathIsList and (AnsiSameText(cCount, Strings[I]) or NameIsListItem(Strings[I])) then
      Strings.Delete(I);
  end;
  if PathIsList then
    Strings.Add('');
end;

function TJvAppXmlStore.IsFolderInt(Path: string;
  ListIsValue: Boolean): Boolean;
var
  RefPath: string;
  ValueNames: TStrings;
  I: Integer;
begin
  RefPath := GetAbsPath(Path);
  if RefPath = '' then
    RefPath := 'EmptyPath';
  Result := Assigned(FXml.Root.Items.ItemNamed[RefPath]);
  if Result and ListIsValue and ValueExists(RefPath, cCount) then
  begin
    Result := False;
    ValueNames := TStringList.Create;
    try
      EnumValues(Path, ValueNames, True);
      I := ValueNames.Count - 1;
      while Result and (I >= 0) do
      begin
        Result := not AnsiSameText(ValueNames[I], cCount) and not NameIsListItem(ValueNames[I]);
        Dec(I);
      end;
    finally
      ValueNames.Free;
    end;
  end;
end;

function TJvAppXmlStore.GetRootNodeName: string;
begin
  Result := FXml.Root.Name;
end;

end.
