{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PackageInformation.pas, released on 2004-05-17.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit PackageInformation;

interface

uses
  SysUtils, Classes, Contnrs,
  JvSimpleXml;

type
  TPackageXmlInfo = class;
  TRequiredPackage = class;
  TContainedFile = class;

  IPackageXmlInfoOwner = interface
    function GetBplNameOf(Package: TRequiredPackage): string;
  end;

  /// <summary>
  /// TRequiredPackage contains one package that is requried by a TPackageInfo
  /// object and it's inclusion conditions.
  /// </summary>
  TRequiredPackage = class(TObject)
  private
    FName: string;
    FTargets: TStrings;
    FCondition: string;
  public
    constructor Create(const AName, ATargets, ACondition: string);
    destructor Destroy; override;

    function IsRequiredByTarget(const TargetSymbol: string): Boolean;
    function GetBplName(Intf: IPackageXmlInfoOwner): string;

    property Name: string read FName;
    property Targets: TStrings read FTargets;
    property Condition: string read FCondition;
  end;

  /// <summary>
  /// TContainedFile contains one file name that is contained in the the
  /// TPackageInfo object and it's inclusion conditions.
  /// </summary>
  TContainedFile = class(TObject)
  private
    FName: string;
    FTargets: TStrings;
    FFormName: string;
    FCondition: string;
  public
    constructor Create(const AName, ATargets, AFormName, ACondition: string);
    destructor Destroy; override;

    function IsUsedByTarget(const TargetSymbol: string): Boolean;

    property Name: string read FName;
    property Targets: TStrings read FTargets;
    property FormName: string read FFormName;
    property Condition: string read FCondition;
  end;

    /// <summary>
  /// TPackageXmlInfo contains the generic .xml file for a bpl target.
  /// </summary>
  TPackageXmlInfo = class(TObject)
  private
    function GetContainCount: Integer;
    function GetContains(Index: Integer): TContainedFile;
    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): TRequiredPackage;
  protected
    FName: string;
    FDisplayName: string;
    FDescription: string;
    FRequires: TObjectList;
    FContains: TObjectList;
    FRequiresDB: Boolean;
    FIsDesign: Boolean;

    procedure LoadFromFile(const Filename: string);
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;

    property Name: string read FName; // "PackageName-"[R|D]
    property DisplayName: string read FDisplayName; // "PackageName"
    property Description: string read FDescription;
    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: TRequiredPackage read GetRequires;
    property ContainCount: Integer read GetContainCount;
    property Contains[Index: Integer]: TContainedFile read GetContains;
    property RequiresDB: Boolean read FRequiresDB;
    property IsDesign: Boolean read FIsDesign;
  end;

var
  BplNameToGenericNameHook: function(const BplName: string): string = nil;
  ExpandPackageTargets: procedure(Targets: TStrings) = nil;

function BplNameToGenericName(const BplName: string): string;
  { BplNameToGenericName converts a "JvCoreD7D.XXX" to "JvCore-D" }
function GetPackageXmlInfo(const BplName, XmlDir: string): TPackageXmlInfo;
  { returns a cached TPackageXmlInfo instance. }

implementation

{$IFDEF MSWINDOWS}
const
  PathDelim = '\';
{$ENDIF MSWINDOWS}

var
  XmlFileCache: TObjectList; // cache for .xml files ( TPackageXmlInfo )

function BplNameToGenericName(const BplName: string): string;
begin
  if Assigned(BplNameToGenericNameHook) then
    Result := BplNameToGenericNameHook(BplName)
  else
  begin
     // obtain package name used in the xml file
    Result := ChangeFileExt(BplName, '');
    Delete(Result, Length(Result) - 2, 2);
    if Length(Result) > 2 then
      Insert('-', Result, Length(Result) - 1); // do not localize
  end;
end;

/// <summary>
/// GetPackageXmlInfo returns a cached TPackageXmlInfo instance.
/// </summary>
function GetPackageXmlInfo(const BplName, XmlDir: string): TPackageXmlInfo;
var
  i: Integer;
  Name: string;
begin
  Name := BplNameToGenericName(BplName);
 // already in the cache
  for i := 0 to XmlFileCache.Count - 1 do
    if CompareText(TPackageXmlInfo(XmlFileCache[i]).Name, Name) = 0 then
    begin
      Result := TPackageXmlInfo(XmlFileCache[i]);
      Exit;
    end;

 // create a new one and add it to the cache
  Result := TPackageXmlInfo.Create(XmlDir + PathDelim + Name + '.xml'); // do not localize
  XmlFileCache.Add(Result);
end;

function EndsWith(const Text, EndText: string; CaseInsensitive: Boolean): Boolean;
var
  Len, i, x: Integer;
begin
  Result := False;
  Len := Length(EndText);
  x := Length(Text);
  if Len > x then
    Exit;
  if CaseInsensitive then
  begin
    for i := Len downto 1 do
      if UpCase(Text[x]) <> UpCase(EndText[i]) then
        Exit
      else
        Dec(x);
  end
  else
  begin
    for i := Len downto 1 do
      if Text[x] <> EndText[i] then
        Exit
      else
        Dec(x);
  end;
  Result := True;
end;

{ TRequiredPackage }

constructor TRequiredPackage.Create(const AName, ATargets, ACondition: string);
begin
  inherited Create;
  FName := AName;
  FTargets := TStringList.Create;
  TStringList(FTargets).Duplicates := dupIgnore;
  FTargets.CommaText := ATargets;
  if Assigned(ExpandPackageTargets) then
    ExpandPackageTargets(FTargets);
  FCondition := ACondition;
end;

destructor TRequiredPackage.Destroy;
begin
  FTargets.Free;
  inherited Destroy;
end;

function TRequiredPackage.GetBplName(Intf: IPackageXmlInfoOwner): string;
begin
  if Intf = nil then
    Result := Name
  else
    Result := Intf.GetBplNameOf(Self);
end;

function TRequiredPackage.IsRequiredByTarget(const TargetSymbol: string): Boolean;
begin
  Result := Targets.IndexOf(TargetSymbol) <> -1;
end;

{ TContainedFile }

constructor TContainedFile.Create(const AName, ATargets, AFormName,
  ACondition: string);
begin
  inherited Create;
  FName := AName;
  FTargets := TStringList.Create;
  TStringList(FTargets).Duplicates := dupIgnore;
  FTargets.CommaText := ATargets;
  if Assigned(ExpandPackageTargets) then
    ExpandPackageTargets(FTargets);
  FFormName := AFormName;
  FCondition := ACondition;
end;

destructor TContainedFile.Destroy;
begin
  FTargets.Free;
  inherited Destroy;
end;

function TContainedFile.IsUsedByTarget(const TargetSymbol: string): Boolean;
begin
  Result := Targets.IndexOf(TargetSymbol) <> -1;
end;

{ TPackageXmlInfo }

constructor TPackageXmlInfo.Create(const Filename: string);
begin
  FName := ChangeFileExt(ExtractFileName(Filename), '');
  FRequires := TObjectList.Create;
  FContains := TObjectList.Create;
  FIsDesign := EndsWith(Name, '-D', True); // do not localize
  LoadFromFile(Filename);
end;

destructor TPackageXmlInfo.Destroy;
begin
  FRequires.Free;
  FContains.Free;
  inherited Destroy;
end;

function TPackageXmlInfo.GetContainCount: Integer;
begin
  Result := FContains.Count;
end;

function TPackageXmlInfo.GetContains(Index: Integer): TContainedFile;
begin
  Result := TContainedFile(FContains[Index]);
end;

function TPackageXmlInfo.GetRequireCount: Integer;
begin
  Result := FRequires.Count;
end;

function TPackageXmlInfo.GetRequires(Index: Integer): TRequiredPackage;
begin
  Result := TRequiredPackage(FRequires[Index]);
end;

procedure TPackageXmlInfo.LoadFromFile(const Filename: string);
var
  i: Integer;
  RequirePkgName, RequireTarget,
  ContainsFileName, FormName, Condition: string;
  xml: TJvSimpleXML;
  RootNode : TJvSimpleXmlElemClassic;
  RequiredNode: TJvSimpleXmlElem;
  PackageNode: TJvSimpleXmlElem;
  ContainsNode: TJvSimpleXmlElem;
  FileNode: TJvSimpleXmlElem;
begin
  FRequires.Clear;
  FRequiresDB := False;
  FContains.Clear;

  xml := TJvSimpleXML.Create(nil);
  try
    xml.LoadFromFile(Filename);
    RootNode := xml.Root;
    RequiredNode := RootNode.Items.ItemNamed['Requires'];               // do not localize
    ContainsNode := RootNode.Items.ItemNamed['Contains'];               // do not localize

    FDisplayName := RootNode.Properties.ItemNamed['Name'].Value;        // do not localize
    if RootNode.Items.ItemNamed['Description'] <> nil then              // do not localize
      FDescription := RootNode.Items.ItemNamed['Description'].Value     // do not localize
    else
      FDescription := '';

   // requires
    for i := 0 to RequiredNode.Items.Count -1 do
    begin
      PackageNode := RequiredNode.Items[i];
      RequirePkgName := PackageNode.Properties.ItemNamed['Name'].Value; // do not localize
      if Pos('dcldb', AnsiLowerCase(RequirePkgName)) > 0 then           // do not localize
        FRequiresDB := True;

     // require only designtime packages
      RequireTarget := PackageNode.Properties.ItemNamed['Targets'].Value; // do not localize
      if RequireTarget = '' then
        RequireTarget := 'all';                                           // do not localize

      Condition := PackageNode.Properties.ItemNamed['Condition'].Value;   // do not localize

     // add new require item
      FRequires.Add(TRequiredPackage.Create(RequirePkgName, RequireTarget, Condition));
    end;

   // contains
    for i := 0 to ContainsNode.Items.Count -1 do
    begin
      FileNode := ContainsNode.Items[i];
      ContainsFileName := FileNode.Properties.ItemNamed['Name'].Value;    // do not localize

      RequireTarget := FileNode.Properties.ItemNamed['Targets'].Value;    // do not localize
      if RequireTarget = '' then
        RequireTarget := 'all';                                           // do not localize

      FormName := FileNode.Properties.ItemNamed['Formname'].Value;        // do not localize
      Condition := FileNode.Properties.ItemNamed['Condition'].Value;      // do not localize

     // add new require item
      FContains.Add(TContainedFile.Create(ContainsFileName, RequireTarget, FormName, Condition));
    end;
  finally
    xml.Free;
  end;
end;

initialization
  XmlFileCache := TObjectList.Create;

finalization
  XmlFileCache.Free;

end.