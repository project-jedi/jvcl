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

{$I jvcl.inc}

unit PackageInformation;

interface

uses
  SysUtils, Classes, Contnrs,
  JvSimpleXml;

type
  { xml Package files }

  TPackageXmlInfo = class;
  TRequiredPackage = class;
  TContainedFile = class;
  TPackageGroup = class;

  /// <summary>
  /// TPackageXmlInfoItem contains common parts of TRequiredPackage and
  /// TContainedFile.
  /// </summary>
  TPackageXmlInfoItem = class(TObject)
  private
    FName: string;
    FTargets: TStrings;
    FCondition: string;
  public
    constructor Create(const AName, ATargets, ACondition: string);
    destructor Destroy; override;

    function IsIncluded(const TargetSymbol: string): Boolean;
      { IsIncluded() returns True if the item has TargetSymbol in it's Targets
        list. }

    property Name: string read FName;
    property Targets: TStrings read FTargets;
    property Condition: string read FCondition;
  end;

  /// <summary>
  /// TRequiredPackage contains one package that is requried by a TPackageXmlInfo
  /// object and it's inclusion conditions.
  /// </summary>
  TRequiredPackage = class(TPackageXmlInfoItem)
  public
    function IsRequiredByTarget(const TargetSymbol: string): Boolean;
    function GetBplName(PackageGroup: TPackageGroup): string;
  end;

  /// <summary>
  /// TContainedFile contains one file name that is contained in the
  /// TPackageXmlInfo object and it's inclusion conditions.
  /// </summary>
  TContainedFile = class(TPackageXmlInfoItem)
  private
    FFormName: string;
  public
    constructor Create(const AName, ATargets, AFormName, ACondition: string);

    function IsUsedByTarget(const TargetSymbol: string): Boolean;

    property FormName: string read FFormName;
  end;

  /// <summary>
  /// TPackageXmlInfo contains the generic .xml file for a bpl target.
  /// </summary>
  TPackageXmlInfo = class(TObject)
  private
    FFilename: string;
    FName: string;
    FDisplayName: string;
    FDescription: string;
    FClxDescription: string;
    FRequires: TObjectList;
    FContains: TObjectList;
    FRequiresDB: Boolean;
    FIsDesign: Boolean;
    FIsXPlatform: Boolean;
    FC5PFlags: string;
    FC6PFlags: string;
    FC5Libs: TStrings;
    FC6Libs: TStrings;
    FGUID: string;

    function GetContainCount: Integer;
    function GetContains(Index: Integer): TContainedFile;
    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): TRequiredPackage;

    procedure LoadFromFile(const Filename: string);
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;

    property Filename: string read FFilename;
    property Name: string read FName; // "PackageName-"[R|D]
    property DisplayName: string read FDisplayName; // "PackageName"
    property Description: string read FDescription;
    property ClxDescription: string read FClxDescription;
    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: TRequiredPackage read GetRequires;
    property ContainCount: Integer read GetContainCount;
    property Contains[Index: Integer]: TContainedFile read GetContains;
    property RequiresDB: Boolean read FRequiresDB;
    property IsDesign: Boolean read FIsDesign;
    property IsXPlaform: Boolean read FIsXPlatform;

    property C5PFlags: string read FC5PFlags;
    property C6PFlags: string read FC6PFlags;
    property C5Libs: TStrings read FC5Libs;
    property C6Libs: TStrings read FC6Libs;

    property GUID: string read FGUID;  // D9 support
  end;

  { Package Group }

  TPackageInfo = class;

  /// <summary>
  /// TBpgPackageTarget contains a .bpl target and the .xml file in the
  /// Info property.
  /// </summary>
  TBpgPackageTarget = class(TObject)
  private
    FOwner: TPackageGroup;
    FUserData: TObject;
    FTargetName: string;
    FSourceName: string;
    FInfo: TPackageInfo;
    FRequireList: TList;
    FContaineList: TList;
    FAutoDeleteUserData: Boolean;

    function GetRelSourceDir: string;
    function GetSourceDir: string;
    function GetContainCount: Integer;
    function GetContains(Index: Integer): TContainedFile;
    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): TRequiredPackage;
  protected
    procedure UpdateContainList; virtual;
    procedure UpdateRequireList; virtual;
    procedure GetDependencies; virtual; // is called after alle package targets are created
  public
    constructor Create(AOwner: TPackageGroup; const ATargetName, ASourceName: string); virtual;
    destructor Destroy; override;

    function FindRuntimePackage: TBpgPackageTarget;

    property TargetName: string read FTargetName;
    property SourceName: string read FSourceName;
    property SourceDir: string read GetSourceDir;
    property RelSourceDir: string read GetRelSourceDir;

    property Info: TPackageInfo read FInfo;

    // In contrast to Info.Xxx these properties only returns the
    // required/contained for this target.
    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: TRequiredPackage read GetRequires;
    property ContainCount: Integer read GetContainCount;
    property Contains[Index: Integer]: TContainedFile read GetContains;

    property Owner: TPackageGroup read FOwner;
    property UserData: TObject read FUserData write FUserData;
    property AutoDeleteUserData: Boolean read FAutoDeleteUserData write FAutoDeleteUserData default True;
  end;

  TBpgPackageTargetClass = class of TBpgPackageTarget;

  /// <summary>
  /// TPackageGroup contains the data from a .bpg (Borland Package Group) file.
  /// </summary>
  TPackageGroup = class(TObject)
  private
    FPackages: TObjectList;
    FFilename: string;
    FPackagesXmlDir: string;
    FTargetSymbol: string;

    function GetCount: Integer;
    function GetPackages(Index: Integer): TBpgPackageTarget;
    function GetBpgName: string;
    function Add(const TargetName, SourceName: string): TBpgPackageTarget;
    procedure LoadBDSGroupFile;
    procedure LoadBPGFile;
  protected
    function GetIsVCLX: Boolean; virtual;
    function GetPackageTargetClass: TBpgPackageTargetClass; virtual;
    procedure LoadFile;
  public
    constructor Create(const AFilename, APackagesXmlDir, ATargetSymbol: string);
      { Set AFilename to '' if you want a PackageGroup instance that does not
        own the TBpgPackageTarget objects. }
    destructor Destroy; override;

    procedure AddPackage(Pkg: TBpgPackageTarget);
    function FindPackageByXmlName(const XmlName: string): TBpgPackageTarget;
      { FindPackageByXmlName returns the TBpgPackageTarget object that contains
        the specified .xml file. }
    function GetBplNameOf(Package: TRequiredPackage): string; virtual;

    property Count: Integer read GetCount;
    property Packages[Index: Integer]: TBpgPackageTarget read GetPackages; default;

    property BpgName: string read GetBpgName;
    property Filename: string read FFilename;
    property IsVCLX: Boolean read GetIsVCLX;
    property PackagesXmlDir: string read FPackagesXmlDir;
    property TargetSymbol: string read FTargetSymbol;
  end;

  /// <summary>
  /// TPackageInfo is a wrapper for TPackageXmlInfo objects that contains the
  /// generic .xml file for a bpl target.
  /// </summary>
  TPackageInfo = class(TObject)
  private
    FOwner: TBpgPackageTarget;
    FXmlInfo: TPackageXmlInfo;
    FXmlDir: string;

    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): TRequiredPackage;
    function GetContainCount: Integer;
    function GetContains(Index: Integer): TContainedFile;
    function GetBplName: string;
    function GetDescription: string;
    function GetDisplayName: string;
    function GetIsDesign: Boolean;
    function GetName: string;
    function GetRequiresDB: Boolean;
  public
    constructor Create(AOwner: TBpgPackageTarget; const AXmlDir: string);

    property Name: string read GetName; // "PackageName-"[R|D]
    property DisplayName: string read GetDisplayName; // "PackageName"
    property BplName: string read GetBplName; // "PackageName"[D|C][5-7][R|D]
    property Description: string read GetDescription;
    property RequiresDB: Boolean read GetRequiresDB;
    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: TRequiredPackage read GetRequires;
    property ContainCount: Integer read GetContainCount;
    property Contains[Index: Integer]: TContainedFile read GetContains;
    property IsDesign: Boolean read GetIsDesign;

    property Owner: TBpgPackageTarget read FOwner;
    property XmlDir: string read FXmlDir;
  end;

var
  BplNameToGenericNameHook: function(const BplName: string): string = nil;
  ExpandPackageTargets: procedure(Targets: TStrings) = nil;
  ExpandPackageTargetsObj: procedure(Targets: TStrings) of object = nil;

function BplNameToGenericName(const BplName: string): string;
  { BplNameToGenericName converts a "JvCoreD7D.XXX" to "JvCore-D" }
function GetPackageXmlInfo(const BplName, XmlDir: string): TPackageXmlInfo; overload;
  { returns a cached TPackageXmlInfo instance. }
function GetPackageXmlInfo(const XmlFilename: string): TPackageXmlInfo; overload;
  { returns a cached TPackageXmlInfo instance. }

implementation

{$IFDEF COMPILER5}
const
  PathDelim = '\';
{$ENDIF COMPILER5}

var
  XmlFileCache: TStringList; // cache for .xml files ( TPackageXmlInfo )

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
      Insert('-', Result, Length(Result)); // do not localize
  end;
end;

procedure ExpandTargets(Targets: TStrings);
begin
  if Assigned(ExpandPackageTargetsObj) then
    ExpandPackageTargetsObj(Targets);
  if Assigned(ExpandPackageTargets) then
    ExpandPackageTargets(Targets);
end;

/// <summary>
/// GetPackageXmlInfo returns a cached TPackageXmlInfo instance.
/// </summary>
function GetPackageXmlInfo(const BplName, XmlDir: string): TPackageXmlInfo; overload;
var
  Index: Integer;
  Name: string;
begin
  Name := XmlDir + PathDelim + BplNameToGenericName(BplName) + '.xml';
 // already in the cache
  if XmlFileCache.Find(Name, Index) then
    Result := TPackageXmlInfo(XmlFileCache.Objects[Index])
  else
  begin
   // create a new one and add it to the cache
    Result := TPackageXmlInfo.Create(Name); // do not localize
    XmlFileCache.AddObject(Name, Result);
  end;
end;

function GetPackageXmlInfo(const XmlFilename: string): TPackageXmlInfo; overload;
var
  Index: Integer;
begin
  if XmlFileCache.Find(XmlFilename, Index) then
    Result := TPackageXmlInfo(XmlFileCache.Objects[Index])
  else
  begin
   // create a new one and add it to the cache
    Result := TPackageXmlInfo.Create(XmlFilename);
    XmlFileCache.AddObject(XmlFilename, Result);
  end;
end;

function StartsWith(const Text, StartText: string; CaseInsensitive: Boolean = False): Boolean;
var
  Len, i: Integer;
begin
  Result := False;
  Len := Length(StartText);
  if Len > Length(Text) then
    Exit;
  if CaseInsensitive then
  begin
    for i := 1 to Len do
      if UpCase(Text[i]) <> UpCase(StartText[i]) then
        Exit;
  end
  else
  begin
    for i := 1 to Len do
      if Text[i] <> StartText[i] then
        Exit;
  end;
  Result := True;
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

function CutFirstDirectory(var Dir: string): string;
var
  ps: Integer;
begin
  ps := Pos(PathDelim, Dir);
  if ps > 0 then
  begin
    Result := Copy(Dir, 1, ps - 1);
    Delete(Dir, 1, ps);
  end
  else
  begin
    Result := Dir;
    Dir := '';
  end;
end;

function FollowRelativeFilename(const RootDir: string; RelFilename: string): string;
var
  Dir: string;
begin
  Result := RootDir;
  while RelFilename <> '' do
  begin
    Dir := CutFirstDirectory(RelFilename);
    if Dir = '..' then
      Result := ExtractFileDir(Result)
    else if Dir = '.' then
      Continue
    else
      Result := Result + PathDelim + Dir;
  end;
end;

function LoadUtf8File(const Filename: string): string;
var
  Content: UTF8String;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Content, Stream.Size);
    Stream.Read(Content[1], Stream.Size);
  finally
    Stream.Free;
  end;
  Delete(Content, 1, 3); // This a little bit dirty but unless we mix UTF8,
                         // UTF16 and ANSI files there is no problem.
  {$IFDEF COMPILER6_UP}
  Result := Utf8ToAnsi(Content);
  {$ELSE}
    { Delphi 5 (should) never reachs this because the Installer uses the newest
      installed Delphi version and only reads the project groups of installed
      Delphi/BCB/BDN versions. }
  Result := Content;
  {$ENDIF COMPILIER6_UP}
end;

{ TPackageXmlInfoItem }

constructor TPackageXmlInfoItem.Create(const AName, ATargets, ACondition: string);
begin
  inherited Create;
  FName := AName;
  FTargets := TStringList.Create;
  TStringList(FTargets).Duplicates := dupIgnore;
  FTargets.CommaText := ATargets;
  ExpandTargets(FTargets);
  TStringList(FTargets).Sorted := True; // sort the targets
  FCondition := ACondition;
end;

destructor TPackageXmlInfoItem.Destroy;
begin
  FTargets.Free;
  inherited Destroy;
end;

function TPackageXmlInfoItem.IsIncluded(const TargetSymbol: string): Boolean;
var
  Index: Integer;
begin
  Result := TStringList(FTargets).Find(TargetSymbol, Index);
end;

{ TRequiredPackage }

function TRequiredPackage.GetBplName(PackageGroup: TPackageGroup): string;
begin
  if PackageGroup = nil then
    Result := Name
  else
    Result := PackageGroup.GetBplNameOf(Self);
end;

function TRequiredPackage.IsRequiredByTarget(const TargetSymbol: string): Boolean;
begin
  Result := IsIncluded(TargetSymbol);
end;

{ TContainedFile }

constructor TContainedFile.Create(const AName, ATargets, AFormName,
  ACondition: string);
begin
  inherited Create(AName, ATargets, ACondition);
  FFormName := AFormName;
end;

function TContainedFile.IsUsedByTarget(const TargetSymbol: string): Boolean;
begin
  Result := IsIncluded(TargetSymbol);
end;

{ TPackageXmlInfo }

constructor TPackageXmlInfo.Create(const AFilename: string);
begin
  inherited Create;
  FFilename := AFilename;
  FName := ChangeFileExt(ExtractFileName(FFilename), '');
  FRequires := TObjectList.Create;
  FContains := TObjectList.Create;
  FIsDesign := EndsWith(Name, '-D', True); // do not localize
    // IsDesign is updated in LoadFromFile
  FC5Libs := TStringList.Create;
  FC6Libs := TStringList.Create;

  LoadFromFile(FFilename);
end;

destructor TPackageXmlInfo.Destroy;
begin
  FC5Libs.Free;
  FC6Libs.Free;
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

    FGUID := RootNode.Items.Value('GUID');
    FC5PFlags := RootNode.Items.Value('C5PFlags');
    FC6PFlags := RootNode.Items.Value('C6PFlags');
    FC5Libs.CommaText := RootNode.Items.Value('C5Libs');
    FC6Libs.CommaText := RootNode.Items.Value('C6Libs');

    RequiredNode := RootNode.Items.ItemNamed['Requires'];               // do not localize
    ContainsNode := RootNode.Items.ItemNamed['Contains'];               // do not localize

    FDisplayName := RootNode.Properties.Value('Name');                  // do not localize
    FIsDesign := RootNode.Properties.BoolValue('Design', IsDesign);     // do not localize
    FIsXPlatform := RootNode.Properties.BoolValue('XPlatform', False);  // do not localize
    FDescription := RootNode.Items.Value('Description');                // do not localize
    FClxDescription := RootNode.Items.Value('ClxDescription');          // do not localize

   // requires
    for i := 0 to RequiredNode.Items.Count -1 do
    begin
      PackageNode := RequiredNode.Items[i];
      RequirePkgName := PackageNode.Properties.Value('Name');           // do not localize
      if Pos('dcldb', AnsiLowerCase(RequirePkgName)) > 0 then           // do not localize
        FRequiresDB := True;

     // require only designtime packages
      RequireTarget := PackageNode.Properties.Value('Targets');         // do not localize
      if RequireTarget = '' then
        RequireTarget := 'all';                                         // do not localize

      Condition := PackageNode.Properties.Value('Condition');           // do not localize

     // add new require item
      FRequires.Add(TRequiredPackage.Create(RequirePkgName, RequireTarget, Condition));
    end;

   // contains
    for i := 0 to ContainsNode.Items.Count -1 do
    begin
      FileNode := ContainsNode.Items[i];
      ContainsFileName := FileNode.Properties.ItemNamed['Name'].Value;  // do not localize

      RequireTarget := FileNode.Properties.Value('Targets');            // do not localize
      if RequireTarget = '' then
        RequireTarget := 'all';                                         // do not localize

      FormName := FileNode.Properties.Value('Formname');                // do not localize
      Condition := FileNode.Properties.Value('Condition');              // do not localize

     // add new require item
      FContains.Add(TContainedFile.Create(ContainsFileName, RequireTarget, FormName, Condition));
    end;
  finally
    xml.Free;
  end;
end;

{ TPackageGroup }

constructor TPackageGroup.Create(const AFilename, APackagesXmlDir, ATargetSymbol: string);
begin
  inherited Create;

  FPackagesXmlDir := APackagesXmlDir;
  if (FPackagesXmlDir <> '') and (FPackagesXmlDir[Length(FPackagesXmlDir)] = PathDelim) then
    Delete(FPackagesXmlDir, Length(FPackagesXmlDir), 1);

  FTargetSymbol := ATargetSymbol;
  FFilename := AFilename;
  FPackages := TObjectList.Create(Filename <> '');
  if Filename <> '' then
    LoadFile;
end;

destructor TPackageGroup.Destroy;
begin
  FPackages.Free;
  inherited Destroy;
end;

function TPackageGroup.Add(const TargetName, SourceName: string): TBpgPackageTarget;
begin
  Result := nil;
  if FileExists(PackagesXmlDir + PathDelim + BplNameToGenericName(TargetName) + '.xml') then // do not localize
  begin
    try
      Result := GetPackageTargetClass.Create(Self, TargetName, SourceName)
    except
      on E: EFOpenError do
        FreeAndNil(Result);
    end;
    if Result <> nil then
      FPackages.Add(Result);
  end;
end;

procedure TPackageGroup.AddPackage(Pkg: TBpgPackageTarget);
begin
  if Pkg <> nil then
    FPackages.Add(Pkg);
end;

function TPackageGroup.FindPackageByXmlName(const XmlName: string): TBpgPackageTarget;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Packages[i];
    if CompareText(Result.Info.Name, XmlName) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TPackageGroup.GetBpgName: string;
begin
  Result := ExtractFileName(Filename);
end;

function TPackageGroup.GetBplNameOf(Package: TRequiredPackage): string;
var
  Pkg: TBpgPackageTarget;
begin
  Pkg := FindPackagebyXmlName(Package.Name);
  if Pkg <> nil then
    Result := Pkg.TargetName
  else
    Result := Package.Name;
end;

function TPackageGroup.GetCount: Integer;
begin
  Result := FPackages.Count;
end;

function TPackageGroup.GetIsVCLX: Boolean;
begin
  Result := Pos('clx', LowerCase(BpgName)) > 0;
end;

function TPackageGroup.GetPackages(Index: Integer): TBpgPackageTarget;
begin
  Result := TBpgPackageTarget(FPackages[Index]);
end;

function SortProc_PackageTarget(Item1, Item2: Pointer): Integer;
var
  p1, p2: TBpgPackageTarget;
begin
  p1 := Item1;
  p2 := Item2;
  Result := CompareText(p1.Info.DisplayName, p2.Info.DisplayName);
  if Result = 0 then
  begin
    if p1.Info.IsDesign and not p2.Info.IsDesign then
      Result := 1
    else if not p1.Info.IsDesign and p2.Info.IsDesign then
      Result := -1;
  end;
end;

function TPackageGroup.GetPackageTargetClass: TBpgPackageTargetClass;
begin
  Result := TBpgPackageTarget;
end;

procedure TPackageGroup.LoadFile;
var
  i: Integer;
begin
  if CompareText(ExtractFileExt(FileName), '.bdsgroup') = 0 then
    LoadBDSGroupFile
  else
    LoadBPGFile;

 // we use dependencies so the order is irrelevant and we can alpha sort. [Comment from Installer]
  FPackages.Sort(SortProc_PackageTarget);

 // update dependencies after all package targets are created
  for i := 0 to Count - 1 do
    Packages[i].GetDependencies;
end;

procedure TPackageGroup.LoadBDSGroupFile;
var
  xml: TJvSimpleXML;
  Options, Projects: TJvSimpleXMLElem;
  i, OptIndex, PrjIndex: Integer;
  Personality: string;
  TgName: string;
begin
  xml := TJvSimpleXML.Create(nil);
  try
    xml.LoadFromString(LoadUtf8File(Filename));

    for i := 0 to xml.Root.Items.Count - 1 do
    begin
      if (CompareText(xml.Root.Items[i].Name, 'PersonalityInfo') = 0) and // <PersonalityInfo>
         (xml.Root.Items[i].Items.Count > 0) then
      begin
        // find correct Personality
        Options := xml.Root.Items[i].Items[0];
        if CompareText(Options.Name, 'Option') = 0 then
        begin
          for OptIndex := 0 to Options.Items.Count - 1 do
            if CompareText(Options.Items[OptIndex].Properties.Value('Name'), 'Personality') = 0 then
            begin
              Personality := Options.Items[OptIndex].Value;
              Break;
            end;
        end;
      end
      else
      if (CompareText(xml.Root.Items[i].Name, Personality) = 0) and
         (xml.Root.Items[i].Items.Count > 0) and
         (CompareText(xml.Root.Items[i].Items[0].Name, 'Projects') = 0) then
      begin
         // Read project list
         Projects := xml.Root.Items[i].Items[0];
         for PrjIndex := 0 to Projects.Items.Count - 1 do
         begin
           TgName := Projects.Items[PrjIndex].Properties.Value('Name');
           if CompareText(TgName, 'Targets') <> 0 then
             // change .bdsproj to .dpk and add the target
             Add(TgName, ChangeFileExt(Projects.Items[PrjIndex].Value, '.dpk'));
         end;
      end;
    end;
  finally
    xml.Free;
  end;
end;

procedure TPackageGroup.LoadBPGFile;
var
  Lines: TStrings;
  i, ps: Integer;
  S: string;
  TgName: string;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Filename);
    i := 0;

    // find "default:" target
    while i < Lines.Count do
    begin
      if StartsWith(Lines[I], 'default:', True) then // do not localize
        Break;
      Inc(i);
    end;
    Inc(i, 2);

    // now read the available targets
    while i < Lines.Count do
    begin
      S := Lines[i];
      // find targets
      if S <> '' then
      begin
        if S[1] > #32 then
        begin
          ps := Pos(':', S);
          if ps > 0 then
          begin
            TgName := TrimRight(Copy(S, 1, ps - 1));
            // does the .xml file exists for this target? <-> is it a vaild target?
            Add(TgName, Trim(Copy(S, ps + 1, MaxInt)));
          end;
        end;
      end;
      Inc(i);
    end;
  finally
    Lines.Free;
  end;
end;

{ TBpgPackageTarget }

constructor TBpgPackageTarget.Create(AOwner: TPackageGroup; const ATargetName,
  ASourceName: string);
begin
  inherited Create;
  FOwner := AOwner;
  FTargetName := ATargetName;
  FSourceName := ASourceName;
  FInfo := TPackageInfo.Create(Self, AOwner.PackagesXmlDir);
  FRequireList := TList.Create;
  FContaineList := TList.Create;
end;

destructor TBpgPackageTarget.Destroy;
begin
  if AutoDeleteUserData then
    FUserData.Free;
  FRequireList.Free;
  FContaineList.Free;
  // FInfo is buffered and is destroyed by XmlFileCache
  inherited Destroy;
end;

function TBpgPackageTarget.FindRuntimePackage: TBpgPackageTarget;
begin
  Result := Owner.FindPackageByXmlName(Copy(Info.Name, 1, Length(Info.Name) - 1) + 'R'); // do not localize
end;

function TBpgPackageTarget.GetContainCount: Integer;
begin
  UpdateContainList;
  Result := FContaineList.Count;
end;

function TBpgPackageTarget.GetContains(Index: Integer): TContainedFile;
begin
  UpdateContainList;
  Result := TContainedFile(FContaineList[Index]);
end;

procedure TBpgPackageTarget.GetDependencies;
begin
  // do nothing by default
end;

function TBpgPackageTarget.GetRelSourceDir: string;
begin
  Result := ExtractFileDir(FSourceName);
end;

function TBpgPackageTarget.GetRequireCount: Integer;
begin
  UpdateRequireList;
  Result := FRequireList.Count;
end;

function TBpgPackageTarget.GetRequires(Index: Integer): TRequiredPackage;
begin
  UpdateRequireList;
  Result := TRequiredPackage(FRequireList[Index]);
end;

function TBpgPackageTarget.GetSourceDir: string;
begin
  Result := FollowRelativeFilename(ExtractFileDir(Owner.Filename), RelSourceDir);
end;

procedure TBpgPackageTarget.UpdateContainList;
var
  i: Integer;
begin
  if FContaineList.Count = 0 then
  begin
    for i := 0 to Info.ContainCount - 1 do
      if Info.Contains[i].IsUsedByTarget(Owner.TargetSymbol) then
        FContaineList.Add(Info.Contains[i]);
  end;
end;

procedure TBpgPackageTarget.UpdateRequireList;
var
  i: Integer;
begin
  if FRequireList.Count = 0 then
  begin
    for i := 0 to Info.RequireCount - 1 do
      if Info.Requires[i].IsRequiredByTarget(Owner.TargetSymbol) then
        FRequireList.Add(Info.Requires[i]);
  end;
end;

{ TPackageInfo }

constructor TPackageInfo.Create(AOwner: TBpgPackageTarget; const AXmlDir: string);
begin
  inherited Create;
  FOwner := AOwner;
  FXmlDir := AXmlDir;
  FXmlInfo := GetPackageXmlInfo(Owner.TargetName, AXmlDir);
end;

function TPackageInfo.GetBplName: string;
begin
  Result := Owner.TargetName;
end;

function TPackageInfo.GetContainCount: Integer;
begin
  Result := FXmlInfo.ContainCount;
end;

function TPackageInfo.GetContains(Index: Integer): TContainedFile;
begin
  Result := FXmlInfo.Contains[Index];
end;

function TPackageInfo.GetRequireCount: Integer;
begin
  Result := FXmlInfo.RequireCount;
end;

function TPackageInfo.GetRequires(Index: Integer): TRequiredPackage;
begin
  Result := FXmlInfo.Requires[Index];
end;

function TPackageInfo.GetDescription: string;
begin
  Result := FXmlInfo.Description;
end;

function TPackageInfo.GetDisplayName: string;
begin
  Result := FXmlInfo.DisplayName;
end;

function TPackageInfo.GetIsDesign: Boolean;
begin
  Result := FXmlInfo.IsDesign;
end;

function TPackageInfo.GetName: string;
begin
  Result := FXmlInfo.Name;
end;

function TPackageInfo.GetRequiresDB: Boolean;
begin
  Result := FXmlInfo.RequiresDB;
end;

procedure FinalizeXmlFileCache;
var
  i: Integer;
begin
  for i := 0 to XmlFileCache.Count - 1 do
    XmlFileCache.Objects[i].Free;
  XmlFileCache.Free;
end;

initialization
  XmlFileCache := TStringList.Create;
  XmlFileCache.Sorted := True;

finalization
  FinalizeXmlFileCache;

end.
