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
    function GetBplName(PackageGroup: TPackageGroup): string;

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
function GetPackageXmlInfo(const BplName, XmlDir: string): TPackageXmlInfo;
  { returns a cached TPackageXmlInfo instance. }

implementation

{$IFDEF COMPILER5}
const
  PathDelim = '\';
{$ENDIF COMPILER5}

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

{ TRequiredPackage }

constructor TRequiredPackage.Create(const AName, ATargets, ACondition: string);
begin
  inherited Create;
  FName := AName;
  FTargets := TStringList.Create;
  TStringList(FTargets).Duplicates := dupIgnore;
  FTargets.CommaText := ATargets;
  ExpandTargets(FTargets);
  FCondition := ACondition;
end;

destructor TRequiredPackage.Destroy;
begin
  FTargets.Free;
  inherited Destroy;
end;

function TRequiredPackage.GetBplName(PackageGroup: TPackageGroup): string;
begin
  if PackageGroup = nil then
    Result := Name
  else
    Result := PackageGroup.GetBplNameOf(Self);
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
  ExpandTargets(FTargets);
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
  Lines: TStrings;
  i, ps: Integer;
  S: string;
  TgName: string;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
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

 // we use dependencies so the order is irrelevant and we can alpha sort. [Comment from Installer]
  FPackages.Sort(SortProc_PackageTarget);

 // update dependencies after all package targets are created
  for i := 0 to Count - 1 do
    Packages[i].GetDependencies;
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
  Result := Owner.FindPackagebyXmlName(Copy(Info.Name, 1, Length(Info.Name) - 1) + 'R'); // do not localize
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

initialization
  XmlFileCache := TObjectList.Create;

finalization
  XmlFileCache.Free;

end.