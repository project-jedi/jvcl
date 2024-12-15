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
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit PackageInformation;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Contnrs,
  JclSimpleXml, JclFileUtils;
  
type
  TCompileTargetPlatform = (ctpWin32, ctpWin64);

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
    FTargets: TStringList;
    FCondition: string;
  public
    constructor Create(const AName, ATargets, ACondition: string);
    destructor Destroy; override;

    function IsIncluded(const TargetSymbol: string): Boolean;
      { IsIncluded() returns True if the item has TargetSymbol in it's Targets
        list. }

    property Name: string read FName;
    property Targets: TStringList read FTargets;
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

  TPlatformType = (pftWin32, pftWin64, pftWin64x);
  TPlatformTypes = set of TPlatformType;

  TPlatform = class(TPackageXmlInfoItem)
  private
    FPlatformType: TPlatformType;
  public
    constructor Create(const AName, ATargets, ACondition: string);
    function IsSupportedByTarget(const TargetSymbol: string): Boolean;
    property PlatformType: TPlatformType read FPlatformType;
  end;

  /// <summary>
  /// TProjectType specifies the type of project defined in the package
  /// </summary>
  TProjectType = ( ptPackageRun, ptPackageDesign, ptPackage, ptLibrary,
    ptProgram );

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
    FPlatforms: TObjectList;
    FRequiresDB: Boolean;
    FProjectType: TProjectType;
    FIsXPlatform: Boolean;
    FGUID: string;
    FProperties: TStrings;
    FC6Libs: TStrings;
    FCompilerDefines: TStrings;

    function GetContainCount: Integer;
    function GetContains(Index: Integer): TContainedFile;
    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): TRequiredPackage;
    function GetPlatformCount: Integer;
    function GetPlatforms(Index: Integer): TPlatform;
    function GetPlatformTypes: TPlatformTypes;

    procedure LoadFromFile(const Filename: string);
    function GetBuildNumber: string;
    function GetC6Libs: TStrings;
    function GetC6PFlags: string;
    function GetCompilerDefines: TStrings;
    function GetImageBase: string;
    function GetReleaseNumber: string;
    function GetVersionMajorNumber: string;
    function GetVersionMinorNumber: string;
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
    property PlatformCount: Integer read GetPlatformCount;
    property Platforms[Index: Integer]: TPlatform read GetPlatforms;
    property PlatformTypes: TPlatformTypes read GetPlatformTypes;
    property RequiresDB: Boolean read FRequiresDB;
    property ProjectType: TProjectType read FProjectType;
    property IsXPlaform: Boolean read FIsXPlatform;
    property ImageBase: string read GetImageBase;
    property VersionMajorNumber: string read GetVersionMajorNumber;
    property VersionMinorNumber: string read GetVersionMinorNumber;
    property ReleaseNumber: string read GetReleaseNumber;
    property BuildNumber: string read GetBuildNumber;

    property C6PFlags: string read GetC6PFlags;
    property C6Libs: TStrings read GetC6Libs;

    property CompilerDefines: TStrings read GetCompilerDefines;
    property GUID: string read FGUID;  // D9 support
    property Properties: TStrings read FProperties;
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
    FPlatformList: TList;
    FAutoDeleteUserData: Boolean;

    function GetRelSourceDir: string;
    function GetSourceDir: string;
    function GetContainCount: Integer;
    function GetContains(Index: Integer): TContainedFile;
    function GetPlatformCount: Integer;
    function GetPlatforms(Index: Integer): TPlatform;
    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): TRequiredPackage;
  protected
    procedure UpdateContainList; virtual;
    procedure UpdateRequireList; virtual;
    procedure UpdatePlatformList; virtual;
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
    property PlatformCount: Integer read GetPlatformCount;
    property Platforms[Index: Integer]: TPlatform read GetPlatforms;

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
    FTargetPlatform: TCompileTargetPlatform;
    FPackageXmlDirFileNames: TStringList;

    function GetCount: Integer;
    function GetPackages(Index: Integer): TBpgPackageTarget;
    function GetBpgName: string;
    function Add(const TargetName, SourceName: string): TBpgPackageTarget;
    procedure LoadGroupProjFile;
    procedure LoadBDSGroupFile;
    procedure LoadBPGFile;
  protected
    function GetIsVCLX: Boolean; virtual;
    function GetPackageTargetClass: TBpgPackageTargetClass; virtual;
    procedure LoadFile;
    function XmlFileExists(const XmlName: string): Boolean;
  public
    constructor Create(const AFilename, APackagesXmlDir, ATargetSymbol: string; ATargetPlatform: TCompileTargetPlatform);
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
    FXmlDir: string;

    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): TRequiredPackage;
    function GetContainCount: Integer;
    function GetContains(Index: Integer): TContainedFile;
    function GetPlatformCount: Integer;
    function GetPlatforms(Index: Integer): TPlatform;
    function GetBplName: string;
    function GetDescription: string;
    function GetDisplayName: string;
    function GetProjectType: TProjectType;
    function GetName: string;
    function GetRequiresDB: Boolean;
    function GetXmlInfo: TPackageXmlInfo;
  public
    constructor Create(AOwner: TBpgPackageTarget; const AXmlDir: string);

    property Name: string read GetName; // "PackageName-"[R|D]
    property DisplayName: string read GetDisplayName; // "PackageName"
    property BplName: string read GetBplName; // "PackageName"[D|C][60-140]
    property Description: string read GetDescription;
    property RequiresDB: Boolean read GetRequiresDB;
    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: TRequiredPackage read GetRequires;
    property ContainCount: Integer read GetContainCount;
    property Contains[Index: Integer]: TContainedFile read GetContains;
    property PlatformCount: Integer read GetPlatformCount;
    property Platforms[Index: Integer]: TPlatform read GetPlatforms;
    property ProjectType: TProjectType read GetProjectType;

    property XmlInfo: TPackageXmlInfo read GetXmlInfo;

    property Owner: TBpgPackageTarget read FOwner;
    property XmlDir: string read FXmlDir;
  end;

var
  ExpandPackageTargets: procedure(Targets: TStrings) = nil;
  ExpandPackageTargetsObj: procedure(Targets: TStrings) of object = nil;

  /// <summary>
  /// BplNameToGenericName converts a "JvCoreDesign140.XXX" to "JvCore-D" }
  /// </summary>
function BplNameToGenericName(const BplName: string): string;

  /// <summary>
  /// GenericNameToBplName converts a "JvCore-D" to "JvCoreDesign140.bpl'}
  /// </summary>
function GenericNameToBplName(const GenericName: string; CompilerVersion: Integer): string;

  /// <summary>
  /// returns a cached TPackageXmlInfo instance.
  /// </summary>
function GetPackageXmlInfo(const BplName, XmlDir: string): TPackageXmlInfo; overload;

  /// <summary>
  /// returns a cached TPackageXmlInfo instance.
  /// </summary>
function GetPackageXmlInfo(const XmlFilename: string): TPackageXmlInfo; overload;

  /// <summary>
  /// ProjectTypeToChar convert a project type into one char
  /// </summary>
function ProjectTypeToChar(AProjectType: TProjectType): Char;

  /// <summary>
  /// CharToProjectType converts a char to one project type
  /// </summary>
function CharToProjectType(AProjectChar: Char): TProjectType;

  /// <summary>
  /// ProjectTypeIsDesign returns if a project is made to be run inside
  ///  the IDE
  /// </summary>
function ProjectTypeIsDesign(AProjectType: TProjectType): Boolean;

  /// <summary>
  /// ProjectTypeToBinaryExtension returns the extension associated to the
  ///  binary file made by the compiler
  /// </summary>
function ProjectTypeToBinaryExtension(AProjectType: TProjectType): string;

  /// <summary>
  /// ProjectTypeToSourceExtension returns the extension of the project file
  /// </summary>
function ProjectTypeToSourceExtension(AProjectType: TProjectType): string;

  /// <summary>
  /// ProjectTypeIsDLL returns if the project type is compiled as a DLL
  /// </summary>
function ProjectTypeIsDLL(AProjectType: TProjectType): Boolean;

  /// <summary>
  /// ProjectTypeIsDLL returns if the project type is compiled as a package
  /// </summary>
function ProjectTypeIsPackage(AProjectType: TProjectType): Boolean;

  /// <summary>
  /// ProjectTypeToProjectName returns the project name associated to the
  ///  source file (usually this is the first word of the project file):
  ///  'Package', 'Program' or 'Library'
  /// </summary>
function ProjectTypeToProjectName(ProjectType: TProjectType): string;

  // <summary>
  // Clear the cache of XML package files
  // </summary>
procedure ClearXmlFileCache;

function PlatformNameToPlatformType(const APlatformName: string): TPlatformType;

const
  C6PFlagsKnownPackageProperty           = 'C6PFlags';
  C6LibsKnownPackageProperty             = 'C6Libs';
  CompilerDefinesKnownPackageProperty    = 'CompilerDefines';
  ImageBaseKnownPackageProperty          = 'ImageBase';
  VersionMajorNumberKnownPackageProperty = 'VersionMajorNumber';
  VersionMinorNumberKnownPackageProperty = 'VersionMinorNumber';
  ReleaseNumberKnownPackageProperty      = 'ReleaseNumber';
  BuildNumberKnownPackageProperty        = 'BuildNumber';
  KnownPackageProperties: array [0..7] of string =
    ( C6PFlagsKnownPackageProperty,
      C6LibsKnownPackageProperty,
      CompilerDefinesKnownPackageProperty,
      ImageBaseKnownPackageProperty,
      VersionMajorNumberKnownPackageProperty,
      VersionMinorNumberKnownPackageProperty,
      ReleaseNumberKnownPackageProperty,
      BuildNumberKnownPackageProperty );

implementation

uses
  JvJCLUtils;

var
  XmlFileCache: TStringList; // cache for .xml files ( TPackageXmlInfo )

procedure ClearXmlFileCache;
var
  i: Integer;
begin
  for i := 0 to XmlFileCache.Count - 1 do
    XmlFileCache.Objects[i].Free;
  XmlFileCache.Clear;
end;

function BplNameToGenericName(const BplName: string): string;
var
  I, Len : Integer;
begin
   // obtain package name used in the xml file
  Result := ChangeFileExt(BplName, '');

  // Remove numbers from the end of the package name
  Len := Length(Result);
  I := Len;
  while (I > 0) and CharInSet(Result[I], ['0'..'9']) do
    Dec(I);
  if (I > 0) and ((Result[I] = 'D') or (Result[I] = 'C')) then
    Dec(I);
  Delete(Result, I+1, Len-I);
end;

function GenericNameToBplName(const GenericName: string; CompilerVersion: Integer): string;
begin
  if EndsText('-R', GenericName) then
    Result := Copy(GenericName, 1, Length(GenericName) - 2) + IntToStr(CompilerVersion) + '0.bpl'
  else if EndsText('-D', GenericName) then
    Result := Copy(GenericName, 1, Length(GenericName) - 2) + 'Design' + IntToStr(CompilerVersion) + '0.bpl'
  else
    Result := GenericName;
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
  GenericPrefix, Name, RuntimeName: string;
begin
  GenericPrefix := XmlDir + PathDelim + BplNameToGenericName(BplName);
  RuntimeName := GenericPrefix + '-R.xml';
  Name := RuntimeName;
 // already in the cache
  if XmlFileCache.Find(Name, Index) then
    Result := TPackageXmlInfo(XmlFileCache.Objects[Index])
  else
  begin
    Result := nil;
    Name := GenericPrefix + '-D.xml';
    if XmlFileCache.Find(Name, Index) then
      Result := TPackageXmlInfo(XmlFileCache.Objects[Index])
    else
    begin
      // Access the disk as the last option
      if FileExists(RuntimeName) then
        Name := RuntimeName;
    end;
    if Result = nil then
    begin
     // create a new one and add it to the cache
      Result := TPackageXmlInfo.Create(Name); // do not localize
      XmlFileCache.AddObject(Name, Result);
    end;
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

  // Remove the BOM if it is present. It is NOT a requirement of UTF-8 that
  // the BOM is present.
  if (Content[1] = #$EF) and (Content[2] = #$BB) and (Content[3] = #$BF) then
    Delete(Content, 1, 3);
    
  Result := {$IFDEF SUPPORTS_UNICODE}Utf8ToString{$ELSE}Utf8ToAnsi{$ENDIF SUPPORTS_UNICODE}(Content);
end;

// (rom) copied from JclStrings.pas

procedure StrToStrings(S, Sep: string; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  Left: string;
begin
  Assert(List <> nil);
  List.BeginUpdate;
  try
    List.Clear;
    L := Length(Sep);
    I := Pos(Sep, S);
    while I > 0 do
    begin
      Left := Copy(S, 1, I - 1);
      if (Left <> '') or AllowEmptyString then
        List.Add(Left);
      Delete(S, 1, I + L - 1);
      I := Pos(Sep, S);
    end;
    if S <> '' then
      List.Add(S);  // Ignore empty strings at the end.
  finally
    List.EndUpdate;
  end;
end;

function ProjectTypeToChar(AProjectType: TProjectType): Char;
begin
  case AProjectType of
    ptPackageRun:
      Result := 'R';              // do not localize
    ptPackageDesign:
      Result := 'D';              // do not localize
    ptPackage:
      Result := 'P';              // do not localize
    ptLibrary:
      Result := 'L';              // do not localize
    ptProgram:
      Result := 'X';              // do not localize
    else
      raise Exception.Create('Invalid project type');
  end;
end;

function CharToProjectType(AProjectChar: Char): TProjectType;
begin
  case AProjectChar of
    'R', 'r':                     // do not localize
      Result := ptPackageRun;
    'D', 'd':                     // do not localize
      Result := ptPackageDesign;
    'P', 'p':                     // do not localize
      Result := ptPackage;
    'L', 'l':                     // do not localize
      Result := ptLibrary;
    'X', 'x':                     // do not localize
      Result := ptProgram;
    else
      raise Exception.Create('Invalid project char');
  end;
end;

function ProjectTypeIsDesign(AProjectType: TProjectType): Boolean;
begin
  Result := AProjectType in [ptPackage,ptPackageDesign];
end;

function ProjectTypeToBinaryExtension(AProjectType: TProjectType): string;
begin
  case AProjectType of
    ptPackageRun,
    ptPackageDesign,
    ptPackage: Result := '.bpl';        // do not localize
    ptLibrary: Result := '.dll';        // do not localize
    ptProgram: Result := '.exe';        // do not localize
    else
      raise Exception.Create('Invalid project type');
  end;
end;

function ProjectTypeToSourceExtension(AProjectType: TProjectType): string;
begin
  case AProjectType of
    ptPackageRun,
    ptPackageDesign,
    ptPackage: Result := '.dpk';        // do not localize
    ptLibrary: Result := '.dpr';        // do not localize
    ptProgram: Result := '.dpr';        // do not localize
    else
      raise Exception.Create('Invalid project type');
  end;
end;

function ProjectTypeIsDLL(AProjectType: TProjectType): Boolean;
begin
  Result := AProjectType = ptLibrary;
end;

function ProjectTypeIsPackage(AProjectType: TProjectType): Boolean;
begin
  Result := AProjectType in [ptPackage, ptPackageRun, ptPackageDesign];
end;

function ProjectTypeToProjectName(ProjectType: TProjectType): string;
begin
  case ProjectType of
    ptPackageRun,
    ptPackageDesign,
    ptPackage:
      Result := 'Package';       // do not localize
    ptLibrary:
      Result := 'Library';       // do not localize
    ptProgram:
      Result := 'Program';       // do not localize
    else
      raise Exception.Create('Invalid project type');
  end;
end;

function PlatformNameToPlatformType(const APlatformName: string): TPlatformType;
begin
  if APlatformName = 'Win32' then
    Result := pftWin32
  else
  if APlatformName = 'Win64' then
    Result := pftWin64
  else
  if APlatformName = 'Win64x' then
    Result := pftWin64x
  else
    raise Exception.Create('Invalid platform type');
end;

{ TPackageXmlInfoItem }

constructor TPackageXmlInfoItem.Create(const AName, ATargets, ACondition: string);
begin
  inherited Create;
  FName := AName;
  FTargets := TStringList.Create;
  FTargets.Duplicates := dupIgnore;
  FTargets.CommaText := ATargets;
  ExpandTargets(FTargets);
  FTargets.Sorted := True; // sort the targets
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
  Result := FTargets.Find(TargetSymbol, Index);
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

{ TPlatform }

constructor TPlatform.Create(const AName, ATargets, ACondition: string);
begin
  inherited Create(AName, ATargets, ACondition);
  FPlatformType := PlatformNameToPlatformType(AName);
end;

function TPlatform.IsSupportedByTarget(const TargetSymbol: string): Boolean;
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
  FPlatforms := TObjectList.Create;
  // FProjectType is updated in LoadFromFile
  try
    if (Length(Name) > 1) and (Name[Length(Name)-1] = '-') then  // do not localize
      FProjectType := CharToProjectType(Name[Length(Name)])
    else
      FProjectType := ptPackageRun;
  except
    FProjectType := ptPackageRun;
  end;

  FProperties := TStringList.Create;

  LoadFromFile(FFilename);
end;

destructor TPackageXmlInfo.Destroy;
begin
  FCompilerDefines.Free;
  FC6Libs.Free;
  FProperties.Free;
  FRequires.Free;
  FContains.Free;
  FPlatforms.Free;
  inherited Destroy;
end;

function TPackageXmlInfo.GetBuildNumber: string;
begin
  Result := FProperties.Values[BuildNumberKnownPackageProperty];
end;

function TPackageXmlInfo.GetC6Libs: TStrings;
begin
  if not Assigned(FC6Libs) then
    FC6Libs := TStringList.Create;

  StrToStrings(FProperties.Values[C6LibsKnownPackageProperty], ' ', FC6Libs, False);

  Result := FC6Libs;
end;

function TPackageXmlInfo.GetC6PFlags: string;
begin
  Result := FProperties.Values[C6PFlagsKnownPackageProperty];
end;

function TPackageXmlInfo.GetCompilerDefines: TStrings;
begin
  if not Assigned(FCompilerDefines) then
    FCompilerDefines := TStringList.Create;

  StrToStrings(FProperties.Values[CompilerDefinesKnownPackageProperty], ' ', FCompilerDefines, False);

  Result := FCompilerDefines;
end;

function TPackageXmlInfo.GetContainCount: Integer;
begin
  Result := FContains.Count;
end;

function TPackageXmlInfo.GetContains(Index: Integer): TContainedFile;
begin
  Result := TContainedFile(FContains[Index]);
end;

function TPackageXmlInfo.GetImageBase: string;
begin
  Result := FProperties.Values[ImageBaseKnownPackageProperty];
end;

function TPackageXmlInfo.GetPlatformCount: Integer;
begin
  Result := FPlatforms.Count;
end;

function TPackageXmlInfo.GetPlatforms(Index: Integer): TPlatform;
begin
  Result := TPlatform(FPlatforms[Index]);
end;

function TPackageXmlInfo.GetPlatformTypes: TPlatformTypes;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to PlatformCount - 1 do
    Include(Result, Platforms[I].PlatformType);
end;

function TPackageXmlInfo.GetReleaseNumber: string;
begin
  Result := FProperties.Values[ReleaseNumberKnownPackageProperty];
end;

function TPackageXmlInfo.GetRequireCount: Integer;
begin
  Result := FRequires.Count;
end;

function TPackageXmlInfo.GetRequires(Index: Integer): TRequiredPackage;
begin
  Result := TRequiredPackage(FRequires[Index]);
end;

function TPackageXmlInfo.GetVersionMajorNumber: string;
begin
  Result := FProperties.Values[VersionMajorNumberKnownPackageProperty];
end;

function TPackageXmlInfo.GetVersionMinorNumber: string;
begin
  Result := FProperties.Values[VersionMinorNumberKnownPackageProperty];
end;

procedure TPackageXmlInfo.LoadFromFile(const Filename: string);
var
  i: Integer;
  RequirePkgName, RequireTarget,
  ContainsFileName, FormName, Condition, PlatformName: string;
  xml: TJclSimpleXML;
  RootNode : TJclSimpleXmlElemClassic;
  PropertiesNode, PropertyNode: TJclSimpleXmlElem;
  RequiredNode: TJclSimpleXmlElem;
  PackageNode: TJclSimpleXmlElem;
  ContainsNode: TJclSimpleXmlElem;
  FileNode: TJclSimpleXmlElem;
  PlatformsNode, PlatformNode: TJclSimpleXmlElem;
begin
  FRequires.Clear;
  FRequiresDB := False;
  FContains.Clear;
  FPlatforms.Clear;

  xml := TJclSimpleXML.Create;
  try
    xml.LoadFromFile(Filename);
    RootNode := xml.Root;

    FGUID := RootNode.Items.Value('GUID');

    for i := Low(KnownPackageProperties) to High(KnownPackageProperties) do
      if Assigned(RootNode.Items.ItemNamed[KnownPackageProperties[i]]) then
        FProperties.Values[KnownPackageProperties[i]] := RootNode.Items.ItemNamed[KnownPackageProperties[i]].Value;

    PropertiesNode := RootNode.Items.ItemNamed['Properties'];
    if Assigned(PropertiesNode) then
      for i := 0 to PropertiesNode.Items.Count - 1 do
    begin
      PropertyNode := PropertiesNode.Items.Item[i];
      FProperties.Values[PropertyNode.Properties.ItemNamed['Name'].Value] := PropertyNode.Properties.ItemNamed['Value'].Value;
    end;

    RequiredNode := RootNode.Items.ItemNamed['Requires'];               // do not localize
    ContainsNode := RootNode.Items.ItemNamed['Contains'];               // do not localize
    PlatformsNode := RootNode.Items.ItemNamed['Platforms'];             // do not localize

    FDisplayName := RootNode.Properties.Value('Name');                  // do not localize

    xml.Options := xml.Options - [sxoAutoCreate];
    if Assigned(RootNode.Properties.ItemNamed['Design']) then           // do not localize
      if RootNode.Properties.ItemNamed['Design'].BoolValue then         // do not localize
        FProjectType := ptPackageDesign
      else
        FProjectType := ptPackageRun
    else
      FProjectType := CharToProjectType(RootNode.Properties.Value('Type', '')[1]); // do not localize
    xml.Options := xml.Options + [sxoAutoCreate];

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

   // platforms
    if Assigned(PlatformsNode) then
      for I := 0 to PlatformsNode.Items.Count - 1 do
      begin
        PlatformNode := PlatformsNode.Items[I];
        PlatformName := PlatformNode.Properties.ItemNamed['Name'].Value;  // do not localize

        RequireTarget := PlatformNode.Properties.Value('Targets');        // do not localize
        if RequireTarget = '' then
          RequireTarget := 'all';                                         // do not localize

        Condition := PlatformNode.Properties.Value('Condition');          // do not localize

       // add new platform item
        FPlatforms.Add(TPlatform.Create(PlatformName, RequireTarget, Condition));
      end;
  finally
    xml.Free;
  end;
end;

{ TPackageGroup }

constructor TPackageGroup.Create(const AFilename, APackagesXmlDir, ATargetSymbol: string; ATargetPlatform: TCompileTargetPlatform);
begin
  inherited Create;

  FPackagesXmlDir := APackagesXmlDir;
  if (FPackagesXmlDir <> '') and (FPackagesXmlDir[Length(FPackagesXmlDir)] = PathDelim) then
    Delete(FPackagesXmlDir, Length(FPackagesXmlDir), 1);

  FTargetSymbol := ATargetSymbol;
  FTargetPlatform := ATargetPlatform;
  FFilename := AFilename;
  FPackageXmlDirFileNames := TStringList.Create;

  FPackages := TObjectList.Create(Filename <> '');
  if Filename <> '' then
    LoadFile;
end;

destructor TPackageGroup.Destroy;
begin
  FPackageXmlDirFileNames.Free;
  FPackages.Free;
  inherited Destroy;
end;

function TPackageGroup.Add(const TargetName, SourceName: string): TBpgPackageTarget;
var
  GenericPrefix: string;
begin
  Result := nil;
  GenericPrefix := {PackagesXmlDir + PathDelim +} BplNameToGenericName(TargetName);
  if XmlFileExists(GenericPrefix + '-R.xml') or XmlFileExists(GenericPrefix + '-D.xml') then // do not localize
  begin
    try
      Result := GetPackageTargetClass.Create(Self, TargetName, SourceName);

      // IDE is only Win32 so there can't be any design package if it's not Win32
      if (FTargetPlatform <> ctpWin32) and ProjectTypeIsDesign(Result.Info.ProjectType) then
        FreeAndNil(Result);
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
    if p1.Info.ProjectType > p2.Info.ProjectType then
      Result := 1
    else if p1.Info.ProjectType < p2.Info.ProjectType then
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
  FPackageXmlDirFileNames.Clear;
  BuildFileList(PackagesXmlDir + '\*.xml', faAnyFile, FPackageXmlDirFileNames, False);
  FPackageXmlDirFileNames.Sorted := True;

  if CompareText(ExtractFileExt(FileName), '.groupproj') = 0 then
    LoadGroupProjFile
  else
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
  xml: TJclSimpleXML;
  Options, Projects: TJclSimpleXMLElem;
  i, OptIndex, PrjIndex: Integer;
  Personality: string;
  TgName: string;
begin
  xml := TJclSimpleXML.Create;
  try
    xml.LoadFromFile(Filename);

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

procedure TPackageGroup.LoadGroupProjFile;
var
  xml: TJclSimpleXML;
  CurItem, MsBuild: TJclSimpleXMLElem;
  NameProperty: TJclSimpleXMLProp;
  i: Integer;
  TgName, S: string;
  DpkFilename: string;
  Lines: TStrings;
  LineIndex: Integer;
begin
  xml := TJclSimpleXML.Create;
  try
    xml.LoadFromFile(Filename);

    for i := 0 to xml.Root.Items.Count - 1 do
    begin
      CurItem := xml.Root.Items[i];
      NameProperty := CurItem.Properties.ItemNamed['Name'];
      // Get all targets that have ':Make' in their name property
      if (CompareText(CurItem.Name, 'Target') = 0) and // <Target>
         (Assigned(NameProperty)) and (Pos(':MAKE', UpperCase(NameProperty.Value)) > 0) and
         (CurItem.Items.Count > 0) then
      begin
        TgName := Copy(NameProperty.Value, 1, Pos(':', NameProperty.Value) - 1);

        MsBuild := CurItem.Items.ItemNamed['MSBuild'];
        // change .dproj to .dpk and add the target
        DpkFilename := ChangeFileExt(MsBuild.Properties.ItemNamed['Projects'].Value, '.dpk');

        // The package could use $LIBSUFFIX. And that means that we must get the LIBSUFFIX
        //  from the dpk file.
        Lines := TStringList.Create;
        try
          Lines.LoadFromFile(ExtractFilePath(Filename) + DpkFilename);
          for LineIndex := 0 to Lines.Count - 1 do
          begin
            S := Trim(Lines[LineIndex]);
            if StartsText('{$LIBSUFFIX', S) or StartsText('(*LIBSUFFIX', S) then
            begin
              S := Copy(S, Pos('''', S) + 1, MaxInt);
              S := Copy(S, 1, Pos('''', S) - 1);
              TgName := TgName + S;
              Break;
            end;
          end;
        finally
          Lines.Free;
        end;

        TgName := TgName + '.bpl';
        Add(TgName, DpkFilename);
      end;
    end;
  finally
    xml.Free;
  end;
end;

function TPackageGroup.XmlFileExists(const XmlName: string): Boolean;
begin
  Result := FPackageXmlDirFileNames.IndexOf(XmlName) <> -1;
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
  FPlatformList := TList.Create;
end;

destructor TBpgPackageTarget.Destroy;
begin
  if AutoDeleteUserData then
    FUserData.Free;
  FRequireList.Free;
  FContaineList.Free;
  FPlatformList.Free;
  FInfo.Free;
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

function TBpgPackageTarget.GetPlatformCount: Integer;
begin
  UpdatePlatformList;
  Result := FPlatformList.Count;
end;

function TBpgPackageTarget.GetPlatforms(Index: Integer): TPlatform;
begin
  UpdatePlatformList;
  Result := TPlatform(FPlatformList[Index]);
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

procedure TBpgPackageTarget.UpdatePlatformList;
var
  I: Integer;
begin
  if FPlatformList.Count = 0 then
    for I := 0 to Info. PlatformCount - 1 do
      if Info.Platforms[I].IsSupportedByTarget(Owner.TargetSymbol) then
        FPlatformList.Add(Info.Platforms[I]);
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
end;

function TPackageInfo.GetBplName: string;
begin
  Result := Owner.TargetName;
end;

function TPackageInfo.GetContainCount: Integer;
begin
  Result := XmlInfo.ContainCount;
end;

function TPackageInfo.GetContains(Index: Integer): TContainedFile;
begin
  Result := XmlInfo.Contains[Index];
end;

function TPackageInfo.GetRequireCount: Integer;
begin
  Result := XmlInfo.RequireCount;
end;

function TPackageInfo.GetRequires(Index: Integer): TRequiredPackage;
begin
  Result := XmlInfo.Requires[Index];
end;

function TPackageInfo.GetDescription: string;
begin
  Result := XmlInfo.Description;
end;

function TPackageInfo.GetDisplayName: string;
begin
  Result := XmlInfo.DisplayName;
end;

function TPackageInfo.GetPlatformCount: Integer;
begin
  Result := XmlInfo.PlatformCount;
end;

function TPackageInfo.GetPlatforms(Index: Integer): TPlatform;
begin
  Result := XmlInfo.Platforms[Index];
end;

function TPackageInfo.GetProjectType: TProjectType;
begin
  Result := XmlInfo.ProjectType;
end;

function TPackageInfo.GetName: string;
begin
  Result := XmlInfo.Name;
end;

function TPackageInfo.GetRequiresDB: Boolean;
begin
  Result := XmlInfo.RequiresDB;
end;

function TPackageInfo.GetXmlInfo: TPackageXmlInfo;
begin
  Result := GetPackageXmlInfo(Owner.TargetName, XmlDir);
end;

initialization
  XmlFileCache := TStringList.Create;
  XmlFileCache.Sorted := True;

finalization
  ClearXmlFileCache;
  XmlFileCache.Free;

end.
