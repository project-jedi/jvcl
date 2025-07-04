{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: DelphiData.pas, released on 2004-03-29.

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

unit DelphiData;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, SysUtils, Classes, Contnrs, Registry, PackageInformation,
  JclSimpleXml;

const
  BDSVersions: array[1..23] of record
                                Name: string;
                                VersionStr: string;
                                Version: Integer;
                                CIV: string; // coreide version
                                ProjectDirResId: Integer;
                                Supported: Boolean;
                              end = (
    (Name: 'C#Builder'; VersionStr: '1.0'; Version: 1; CIV: '71'; ProjectDirResId: 64507; Supported: False),
    (Name: 'Delphi'; VersionStr: '8'; Version: 8; CIV: '71'; ProjectDirResId: 64460; Supported: False),
    (Name: 'Delphi'; VersionStr: '2005'; Version: 9; CIV: '90'; ProjectDirResId: 64431; Supported: True),
    (Name: 'Borland Developer Studio'; VersionStr: '2006'; Version: 10; CIV: '100'; Supported: True),
    (Name: 'CodeGear RAD Studio'; VersionStr: '2007'; Version: 11; CIV: '100'; Supported: True),
    (Name: 'CodeGear RAD Studio'; VersionStr: '2009'; Version: 12; CIV: '120'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: '2010'; Version: 14; CIV: '140'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: 'XE'; Version: 15; CIV: '150'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: 'XE2'; Version: 16; CIV: '160'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: 'XE3'; Version: 17; CIV: '170'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: 'XE4'; Version: 18; CIV: '180'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: 'XE5'; Version: 19; CIV: '190'; Supported: True),
    (Name: 'skipped'; VersionStr: 'skipped'; Version: 19; CIV: '190'; Supported: False),
    (Name: 'Embarcadero RAD Studio'; VersionStr: 'XE6'; Version: 20; CIV: '200'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: 'XE7'; Version: 21; CIV: '210'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: 'XE8'; Version: 22; CIV: '220'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: '10'; Version: 23; CIV: '230'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: '10.1'; Version: 24; CIV: '240'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: '10.2'; Version: 25; CIV: '250'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: '10.3'; Version: 26; CIV: '260'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: '10.4'; Version: 27; CIV: '270'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: '11'; Version: 28; CIV: '280'; Supported: True),
    (Name: 'Embarcadero RAD Studio'; VersionStr: '12'; Version: 29; CIV: '290'; Supported: True)
  );

type
  TPersonalityType = (
    persDelphi,
    persBCB
  );
  TPersonalities = set of TPersonalityType;

const
  PersNames: array[TPersonalityType] of string = (
    'Delphi.Win32', // This is "Delphi" but Embarcadero couldn't rename it
    'BCB'
  );

type
  TCompileTarget = class;
  TCompileTargetList = class;
  TDelphiPackage = class;
  TDelphiPackageList = class;

  TCompileTargetList = class(TObjectList)
  private
    function GetItems(Index: Integer): TCompileTarget;
    procedure LoadTargets(const RootKey, SubKey, HKCUSubKey: string);
    function IsBDSSupported(const IDEVersionStr: string): Boolean;
  public
    constructor Create;
    property Items[Index: Integer]: TCompileTarget read GetItems; default;
  end;

  TEnvVarStringItem = class(TObject)
  private
    FValue: string;
  public
    property Value: string read FValue write FValue;
  end;

  TEnvVarStrings = class(TObject)
  private
    FItems: TStringList; // Objects[]: TEnvVarStringItem
    function GetCount: Integer;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    function GetValueByIndex(Index: Integer): string;
    procedure SetValue(const Name, Value: string);
    procedure SetValueByIndex(Index: Integer; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOfName(const AName: string): Integer;
    procedure Add(const AName, AValue: string);
    procedure Assign(ASource: TEnvVarStrings);
    procedure Clear;
    function Matches(AList: TEnvVarStrings): Boolean;

    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property ValuesByIndex[Index: Integer]: string read GetValueByIndex write SetValueByIndex;
    property Values[const Name: string]: string read GetValue write SetValue; default;
  end;

  TCompileTarget = class(TObject)
  private
    FIsValid: Boolean;
    FIsEvaluation: Boolean;
    FName: string;
    FIDEName: string;
    FPlatform: TCompileTargetPlatform;
    FLatestRTLPatch: Integer;
    FLatestUpdate: Integer;
    FIDEVersion: Integer;
    FIDEVersionStr: string;
    FVersion: Integer;
    FVersionStr: string;
    FExecutable: string;
    FEdition: string;
    FRootDir: string;
    FIsPersonal: Boolean;
    FProductVersion: string;
    FBrowsingPaths: TStringList;
    FDCPOutputDir: string;
    FBPLOutputDir: string;
    FPackageSearchPaths: TStringList;
    FSearchPaths: TStringList;
    FDisabledPackages32: TDelphiPackageList;
    FDisabledPackages64: TDelphiPackageList;
    FKnownPackages32: TDelphiPackageList;
    FKnownIDEPackages32: TDelphiPackageList;
    FKnownPackages64: TDelphiPackageList;
    FKnownIDEPackages64: TDelphiPackageList;
    FHKLMRegistryKey: string;
    FRegistryKey: string;
    FDebugDcuPaths: TStringList;
    FInstalledPersonalities: TStrings;
    FGlobalIncludePaths: TStringList;
    FGlobalCppBrowsingPaths: TStringList;
    FGlobalCppLibraryPaths: TStringList;
    FGlobalIncludePathsClang32: TStringList;
    FGlobalCppBrowsingPathsClang32: TStringList;
    FGlobalCppLibraryPathsClang32: TStringList;
    FGlobalIncludePathsWin64x: TStringList;
    FGlobalCppBrowsingPathsWin64x: TStringList;
    FGlobalCppLibraryPathsWin64x: TStringList;

    FOrgEnvVars: TEnvVarStrings;
    FEnvVars: TEnvVarStrings;
    FDefaultBDSProjectsDir: string;
    FCommonProjectsDir: string;

    function GetEnvPath: string;
    procedure SetEnvPath(const Value: string);
    function GetBDSProjectsDir: string;
    function GetCommonProjectsDir: string;
    procedure LoadFromRegistry;
    function GetEnvOptionsFileName: string; // Delphi 2007
    function GetPlatformStr: string;
    function ReadBDSProjectsDir: string;
    function ReadCommonProjectsDir: string;
    procedure LoadPackagesFromRegistry(APackageList: TDelphiPackageList;
      const SubKey: string);
    procedure SavePackagesToRegistry(APackageList: TDelphiPackageList;
      const SubKey: string);
    function GetHomepage: string;
    procedure GetBDSVersion(out Name: string; out Version: Integer; out VersionStr: string);
    function GetMake: string;
    function GetDcc32: string;
    function GetDcc64: string;
    function GetDccil: string;
    function GetBcc32: string;
    function GetBcc64: string;
    function GetBcc64x: string;
    function GetIlink32: string;
    function GetTlib: string;
    function GetBplDir: string;
    function GetDcpDir: string;
    function GetRootLibDir: string;
    function GetRootLibReleaseDir: string;
    function GetProjectDir: string;

    function GetEnvOptionsPropertyGroupNode(AEnvOptions: TJclSimpleXML; const APlatformStrSuffix: string): TJclSimpleXMLElem;
  protected
    property DCPOutputDir: string read FDCPOutputDir; // with macros, could contain double backslashes when resolving the macro
    property BPLOutputDir: string read FBPLOutputDir; // with macros, could contain double backslashes when resolving the macro
  public
    constructor Create(const AName, AVersion, ARegSubKey: string; APlatform: TCompileTargetPlatform);
    destructor Destroy; override;

    function IsBDS: Boolean;
    function IsBCB: Boolean;
    function IsDelphi: Boolean;
    function IsPersonal: Boolean;
    function DisplayName: string;
    function HasBDE: Boolean;
    function HasWin64x: Boolean;

    function IsInEnvPath(const Dir: string): Boolean;
      { IsInEnvPath returns True if Dir is in the EnvPath. (ShortPaths and
        LongPaths are tested) }

    function SupportedPersonalities: TPersonalities;
      { SupportedPersonalities returns all installed personalities for the
        target. Delphi/BCB < 9.0 return either [persDelphi] or [persBCB]. }
    function SupportsPersonalities(Personalities: TPersonalities; Exact: Boolean = False): Boolean;
      { SupportsPersonalities tests if the target supports all specified
        personalities if Exact=False. If Exact=True the target must exactly
        support all specified personalities, nothing less, nothing more. }

    function VersionedDCP(const Filename: string): string;
      { returns the filename + version + extension for Delphi 5 and BCB 5
        else it returns the Filename. }
    function VersionedBPL(const Filename: string): string;
      { returns the filename + version + extension. }
    function VersionedIDEBPL(const Filename: string): string;
      { returns the filename + version + extension. }

    function TargetType: string;
      { return 'D' for Delphi, 'C' for C++ and ?? for BDS }

    function FindPackage(const PackageName: string): TDelphiPackage;
    function FindPackageEx(const PackageNameStart: string): TDelphiPackage;
    function ExpandDirMacros(const Dir: string): string;
    function InsertDirMacros(const Dir: string): string;

    procedure SavePaths;
      { writes BrowsingPaths and SearchPaths to the registry }
    procedure SavePackagesLists;
      { writes KnownPackages and DisabledPackages to the registry }

    function GetDisabledPackages: TDelphiPackageList;
    function GetKnownPackages: TDelphiPackageList;
    function GetKnownIDEPackages: TDelphiPackageList;

    property Homepage: string read GetHomepage;
    property RegistryKey: string read FRegistryKey;
    property HKLMRegistryKey: string read FHKLMRegistryKey;
    property IsValid: Boolean read FIsValid; // is True if the installation is valid
    property IsEvaluation: Boolean read FIsEvaluation;

    property Make: string read GetMake;
    property Dcc32: string read GetDcc32;
    property Dcc64: string read GetDcc64;
    property Dccil: string read GetDccil;
    property Bcc32: string read GetBcc32;
    property Bcc64: string read GetBcc64;
    property Bcc64x: string read GetBcc64x;
    property Ilink32: string read GetIlink32;
    property Tlib: string read GetTlib;

    property Name: string read FName;
    property Version: Integer read FVersion;  // 1, 7, 10
    property VersionStr: string read FVersionStr; // '1.0', '7.0', '2006'
    property IDEName: string read FIDEName;
    property Platform: TCompileTargetPlatform read FPlatform;
    property PlatformName: string read GetPlatformStr;
    property IDEVersion: Integer read FIDEVersion; // 1, 7, 4
    property IDEVersionStr: string read FIDEVersionStr;
    property ProductVersion: string read FProductVersion;
    property Executable: string read FExecutable; // [Reg->App] x:\path\Delphi.exe
    property RootDir: string read FRootDir; // [Reg->RootDir] x:\path
    property RootLibReleaseDir: string read GetRootLibReleaseDir; // new: $(RootDir)\lib\$(Platform)\release, old: $(RootDir)\lib
    property Edition: string read FEdition; // [Reg->Version] PER/PRO/CSS
    property LatestUpdate: Integer read FLatestUpdate;
    property LatestRTLPatch: Integer read FLatestRTLPatch;
    property EnvPath: string read GetEnvPath write SetEnvPath;
    property EnvVars: TEnvVarStrings read FEnvVars;

    property BrowsingPaths: TStringList read FBrowsingPaths; // with macros
    property PackageSearchPathList: TStringList read FPackageSearchPaths; // with macros
    property SearchPaths: TStringList read FSearchPaths; // with macros
    property DebugDcuPaths: TStringList read FDebugDcuPaths; // with macros
    property GlobalIncludePaths: TStringList read FGlobalIncludePaths; // BDS only, with macros
    property GlobalCppBrowsingPaths: TStringList read FGlobalCppBrowsingPaths; // BDS only, with macros
    property GlobalCppLibraryPaths: TStringList read FGlobalCppLibraryPaths;  // BDS v5 and upper only, with macros
    property GlobalIncludePathsClang32: TStringList read FGlobalIncludePathsClang32; // BDS v23 and upper only, with macros
    property GlobalCppBrowsingPathsClang32: TStringList read FGlobalCppBrowsingPathsClang32; // BDS v23 and upper  only, with macros
    property GlobalCppLibraryPathsClang32: TStringList read FGlobalCppLibraryPathsClang32;  // BDS v23 and upper, with macros
    property GlobalIncludePathsWin64x: TStringList read FGlobalIncludePathsWin64x; // BDS v23 and upper only, with macros
    property GlobalCppBrowsingPathsWin64x: TStringList read FGlobalCppBrowsingPathsWin64x; // BDS v23 and upper  only, with macros
    property GlobalCppLibraryPathsWin64x: TStringList read FGlobalCppLibraryPathsWin64x;  // BDS v23 and upper, with macros

    property BDSProjectsDir: string read GetBDSProjectsDir;
    property CommonProjectsDir: string read GetCommonProjectsDir;
    property ProjectDir: string read GetProjectDir; // Delphi 5-7: RootDir\Projects BDS: BDSProjectDir\Projects
    property BplDir: string read GetBplDir; // macros are expanded
    property DcpDir: string read GetDcpDir; // macros are expanded

    property KnownIDEPackages32: TDelphiPackageList read FKnownIDEPackages32;
    property KnownPackages32: TDelphiPackageList read FKnownPackages32;
    property DisabledPackages32: TDelphiPackageList read FDisabledPackages32;
    property KnownIDEPackages64: TDelphiPackageList read FKnownIDEPackages64;
    property KnownPackages64: TDelphiPackageList read FKnownPackages64;
    property DisabledPackages64: TDelphiPackageList read FDisabledPackages64;
  end;

  TDelphiPackageList = class(TObjectList)
  private
    function GetItems(Index: Integer): TDelphiPackage;
  public
    function IndexOfFilename(const Filename: string): Integer;
    procedure Add(const Filename, Description: string);
    procedure Remove(const Filename: string);

    property Items[Index: Integer]: TDelphiPackage read GetItems; default;
  end;

  TDelphiPackage = class(TObject)
  private
    FFilename: string;
    FDescription: string;
    function GetName: string;
  public
    constructor Create(const AFilename, ADescription: string);

    property Name: string read GetName;
    property Filename: string read FFilename;
    property Description: string read FDescription;
  end;


procedure ConvertPathList(const Paths: string; List: TStrings); overload;
function ConvertPathList(List: TStrings): string; overload;

implementation

uses
  StrUtils,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  CmdLineUtils, Utils,
  JvConsts,
  JclBase, JclSysInfo, JclSysUtils, JclFileUtils, JclIDEUtils, JclStrings;

function DequoteStr(const S: string): string;
begin
  Result := S;
  if (Length(Result) > 1) and (Result[1] = '"') and (Result[Length(Result)] = '"') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

const
  KeyBorland = '\SOFTWARE\Borland\'; // do not localize
  KeyCodeGear = '\SOFTWARE\CodeGear\'; // do not localize
  KeyEmbarcadero = '\SOFTWARE\Embarcadero\'; // do not localize

var
  GlobalPathEnvVar: string;

function SubStr(const Text: string; StartIndex, EndIndex: Integer): string;
begin
  Result := Copy(Text, StartIndex, EndIndex - StartIndex + 1);
end;

procedure ConvertPathList(const Paths: string; List: TStrings); overload;
var
  F, P: PChar;
  S: string;
begin
  List.Clear;
  P := PChar(Paths);
  while (P[0] <> #0) do
  begin
   // trim
    while (P[0] = ' ') do
      Inc(P);
    if P[0] = #0 then
      Break;

    F := P;
    while (P[0] <> #0) and (P[0] <> ';') do
      Inc(P);
    SetString(S, F, P - F);
    List.Add(ExcludeTrailingPathDelimiter(Trim(DequoteStr(Trim(S)))));
    if P[0] = #0 then
      Break;
    Inc(P);
  end;
end;

function ConvertPathList(List: TStrings): string; overload;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
    Result := Result + List[I] + ';';
  SetLength(Result, Length(Result) - 1);
end;

function LoadResStrings(const BaseBinName: string; const ResId: array of Integer): string;
var
  H: HMODULE;
  LocaleName: array [0..4] of Char;
  FileName, ResValue: string;
  Index: Integer;
begin
  Result := '';

  FileName := BaseBinName;
  if not FileExists(FileName) then
  begin
    FillChar(LocaleName, SizeOf(LocaleName[0]), 0);
    GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
    if LocaleName[0] <> #0 then
    begin
      if FileExists(FileName + LocaleName) then
        FileName := FileName + LocaleName
      else
      begin
        LocaleName[2] := #0;
        if FileExists(FileName + LocaleName) then
          FileName := FileName + LocaleName
        else
          FileName := '';
      end;
    end;
  end;

  if FileName <> '' then
  begin
    H := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
    if H <> 0 then
    try
      for Index := Low(ResId) to High(ResId) do
      begin
        SetLength(ResValue, 1024);
        SetLength(ResValue, LoadString(H, ResId[Index], PChar(ResValue), Length(ResValue) - 1));
        if Result <> '' then
          Result := AppendPath(Result, ResValue)
        else
          Result := ResValue;
      end;
    finally
      FreeLibrary(H);
    end;
  end;
end;

{ TCompileTargetList }

function SortTargetsByVersionNumber(Item1, Item2: Pointer): Integer;
begin
  Result := TCompileTarget(Item1).Version - TCompileTarget(Item2).Version;
end;

constructor TCompileTargetList.Create;
begin
  inherited Create;
  if CmdOptions.RegistryKeyDelphi = '' then
    CmdOptions.RegistryKeyDelphi := 'Delphi'; // do not localize
  if CmdOptions.RegistryKeyBCB = '' then
    CmdOptions.RegistryKeyBCB := 'C++Builder'; // do not localize
  if CmdOptions.RegistryKeyBDS = '' then
    CmdOptions.RegistryKeyBDS := 'BDS'; // do not localize

  if not CmdOptions.IgnoreDelphi then
    LoadTargets(KeyBorland, 'Delphi', CmdOptions.RegistryKeyDelphi); // do not localize
  if not CmdOptions.IgnoreBCB then
    LoadTargets(KeyBorland, 'C++Builder', CmdOptions.RegistryKeyBCB); // do not localize
  if not CmdOptions.IgnoreDelphi then
  begin
    LoadTargets(KeyBorland, 'BDS', CmdOptions.RegistryKeyBDS); // do not localize
    LoadTargets(KeyCodeGear, 'BDS', CmdOptions.RegistryKeyBDS); // do not localize
    LoadTargets(KeyEmbarcadero, 'BDS', CmdOptions.RegistryKeyBDS); // do not localize
  end;
  Sort(SortTargetsByVersionNumber);
end;

function TCompileTargetList.GetItems(Index: Integer): TCompileTarget;
begin
  Result := TCompileTarget(inherited Items[Index]);
end;

procedure TCompileTargetList.LoadTargets(const RootKey, SubKey, HKCUSubKey: string);
var
  Reg, HKCUReg: TRegistry;
  List: TStrings;
  I: Integer;
  Target: TCompileTarget;
  KeyName: string;
begin
  Reg := TRegistry.Create;
  HKCUReg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    HKCUReg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(RootKey + SubKey) then
    begin
      List := TStringList.Create;
      try
        Reg.GetKeyNames(List);
        for I := 0 to List.Count - 1 do
        begin
          KeyName := List[I];
          if (KeyName <> '') and CharInSet(KeyName[1], ['1'..'9']) then // only version numbers (not "BDS\DBExpress")
            if (SubKey <> 'BDS') or IsBDSSupported(KeyName) then // do not localize
            begin
              // Any Delphi under Delphi 6 is not supported
              if not ((SubKey <> 'BDS') and (KeyName[1] <= '5')) then
              begin
                if HKCUReg.KeyExists(RootKey + HKCUSubKey + '\' + KeyName) then
                begin
                  Target := TCompileTarget.Create(SubKey, KeyName, HKCUSubKey, ctpWin32);
                  if Target.IsValid then // only valid targets are allowed
                    Add(Target)
                  else
                    Target.Free;

                  if (SubKey = 'BDS') and (StrToIntDef(Copy(KeyName, 1, Pos('.', KeyName) - 1), -1) >= 9) then
                  begin
                    Target := TCompileTarget.Create(SubKey, KeyName, HKCUSubKey, ctpWin64);
                    if Target.IsValid then // only valid targets are allowed
                      Add(Target)
                    else
                      Target.Free;
                  end;
                end;
              end;
            end;
        end;
      finally
        List.Free;
      end;
    end;
  finally
    HKCUReg.Free;
    Reg.Free;
  end;
end;

function TCompileTargetList.IsBDSSupported(const IDEVersionStr: string): Boolean;
var
  P, IDEVersion: Integer;
begin
  Result := False;
  P := Pos('.', IDEVersionStr);
  if P > 0 then
    IDEVersion := StrToInt(Copy(IDEVersionStr, 1, P - 1))
  else
    IDEVersion := StrToInt(IDEVersionStr[1]);
  if (IDEVersion >= Low(BDSVersions)) and (IDEVersion <= High(BDSVersions)) then
    Result := BDSVersions[IDEVersion].Supported;
end;

{ TCompileTarget }

constructor TCompileTarget.Create(const AName, AVersion, ARegSubKey: string; APlatform: TCompileTargetPlatform);
begin
  inherited Create;

  if GlobalPathEnvVar = '' then
    GlobalPathEnvVar := GetEnvironmentVariable('PATH');

  FInstalledPersonalities := TStringList.Create;
  FIDEName := AName;
  FPlatform := APlatform;
  FIDEVersionStr := AVersion;
  FIDEVersion := StrToIntDef(Copy(FIDEVersionStr, 1, Pos('.', FIDEVersionStr) - 1), 0);
  if not IsBDS then
  begin
    FName := FIDEName;
    FVersion := FIDEVersion;
    FVersionStr := FIDEVersionStr;
  end
  else
    GetBDSVersion(FName, FVersion, FVersionStr);

  if IsBDS and (IDEVersion >= 8) then
  begin
    FHKLMRegistryKey := KeyEmbarcadero + IDEName + '\' + IDEVersionStr;
    FRegistryKey := KeyEmbarcadero + ARegSubKey + '\' + IDEVersionStr;
  end
  else
  if IsBDS and (IDEVersion >= 6) then
  begin
    FHKLMRegistryKey := KeyCodeGear + IDEName + '\' + IDEVersionStr;
    FRegistryKey := KeyCodeGear + ARegSubKey + '\' + IDEVersionStr;
  end
  else
  begin
    FHKLMRegistryKey := KeyBorland + IDEName + '\' + IDEVersionStr;
    FRegistryKey := KeyBorland + ARegSubKey + '\' + IDEVersionStr;
  end;

  FOrgEnvVars := TEnvVarStrings.Create;
  FEnvVars := TEnvVarStrings.Create;

  FBrowsingPaths := TStringList.Create;
  FPackageSearchPaths := TStringList.Create;
  FSearchPaths := TStringList.Create;
  FDebugDcuPaths := TStringList.Create;
  FGlobalIncludePaths := TStringList.Create;
  FGlobalCppBrowsingPaths := TStringList.Create;
  FGlobalCppLibraryPaths := TStringList.Create;
  FGlobalIncludePathsClang32 := TStringList.Create;
  FGlobalCppBrowsingPathsClang32 := TStringList.Create;
  FGlobalCppLibraryPathsClang32 := TStringList.Create;
  FGlobalIncludePathsWin64x := TStringList.Create;
  FGlobalCppBrowsingPathsWin64x := TStringList.Create;
  FGlobalCppLibraryPathsWin64x := TStringList.Create;

  FBrowsingPaths.Duplicates := dupIgnore;
  FPackageSearchPaths.Duplicates := dupIgnore;
  FSearchPaths.Duplicates := dupIgnore;
  FDebugDcuPaths.Duplicates := dupIgnore;
  FGlobalIncludePaths.Duplicates := dupIgnore;
  FGlobalCppBrowsingPaths.Duplicates := dupIgnore;
  FGlobalCppLibraryPaths.Duplicates := dupIgnore;
  FGlobalIncludePathsClang32.Duplicates := dupIgnore;
  FGlobalCppBrowsingPathsClang32.Duplicates := dupIgnore;
  FGlobalCppLibraryPathsClang32.Duplicates := dupIgnore;
  FGlobalIncludePathsWin64x.Duplicates := dupIgnore;
  FGlobalCppBrowsingPathsWin64x.Duplicates := dupIgnore;
  FGlobalCppLibraryPathsWin64x.Duplicates := dupIgnore;

  FDisabledPackages32 := TDelphiPackageList.Create;
  FKnownIDEPackages32 := TDelphiPackageList.Create;
  FKnownPackages32 := TDelphiPackageList.Create;
  FDisabledPackages64 := TDelphiPackageList.Create;
  FKnownIDEPackages64 := TDelphiPackageList.Create;
  FKnownPackages64 := TDelphiPackageList.Create;

  LoadFromRegistry;
  FIsEvaluation := not FileExists(RootDir + '\bin\dcc32.exe');
end;

destructor TCompileTarget.Destroy;
begin
  FOrgEnvVars.Free;
  FEnvVars.Free;

  FBrowsingPaths.Free;
  FPackageSearchPaths.Free;
  FSearchPaths.Free;
  FDebugDcuPaths.Free;
  FGlobalIncludePaths.Free;
  FGlobalCppBrowsingPaths.Free;
  FGlobalCppLibraryPaths.Free;
  FGlobalIncludePathsClang32.Free;
  FGlobalCppBrowsingPathsClang32.Free;
  FGlobalCppLibraryPathsClang32.Free;
  FGlobalIncludePathsWin64x.Free;
  FGlobalCppBrowsingPathsWin64x.Free;
  FGlobalCppLibraryPathsWin64x.Free;

  FDisabledPackages32.Free;
  FKnownIDEPackages32.Free;
  FKnownPackages32.Free;
  FDisabledPackages64.Free;
  FKnownIDEPackages64.Free;
  FKnownPackages64.Free;

  FInstalledPersonalities.Free;
  inherited Destroy;
end;

function TCompileTarget.DisplayName: string;
begin
  if IsBDS and (IDEVersion >= 9) then
    Result := Format('%s %s %s (%s)', [Name, VersionStr, GetPlatformStr, Edition]) // do not localize
  else
    Result := Format('%s %s (%s)', [Name, VersionStr, Edition]); // do not localize
end;

function TCompileTarget.ExpandDirMacros(const Dir: string): string;
var
  I, EndPs: Integer;
  S, NewS: string;
begin
  Result := Dir;
  I := 1;
  while I < Length(Result) do
  begin
    if (Result[I] = '$') and (Result[I + 1] = '(') then
    begin
      EndPs := I + 2;
      while (EndPs <= Length(Result)) and (Result[EndPs] <> ')') do
        Inc(EndPs);
      S := AnsiLowerCase(SubStr(Result, I + 2, EndPs - 1));
      NewS := S;

     // available macros
      if (S = 'delphi') or (S = 'bcb') or (S = 'bds') then // do not localize
        NewS := FRootDir
      else if S = 'bdslib' then // don't trust the env-var for this as it may be the wrong version
        NewS := FRootDir + '\lib'
      else if IsBDS and (S = 'bdsprojectsdir') then // do not localize
        NewS := BDSProjectsDir
      else if IsBDS and (IDEVersion >= 5) and (S = 'bdscommondir') then
        NewS := CommonProjectsDir
      else if IsBDS and (IDEVersion >= 9) and (S = 'platform') then
        NewS := GetPlatformStr
      else
      begin
        if EnvVars.IndexOfName(S) >= 0 then
          NewS := FixBackslashBackslash(EnvVars.Values[S])
        else
          NewS := FixBackslashBackslash(GetEnvironmentVariable(S));
      end;

      if NewS <> S then
      begin
        Delete(Result, i, EndPs - I + 1);
        Insert(NewS, Result, I);
        Inc(I, Length(NewS) - 1);
        NewS := '';
      end;
    end;
    Inc(I);
  end;
end;

function TCompileTarget.InsertDirMacros(const Dir: string): string;
begin
  Result := Dir;
  if AnsiStartsText(RootDir + PathDelim, Dir) then
  begin
    if IsBDS then
      Result := '$(BDS)' // do not localize
    else
    if IsDelphi then
      Result := '$(DELPHI)' // do not localize
    else
    if IsBCB then
      Result := '$(BCB)' // do not localize
    else
      Result := RootDir;

    Result := Result + Copy(Dir, Length(RootDir) + 1, MaxInt);
  end
  else if IsBDS and AnsiStartsText(BDSProjectsDir + PathDelim, Dir) then
  begin
    Result := '$(BDSPROJECTSDIR)'; // do not localize
    Result := Result + Copy(Dir, Length(BDSProjectsDir) + 1, MaxInt);
  end
  else if IsBDS and (IDEVersion >= 5) and AnsiStartsText(CommonProjectsDir + PathDelim, Dir) then
  begin
    Result := '$(BDSCOMMONDIR)';
    Result := Result + Copy(Dir, Length(CommonProjectsDir) + 1, MaxInt);
  end;
end;

function TCompileTarget.FindPackage(const PackageName: string): TDelphiPackage;

  function Find(List: TDelphiPackageList): TDelphiPackage;
  var
    i: Integer;
  begin
    for i := 0 to List.Count - 1 do
      if CompareText(PackageName, List[i].Name) = 0 then
      begin
        Result := List[i];
        Exit;
      end;
    Result := nil;
  end;

begin
  Result := Find(GetKnownIDEPackages);
  if Result = nil then
    Result := Find(GetKnownPackages);
end;

function TCompileTarget.FindPackageEx(const PackageNameStart: string): TDelphiPackage;

  function Find(List: TDelphiPackageList): TDelphiPackage;
  var
    i: Integer;
  begin
    for i := 0 to List.Count - 1 do
      if AnsiStartsText(PackageNameStart, List[i].Name) then
      begin
        Result := List[i];
        Exit;
      end;
    Result := nil;
  end;

begin
  Result := Find(GetKnownIDEPackages);
  if Result = nil then
    Result := Find(GetKnownPackages);
end;

function TCompileTarget.IsBCB: Boolean;
begin
  Result := (AnsiPos(AnsiUppercase('Builder'), AnsiUppercase(Name)) > 0);
end;

function TCompileTarget.IsDelphi: Boolean;
begin
  Result := (AnsiPos(AnsiUppercase('Delphi'), AnsiUppercase(Name)) > 0);
end;

function TCompileTarget.IsInEnvPath(const Dir: string): Boolean;
var
  ShortDir, Path: string;
begin
  if (Dir <> '') and (EnvPath <> '') then
  begin
    Path := ';' + AnsiLowerCase(EnvPath) + ';';
    Result := True;
    if Pos(';' + AnsiLowerCase(Dir) + ';', Path) > 0 then
      Exit;
    if Pos(';' + AnsiLowerCase(Dir) + '\;', Path) > 0 then
      Exit;
    ShortDir := AnsiLowerCase(ExtractShortPathName(Dir));
    if ShortDir <> '' then
    begin
      if Pos(';' + ShortDir + ';', Path) > 0 then
        Exit;
      if Pos(';' + ShortDir + '\;', Path) > 0 then
        Exit;
    end;
  end;
  Result := False;
end;

function TCompileTarget.IsBDS: Boolean;
begin
  Result := CompareText(IDEName, 'BDS') = 0;
end;

function TCompileTarget.IsPersonal: Boolean;
begin
  Result := FIsPersonal;
end;

procedure LoadCppPaths(APropertyGroupNode: TJclSimpleXMLElem; AIncludePaths, ABrowsingPaths, ALibraryPaths: TStrings; const AItemNameSuffix: string); overload;
var
  PropertyNode: TJclSimpleXMLElem;
begin
  PropertyNode := APropertyGroupNode.Items.ItemNamed['CBuilderIncludePath' + AItemNameSuffix]; // do not localize
  if Assigned(PropertyNode) then
    ConvertPathList(PropertyNode.Value, AIncludePaths); // do not localize

  PropertyNode := APropertyGroupNode.Items.ItemNamed['CBuilderBrowsingPath' + AItemNameSuffix]; // do not localize
  if Assigned(PropertyNode) then
    ConvertPathList(PropertyNode.Value, ABrowsingPaths); // do not localize

  PropertyNode := APropertyGroupNode.Items.ItemNamed['CBuilderLibraryPath' + AItemNameSuffix]; // do not localize
  if Assigned(PropertyNode) then
    ConvertPathList(PropertyNode.Value, ALibraryPaths); // do not localize
end;

procedure LoadCppPaths(AReg: TRegistry; AIncludePaths, ABrowsingPaths, ALibraryPaths: TStrings; const AItemNameSuffix: string); overload;
begin
  if AIncludePaths.Count = 0 then
    ConvertPathList(AReg.ReadString('IncludePath' + AItemNameSuffix), AIncludePaths); // do not localize
  if ABrowsingPaths.Count = 0 then
    ConvertPathList(AReg.ReadString('BrowsingPath' + AItemNameSuffix), ABrowsingPaths); // do not localize
  if ALibraryPaths.Count = 0 then
    ConvertPathList(AReg.ReadString('LibraryPath' + AItemNameSuffix), ALibraryPaths); // do not localize
end;

procedure TCompileTarget.LoadFromRegistry;

  procedure ReadUpdateState(Reg: TRegistry);
  var
    List: TStrings;
    i: Integer;
  begin
    if IsBDS then
    begin
      if Reg.ValueExists('UpdatePackInstalled') then
        FLatestUpdate := StrToIntDef(Reg.ReadString('UpdatePackInstalled'), 0);
    end
    else
    begin
      // obtain updates state
      List := TStringList.Create;
      try
        Reg.GetValueNames(List);
        for i := 1 to 10 do
        begin
          if Reg.ValueExists('Update #' + IntToStr(i)) then // do not localize
            FLatestUpdate := i;
          if i = 1 then
          begin
            if Reg.ValueExists('Pascal RTL Patch') then // do not localize
              FLatestRTLPatch := i;
          end
          else
            if Reg.ValueExists('Pascal RTL Patch #' + IntToStr(i)) then // do not localize
              FLatestRTLPatch := i;
        end;
      finally
        List.Free;
      end;
    end;
  end;

const
  BDS19UpCatalogRepositoryElementsSubKey = '\CatalogRepository\Elements';   // do not localize
  BDS19UpCoreCommonFilesPrefix = 'Core_Common_Files_';   // do not localize

var
  Reg: TRegistry;
  i: Integer;
  EnvOptions: TJclSimpleXml;
  PropertyGroupNode, PropertyNode: TJclSimpleXMLElem;
  ForceEnvOptionsUpdate: Boolean;
  LibraryKey: string;
  ValueInfo: TRegDataInfo;
  EnvVarNames: TStrings;
  KeyNames: TStrings;
  CandidateEdition: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKeyReadOnly(HKLMRegistryKey) then
    begin
      if Reg.ValueExists('Edition') then // do not localize
        FEdition := Reg.ReadString('Edition') // do not localize
      else
      if Reg.ValueExists('Version') then // do not localize
        FEdition := Reg.ReadString('Version') // do not localize
      else if IsBDS and (IDEVersion = 5) then
        FEdition := ''
      else
        //FEdition := 'Pers'; // do not localize
        FEdition := '';

      if Reg.ValueExists('App') then
        FExecutable := Reg.ReadString('App'); // do not localize
      if Reg.ValueExists('RootDir') then
        FRootDir := ExcludeTrailingPathDelimiter(Reg.ReadString('RootDir')); // do not localize
      if Reg.ValueExists('ProductVersion') then
        FProductVersion := Reg.ReadString('ProductVersion');

      ReadUpdateState(Reg);
      Reg.CloseKey;
    end;

    // Starting with Delphi 10.2, the Edition key is missing so we have to find the information somewhere else
    if IsBDS and (IDEVersion >= 19) and Reg.OpenKeyReadOnly(HKLMRegistryKey + BDS19UpCatalogRepositoryElementsSubKey) then
    begin
      KeyNames := TStringList.Create;
      try
        Reg.GetKeyNames(KeyNames);
        for i := 0 to KeyNames.Count - 1 do
          if StrHasPrefix(KeyNames[i], [BDS19UpCoreCommonFilesPrefix]) then
          begin
            CandidateEdition := Copy(KeyNames[i], Length(BDS19UpCoreCommonFilesPrefix) + 1, MaxInt);
            if Pos('_', CandidateEdition) = 0 then
            begin
              FEdition := Copy(CandidateEdition, 1, Pos('-', CandidateEdition) - 1);
              Break;
            end;
          end;
      finally
        KeyNames.Free;
      end;
    end;

    FIsValid := (RootDir <> '') and (Executable <> '') and FileExists(Executable);

    // If we are not valid, no need to go any further as reading the rest of the values
    // might try to access executable or batch files that are not here.
    if not IsValid then
      Exit;

    Reg.Free;
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CURRENT_USER;

    if IsBDS then
    begin
      FDefaultBDSProjectsDir := ReadBDSProjectsDir; // reads from COREIDExx.XX's resource strings
      FCommonProjectsDir := ReadCommonProjectsDir;
    end;

    // read special environnment variables and their overwrite
    if Reg.OpenKeyReadOnly(RegistryKey + '\Environment Variables') then // do not localize
    begin
      EnvVarNames := TStringList.Create;
      try
        Reg.GetValueNames(EnvVarNames);
        for i := 0 to EnvVarNames.Count - 1 do
          FOrgEnvVars.Add(EnvVarNames[i], Reg.ReadString(EnvVarNames[i]));
      finally
        EnvVarNames.Free;
      end;
      FEnvVars.Assign(FOrgEnvVars);
      Reg.CloseKey;
    end;

    {if Reg.OpenKeyReadOnly(RegistryKey) then
    begin
      ReadUpdateState(Reg);
      Reg.CloseKey;
    end;}

    FDCPOutputDir := '';
    FBPLOutputDir := '';
    FBrowsingPaths.Clear;
    FPackageSearchPaths.Clear;
    FSearchPaths.Clear;
    FDebugDcuPaths.Clear;
    FGlobalIncludePaths.Clear;
    FGlobalCppBrowsingPaths.Clear;
    FGlobalCppLibraryPaths.Clear;
    FGlobalIncludePathsClang32.Clear;
    FGlobalCppBrowsingPathsClang32.Clear;
    FGlobalCppLibraryPathsClang32.Clear;
    FGlobalIncludePathsWin64x.Clear;
    FGlobalCppBrowsingPathsWin64x.Clear;
    FGlobalCppLibraryPathsWin64x.Clear;

    // Must read personalities before using library paths.
    if IsBDS and Reg.OpenKeyReadOnly(RegistryKey + '\Personalities') then  // do not localize
    begin
      Reg.GetValueNames(FInstalledPersonalities);
      Reg.CloseKey;
    end;

    // BDS IDE Version 5 comes in three flavors:
    // - Delphi only  (Spacely)
    // - C++ Builder only  (Cogswell)
    // - Delphi and C++Builder
    if IsBDS and (IDEVersion = 5) and SupportsPersonalities([persDelphi], True) then
      FName := 'CodeGear Delphi for Win32'
    else
    if IsBDS and (IDEVersion = 5) and SupportsPersonalities([persBCB], True) then
      FName := 'CodeGear C++Builder';

    ForceEnvOptionsUpdate := False;
    if IsBDS and (IDEVersion >= 5) then
    begin
      // If "ForceEnvOptionsUpdate" is set the EnvOptions.dproj file must be considered out of date
      if Reg.OpenKeyReadOnly(RegistryKey + '\Globals') then
        if Reg.GetDataInfo('ForceEnvOptionsUpdate', ValueInfo) then
          case ValueInfo.RegData of // some installer write it as REG_DWORD instead of REG_SZ
            rdString:
              ForceEnvOptionsUpdate := Reg.ReadString('ForceEnvOptionsUpdate') = '1';
            rdInteger:
              ForceEnvOptionsUpdate := Reg.ReadInteger('ForceEnvOptionsUpdate') = 1;
          end;
    end;

    // get library paths
    if IsBDS and (IDEVersion >= 5) and FileExists(GetEnvOptionsFileName) and not ForceEnvOptionsUpdate then
    begin
      // MSBuild
      EnvOptions := TJclSimpleXML.Create;
      try
        EnvOptions.LoadFromFile(GetEnvOptionsFileName);
        EnvOptions.Options := EnvOptions.Options - [sxoAutoCreate];
        EnvOptions.Options := EnvOptions.Options + [sxoDoNotSaveProlog];

        PropertyGroupNode := GetEnvOptionsPropertyGroupNode(EnvOptions, '');

        if Assigned(PropertyGroupNode) then
        begin
          if IDEVersion >= 8 then
          begin
            PropertyNode := PropertyGroupNode.Items.ItemNamed['DelphiDCPOutput']; // do not localize
            if Assigned(PropertyNode) then
              FDCPOutputDir := ExcludeTrailingPathDelimiter(PropertyNode.Value);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['DelphiDLLOutputPath']; // do not localize
            if Assigned(PropertyNode) then
              FBPLOutputDir := ExcludeTrailingPathDelimiter(PropertyNode.Value);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['DelphiBrowsingPath']; // do not localize
            if Assigned(PropertyNode) then
              ConvertPathList(PropertyNode.Value, FBrowsingPaths);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['DelphiLibraryPath']; // do not localize
            if Assigned(PropertyNode) then
              ConvertPathList(PropertyNode.Value, FSearchPaths);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['DelphiDLLOutputPath']; // do not localize
            if Assigned(PropertyNode) then
              ConvertPathList(PropertyNode.Value, FPackageSearchPaths);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['DelphiDebugDCUPath']; // do not localize
            if Assigned(PropertyNode) then
              ConvertPathList(PropertyNode.Value, FDebugDcuPaths);
          end
          else
          begin
            PropertyNode := PropertyGroupNode.Items.ItemNamed['Win32DCPOutput']; // do not localize
            if Assigned(PropertyNode) then
              FDCPOutputDir := ExcludeTrailingPathDelimiter(PropertyNode.Value);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['CBuilderBPLOutputPath']; // do not localize
            if not Assigned(PropertyNode) then
              PropertyNode := PropertyGroupNode.Items.ItemNamed['Win32DLLOutputPath']; // do not localize
            if Assigned(PropertyNode) then
              FBPLOutputDir := ExcludeTrailingPathDelimiter(PropertyNode.Value);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['Win32BrowsingPath']; // do not localize
            if Assigned(PropertyNode) then
              ConvertPathList(PropertyNode.Value, FBrowsingPaths);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['Win32LibraryPath']; // do not localize
            if Assigned(PropertyNode) then
              ConvertPathList(PropertyNode.Value, FSearchPaths);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['Win32DLLOutputPath']; // do not localize
            if Assigned(PropertyNode) then
              ConvertPathList(PropertyNode.Value, FPackageSearchPaths);
            PropertyNode := PropertyGroupNode.Items.ItemNamed['Win32DebugDCUPath']; // do not localize
            if Assigned(PropertyNode) then
              ConvertPathList(PropertyNode.Value, FDebugDcuPaths);
          end;
            
          if SupportsPersonalities([persBCB]) then
          begin
            LoadCppPaths(PropertyGroupNode, FGlobalIncludePaths, FGlobalCppBrowsingPaths, FGlobalCppLibraryPaths, '');

            if IDEVersion >= 23 then
            begin
              case Platform of
                ctpWin32:
                  LoadCppPaths(PropertyGroupNode, FGlobalIncludePathsClang32, FGlobalCppBrowsingPathsClang32, FGlobalCppLibraryPathsClang32, '_Clang32');
                ctpWin64:
                  if HasWin64x then
                    LoadCppPaths(GetEnvOptionsPropertyGroupNode(EnvOptions, 'x'), FGlobalIncludePathsWin64x, FGlobalCppBrowsingPathsWin64x, FGlobalCppLibraryPathsWin64x, '');
              end;
            end;
          end;
        end;
      finally
        EnvOptions.Free;
      end;
    end;

    { for BDS >= 5.0 this is the failsafe code
      for BDS <= 4.0 this is the normal way }
    LibraryKey := RegistryKey + '\Library';
    if IDEVersion >= 9 then
      LibraryKey := LibraryKey + '\' + PlatformName;
    if Reg.OpenKeyReadOnly(LibraryKey) then // do not localize
    begin
      if FBPLOutputDir = '' then
        FBPLOutputDir := ExcludeTrailingPathDelimiter(Reg.ReadString('Package DPL Output')); // do not localize
      if (FBPLOutputDir = '') and not IsBDS then
        FBPLOutputDir := RootDir + PathDelim + 'Projects\BPL';
      if FDCPOutputDir = '' then
        FDCPOutputDir := ExcludeTrailingPathDelimiter(Reg.ReadString('Package DCP Output')); // do not localize
      if (FDCPOutputDir = '') and not IsBDS then
        FDCPOutputDir := FBPLOutputDir;
      if FBrowsingPaths.Count = 0 then
        ConvertPathList(Reg.ReadString('Browsing Path'), FBrowsingPaths); // do not localize
      if FPackageSearchPaths.Count = 0 then
        ConvertPathList(Reg.ReadString('Package Search Path'), FPackageSearchPaths); // do not localize
      if FSearchPaths.Count = 0 then
        ConvertPathList(Reg.ReadString('Search Path'), FSearchPaths); // do not localize
      // BDS debug DCUs
      if (FDebugDcuPaths.Count = 0) and Reg.ValueExists('Debug DCU Path') then // do not localize
        ConvertPathList(Reg.ReadString('Debug DCU Path'), FDebugDcuPaths); // do not localize
      Reg.CloseKey;
    end;
    
    if (FDebugDcuPaths.Count = 0) and Reg.OpenKeyReadOnly(RegistryKey + '\Debugging') then // do not localize
    begin
      ConvertPathList(Reg.ReadString('Debug DCUs Path'), FDebugDcuPaths); // do not localize
      Reg.CloseKey;
    end;

    // C++Builder global directories
    if IsBDS then
    begin
      // C++Builder (2006) 2007
      if {(IDEVersion < 6) and} Reg.OpenKeyReadOnly(RegistryKey + '\CppPaths') then // do not localize
      begin
        ConvertPathList(Reg.ReadString('IncludePath'), FGlobalIncludePaths); // do not localize
        ConvertPathList(Reg.ReadString('BrowsingPath'), FGlobalCppBrowsingPaths); // do not localize
        Reg.CloseKey;
      end;
      // C++Builder 2009, 2010, XE
      if (IDEVersion >= 6) and (IDEVersion < 9) and Reg.OpenKeyReadOnly(RegistryKey + '\C++\Paths') then // do not localize
      begin
        if FGlobalIncludePaths.Count = 0 then
          ConvertPathList(Reg.ReadString('IncludePath'), FGlobalIncludePaths); // do not localize
        if FGlobalCppBrowsingPaths.Count = 0 then
          ConvertPathList(Reg.ReadString('BrowsingPath'), FGlobalCppBrowsingPaths); // do not localize
        if FGlobalCppLibraryPaths.Count = 0 then
          ConvertPathList(Reg.ReadString('LibraryPath'), FGlobalCppLibraryPaths); // do not localize
        Reg.CloseKey;
      end;
      // C++Builder XE2 only supports Win32, later versions support other platforms, in different flavours
      if (IDEVersion >= 9) and Reg.OpenKeyReadOnly(RegistryKey + '\C++\Paths\' + GetPlatformStr) then // do not localize
      begin
        LoadCppPaths(Reg, FGlobalIncludePaths, FGlobalCppBrowsingPaths, FGlobalCppLibraryPaths, '');

        if IDEVersion >= 23 then
        begin
          case Platform of
            ctpWin32:
              LoadCppPaths(Reg, FGlobalIncludePathsClang32, FGlobalCppBrowsingPathsClang32, FGlobalCppLibraryPathsClang32, '_Clang32');
            ctpWin64:
              if HasWin64x and Reg.OpenKeyReadOnly(RegistryKey + '\C++\Paths\' + GetPlatformStr + 'x') then
                LoadCppPaths(Reg, FGlobalIncludePathsWin64x, FGlobalCppBrowsingPathsWin64x, FGlobalCppLibraryPathsWin64x, '');
          end;
        end;

        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;

{  FIsPersonal := not FileExists(RootDir + '\Lib\db.dcu');
  if FIsPersonal and IsBCB and FileExists(RootDir + '\Lib\Obj\db.dcu') then
    FIsPersonal := False;}
  FIsPersonal := not FileExists(VersionedIDEBPL(RootDir + '\bin\dcldb.bpl'));
  if FIsPersonal then
    FEdition := 'Personal';


  if FProductVersion = '' then
    FProductVersion := Format('%d.%d', [Version, LatestUpdate]);

  LoadPackagesFromRegistry(FKnownIDEPackages32, 'Known IDE Packages'); // do not localize
  LoadPackagesFromRegistry(FKnownPackages32, 'Known Packages'); // do not localize
  LoadPackagesFromRegistry(FKnownIDEPackages64, 'Known IDE Packages x64'); // do not localize
  LoadPackagesFromRegistry(FKnownPackages64, 'Known Packages x64'); // do not localize
  LoadPackagesFromRegistry(FDisabledPackages32, 'Disabled Packages'); // do not localize
  LoadPackagesFromRegistry(FDisabledPackages64, 'Disabled Packages x64'); // do not localize
end;

procedure TCompileTarget.LoadPackagesFromRegistry(APackageList: TDelphiPackageList;
  const SubKey: string);
var
  Reg: TRegistry;
  List: TStrings;
  I: Integer;
begin
  APackageList.Clear;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(RegistryKey + '\' + SubKey) then
    begin
      List := TStringList.Create;
      try
        Reg.GetValueNames(List);
        for i := 0 to List.Count - 1 do
          APackageList.Add(List[i], Reg.ReadString(List[i]));
      finally
        List.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TCompileTarget.SavePackagesToRegistry(APackageList: TDelphiPackageList;
  const SubKey: string);
var
  Reg: TRegistry;
  List: TStrings;
  I: Integer;
begin
{  for I := 0 to APackageList.Count - 1 do
    APackageList[I].FFilename := InsertDirMacros(APackageList[I].FFilename);}

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(RegistryKey + '\' + SubKey, False) then
    begin
      List := TStringList.Create;
      try
        Reg.GetValueNames(List);

        // remove old packages
        for I := 0 to List.Count - 1 do
          if APackageList.IndexOfFilename(List[I]) = -1 then
            Reg.DeleteValue(List[I]);

        // add new packages
        for I := 0 to APackageList.Count - 1 do
          if List.IndexOf(APackageList[I].Filename) = -1 then
            Reg.WriteString(APackageList[I].Filename, APackageList[I].Description);

      finally
        List.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function TCompileTarget.GetHomepage: string;
begin
  if IsBCB and not IsBDS then
    Result := 'http://cc.embarcadero.com/reg/c_builder' // do not localize
  else
  begin
    if Version = 5 then
      Result := 'http://cc.embarcadero.com/reg/' // do not localize
    else
      Result := 'http://cc.embarcadero.com/reg/delphi' // do not localize
  end;
end;

procedure TCompileTarget.SavePackagesLists;
begin
  SavePackagesToRegistry(FKnownPackages32, 'Known Packages'); // do not localize
  SavePackagesToRegistry(FKnownPackages64, 'Known Packages x64'); // do not localize
  SavePackagesToRegistry(FDisabledPackages32, 'Disabled Packages'); // do not localize
  SavePackagesToRegistry(FDisabledPackages64, 'Disabled Packages x64'); // do not localize
end;

procedure ApplyCppPaths(APropertyGroupNode: TJclSimpleXMLElem; AIncludePaths, ABrowsingPaths, ALibraryPaths: TStrings; const AItemNameSuffix: string); overload;
begin
  APropertyGroupNode.Items.ItemNamed['CBuilderIncludePath' + AItemNameSuffix].Value := ConvertPathList(AIncludePaths); // do not localize
  APropertyGroupNode.Items.ItemNamed['CBuilderBrowsingPath' + AItemNameSuffix].Value := ConvertPathList(ABrowsingPaths); // do not localize
  APropertyGroupNode.Items.ItemNamed['CBuilderLibraryPath' + AItemNameSuffix].Value := ConvertPathList(ALibraryPaths); // do not localize
end;

procedure ApplyCppPaths(AReg: TRegistry; APaths: TStrings; const AName: string); overload;
var
  S: string;
begin
  S := ConvertPathList(APaths);
  if not AReg.ValueExists(AName) or (S <> AReg.ReadString(AName)) then
    AReg.WriteString(AName, S);
end;

procedure ApplyCppPaths(AReg: TRegistry; AIncludePaths, ABrowsingPaths, ALibraryPaths: TStrings; const ANameSuffix: string); overload;
begin
  ApplyCppPaths(AReg, AIncludePaths, 'IncludePath' + ANameSuffix);
  ApplyCppPaths(AReg, ABrowsingPaths, 'BrowsingPath' + ANameSuffix);
  ApplyCppPaths(AReg, ALibraryPaths, 'LibraryPath' + ANameSuffix);
end;

procedure TCompileTarget.SavePaths;
var
  Reg: TRegistry;
  S, Value, PlatformStr: string;
  i: Integer;
  EnvOptions: TJclSimpleXml;
  PropertyGroupNode: TJclSimpleXMLElem;
begin
  // save both the registry and EnvOptions.proj for Delphi 2007
  if IsBDS and (IDEVersion >= 5) then
  begin
    // MsBuild
    EnvOptions := TJclSimpleXML.Create;
    try
      EnvOptions.LoadFromFile(GetEnvOptionsFileName);
      EnvOptions.Options := EnvOptions.Options + [sxoAutoCreate];
      EnvOptions.Options := EnvOptions.Options + [sxoDoNotSaveProlog];

      PropertyGroupNode := GetEnvOptionsPropertyGroupNode(EnvOptions, '');

      if IDEVersion >= 8 then
      begin
        PropertyGroupNode.Items.ItemNamed['DelphiBrowsingPath'].Value := ConvertPathList(FBrowsingPaths); // do not localize
        PropertyGroupNode.Items.ItemNamed['DelphiLibraryPath'].Value := ConvertPathList(FSearchPaths); // do not localize
        PropertyGroupNode.Items.ItemNamed['DelphiDebugDCUPath'].Value := ConvertPathList(FDebugDcuPaths); // do not localize
      end
      else
      begin
        PropertyGroupNode.Items.ItemNamed['Win32BrowsingPath'].Value := ConvertPathList(FBrowsingPaths); // do not localize
        PropertyGroupNode.Items.ItemNamed['Win32LibraryPath'].Value := ConvertPathList(FSearchPaths); // do not localize
        PropertyGroupNode.Items.ItemNamed['Win32DebugDCUPath'].Value := ConvertPathList(FDebugDcuPaths); // do not localize
      end;

      if SupportsPersonalities([persBCB]) then
      begin
        ApplyCppPaths(PropertyGroupNode, FGlobalIncludePaths, FGlobalCppBrowsingPaths, FGlobalCppLibraryPaths, '');

        if (IDEVersion >= 23) then
        begin
          case Platform of
            ctpWin32:
              ApplyCppPaths(PropertyGroupNode, FGlobalIncludePathsClang32, FGlobalCppBrowsingPathsClang32, FGlobalCppLibraryPathsClang32, '_Clang32');
            ctpWin64:
              if HasWin64x then
                ApplyCppPaths(GetEnvOptionsPropertyGroupNode(EnvOptions, 'x'), FGlobalIncludePathsWin64x, FGlobalCppBrowsingPathsWin64x, FGlobalCppLibraryPathsWin64x, '');
          end;
        end;
      end;

      EnvOptions.SaveToFile(GetEnvOptionsFileName);
    finally
      EnvOptions.Free;
    end;
  end;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if IsBDS and (IDEVersion >= 9) then
      PlatformStr := '\' + GetPlatformStr
    else
      PlatformStr := '';
    if Reg.OpenKey(RegistryKey + '\Library' + PlatformStr, False) then // do not localize
    begin
      Reg.WriteString('Browsing Path', ConvertPathList(FBrowsingPaths)); // do not localize
      Reg.WriteString('Search Path', ConvertPathList(FSearchPaths)); // do not localize
      // BDS debug DCU
      if Reg.ValueExists('Debug DCU Path') then // do not localize
        Reg.WriteString('Debug DCU Path', ConvertPathList(FDebugDcuPaths));
      Reg.CloseKey;
    end;
    if Reg.OpenKey(RegistryKey + '\Debugging', False) then // do not localize
    begin
      S := ConvertPathList(FDebugDcuPaths);
      if S <> Reg.ReadString('Debug DCUs Path') then
        Reg.WriteString('Debug DCUs Path', S); // do not localize
      Reg.CloseKey;
    end;
    if IsBDS and
       (((IDEVersion >= 5) and (IDEVersion < 9) and Reg.OpenKey(RegistryKey + '\C++\Paths', False)) or // may not exist  // do not localize
        ((IDEVersion >= 9) and Reg.OpenKey(RegistryKey + '\C++\Paths\' + GetPlatformStr, False)) or // may not exist  // do not localize
        Reg.OpenKey(RegistryKey + '\CppPaths', False)) then     // may not exist  // do not localize
    begin
      ApplyCppPaths(Reg, FGlobalIncludePaths, FGlobalCppBrowsingPaths, FGlobalCppLibraryPaths, '');

      if IDEVersion >= 23 then
      begin
        case Platform of
          ctpWin32:
            ApplyCppPaths(Reg, FGlobalIncludePathsClang32, FGlobalCppBrowsingPathsClang32, FGlobalCppLibraryPathsClang32, '_Clang32');
          ctpWin64:
            if HasWin64x and Reg.OpenKey(RegistryKey + '\C++\Paths\' + GetPlatformStr + 'x', False) then
              ApplyCppPaths(Reg, FGlobalIncludePathsWin64x, FGlobalCppBrowsingPathsWin64x, FGlobalCppLibraryPathsWin64x, '');
        end;
      end;

      Reg.CloseKey;
    end;

    if not FEnvVars.Matches(FOrgEnvVars) then
    begin
      if Reg.OpenKey(RegistryKey + '\Environment Variables', True) then // do not localize
      begin
        // delete deleted items
        for i := 0 to FOrgEnvVars.Count - 1 do
          if FEnvVars.IndexOfName(FOrgEnvVars.Names[i]) = -1 then
            Reg.DeleteValue(FOrgEnvVars.Names[i]);

        // add new
        for i := 0 to FEnvVars.Count - 1 do
        begin
          Value := FEnvVars.Values[FEnvVars.Names[i]];
          if Value <> FOrgEnvVars.Values[FEnvVars.Names[i]] then
            Reg.WriteString(FEnvVars.Names[i], Value);
        end;
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TCompileTarget.SetEnvPath(const Value: string);
begin
  FEnvVars.Values['PATH'] := ExpandDirMacros(Value);
end;

function TCompileTarget.SupportedPersonalities: TPersonalities;
var
  Pers: TPersonalityType;
begin
  Result := [];
  if not IsBDS then
  begin
    if IsDelphi then
      Result := [persDelphi]
    else
    if IsBCB then
      Result := [persBCB];
  end
  else
  begin
    for Pers := Low(TPersonalityType) to High(TPersonalityType) do
      if FInstalledPersonalities.IndexOf(PersNames[Pers]) >= 0 then
        Include(Result, Pers);
  end;
end;

function TCompileTarget.SupportsPersonalities(Personalities: TPersonalities; Exact: Boolean = False): Boolean;
begin
  if Exact then
    Result := SupportedPersonalities = Personalities
  else
    Result := SupportedPersonalities * Personalities = Personalities;
  // C++ Win64 personality appeared with XE3
  if (Personalities = [persBCB]) and IsBDS and (IDEVersion >= 9) and (IDEVersion < 11) and (FPlatform = ctpWin64) then
    Result := False;
end;

function TCompileTarget.TargetType: string;
begin
  if IsBDS then
  begin
    if SupportsPersonalities([persDelphi]) or (Version >= 4) then  // Versions 4 and upper support dual mode packages
      Result := 'D' // do not localize
    else
    if SupportsPersonalities([persBCB]) then
      Result := 'C' // do not localize
    else
      Result := '';
  end
  else
  if IsDelphi then
    Result := 'D' // do not localize
  else
  if IsBCB then
    Result := 'C' // do not localize
  else
    Result := '';
end;

function TCompileTarget.GetMake: string;
begin
  Result := RootDir + '\Bin\make.exe'; // do not localize
end;

function TCompileTarget.GetBplDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(ExpandDirMacros(BPLOutputDir));
end;

function TCompileTarget.GetDcc32: string;
begin
  Result := RootDir + '\Bin\dcc32.exe'; // do not localize
end;

function TCompileTarget.GetDcc64: string;
begin
  Result := RootDir + '\Bin\dcc64.exe'; // do not localize
end;

function TCompileTarget.GetDccil: string;
begin
  Result := RootDir + '\Bin\dccil.exe'; // do not localize
end;

function TCompileTarget.GetBcc32: string;
begin
  Result := RootDir + '\Bin\bcc32.exe'; // do not localize
end;

function TCompileTarget.GetBcc64: string;
begin
  Result := RootDir + '\Bin\bcc64.exe'; // do not localize
end;

function TCompileTarget.GetBcc64x: string;
begin
  Result := RootDir + '\Bin64\bcc64x.exe'; // do not localize
end;

function TCompileTarget.GetIlink32: string;
begin
  Result := RootDir + '\Bin\ilink32.exe'; // do not localize
end;

function TCompileTarget.GetKnownIDEPackages: TDelphiPackageList;
begin
  if Platform = ctpWin64 then
    Result := FKnownIDEPackages64
  else
    Result := FKnownIDEPackages32;
end;

function TCompileTarget.GetKnownPackages: TDelphiPackageList;
begin
  if Platform = ctpWin64 then
    Result := FKnownPackages64
  else
    Result := FKnownPackages32;
end;

function TCompileTarget.GetTlib: string;
begin
  Result := RootDir + '\Bin\tlib.exe'; // do not localize
end;

function TCompileTarget.HasBDE: Boolean;
begin
  Result := (Platform = ctpWin32) and ((Version < 21) or FileExists(PathAddSeparator(RootLibReleaseDir) + 'bdertl.dcp'));
end;

function TCompileTarget.HasWin64x: Boolean;
begin
  Result := (Platform = ctpWin64) and (Version >= 29) and FileExists(Bcc64x);
end;

function TCompileTarget.GetDcpDir: string;
begin
  Result := ExpandDirMacros(DCPOutputDir);
end;

function TCompileTarget.GetDisabledPackages: TDelphiPackageList;
begin
  if Platform = ctpWin64 then
    Result := FDisabledPackages64
  else
    Result := FDisabledPackages32;
end;

function TCompileTarget.GetEnvPath: string;
begin
  if EnvVars.IndexOfName('PATH') = -1 then
    Result := GlobalPathEnvVar
  else
    Result := FEnvVars.Values['PATH'];
end;

function TCompileTarget.GetBDSProjectsDir: string;
begin
  if IsBDS then
  begin
    if FEnvVars.IndexOfName('BDSPROJECTSDIR') >= 0 then // do not localize
      Result := ExpandDirMacros(EnvVars.Values['BDSPROJECTSDIR']) // do not localize
    else
      Result := ExpandDirMacros(GetEnvironmentVariable('BDSPROJECTSDIR')); // do not localize

    Result := ExcludeTrailingPathDelimiter(Result);
    if Result = '' then // ignore BDSPROJECTSDIR env-var because Delphi and BCB do not know them
      Result := FDefaultBDSProjectsDir;
  end
  else
    Result := RootDir + '\Projects'; // do not localize
  Result := FixBackslashBackslash(Result);
end;

function TCompileTarget.GetCommonProjectsDir: string;
begin
  if IsBDS and (IDEVersion >= 5) then
    Result := FCommonProjectsDir    // Filled in LoadFromRegistry
  else
    Result := GetBDSProjectsDir;
end;

procedure TCompileTarget.GetBDSVersion(out Name: string; out Version: Integer; out VersionStr: string);
begin
  if (IDEVersion >= Low(BDSVersions)) and (IDEVersion <= High(BDSVersions)) then
  begin
    Name := BDSVersions[IDEVersion].Name;
    VersionStr := BDSVersions[IDEVersion].VersionStr;
    Version := BDSVersions[IDEVersion].Version;
  end
  else
  begin
    Name := IDEName;
    Version := IDEVersion;
    VersionStr := IDEVersionStr;
  end;
end;

function TCompileTarget.ReadBDSProjectsDir: string;
begin
  if IsBDS and (IDEVersion >= Low(BDSVersions)) and (IDEVersion <= High(BDSVersions)) then
  begin
    if IDEVersion < 4 then
    begin
      Result := LoadResStrings(RootDir + '\Bin\coreide' + BDSVersions[IDEVersion].CIV + '.',
        [BDSVersions[IDEVersion].ProjectDirResId]);

      if Result = '' then
        Result := 'Borland Studio Projects'; // do not localize
      Result := ExcludeTrailingPathDelimiter(FixBackslashBackslash(ExcludeTrailingPathDelimiter(GetPersonalFolder) + '\' + Result));
    end
    else
      Result := TJclBDSInstallation.GetDefaultProjectsDirectory(RootDir, IDEVersion);
  end
  else
    Result := '';
end;

function TCompileTarget.GetEnvOptionsFileName: string; // Delphi 2007
begin
  if IsBDS and (IDEVersion >= 8) then
    Result := Format('%s\Embarcadero\BDS\%d.0\EnvOptions.proj', [ExcludeTrailingPathDelimiter(GetAppdataFolder), IDEVersion])
  else
  if IsBDS and (IDEVersion >= 6) then
    Result := Format('%s\CodeGear\BDS\%d.0\EnvOptions.proj', [ExcludeTrailingPathDelimiter(GetAppdataFolder), IDEVersion])
  else
  if IsBDS and (IDEVersion = 5) then
    Result := Format('%s\Borland\BDS\%d.0\EnvOptions.proj', [ExcludeTrailingPathDelimiter(GetAppdataFolder), IDEVersion])
  else
    Result := '';
end;

function TCompileTarget.GetEnvOptionsPropertyGroupNode(
  AEnvOptions: TJclSimpleXML;
  const APlatformStrSuffix: string): TJclSimpleXMLElem;
var
  I: Integer;
  ConditionProperty: TJclSimpleXMLProp;
begin
  if IDEVersion >= 9 then
  begin
    Result := nil;
    for I := 0 to AEnvOptions.Root.Items.Count - 1 do
    begin
      ConditionProperty := AEnvOptions.Root.Items[I].Properties.ItemNamed['Condition'];
      if Assigned(ConditionProperty) and
        (ConditionProperty.Value = Format('''$(Platform)''==''%s''', [GetPlatformStr + APlatformStrSuffix])) then
      begin
        Result := AEnvOptions.Root.Items[I];
        Break;
      end;
    end;
  end
  else
    Result := AEnvOptions.Root.Items.ItemNamed['PropertyGroup']; // do not localize
end;

function TCompileTarget.ReadCommonProjectsDir: string;
begin
  if IsBDS and (IDEVersion >= 5) then
    Result := TJclBDSInstallation.GetCommonProjectsDirectory(RootDir, IDEVersion)
  else
    Result := '';
end;

function TCompileTarget.VersionedDCP(const Filename: string): string;
begin
  { This function is used for 3rd-Party dependency packages. No JVCL package name is going
    through this function. }
  if Version > 5 then
    Result := Filename
  else
    Result := ChangeFileExt(Filename, '') + IntToStr(Version) + '0' + ExtractFileExt(Filename);
end;

function TCompileTarget.VersionedBPL(const Filename: string): string;
begin
  { This function is used for 3rd-Party dependency packages. No JVCL package name is going
    through this function. }
  Result := ChangeFileExt(Filename, '') + IntToStr(Version) + '0' + ExtractFileExt(Filename);
end;

function TCompileTarget.VersionedIDEBPL(const Filename: string): string;
begin
  if IsBDS and (IDEVersion = 5) then // Delphi 2007 is identified as version 11 for JVCL packages, but the IDE packages have version 10
    Result := ChangeFileExt(Filename, '') + '100' + ExtractFileExt(Filename)
  else
    Result := VersionedBPL(Filename);
end;

function TCompileTarget.GetPlatformStr: string;
begin
  Result := '';
  if IsBDS and (IDEVersion >= 9) then
    case FPlatform of
      ctpWin32: Result := 'Win32'; // do not localize
      ctpWin64: Result := 'Win64'; // do not localize
    end;
end;

function TCompileTarget.GetRootLibDir: string;
var
  PlatformDir: string;
begin
  PlatformDir := '';
  if IsBDS and (IDEVersion >= 8) then // RAD Studio XE started the new directory structure
    case FPlatform of
      ctpWin32: PlatformDir := 'win32'; // do not localize
      ctpWin64: PlatformDir := 'win64'; // do not localize
    end;

  Result := AppendPath(AppendPath(RootDir, 'lib'), PlatformDir);
end;

function TCompileTarget.GetRootLibReleaseDir: string;
begin
  if IsBDS and (IDEVersion >= 8) then // RAD Studio XE started the new directory structure
    Result := AppendPath(GetRootLibDir, 'release')
  else
    Result := GetRootLibDir;
end;

function TCompileTarget.GetProjectDir: string;
begin
  if IsBDS then
    Result := BDSProjectsDir
  else
    Result := RootDir;
  Result := AppendPath(Result, 'Projects');
end;

{ TDelphiPackageList }

procedure TDelphiPackageList.Add(const Filename, Description: string);
var
  Item: TDelphiPackage;
begin
  if Description = '' then
    Item := TDelphiPackage.Create(Filename, ChangeFileExt(ExtractFileName(Filename), ''))
  else
    Item := TDelphiPackage.Create(Filename, Description);
  inherited Add(Item);
end;

procedure TDelphiPackageList.Remove(const Filename: string);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Filename, Filename) = 0 then
    begin
      Delete(i);
      Exit;
    end;
end;

function TDelphiPackageList.GetItems(Index: Integer): TDelphiPackage;
begin
  Result := TDelphiPackage(inherited Items[Index]);
end;

function TDelphiPackageList.IndexOfFilename(const Filename: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if CompareText(Items[Result].Filename, Filename) = 0 then
      Exit;
  Result := -1;
end;

{ TDelphiPackage }

constructor TDelphiPackage.Create(const AFilename, ADescription: string);
begin
  inherited Create;
  FFilename := AFilename;
  FDescription := ADescription;
end;

function TDelphiPackage.GetName: string;
begin
  Result := ExtractFileName(Filename);
end;

{ TEnvVarStrings }

constructor TEnvVarStrings.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
  FItems.Duplicates := dupIgnore;
  FItems.Sorted := True;
  //FItems.OwnsObjects := True;  older Delphi versions do not support this, so we do it ourself in Clear()
end;

destructor TEnvVarStrings.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TEnvVarStrings.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems.Objects[I].Free;
  FItems.Clear;
end;

procedure TEnvVarStrings.Add(const AName, AValue: string);
var
  Item: TEnvVarStringItem;
begin
  Item := TEnvVarStringItem.Create;
  Item.Value := AValue;
  FItems.AddObject(AName, Item);
end;

procedure TEnvVarStrings.Assign(ASource: TEnvVarStrings);
var
  I: Integer;
begin
  Clear;
  FItems.Clear;
  if ASource <> nil then
    for I := 0 to ASource.Count - 1 do
      Add(ASource.Names[I], ASource.ValuesByIndex[I]);
end;

function TEnvVarStrings.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TEnvVarStrings.GetName(Index: Integer): string;
begin
  Result := FItems[Index];
end;

function TEnvVarStrings.GetValue(const Name: string): string;
var
  Index: Integer;
begin
  Index := IndexOfName(Name);
  if Index <> -1 then
    Result := ValuesByIndex[Index];
end;

procedure TEnvVarStrings.SetValue(const Name, Value: string);
var
  Index: Integer;
begin
  Index := IndexOfName(Name);
  if Index = -1 then
    Add(Name, Value)
  else
    ValuesByIndex[Index] := Value;
end;

function TEnvVarStrings.GetValueByIndex(Index: Integer): string;
begin
  Result := TEnvVarStringItem(FItems.Objects[Index]).Value;
end;

procedure TEnvVarStrings.SetValueByIndex(Index: Integer; const Value: string);
begin
  TEnvVarStringItem(FItems.Objects[Index]).Value := Value;
end;

function TEnvVarStrings.IndexOfName(const AName: string): Integer;
begin
  Result := FItems.IndexOf(AName);
end;

function TEnvVarStrings.Matches(AList: TEnvVarStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (AList <> nil) and (Count = AList.Count) then
  begin
    for I := 0 to Count - 1 do
    begin
      if not SameText(FItems[I], AList.FItems[I]) then
        Exit;
      if ValuesByIndex[i] <> AList.ValuesByIndex[I] then
        Exit;
    end;
    Result := True;
  end;
end;

end.
