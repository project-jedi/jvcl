unit DelphiData;

interface

uses
  Windows, SysUtils, Classes, Contnrs, Registry, ShlObj;

const
  BDSVersions: array[1..5] of record
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
    (Name: 'BDS'; VersionStr: '2006'; Version: 10; CIV: '100'; ProjectDirResId: 64719; Supported: True),
    // { TODO : change ProjectDirResId when BDS 2007 is available }
    (Name: 'BDS'; VersionStr: '2007'; Version: 11; CIV: '110'; ProjectDirResId: 0; Supported: True)
  );

type
  TDelphiIDE = class;
  TDelphiIDEList = class;
  TDelphiPackage = class;
  TDelphiPackageList = class;
  TIDEPersonality = class;

  TDelphiIDEList = class(TObjectList)
  private
    function GetItems(Index: Integer): TDelphiIDE;
    procedure LoadIDEs(const SubKey, HKCUSubKey: string);
    function IsBDSSupported(const IDEVersionStr: string): Boolean;
  public
    constructor Create;
    property Items[Index: Integer]: TDelphiIDE read GetItems; default;
  end;

  TIDEPersonality = class(TObject)
  private
    FIDE: TDelphiIDE;
    FName: string;

    FBrowsingPaths: TStringList;
    FDCPOutputDir: string;
    FBPLOutputDir: string;
    FPackageSearchPaths: TStringList;
    FSearchPaths: TStringList;
    FDebugDcuPaths: TStringList;
    FNamespaceSearchPaths: TStringList;

    FDisabledPackages: TDelphiPackageList;
    FKnownPackages: TDelphiPackageList;
    FKnownIDEPackages: TDelphiPackageList;

    function GetBplDir: string;
    function GetDcpDir: string;
  protected
    procedure LoadPackagesFromRegistry(APackageList: TDelphiPackageList; const SubKey: string);
    procedure SavePackagesToRegistry(APackageList: TDelphiPackageList; const SubKey: string);
    procedure LoadFromRegistry; virtual; abstract;
  public
    constructor Create(const AName: string; AIDE: TDelphiIDE);
    destructor Destroy; override;

    procedure SavePaths; virtual; abstract;
      { writes BrowsingPaths, SearchPaths, DebugDcuPath, PackaggeSearchPaths to the registry }
    procedure SavePackagesLists; virtual; abstract;
      { writes KnownIDEPackages, KnownPackages and DisabledPackages to the registry }

    function FindPackage(const PackageName: string): TDelphiPackage;
    function FindPackageEx(const PackageNameStart: string): TDelphiPackage;

    property Name: string read FName;
    property IDE: TDelphiIDE read FIDE;

    property BrowsingPaths: TStringList read FBrowsingPaths; // with macros
    property DCPOutputDir: string read FDCPOutputDir; // with macros
    property BPLOutputDir: string read FBPLOutputDir; // with macros
    property PackageSearchPaths: TStringList read FPackageSearchPaths; // BDS // with macros
    property NamespaceSearchPaths: TStringList  read FNamespaceSearchPaths; // BDS (e.g.: Borland.Vcl)
    property SearchPaths: TStringList read FSearchPaths; // with macros
    property DebugDcuPaths: TStringList read FDebugDcuPaths; // with macros

    property BplDir: string read GetBplDir; // macros are expanded
    property DcpDir: string read GetDcpDir; // macros are expanded

    property KnownIDEPackages: TDelphiPackageList read FKnownIDEPackages;
    property KnownPackages: TDelphiPackageList read FKnownPackages;
    property DisabledPackages: TDelphiPackageList read FDisabledPackages;
  end;

  { Delphi Win32 }
  TDelphiPersonality = class(TIDEPersonality)
  protected
    procedure LoadFromRegistry; override;
  public
    procedure SavePaths; override;
    procedure SavePackagesLists; override;
  end;

  { C++ Win32 }
  TCppPersonality = class(TDelphiPersonality)
  protected
    procedure LoadFromRegistry; override;
  public
    constructor Create(const AName: string; IDE: TDelphiIDE);
    procedure SavePaths; override;
    procedure SavePackagesLists; override;
  end;

  { Delphi/C++ Win32 (BDS) }
  TDelphiCppPersonality = class(TCppPersonality)
  protected
    procedure LoadFromRegistry; override;
  public
    constructor Create(const AName: string; IDE: TDelphiIDE);
    procedure SavePaths; override;
    procedure SavePackagesLists; override;
  end;

  { C# and Delphi.NET }
  TDotNetPersonality = class(TDelphiPersonality)
  protected
    procedure LoadFromRegistry; override;
  public
    procedure SavePaths; override;
  end;

  TDelphiIDE = class(TObject)
  private
    FIsValid: Boolean;
    FName: string;
    FIDEName: string;
    FPersonalities: TObjectList;
    FLatestRTLPatch: Integer;
    FLatestUpdate: Integer;
    FIDEVersion: Integer;
    FIDEVersionStr: string;
    FVersion: Integer;
    FVersionStr: string;
    FExecutable: string;
    FEdition: string;
    FDCMName: string;
    FRootDir: string;
    FBDSProjectsDir: string;
    FHKLMRegistryKey: string;
    FRegistryKey: string;
    function GetPersonality(Index: Integer): TIDEPersonality;
    function GetPersonalityCount: Integer;
  protected
    procedure LoadFromRegistry;
    function ReadBDSProjectsDir: string;
    function GetHomepage: string;
    procedure GetBDSVersion(out Name: string; out Version: Integer; out VersionStr: string);
    function GetMake: string;
    function GetProjectDir: string;
  public
    constructor Create(const AName, AVersion, ARegSubKey: string);
    destructor Destroy; override;

    function IsBCB: Boolean;
    function IsBDS: Boolean;
    function IsPersonal: Boolean;
    function DisplayName: string;

    function VersionedDCP(const Filename: string): string;
      { returns the filename + version + extension for Delphi 5 and BCB 5
        else it returns the Filename. }

    function ExpandDirMacros(const Dir: string): string;
    function InsertDirMacros(const Dir: string): string;

    property Homepage: string read GetHomepage;
    property RegistryKey: string read FRegistryKey;
    property HKLMRegistryKey: string read FHKLMRegistryKey;

    property Make: string read GetMake;

    property Name: string read FName;
    property Version: Integer read FVersion;
    property VersionStr: string read FVersionStr;
    property IDEName: string read FIDEName;
    property IDEVersion: Integer read FIDEVersion;
    property IDEVersionStr: string read FIDEVersionStr;
    property Executable: string read FExecutable; // [Reg->App] x:\path\Delphi.exe
    property RootDir: string read FRootDir; // [Reg->RootDir] x:\path
    property Edition: string read FEdition; // [Reg->Version] PER/PRO/CSS
    property LatestUpdate: Integer read FLatestUpdate;
    property LatestRTLPatch: Integer read FLatestRTLPatch;

    property PersonalityCount: Integer read GetPersonalityCount;
    property Personalities[Index: Integer]: TIDEPersonality read GetPersonality;

    property BDSProjectsDir: string read FBDSProjectsDir;
    property ProjectDir: string read GetProjectDir; // Delphi 5-7: RootDir\Projects BDS: BDSProjectDir\Projects

    property IsValid: Boolean read FIsValid; // is True if the installation is valid
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

const
  KeyBorland = '\SOFTWARE\Borland\'; // do not localize

implementation

uses
  CmdLineUtils, Utils;

{ TDelphiIDEList }

constructor TDelphiIDEList.Create;
var
  Reg: TRegistry;
  List: TStringList;
  i: Integer;
begin
  inherited Create;
  if CmdOptions.RegistryKeyDelphi = '' then
    CmdOptions.RegistryKeyDelphi := 'Delphi'; // do not localize
  if CmdOptions.RegistryKeyBCB = '' then
    CmdOptions.RegistryKeyBCB := 'C++Builder'; // do not localize
  if CmdOptions.RegistryKeyBDS = '' then
    CmdOptions.RegistryKeyBDS := 'BDS'; // do not localize

  if not CmdOptions.IgnoreDelphi then
    LoadIDEs('Delphi', CmdOptions.RegistryKeyDelphi); // do not localize
  if not CmdOptions.IgnoreBCB then
    LoadIDEs('C++Builder', CmdOptions.RegistryKeyBCB); // do not localize
  if not CmdOptions.IgnoreDelphi then
    LoadIDEs('BDS', CmdOptions.RegistryKeyBDS); // do not localize

  // DCM support for BDS (Delphi and BCB are not supported because their version numbers are the same)
  if not CmdOptions.IgnoreDelphi then
  begin
    Reg := TRegistry.Create;
    try
      if (CmdOptions.RegistryKeyBDS = 'BDS') and // user has not specified a special regkey
          Reg.OpenKeyReadOnly(KeyBorland + 'DCM') then
      begin
        List := TStringList.Create;
        try
          Reg.GetKeyNames(List);
          for i := 0 to List.Count - 1 do
            LoadIDEs('BDS', 'DCM\' + List[i]); // do not localize
        finally
          List.Free;
        end;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

function TDelphiIDEList.GetItems(Index: Integer): TDelphiIDE;
begin
  Result := TDelphiIDE(inherited Items[Index]);
end;

procedure TDelphiIDEList.LoadIDEs(const SubKey, HKCUSubKey: string);
var
  Reg, HKCUReg: TRegistry;
  List: TStrings;
  i: Integer;
begin
  Reg := TRegistry.Create;
  HKCUReg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    HKCUReg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(KeyBorland + SubKey) then
    begin
      List := TStringList.Create;
      try
        Reg.GetKeyNames(List);
        for i := 0 to List.Count - 1 do
          if List[i][1] in ['1'..'9'] then // only version numbers (not "BDS\DBExpress")
            if (SubKey <> 'BDS') or IsBDSSupported(List[i]) then
            begin
              if HKCUReg.KeyExists(KeyBorland + HKCUSubKey + '\' + List[i]) then
                Add(TDelphiIDE.Create(SubKey, List[i], HKCUSubKey));
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

function TDelphiIDEList.IsBDSSupported(const IDEVersionStr: string): Boolean;
var
  IDEVersion: Integer;
begin
  Result := False;
  IDEVersion := StrToInt(IDEVersionStr[1]);
  if (IDEVersion >= Low(BDSVersions)) and (IDEVersion <= High(BDSVersions)) then
    Result := BDSVersions[IDEVersion].Supported;
end;

{ TDelphiIDE }

constructor TDelphiIDE.Create(const AName, AVersion, ARegSubKey: string);
begin
  inherited Create;
  FPersonalities := TObjectList.Create;
  FIDEName := AName;
  FIDEVersionStr := AVersion;
  FIDEVersion := StrToIntDef(Copy(FIDEVersionStr, 1, Pos('.', FIDEVersionStr) - 1), 0);
  if AName <> ARegSubKey then
    FDCMName := ExtractFileName(ARegSubKey);
  if not IsBDS then
  begin
    FName := FIDEName;
    FVersion := FIDEVersion;
    FVersionStr := FIDEVersionStr;
  end
  else
    GetBDSVersion(FName, FVersion, FVersionStr);

  FHKLMRegistryKey := KeyBorland + IDEName + '\' + IDEVersionStr;
  FRegistryKey := KeyBorland + ARegSubKey + '\' + IDEVersionStr;

  if IsBDS then
  begin
    if Version <= 9 then
      FPersonalities.Add(TDelphiPersonality.Create('Delphi Win32', Self))
    else
      FPersonalities.Add(TDelphiCppPersonality.Create('Delphi/C++ Win32', Self));
    FPersonalities.Add(TDotNetPersonality.Create('.NET', Self));
  end
  else
  if IsBCB then
  begin
    if Version in [6, 7] then
    begin
      FPersonalities.Add(TCppPersonality.Create('BCB VCL ', Self));
      FPersonalities.Add(TCppPersonality.Create('BCB VisualCLX ', Self));
    end
    else
      FPersonalities.Add(TCppPersonality.Create('BCB Win32', Self))
  end
  else
  begin
    if Version in [6, 7] then
    begin
      FPersonalities.Add(TDelphiPersonality.Create('Delphi VCL ', Self));
      FPersonalities.Add(TDelphiPersonality.Create('Delphi VisualCLX ', Self));
    end
    else
      FPersonalities.Add(TDelphiPersonality.Create('Delphi Win32', Self));
  end;

  LoadFromRegistry;

  FIsValid := (RootDir <> '') and (Executable <> '') and FileExists(Executable) and FileExists(RootDir + '\bin\dcc32.exe');
end;

destructor TDelphiIDE.Destroy;
begin
  FPersonalities.Free;
  inherited Destroy;
end;

function TDelphiIDE.DisplayName: string;
begin
  if FDCMName <> '' then
    Result := Format('%s %s [%s] (%s)', [Name, VersionStr, FDCMName, Edition]) // do not localize
  else
    Result := Format('%s %s (%s)', [Name, VersionStr, Edition]); // do not localize
end;

function TDelphiIDE.ExpandDirMacros(const Dir: string): string;
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
      else if IsBDS and (S = 'bdsprojectsdir') then
        NewS := BDSProjectsDir
      else
        NewS := GetEnvironmentVariable(S);

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

function TDelphiIDE.InsertDirMacros(const Dir: string): string;
begin
  Result := Dir;
  if AnsiStartsText(RootDir + PathDelim, Dir) then
  begin
    if IsBCB then
      Result := '$(BCB)' // do not localize
    else if not IsBDS then
      Result := '$(DELPHI)' // do not localize
    else
      Result := '$(BDS)'; // do not localize
    Result := Result + Copy(Dir, Length(RootDir) + 1, MaxInt);
  end;
  if IsBDS then
  begin
    if AnsiStartsText(BDSProjectsDir + PathDelim, Dir) then
    begin
      Result := '$(BDSPROJECTSDIR)'; // do not localize
      Result := Result + Copy(Dir, Length(BDSProjectsDir) + 1, MaxInt);
    end;
  end;
end;

function TDelphiIDE.IsBCB: Boolean;
begin
  Result := (CompareText(Name, 'Delphi') <> 0);
end;

function TDelphiIDE.IsBDS: Boolean;
begin
  Result := CompareText(IDEName, 'BDS') = 0;
end;

function TDelphiIDE.IsPersonal: Boolean;
begin
  Result := (CompareText(Edition, 'PER') = 0) or // do not localize
            (CompareText(Edition, 'PERS') = 0) or // do not localize
            (CompareText(Edition, 'Personal') = 0) or // do not localize
            (CompareText(Edition, 'PersonalEdition') = 0) or // do not localize
            (CompareText(Edition, 'Personal Edition') = 0) or // do not localize
            (CompareText(Edition, 'STD') = 0); // do not localize
end;

procedure TDelphiIDE.LoadFromRegistry;
var
  Reg: TRegistry;
  List: TStrings;
  i: Integer;
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
      else
        FEdition := 'Pers'; // do not localize

      FExecutable := Reg.ReadString('App'); // do not localize
      FRootDir := ExcludeTrailingPathDelimiter(Reg.ReadString('RootDir')); // do not localize

      if IsBDS then
        FBDSProjectsDir := ReadBDSProjectsDir // reads from COREIDExx.XX's resource strings
      else
        FBDSProjectsDir := RootDir + '\Projects';

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

    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKeyReadOnly(RegistryKey) then
    begin
      // obtain updates state
      List := TStringList.Create;
      try
        Reg.GetValueNames(List);
        for i := 1 to 10 do
        begin
          if Reg.ValueExists('Update #' + IntToStr(i)) then // do not localize
            if FLatestUpdate < i then
              FLatestUpdate := i;
          if i = 1 then
          begin
            if Reg.ValueExists('Pascal RTL Patch') then // do not localize
              if FLatestRTLPatch < i then
                FLatestRTLPatch := i;
          end
          else
            if Reg.ValueExists('Pascal RTL Patch #' + IntToStr(i)) then // do not localize
              if FLatestRTLPatch < i then
                FLatestRTLPatch := i;
        end;
      finally
        List.Free;
      end;
      Reg.CloseKey;
    end;

    for i := 0 to PersonalityCount - 1  do
      Personalities[i].LoadFromRegistry;
  finally
    Reg.Free;
  end;
end;

function TDelphiIDE.GetHomepage: string;
begin
  if IsBCB then
    Result := 'http://www.borland.com/downloads/download_cbuilder.html' // do not localize
  else
  begin
    if Version = 5 then
      Result := 'http://info.borland.com/devsupport/delphi/downloads/index.html' // do not localize
    else
      Result := 'http://www.borland.com/downloads/download_delphi.html' // do not localize
  end;
end;

function TDelphiIDE.GetMake: string;
begin
  Result := RootDir + '\Bin\make.exe'; // do not localize
end;

procedure TDelphiIDE.GetBDSVersion(out Name: string; out Version: Integer; out VersionStr: string);
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

var
  _SHGetSpecialFolderPathA: function(hwndOwner: HWND; lpszPath: PAnsiChar;
    nFolder: Integer; fCreate: BOOL): BOOL; stdcall;

function TDelphiIDE.ReadBDSProjectsDir: string;
var
  h: HMODULE;
  LocaleName: array[0..4] of Char;
  Filename: string;
  PersDir: string;
  Reg: TRegistry;
begin
  if IsBDS and (IDEVersion >= Low(BDSVersions)) and (IDEVersion <= High(BDSVersions)) then
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      // update BDSProjectsDir if the user has defined an IDE-environment variable
      if Reg.OpenKeyReadOnly(HKLMRegistryKey + '\Environment Variables') then // do not localize
      begin
        if Reg.ValueExists('BDSPROJECTSDIR') then // do not localize
          FBDSProjectsDir := ExpandDirMacros(Reg.ReadString('BDSPROJECTSDIR')); // do not localize
        Reg.CloseKey;
        Exit;
      end;
    finally
      Reg.Free;
    end;

    Result := 'Borland Studio Projects'; // do not localize

    FillChar(LocaleName, SizeOf(LocaleName[0]), 0);
    GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
    if LocaleName[0] <> #0 then
    begin
      Filename := RootDir + '\Bin\coreide' + BDSVersions[IDEVersion].CIV + '.'; // do not localize
      if FileExists(Filename + LocaleName) then
        Filename := Filename + LocaleName
      else
      begin
        LocaleName[2] := #0;
        if FileExists(Filename + LocaleName) then
          Filename := Filename + LocaleName
        else
          Filename := '';
      end;

      if Filename <> '' then
      begin
        h := LoadLibraryEx(PChar(Filename), 0,
          LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
        if h <> 0 then
        begin
          SetLength(Result, 1024);
          SetLength(Result, LoadString(h, BDSVersions[IDEVersion].ProjectDirResId, PChar(Result), Length(Result) - 1));
          FreeLibrary(h);
        end;
      end;
    end;

    if not Assigned(_SHGetSpecialFolderPathA) then
      _SHGetSpecialFolderPathA := GetProcAddress(GetModuleHandle('shell32.dll'), 'SHGetSpecialFolderPathA'); // do not localize
    SetLength(PersDir, MAX_PATH);
    if Assigned(_SHGetSpecialFolderPathA) and
       _SHGetSpecialFolderPathA(0, PChar(PersDir), CSIDL_PERSONAL, False) then
    begin
      SetLength(PersDir, StrLen(PChar(PersDir)));
      Result := ExcludeTrailingPathDelimiter(PersDir) + '\' + Result;
    end
    else
      Result := '';
  end
  else
    Result := '';
end;

function TDelphiIDE.VersionedDCP(const Filename: string): string;
begin
  if Version > 5 then
    Result := Filename
  else
    Result := ChangeFileExt(Filename, '') + IntToStr(Version) + '0' + ExtractFileExt(Filename);
end;

function TDelphiIDE.GetProjectDir: string;
begin
  if IsBDS then
    Result := BDSProjectsDir
  else
    Result := RootDir;
  Result := Result + PathDelim + 'Projects';
end;

function TDelphiIDE.GetPersonalityCount: Integer;
begin
  Result := FPersonalities.Count;
end;

function TDelphiIDE.GetPersonality(Index: Integer): TIDEPersonality;
begin
  Result := TIDEPersonality(FPersonalities[Index]);
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

{ TIDEPersonality }

constructor TIDEPersonality.Create(const AName: string; AIDE: TDelphiIDE);
begin
  inherited Create;
  FName := AName;
  FIDE := AIDE;

  FBrowsingPaths := TStringList.Create;
  FPackageSearchPaths := TStringList.Create;
  FSearchPaths := TStringList.Create;
  FDebugDcuPaths := TStringList.Create;
  FNamespaceSearchPaths := TStringList.Create;

  FBrowsingPaths.Duplicates := dupIgnore;
  FPackageSearchPaths.Duplicates := dupIgnore;
  FSearchPaths.Duplicates := dupIgnore;
  FDebugDcuPaths.Duplicates := dupIgnore;
  FNamespaceSearchPaths.Duplicates := dupIgnore;

  FDisabledPackages := TDelphiPackageList.Create;
  FKnownIDEPackages := TDelphiPackageList.Create;
  FKnownPackages := TDelphiPackageList.Create;
end;

destructor TIDEPersonality.Destroy;
begin
  FBrowsingPaths.Free;
  FPackageSearchPaths.Free;
  FSearchPaths.Free;
  FDebugDcuPaths.Free;
  FNamespaceSearchPaths.Free;

  FDisabledPackages.Free;
  FKnownIDEPackages.Free;
  FKnownPackages.Free;
  inherited Destroy;
end;

function TIDEPersonality.FindPackage(const PackageName: string): TDelphiPackage;

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
  Result := Find(KnownIDEPackages);
  if Result = nil then
    Result := Find(KnownPackages);
end;

function TIDEPersonality.FindPackageEx(const PackageNameStart: string): TDelphiPackage;

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
  Result := Find(KnownIDEPackages);
  if Result = nil then
    Result := Find(KnownPackages);
end;

function TIDEPersonality.GetBplDir: string;
begin
  Result := IDE.ExpandDirMacros(BPLOutputDir);
end;

function TIDEPersonality.GetDcpDir: string;
begin
  Result := IDE.ExpandDirMacros(DCPOutputDir);
end;

procedure TIDEPersonality.LoadPackagesFromRegistry(APackageList: TDelphiPackageList;
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
    if Reg.OpenKeyReadOnly(IDE.RegistryKey + '\' + SubKey) then
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

procedure TIDEPersonality.SavePackagesToRegistry(APackageList: TDelphiPackageList;
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
    if Reg.OpenKey(IDE.RegistryKey + '\' + SubKey, False) then
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

{ TDelphiPersonality }

procedure TDelphiPersonality.LoadFromRegistry;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    // get library paths
    if Reg.OpenKeyReadOnly(IDE.RegistryKey + '\Library') then // do not localize
    begin
      FDCPOutputDir := ExcludeTrailingPathDelimiter(Reg.ReadString('Package DCP Output')); // do not localize
      FBPLOutputDir := ExcludeTrailingPathDelimiter(Reg.ReadString('Package DPL Output')); // do not localize
      ConvertPathList(Reg.ReadString('Browsing Path'), FBrowsingPaths); // do not localize
      ConvertPathList(Reg.ReadString('Search Path'), FSearchPaths); // do not localize
      if IDE.IsBDS then
      begin
        ConvertPathList(Reg.ReadString('Namespace Search Path'), FNamespaceSearchPaths); // do not localize
        ConvertPathList(Reg.ReadString('Package Search Path'), FPackageSearchPaths); // do not localize
        ConvertPathList(Reg.ReadString('Debug DCU Path'), FDebugDcuPaths); // do not localize
      end;
    end;
    if not IDE.IsBDS then
    begin
      if Reg.OpenKeyReadOnly(IDE.RegistryKey + '\Debugging') then // do not localize
        ConvertPathList(Reg.ReadString('Debug DCUs Path'), FDebugDcuPaths); // do not localize
      if Reg.OpenKeyReadOnly(IDE.RegistryKey + '\Environment Variables') then // do not localize
        ConvertPathList(Reg.ReadString('Path'), FPackageSearchPaths); // do not localize
    end;
  finally
    Reg.Free;
  end;

  LoadPackagesFromRegistry(FKnownIDEPackages, 'Known IDE Packages'); // do not localize
  LoadPackagesFromRegistry(FKnownPackages, 'Known Packages'); // do not localize
  LoadPackagesFromRegistry(FDisabledPackages, 'Disabled Packages'); // do not localize
end;

procedure TDelphiPersonality.SavePaths;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(IDE.RegistryKey + '\Library', False) then // do not localize
    begin
      Reg.WriteString('Browsing Path', ConvertPathList(FBrowsingPaths)); // do not localize
      Reg.WriteString('Search Path', ConvertPathList(FSearchPaths)); // do not localize
      if IDE.IsBDS then
      begin
        Reg.WriteString('Namespace Search Path', ConvertPathList(FNamespaceSearchPaths)); // do not localize
        Reg.WriteString('Package Search Path', ConvertPathList(FPackageSearchPaths)); // do not localize
        Reg.WriteString('Debug DCU Path', ConvertPathList(FDebugDcuPaths)); // do not localize
      end;
    end;
    if not IDE.IsBDS then
    begin
      if Reg.OpenKey(IDE.RegistryKey + '\Debugging', True) then // do not localize
        Reg.WriteString('Debug DCUs Path', ConvertPathList(FDebugDcuPaths)); // do not localize
      if (FPackageSearchPaths.Count > 0) and Reg.OpenKey(IDE.RegistryKey + '\Environment Variables', True) then // do not localize
        Reg.WriteString('Path', ConvertPathList(FPackageSearchPaths)); // do not localize
    end;
  finally
    Reg.Free;
  end;
end;

procedure TDelphiPersonality.SavePackagesLists;
begin
  SavePackagesToRegistry(FKnownPackages, 'Known Packages'); // do not localize
  SavePackagesToRegistry(FDisabledPackages, 'Disabled Packages'); // do not localize
  SavePackagesToRegistry(FKnownIDEPackages, 'Known IDE Packages'); // do not localize
end;

{ TCppPersonality }

constructor TCppPersonality.Create(const AName: string; IDE: TDelphiIDE);
begin
  inherited Create(AName, IDE);
end;

procedure TCppPersonality.SavePaths;
begin
  inherited SavePaths
end;

procedure TCppPersonality.LoadFromRegistry;
begin
  inherited LoadFromRegistry
end;

procedure TCppPersonality.SavePackagesLists;
begin
  inherited SavePackagesLists
end;

{ TDelphiCppPersonality }

constructor TDelphiCppPersonality.Create(const AName: string; IDE: TDelphiIDE);
begin
  inherited Create(AName, IDE);
end;

procedure TDelphiCppPersonality.SavePaths;
begin
  inherited SavePaths
end;

procedure TDelphiCppPersonality.LoadFromRegistry;
begin
  inherited LoadFromRegistry
end;

procedure TDelphiCppPersonality.SavePackagesLists;
begin
  inherited SavePackagesLists
end;

{ TDotNetPersonality }

procedure TDotNetPersonality.SavePaths;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(IDE.RegistryKey + '\Library .NET', False) then // do not localize
    begin
      Reg.WriteString('Browsing Path', ConvertPathList(FBrowsingPaths)); // do not localize
      Reg.WriteString('Search Path', ConvertPathList(FSearchPaths)); // do not localize
      Reg.WriteString('Namespace Search Path', ConvertPathList(FNamespaceSearchPaths)); // do not localize
      Reg.WriteString('Package Search Path', ConvertPathList(FPackageSearchPaths)); // do not localize
      Reg.WriteString('Debug DCUIL Path', ConvertPathList(FDebugDcuPaths)); // do not localize
    end;
  finally
    Reg.Free;
  end;
end;

procedure TDotNetPersonality.LoadFromRegistry;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    // get library paths
    if Reg.OpenKeyReadOnly(IDE.RegistryKey + '\Library .NET') then // do not localize
    begin
      FDCPOutputDir := ExcludeTrailingPathDelimiter(Reg.ReadString('Package DCPIL Output')); // do not localize
      FBPLOutputDir := ExcludeTrailingPathDelimiter(Reg.ReadString('Package DPL Output')); // do not localize
      ConvertPathList(Reg.ReadString('Namespace Search Path'), FNamespaceSearchPaths); // do not localize
      ConvertPathList(Reg.ReadString('Browsing Path'), FBrowsingPaths); // do not localize
      ConvertPathList(Reg.ReadString('Package Search Path'), FPackageSearchPaths); // BDS // do not localize
      ConvertPathList(Reg.ReadString('Search Path'), FSearchPaths); // do not localize
      ConvertPathList(Reg.ReadString('Debug DCUIL Path'), FDebugDcuPaths); // do not localize
    end;
  finally
    Reg.Free;
  end;

  LoadPackagesFromRegistry(FKnownIDEPackages, 'Known IDE Assemblies'); // do not localize
  LoadPackagesFromRegistry(FKnownPackages, 'Known Assemblies'); // do not localize
  LoadPackagesFromRegistry(FDisabledPackages, 'Disabled Assemblies'); // do not localize
end;

end.
