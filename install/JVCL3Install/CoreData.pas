{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: CoreData.pas, released on 2003-11-27.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-12-07

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

{.$define DoNotTouchRegistry}

unit CoreData;
interface
uses
  Windows, SysUtils, Classes, Contnrs, Registry, JvSimpleXML;

type
  TPackageList = class;
  TTargetList = class;

  TTargetInfo = class(TObject)
  private
    FTargetList: TTargetList;
    FPackages: TPackageList;

    FRegKey: string;
    FExecutable: string;
    FProductName: string;
    FVersion: string;
    FRootDir: string;
    FVersionType: string;
    FLastUpdate: Integer; // 0: no Update, 1: Update #1, 2: Update #2, ...
    FDcpDir: string;
    FBplDir: string;
    FSearchPaths: string;
    FKnownPackages: TStrings;
    FIsJVCLInstalled: Boolean;
    FIsJCLInstalled: Boolean;
    FJCLDir: string;

    FCompileFor: Boolean;
    FNeedsUpdate: Boolean;
    FClearJVCLPalette: Boolean;
    FBuild: Boolean;
    FDeveloperInstall: Boolean;
    FInstallJcl: Boolean;
    FJCLNeedsDcp: Boolean;
    FCompileOnly: Boolean;
    FHppFilesDir: string;
    FMoveHppFiles: Boolean;

    function GetSymbol: string;
    function GetMajorVersion: Integer;
    function GetIsPersonal: Boolean;
    function GetBpgName: string;
    function GetLibDir: string;
    function GetDisplayName: string;
    function GetIsOldJVCLInstalled: Integer;

    function AddRemovePaths(const Paths: string; const NewPaths: array of string;
      Add: Boolean): string;
    function GetJclDirName: string;
    function GetIsBCB: Boolean;
    function GetIsDelphi: Boolean;
    function GetJVCLDirName: string;
    procedure AddRemoveJCLPaths(Add: Boolean);
    function GetJCLPackageDir: string;
    function GetJCLPackageXmlDir: string;
  protected
    procedure ClearPalette(reg: TRegistry; const Name: string);
    procedure DoClearJVCLPalette;
    procedure GetKnownPackages;
    procedure UpdateKnownPackage;
    procedure AddRemoveJVCLPaths(Add: Boolean);

    procedure ReadData;
  public
    constructor Create(ATargetList: TTargetList);
    destructor Destroy; override;

    procedure RegistryInstall;
    procedure RegistryUninstall;
    procedure UninstallOldJVCL;

    procedure JclRegistryInstall;

    function ExpandDirMacros(const Path: string): string;
    function InsertDirMacros(const Dir: string): string;
    function IsTargetFor(const Targets: string): Boolean;

    procedure SetJCLDir(const NewDir: string);

    property Executable: string read FExecutable;
    property RegKey: string read FRegKey;
    property ProductName: string read FProductName;
    property Version: string read FVersion;
    property MajorVersion: Integer read GetMajorVersion;
    property RootDir: string read FRootDir;
    property IsPersonal: Boolean read GetIsPersonal;
    property VersionType: string read FVersionType;
    property LastUpdate: Integer read FLastUpdate;
    property DisplayName: string read GetDisplayName;
    property IsBCB: Boolean read GetIsBCB;
    property IsDelphi: Boolean read GetIsDelphi;
    property Symbol: string read GetSymbol;
    property BpgName: string read GetBpgName;
    property JclDirName: string read GetJclDirName; // c5, c6, d5, d6, d7(, k3)
    property JVCLDirName: string read GetJVCLDirName; // directory in Packages\
    property LibDir: string read GetLibDir; // relative to $(JVCL)

    property JCLDir: string read FJCLDir; // directory where JCL is installed or the global JCLDir
    property JCLPackageDir: string read GetJCLPackageDir;
    property JCLPackageXmlDir: string read GetJCLPackageXmlDir;
    property JCLNeedsDcp: Boolean read FJCLNeedsDcp;
    property SearchPaths: string read FSearchPaths;
    property BplDir: string read FBplDir;
    property DcpDir: string read FDcpDir;
    property KnownPackages: TStrings read FKnownPackages;
    property IsJVCLInstalled: Boolean read FIsJVCLInstalled;
    property IsOldJVCLInstalled: Integer read GetIsOldJVCLInstalled; // JVCL 1 or 2
    property IsJCLInstalled: Boolean read FIsJCLInstalled;

    property CompileFor: Boolean read FCompileFor write FCompileFor;
    property NeedsUpdate: Boolean read FNeedsUpdate write FNeedsUpdate;
    property ClearJVCLPalette: Boolean read FClearJVCLPalette write FClearJVCLPalette;
    property Build: Boolean read FBuild write FBuild;
    property DeveloperInstall: Boolean read FDeveloperInstall write FDeveloperInstall;
    property InstallJcl: Boolean read FInstallJcl write FInstallJcl;
    property CompileOnly: Boolean read FCompileOnly write FCompileOnly;
    property HppFilesDir: string read FHppFilesDir write FHppFilesDir;
    property MoveHppFiles: Boolean read FMoveHppFiles write FMoveHppFiles;

    property Packages: TPackageList read FPackages;
    property TargetList: TTargetList read FTargetList;
  end;

  // installed Delphi and BCB Targets
  TTargetList = class(TObject)
  private
    FItems: TObjectList;
    function GetItems(Index: Integer): TTargetInfo;
    function GetCount: Integer;
  protected
    procedure ReadTargetData(const Key, Name, Version: string);
    procedure GetTargets; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TTargetInfo read GetItems; default;
  end;

  TRequirePkg = class(TObject)
  private
    FName: string;
    FTargets: string;
    FCondition: string;
  public
    constructor Create(const AName, ATargets, ACondition: string);
    property Name: string read FName;
    property Targets: string read FTargets;
    property Condition: string read FCondition;
  end;

  TContainsFile = class(TObject)
  private
    FName: string;
    FTargets: string;
    FFormName: string;
    FCondition: string;
  public
    constructor Create(const AName, ATargets, AFormName, ACondition: string);
    property Name: string read FName;
    property Targets: string read FTargets;
    property FormName: string read FFormName;
    property Condition: string read FCondition;
  end;

  TPackageInfo = class(TObject)
  private
    FPackageList: TPackageList;
    FXmlDir: string; // depends on IsJclPackage and set in Create

    FName: string;
    FDisplayName: string;
    FDescription: string;
    FRequires: TObjectList;
    FContains: TObjectList;
    FRequiresDB: Boolean;
    FIsInstalled: Boolean;
    FIsDesign: Boolean;

    FInstall: Boolean;

    procedure SetInstall(const Value: Boolean);
    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): TRequirePkg;
    function GetTarget: TTargetInfo;
    function GetBplName: string;
    function GetContainCount: Integer;
    function GetContains(Index: Integer): TContainsFile;
  protected
    procedure ReadXmlPackage;
  public
    constructor Create(const AName, AXmlDir: string; APackageList: TPackageList);
    destructor Destroy; override;

    function DependsOn(PackageInfo: TPackageInfo): Boolean;
    procedure UpdateRuntimePackages;

    property Name: string read FName;
    property DisplayName: string read FDisplayName;
    property BplName: string read GetBplName;
    property Description: string read FDescription;
    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: TRequirePkg read GetRequires;
    property RequiresDB: Boolean read FRequiresDB;
    property ContaionCount: Integer read GetContainCount;
    property Contains[Index: Integer]: TContainsFile read GetContains;
    property IsInstalled: Boolean read FIsInstalled;
    property IsDesign: Boolean read FIsDesign;

    property Install: Boolean read FInstall write SetInstall;

    property Target: TTargetInfo read GetTarget;
    property PackageList: TPackageList read FPackageList;
  end;

  TPackageList = class(TObject)
  private
    FTarget: TTargetInfo;
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TPackageInfo;
  public
    constructor Create(ATarget: TTargetInfo);
    destructor Destroy; override;

    procedure ReadPackages;
    function FindPackage(const Name: string): TPackageInfo;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPackageInfo read GetItems; default;
    property Target: TTargetInfo read FTarget;
  end;

var
  TargetList: TTargetList;

  JVCLDir: string;
  JVCLPackageDir: string;
  JVCLPackageXmlDir: string;

  JCLPairInstallation: Boolean; // JCL source is in $(JVCL)..\JCL
  xJCLDir: string;
  xJCLPackageDir: string;
  xJCLPackageXmlDir: string; // only runtime packages

procedure SplitPaths(const Paths: string; List: TStrings);
function CombinePaths(List: TStrings): string;
function StartsWith(const Text, StartText: string; CaseInsensitive: Boolean = False): Boolean;
function EndsWith(const Text, EndText: string; CaseInsensitive: Boolean): Boolean;
function ReadStringFromFile(const Filename: string): string;
function SubStr(const Text: string; StartIndex, EndIndex: Integer): string;
function IsDelphiRunning: Boolean;
function FindCmdSwitch(const Switch: string): Boolean;

function MoveFile(const Source, Dest: string): Boolean;
{$IFNDEF COMPILER6_UP}
function DirectoryExists(const Dir: string): Boolean;
{$ENDIF}

function CreateJclPackageList(Target: TTargetInfo): TPackageList;

implementation

uses Math;

const
  cDelphiKeyName = 'SOFTWARE\Borland\Delphi';
  cBCBKeyName = 'SOFTWARE\Borland\C++Builder';

  cComponentPalettePrefix = 'TJv';

  cJVCLDirList: array[Boolean, 0..2] of PChar = (
    ('$(JVCL)\common', '$(JVCL)\libX', ''), // no developer install (libX is replaced)
    ('$(JVCL)\common', '$(JVCL)\run', '$(JVCL)\design') // developer install
  );
  cJVCLDirBrowseList: array[0..1] of PChar = (
    '$(JVCL)\run', '$(JVCL)\design'
  );

  cJCLDirList: array[0..0] of PChar = (
    '$(JCL)\lib\xx\obj' // xx is replaced
  );
  cJCLDirBrowseList: array[0..3] of PChar = (
    '$(JCL)\source\common', '$(JCL)\source\vcl', '$(JCL)\source\visclx',
    '$(JCL)\source\windows'
  );

function GetPackageGroupDir(Target: TTargetInfo): string;
begin
  with Target do
  begin
    if IsDelphi then
      Result := 'D' + IntToStr(MajorVersion)
    else
      Result := 'BCB' + IntToStr(MajorVersion);

    if IsPersonal then
      if MajorVersion >= 6 then
        Result := Result + 'Per'
      else
        Result := Result + 'Std';
  end;
end;

function GetPackageGroupName(Target: TTargetInfo): string;
begin
  Result := GetPackageGroupDir(Target) + ' Packages.bpg';
end;

function GetBplNameFrom(const PkgName: string; Target: TTargetInfo; DesignPackage: Boolean): string;
const
  Design: array[Boolean] of Char = ('R', 'D');
begin
  if Target.IsDelphi then
    Result := PkgName + 'D' + IntToStr(Target.MajorVersion) + Design[DesignPackage] + '.bpl'
  else
    Result := PkgName + 'C' + IntToStr(Target.MajorVersion) + Design[DesignPackage] + '.bpl';
end;

// -----------------------------------------------------------------------------

function CreateJclPackageList(Target: TTargetInfo): TPackageList;
var
  Item: TPackageInfo;
  LibVer: string;
begin
  Result := TPackageList.Create(Target);
  try
   // prefix is added in BuildHelpers.CreateDelphiPackageForBCB
    if Target.MajorVersion < 6 then LibVer := '50' else LibVer := '';

    Item := TPackageInfo.Create('Jcl' + LibVer + '-R', Target.JCLPackageXmlDir, Result);
    Result.FItems.Add(Item);

    if Target.MajorVersion > 5 then
    begin
      Item := TPackageInfo.Create('JclVcl-R', Target.JCLPackageXmlDir, Result);
      Result.FItems.Add(Item);

      Item := TPackageInfo.Create('JclVClx-R', Target.JCLPackageXmlDir, Result);
      Result.FItems.Add(Item);
    end;
  except
    Result.Free;
    raise;
  end;
end;


function FindCmdSwitch(const Switch: string): Boolean;
var i: Integer;
begin
  Result := True;
  for i := 1 to ParamCount do
    if CompareText(Switch, ParamStr(i)) = 0 then
      Exit;
  Result := False;
end;

function IsUNCPath(const Filename: string): Boolean;
begin
  Result := Copy(Filename, 1, 2) = '\\';
end;

function GetUNCShare(const Filename: string): string;
var nps: Integer;
begin
  Result := '';
  if Filename = '' then exit;
  if IsUNCPath(Filename) then
  begin
    Result := Copy(Filename, 3, Length(Filename));
    nps := pos('\', Result);
    Delete(Result, 1, nps);
    Result := Copy(Result, 1, pos('\', Result) - 1);
    Result := Copy(Filename, 1, nps + 2) + Result;
   end else Result := Copy(Filename, 1, 2);
end;

function CopyOneFile(const SourceFileName, DestFileName: string): Boolean;
begin
  Result := Windows.CopyFile(PChar(SourceFileName), PChar(DestFileName), False);
end;

function MoveFile(const Source, Dest: string): Boolean;
var CopyNecessary: Boolean;
begin
  Result := False;
  if (Source = '') or (Dest = '') or (not FileExists(Source)) then Exit;
  if CompareText(Source, Dest) = 0 then
  begin
    Result := True;
    Exit;
  end;

  //ForceDirectories(ExtractFilePath(Dest));
  if FileExists(Dest) then
  begin
    SetFileAttributes(PChar(Dest), 0);
    DeleteFile(Dest);
  end;

  CopyNecessary := True;
  if CompareText(GetUNCShare(Source), GetUNCShare(Dest)) = 0 then
     CopyNecessary := not RenameFile(Source, Dest);

  if CopyNecessary then
  begin
    if CopyOneFile(Source, Dest) then
    begin
      SetFileAttributes(PChar(Source), 0);
      DeleteFile(Source);
      Result := True;
    end;
  end
  else
    Result := True;
end;

{$IFNDEF COMPILER6_UP}
function DirectoryExists(const Dir: string): Boolean;
var Attr: Integer;
begin
  Attr := Integer(GetFileAttributes(PChar(Dir)));
  Result := (Attr <> -1) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;
{$ENDIF}

function IsVersionNumber(const S: string): Boolean;
var
  ps, i: Integer;
begin // 'xx.yy'
  Result := False;
  ps := Pos('.', S);
  if ps = 0 then Exit;
  for i := 1 to ps - 1 do
    if not (S[i] in ['0'..'9']) then
      Exit;
  for i := ps + 1 to Length(S) do
    if not (S[i] in ['0'..'9']) then
      Exit;
  Result := True;
end;

procedure SplitPaths(const Paths: string; List: TStrings);
var
  S: string;
  F, P: PChar;
begin
  P := PChar(Paths);
  while P[0] <> #0 do
  begin
    F := P;
    while not (P[0] in [#0, ';']) do Inc(P);
    SetString(S, F, P - F);
    S := Trim(S);
    if S <> '' then
      List.Add(S);
    if P^ <> #0 then
      Inc(P);
  end;
end;

function CombinePaths(List: TStrings): string;
var i: Integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
    Result := Result + List[i] + ';';
  SetLength(Result, Length(Result) - 1);
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

function ReadStringFromFile(const Filename: string): string;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.Read(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

function SubStr(const Text: string; StartIndex, EndIndex: Integer): string;
begin
  Result := Copy(Text, StartIndex, EndIndex - StartIndex + 1);
end;


{ TTargetInfo }

constructor TTargetInfo.Create(ATargetList: TTargetList);
begin
  inherited Create;
  FTargetList := ATargetList;
  FKnownPackages := TStringList.Create;
  FPackages := TPackageList.Create(Self);

  FClearJVCLPalette := True;
end;

destructor TTargetInfo.Destroy;
begin
  FKnownPackages.Free;
  FPackages.Free;
  inherited;
end;

procedure TTargetInfo.ClearPalette(reg: TRegistry; const Name: string);
var
  Entries, S: string;
  List: TStrings;
  i, ps: Integer;
begin
  Entries := reg.ReadString(Name);
  List := TStringList.Create;
  try
    ps := 0;
    for i := 1 to Length(Entries) do
      if Entries[i] = ';' then
      begin
        List.Add(SubStr(Entries, ps + 1, i - 1));
        ps := i;
      end;
    if ps < Length(Entries) then
      List.Add(SubStr(Entries, ps + 1, Length(Entries)));

    for i := List.Count - 1 downto 0 do
    begin
      ps := Pos('.', List[i]); // for Delphi 6,7 and BCB 6
      if Copy(List[i], ps + 1, 3) = cComponentPalettePrefix then
        List.Delete(i);
    end;

    S := '';
    for i := 0 to List.Count - 1 do
      S := S + List[i] + ';';
    // last char is ';'

    if S <> Entries then
    {$ifndef DoNotTouchRegistry}
      reg.WriteString(Name, S);
    {$else}
      OutputDebugString(PChar('reg.WriteString(''' + Name + ''', ''' + S + ''')'));
    {$endif}
  finally
    List.Free;
  end;
end;

procedure TTargetInfo.DoClearJVCLPalette;
var
  i: Integer;
  reg: TRegistry;
  List: TStrings;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(RegKey + '\Palette', False) then
    begin
      List := TStringList.Create;
      try
        reg.GetValueNames(List);
        for i := 0 to List.Count - 1 do
          ClearPalette(reg, List[i]);
      finally
        List.Free;
      end;
    end;
  finally
    reg.Free;
  end;
end;

procedure TTargetInfo.UpdateKnownPackage;
var
  i: Integer;
  PkgIndex: Integer;
  PkgName: string;
  reg: TRegistry;
begin
  GetKnownPackages; // update the list

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(RegKey + '\Known Packages', False) then
    begin

      // remove unused packages
      for i := 0 to FKnownPackages.Count - 1 do
      begin
        PkgName := ExtractFileName(FKnownPackages[i]);
        for PkgIndex := 0 to Packages.Count - 1 do
        begin
          if CompareText(Packages[PkgIndex].BplName, PkgName) = 0 then
          begin
            if not Packages[PkgIndex].Install then
            {$ifndef DoNotTouchRegistry}
              reg.DeleteValue(FKnownPackages[i]);
            {$else}
              OutputDebugString(PChar('reg.DeleteValue(''' + FKnownPackages[i] + ''')'));
            {$endif}
          end;
        end;
      end;

      for i := 0 to Packages.Count - 1 do
      begin
        if not Packages[i].IsDesign then Continue;
        if (not Packages[i].IsInstalled) and (Packages[i].Install) then
        {$ifndef DoNotTouchRegistry}
          reg.WriteString(FBplDir + '\' + Packages[i].BplName, Packages[i].Description);
        {$else}
          OutputDebugString(PChar('reg.WriteString(''' + FBplDir + '\' + Packages[i].BplName + ''', ''' + Packages[i].Description + ''')'));
        {$endif}
      end;
    end;
  finally
    reg.Free;
  end;

  GetKnownPackages; // update the list
end;

function TTargetInfo.AddRemovePaths(const Paths: string; const NewPaths: array of string; Add: Boolean): string;
var
  i, DirIndex: Integer;
  List: TStrings;
  Found: Boolean;
begin
  List := TStringList.Create;
  try
    SplitPaths(Paths, List);
    for DirIndex := 0 to High(NewPaths) do
    begin
      Found := False;
      for i := List.Count - 1 downto 0 do
        if (CompareText(ExpandDirMacros(List[i]), NewPaths[DirIndex]) = 0) then
        begin
          Found := True;
          if not Add then
            List.Delete(i);
          Break;
        end;
      if (not Found) and (Add) then
      begin
        if StartsWith(NewPaths[DirIndex], RootDir + '\', True) then
        begin
          if IsDelphi then
            List.Add('$(DELPHI)' + Copy(NewPaths[DirIndex], 1 + Length(RootDir), MaxInt))
          else               
            List.Add('$(BCB)' + Copy(NewPaths[DirIndex], 1 + Length(RootDir), MaxInt));
        end
        else
          List.Add(NewPaths[DirIndex]);
      end;
    end;
    Result := CombinePaths(List);
  finally
    List.Free;
  end;
end;

procedure TTargetInfo.AddRemoveJVCLPaths(Add: Boolean);
var
  reg: TRegistry;
  S, Paths: string;
  i: Integer;
  JVCLDirList: array of string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(FRegKey + '\Library', True) then
    begin

      if reg.ValueExists('Search Path') then
        Paths := reg.ReadString('Search Path');

      SetLength(JVCLDirList, Length(cJVCLDirList[False]));
      for i := 0 to High(JVCLDirList) do
        JVCLDirList[i] := ExpandDirMacros(cJVCLDirList[FDeveloperInstall, i]);
      if not FDeveloperInstall then // replace $(JVCL)\libX
        JVCLDirList[1] := ExpandDirMacros('$(JVCL)\' + LibDir);

      S := AddRemovePaths(Paths, JVCLDirList, Add);
      if S <> Paths then
      {$ifndef DoNotTouchRegistry}
        reg.WriteString('Search Path', S);
      {$else}
        OutputDebugString(PChar('reg.WriteString(''Search Path'', ''' + S + ''')'));
      {$endif}

     // ------

      if reg.ValueExists('Browsing Path') then
        Paths := reg.ReadString('Browsing Path');

      SetLength(JVCLDirList, Length(cJVCLDirBrowseList));
      for i := 0 to High(JVCLDirList) do
        JVCLDirList[i] := ExpandDirMacros(cJVCLDirBrowseList[i]);

      S := AddRemovePaths(Paths, JVCLDirList, Add);
      if S <> Paths then
      {$ifndef DoNotTouchRegistry}
        reg.WriteString('Browsing Path', S);
      {$else}
        OutputDebugString(PChar('reg.WriteString(''Browsing Path'', ''' + S + ''')'));
      {$endif}

      reg.CloseKey;
    end;

  finally
    reg.Free;
  end;
end;

procedure TTargetInfo.AddRemoveJCLPaths(Add: Boolean);
var
  reg: TRegistry;
  S, Paths: string;
  i: Integer;
  JCLDirList: array of string;
begin
  if not JCLPairInstallation then Exit;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(FRegKey + '\Library', True) then
    begin

      if reg.ValueExists('Search Path') then
        Paths := reg.ReadString('Search Path');

      SetLength(JCLDirList, Length(cJCLDirList));
      for i := 0 to High(JCLDirList) do
        JCLDirList[i] := ExpandDirMacros(cJCLDirList[i]);
      JCLDirList[0] := ExpandDirMacros('$(JCL)\lib\' + JclDirName + '\obj');

      S := AddRemovePaths(Paths, JCLDirList, Add);
      if S <> Paths then
      {$ifndef DoNotTouchRegistry}
        reg.WriteString('Search Path', S);
      {$else}
        OutputDebugString(PChar('reg.WriteString(''Search Path'', ''' + S + ''')'));
      {$endif}

     // ------

      if reg.ValueExists('Browsing Path') then
        Paths := reg.ReadString('Browsing Path');

      SetLength(JCLDirList, Length(cJCLDirBrowseList));
      for i := 0 to High(JCLDirList) do
        JCLDirList[i] := ExpandDirMacros(cJCLDirBrowseList[i]);

      S := AddRemovePaths(Paths, JCLDirList, Add);
      if S <> Paths then
      {$ifndef DoNotTouchRegistry}
        reg.WriteString('Browsing Path', S);
      {$else}
        OutputDebugString(PChar('reg.WriteString(''Browsing Path'', ''' + S + ''')'));
      {$endif}

      reg.CloseKey;
    end;

  finally
    reg.Free;
  end;
end;

function TTargetInfo.ExpandDirMacros(const Path: string): string;
var
  i, EndPs: Integer;
  S, NewS: string;
begin
  Result := Path;
  i := 1;
  while i < Length(Result) do
  begin
    if (Result[i] = '$') and (Result[i + 1] = '(') then
    begin
      EndPs := i + 2;
      while (EndPs <= Length(Result)) and (Result[EndPs] <> ')') do Inc(EndPs);
      S := AnsiLowerCase(SubStr(Result, i + 2, EndPs - 1));
      NewS := S;

     // available macros
      if (S = 'delphi') or (S = 'bcb') then
        NewS := FRootDir

      else if S = 'jvcl' then // used internally
        NewS := JVCLDir
      else if S = 'jcl' then  // used internally
        NewS := JCLDir;

      if NewS <> S then
      begin
        Delete(Result, i, EndPs - i + 1);
        Insert(NewS, Result, i);
        Inc(i, Length(NewS) - 1);
        NewS := '';
      end;
    end;
    Inc(i);
  end;
end;

function TTargetInfo.InsertDirMacros(const Dir: string): string;
begin
  Result := Dir + '\';
  if StartsWith(Dir, RootDir + '\', True) then
  begin
    if IsDelphi then
      Result := '$(DELPHI)'
    else
      Result := '$(BCB)';
    Result := Result + Copy(Dir, Length(RootDir) + 1, MaxInt);
  end
  else if StartsWith(Dir, JVCLDir + '\', True) then
  begin
    Result := '$(JVCL)' + Copy(Dir, Length(JVCLDir) + 1, MaxInt);
  end
  else if (JCLDir <> '') and StartsWith(Dir, JCLDir + '\', True) then
  begin
    Result := '$(JCL)' + Copy(Dir, Length(JCLDir) + 1, MaxInt);
  end
end;

function TTargetInfo.GetBpgName: string;
begin
  Result := GetPackageGroupName(Self);
end;

function TTargetInfo.GetIsPersonal: Boolean;
begin
  Result := (CompareText(FVersionType, 'PER') = 0) or
            (CompareText(FVersionType, 'STD') = 0) or
            (CompareText(FVersionType, 'PERS') = 0);
end;

function TTargetInfo.GetMajorVersion: Integer;
begin
  Result := StrToInt(Copy(FVersion, 1, Pos('.', FVersion) - 1)); // major Version
end;

function TTargetInfo.GetSymbol: string;
begin
  if IsDelphi then Result := 'D' else Result := 'C';
  Result := Result + IntToStr(MajorVersion);

  if IsPersonal then
  begin
    if Result[2] = '5' then
      Result := Result + 's'
    else
      Result := Result + 'p';
  end;
end;

procedure TTargetInfo.GetKnownPackages;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    FKnownPackages.Clear;
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly(FRegKey + '\Known Packages') then
    begin
      reg.GetValueNames(FKnownPackages);
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure TTargetInfo.RegistryInstall;
var
  i: Integer;
begin
  if FCompileOnly then Exit;

  AddRemoveJVCLPaths(True);
  if FClearJVCLPalette then
    DoClearJVCLPalette;
  UpdateKnownPackage;
  for i := 0 to Packages.Count - 1 do
    Packages[i].FIsInstalled := Packages[i].Install;
  FIsJVCLInstalled := True;
 // do not call ReadData here
end;

procedure TTargetInfo.RegistryUninstall;
var i: Integer;
begin
  if FCompileOnly then Exit;

  AddRemoveJVCLPaths(False);
  DoClearJVCLPalette;
  for i := 0 to Packages.Count - 1 do
  begin
    Packages[i].Install := False;
    Packages[i].FIsInstalled := False;
  end;
  UpdateKnownPackage;
  FIsJVCLInstalled := False;
 // do not call ReadData here
end;

procedure TTargetInfo.JclRegistryInstall;
begin
  if FCompileOnly then Exit;

  if JCLDir <> '' then
    AddRemoveJCLPaths(True);
end;

function TTargetInfo.GetJclDirName: string;
begin
  if IsDelphi then
    Result := 'd' + IntToStr(MajorVersion)
  else
    Result := 'c' + IntToStr(MajorVersion);
end;

function TTargetInfo.GetLibDir: string;
begin
  Result := 'lib' + IntToStr(MajorVersion);
end;

procedure TTargetInfo.ReadData;
var
  reg: TRegistry;
  i: Integer;
  JclFileName, JclFileNameNoVer: string;
  List: TStrings;
begin
  reg := TRegistry.Create;
  try
   // ****
   // HKEY_LOCAL_MACHINE:
   // ****
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly(FRegKey) then
    begin
      if reg.ValueExists('App') then
        FExecutable := reg.ReadString('App');
      FRootDir := reg.ReadString('RootDir');
      FVersionType := reg.ReadString('Version');

     // what Updates are installed? (max. 10)
      FLastUpdate := 0;
      for i := 1 to 10 do
        if reg.ValueExists(Format('Update #%d', [i])) then
          FLastUpdate := i;

      reg.CloseKey;

     // ****
     // HKEY_CURRENT_USER:
     // ****
      reg.RootKey := HKEY_CURRENT_USER;
      if reg.OpenKeyReadOnly(FRegKey + '\Library') then
      begin
        if reg.ValueExists('Package DCP Output') then
          FDcpDir := ExpandDirMacros(reg.ReadString('Package DCP Output'))
        else
          FDcpDir := RootDir + '\Projects\Bpl';
        if FDcpDir = '' then // no directory
          FDcpDir := RootDir + '\Projects\Bpl';

        if reg.ValueExists('Package DPL Output') then
          FBplDir := ExpandDirMacros(reg.ReadString('Package DPL Output'))
        else
          FBplDir := RootDir + '\Projects\Bpl';
        if FBplDir = '' then // no directory
          FBplDir := RootDir + '\Projects\Bpl';

        if reg.ValueExists('Search Path') then
          FSearchPaths := ExpandDirMacros(reg.ReadString('Search Path'))
        else
          FSearchPaths := ExpandDirMacros('$(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;$(DELPHI)\Projects\Bpl');
        reg.CloseKey;
      end;
    end;
  finally
    reg.Free;
  end;
  GetKnownPackages;

  FCompileFor := True;

 // check for JCL:
  if IsDelphi then
    JclFileName := 'DJcl' + IntToStr(MajorVersion) + '0'
  else
    JclFileName := 'CJcl' + IntToStr(MajorVersion) + '0';
  JclFileNameNoVer := Copy(JclFileName, 1, Length(JclFileName) - 2);
  FIsJCLInstalled := FileExists(FBplDir + '\' + JclFileName + '.bpl');
  FJCLNeedsDcp := not (
    FileExists(FBplDir + '\' + JclFileName + '.dcp') or
    FileExists(FBplDir + '\' + JclFileNameNoVer + '.dcp') or
    FileExists(FDcpDir + '\' + JclFileName + '.dcp') or
    FileExists(FDcpDir + '\' + JclFileNameNoVer + '.dcp')
  );

 // find JCL directory or use the JCL shipped with this JVCL (if available) 
  FJCLDir := xJCLDir;
  if FIsJCLInstalled then
  begin
    List := TStringList.Create;
    try
      SplitPaths(FSearchPaths, List);
      for i := 0 to List.Count - 1 do
        if EndsWith(List[i], '\lib\' + JclDirName + '\obj', True) then
        begin
          FJCLDir := ExpandDirMacros(List[i]);
          FJCLDir := ExtractFileDir(FJCLDir); // -> $(JCL)\lib\xx
          FJCLDir := ExtractFileDir(FJCLDir); // -> $(JCL)\lib
          FJCLDir := ExtractFileDir(FJCLDir); // -> $(JCL)
          Break;
        end
        else if EndsWith(List[i], '\jcl\source\common', True) then
        begin
          FJCLDir := ExpandDirMacros(List[i]);
          FJCLDir := ExtractFileDir(FJCLDir); // -> $(JCL)\source
          FJCLDir := ExtractFileDir(FJCLDir); // -> $(JCL)
          Break;
        end;
    finally
      List.Free;
    end;
  end;

 // Jcl is not installed and we have a JCL directory
  FInstallJcl := ((not IsJCLInstalled) or (IsOldJVCLInstalled <> 0))
                 {and (JCLPairInstallation)};

  FCompileFor := FCompileFor and not ((not FIsJCLInstalled) and (not JCLPairInstallation));

  FHppFilesDir := FRootDir + '\Include\Vcl';
  FMoveHppFiles := True;
end;

function TTargetInfo.GetDisplayName: string;
begin
  Result := FProductName + ' ' + FVersion + ' ' + FVersionType;
end;

procedure TTargetInfo.UninstallOldJVCL;
var
  reg: TRegistry;
  List: TStrings;
  i: Integer;
  Paths, S: string;
  OldDir: string;
  VerName: string;
begin
  if IsOldJVCLInstalled = 0 then
    Exit;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
   // remove paths
    if reg.OpenKey(FRegKey + '\Library', True) then
    begin
      if reg.ValueExists('Search Path') then
        Paths := reg.ReadString('Search Path');
      OldDir := '';
      List := TStringList.Create;
      try
        SplitPaths(Paths, List);
        for i := List.Count - 1 downto 0 do
        begin
          if (Pos('\jvcl\', AnsiLowerCase(List[i])) > 0) or
             (Pos('\jvpack\', AnsiLowerCase(List[i])) > 0) then
          begin
            OldDir := List[i];
            List.Delete(i);
          end;
        end;

        S := '';
        for i := 0 to List.Count - 1 do
          S := S + List[i] + ';';
        Delete(S, Length(S), 1);
      finally
        List.Free;
      end;

      if S <> Paths then
      {$ifndef DoNotTouchRegistry}
        reg.WriteString('Search Path', S);
      {$else}
        OutputDebugString(PChar('reg.WriteString(''Search Path'', ''' + S + ''')'));
      {$endif}
      reg.CloseKey;
    end;

   // remove design-time package
    if reg.OpenKey(FRegKey + '\Known Packages', False) then
    begin
      for i := 0 to FKnownPackages.Count - 1 do
      begin
        S := AnsiLowerCase(ExtractFileName(FKnownPackages[i]));
        if StartsWith(S, 'jvcl200_') or StartsWith(S, 'jvpack100_') then
        begin
          {$ifndef DoNotTouchRegistry}
          reg.DeleteValue(FKnownPackages[i]);
          {$else}
          OutputDebugString('reg.DeleteValue(''' + FKnownPackages[i] + ''');');
          {$endif}
          Exit;
        end;
      end;
      reg.CloseKey;
    end;

   // delete .bpl, .dcp files
    if OldDir <> '' then
    begin
      repeat
        S := AnsiLowerCase(ExtractFileName(OldDir));
        if (S = 'jvcl') or (S = 'jvpack') then Break;
        OldDir := ExtractFileDir(OldDir);
      until OldDir = '';
    end
    else
      OldDir := BplDir;
    OldDir := ExpandDirMacros(OldDir);

    if IsDelphi then
      VerName := IntToStr(MajorVersion) + '0'
    else
      VerName := IntToStr(MajorVersion) + '0C';

   // JVCL 2   
    DeleteFile(OldDir + '\' + LibDir + '\JVCL200_D' + VerName + '.bpl');
    DeleteFile(OldDir + '\' + LibDir + '\JVCL200_D' + VerName + '.dcp');
    DeleteFile(OldDir + '\' + LibDir + '\JVCL200_R' + VerName + '.bpl');
    DeleteFile(OldDir + '\' + LibDir + '\JVCL200_R' + VerName + '.dcp');

    DeleteFile(OldDir + '\' + LibDir + '\JVCL200_R' + VerName + 'Personal.bpl');
    DeleteFile(OldDir + '\' + LibDir + '\JVCL200_R' + VerName + 'Personal.dcp');

    DeleteFile(OldDir + '\' + LibDir + '\JVCL200_R' + VerName + 'Standard.bpl');
    DeleteFile(OldDir + '\' + LibDir + '\JVCL200_R' + VerName + 'Standard.dcp');

   // JVCL 1
    DeleteFile(OldDir + '\' + LibDir + '\JvPack100_' + VerName + '.bpl');
    DeleteFile(OldDir + '\' + LibDir + '\JvPack100_' + VerName + '.dcp');
  finally
    reg.Free;
  end;
end;

function TTargetInfo.GetIsOldJVCLInstalled: Integer;
var
  i: Integer;
  S: string;
begin
  for i := 0 to FKnownPackages.Count - 1 do
  begin
    S := AnsiLowerCase(ExtractFileName(FKnownPackages[i]));
    if StartsWith(S, 'jvcl200_') then
    begin
      Result := 2;
      Exit;
    end
    else if StartsWith(S, 'jvpack100_') then
    begin
      Result := 1;
      Exit;
    end;
  end;
  Result := 0;
end;

function TTargetInfo.IsTargetFor(const Targets: string): Boolean;
begin
  if Targets = 'all' then
    Result := True
  else
    Result := Pos(',' + AnsiLowerCase(Symbol) + ',', ',' + AnsiLowerCase(Targets) + ',') > 0;
end;

function TTargetInfo.GetIsBCB: Boolean;
begin
  Result := not GetIsDelphi;
end;

function TTargetInfo.GetIsDelphi: Boolean;
begin
  Result := FProductName = 'Delphi';
end;

function TTargetInfo.GetJVCLDirName: string;
begin
  Result := GetPackageGroupDir(Self);
end;

function TTargetInfo.GetJCLPackageDir: string;
begin
  Result := JCLDir + '\packages';
end;

function TTargetInfo.GetJCLPackageXmlDir: string;
begin
  Result := JCLPackageDir + '\xml';
end;

procedure TTargetInfo.SetJCLDir(const NewDir: string);
begin
  FJCLDir := NewDir;
end;

{ TTargetList }

constructor TTargetList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
  GetTargets;
end;

destructor TTargetList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TTargetList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TTargetList.GetItems(Index: Integer): TTargetInfo;
begin
  Result := TTargetInfo(FItems[Index]);
end;

procedure TTargetList.GetTargets;
var
  reg: TRegistry;
  Names: TStrings;
  i: Integer;
begin
  Names := TStringList.Create;
  try
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;

      if not FindCmdSwitch('-NoDelphi') then
      begin
       // obtain Delphi Targets
        if reg.OpenKeyReadOnly(cDelphiKeyName) then
        begin
          reg.GetKeyNames(Names);
          for i := 0 to Names.Count - 1 do
          begin
            if IsVersionNumber(Names[i]) then
              ReadTargetData(cDelphiKeyName + '\' + Names[i], 'Delphi', Names[i]);
          end;
          reg.CloseKey;
        end;
      end;

      if not FindCmdSwitch('-NoBCB') then
      begin
       // obtain BCB Targets
        if reg.OpenKeyReadOnly(cBCBKeyName) then
        begin
          reg.GetKeyNames(Names);
          for i := 0 to Names.Count - 1 do
            if IsVersionNumber(Names[i]) then
              ReadTargetData(cBCBKeyName + '\' + Names[i], 'C++Builder', Names[i]);
          reg.CloseKey;
        end;
      end;
    finally
      reg.Free;
    end;
  finally
    Names.Free;
  end;

 // limit to Delphi/BCB 5 
  for i := Count - 1 downto 0 do
    if Items[i].MajorVersion < 5 then
      FItems.Delete(i);
end;

procedure TTargetList.ReadTargetData(const Key, Name, Version: string);
var
  reg: TRegistry;
  Item: TTargetInfo;
begin
  reg := TRegistry.Create;
  try
   // ****
   // HKEY_LOCAL_MACHINE:
   // ****
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly(Key) then
    begin
      if reg.ValueExists('RootDir') and reg.ValueExists('Version') then
      begin
        reg.CloseKey;
        Item := TTargetInfo.Create(Self);
        FItems.Add(Item);
        Item.FRegKey := Key;
        Item.FProductName := Name;
        Item.FVersion := Version;
        Item.ReadData;
      end;
    end;
  finally
    reg.Free;
  end;
end;

{ TRequirePkg }

constructor TRequirePkg.Create(const AName, ATargets, ACondition: string);
begin
  inherited Create;
  FName := AName;
  FTargets := ATargets;
  FCondition := ACondition;
end;

{ TContainsFile }

constructor TContainsFile.Create(const AName, ATargets, AFormName,
  ACondition: string);
begin
  inherited Create;
  FName := AName;
  FTargets := ATargets;
  FFormName := AFormName;
  FCondition := ACondition;
end;

{ TPackageInfo }

constructor TPackageInfo.Create(const AName, AXmlDir: string; APackageList: TPackageList);
begin
  inherited Create;
  FPackageList := APackageList;
  FName := AName;
  FRequires := TObjectList.Create;
  FContains := TObjectList.Create;
  FInstall := False;
  FXmlDir := AXmlDir;
  FIsDesign := EndsWith(AName, '-d', True);

  ReadXmlPackage; // fills FDisplayName, FDescription, FRequires
end;

destructor TPackageInfo.Destroy;
begin
  FRequires.Free;
  FContains.Free;
  inherited Destroy;
end;

function TPackageInfo.DependsOn(PackageInfo: TPackageInfo): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to RequireCount - 1 do
  begin
    if CompareText(PackageInfo.Name, Requires[i].Name) = 0 then
    begin
      Result := Target.IsTargetFor(Requires[i].Targets);
      Exit;
    end;
  end;
end;

function TPackageInfo.GetBplName: string;
begin
  Result := GetBplNameFrom(FDisplayName, Target, IsDesign);
end;

function TPackageInfo.GetContainCount: Integer;
begin
  Result := FContains.Count;
end;

function TPackageInfo.GetContains(Index: Integer): TContainsFile;
begin
  Result := TContainsFile(FContains.Items[Index]);
end;

function TPackageInfo.GetRequireCount: Integer;
begin
  Result := FRequires.Count;
end;

function TPackageInfo.GetRequires(Index: Integer): TRequirePkg;
begin
  Result := TRequirePkg(FRequires[Index]);
end;

function TPackageInfo.GetTarget: TTargetInfo;
begin
  Result := PackageList.Target;
end;

procedure TPackageInfo.ReadXmlPackage;
var
  i: Integer;
  RequirePkgName, RequireTarget, PkgName,
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

  xml := TJvSimpleXML.Create(nil);
  try
    xml.LoadFromFile(FXmlDir + '\' + FName + '.xml');
    RootNode := xml.Root;
    RequiredNode := RootNode.Items.ItemNamed['Requires'];
    ContainsNode := RootNode.Items.ItemNamed['Contains'];

    FDisplayName := RootNode.Properties.ItemNamed['Name'].Value;
    FDescription := RootNode.Items.ItemNamed['Description'].Value;

   // requires
    for i := 0 to RequiredNode.Items.Count -1 do
    begin
      PackageNode := RequiredNode.Items[i];
      RequirePkgName := PackageNode.Properties.ItemNamed['Name'].Value;
      if Pos('dcldb', AnsiLowerCase(RequirePkgName)) > 0 then
        FRequiresDB := True;

     // require only designtime packages
      RequireTarget := PackageNode.Properties.ItemNamed['Targets'].Value;
      if RequireTarget = '' then
        RequireTarget := 'all';
      Condition := PackageNode.Properties.ItemNamed['Condition'].Value;

     // add new require item
      FRequires.Add(TRequirePkg.Create(RequirePkgName, RequireTarget, Condition));
    end;

   // contains
    for i := 0 to ContainsNode.Items.Count -1 do
    begin
      FileNode := ContainsNode.Items[i];
      ContainsFileName := FileNode.Properties.ItemNamed['Name'].Value;

      RequireTarget := FileNode.Properties.ItemNamed['Targets'].Value;
      if RequireTarget = '' then
        RequireTarget := 'all';

      FormName := FileNode.Properties.ItemNamed['Formname'].Value;
      Condition := FileNode.Properties.ItemNamed['Condition'].Value;

     // add new require item
      FContains.Add(TContainsFile.Create(ContainsFileName, RequireTarget, FormName, Condition));
    end;

  finally
    xml.Free;
  end;

 // not installed or an older version of the package already installed
  PkgName := BplName;
  for i := 0 to Target.KnownPackages.Count - 1 do
  begin
    if CompareText(PkgName, ExtractFileName(Target.KnownPackages[i])) = 0 then
    begin
      FIsInstalled := True;
      FInstall := True;
      Target.FIsJVCLInstalled := True;
      Break;
    end;
  end;
end;

procedure TPackageInfo.SetInstall(const Value: Boolean);
var
  i: Integer;
begin
  if Value <> FInstall then
  begin
    FInstall := Value;
    if FInstall then
    begin
      // activate all required packages
      UpdateRuntimePackages;
    end
    else
    begin
      // deactivate all packages that depend on this package
      for i := 0 to PackageList.Count - 1 do
        if PackageList[i].DependsOn(Self) then
          PackageList[i].Install := False;
    end;
  end;
end;

procedure TPackageInfo.UpdateRuntimePackages;
var
  i: Integer;
  Pkg: TPackageInfo;
begin
  // activate all required packages
  for i := 0 to FRequires.Count - 1 do
  begin
    Pkg := PackageList.FindPackage(Requires[i].Name);
    if Pkg <> nil then
      Pkg.Install := True;
  end;
end;

{ TPackageList }

constructor TPackageList.Create(ATarget: TTargetInfo);
begin
  inherited Create;
  FTarget := ATarget;
  FItems := TObjectList.Create;
end;

destructor TPackageList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TPackageList.FindPackage(const Name: string): TPackageInfo;
var
  i: Integer;
  NameBpl: string;
begin
  NameBpl := Name + '.bpl';
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if (CompareText(Result.Name, Name) = 0) or (CompareText(Result.BplName, NameBpl) = 0) then
      Exit;
  end;
  Result := nil;
end;

function TPackageList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TPackageList.GetItems(Index: Integer): TPackageInfo;
begin
  Result := TPackageInfo(FItems[Index]);
end;

procedure TPackageList.ReadPackages;
var
  BpgText: AnsiString;

  function PackageUsedBy(const PkgName: string; DesignPackage: Boolean): Boolean;
  begin
    Result := Pos(AnsiLowerCase(GetBplNameFrom(PkgName, Target, DesignPackage)) + ':', BpgText) > 0;
  end;

var
  sr: TSearchRec;
  Item: TPackageInfo;
  ps, i: Integer;
  DesignPackage: Boolean;
begin
  BpgText := AnsiLowerCase(ReadStringFromFile(JVCLPackageDir + '\' + Target.BpgName));

  if FindFirst(JVCLPackageXmlDir + '\*.xml',
    faAnyFile and not faDirectory, sr) = 0 then
  try
    repeat
      DesignPackage := True;
      ps := Pos('-d.xml', AnsiLowerCase(sr.Name));
      if ps = 0 then
      begin
        ps := Pos('-r.xml', AnsiLowerCase(sr.Name)); // allow runtime packages
        if ps > 0 then DesignPackage := False;
      end;
      if (ps > 0) and (PackageUsedBy(Copy(sr.Name, 1, ps - 1), DesignPackage)) then
      begin
        Item := nil;
        try
          Item := TPackageInfo.Create(ChangeFileExt(sr.Name, ''), JVCLPackageXmlDir, Self);
          FItems.Add(Item);
        except
          Item.Free;
          raise;
        end;
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;

 // update package "install" dependencies.
  for i := 0 to Count - 1 do
    if Items[i].IsInstalled then
      Items[i].UpdateRuntimePackages;
end;

function IsDelphiRunning: Boolean;
begin
  Result := FindWindow('TAppBuilder', nil) <> 0;
end;

initialization
  JVCLDir := ExtractFileDir(ParamStr(0)); // = $(JVCL) or $(JVCL)\packages
  if CompareText(ExtractFileName(JVCLDir), 'packages') = 0 then
    JVCLDir := ExtractFileDir(JVCLDir);    // = $(JVCL)

  JVCLPackageDir := JVCLDir + '\packages';
  JVCLPackageXmlDir := JVCLPackageDir + '\xml';

  xJCLDir := ExtractFileDir(JVCLDir) + '\jcl';
  if DirectoryExists(xJCLDir) then
  begin
    JCLPairInstallation := True;
    xJCLPackageDir := xJCLDir + '\packages';
    xJCLPackageXmlDir := xJCLPackageDir + '\xml';
  end
  else
  begin
    JCLPairInstallation := False;
    xJCLDir := '';
    xJCLPackageDir := '';
    xJCLPackageXmlDir := '';
  end;

  TargetList := TTargetList.Create;

finalization
  TargetList.Free;

end.
