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

Last Modified: 2003-11-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{.$I JVCL.INC}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$IFDEF VER150}
 // Delphi 7 .NET preview warnings
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{.$define DoNotTouchRegistry}

unit CoreData;
interface
uses
  Windows, SysUtils, Classes, Contnrs, Registry;

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
    FLastUpdate: Integer; // 0: no Update, 1: Update #1, 2: Update #3, ...
    FDcpDir: string;
    FBplDir: string;
    FSearchPath: string;
    FKnownPackages: TStrings;
    FIsJVCLInstalled: Boolean;

    FCompileFor: Boolean;
    FNeedsUpdate: Boolean;
    FClearJVCLPalette: Boolean;
    FBuild: Boolean;
    FDeveloperInstall: Boolean;

    function GetSymbol: string;
    function GetMajorVersion: Integer;
    function GetIsPersonal: Boolean;
    function GetBpgName: string;
    function GetLibDir: string;
    function GetDisplayName: string;
    function GetIsOldJVCLInstalled: Integer;

    function AddRemovePaths(const Paths: string; const NewPaths: array of string;
      Add: Boolean): string;
  protected
    procedure ClearPalette(reg: TRegistry; const Name: string);
    procedure DoClearJVCLPalette;
    procedure GetKnownPackages;
    procedure UpdateKnownPackage;
    procedure AddRemoveJVCLPaths(Add: Boolean);

    procedure ReadData; // reads data from the registry
  public
    constructor Create(ATargetList: TTargetList);
    destructor Destroy; override;

    procedure RegistryInstall;
    procedure RegistryUninstall;
    procedure UninstallOldJVCL;

    function ExpandDirMacros(const Path: string): string;

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
    property Symbol: string read GetSymbol;
    property BpgName: string read GetBpgName;
    property LibDir: string read GetLibDir; // relative to $(JVCL)

    property SearchPaths: string read FSearchPath;
    property BplDir: string read FBplDir;
    property DcpDir: string read FDcpDir;
    property KnownPackages: TStrings read FKnownPackages;
    property IsJVCLInstalled: Boolean read FIsJVCLInstalled;
    property IsOldJVCLInstalled: Integer read GetIsOldJVCLInstalled; // 1, 2

    property CompileFor: Boolean read FCompileFor write FCompileFor;
    property NeedsUpdate: Boolean read FNeedsUpdate write FNeedsUpdate;
    property ClearJVCLPalette: Boolean read FClearJVCLPalette write FClearJVCLPalette;
    property Build: Boolean read FBuild write FBuild;
    property DeveloperInstall: Boolean read FDeveloperInstall write FDeveloperInstall;

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
  public
    constructor Create(const AName, ATargets: string);
    property Name: string read FName;
    property Targets: string read FTargets;
  end;

  TPackageInfo = class(TObject)
  private
    FPackageList: TPackageList;
    
    FName: string;
    FDisplayName: string;
    FDescription: string;
    FRequires: TObjectList;
    FRequiresDB: Boolean;
    FIsInstalled: Boolean;

    FInstall: Boolean;

    procedure SetInstall(const Value: Boolean);
    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): TRequirePkg;
    function GetTarget: TTargetInfo;
    function GetBplName: string;
  protected
    procedure ReadXmlPackage;
  public
    constructor Create(const AName: string; APackageList: TPackageList);
    destructor Destroy; override;

    function DependsOn(PackageInfo: TPackageInfo): Boolean;

    property Name: string read FName;
    property DisplayName: string read FDisplayName;
    property BplName: string read GetBplName;
    property Description: string read FDescription;
    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: TRequirePkg read GetRequires;
    property RequiresDB: Boolean read FRequiresDB;
    property IsInstalled: Boolean read FIsInstalled;

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

procedure SplitPaths(const Paths: string; List: TStrings);
function CombinePaths(List: TStrings): string;
function StartsWith(const Text, StartText: string): Boolean;
function ReadStringFromFile(const Filename: string): string;
function SubStr(const Text: string; StartIndex, EndIndex: Integer): string;
function IsDelphiRunning: Boolean;


implementation

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

function GetPackageGroupName(Target: TTargetInfo): string;
begin
  with Target do
  begin
    if FProductName = 'Delphi' then
      Result := 'D' + IntToStr(MajorVersion)
    else
      Result := 'BCB' + IntToStr(MajorVersion);

    if IsPersonal then
      if MajorVersion >= 6 then
        Result := Result + 'Per'
      else
        Result := Result + 'Std';

    Result := Result + ' Packages.bpg';
  end;
end;

function GetBplNameFrom(const PkgName: string; Target: TTargetInfo): string;
begin
  if Target.ProductName = 'Delphi' then
    Result := PkgName + 'D' + IntToStr(Target.MajorVersion) + 'D.bpl'
  else
    Result := PkgName + 'C' + IntToStr(Target.MajorVersion) + 'D.bpl';
end;

// -----------------------------------------------------------------------------

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
    if P <> #0 then
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

function StartsWith(const Text, StartText: string): Boolean;
var
  Len, i: Integer;
begin
  Result := False;
  Len := Length(StartText);
  if Len > Length(Text) then
    Exit;
  for i := 0 to Len do
    if Text[i] <> StartText[i] then
      Exit;
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
        if (not Packages[i].IsInstalled) and (Packages[i].Install) then
        {$ifndef DoNotTouchRegistry}
          reg.WriteString(FBplDir + '\' + Packages[i].BplName, Packages[i].Description);
        {$else}
          OutputDebugString(PChar('reg.WriteString(''' + FBplDir + '\' + Packages[i].BplName + ''', ''' + Packages[i].Description + ''')'));
        {$endif}
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
      if not Found then
      begin
        if StartsWith(AnsiLowerCase(NewPaths[DirIndex]), AnsiLowerCase(RootDir) + '\') then
        begin
          if FProductName = 'Delphi' then
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
        NewS := JVCLDir;

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
  if FProductName = 'Delphi' then
    Result := 'D'
  else
    Result := 'C';

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
  AddRemoveJVCLPaths(False);
  DoClearJVCLPalette;
  UpdateKnownPackage;
  for i := 0 to Packages.Count - 1 do
  begin
    Packages[i].Install := False;
    Packages[i].FIsInstalled := False;
  end;
  FIsJVCLInstalled := False;
 // do not call ReadData here
end;

function TTargetInfo.GetLibDir: string;
begin
  Result := 'lib' + IntToStr(MajorVersion);
end;

procedure TTargetInfo.ReadData;
var
  reg: TRegistry;
  i: Integer;
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

      FCompileFor := True;
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

        if reg.ValueExists('Package DPL Output') then
          FBplDir := ExpandDirMacros(reg.ReadString('Package DPL Output'))
        else
          FBplDir := RootDir + '\Projects\Bpl';

        if reg.ValueExists('Search Path') then
          FSearchPath := ExpandDirMacros(reg.ReadString('Search Path'))
        else
          FSearchPath := ExpandDirMacros('$(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports;$(DELPHI)\Projects\Bpl');
        reg.CloseKey;
      end;
    end;
  finally
    reg.Free;
  end;
  GetKnownPackages;
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

    if FProductName = 'Delphi' then
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

     // obtain BCB Targets
      if reg.OpenKeyReadOnly(cBCBKeyName) then
      begin
        reg.GetKeyNames(Names);
        for i := 0 to Names.Count - 1 do
          if IsVersionNumber(Names[i]) then
            ReadTargetData(cBCBKeyName + '\' + Names[i], 'C++Builder', Names[i]);
        reg.CloseKey;
      end;
    finally
      reg.Free;
    end;
  finally
    Names.Free;
  end;
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

constructor TRequirePkg.Create(const AName, ATargets: string);
begin
  inherited Create;
  FName := AName;
  FTargets := ATargets;
end;

{ TPackageInfo }

constructor TPackageInfo.Create(const AName: string; APackageList: TPackageList);
begin
  inherited Create;
  FPackageList := APackageList;
  FName := AName;
  FRequires := TObjectList.Create;
  FInstall := True;

  ReadXmlPackage; // fills FDisplayName, FDescription, FRequires
end;

function TPackageInfo.DependsOn(PackageInfo: TPackageInfo): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to RequireCount - 1 do
  begin
    if CompareText(PackageInfo.Name, Requires[i].Name) = 0 then
    begin
      if Requires[i].Targets = 'all' then
        Result := True
      else
        Result := Pos(',' + Target.Symbol + ',', ',' + Requires[i].Targets + ',') > 0;
      Exit;
    end;
  end;
end;

destructor TPackageInfo.Destroy;
begin
  FRequires.Free;
  inherited Destroy;
end;

function TPackageInfo.GetBplName: string;
begin
  Result := GetBplNameFrom(FDisplayName, Target);
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
  S: string;
  i, ps: Integer;
  RequirePkgName, RequireTarget, PkgName: string;
begin
  FRequires.Clear;

 // ***
 // (ahuser) Maybe we should use JvSimpleXml.pas here, but I do not want to
 //          depend on JVCL/JCL code.
 // ***

  S := ReadStringFromFile(JVCLPackageXmlDir + '\' + FName + '.xml');

  ps := Pos('<Package Name="', S);
  if ps > 0 then
  begin
    Delete(S, 1, ps + 14);
    ps := Pos('"', S);
    FDisplayName := Copy(S, 1, ps - 1);
  end;

  ps := Pos('<Description>', S);
  if ps > 0 then
  begin
    Delete(S, 1, ps + 12);
    ps := Pos('</Description>', S);
    FDescription := Copy(S, 1, ps - 1);
  end;

  repeat
    ps := Pos('<Package Name="', S);
    if ps = 0 then
      Break; // -> leave
    Delete(S, 1, ps + 14);
    ps := Pos('"', S);
    RequirePkgName := Copy(S, 1, ps - 1);

    if Pos('dcldb', AnsiLowerCase(RequirePkgName)) > 0 then
      FRequiresDB := True;

   // require only designtime packages
    if (Copy(RequirePkgName, 1, 2) = 'Jv') and (Pos('-D', RequirePkgName) > 0) then
    begin
      ps := Pos('Targets="', S);
      if (ps > 0) and (ps < Pos('<Package Name="', S)) then
      begin
        Delete(S, 1, ps + 8);
        ps := Pos('"', S);
        RequireTarget := Copy(S, 1, ps - 1);
      end
      else
        RequireTarget := 'all';

     // add new require item
      FRequires.Add(TRequirePkg.Create(RequirePkgName, RequireTarget));
    end;
  until False;

 // not installed or an older version of the package already installed
  PkgName := BplName;
  for i := 0 to Target.KnownPackages.Count - 1 do
  begin
    if CompareText(PkgName, ExtractFileName(Target.KnownPackages[i])) = 0 then
    begin
      FIsInstalled := True;
      Target.FIsJVCLInstalled := True;
      Break;
    end;
  end;
end;

procedure TPackageInfo.SetInstall(const Value: Boolean);
var
  i: Integer;
  Pkg: TPackageInfo;
begin
  if Value <> FInstall then
  begin
    FInstall := Value;
    if FInstall then
    begin
      // activate all required packages
      for i := 0 to FRequires.Count - 1 do
      begin
        Pkg := PackageList.FindPackage(Requires[i].Name);
        if Pkg <> nil then
          Pkg.Install := True;
      end;
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
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if CompareText(Result.Name, Name) = 0 then
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
  sr: TSearchRec;
  Item: TPackageInfo;
  ps: Integer;

  function PackageUsedBy(PkgName: string): Boolean;
  begin
    Result := Pos(AnsiLowerCase(GetBplNameFrom(PkgName, Target)) + ':', BpgText) > 0;
  end;

begin
  BpgText := AnsiLowerCase(ReadStringFromFile(JVCLPackageDir + '\' + Target.BpgName));

  if FindFirst(JVCLPackageXmlDir + '\*.xml',
    faAnyFile and not faDirectory, sr) = 0 then
  try
    repeat
      ps := Pos('-D.xml', sr.Name);
      if (ps > 0) and (PackageUsedBy(Copy(sr.Name, 1, ps - 1))) then
      begin
        Item := nil;
        try
          Item := TPackageInfo.Create(ChangeFileExt(sr.Name, ''), Self);
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

  TargetList := TTargetList.Create;

finalization
  TargetList.Free;

end.
