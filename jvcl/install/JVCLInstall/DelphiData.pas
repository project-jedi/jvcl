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
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit DelphiData;

interface

uses
  Windows, SysUtils, Classes, Contnrs, Registry;

type
  TCompileTarget = class;
  TCompileTargetList = class;
  TDelphiPackage = class;
  TDelphiPackageList = class;

  TCompileTargetList = class(TObjectList)
  private
    function GetItems(Index: Integer): TCompileTarget;
    procedure LoadTargets(const SubKey: string);
  public
    constructor Create;
    property Items[Index: Integer]: TCompileTarget read GetItems; default;
  end;

  TCompileTarget = class(TObject)
  private
    FName: string;
    FLatestRTLPatch: Integer;
    FLatestUpdate: Integer;
    FVersion: Integer;
    FVersionStr: string;
    FExecutable: string;
    FEdition: string;
    FRootDir: string;
    FBrowsingPaths: TStrings;
    FDCPOutputDir: string;
    FBPLOutputDir: string;
    FPackageSearchPaths: TStrings;
    FSearchPaths: TStrings;
    FDisabledPackages: TDelphiPackageList;
    FKnownPackages: TDelphiPackageList;
    FKnownIDEPackages: TDelphiPackageList;
    FRegistryKey: string;

    procedure LoadFromRegistry;
    procedure LoadPackagesFromRegistry(APackageList: TDelphiPackageList;
      const SubKey: string);
    procedure SavePackagesToRegistry(APackageList: TDelphiPackageList;
      const SubKey: string);
    function GetHomepage: string;
    function GetMake: string;
    function GetBplDir: string;
    function GetDcpDir: string;
  public
    constructor Create(const AName, AVersion: string);
    destructor Destroy; override;

    function IsBCB: Boolean;
    function IsPersonal: Boolean;
    function DisplayName: string;

    function FindPackage(const PackageName: string): TDelphiPackage;
    function FindPackageEx(const PackageNameStart: string): TDelphiPackage;
    function ExpandDirMacros(const Dir: string): string;
    function InsertDirMacros(const Dir: string): string;

    procedure SavePaths;
      { writes BrowsingPaths and SearchPaths to the registry }
    procedure SavePackagesLists;
      { writes KnownPackages and DisabledPackages to the registry }

    property Homepage: string read GetHomepage;
    property RegistryKey: string read FRegistryKey;

    property Make: string read GetMake;

    property Name: string read FName;
    property VersionStr: string read FVersionStr;
    property Version: Integer read FVersion;
    property Executable: string read FExecutable; // [Reg->App] x:\path\Delphi.exe
    property RootDir: string read FRootDir; // [Reg->RootDir] x:\path
    property Edition: string read FEdition; // [Reg->Version] PER/PRO/CSS
    property LatestUpdate: Integer read FLatestUpdate;
    property LatestRTLPatch: Integer read FLatestRTLPatch;

    property BrowsingPaths: TStrings read FBrowsingPaths; // with macros
    property DCPOutputDir: string read FDCPOutputDir; // with macros
    property BPLOutputDir: string read FBPLOutputDir; // with macros
    property PackageSearchPaths: TStrings read FPackageSearchPaths; // with macros
    property SearchPaths: TStrings read FSearchPaths; // with macros

    property BplDir: string read GetBplDir; // macros are expanded
    property DcpDir: string read GetDcpDir; // macros are expanded

    property KnownIDEPackages: TDelphiPackageList read FKnownIDEPackages;
    property KnownPackages: TDelphiPackageList read FKnownPackages;
    property DisabledPackages: TDelphiPackageList read FDisabledPackages;
  end;

  TDelphiPackageList = class(TObjectList)
  private
    function GetItems(Index: Integer): TDelphiPackage;
  public
    function IndexOfFilename(const Filename: string): Integer;
    procedure Add(const Filename, Description: string);

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
  {$IFDEF COMPILER6_UP}
  StrUtils,
  {$ENDIF COMPILER6_UP}
  CmdLineUtils;

{$IFDEF COMPILER5}
function AnsiStartsText(const SubStr, Text: string): Boolean;
begin
  Result := AnsiStrLIComp(PChar(SubStr), PChar(Text), Length(SubStr)) = 0;
end;

function ExcludeTrailingPathDelimiter(const Path: string): string;
begin
  if (Path <> '') and (Path[Length(Path)] = '\') then // Delphi 5 only knows Windows
    Result := Copy(Path, 1, Length(Path) - 1)
  else
    Result := Path;
end;
{$ENDIF COMPIELR5}

const
  KeyBorland = '\SOFTWARE\Borland\'; // do not localize

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
    while not (P[0] in [#0, ';']) do
      Inc(P);
    SetString(S, F, P - F);
    List.Add(ExcludeTrailingPathDelimiter(S));
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

{ TCompileTargetList }

constructor TCompileTargetList.Create;
begin
  inherited Create;
  if not CmdOptions.IgnoreDelphi then
    LoadTargets('Delphi'); // do not localize
  if not CmdOptions.IgnoreBCB then
    LoadTargets('C++Builder'); // do not localize
end;

function TCompileTargetList.GetItems(Index: Integer): TCompileTarget;
begin
  Result := TCompileTarget(inherited Items[Index]);
end;

procedure TCompileTargetList.LoadTargets(const SubKey: string);
var
  Reg: TRegistry;
  List: TStrings;
  i: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(KeyBorland + SubKey) then
    begin
      List := TStringList.Create;
      try
        Reg.GetKeyNames(List);
        for i := 0 to List.Count - 1 do
          Add(TCompileTarget.Create(SubKey, List[i]));
      finally
        List.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

{ TCompileTarget }

constructor TCompileTarget.Create(const AName, AVersion: string);
begin
  inherited Create;
  FName := AName;
  FVersionStr := AVersion;
  FVersion := StrToIntDef(Copy(FVersionStr, 1, Pos('.', FVersionStr) - 1), 0);
  FRegistryKey := KeyBorland + Name + '\' + VersionStr;

  FBrowsingPaths := TStringList.Create;
  FPackageSearchPaths := TStringList.Create;
  FSearchPaths := TStringList.Create;

  FDisabledPackages := TDelphiPackageList.Create;
  FKnownIDEPackages := TDelphiPackageList.Create;
  FKnownPackages := TDelphiPackageList.Create;

  LoadFromRegistry;
end;

destructor TCompileTarget.Destroy;
begin
  FBrowsingPaths.Free;
  FPackageSearchPaths.Free;
  FSearchPaths.Free;

  FDisabledPackages.Free;
  FKnownIDEPackages.Free;
  FKnownPackages.Free;

  inherited Destroy;
end;

function TCompileTarget.DisplayName: string;
begin
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
      if (S = 'delphi') or (S = 'bcb') then // do not localize
        NewS := FRootDir;

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
  if AnsiStartsText(RootDir + '\', Dir) then
  begin
    if IsBCB then
      Result := '$(BCB)' // do not localize
    else
      Result := '$(DELPHI)'; // do not localize
    Result := Result + Copy(Dir, Length(RootDir) + 1, MaxInt);
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
  Result := Find(KnownIDEPackages);
  if Result = nil then
    Result := Find(KnownPackages);
end;

function TCompileTarget.FindPackageEx(
  const PackageNameStart: string): TDelphiPackage;

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

function TCompileTarget.IsBCB: Boolean;
begin
  Result := CompareText(Name, 'Delphi') <> 0;
end;

function TCompileTarget.IsPersonal: Boolean;
begin
  Result := (CompareText(Edition, 'PER') = 0) or // do not localize
            (CompareText(Edition, 'PERS') = 0) or // do not localize
            (CompareText(Edition, 'STD') = 0); // do not localize
end;

procedure TCompileTarget.LoadFromRegistry;
var
  Reg: TRegistry;
  List: TStrings;
  i: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKeyReadOnly(RegistryKey) then
    begin
      FExecutable := Reg.ReadString('App'); // do not localize
      FEdition := Reg.ReadString('Version'); // do not localize
      FRootDir := Reg.ReadString('RootDir'); // do not localize

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

   // get library paths
    if Reg.OpenKeyReadOnly(RegistryKey + '\Library') then // do not localize
    begin
      FDCPOutputDir := ExcludeTrailingPathDelimiter(Reg.ReadString('Package DCP Output')); // do not localize
      FBPLOutputDir := ExcludeTrailingPathDelimiter(Reg.ReadString('Package DPL Output')); // do not localize
      ConvertPathList(Reg.ReadString('Browsing Path'), FBrowsingPaths); // do not localize
      ConvertPathList(Reg.ReadString('Package Search Path'), FPackageSearchPaths); // do not localize
      ConvertPathList(Reg.ReadString('Search Path'), FSearchPaths); // do not localize
    end;
  finally
    Reg.Free;
  end;

  LoadPackagesFromRegistry(FKnownIDEPackages, 'Known IDE Packages'); // do not localize
  LoadPackagesFromRegistry(FKnownPackages, 'Known Packages'); // do not localize
  LoadPackagesFromRegistry(FDisabledPackages, 'Disabled Packages'); // do not localize
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
    if Reg.OpenKey(RegistryKey + '\' + SubKey, True) then
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
  if IsBCB then
    Result := 'http://www.borland.com/products/downloads/download_cbuilder.html' // do not localize
  else
  begin
    if Version = 5 then
      Result := 'http://info.borland.com/devsupport/delphi/downloads/index.html' // do not localize
    else
      Result := 'http://www.borland.com/products/downloads/download_delphi.html' // do not localize
  end;
end;

procedure TCompileTarget.SavePackagesLists;
begin
  SavePackagesToRegistry(FKnownPackages, 'Known Packages'); // do not localize
  SavePackagesToRegistry(FDisabledPackages, 'Disabled Packages'); // do not localize
end;

procedure TCompileTarget.SavePaths;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(RegistryKey + '\Library', True) then // do not localize
    begin
      Reg.WriteString('Browsing Path', ConvertPathList(FBrowsingPaths)); // do not localize
      Reg.WriteString('Search Path', ConvertPathList(FSearchPaths)); // do not localize
    end;
  finally
    Reg.Free;
  end;
end;

function TCompileTarget.GetMake: string;
begin
  Result := RootDir + '\Bin\make.exe'; // do not localize
end;

function TCompileTarget.GetBplDir: string;
begin
  Result := ExpandDirMacros(BPLOutputDir);
end;

function TCompileTarget.GetDcpDir: string;
begin
  Result := ExpandDirMacros(DCPOutputDir);
end;

{ TDelphiPackageList }

procedure TDelphiPackageList.Add(const Filename, Description: string);
var
  Item: TDelphiPackage;
begin
  Item := TDelphiPackage.Create(Filename, Description);
  inherited Add(Item);
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

end.
