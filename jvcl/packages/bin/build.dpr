{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: BuildTarget.pas, released on 2004-03-25.

The Initial Developer of the Original Code is Andreas Hausladen
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

program build;

{$APPTYPE CONSOLE}
{$DEFINE JVCL}

{ build.exe setups the environment for a Delphi compiler }

uses
  Windows, ShlObj;

type
  TOption = record
   Name: string;
   Env: string;
   Default: string;
 end;

{$IFDEF JCL}
const
  LibraryName = 'JCL';
  LibraryRootDirRelativeToBuild = 2; // means: '..\..'
  pgEditFile = 'install\build\pgEdit.xml'; // relative to the Library-Directory
  ExtraOptions: array[0..0] of TOption = (
    (Name: ''; Env: ''; Default: '')
  );
  PackageGroupName = 'JclPackages*0';
{$ENDIF JCL}
{$IFDEF JVCL}
const
  LibraryName = 'JVCL';
  LibraryRootDirRelativeToBuild = 2; // means: '..\..'
  pgEditFile = 'devtools\bin\pgEdit.xml'; // relative to the Library-Directory
  ExtraOptions: array[0..0] of TOption = (
    (Name: 'jcl-path'; Env: 'JCLROOT'; Default: '..\..\..\jcl')
  );
  PackageGroupName = '* Packages';
{$ENDIF JVCL}

{$IFNDEF JCL}
 {$IFNDEF JVCL}
  {$IFDEF MSWINDOWS}
   {$Message Fatal 'Neither JCL nor JVCL is defined'}
  {$ENDIF MSWINDOWS}
 {$ENDIF ~JVCL}
{$ENDIF ~JCL}

type
  TTarget = record
    Name: string;
    PerName: string;
    PerDir: string;
  end;

const // keep in sync with JVCL Installer's DelphiData.pas
  BDSVersions: array[1..4] of record
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
    (Name: 'Borland Developer Studio'; VersionStr: '2006'; Version: 10; CIV: '100'; ProjectDirResId: 64719; Supported: True) 
  );

type
  TProduct = class(TObject)
  private
    FMainName: string;      // d7
    FName: string;          // d7p        ( with/-out personal "p" )

    FRootDir: string;
    FBplDir: string;
    FDcpDir: string;
    FLibDir: string;
    FIsPersonal: Boolean;
    FIsCLX: Boolean;
    FKeyName: string;

    function GetBDSProjectsDir: string;
    procedure ReadRegistryData;
  public
    Typ: (Delphi, BCB, BDS);
    VersionStr: string;     // '9' for BDS 3.0
    Version: Integer;       // 9 for BDS 3.0
    IDEVersionStr: string;  // '3' for BDS 3.0
    IDEVersion: Integer;    // 3 for BDS 3.0
    PkgDir: string;         // d7 / d7per
  public
    constructor Create(const AProductName, PerDirName: string);
    function IsValid: Boolean;

    property RootDir: string read FRootDir;
    property BDSProjectsDir: string read GetBDSProjectsDir;
    property BplDir: string read FBplDir;
    property DcpDir: string read FDcpDir;
    property LibDir: string read FLibDir;

    property MainName: string read FMainName;
    property Name: string read FName;
    property IsPersonal: Boolean read FIsPersonal;
    property IsCLX: Boolean read FIsCLX;
    property KeyName: string read FKeyName;
  end;

var
  LibraryRootDir: string;
  DxgettextDir: string = '';
  ExtraUnitDirs: string = '';
  MakeOptions: string = '';
  Verbose: Boolean = False;
  Force: Boolean = False; // force even if the target is not installed
  DccOpt: string = '-Q -M';
  UserLibDir, UserDcpDir, UserBplDir: string;

  Targets: array of TTarget = nil;
  Products: array of TProduct = nil;

{ Helper functions because no SysUtils unit is used. }
{******************************************************************************}
function ExtractFileDir(const S: string): string;
var
  ps: Integer;
begin
  ps := Length(S);
  while (ps > 1) and (S[ps] <> '\') do
    Dec(ps);
  Result := Copy(S, 1, ps - 1);
end;
{******************************************************************************}
function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  if (S <> '') and (S[Length(S)] = '\') then
    Result := Copy(S, 1, Length(S) - 1)
  else
    Result := S;
end;
{******************************************************************************}
function StrLen(P: PChar): Integer;
begin
  Result := 0;
  while P[Result] <> #0 do
    Inc(Result);
end;
{******************************************************************************}
function StrToInt(const S: string): Integer;
var
  Error: Integer;
begin
  Val(S, Result, Error);
end;
{******************************************************************************}
function IntToStr(Value: Integer): string;
begin
  Str(Value, Result);
end;
{******************************************************************************}
function SameText(const S1, S2: string): Boolean;
var
  i, len: Integer;
begin
  Result := False;
  len := Length(S1);
  if len = Length(S2) then
  begin
    for i := 1 to len do
      if UpCase(S1[i]) <> UpCase(S2[i]) then
        Exit;
    Result := True;
  end;
end;
{******************************************************************************}
function StartsText(const SubStr, S: string): Boolean;
var
  i, len: Integer;
begin
  Result := False;
  len := Length(SubStr);
  if len <= Length(S) then
  begin
    for i := 1 to len do
      if UpCase(SubStr[i]) <> UpCase(S[i]) then
        Exit;
    Result := True;
  end;
end;
{******************************************************************************}
function GetEnvironmentVariable(const Name: string): string;
begin
  SetLength(Result, 8 * 1024);
  SetLength(Result, Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Length(Result)));
end;
{******************************************************************************}
function FileExists(const Filename: string): Boolean;
var
  attr: Cardinal;
begin
  attr := GetFileAttributes(PChar(Filename));
  Result := (attr <> $FFFFFFFF) and (attr and FILE_ATTRIBUTE_DIRECTORY = 0);
end;
{******************************************************************************}
function Execute(const Cmd: string): Integer;
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  StartupInfo.cb := SizeOf(StartupInfo);
  GetStartupInfo(StartupInfo);
  if CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil,
    PChar(ExtractFileDir(ParamStr(0))), StartupInfo, ProcessInfo) then
  begin
    CloseHandle(ProcessInfo.hThread);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(Result));
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    Result := -1;
end;
{******************************************************************************}
function GetWindowsDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetWindowsDirectory(PChar(Result), Length(Result)));
end;
{******************************************************************************}
function GetSystemDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetSystemDirectory(PChar(Result), Length(Result)));
end;
{******************************************************************************}

{ a very small XML parser }
type
  IAttr = interface
    function Name: string;
    function Value: string;
  end;

  ITag = interface
    function Name: string;
    function Attrs(const Name: string): IAttr;
  end;

  TXmlFile = class(TObject)
  private
    FText: string;
    FPosition: Integer;
  public
    constructor Create(const Filename: string);
    function NextTag: ITag;
  end;

  TTag = class(TInterfacedObject, ITag)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    function Name: string;
    function Attrs(const Name: string): IAttr;
  end;

  TAttr = class(TInterfacedObject, IAttr)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    function Name: string;
    function Value: string;
  end;

{******************************************************************************}
{ TXmlFile }

constructor TXmlFile.Create(const Filename: string);
var
  f: file of Byte;
begin
  inherited Create;
  FileMode := 0;
  AssignFile(f, Filename);
  Reset(f);
  SetLength(FText, FileSize(f));
  BlockRead(f, FText[1], FileSize(f));
  CloseFile(f);
  FPosition := 0;
end;
{******************************************************************************}
function TXmlFile.NextTag: ITag;
var
  F, P: PChar;
  InStr1, InStr2: Boolean;
  S: string;
begin
  InStr1 := False;
  InStr2 := False;
  if FPosition >= Length(FText) then
  begin
    Result := nil;
    Exit;
  end;

  P := PChar(FText) + FPosition;
  while (P[0] <> #0) and (P[0] <> '<') do
    Inc(P);
  if P[0] <> #0 then
  begin
    if P[1] = '!' then // comment
    begin
      while (P[0] <> #0) do
      begin
        if (P[0] = '-') and (P[1] = '-') and (P[2] = '>') then
          Break;
        Inc(P);
      end;
      FPosition := P - PChar(FText);
      Result := NextTag;
      Exit;
    end;
    F := P;
    while True do
    begin
      case P[0] of
        #0:
          Break;
        '>':
          if not (InStr1 or InStr2) then
          begin
            SetString(S, F + 1, P - F - 1);
            Result := TTag.Create(S);
            Inc(P);
            Break;
          end;
        '''':
          InStr1 := not InStr1;
        '"':
          InStr2 := not InStr2;
      end;
      Inc(P);
    end;
  end;
  FPosition := P - PChar(FText);
end;
{******************************************************************************}
{ TTag }

constructor TTag.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;
{******************************************************************************}
function TTag.Name: string;
var
  ps: Integer;
begin
  ps := Pos(' ', FText);
  if ps = 0 then
    Result := FText
  else
    Result := Copy(FText, 1, ps - 1);
end;
{******************************************************************************}
function TTag.Attrs(const Name: string): IAttr;
var
  ps: Integer;
  InStr1, InStr2: Boolean;
  F, P: PChar;
  S: string;
begin
  Result := TAttr.Create('');
  ps := Pos(' ', FText);
  if ps = 0 then
    Exit;
  P := PChar(FText) + ps;
  while P[0] <> #0 do
  begin
    while P[0] in [#1..#32] do
      Inc(P);
    if P[0] = #0 then
      Break;
    F := P;
    InStr1 := False;
    InStr2 := False;
    while True do
    begin
      case P[0] of
        #0, #9, #32, '/':
          if not (InStr1 or InStr2) or (P[0] = #0) then
          begin
            SetString(S, F, P - F);
            Result := TAttr.Create(S);
            if SameText(Result.Name, Name) then
              Exit;
            Inc(P);
            Break;
          end;
        '''':
          InStr1 := not InStr1;
        '"':
          InStr2 := not InStr2;
      end;
      Inc(P);
    end;
  end;
  Result := TAttr.Create('');
end;
{******************************************************************************}
{ TAttr }

constructor TAttr.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;
{******************************************************************************}
function TAttr.Name: string;
var
  ps: Integer;
begin
  ps := Pos('=', FText);
  if ps = 0 then
    Result := FText
  else
    Result := Copy(FText, 1, ps - 1);
end;
{******************************************************************************}
function TAttr.Value: string;
var
  ps: Integer;
begin
  ps := Pos('=', FText);
  if ps = 0 then
    Result := ''
  else
  begin
    Result := Copy(FText, ps + 1, MaxInt);
    if (Result <> '') and (Result[1] in ['''', '"']) then
    begin
      Delete(Result, 1, 1);
      Delete(Result, Length(Result), 1);
    end;
  end;
end;
{******************************************************************************}
function AsterixMacro(const S, AsterixRepl: string): string;
var
  I: Integer;
begin
  Result := S;
  I := Pos('*', Result);
  if I > 0 then
  begin
    Delete(Result, I, 1);
    Insert(AsterixRepl, Result, I);
  end;
end;
{******************************************************************************}
procedure LoadTargetNames;
var
  xml: TXmlFile;
  tg: ITag;
begin
  xml := TXmlFile.Create(LibraryRootDir + '\' + pgEditFile);
  try
    tg := xml.NextTag;
    while tg <> nil do
    begin
      if SameText(tg.Name, 'model') and SameText(tg.Attrs('name').Value, LibraryName) then
      begin
        tg := xml.NextTag;
        while not SameText(tg.Name, 'targets') do
          tg := xml.NextTag;
        while not SameText(tg.Name, '/targets') do
        begin
          if SameText(tg.Name, 'target') then
          begin
            if FileExists(LibraryRootDir + '\packages\' + AsterixMacro(PackageGroupName, tg.Attrs('name').Value) + '.bpg') or
               FileExists(LibraryRootDir + '\packages\' + AsterixMacro(PackageGroupName, tg.Attrs('name').Value) + '.bdsgroup') then
            begin
              SetLength(Targets, Length(Targets) + 1); // we do not have 10tnds iterations so this is acceptable
              with Targets[High(Targets)] do
              begin
                Name := tg.Attrs('name').Value;
                PerName := tg.Attrs('pname').Value;
                PerDir := tg.Attrs('pdir').Value;
              end;
            end;
          end;
          tg := xml.NextTag;
        end;
        Break; // we only want the "LibraryName" part
      end;
      tg := xml.NextTag;
    end;
  finally
    xml.Free;
  end;
end;
{******************************************************************************}
{ TProduct }

constructor TProduct.Create(const AProductName, PerDirName: string);
var
  Index: Integer;
begin
  if UpCase(AProductName[1]) = 'D' then
    Typ := Delphi
  else
    Typ := BCB;

  VersionStr := AProductName[2];
  if (Length(AProductName) > 2) and (AProductName[3] in ['0'..'9']) then
  begin
    VersionStr := VersionStr + AProductName[3];
    Index := 4;
  end
  else
    Index := 3;

  Version := StrToInt(VersionStr);
  IDEVersionStr := VersionStr;
  IDEVersion := Version;

  if Version > 7 then
  begin
    Typ := BDS;
    IDEVersion := Version - 6; // D 8 = BDS 2
    IDEVersionStr := IntToStr(IDEVersion);
  end;

  FMainName := Copy(AProductName, 1, Index - 1);
  FName := AProductName;
  PkgDir := AProductName;

  FIsCLX := SameText('clx', Copy(AProductName, Index, 3));
  FIsPersonal := False;
  if Length(AProductName) > Index then
  begin
    if (UpCase(AProductName[Index]) = 'P') or (UpCase(AProductName[Index]) = 'S') then
    begin
      FIsPersonal := True;
      PkgDir := PerDirName
    end;
  end;

  ReadRegistryData;
end;
{******************************************************************************}
function TProduct.IsValid: Boolean;
begin
  Result := (RootDir <> '') and
            FileExists(RootDir + '\bin\dcc32.exe') and
            FileExists(RootDir + '\bin\brc32.exe');
            FileExists(RootDir + '\bin\make.exe');
end;
{******************************************************************************}
procedure TProduct.ReadRegistryData;
var
  Reg: HKEY;
  RegTyp: LongWord;
  ProjectsDir: string;

  function ReadStr(const Name: string): string;
  var
    Len: Longint;
  begin
    Len := MAX_PATH;
    SetLength(Result, MAX_PATH);
    RegQueryValueEx(Reg, PChar(Name), nil, @RegTyp, PByte(Result), @Len);
    SetLength(Result, StrLen(PChar(Result)));
  end;

  function ResolveMacros(const Dir: string): string;
  var
    ps, psEnd: Integer;
    S: string;
  begin
    if StartsText('$(DELPHI)', Dir) then
      Result := FRootDir + Copy(Dir, 10, MaxInt)
    else if StartsText('$(BCB)', Dir) then
      Result := FRootDir + Copy(Dir, 7, MaxInt)
    else if StartsText('$(BDS)', Dir) then
      Result := FRootDir + Copy(Dir, 7, MaxInt)
    else if StartsText('$(BDSPROJECTSDIR)', Dir) then
      Result := GetBDSProjectsDir + Copy(Dir, 18, MaxInt)
    else
    begin
      Result := Dir;
      ps := Pos('$(', Result);
      if ps > 0 then
      begin
        psEnd := Pos(')', Result);
        if psEnd > 0 then
        begin
          S := Copy(Result, ps + 2, psEnd - ps - 2);
          if S <> '' then
          begin
            Delete(Result, ps, 2 + Length(S) + 1);
            Insert(GetEnvironmentVariable(S), Result, ps);
          end
        end;
      end;
    end
  end;

begin
  case Typ of
    Delphi:
      FKeyName := 'Software\Borland\Delphi\' + IDEVersionStr + '.0';
    BCB:
      FKeyName := 'Software\Borland\C++Builder\' + IDEVersionStr + '.0';
    BDS:
      FKeyName := 'Software\Borland\BDS\' + IDEVersionStr + '.0';
  end;

  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(KeyName), 0, KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS then
  begin
    FRootDir := ExcludeTrailingPathDelimiter(ReadStr('RootDir'));
    RegCloseKey(Reg);
  end;

  if Typ = BDS then
    ProjectsDir := GetBDSProjectsDir
  else
    ProjectsDir := FRootDir + '\Projects';

  FDcpDir := FRootDir + '\Projects\Bpl';
  FBplDir := FRootDir + '\Projects\Bpl';
  if Typ = BCB then
    FLibDir := FRootDir + '\Projects\Lib'
  else
    FLibDir := FRootDir + '\Projects\Bpl';

  if RegOpenKeyEx(HKEY_CURRENT_USER, PChar(KeyName + '\Library'), 0, KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS then
  begin
    FDcpDir := ResolveMacros(ExcludeTrailingPathDelimiter(ReadStr('Package DCP Output')));
    FBplDir := ResolveMacros(ExcludeTrailingPathDelimiter(ReadStr('Package DPL Output')));
    RegCloseKey(Reg);
  end;
end;
{******************************************************************************}
function TProduct.GetBDSProjectsDir: string;
var
  h: HMODULE;
  LocaleName: array[0..4] of Char;
  Filename: string;
  PersDir: string;
begin
  if (Typ = BDS) and (IDEVersion >= Low(BDSVersions)) and (IDEVersion <= High(BDSVersions)) then
  begin
    Result := 'Borland Studio Projects'; // do not localize

    FillChar(LocaleName, SizeOf(LocaleName[0]), 0);
    GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
    if LocaleName[0] <> #0 then
    begin
      Filename := RootDir + '\Bin\coreide' + BDSVersions[IDEVersion].CIV + '.';
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

    SetLength(PersDir, MAX_PATH);
    if SHGetSpecialFolderPath(0, PChar(PersDir), CSIDL_PERSONAL, False) then
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
{******************************************************************************}
procedure FindDxgettext(Version: Integer);
var
  reg: HKEY;
  len: Longint;
  RegTyp: LongWord;
  i: Integer;
  S: string;
begin
 // dxgettext detection
  if RegOpenKeyEx(HKEY_CLASSES_ROOT, 'bplfile\Shell\Extract strings\Command', 0, KEY_QUERY_VALUE or KEY_READ, reg) <> ERROR_SUCCESS then
    Exit;
  SetLength(S, MAX_PATH);
  len := MAX_PATH;
  RegQueryValueEx(reg, '', nil, @RegTyp, PByte(S), @len);
  SetLength(S, StrLen(PChar(S)));
  RegCloseKey(reg);

  if S <> '' then
  begin
    if S[1] = '"' then
    begin
      Delete(S, 1, 1);
      i := 1;
      while (i <= Length(S)) and (S[i] <> '"') do
        Inc(i);
      SetLength(S, i - 1);
    end;
    S := ExtractFileDir(S);
    DxgettextDir := S;
    if not FileExists(DxgettextDir + '\msgfmt.exe') then
      DxgettextDir := ''
    else
    begin
      if Version = 5 then
        S := S + '\delphi5';
      ExtraUnitDirs := ExtraUnitDirs + ';' + S;
    end;
  end;
end;
{******************************************************************************}
function TargetIndexOfProduct(const ed: string): Integer;
begin
  for Result := 0 to High(Targets) do
    if SameText(Targets[Result].Name, ed) or SameText(Targets[Result].PerName, ed) then
      Exit;
  Result := -1;
end;
{******************************************************************************}
procedure AddProduct(const ed: string);
var
  I: Integer;
  Product: TProduct;
begin
  if ed = '' then
    Exit;
  if SameText(ed, 'k3') then // build.exe is for Windows only (maybe CrossKylix)
    Exit;
  for I := 0 to High(Products) do
    if SameText(Products[i].Name, ed) then
      Exit;

  I := TargetIndexOfProduct(ed);
  if I >= 0 then
  begin
    Product := TProduct.Create(ed, Targets[I].PerDir);
    SetLength(Products, Length(Products) + 1);
    Products[High(Products)] := Product;
  end;
end;
{******************************************************************************}
procedure AddAllProducts(AddPersonal: Boolean);
var
  i: Integer;
begin
  Products := nil;
  for i := 0 to High(Targets) do
  begin
    AddProduct(Targets[i].Name);
    if AddPersonal then
      AddProduct(Targets[i].PerName);
  end;
end;
{******************************************************************************}
function GetNewestProduct: TProduct;
var
  I: Integer;
  ed: TProduct;
begin
  Result := TProduct.Create('d5', '');
  for I := High(Targets) downto 0 do
  begin
    ed := TProduct.Create(Targets[I].Name, Targets[I].PerDir);
    try
      if ed.Version >= Result.Version then
      begin
        if (Result.Version < ed.Version) or
           { prefer Delphi version instead of C++Builder version: }
           ((Result.Typ = BCB) and (ed.Typ <> BCB)) or
           { prefer the new version if the result is not valid (no root set) }
           (Result.RootDir = '') then
        begin
          if ed.IsCLX then
            Continue; // this is not a valid version

          if ed.IsValid then
          begin
            Result.Free;
            Result := ed;
            ed := nil;
          end
          else
          begin
            //WriteLn('Ignoring invalid installation or trial version of ', ed.MainName, ' ', ed.VersionStr, '.');
            ed.Free;
            ed := nil;
          end;
        end;
      end;
    finally
      ed.Free;
    end;
  end;
end;
{******************************************************************************}
function GetNewestProductName: string;
var
  ed: TProduct;
begin
  ed := GetNewestProduct;
  try
    if ed <> nil then
      Result := ed.Name
    else
      Result := '';
  finally
    ed.Free;
  end;
end;
{******************************************************************************}
procedure AddNewestProduct;
begin
  Products := nil;
  AddProduct(GetNewestProductName);
end;
{******************************************************************************}
procedure Help;
var
  I: Integer;
begin
  AddAllProducts(True);
  WriteLn('build.exe setups the environment for the given targets and executes the');
  WriteLn('makefile that does the required actions.');
  WriteLn;
  WriteLn('build.exe [TARGET] [OPTIONS]');
  WriteLn('  TARGETS:');

  Write('    ');
  for I := 0 to High(Products) - 1 do
    Write(Products[I].Name, ', ');
  if Length(Products) > 0 then
    WriteLn(Products[High(Products)].Name);
  //WriteLn('    c5, c6, c6p, d5, d5s, d6, d6p, d7, d7p, d7clx, d9');

  WriteLn;
  WriteLn('  OPTIONS:');
  WriteLn('    --make=X        X will be added to the make command line.');
  WriteLn('    --dcc-opt=X     sets the DCCOPT environment variable to X.');
  WriteLn('    --bpl-path=X    sets the BPLDIR and DCPDIR environment variable to X.');
  WriteLn('    --lib-path=X    sets the LIBDIR environment variable to X (BCB only).');
  WriteLn('    --hpp-path=X    sets the HPPDIR environment variable to X (BCB only).');
  WriteLn('                      Defaults to $(ROOT)\Include\Vcl');
  WriteLn('                      Set this to an empty string if you want the hpp files to');
  WriteLn('                      be left in the same directory as their source pas file.');

  for I := 0 to High(ExtraOptions) do
    if ExtraOptions[I].Name <> '' then
      WriteLn('    --', ExtraOptions[I].Name, '=X    sets the ', ExtraOptions[I].Env, ' environment variable to X.');

  WriteLn('    --targets=X     sets the TARGETS environment variable to X. Only these .bpl');
  WriteLn('                    files will be compiled.');
  WriteLn('                    (Example:');
  WriteLn('                      buildtarget "--targets=JvCoreD7R.bpl JvCoreD7R.bpl" )');
  WriteLn;
  WriteLn('    --build         forces the Delphi compiler to build the targets.');
  WriteLn('    --force         Compile/Generate even if the target is not installed.');
  WriteLn('    --verbose       Show all commands that are executed.');
  WriteLn;
end;
{******************************************************************************}
procedure ProcessArgs;
var
  i, j, Count: Integer;
  S: string;
  HppPathSet: Boolean;
begin
  i := 1;
  Count := ParamCount;
  HppPathSet := False;
  while i <= Count do
  begin
    S := ParamStr(i);
    if S[1] = '-' then
    begin
      if StartsText('--make=', S) then
      begin
        Delete(S, 1, 7);
        if S <> '' then
          if Pos(' ', S) > 0 then
            MakeOptions := MakeOptions + ' "' + S + '"'
          else
            MakeOptions := MakeOptions + ' ' + S;
      end
      else if StartsText('--dcc-opt=', S) then
      begin
        Delete(S, 1, 10);
        DccOpt := S;
      end
      else if StartsText('--bpl-path=', S) then
      begin
        Delete(S, 1, 11);
        UserBplDir := S;
        UserDcpDir := S;
      end
      else if StartsText('--lib-path=', S) then
      begin
        Delete(S, 1, 11);
        UserLibDir := S;
      end
      else if StartsText('--hpp-path=', S) then
      begin
        Delete(S, 1, 11);
        SetEnvironmentVariable('HPPDIR', Pointer(S));
        HppPathSet := True;
      end
      else if StartsText('--targets=', S) then
      begin
        Delete(S, 1, 10);
        SetEnvironmentVariable('TARGETS', Pointer(S));
      end
      else if SameText(S, '--build') then
      begin
        DccOpt := DccOpt + ' -B';
      end
      else if SameText('--force', S) then
      begin
        Force := True;
      end
      else if SameText('--verbose', S) then
      begin
        Verbose := True;
      end
      else
      begin
        for j := 0 to High(ExtraOptions) do
        begin
          if (ExtraOptions[I].Name <> '') and StartsText('--' + ExtraOptions[j].Name + '=', S) then
          begin
            Delete(S, 1, 2 + Length(ExtraOptions[j].Name) + 1);
            SetEnvironmentVariable(PChar(ExtraOptions[j].Env), Pointer(S));
          end;
        end
      end;
    end
    else
    begin
      if SameText(S, 'all') then
        AddAllProducts(False)
      else if SameText(S, 'newest') then
      begin
        AddNewestProduct;
        WriteLn('Using ', GetNewestProductName, ' for build process.');
        WriteLn;
      end
      else if TargetIndexOfProduct(S) = -1 then
      begin
        WriteLn('Unknown version: ', S);
        Halt(1);
      end
      else
        AddProduct(S);
    end;
    Inc(i);
  end;
  if not HppPathSet then
    SetEnvironmentVariable('HPPDIR', '$(ROOT)\Include\Vcl');
end;
{******************************************************************************}
function GetLibraryRootDir: string;
var
  I: Integer;
begin
  Result := ExtractFileDir(ParamStr(0));
  for I := 1 to LibraryRootDirRelativeToBuild do
    Result := ExtractFileDir(Result);
end;
{******************************************************************************}
function ExtractShortPathName(const Path: string): string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetShortPathName(PChar(Path), PChar(Result), Length(Result)));
end;
{******************************************************************************}
procedure FixDcc32Cfg(Product: TProduct);
var
  f: TextFile;
  S: string;
  FoundU, FoundLU: Boolean;
begin
  AssignFile(f, Product.RootDir + '\bin\dcc32.cfg');
  if not FileExists(Product.RootDir + '\bin\dcc32.cfg') then
  begin
    // Invalid Delphi/BCB installation or someone deleted the dcc32.cfg file.
    {$I-}
    Rewrite(f);
    {$I+}
    if IOResult = 0 then
    begin
      WriteLn(f, '-aWinTypes=Windows;WinProcs=Windows;DbiProcs=BDE;DbiTypes=BDE;DbiErrs=BDE');
      if Product.Typ <> Delphi then
        WriteLn(f, '-u"', Product.RootDir, '\lib";"', Product.RootDir, '\lib\obj"')
      else
        WriteLn(f, '-u"', Product.RootDir, '\lib"');
      if (Product.Typ = BCB) and (Product.Version = 5) then
        WriteLn(f, '-LUvcl50');
      CloseFile(f);
    end
    else
    begin
      WriteLn('Cannot create missing default ', Product.RootDir, '\bin\dcc32.cfg');
      Halt(0);
    end;
  end
  else
  begin
    FoundU := False;
    FoundLU := (Product.Typ <> BCB) and (Product.Version = 5);
    Reset(f);
    while not EOF(f) and not (FoundU and FoundLU) do
    begin
      ReadLn(f, S);
      if Product.Typ = Delphi then
        FoundU := FoundU or SameText(S, '-u"' + Product.RootDir + '\lib"') or
                  SameText(S, '-u"' + ExtractShortPathName(Product.RootDir) + '\lib"') or
                  SameText(S, '-u' + ExtractShortPathName(Product.RootDir) + '\lib')
      else
        FoundU := FoundU or SameText(S, '-u"' + Product.RootDir + '\lib";"' + Product.RootDir + '\lib\obj"') or
                  SameText(S, '-u"' + ExtractShortPathName(Product.RootDir) + '\lib";"' + ExtractShortPathName(Product.RootDir) + '\lib\obj"') or
                  SameText(S, '-u' + ExtractShortPathName(Product.RootDir) + '\lib;' + ExtractShortPathName(Product.RootDir) + '\lib\obj');
      if (Product.Typ = BCB) and (Product.Version = 5) then
        FoundLU := FoundLU or SameText(S, '-LUvcl50');
    end;
    CloseFile(f);
    if not FoundU or not FoundLU then
    begin
      {$I-}
      Append(f);
      {$I+}
      WriteLn(f);
      if IOResult = 0 then
      begin
        if not FoundU then
        begin
          if Product.Typ <> Delphi then
            WriteLn(f, '-u"', Product.RootDir, '\lib";"', Product.RootDir, '\lib\obj"')
          else
            WriteLn(f, '-u"', Product.RootDir, '\lib"');
        end;
        if not FoundLU and (Product.Typ = BCB) and (Product.Version = 5) then
          WriteLn(f, '-LUvcl50');
        CloseFile(f);
      end
      else
      begin
        WriteLn('You do not have the required permissions to alter the defect ', Product.RootDir, '\bin\dcc32.cfg');
        Halt(0);
      end;
    end;
  end;
end;


var
  I: Integer;
  UnitOutDir, Path: string;
  Product: TProduct;
begin
  LibraryRootDir := GetLibraryRootDir;

  // set ExtraOptions default values
  for I := 0 to High(ExtraOptions) do
    if ExtraOptions[I].Name <> '' then
      SetEnvironmentVariable(PChar(ExtraOptions[I].Env), Pointer(ExtraOptions[I].Default));
  SetEnvironmentVariable(PChar(LibraryName + 'ROOT'), PChar(LibraryRootDir));

  UserBplDir := '';
  UserDcpDir := '';
  UserLibDir := '';

  LoadTargetNames;
  ProcessArgs;

  if Length(Products) = 0 then
  begin
    Help;
    Halt(1);
  end;
  if not Verbose then
  begin
    MakeOptions := ' -s' + MakeOptions;
    SetEnvironmentVariable('QUIET', '-s');
  end
  else
    SetEnvironmentVariable('QUIET', nil);

  for I := 0 to High(Products) do
  begin
    ExtraUnitDirs := '';

    Product := Products[I];
    if Length(Products) > 1 then
      WriteLn('################################ ' + Product.Name + ' #########################################');

    // test for valid root directory/valid IDE installation
    if not Force then
    begin
      if Product.RootDir = '' then
      begin
        WriteLn('Delphi/BCB version not installed or the registry value of ');
        WriteLn('[HKLM\', Product.KeyName, ']\RootDir is empty.');
        Continue;
      end;
    end
    else
    begin
      if Product.RootDir = '' then
        Product := GetNewestProduct;
      if Product.RootDir = '' then
      begin
        WriteLn('Delphi/BCB version not installed or the registry value of ');
        WriteLn('[HKLM\', Product.KeyName, ']\RootDir is empty.');
        Continue;
      end;
    end;

    // correct dcc32.cfg file if necessary
    FixDcc32Cfg(Product);

    UnitOutDir := LibraryRootDir + '\lib\' + Product.MainName;
    if UserDcpDir = '' then
      UserDcpDir := Product.DcpDir;
    if UserBplDir = '' then
      UserBplDir := Product.BplDir;
    if UserLibDir = '' then
      UserLibDir := Product.LibDir;

    FindDxgettext(Product.Version);

    // setup environment and execute make.exe
    Path := GetWindowsDir + ';' + GetSystemDir + ';' + GetWindowsDir + '\Command';
    if UserLibDir <> UserBplDir then
      Path := ExtractShortPathName(Product.RootDir) + '\bin;' + ExtractShortPathName(UserBplDir) + ';' + ExtractShortPathName(UserLibDir) + ';' + Path
    else
      Path := ExtractShortPathName(Product.RootDir) + '\bin;' + ExtractShortPathName(UserBplDir) + ';' + Path;
    { Add original BPL directory for "common" BPLs, but add it as the very last
      path to prevent collisions between packages in TargetConfig.BplDir and
      Target.BplDir. }
    Path := Path + ';' + ExtractShortPathName(Product.BplDir);

    SetEnvironmentVariable('PATH', Pointer(Path));

    SetEnvironmentVariable('MAINBPLDIR', Pointer(Product.BplDir));
    SetEnvironmentVariable('MAINDCPDIR', Pointer(Product.DcpDir));
    SetEnvironmentVariable('BPLDIR', Pointer(UserBplDir));
    SetEnvironmentVariable('DCPDIR', Pointer(UserDcpDir));
    SetEnvironmentVariable('LIBDIR', Pointer(UserLibDir));
    SetEnvironmentVariable('BPILIBDIR', Pointer(UserLibDir));
    SetEnvironmentVariable('PERSONALProduct_OPTION', nil);
    SetEnvironmentVariable('ROOT', PChar(Product.RootDir));
    SetEnvironmentVariable('VERSION', PChar(Product.VersionStr));
    SetEnvironmentVariable('UNITOUTDIR', PChar(UnitOutDir));
    SetEnvironmentVariable('DCCOPT', Pointer(DccOpt));
    SetEnvironmentVariable('DCC', PChar('"' + Product.RootDir + '\bin\dcc32.exe" ' + DccOpt));

    if Product.IsPersonal then
    begin
      SetEnvironmentVariable('PERSONALProduct_OPTION', '-DDelphiPersonalProduct');
      SetEnvironmentVariable('PKGDIR', PChar(Product.PkgDir));
      SetEnvironmentVariable('EDITION', PChar(Product.MainName));
      if Verbose then
        Execute('"' + Product.RootDir + '\bin\make.exe" -f makefile.mak pg.exe')
      else
        Execute('"' + Product.RootDir + '\bin\make.exe" -s -f makefile.mak pg.exe');
    end;

    SetEnvironmentVariable('EDITION', PChar(Product.Name));
    SetEnvironmentVariable('PKGDIR', PChar(Product.PkgDir));

    if (ExtraUnitDirs <> '') and (ExtraUnitDirs[1] = ';') then
      Delete(ExtraUnitDirs, 1, 1);
    SetEnvironmentVariable('EXTRAUNITDIRS', Pointer(ExtraUnitDirs));
    SetEnvironmentVariable('DXGETTEXTDIR', Pointer(DxgettextDir));


    ExitCode := Execute('"' + Product.RootDir + '\bin\make.exe" ' + MakeOptions);
    if ExitCode <> 0 then
    begin
      if ExitCode < 0 then
        WriteLn('Failed: ', '"' + Product.RootDir + '\bin\make.exe" ' + MakeOptions);
      WriteLn('Press ENTER to continue');
      ReadLn;
    end;
  end;
end.

