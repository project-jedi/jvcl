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

{$I jvcl.inc}

program build;

{$APPTYPE CONSOLE}

uses
  Windows, ShlObj;

type
  TTarget = record
    Name: string;
    PerName: string;
    PerDir: string;
  end;

const // keep in sync with JVCL Installer's DelphiData.pas
  BDSVersions: array[1..3] of record
                                Name: string;
                                VersionStr: string;
                                Version: Integer;
                                CIV: string; // coreide version
                                ProjectDirResId: Integer;
                                Supported: Boolean;
                              end = (
    (Name: 'C#Builder'; VersionStr: '1.0'; Version: 1; CIV: '71'; ProjectDirResId: 64507; Supported: False),
    (Name: 'Delphi'; VersionStr: '8'; Version: 8; CIV: '71'; ProjectDirResId: 64460; Supported: False),
    (Name: 'Delphi'; VersionStr: '2005'; Version: 9; CIV: '90'; ProjectDirResId: 64431; Supported: True)
  );

var
  Edition: string;
  BDSProjectsDir: string;
  RootDir: string;
  JVCLRootDir: string;
  UnitOutDir: string;
  DxgettextDir: string = '';
  ExtraUnitDirs: string = '';
  MakeOptions: string = '';
  Verbose: Boolean = False;
  Force: Boolean = False; // force even if the target is not installed

  DccOpt: string = '-Q -M';
  LibDir, DcpDir, BplDir: string;

  Editions: array of string = nil;
  Targets: array of TTarget = nil;

function ExtractFileDir(const S: string): string;
var
  ps: Integer;
begin
  ps := Length(S);
  while (ps > 1) and (S[ps] <> '\') do
    Dec(ps);
  Result := Copy(S, 1, ps - 1);
end;

function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  if (S <> '') and (S[Length(S)] = '\') then
    Result := Copy(S, 1, Length(S) - 1)
  else
    Result := S;
end;

function StrLen(P: PChar): Integer;
begin
  Result := 0;
  while P[Result] <> #0 do
    Inc(Result);
end;

function StrToInt(const S: string): Integer;
var
  Error: Integer;
begin
  Val(S, Result, Error);
end;

function IntToStr(Value: Integer): string;
begin
  Str(Value, Result);
end;

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

function FileExists(const Filename: string): Boolean;
var
  attr: Cardinal;
begin
  attr := GetFileAttributes(PChar(Filename));
  Result := (attr <> $FFFFFFFF) and (attr and FILE_ATTRIBUTE_DIRECTORY = 0);
end;

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

function GetWindowsDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetWindowsDirectory(PChar(Result), Length(Result)));
end;

function GetSystemDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetSystemDirectory(PChar(Result), Length(Result)));
end;

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

{ TTag }

constructor TTag.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

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

{ TAttr }

constructor TAttr.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

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


procedure LoadTargetNames;
var
  xml: TXmlFile;
  tg: ITag;
begin
  xml := TXmlFile.Create(JVCLRootDir + '\devtools\bin\pgEdit.xml');
  try
    tg := xml.NextTag;
    while tg <> nil do
    begin
      if SameText(tg.Name, 'model') and SameText(tg.Attrs('name').Value, 'JVCL') then
      begin
        tg := xml.NextTag;
        while not SameText(tg.Name, 'targets') do
          tg := xml.NextTag;
        while not SameText(tg.Name, '/targets') do
        begin
          if SameText(tg.Name, 'target') then
          begin
            if FileExists(JVCLRootDir + '\packages\' + tg.Attrs('name').Value + ' Packages.bpg') or
               FileExists(JVCLRootDir + '\packages\' + tg.Attrs('name').Value + ' Packages.bdsgroup') then
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

        Break; // we do only want the JVCL part
      end;
      tg := xml.NextTag;
    end;
  finally
    xml.Free;
  end;
end;

function IndexOfEdition(const ed: string): Integer;
begin
  for Result := 0 to High(Targets) do
    if SameText(Targets[Result].Name, ed) or SameText(Targets[Result].PerName, ed) then
      Exit;
  Result := -1;
end;

type
  TEdition = record
    Typ: (Delphi, BCB, BDS);
    VersionStr: string;     // '9' for BDS 3.0
    Version: Integer;       // 9 for BDS 3.0
    IDEVersionStr: string;  // '3' for BDS 3.0
    IDEVersion: Integer;    // 3 for BDS 3.0
    MainEdition: string;    // d7
    ActualEdition: string;  // d7p
    PkgDir: string;         // d7 / d7per
    Personal: Boolean;
    CLX: Boolean;
  end;

function GetEdition(const Edition: string): TEdition;
var
  Index: Integer;
begin
  if UpCase(Edition[1]) = 'D' then
    Result.Typ := Delphi
  else
    Result.Typ := BCB;

  Result.VersionStr := Edition[2];
  if (Length(Edition) > 2) and (Edition[3] in ['0'..'9']) then
  begin
    Result.VersionStr := Result.VersionStr + Edition[3];
    Index := 4;
  end
  else
    Index := 3;

  Result.Version := StrToInt(Result.VersionStr);
  Result.IDEVersionStr := Result.VersionStr;
  Result.IDEVersion := Result.Version;

  if Result.Version > 7 then
  begin
    Result.Typ := BDS;
    Result.IDEVersion := Result.Version - 6; // D 8 = BDS 2
    Result.IDEVersionStr := IntToStr(Result.IDEVersion);
  end;

  Result.MainEdition := Copy(Edition, 1, Index - 1);
  Result.ActualEdition := Edition;
  Result.PkgDir := Edition;
  Result.CLX := SameText('clx', Copy(Edition, Index, 3));
  Result.Personal := False;
  if Length(Edition) > Index then
  begin
    if (UpCase(Edition[Index]) = 'P') or (UpCase(Edition[Index]) = 'S') then
    begin
      Result.Personal := True;
      if Result.Version = 5 then
        Result.PkgDir := Copy(Edition, 1, Index - 1) + 'std'
      else
        Result.PkgDir := Copy(Edition, 1, Index - 1) + 'per'
    end;
  end;
end;

function GetRootDirOf(const Edition: string): string;
var
  KeyName: string;
  reg: HKEY;
  len: Longint;
  RegTyp: LongWord;
  ed: TEdition;
begin
  ed := GetEdition(Edition);
  Result := '';

  case ed.Typ of
    Delphi:
      KeyName := 'Software\Borland\Delphi\' + ed.IDEVersionStr + '.0';
    BCB:
      KeyName := 'Software\Borland\C++Builder\' + ed.IDEVersionStr + '.0';
    BDS:
      KeyName := 'Software\Borland\BDS\' + ed.IDEVersionStr + '.0';
  end;

  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(KeyName), 0, KEY_QUERY_VALUE or KEY_READ, reg) <> ERROR_SUCCESS then
    Exit;
  SetLength(Result, MAX_PATH);
  len := MAX_PATH;
  RegQueryValueEx(reg, 'RootDir', nil, @RegTyp, PByte(Result), @len);
  SetLength(Result, StrLen(PChar(Result)));
  RegCloseKey(reg);
  Result := ExcludeTrailingPathDelimiter(Result);
end;

procedure FindDxgettext(const Version: string);
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
    if Version = '5' then
      S := S + '\delphi5';
    ExtraUnitDirs := ExtraUnitDirs + ';' + S;
  end;
end;

function ReadBDSProjectsDir(const ed: TEdition): string;
var
  h: HMODULE;
  LocaleName: array[0..4] of Char;
  Filename: string;
  PersDir: string;
begin
  if (ed.Typ = BDS) and (ed.IDEVersion >= Low(BDSVersions)) and (ed.IDEVersion <= High(BDSVersions)) then
  begin
    Result := 'Borland Studio Projects'; // do not localize

    FillChar(LocaleName, SizeOf(LocaleName[0]), 0);
    GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
    if LocaleName[0] <> #0 then
    begin
      Filename := RootDir + '\Bin\coreide' + BDSVersions[ed.IDEVersion].CIV + '.';
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
          SetLength(Result, LoadString(h, BDSVersions[ed.IDEVersion].ProjectDirResId, PChar(Result), Length(Result) - 1));
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

procedure AddEdition(const ed: string);
var
  i: Integer;
begin
  if ed = '' then
    Exit;
  {$IFDEF MSWINDOWS}
  if SameText(ed, 'k3') then
    Exit;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  if not SameText(ed, 'k3') then
    Exit;
  {$ENDIF LINUX}
  for i := 0 to High(Editions) do
    if SameText(Editions[i], ed) then
      Exit;
  SetLength(Editions, Length(Editions) + 1);
  Editions[High(Editions)] := ed;
end;

procedure AddAllEditions(AddPersonal: Boolean);
var
  i: Integer;
begin
  Editions := nil;
  for i := 0 to High(Targets) do
  begin
    AddEdition(Targets[i].Name);
    if AddPersonal then
      AddEdition(Targets[i].PerName);
  end;
end;

function GetNewestEditionName: string;
var
  i: Integer;
  RetEd: TEdition;
  ed: TEdition;
  LRootDir: string;
begin
  Result := 'd5';
  for i := High(Targets) downto 0 do
  begin
    RetEd := GetEdition(Result);
    ed := GetEdition(Targets[i].Name);
    if ed.Version >= RetEd.Version then
    begin
      if (RetEd.Version < ed.Version) or
         { prefere Delphi version instead of C++Builder version: }
         ((RetEd.Typ = BCB) and (ed.Typ <> BCB)) then
      begin
        if ed.CLX then
          Continue; // this is not a valid version

        LRootDir := GetRootDirOf(ed.ActualEdition);
        if (LRootDir <> '') and FileExists(LRootDir + '\bin\dcc32.exe') then
          Result := Targets[i].Name;
      end;
    end;
  end;
end;

procedure AddNewestEdition;
begin
  Editions := nil;
  AddEdition(GetNewestEditionName);
end;

procedure Help;
var
  i: Integer;
begin
  AddAllEditions(True);
  WriteLn('build.exe setups the environment for the given targets and executes the');
  WriteLn('make file that does the required actions.');
  WriteLn;
  WriteLn('build.exe [TARGET] [OPTIONS]');
  WriteLn('  TARGETS:');

  Write('    ');
  for i := 0 to High(Editions) - 1 do
    Write(Editions[i], ', ');
  if Length(Editions) > 0 then
    WriteLn(Editions[High(Editions)]);
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

  WriteLn('    --jcl-path=X    sets the JCLROOT environment variable to X.');

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

procedure ProcessArgs;
var
  i, Count: Integer;
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
      else if StartsText('--jcl-path=', S) then
      begin
        Delete(S, 1, 11);
        SetEnvironmentVariable('JCLROOT', Pointer(S));
      end
      else if StartsText('--bpl-path=', S) then
      begin
        Delete(S, 1, 11);
        BplDir := S;
        DcpDir := S;
      end
      else if StartsText('--lib-path=', S) then
      begin
        Delete(S, 1, 11);
        LibDir := S;
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
      end;
    end
    else
    begin
      if SameText(S, 'all') then
      begin
        AddAllEditions(False);
      end
      else if SameText(S, 'newest') then
      begin
        AddNewestEdition;
        WriteLn('Using ', GetNewestEditionName, ' for build process.');
        WriteLn;
      end
      else if IndexOfEdition(S) = -1 then
      begin
        WriteLn('Unknown edition: ', S);
        Halt(1);
      end
      else
        AddEdition(S);
    end;
    Inc(i);
  end;
  if not HppPathSet then
    SetEnvironmentVariable('HPPDIR', '$(ROOT)\Include\Vcl');
end;

var
  i: Integer;
  Path: string;
  ed: TEdition;
begin
  JVCLRootDir := ExtractFileDir(ParamStr(0)); // $(JVCL)\Packages\bin
  JVCLRootDir := ExtractFileDir(JVCLRootDir); // $(JVCL)\Packages
  JVCLRootDir := ExtractFileDir(JVCLRootDir); // $(JVCL)

  SetEnvironmentVariable('JCLROOT', '..\..\..\jcl'); // meight be changed by command line option
  SetEnvironmentVariable('JVCLROOT', PChar(JVCLRootDir));

  BplDir := '';
  DcpDir := '';
  LibDir := '';

  LoadTargetNames;
  ProcessArgs;

  if Length(Editions) = 0 then
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

  for i := 0 to High(Editions) do
  begin
    ExtraUnitDirs := '';

    Edition := Editions[i];
    if Length(Editions) > 1 then
      WriteLn('################################ ' + Edition + ' #########################################');

    // test for valid root directory/valid IDE installation
    RootDir := GetRootDirOf(Edition);
    if not Force then
    begin
      if RootDir = '' then
      begin
        WriteLn('Delphi/BCB version not installed.');
        Continue;
      end;
    end
    else
    begin
      if RootDir = '' then
        RootDir := GetRootDirOf(GetNewestEditionName);
      if RootDir = '' then
      begin
        WriteLn('No Delphi/BCB version installed.');
        Continue;
      end;
    end;

    ed := GetEdition(Edition);

    UnitOutDir := JVCLRootDir + '\lib\' + ed.MainEdition;
    if DcpDir = '' then
    begin
      if ed.Typ = BDS then
      begin
        BDSProjectsDir := ReadBDSProjectsDir(ed);
        BplDir := BDSProjectsDir + '\Bpl';
        DcpDir := BDSProjectsDir + '\Bpl';
        LibDir := BDSProjectsDir + '\Bpl';
      end
      else
      begin
        BplDir := RootDir + '\Projects\Bpl';
        DcpDir := RootDir + '\Projects\Bpl';
        LibDir := RootDir + '\Projects\Lib';
      end;
    end;

    FindDxgettext(ed.VersionStr);

   // setup environment and execute build.bat
    Path := GetWindowsDir + ';' + GetSystemDir + ';' + GetWindowsDir + '\Command';
    if LibDir <> BplDir then
      Path := RootDir + ';' + BplDir + ';' + LibDir + ';' + Path
    else
      Path := RootDir + ';' + BplDir + ';' + Path;
    SetEnvironmentVariable('PATH', Pointer(Path));

    SetEnvironmentVariable('BPLDIR', Pointer(BplDir));
    SetEnvironmentVariable('DCPDIR', Pointer(DcpDir));
    SetEnvironmentVariable('LIBDIR', Pointer(LibDir));
    SetEnvironmentVariable('BPILIBDIR', Pointer(LibDir));
    SetEnvironmentVariable('PERSONALEDITION_OPTION', nil);
    SetEnvironmentVariable('ROOT', PChar(RootDir));
    SetEnvironmentVariable('VERSION', PChar(ed.VersionStr));
    SetEnvironmentVariable('UNITOUTDIR', PChar(UnitOutDir));
    SetEnvironmentVariable('DCCOPT', Pointer(DccOpt));
    SetEnvironmentVariable('DCC', PChar('"' + RootDir + '\bin\dcc32.exe" ' + DccOpt));

    if ed.Personal then
    begin
      SetEnvironmentVariable('PERSONALEDITION_OPTION', '-DDelphiPersonalEdition');
      SetEnvironmentVariable('PKGDIR', PChar(ed.PkgDir));
      SetEnvironmentVariable('EDITION', PChar(ed.MainEdition));
      if Verbose then
        Execute('"' + RootDir + '\bin\make.exe" -l+ -f makefile.mak pg.exe')
      else
        Execute('"' + RootDir + '\bin\make.exe" -l+ -s -f makefile.mak pg.exe');
    end;

    SetEnvironmentVariable('EDITION', PChar(ed.ActualEdition));
    SetEnvironmentVariable('PKGDIR', PChar(ed.PkgDir));

    if (ExtraUnitDirs <> '') and (ExtraUnitDirs[1] = ';') then
      Delete(ExtraUnitDirs, 1, 1);
    SetEnvironmentVariable('EXTRAUNITDIRS', Pointer(ExtraUnitDirs));
    SetEnvironmentVariable('DXGETTEXTDIR', Pointer(DxgettextDir));


    ExitCode := Execute('"' + RootDir + '\bin\make.exe" -l+ ' + MakeOptions);
    if ExitCode <> 0 then
    begin
      if ExitCode < 0 then
        WriteLn('Failed: ', '"' + RootDir + '\bin\make.exe" -l+ ' + MakeOptions);
      WriteLn('Press ENTER to continue');
      ReadLn;
    end;

  end;
end.

