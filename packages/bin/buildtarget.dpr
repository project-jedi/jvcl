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

Last Modified: 2004-03-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

program buildtarget;
{$APPTYPE CONSOLE}
uses
  Windows;

function ExtractFileDir(const S: string): string;
var
  ps: Integer;
begin
  ps := Length(S);
  while (ps > 1) and (S[ps] <> '\') do
    Dec(ps);
  Result := Copy(S, 1, ps - 1);
end;

function StrLen(P: PChar): Integer;
begin
  Result := 0;
  while P[Result] <> #0 do
    Inc(Result);
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

function FileExists(const Filename: string): Boolean;
var
  attr: Cardinal;
begin
  attr := GetFileAttributes(PChar(Filename));
  Result := (attr <> $FFFFFFFF) and (attr and FILE_ATTRIBUTE_DIRECTORY = 0);
end;

type
  TTarget = record
    Name: string;
    PerName: string;
    PerDir: string;
  end;

var
  Targets: array of TTarget = nil;

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

var
  Version: string;
  Edition: string;
  Root: string;
  JVCLRoot: string;
  PkgDir: string;
  UnitOutDir: string;
  Editions: array of string = nil;


procedure LoadTargetNames;
var
  xml: TXmlFile;
  tg: ITag;
begin
  xml := TXmlFile.Create(JVCLRoot + '\devtools\bin\pgEdit.xml');
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
            if FileExists(JVCLRoot + '\packages\' + tg.Attrs('name').Value + ' Packages.bpg') then
            begin
              SetLength(Targets, Length(Targets) + 1); // I know that I should not do this, but it is that easier
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

procedure AddEdition(const ed: string);
var
  i: Integer;
begin
  if ed = '' then
    Exit;
  for i := 0 to High(Editions) do
    if SameText(Editions[i], ed) then
      Exit;
  SetLength(Editions, Length(Editions) + 1);
  Editions[High(Editions)] := ed;
end;

procedure AddAllEditions;
var
  i: Integer;
begin
  Editions := nil;
  for i := 0 to High(Targets) do
  begin
    AddEdition(Targets[i].Name);
    AddEdition(Targets[i].PerName);
  end;
end;

var
  KeyName: string;
  reg: HKEY;
  len: Longint;
  RegTyp: LongWord;
  MakeOptions, S: string;

  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  Cmd: string;
  i, Count: Integer;
begin
  JVCLRoot := ExtractFileDir(ParamStr(0)); // $(JVCL)\Packages\bin
  JVCLRoot := ExtractFileDir(JVCLRoot); // $(JVCL)\Packages
  JVCLRoot := ExtractFileDir(JVCLRoot); // $(JVCL)

  LoadTargetNames;

 // read command line 
  i := 1;
  Count := ParamCount;
  while i <= Count do
  begin
    S := ParamStr(i);
    if S[1] = '-' then
    begin
      if SameText(S, '-MAKE') then
      begin
        Inc(i);
        MakeOptions := MakeOptions + ' "' + ParamStr(i) + '"';
      end;
    end
    else
    begin
      if SameText(S, 'all') then
      begin
        AddAllEditions;
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

  if Length(Editions) = 0 then
  begin
    WriteLn('You must specify an Edition.');
    AddAllEditions;
    Write('  available editions: ');
    for i := 0 to High(Editions) - 1 do
      Write(Editions[i], ', ');
    if Length(Editions) > 0 then
      Write(Editions[High(Editions)]);
    Halt(1);
  end;

  for i := 0 to High(Editions) do
  begin
    Edition := Editions[i];
    if Length(Editions) > 1 then
      WriteLn('################################ ' + Edition + ' #########################################');

    Version := Edition[2];

    if UpCase(Edition[1]) = 'D' then
      KeyName := 'Software\Borland\Delphi\' + Version + '.0'
    else
      KeyName := 'Software\Borland\C++Builder\' + Version + '.0';

    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(KeyName), 0, KEY_QUERY_VALUE or KEY_READ, reg) <> ERROR_SUCCESS then
    begin
      WriteLn('Delphi/BCB version not installed.');
      Continue;
    end;
    SetLength(Root, MAX_PATH);
    len := MAX_PATH;
    RegQueryValueEx(reg, 'RootDir', nil, @RegTyp, PByte(Root), @len);
    SetLength(Root, StrLen(PChar(Root)));
    RegCloseKey(reg);

    PkgDir := Edition;
    if (UpCase(PkgDir[3]) = 'P') or (UpCase(PkgDir[3]) = 'S') then
      if PkgDir[2] = '5' then
        PkgDir := Copy(PkgDir, 1, 2) + 'std'
      else
        PkgDir := Copy(PkgDir, 1, 2) + 'per';

    UnitOutDir := JVCLRoot + '\lib\' + Copy(Edition, 1, 2);

   // setup environment and execute build.bat
    SetEnvironmentVariable('ROOT', PChar(Root));
    SetEnvironmentVariable('JVCLROOT', PChar(JVCLRoot));
    SetEnvironmentVariable('VERSION', PChar(Version));
    SetEnvironmentVariable('EDITION', PChar(Edition));
    SetEnvironmentVariable('PKGDIR', PChar(PkgDir));
    SetEnvironmentVariable('UNITOUTDIR', PChar(UnitOutDir));
    Cmd := '"' + Root + '\bin\make.exe" -f makefile.mak' + MakeOptions;

    StartupInfo.cb := SizeOf(StartupInfo);
    GetStartupInfo(StartupInfo);
    if CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
    begin
      CloseHandle(ProcessInfo.hThread);
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(ExitCode));
      CloseHandle(ProcessInfo.hProcess);
      if ExitCode <> 0 then
      begin
        WriteLn('Press ENTER to continue');
        ReadLn;
      end;
    end
    else
      Halt(1);

  end;
end.
