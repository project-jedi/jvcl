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

Last Modified: 2004-03-25

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

var
  Version, Edition, Root, JVCLRoot, PkgDir, UnitOutDir: string;
  KeyName: string;
  reg: HKEY;
  len: Longint;
  RegTyp: LongWord;

  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  Cmd: string;
  i: Integer;
begin
  Edition := ParamStr(1);

  JVCLRoot := ExtractFileDir(ParamStr(0)); // $(JVCL)\Packages\bin
  JVCLRoot := ExtractFileDir(JVCLRoot); // $(JVCL)\Packages
  JVCLRoot := ExtractFileDir(JVCLRoot); // $(JVCL)

  if Edition = '' then
  begin
    WriteLn('You must specify an Edition.');
    WriteLn('  d5, d6, d7, c5, c6, d5p, d6p, d7p, c6p, d7clx');
    Halt(1);
  end;

  Version := Edition[2];

  if UpCase(Edition[1]) = 'D' then
    KeyName := 'Software\Borland\Delphi\' + Version + '.0'
  else
    KeyName := 'Software\Borland\C++Builder\' + Version + '.0';

  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(KeyName), 0, KEY_QUERY_VALUE or KEY_READ, reg) <> ERROR_SUCCESS then
  begin
    WriteLn('Delphi/BCB version not installed.');
    Halt(2);
  end;
  SetLength(Root, MAX_PATH);
  len := MAX_PATH;
  RegQueryValueEx(reg, 'RootDir', nil, @RegTyp, PByte(Root), @len);
  SetLength(Root, StrLen(PChar(Root)));
  RegCloseKey(reg);

  PkgDir := Edition;
  if UpCase(PkgDir[3]) = 'P' then
    if PkgDir[2] = '5' then
      PkgDir := Copy(PkgDir, 1, 2) + 'std'
    else
      PkgDir := Copy(PkgDir, 1, 2) + 'per';

  UnitOutDir := JVCLRoot + '\lib\' + Copy(Edition, 1, 2);
  //if UpCase(PkgDir[1]) = 'C' then
  //  UnitOutDir := UnitOutDir + '\obj';

 // setup environment and execute build.bat
  SetEnvironmentVariable('ROOT', PChar(Root));
  SetEnvironmentVariable('JVCLROOT', PChar(JVCLRoot));
  SetEnvironmentVariable('VERSION', PChar(Version));
  SetEnvironmentVariable('EDITION', PChar(Edition));
  SetEnvironmentVariable('PKGDIR', PChar(PkgDir));
  SetEnvironmentVariable('UNITOUTDIR', PChar(UnitOutDir));
  Cmd := '"' + Root + '\bin\make.exe" -f makefile.mak';

  for i := 2 to ParamCount do
    Cmd := Cmd + ' "' + ParamStr(i) + '"';

  StartupInfo.cb := SizeOf(StartupInfo);
  GetStartupInfo(StartupInfo);
  if CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
  begin
    CloseHandle(ProcessInfo.hThread);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(ExitCode));
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    Halt(1);
end.
