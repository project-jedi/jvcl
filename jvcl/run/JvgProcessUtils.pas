{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgProcessUtils.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgProcessUtils;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, SysUtils;

// (rom) definitely goes to JCL

procedure GetProcessList(const SList: TStrings);
procedure KillProcessByName(Name: string);

const
  // (rom) from WINNT.h
  PROCESS_TERMINATE         = $0001;
  {$EXTERNALSYM PROCESS_TERMINATE}
  PROCESS_CREATE_THREAD     = $0002;
  {$EXTERNALSYM PROCESS_CREATE_THREAD}
  PROCESS_SET_SESSIONID     = $0004;
  {$EXTERNALSYM PROCESS_SET_SESSIONID}
  PROCESS_VM_OPERATION      = $0008;
  {$EXTERNALSYM PROCESS_VM_OPERATION}
  PROCESS_VM_READ           = $0010;
  {$EXTERNALSYM PROCESS_VM_READ}
  PROCESS_VM_WRITE          = $0020;
  {$EXTERNALSYM PROCESS_VM_WRITE}
  PROCESS_DUP_HANDLE        = $0040;
  {$EXTERNALSYM PROCESS_DUP_HANDLE}
  PROCESS_CREATE_PROCESS    = $0080;
  {$EXTERNALSYM PROCESS_CREATE_PROCESS}
  PROCESS_SET_QUOTA         = $0100;
  {$EXTERNALSYM PROCESS_SET_QUOTA}
  PROCESS_SET_INFORMATION   = $0200;
  {$EXTERNALSYM PROCESS_SET_INFORMATION}
  PROCESS_QUERY_INFORMATION = $0400;
  {$EXTERNALSYM PROCESS_QUERY_INFORMATION}
  PROCESS_ALL_ACCESS        = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $0FFF;
  {$EXTERNALSYM PROCESS_ALL_ACCESS}

implementation

uses
  TLHelp32;

procedure GetProcessList(const SList: TStrings);
var
  hSnapshot: THandle;
  Pe32: TProcessEntry32;
begin
  SList.BeginUpdate;
  try
    SList.Clear;
    hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    Pe32.dwSize := SizeOf(TProcessEntry32);

    if Process32First(hSnapshot, Pe32) then
      repeat
        SList.Add(Format('%x, %x: %s', [Pe32.th32ProcessID, Pe32.th32ParentProcessID, Pe32.szExeFile]));
      until not Process32Next(hSnapshot, Pe32);

    CloseHandle(hSnapshot);
  finally
    SList.EndUpdate;
  end;
end;

procedure KillProcessByName(Name: string);
var
  hProc, hSnapshot: THandle;
  Pe32: TProcessEntry32;
begin
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  Pe32.dwSize := SizeOf(TProcessEntry32);

  if Process32First(hSnapshot, Pe32) then
    repeat
      if CompareText(ExtractFileName(Pe32.szExeFile), Name) = 0 then
      begin
        hProc := OpenProcess(PROCESS_TERMINATE, False, Pe32.th32ProcessID);
        if hProc <> 0 then
          TerminateProcess(hProc, 0);
      end;
    until not Process32Next(hSnapshot, Pe32);

  CloseHandle(hSnapshot);
end;

end.

