{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgProcessUtils.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgProcessUtils;

interface

uses
  Windows, Messages, Classes, SysUtils;

procedure GetProcessList(const SList: TStrings);
procedure KillProcessByName(Name: string);

const
  PROCESS_TERMINATE = $0001;
  PROCESS_CREATE_THREAD = $0002;
  PROCESS_VM_OPERATION = $0008;
  PROCESS_VM_READ = $0010;
  PROCESS_VM_WRITE = $0020;
  PROCESS_DUP_HANDLE = $0040;
  PROCESS_CREATE_PROCESS = $0080;
  PROCESS_SET_QUOTA = $0100;
  PROCESS_SET_INFORMATION = $0200;
  PROCESS_QUERY_INFORMATION = $0400;
  PROCESS_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $0FFF;

implementation

uses
  TLHelp32;

procedure GetProcessList(const SList: TStrings);
var
  hSnapshot: THandle;
  Pe32: TProcessEntry32;
begin
  SList.Clear;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  Pe32.dwSize := SizeOf(TProcessEntry32);

  if Process32First(hSnapshot, Pe32) then
    repeat
      SList.Add(Format('%x, %x: %s', [Pe32.th32ProcessID, Pe32.th32ParentProcessID, Pe32.szExeFile]));
    until not Process32Next(hSnapshot, Pe32);
  CloseHandle(hSnapshot);
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
