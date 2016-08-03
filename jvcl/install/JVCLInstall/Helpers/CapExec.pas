{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: CapExec.pas, released on 2003-11-28.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit CapExec;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, SysUtils, Classes;

type
  TCaptureLine = procedure(const Line: string; var Aborted: Boolean) of object;
  TInjectionProc = procedure(const ProcessInfo: TProcessInformation);

var
  CaptureStatusLine: TCaptureLine = nil;

function CaptureExecute(const App, Args, Dir: string; CaptureLine: TCaptureLine;
  OnIdle: TNotifyEvent = nil; CtrlCAbort: Boolean = False; const EnvPath: string = '';
  InjectionProc: TInjectionProc = nil): Integer;

implementation

function GetEnvironmentVariable(const Name: string): string;
begin
  SetLength(Result, 8 * 1024);
  SetLength(Result, Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Length(Result)));
end;

function Oem2Ansi(const Text: AnsiString): AnsiString;
begin
  SetLength(Result, Length(Text));
  if not OemToCharBuffA(PAnsiChar(Text), PAnsiChar(Result), Length(Text)) then
    Result := Text;
end;

function CaptureExecute(const App, Args, Dir: string; CaptureLine: TCaptureLine;
  OnIdle: TNotifyEvent; CtrlCAbort: Boolean; const EnvPath: string;
  InjectionProc: TInjectionProc): Integer;
var
  Aborted: Boolean;

  procedure ProcessInput(hRead: THandle; var Line: AnsiString; CaptureLine: TCaptureLine);
  var
    BytesInPipe, n: Cardinal;
    S: AnsiString;
    I: Integer;
    Found: Boolean;
  begin
    BytesInPipe := 0;
    if not PeekNamedPipe(hRead, nil, 0, nil, @BytesInPipe, nil) then
      BytesInPipe := 0;
    SetLength(S, BytesInPipe);
    if S <> '' then
    begin
      ReadFile(hRead, S[1], BytesInPipe, n, nil);
      SetLength(S, n);
      Line := Line + S;
      repeat
        Found := False;
        for I := 1 to Length(Line) do
          if (Line[I] = #10) or (Line[I] = #13) then
          begin
            if Assigned(CaptureLine) then
              CaptureLine(string(Oem2Ansi(Copy(Line, 1, I - 1))), Aborted);
            if (Line[I] = #13) and (Line[I + 1] = #10) then
            begin
              if (I + 2 <= Length(Line)) and (Line[I + 2] = #13) then
                Delete(Line, 1, I + 2)
              else
                Delete(Line, 1, I + 1);
            end
            else
              Delete(Line, 1, I);
            Found := True;
            Break;
          end;
      until Aborted or not Found;
    end;
  end;

var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  SecAttrib: TSecurityAttributes;
  hRead, hWrite: THandle;
  hAbortRead, hAbortWrite: THandle;
  ReadStatusPipe, WriteStatusPipe: THandle;
  Line: AnsiString;
  StatusLine: AnsiString;
  OrgEnvPath: string;
  Flags: DWORD;
begin
  Result := -2;
  if not Assigned(CaptureLine) then
    Exit;

  FillChar(SecAttrib, SizeOf(SecAttrib), 0);
  with SecAttrib do
  begin
    nLength := SizeOf(SecAttrib);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end; // with

  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);

  Aborted := False;
  if not CreatePipe(hRead, hWrite, @SecAttrib, 0) then
    Exit;
  try
    if not CreatePipe(hAbortRead, hAbortWrite, @SecAttrib, 0) then
      Exit;
    if not CreatePipe(ReadStatusPipe, WriteStatusPipe, @SecAttrib, 0) then
    begin
      WriteStatusPipe := 0;
      ReadStatusPipe := 0;
    end;

    try
      StartupInfo.wShowWindow := SW_HIDE;
      StartupInfo.hStdInput := hAbortRead;
      StartupInfo.hStdOutput := hWrite;
      StartupInfo.hStdError := StartupInfo.hStdOutput; // redirect
      StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;

      OrgEnvPath := GetEnvironmentVariable('PATH');
      if EnvPath <> '' then
        SetEnvironmentVariable('PATH', Pointer(EnvPath));
      try
        Flags := CREATE_DEFAULT_ERROR_MODE;
        if Assigned(InjectionProc) then
          Flags := Flags or CREATE_SUSPENDED;

        if CreateProcess(nil, PChar(App + ' ' + Args), @SecAttrib, nil, True,
           Flags, nil, PChar(Dir), StartupInfo, ProcessInfo) then
        begin
          if Assigned(InjectionProc) then
          begin
            InjectionProc(ProcessInfo);
            ResumeThread(ProcessInfo.hThread);
          end;

          CloseHandle(ProcessInfo.hThread);
          try
            while (WaitForSingleObject(ProcessInfo.hProcess, 80) = WAIT_TIMEOUT) and (not Aborted) do
            begin
              ProcessInput(hRead, Line, CaptureLine);
              if ReadStatusPipe <> 0 then
                ProcessInput(ReadStatusPipe, StatusLine, CaptureStatusLine);
              if Assigned(OnIdle) then
                OnIdle(nil);
            end;
            ProcessInput(hRead, Line, CaptureLine);
            if (Line <> '') and Assigned(CaptureLine) then
              CaptureLine(string(Line), Aborted);
            if ReadStatusPipe <> 0 then
              ProcessInput(ReadStatusPipe, StatusLine, CaptureStatusLine);
            if (StatusLine <> '') and Assigned(CaptureStatusLine) then
              CaptureStatusLine(string(StatusLine), Aborted);
            if Aborted then
            begin
              if CtrlCAbort then
              begin
                GenerateConsoleCtrlEvent(CTRL_C_EVENT, ProcessInfo.dwProcessId);
                if WaitForSingleObject(ProcessInfo.hProcess, 500) = WAIT_TIMEOUT then
                  TerminateProcess(ProcessInfo.hProcess, Cardinal(1));
              end
              else
                TerminateProcess(ProcessInfo.hProcess, Cardinal(1));
            end;
            GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(Result));
          finally
            CloseHandle(ProcessInfo.hProcess);
          end;
        end
        else
          Result := -1;
      finally
        if EnvPath <> '' then
          SetEnvironmentVariable('PATH', Pointer(OrgEnvPath));
      end;
    finally
      if WriteStatusPipe <> 0 then
        CloseHandle(WriteStatusPipe);
      if ReadStatusPipe <> 0 then
        CloseHandle(ReadStatusPipe);
      CloseHandle(hAbortRead);
      CloseHandle(hAbortWrite);
    end;
  finally
    CloseHandle(hRead);
    CloseHandle(hWrite);
  end;
end;

procedure NoHooking;
begin
end;

exports
  NoHooking; // prevent DllInjection to hook this process

end.