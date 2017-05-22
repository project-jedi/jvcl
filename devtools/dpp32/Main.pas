{**************************************************************************************************}
{                                                                                                  }
{ Delphi language Preprocessor (dpp32)                                                             }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is Main.pas                                                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen                                  }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Projects home page, located at           }
{ http://www.sourceforge.net/projects/dpp32                                                        }
{                                                                                                  }
{**************************************************************************************************}

{ program arguments:
   -dppCompile           preprocess files and compile them with dcc/dcc32
   -dppCaseInsensitive   macros are case insensitive
   -dppConditional       macros are replaced for conditions that are true
}
unit Main;
interface
uses
{$ifdef MSWINDOWS}
  Windows,
{$endif}
{$ifdef LINUX}
  Libc,
{$endif}
  SysUtils, Classes, dpp_PreProcess, dpp_Utils;

function EntryPoint: Integer;

implementation
const
{$ifdef MSWINDOWS}
  dcc = 'dcc32.exe';
{$endif}
{$ifdef LINUX}
  dcc = 'dcc';
{$endif}

var
  PreProcessor: TPreProcessor;
  Filename: string;

{$ifdef MSWINDOWS}
function ExecuteConsole(const Cmd: string): Integer;
var
  ProcessInfo: TProcessInformation;
  StartInfo: TStartupInfo;
begin
  AllocConsole;
  StartInfo.cb := SizeOf(StartInfo);
  GetStartupInfo(StartInfo);
  StartInfo.dwFlags := STARTF_USESTDHANDLES;
  StartInfo.hStdOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  StartInfo.hStdError := GetStdHandle(STD_ERROR_HANDLE);
  StartInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  if CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil, nil, StartInfo, ProcessInfo) then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(Result));
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    RaiseLastOSError;
end;
{$endif}
{$ifdef LINUX}
function ExecuteConsole(const Cmd: string): Integer;
begin
  Result := Libc.system(PChar(Cmd));
end;
{$endif}
function GetDccArgs: string;
var
  i: Integer;
  s: String;
begin
  Result := '';
  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    if SameText(s, '-dppCompile') or
       SameText(s, '-dppCaseInsensitive') or
       SameText(s, '-dppConditional') then
      Continue;
    if Pos(' ', s) > 0 then s := '"' + s + '"';
    Result := Result + ' ' + s;
  end;
  Delete(Result, 1, 1);
end;

procedure SetPreProcessorParam(S: string);
var ps: Integer;
begin
 // remove ""
  ps := Pos('"', s);
  while ps > 0 do
  begin
    Delete(S, ps, 1);
    ps := Pos('"', S);
  end;

  if SameText(S, '-dppCompile') then // Preprocessor command switch
    PreProcessor.CompilePrepare := True
  else if SameText(S, '-dppCaseInsensitive') then // Preprocessor command switch
    PreProcessor.CaseSensitive := False
  else if SameText(S, '-dppConditional') then // Preprocessor command switch
    PreProcessor.ConditionalParse := True
  else if StartsText('-U', S) then
    PreProcessor.UnitPaths := PreProcessor.UnitPaths + Copy(s, 3, MaxInt)
  else if StartsText('-I', S) then
    PreProcessor.IncludePaths := PreProcessor.IncludePaths + Copy(s, 3, MaxInt)
  else if StartsText('-D', S) then // conditionals
    PreProcessor.Conditionals.Add(Copy(s, 3, MaxInt))
  else if (s[1] <> '-') and (FileExistsX(S)) then
    Filename := S;
end;

procedure LoadConfigFile(const ConfigFile: string);
var
  List: TStrings;
  i: Integer;
begin
  List := TStringList.Create;
  try
    List.LoadFromFile(ConfigFile);
    for i := 0 to List.Count - 1 do
      SetPreProcessorParam(List[i]);
  finally
    List.Free;
  end;
end;

function EntryPoint: Integer;
var
  i: Integer;
  ConfigFile: string;
begin
  try
    PreProcessor := TPreProcessor.Create(TNoVirtualFileSys.Create);
    try
      PreProcessor.CompilePrepare := False;

     // Config file and Arguments
      ConfigFile := IncludeTrailingPathDelimiter(GetCurrentDir) + ChangeFileExt(dcc, '.cfg');
      if not FileExistsX(ConfigFile) then
        ConfigFile := ExtractFilePath(ParamStr(0)) + ChangeFileExt(dcc, '.cfg');

      if FileExistsX(ConfigFile) then
        LoadConfigFile(ConfigFile);

      for i := 1 to ParamCount do
        SetPreProcessorParam(ParamStr(i));


     // file not found -> let dcc32 handle this
      if not FileExistsX(Filename) then
      begin
        Result := ExecuteConsole(dcc + ' ' + GetDccArgs);
        Exit;
      end;

     // proceed
      WriteLn('Delphi language Preprocessor Version ' + PreProcessorVersion);
      WriteLn('Copyright (c) 2003 Andreas Hausladen');

//      SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
      try
        if PreProcessor.BeginPreProcessing(Filename) then
        begin
          try
            if PreProcessor.CompilePrepare then
            begin
              WriteLn;
              Result := ExecuteConsole(dcc + ' -DPREPROCESSOR ' + GetDccArgs)
            end
            else
              Result := 0;
          finally
            PreProcessor.EndPreProcessing;
          end;
        end
        else
          Result := 1;
      finally
//        SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
      end;
    finally
      PreProcessor.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, E.Message);
      Result := 1;
    end;
  end;
//  if Result <> 0 then ReadLn;
end;

end.
