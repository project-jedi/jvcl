{$I jvcl.inc}
unit JTouchUtils;

interface

procedure Run;

implementation
uses
  Windows, SysUtils, JTools, JvSearchFiles;

type
  TTouchType = (mtCreated, mtChanged, mtAccess);
  TTouchTypes = set of TTouchType;

function StrToType(const S: string): TTouchTypes;
var i: integer;
begin
  Result := [];
  for i := 1 to Length(S) do
    case S[i] of
      'C': Include(Result, mtCreated);
      'H': Include(Result, mtChanged);
      'L': Include(Result, mtAccess);
    end;
end;

function StrToAttrs(const S: string): Cardinal;
var i: integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    case UpCase(S[i]) of
      'A':
        Result := Result or FILE_ATTRIBUTE_ARCHIVE;
      'S':
        Result := Result or FILE_ATTRIBUTE_SYSTEM;
      'R':
        Result := Result or FILE_ATTRIBUTE_READONLY;
      'H':
        Result := Result or FILE_ATTRIBUTE_HIDDEN;
    end;
end;

function DoChangeTimes(const Filename: string; ft: TFileTime; ATypes: TTouchTypes): boolean;
var hFile: integer;
  aCreate, aChange, aAccess: TFileTime;
begin
  Result := false;
  hFile := FileOpen(Filename, fmOpenReadWrite or fmShareDenyWrite);
  if hFile = -1 then
    Exit;
  try
    GetFileTime(hFile, @aCreate, @aAccess, @aChange);
    if mtCreated in ATypes then
      aCreate := ft;
    if mtChanged in ATypes then
      aChange := ft;
    if mtAccess in ATypes then
      aAccess := ft;
    Result := SetFileTime(hFile, @aCreate, @aAccess, @aChange);
  finally
    FileClose(hFile);
  end;
end;

function DateTimeToFileTime(ADate: TDateTime): TFileTime;
var
  tmp: integer;
  LocalFileTime: TFileTime;
begin
  tmp := DateTimeToFileDate(ADate);
  DosDateTimeToFileTime(LongRec(tmp).Hi, LongRec(tmp).Lo, LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, Result);
end;

function DoTouch(const Filemask: string; ATouchDate: TDateTime; ATypes: TTouchTypes; IncAttr, ExcAttr: Cardinal; Recurse, ForceTouch: boolean): integer;
var fs: TJvSearchFiles;
  i: integer;
  tmpAttr: Cardinal;
  ft: TFileTime;
begin
  Result := 0;
  fs := TJvSearchFiles.Create(nil);
  try
    ft := DateTimeToFileTime(ATouchDate);
    if FileMask = '' then
      fs.FileParams.FileMask := '*.*'
    else
      fs.FileParams.FileMask := Filemask;
    fs.RootDirectory := ExtractFilePath(ExpandUNCFilename(Filemask));
    if Recurse then
      fs.DirOption := doIncludeSubDirs
    else
      fs.DirOption := doExcludeSubDirs;
    fs.Search;
    for i := 0 to fs.Files.Count - 1 do
    begin
      tmpAttr := GetFileAttributes(PChar(fs.Files[i]));
      if ForceTouch then
        SetFileAttributes(PChar(fs.Files[i]), FILE_ATTRIBUTE_NORMAL);
      if DoChangeTimes(fs.Files[i], ft, ATypes) then
      begin
        Inc(Result);
        if not SetFileAttributes(PChar(fs.Files[i]), (tmpAttr or IncAttr) and not ExcAttr) then
          ShowError('Could not change attributes on %s!', [fs.Files[i]]);
      end
      else
      begin
        if ForceTouch then
          SetFileAttributes(PChar(fs.Files[i]), tmpAttr);
        ShowError('Could not touch %s!', [fs.Files[i]]);
      end;
    end;
  finally
    fs.Free;
  end;
end;

// commandline:
// jtouch [options] <filemask>
// where options can be:
// /D:YYYY-MM-DD  set the date (default today)
// /T:HH:MM:SS    set the time (default now)
// /A:[-|+]ARSH   add/remove attributes (archive,read-only,system,hidden), (no default)
// /M:CHL         modify Create, cHange and/or Last access date (can be combined), (default cHange)
// /S             recurse into subdirs, (default false)
// /F             force touch of read-only files (temporarily changes attributes to FILE_ATTRIBUTE_NORMAL)

procedure ShowHelp;
begin
  writeln('');
  writeln('');
  writeln('jtouch: Changes file dates and times and / or attributes');
  writeln('');
  writeln('Usage:');
  writeln(ExtractFilename(ParamStr(0)), ' [options] <filemask>');
  writeln('');
  writeln('options:');
  writeln(#9'/D:YYYY-MM-DD'#9'set the date (default today)');
  writeln(#9'/T:HH:MM:SS'#9'set the time (default now)');
  writeln(#9'/A:[-|+]ARSH'#9'add/remove attributes (archive,read-only,system,hidden), (no default)');
  writeln(#9'/M:CHL'#9' modify Create, cHange and/or Last access date (can be combined), (default cHange)');
  writeln(#9'/S'#9'recurse into subdirs, (default false)');
  writeln(#9'/F'#9'force touching even for read-only files, (default false)');
  writeln('');
  writeln(#9'<filemask> - the files to find');
end;

{$IFNDEF COMPILER6_UP}
function StrToDateDef(const S:string;Default:TdateTime):TDateTime;
begin
  try
    Result := StrToDate(S);
  except
    Result := Default;
  end;
end;

function StrToTimeDef(const S:string;Default:TdateTime):TDateTime;
begin
  try
    Result := StrToTime(S);
  except
    Result := Default;
  end;
end;
{$ENDIF}

procedure Run;
var
  tmp: string;
  ATouchDate: TDateTime;
  IncludeAttributes, ExcludeAttributes: Cardinal;
  ACount: integer;
  TouchTypes: TTouchTypes;
  Recurse, ForceTouch: boolean;
begin
  try
    if (ParamCount < 1) or GetCmdSwitchValue('?', ['-', '/'], tmp, true) or
      GetCmdSwitchValue('h', ['-', '/'], tmp, true) then
    begin
      ShowHelp;
      Exit;
    end;
    ATouchDate := Now;
    if GetCmdSwitchValue('D:', ['-', '/'], tmp, true) then
      ATouchDate := StrToDateDef(tmp, ATouchDate) + frac(ATouchDate);
    if GetCmdSwitchValue('T:', ['-', '/'], tmp, true) then
      ATouchDate := StrToTimeDef(tmp, ATouchDate) + trunc(ATouchDate);
    IncludeAttributes := 0;
    ExcludeAttributes := 0;
    if GetCmdSwitchValue('A:', ['-', '/'], tmp, true) and (Length(tmp) > 0) then
    begin
      if tmp[1] = '-' then
      begin
        ExcludeAttributes := StrToAttrs(tmp);
        writeln('Exclude:', tmp);
      end
      else
      begin
        IncludeAttributes := StrToAttrs(tmp);
        writeln('Include:', tmp);
      end;
    end;
    TouchTypes := [mtChanged];
    if GetCmdSwitchValue('M:', ['-', '/'], tmp, true) then
      TouchTypes := StrToType(tmp);
    if TouchTypes = [] then
      TouchTypes := [mtChanged];
    Recurse := GetCmdSwitchValue('S', ['-', '/'], tmp, true);
    ForceTouch := GetCmdSwitchValue('F', ['-', '/'], tmp, true);
    // now get the filemask, this should be the last param:
    tmp := ParamStr(ParamCount);
    aCount := DoTouch(tmp, ATouchDate, TouchTypes, IncludeAttributes, ExcludeAttributes, Recurse, ForceTouch);
    writeln('Done: touched ', aCount, ' files');
  except
    on E: Exception do
      ShowError(E.Message, []);
  end;
end;

end.

