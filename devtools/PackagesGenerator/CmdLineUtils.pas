unit CmdLineUtils;

interface

procedure CmdLineRun;
procedure Help;

implementation

uses Classes, SysUtils, GenerateUtils, JclStrings, JclFileUtils, FileUtils;

procedure Help;
begin
  WriteLn('pg - Jedi Package generator');
  WriteLn;
  WriteLn(#9'-h'#9#9'prints this help message');
  WriteLn(#9'-d'#9#9'Generates the DOFs files where applicable');
  WriteLn(#9'-p=PATH'#9#9'the path to packages');
  WriteLn(#9'-t=TARGETS'#9'comma separated list of targets');
  WriteLn(#9'-r=PREFIX'#9'Prefix to use for package name generation (Jv)');
  WriteLn(#9'-f=FORMAT'#9'Format of generated package name (%p%n%e%v%t)');
end;

procedure Error(Msg : string);
begin
  WriteLn('Error !!!');
  WriteLn(Msg);
  Writeln;
  Help; 
end;

procedure WriteMsg(Msg : string);
begin
  WriteLn(Msg);
end;

procedure CmdLineRun;
var
  I : Integer;
  targetList : string;
  packagesPath : string;
  prefix : string;
  Format : string;
  curParam : string;
  targets : TStringList;
  packages : TStringList;
begin
  targets := TStringList.Create;
  try
    for i := 1 to ParamCount do
    begin
      curParam := ParamStr(i);
      if AnsiSameText(Copy(curParam, 2, 1), 't') then
      begin
        targetList := Copy(curParam, 4, Length(ParamStr(i)));
      end
      else if AnsiSameText(Copy(curParam, 2, 1), 'p') then
      begin
        packagesPath := Copy(curParam, 4, Length(ParamStr(i)));
      end
      else if AnsiSameText(Copy(curParam, 2, 1), 'r') then
      begin
        prefix := Copy(curParam, 4, Length(ParamStr(i)));
      end
      else if AnsiSameText(Copy(curParam, 2, 1), 'f') then
      begin
        format := Copy(curParam, 4, Length(ParamStr(i)));
      end
      else if AnsiSameText(Copy(curParam, 2, 1), 'h') then
      begin
        Help;
        Exit;
      end;
    end;

    if packagesPath = '' then
      Error('You must indicate the path to the packages')
    else if targetList = '' then
      Error('You must indicate a target list')
    else if prefix = '' then
      Error('You must indicate a prefix')
    else if format = '' then
      Error('You must indicate a format')
    else
    begin
      StrToStrings(targetList, ',', targets, False);
      ExpandTargets(targets);

      if PathIsAbsolute(packagesPath) then
        packagesPath := packagesPath
      else
        packagesPath := PathNoInsideRelative(StrEnsureSuffix('\', StartupDir) + packagesPath);

      packages := TStringList.Create;
      try
        EnumeratePackages(packagesPath, packages);
        Generate(packages,
                 targets,
                 packagesPath,
                 prefix,
                 Format,
                 WriteMsg,
                 FindCmdLineSwitch('d', ['-', '/'], True));
      finally
        packages.Free;
      end;
    end;
  finally
    targets.Free;
  end;
end;

end.
