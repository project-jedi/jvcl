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
  WriteLn('   pg [-m=MODEL] [-x=CONFIGFILE] [-t=TARGETS]');
  WriteLn('      [-p=PATH] [-r=PREFIX] [-f=FORMAT] [-i=INCLUDEFILE] [-d]');
  WriteLn;
  WriteLn(#9'-h'#9#9'prints this help message');
  WriteLn(#9'-m=MODEL'#9'The name of the model to use');
  WriteLn(#9#9#9'  Defaults to "JVCL"');
  WriteLn(#9'-x=CONFIGFILE'#9'Location of the xml configuration file');
  WriteLn(#9#9#9'  Defaults to "pgEdit.xml"');
  WriteLn(#9'-t=TARGETS'#9'comma separated list of targets');
  WriteLn(#9#9#9'  Defaults to "all"');
  WriteLn(#9'-p=PATH'#9#9'the path to packages');
  WriteLn(#9#9#9'  Defaults to the value from the model');
  WriteLn(#9'-r=PREFIX'#9'Prefix to use for package name generation');
  WriteLn(#9#9#9'  Defaults to the value from the model');
  WriteLn(#9'-f=FORMAT'#9'Format of generated package name');
  WriteLn(#9#9#9'  Defaults to the value from the model');
  WriteLn(#9'-i=INCLUDEFILE'#9'Location of the include file');
  WriteLn(#9#9#9'  Defaults to the value from the model');
  WriteLn(#9'-d'#9#9'Generates the DOFs files where applicable');
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
  xmlconfig : string;
  modelName : string;
  incfile : string;
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
      else if AnsiSameText(Copy(curParam, 2, 1), 'x') then
      begin
        xmlconfig := Copy(curParam, 4, Length(ParamStr(i)));
      end
      else if AnsiSameText(Copy(curParam, 2, 1), 'i') then
      begin
        incfile := Copy(curParam, 4, Length(ParamStr(i)));
      end
      else if AnsiSameText(Copy(curParam, 2, 1), 'm') then
      begin
        modelname := Copy(curParam, 4, Length(ParamStr(i)));
      end
      else if AnsiSameText(Copy(curParam, 2, 1), 'h') then
      begin
        Help;
        Exit;
      end;
    end;


    if targetList = '' then
      targetList := 'all';

    if xmlconfig = '' then
      xmlconfig := 'pgEdit.xml';

    if modelName = '' then
      modelName := 'JVCL';

{
    if packagesPath = '' then
      packagesPath := '..'+PathSeparator+'..'+PathSeparator+'packages';

    if prefix = '' then
      prefix := 'Jv';

    if format = '' then
      format := '%p%n%e%v%t';

    if incfile = '' then
      incfile := '..'+PathSeparator+'..'+PathSeparator+'common'+PathSeparator+'JVCL.INC';}

    LoadConfig(xmlconfig, modelName);

   // EnumeratePackages() needs this 
    if packagesPath = '' then
      packagesPath := PackagesLocation;
      
    if PathIsAbsolute(packagesPath) then
      packagesPath := packagesPath
    else
      packagesPath := PathNoInsideRelative(StrEnsureSuffix(PathSeparator, StartupDir) + packagesPath);


    StrToStrings(targetList, ',', targets, False);
    ExpandTargetsNoPerso(targets);

    packages := TStringList.Create;
    try
      EnumeratePackages(packagesPath, packages);
      Generate(packages,
               targets,
               WriteMsg,
               XmlConfig,
               ModelName,
               FindCmdLineSwitch('d', ['-', '/'], True),
               packagesPath,
               prefix,
               Format,
               incfile
              );
    finally
      packages.Free;
    end;
  finally
    targets.Free;
  end;
end;

end.
