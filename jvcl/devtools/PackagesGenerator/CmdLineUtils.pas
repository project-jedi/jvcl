unit CmdLineUtils;

interface

procedure CmdLineRun;
procedure Help;

implementation

uses
  Classes, SysUtils, GenerateUtils,
  {$IFDEF NO_JCL}
  UtilsJcl,
  {$ELSE}
  JclStrings, JclFileUtils,
  {$ENDIF NO_JCL}
  FileUtils;

procedure Help;
begin
  WriteLn('pg - Jedi Package generator');
  WriteLn;
  WriteLn('   pg [-m=MODEL] [-x=CONFIGFILE] [-t=TARGETS]');
  WriteLn('      [-p=PATH] [-r=PREFIX] [-f=FORMAT] [-i=INCLUDEFILE] [-d]');
  WriteLn;
  WriteLn('     -h              Prints this help message');
  WriteLn('     -m=MODEL        The name of the model to use');
  WriteLn('                       Defaults to "JVCL"');
  WriteLn('     -x=CONFIGFILE   Location of the xml configuration file');
  WriteLn('                       Defaults to "pgEdit.xml"');
  WriteLn('     -t=TARGETS      Comma separated list of targets');
  WriteLn('                       Defaults to "all"');
  WriteLn('     -p=PATH         The path to packages');
  WriteLn('                       Defaults to the value from the model');
  WriteLn('     -r=PREFIX       Prefix to use for package name generation');
  WriteLn('                       Defaults to the value from the model');
  WriteLn('     -f=FORMAT       Format of generated package name');
  WriteLn('                       Defaults to the value from the model');
  WriteLn('     -i=INCLUDEFILE  Location of the include file');
  WriteLn('                       Defaults to the value from the model');
end;

procedure Error(const Msg : string);
begin
  WriteLn('Error !!!');
  WriteLn(Msg);
  Writeln;
  Help;
end;

procedure WriteMsg(const Msg : string);
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
  ErrMsg : string;
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
    begin
      if FileExists('pg.xml') then
        xmlconfig := 'pg.xml'
      else
        xmlconfig := 'pgEdit.xml';
    end;

    if modelName = '' then
      modelName := 'JVCL';

    if not LoadConfig(xmlconfig, modelName, ErrMsg) then
    begin
      WriteLn(ErrMsg);
      Exit;
    end;

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
      if not Generate(packages,
                      targets,
                      WriteMsg,
                      XmlConfig,
                      ModelName,
                      ErrMsg,
                      packagesPath,
                      prefix,
                      Format,
                      incfile
                     ) then
      begin
        WriteLn(ErrMsg);
        Exit;
      end;
    finally
      packages.Free;
    end;
  finally
    targets.Free;
  end;
end;

end.
