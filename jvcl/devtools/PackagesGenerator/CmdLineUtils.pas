unit CmdLineUtils;

interface

procedure CmdLineRun;
procedure Help;

implementation

uses Classes, SysUtils, GenerateUtils, JclStrings;

procedure Help;
begin
  WriteLn('pg - JVCL Package generator');
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
    else
    begin
      if AnsiSameText(targetList, 'all') then
        EnumerateTargets(packagesPath, targets)
      else
        StrToStrings(targetList, ',', targets, False);

      packages := TStringList.Create;
      try
        EnumeratePackages(packagesPath, packages);
        Generate(packages, targets, packagesPath, WriteMsg);
      finally
        packages.Free;
      end;
    end;
  finally
    targets.Free;
  end;
end;

end.
