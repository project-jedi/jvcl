program pg;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  CmdLineUtils in 'CmdLineUtils.pas',
  GenerateUtils in 'GenerateUtils.pas',
  FileUtils in 'FileUtils.pas',
  PackageModels in '..\Common\PackageModels.pas',
  PackageInformation in '..\Common\PackageInformation.pas',
  ConditionParser in 'ConditionParser.pas';

begin
  CmdLineRun;
end.
