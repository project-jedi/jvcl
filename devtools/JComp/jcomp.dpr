program jcomp;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  jcompUtils in 'jcompUtils.pas',
  JTools in '..\Common\JTools.pas';

begin
  try
    Run;
  except
    on E:Exception do
    writeln('ERROR: ', E.Message);
  end;
end.
