program sc;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  scUtils in 'scUtils.pas',
  JvSurveyImpl in '..\common\JvSurveyImpl.pas';

begin
  try
    Run;
  except
    on E:Exception do
      writeln('ERROR: ',E.MEssage);
  end;
end.
