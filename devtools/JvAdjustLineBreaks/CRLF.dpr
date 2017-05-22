program crlf;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  crlfutils in 'crlfutils.pas';

begin
  try
    Run;
  except
    on E:Exception do
      writeln('ERROR: ', E.Message);
  end;
end.