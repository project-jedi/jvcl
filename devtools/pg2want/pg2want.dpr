program pg2want;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  p2wantUtils in 'p2wantUtils.pas';

begin
  try
    Run;
  except
    on E:Exception do
      writeln('ERROR: ', E.Message);
  end;
end.
 