program GenGroups;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Classes,
  Main_GenGroups in 'Main_GenGroups.pas';

begin
  try
    ShowProgInfo;
    GenerateGroupFiles;
  except
    WriteLn('Exception');
    WriteLn(ExceptObject.ClassName, ': ' , Exception(ExceptObject).Message);
  end;
  WaitAndExit;
end.
