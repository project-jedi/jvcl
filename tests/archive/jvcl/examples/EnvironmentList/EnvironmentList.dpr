program EnvironmentList;

{$APPTYPE CONSOLE}

uses
  Classes, JclSysInfo;

var
  List: TStringList;
  I: Integer;

begin
  List := TStringList.Create;
  try
    GetEnvironmentVars(List, False);
    for I := 0 to List.Count - 1 do
      Writeln(List[I]);
    Readln;
  finally
    List.Free;
  end;
end.
