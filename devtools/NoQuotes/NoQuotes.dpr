program NoQuotes;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure usage;
begin
  writeln('NoQuotes EnvVar Value');
end;

var
  F : textfile;
  EnvVar : string;
  Value : string;

begin
  if ParamCount <> 2 then
    usage
  else
  begin
    EnvVar := ParamStr(1);
    Value := ParamStr(2);
    if Value[1] = '"' then
      Value := Copy(Value, 2, Length(Value));

    if Value[Length(Value)] = '"' then
      Value := Copy(Value, 1, Length(Value)-1);

    AssignFile(F, 'NoQuotesBatch.bat');
    Rewrite(F);
    Write(F, 'SET ');
    Write(F, EnvVar);
    Write(F, '=');
    Write(F, Value);
    CloseFile(F);
  end;
end.
 