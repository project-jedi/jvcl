unit stripUtils;

interface

procedure Run;

implementation
uses
  Classes, SysUtils;

procedure ShowHelp;
begin
  writeln('Strips comments from PO files (except the first)');
  writeln('Usage:');
  writeln(' stripCmtPO input.po output.po');
  writeln(' if output.po is not given, output is written to input.new.po');
end;

procedure Run;
var
  i, j: integer;
  S: TStringlist;
  AFilename, AOutput: string;
begin
  try
    if ParamCount < 1 then
    begin
      ShowHelp;
      Exit;
    end;
    AFilename := ExpandUNCFilename(ParamStr(1));
    if FileExists(AFilename) then
    begin
      S := TStringlist.Create;
      try
        S.LoadFromFile(AFilename);
        // first, skip past first comment
        j := 0;
        while (j < S.Count) and (Pos('#',S[j]) = 1) do
          Inc(j);
        // strip out the rest
        for i := S.Count - 1 downto j + 1 do
          if Pos('#', S[i]) = 1 then
            S.Delete(i);
        AOutput := ExpandUNCFilename(ParamStr(2));
        if AOutput <> '' then
          S.SaveToFile(AOutput)
        else
          S.SaveToFile(ChangeFileExt(AFilename,'.new.po'));
      finally
        S.Free;
      end;
    end;
  except
    on E: Exception do
      Writeln('ERROR: ', E.Message);
  end;
end;
end.

