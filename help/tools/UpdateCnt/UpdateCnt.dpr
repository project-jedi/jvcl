program UpdateCnt;
{$APPTYPE CONSOLE}
uses
  SysUtils, Classes;

var
  I: Integer;
  S: string;

begin
  try
    WriteLn('UpdateCnt: Update contents file. Copyright (c) 2002 Project JEDI.');
    with TStringList.Create do
    try
      WriteLn('Loading file...');
      LoadFromFile(ExtractFilePath(ParamStr(0)) + '..\output\WinHELP\JVCL.cnt');
      WriteLn(Format('  done. %d lines', [Count]));
      S := Strings[2];
      if AnsiSameText(Copy(S, Length(S) - 3, 4), 'id_0') then
      begin
        Delete(2);
        WriteLn('Removed Welcome page.');
        WriteLn('Moving Symbol Reference...');
        I := Count - 1;
        while (I > 0) do
        begin
          S := Strings[I];
          S[1] := IntToStr(StrToInt(S[1]) + 1)[1];
          Strings[I] := S;
          if AnsiSameText(S, '2 Symbol Reference') then
            Break;
          Dec(I);
        end;
        WriteLn(Format('  done. %d nodes moved.', [Count - I]));
        WriteLn('Saving file...');
        SaveToFile(ExtractFilePath(ParamStr(0)) + '..\output\WinHELP\JVCL.cnt');
      end
      else
        WriteLn('Contents file already updated.');
    finally
      Free;
    end;
  except
    WriteLn('Exception');
    WriteLn(ExceptObject.ClassName, ': ' , Exception(ExceptObject).Message);
  end;
  WriteLn('Press Enter to quit.');
  ReadLn;
end.