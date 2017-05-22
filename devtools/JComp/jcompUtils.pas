unit jcompUtils;

interface
uses
  SysUtils, Classes, JTools;

procedure Run;

implementation

procedure Help;
begin
  writeln('jcomp - creates a list of all registered components');
  writeln('');
  writeln('Usage:');
  writeln('jcomp <filemask>');
  writeln('where');
  writeln(' filemask - the files to scan');
end;

function RemoveChars(const S:String; Chars:TSysCharSet):String;
var I, J:Integer;
begin
  Result := S;
  I := 1;
  J := 0;
  while I <= Length(S) do
  begin
    if not (S[i] in Chars) then
    begin
      Inc(J);
      Result[j] := S[i];
    end;
    Inc(I);
  end;
  SetLength(Result, J);
end;

procedure ParseComponents(const S: string);
var
  I, J, K: Integer;
  T: string;
begin
  I := 1;
  J := Length(S);
  while I <= J do
  begin
    while S[i] in [#0..#32, '(', '[', ',', ']', ')', ';'] do
    begin
      Inc(I);
      if I >= J then Exit;
    end;
    K := I;
    while not (S[I] in [#0..#32, ')', ']', ',']) do
    begin
      if S[i] = '{' then
      begin
        K := I;
        while S[i] <> '}' do
        begin
          Inc(I);
          if I >= J then Exit;
        end;
        Inc(I);
        T := trim(Copy(S, K, I - K));
        if T <> '' then
          writeln(#9, T);
        K := I;
      end
      else if (S[i] = '/') then
      begin
        K := I;
        while (I <= J) and (S[i] <> #10) do
          Inc(I);
        Inc(I);
        T := trim(Copy(S, K, I - K));
        if T <> '' then
          writeln(#9, T);
        K := I;
      end;
      Inc(I);
      if I >= J then Exit;
    end;
    T := RemoveChars(Copy(S, K, I - K), [#0..#32,'[',']','(',')',',',';',')']);
    if T <> '' then
      writeln(#9, T);
  end;
end;

procedure ParseFile(const Filename: string);
var
  I, J: Integer;
  S: string;
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    M.LoadFromFile(Filename);
    writeln(ExtractFileName(Filename), ':');
    SetLength(S, M.Size);
    if Length(S) > 0 then
    begin
      M.Seek(0, soFromBeginning);
      M.Read(S[1], M.Size);
      I := Pos('RegisterComponents', S);
      while I > 0 do
      begin
        while S[i] <> ',' do
        begin
          Inc(I); // remove palette tab
          if I >= Length(S) then Exit;
        end;
        S := Copy(S, I + 1, MaxInt); // remove RegisterComponents and palette tab name
        J := Pos(';', S);
        if J > 0 then
          ParseComponents(Copy(S, 1, J))
        else
          Exit;
        S := Copy(S, J + 1, MaxInt); // remove the part we just parsed
        I := Pos('RegisterComponents', S); // try again
      end;
    end;
  finally
    M.Free;
  end;
end;

procedure GetFiles(Filemask: string; Strings: TStrings);
var
  APath: string;
  F: TSearchRec;
begin
  APath := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandUNCFilename(Filemask)));
  Filemask := APath + ExtractFileName(Filemask);
  if FindFirst(Filemask, faAnyFile, F) = 0 then
  begin
    repeat
      Strings.Add(APath + F.Name);
    until FindNext(F) <> 0;
    FindClose(F);
  end;
end;

procedure Run;
var
  Files: TStringlist;
  Dummy: string;
  i: integer;
begin
  if (ParamCount < 1) or GetCmdSwitchValue('h', ['-', '/'], Dummy, true) or GetCmdSwitchValue('?', ['-', '/'], Dummy, false) then
  begin
    Help;
    Exit;
  end;
  Files := TStringlist.Create;
  try
    GetFiles(ParamStr(1), Files);
    for i := 0 to Files.Count - 1 do
      ParseFile(Files[i]);
  finally
    Files.Free;
  end;
//  writeln('Done. Press ENTER to exit.');
//  readln;
end;
end.

