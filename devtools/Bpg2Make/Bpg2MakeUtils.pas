{$I JVCL.INC}

unit Bpg2MakeUtils;

interface

procedure Run;

implementation

uses
  Windows, Classes, SysUtils,
  JvConsts; // (rom) for sLineBreak, PathDelim  no dependencies to packages

const
  DefaultMakeFile =
    '#--------------------------------------------------------------------------------------------------#' + sLineBreak +
    '#                                                                                                  #' + sLineBreak +
    '# devtools                                                                                         #' + sLineBreak +
    '#                                                                                                  #' + sLineBreak +
    '#--------------------------------------------------------------------------------------------------#' + sLineBreak +
    '' + sLineBreak +
    '!ifndef ROOT' + sLineBreak +
    'ROOT = $(MAKEDIR)\..' + sLineBreak +
    '!endif' + sLineBreak +
    '#---------------------------------------------------------------------------------------------------' + sLineBreak +
{    'SRC = ..\Run' + sLineBreak +
    'ARCH = ..\Archive' + sLineBreak +
    'COM = ..\Common' + sLineBreak +
    'BIN = ..\Bin' + sLineBreak +
    'DCU = ..\Dcu' + sLineBreak +
    'JCL = ..\..\..\JCL\source' + sLineBreak +
    'DRC = $&.drc' + sLineBreak +
    'SRCP = $(SRC);$(COM);$(JCL);$(ARCH);$(DCU)' + sLineBreak +
    'SRCH = ..\$(SRC);..\$(COM);..\$(JCL);..\$(ARCH);..\$(DCU)' + sLineBreak +}
    '#---------------------------------------------------------------------------------------------------' + sLineBreak +
    'MAKE = $(ROOT)\bin\make.exe -$(MAKEFLAGS) -f$**' + sLineBreak +
    'DCC  = $(ROOT)\bin\dcc32.exe -U"$(DCPDIR)" -LE"$(DCPDIR)" -LN"$(DCPDIR)" -Q -W -H -M $&.dpk' + sLineBreak +
//    'DCC  = $(ROOT)\dcc32.exe -e$(BIN) -i$(SRCP) -n$(DCU) -r$(SRCP) -u$(SRCP) -q -w -m' + sLineBreak +
    'BRCC = $(ROOT)\bin\brcc32.exe $**' + sLineBreak +
    '#---------------------------------------------------------------------------------------------------' + sLineBreak;

//--------------------------------------------------------------------------------------------------

function StrEqualText(Text: PChar; SearchText: PChar; MaxLen: Integer;
  IgnoreCase: Boolean): Boolean;
var
  i: Integer;
begin
  if IgnoreCase then
    Result := StrLIComp(Text, SearchText, MaxLen) = 0
  else
  begin 
    Result := False;
    for i := 0 to MaxLen - 1 do
      if (Text[i] = #0) or {(SearchText[i] = #0) or}
         (Text[i] <> SearchText[i]) then Exit;
    Result := True;
  end;
end;


function FastStringReplace(const Text, SearchText, ReplaceText: string;
  ReplaceAll, IgnoreCase: Boolean): string;
var
  LenSearchText, LenReplaceText, LenText: Integer;
  Index, Len, StartIndex: Integer;
begin
  LenSearchText := Length(SearchText);
  LenReplaceText := Length(ReplaceText);
  LenText := Length(Text);
  if LenSearchText = 0 then
  begin
    Result := Text;
    Exit;
  end;

  if ReplaceAll then
  begin
    if LenReplaceText - LenSearchText > 0 then
      SetLength(Result, LenText +
        (LenReplaceText - LenSearchText) * (LenText div LenSearchText))
    else
      SetLength(Result, LenText);
  end
  else
    SetLength(Result, LenText + (LenReplaceText - LenSearchText));


  Len := 0;
  StartIndex := 1;
  for Index := 1 to LenText do
  begin
    if StrEqualText(PChar(Pointer(Text)) + Index - 1, Pointer(SearchText),
                   LenSearchText, IgnoreCase) then
    begin
      if Index > StartIndex then
      begin 
        Move(Text[StartIndex], Result[Len + 1], Index - StartIndex); 
        Inc(Len, Index - StartIndex); 
      end; 
      StartIndex := Index + LenSearchText; 

     // Ersetzungstext einfügen 
      if LenReplaceText > 0 then 
      begin 
        Move(ReplaceText[1], Result[Len + 1], LenReplaceText); 
        Inc(Len, LenReplaceText); 
      end; 

      if not ReplaceAll then Break; 
    end; 
  end; 

  Index := LenText + 1;
  if Index > StartIndex then
  begin
    Move(Text[StartIndex], Result[Len + 1], Index - StartIndex);
    Inc(Len, Index - StartIndex);
  end;

  SetLength(Result, Len);
end;

procedure ShowHelp;
begin
  writeln('');
  writeln('');
  writeln('Bpg2Make: Creates a MAKEFILE from a BPG file');
  writeln('');
  writeln('Usage:');
  writeln(ExtractFilename(ParamStr(0)),' <bpgfile>');
  writeln('');
  writeln(#9'<bpgfile> - the Borland Package Group file');
end;

function GetReturnPath(const Dir: string): string;
var
  i: Integer;
begin
  Result := '';
  if Dir <> '' then
  begin
    Result := '..';
    for i := 1 to Length(Dir) do
      if Dir[i] = PathDelim then
        Result := Result + '\..';
  end;
end;

function CreateMakeFile(const Filename: string): Boolean;
var
  i, ps: Integer;
  List, MkLines, Targets, Commands: TStringlist;
  S, SourceFile, Dir: string;
  ProjectCommands: string;
begin
  Result := False;
  if not FileExists(Filename) then
  begin
    WriteLn('ERROR: ', Filename, ' not found!');
    Exit;
  end;

  List := TStringList.Create;
  MkLines := TStringList.Create;
  Targets := TStringList.Create;
  Commands := TStringList.Create;
  try
    List.LoadFromFile(Filename);

    MkLines.Text := DefaultMakeFile;

    i := 0;
    while i < List.Count do
    begin
      S := List[i];
      ps := Pos('bpl: ', S);
      if ps <> 0 then
      begin
        Targets.Add(Trim(Copy(S, 1, ps + 2)) + '=' + Trim(Copy(S, ps + 5, MaxInt)));
        ProjectCommands := '';
        Inc(i);
        while (i < List.Count) and (List[i] <> '') do
        begin
          ProjectCommands := ProjectCommands + #9 + Trim(List[i]) + #13#10;
          Inc(i);
        end;
        SetLength(ProjectCommands, Length(ProjectCommands) - 2);
        Commands.Add(ProjectCommands);
      end;

      if StrLIComp('PROJECTS =', PChar(S), 10) = 0 then
      begin
        s := Trim(S);
        MkLines.Add(S);
        while (i < List.Count) and (S <> '') and (S[Length(S)] = '\') do
        begin
          Inc(i);
          S := Trim(List[i]);
          MkLines.Add(#9 + S);
        end;
      end;
      Inc(i);
    end;
    MkLines.Add('#---------------------------------------------------------------------------------------------------');
    MkLines.Add('default: $(PROJECTS)');
    MkLines.Add('#---------------------------------------------------------------------------------------------------');
    MkLines.Add('');
    for i := 0 to Targets.Count - 1 do
    begin
      S := Targets[i];
      ps := Pos('=', S);
      SourceFile := Copy(S, ps + 1, MaxInt);
      SetLength(S, ps - 1);
      Dir := ExtractFileDir(SourceFile);
      MkLines.Add(S + ': ' + SourceFile);
      SourceFile := ExtractFileName(SourceFile);
      if Dir <> '' then
        MkLines.Add(#9'@cd ' + Dir);

      ProjectCommands := Commands[i];
      ProjectCommands := FastStringReplace(ProjectCommands, '$**', SourceFile, True, False);
      ProjectCommands := FastStringReplace(ProjectCommands, '$*', Copy(SourceFile, 1, Length(SourceFile) - Length(ExtractFileExt(SourceFile))), True, False);
      MkLines.Add(ProjectCommands);
      if Dir <> '' then
        MkLines.Add(#9'@cd ' + GetReturnPath(Dir));
      MkLines.Add('');
    end;
    MkLines.SaveToFile(ChangeFileExt(FileName, '.mak'));
  finally
    Commands.Free;
    Targets.Free;
    MkLines.Free;
    List.Free;
  end;
  Result := True;
end;

procedure Run;
begin
  {
    command-line:
      <filename>
  }
  if (ParamCount <> 1) or (LowerCase(ExtractFileExt(ParamStr(1))) <> '.bpg') then
    ShowHelp
  else
  try
    if CreateMakeFile(ExpandUNCFileName(ParamStr(1))) then
      WriteLn(' Makefile created');
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end;

end.

