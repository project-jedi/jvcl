unit SetPoHeaderUtils;

interface

procedure Run;

implementation

uses SysUtils, Classes, JclStrings;

const
  TitleConst : string = '# SOME DESCRIPTIVE TITLE.';
  CopyrightConst : string = '# Copyright (C)';
  AuthorConst : string = 'FIRST AUTHOR';
  EmailConst : string = 'EMAIL@ADDRESS';
  DefaultLastTranslatorConst : string = '"Last-Translator: Somebody <your.email@address.com>\n"';
  NoneLastTranslatorConst : string = '"Last-Translator: none <none@none.com>\n"';

procedure ApplyHeader(title, copyright, package, version, author, email, poFile : string);
var
  Lines : TStringList;
  I : Integer;
  year : string;
  curLine : string;
begin
  year := IntToStr(CurrentYear);
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(poFile);
    I := 0;
    while (I < Lines.Count) and (Copy(Lines[i], 1, 2) <> '#.') do
    begin
      if Copy(Lines[I], 1, Length(TitleConst)) = TitleConst then
        Lines[I] := '# ' + title
      else if Copy(Lines[I], 1, Length(CopyrightConst)) = CopyrightConst then
        Lines[I] := CopyrightConst + ' ' + year + ' ' + copyright
      else if Copy(Lines[I], 1, Length(DefaultLastTranslatorConst)) = DefaultLastTranslatorConst then
        Lines[I] := NoneLastTranslatorConst
      else
      begin
        curLine := Lines[I];
        StrReplace(curLine, AuthorConst, author);
        StrReplace(curLine, EmailConst, email);
        StrReplace(curLine, 'PACKAGE', package);
        StrReplace(curLine, 'VERSION', version);
        StrReplace(curLine, 'YEAR', year);
        Lines[I] := curLine;
      end;

      // go to next line
      Inc(I);
    end;
    Lines.SaveToFile(poFile);
  finally
    Lines.Free;
  end;
end;

procedure Help;
begin
  WriteLn('SetPoHeader - Sets the headers in a PO File');
  WriteLn;
  WriteLn('   SetPoHeader -t TITLE -c COPYRIGHT -p PACKAGE');
  WriteLn('               -v VERSION -a AUTHOR -e EMAIL');
  WriteLn('               poFile.po');
  WriteLn;
  WriteLn(#9'poFile.po'#9'The po file to modify');
  WriteLn(#9'-h'#9#9'prints this help message');
  WriteLn(#9'-t TITLE'#9'The title of the file');
  WriteLn(#9'-c COPYRIGHT'#9'The copyright notice');
  WriteLn(#9'-p PACKAGE'#9'The package name');
  WriteLn(#9'-v VERSION'#9'The version of the package');
  WriteLn(#9'-a AUTHOR'#9'The author''s name');
  WriteLn(#9'-e EMAIL'#9'The author''s email');
end;

procedure Run;
var
  I : Integer;
  curParam : string;
  title : string;
  copyright : string;
  package : string;
  version : string;
  author : string;
  email : string;
  poFile : string;
begin
  // Analyse command line parameters to get informations
  // to use when modifying the headers
  I := 1;
  while I <= ParamCount do
  begin
    curParam := ParamStr(I);
    if AnsiSameText(curParam, '-h') then
    begin
      Help;
      Exit;
    end
    else if AnsiSameText(curParam, '-t') then
    begin
      title := ParamStr(I+1);
      Inc(I);
    end
    else if AnsiSameText(curParam, '-c') then
    begin
      copyright := ParamStr(I+1);
      Inc(I);
    end
    else if AnsiSameText(curParam, '-p') then
    begin
      package := ParamStr(I+1);
      Inc(I);
    end
    else if AnsiSameText(curParam, '-v') then
    begin
      version := ParamStr(I+1);
      Inc(I);
    end
    else if AnsiSameText(curParam, '-a') then
    begin
      author := ParamStr(I+1);
      Inc(I);
    end
    else if AnsiSameText(curParam, '-e') then
    begin
      email := ParamStr(I+1);
      Inc(I);
    end
    else
      poFile := ParamStr(I);

    Inc(I);
  end;

  // check that we have everything
  if (title = '') or
     (copyright = '') or
     (package = '') or
     (version = '') or
     (author = '') or
     (email = '') or
     (poFile = '') then
  begin
    WriteLn('Error: You MUST specify all options');
    WriteLn;
    Help;
    Exit;
  end;

  // Now, work on the file
  if not FileExists(poFile) then
  begin
    WriteLn('Error: '+poFile+' does not exists');
    Exit;
  end
  else
    ApplyHeader(title, copyright, package, version, author, email, poFile);
end;

end.
