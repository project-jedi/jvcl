unit scUtils;

interface

procedure Run;

implementation
uses
  JvSurveyIntf, SysUtils;

function ConvertSurvey(const Filename:string;InPlace,ToBinary:boolean):integer;
const
  cFormat:array[boolean] of TJvSurveyFileFormat = (ffText,ffBinary);
  cExtension:array[boolean] of PChar = ('.xml','.bin');
var
  FSurvey:IJvSurvey;
begin
  Result := 0;
  if FileExists(Filename) then
  begin
    FSurvey := CreateSurvey;
    FSurvey.LoadFromFile(Filename);
    if InPlace then
      FSurvey.SaveToFile(Filename,cFormat[ToBinary])
    else
      FSurvey.SaveToFile(ChangeFileExt(Filename,cExtension[ToBinary]),cFormat[ToBinary]);
    Result := 1;
  end;
end;

function ConvertSurveys(const FileMask:string;InPlace,ToBinary:boolean):integer;
var F:TSearchRec;
begin
  Result := 0;
  if FindFirst(Filemask,faAnyFile,F) = 0 then
  try
    repeat
      if (F.Attr and faDirectory = 0) then
        Inc(Result,ConvertSurvey(ExtractFilePath(Filemask) + F.Name,InPlace,ToBinary));
    until FindNext(F) <> 0;
  finally
    FindClose(F);
  end;
end;

procedure ShowHelp;
begin
  writeln('SurveyConvert: converts survey files between binary and text format');
  writeln('');
  writeln('Usage:');
  writeln('');
  writeln('sc.exe [-i] [-b] [-t] <filemask>');
  writeln('');
  writeln('where');
  writeln('-i   convert inplace (output overwrites input)');
  writeln('-b   convert to binary');
  writeln('-t   convert to text (default)');
  writeln('<filemask>  files to convert - can cntain wildcards (* and ?)');
  writeln('');
end;
procedure Run;
var InPlace,ToBinary:boolean;i:integer;
begin
  InPlace := FindCmdLineSwitch('i',['-','/'],true);
  ToBinary := FindCmdLineSwitch('b',['-','/'],true);
  if (ParamCount = 0) or FindCmdLineSwitch('?',['-','/'],true) then
  begin
    ShowHelp;
    Exit;
  end;

  for i := 1 to ParamCount do
    if not (ParamStr(i)[1] in ['-','/']) then
      ConvertSurveys(ExpandUNCFilename(ParamStr(i)),InPlace,ToBinary);
end;

end.


