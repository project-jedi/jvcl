unit genimage_main;

interface

procedure Execute;

var
  InPath: string;
  OutPath: string;

implementation

uses
  SysUtils, UpdBckgrnd;

procedure WriteOutHelp;
begin
  WriteLn('Usage: GenImages [/?]');
  WriteLn('');
  WriteLn('Parameters:');
  WriteLn('  /? - Show this help page');
  WriteLn('');
  WriteLn('Purpose:');
  WriteLn('  The application will transform all BMPs for the JEDI-VCL into BMPs and PNGs to ');
  WriteLn('  be used in the help file. Upon transformation the color of lower left pixel will');
  WriteLn('  be changed in the entire bitmap into a gray color.');
  WriteLn('');
  WriteLn('  All .BMP files in the jvcl/images folder will be transformed and written as both');
  WriteLn('  BMP as well as PNG into the dev/help/images/comp folder.');
end;

function UpPath(const Path: string; Level: Integer): string;
var
  I: Integer;
begin
  if Level = 0 then
    Result := Path
  else
  begin
    I := Length(Path);
    repeat
      Dec(I);
      while (I > 0) and (Path[I] <> '\') do
        Dec(I);
      Dec(Level);
    until (Level = 0) or (I <= 0);
    Result := Copy(Path, 1, I);
  end;
end;

procedure Init;
var
  ExePath: string;
begin
  WriteLn('GenImages: Generate images for help file');
  WriteLn('Copyright © 2002 Project JEDI.');
  WriteLn('Released under MPL license.');
  WriteLn('');
  ExePath := ExtractFilePath(ParamStr(0));
  InPath := UpPath(ExePath, 2) + 'JVCL3\images\';
  OutPath := UpPath(ExePath, 1) + 'images\comp\';
end;

procedure Done;
begin
  WriteLn('Press enter to quit.');
  ReadLn;
end;

procedure IterateFiles;
var
  I: Integer;
  Res: Integer;
  SR: TSearchRec;
begin
  WriteLn('Transforming files...');
  I := 0;
  Res := FindFirst(InPath + '*.bmp', faAnyFile - faDirectory, SR);
  try
    while Res = 0 do
    begin
      Inc(I);
      HandleFile(InPath + SR.FindData.cFileName);
      Res := FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
  WriteLn('Done. Transformed ', I, ' files.');
end;

procedure Execute;
begin
  Init;
  try
    if (ParamCount = 1) then
    begin
      if ParamStr(1) <> '/?' then
        WriteLn('Error: Invalid parameter: ' + ParamStr(1));
      WriteOutHelp;
    end
    else if ParamCount <> 0 then
    begin
      WriteLn('Error: invalid number of parameters.');
      WriteOutHelp;
    end
    else
    begin
      IterateFiles;
    end;
  except
    WriteLn('Exception');
    WriteLn(ExceptObject.ClassName, ': ' , Exception(ExceptObject).Message);
  end;
  Done;
end;

end.
