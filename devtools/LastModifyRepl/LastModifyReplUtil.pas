{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: LastModifyReplUtil.PAS, released on 2004-03-26

The Initial Developers of the Original Code are: Andreas Hausladen <ahuser att users dott sourceforge dott net>
All Rights Reserved.

Last Modified: 2004-03-26

Contributers:

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit LastModifyReplUtil;

interface

uses
  SysUtils, Classes, JFileSearch;

function Main: Integer;

implementation

uses StrUtils;

const
  LastModifiedLine = 'Last Modified: $' + 'Date$';
  IDLine = '// $' + 'Id$';

var
  StartDir: string = '';
  Filemask: string = '*.pas';
  LastModify: Integer = 0;
  ID: Integer = 1;

procedure ParseParams;
var
  i, Count: Integer;
  S: string;
begin
  Count := ParamCount;
  S := ParamStr(1);
  if (Count < 1) xor ((S <> '') and (S[1] = '-')) then
  begin
    WriteLn('Usage:');
    WriteLn('  LastModifyRepl.exe [Directory] [Filemask] [Options]');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  -LastModify=0  (default)   remove "Last modified" line');
    WriteLn('  -LastModify=1              insert "Last modified" line');
    WriteLn('  -LastModify=2              replace "Last modified" line');
    WriteLn('  -Id=0                      remove "// $', 'Id$" line');
    WriteLn('  -Id=1          (default)   insert "// $', 'Id$" line');
    Halt(1);
  end;
  i := 1;
  Count := ParamCount;
  while i <= Count do
  begin
    S := ParamStr(i);
    if S[1] = '-' then
    begin
      if AnsiStartsText('-LastModify=', S) then
        LastModify := StrToInt(Copy(S, 13, 1))
      else
      if AnsiStartsText('-Id=', S) then
        ID := StrToInt(Copy(S, 5, 1));
    end
    else
    begin
      if StartDir = '' then
        StartDir := S
      else
        Filemask := S;
    end;
    Inc(i);
  end;
end;

procedure ProcessFile(const Filename: string);
var
  Lines: TStrings;
  i: Integer;
  LastModifyInsertLineNum, IDInsertLineNum: Integer;
  LastModifyInserted, IDInserted: Boolean;
  Modified: Boolean;
  S, TS: string;
begin
  LastModifyInserted := False;
  IDInserted := False;
  Modified := False;

  IDInsertLineNum := -1;
  LastModifyInsertLineNum := -1;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(StartDir + '\' + Filename);
    i := 0;
    while i < Lines.Count - 1 do
    begin
      S := Lines[i];
      TS := Trim(S);
      if AnsiStartsText('Last Modified', TS) then
      begin
        case LastModify of
          0:
            begin
              Lines.Delete(i);
              if (Trim(Lines[i]) = '') and (Trim(Lines[i - 1]) = '') then
                Lines.Delete(i);
              Modified := True;
              Continue;
            end;
          1, 3:
            begin
              if (S <> LastModifiedLine) and (not AnsiStartsText('// $' + 'Id:', S)) then
              begin
                Lines[i] := LastModifiedLine;
                Modified := True;
              end;
              LastModifyInserted := True;
            end;
        end;
      end
      else if AnsiStartsText('// $' + 'Id$', TS) or (AnsiStartsText('// $' + 'Id:', TS)) then
      begin
        case ID of
          0:
            begin
              Lines.Delete(i);
              Modified := True;
              Continue;
            end;
          1:
            begin
              IDInserted := True;
            end;
        end;
      end
      else if AnsiStartsText('You may retrieve', TS) then
        LastModifyInsertLineNum := i - 1
      else if AnsiEndsStr('----------}', TS) then
        IDInsertLineNum := i + 1
      ;

      if (IDInsertLineNum <> -1) and (i > IDInsertLineNum + 10) then
        Break;

      Inc(i);
    end;

   // first the "// S|Id|S" line
    if (ID = 1) and not IDInserted and (IDInsertLineNum <> -1) then
    begin
      Lines.Insert(IDInsertLineNum, IDLine);
      Modified := True;
    end;

   // second the "Last modified" (line numbers!!)
    if (LastModify = 1) and not LastModifyInserted and (LastModifyInsertLineNum <> -1) then
    begin
      Lines.Insert(LastModifyInsertLineNum, LastModifiedLine);
      Lines.Insert(LastModifyInsertLineNum + 1, '');
      Modified := True;
    end;

    if Modified then
      Lines.SaveToFile(StartDir + '\' + Filename);
  finally
    Lines.Free;
  end;
end;

procedure Progress(Position, Max: Integer; const Filename: string);
const
  ProgressLine = '                                   ';
var
  Line: string;
  Percentage, Count: Integer;
begin
  if Max = 0 then
    Exit;
  Line := ProgressLine;
  Count := Position * Length(ProgressLine) div Max;
  Percentage := Position * 100 div Max;
  if Count > 0 then
    FillChar(Line[1], Count, '#');
  Write('[' + Line + '] ', Percentage:3, '% ', Filename: (80-44), ^M);
end;

function Main: Integer;
var
  Search: TJvSearchFiles;
  i: Integer;
begin
  Result := 1; // exit code

  ParseParams;
  if not DirectoryExists(StartDir) then
  begin
    WriteLn('Directory "', StartDir, '" does not exist.');
    Exit;
  end;

  Search := TJvSearchFiles.Create(nil);
  try
    Search.RootDirectory := StartDir;
    Search.FileParams.FileMask := Filemask;
    Search.FileParams.SearchTypes := [stFileMask];
    Search.DirOption := doIncludeSubDirs;
    Search.Search;

    WriteLn;
    Progress(0, Search.Files.Count, '');
    for i := 0 to Search.Files.Count - 1 do
    begin
      Progress(i, Search.Files.Count, Search.Files[i]);
      ProcessFile(Search.Files[i]);
      Progress(i + 1, Search.Files.Count, Search.Files[i]);
    end;
    Progress(Search.Files.Count, Search.Files.Count, '');
  finally
    WriteLn;
    Search.Free;
  end;

  Result := 0;
end;

end.