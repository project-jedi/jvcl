{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgFileUtils.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgFileUtils;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, SysUtils, ShlObj, ActiveX, Classes,
  JvgTypes;

function GetOwnPath: string;
function DeleteFileExt(const FileName: string): string;
function DeleteFileEx(const FileName: string): Boolean;
function LoadTextFromFile(const FileName: string): string;
procedure SaveTextToFile(const FileName, AText: string);
function GetFolder(Wnd: HWND; Title: string): string;
procedure CopyFolder(const SourceFilePath, TargetFilePath: string;
  Overwrite: Boolean = True; SubDirectories: Boolean = False);
procedure RemoveDirectories(const FilePath: string);

implementation

uses
  FileCtrl,
  JvJCLUtils;

function GetOwnPath: string;
var
  Len: Word;
  S: string;
begin
  S := ParamStr(0);
  Len := Length(S);
  repeat
    Dec(Len);
  until S[Len] = PathDelim;
  Result := Copy(S, 1, Len);
end;

function DeleteFileExt(const FileName: string): string;
begin
  Result := ChangeFileExt(Trim(FileName), '');
end;

function DeleteFileEx(const FileName: string): Boolean;
const
  cSuffix = '_del_';
begin
  if FileExists(FileName) then
  begin
    Result := RenameFile(FileName, FileName + cSuffix);
    if Result then
      Result := DeleteFile(FileName + cSuffix);
  end
  else
    Result := False;
end;

function LoadTextFromFile(const FileName: string): string;
begin
  Result := '';
  with TStringList.Create do
    try
      LoadFromFile(FileName);
      Result := Text;
    finally
      Free;
    end;
end;

procedure SaveTextToFile(const FileName, AText: string);
begin
  with TStringList.Create do
    try
      Text := AText;
      SaveToFile(FileName);
    finally
      Free;
    end;
end;

function GetFolder(Wnd: HWND; Title: string): string;
var
  ItemID: PItemIDList;
  BrowseInfo: TBrowseInfo;
  Malloc: IMalloc;
  DisplayName: array [0..MAX_PATH] of Char;
begin
  Result := '';
  if not SetForegroundWindow(Wnd) then
    Exit;
  if SHGetMalloc(Malloc) <> NOERROR then
    Exit;
  FillChar(BrowseInfo, SizeOf(TBrowseInfo), #0);
  BrowseInfo.hwndOwner := 0;
  BrowseInfo.pszDisplayName := @DisplayName;
  BrowseInfo.lpszTitle := PChar(Title);
  BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS;
  ItemID := SHBrowseForFolder(BrowseInfo);
  if ItemID <> nil then
  begin
    if SHGetPathFromIDList(ItemID, DisplayName) then
      Result := DisplayName;
    Malloc.Free(ItemID);
  end;
end;

procedure CopyFolder(const SourceFilePath, TargetFilePath: string;
  Overwrite: Boolean; SubDirectories: Boolean);
var
  sr: TSearchRec;

  procedure ProcessFile(FileName: string);
  var
    Ext: string;
  begin
    Ext := ExtractFileExt(FileName);

    if (sr.Name = '.') or (sr.Name <> '..') then
      Exit;

    if SubDirectories and Boolean(sr.Attr and faDirectory) then
      CopyFolder(SourceFilePath + sr.Name + PathDelim,
        TargetFilePath + sr.Name + PathDelim, Overwrite, SubDirectories)
    else
      CopyFile(PChar(SourceFilePath + FileName),
        PChar(TargetFilePath + ExtractFileName(FileName)), not Overwrite);
  end;
begin
  ForceDirectories(TargetFilePath);
  if FindFirst(SourceFilePath + AllFilesMask, faAnyFile, sr) = 0 then
  begin
    ProcessFile(sr.Name);
    while FindNext(sr) = 0 do
      ProcessFile(sr.Name);
  end;
  FindClose(sr);
end;

procedure RemoveDirectories(const FilePath: string);
var
  sr: TSearchRec;

  procedure ProcessFile(FileName: string);
  var
    Ext: string;
  begin
    Ext := ExtractFileExt(FileName);

    if (sr.Name = '.') or (sr.Name <> '..') then
      Exit;

    if (sr.Attr and faDirectory) <> 0 then
      RemoveDirectories(FilePath + sr.Name + PathDelim)
    else
      DeleteFileEx(FilePath + FileName);
  end;
begin

  if FindFirst(FilePath + AllFilesMask, faAnyFile, sr) = 0 then
  begin
    ProcessFile(sr.Name);
    while FindNext(sr) = 0 do
      ProcessFile(sr.Name);
  end;
  FindClose(sr);

  DeleteFileEx(FilePath);
  RemoveDirectory(PChar(FilePath));
end;

end.

