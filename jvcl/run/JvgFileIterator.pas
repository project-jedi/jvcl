{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgFileIterator.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Итератор для поиска файлов по FindFirst/Next включая поддиректории.
  Iterator, searching files by FindFirst/Next including subdirs [translated]

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgFileIterator;

{$I jvcl.inc}

interface

uses
  Windows, Classes, SysUtils;

type
  PSearchData = ^TSearchData;
  TSearchData = record
    sr: TSearchRec;
    Path: string;
  end;

  TJvgFileIterator = class(TObject)
  private
    //    FileExt: string;
    FPath: string;
    FAttr: Integer;
    FRecurse: Boolean;
    FPCurrentItem: PSearchData;
    FLSearchRecs: TList;
    FSLFileExt: TStringList;
    FLastSearchResult: Integer;
    FFindOpened: Boolean;
    function CheckResult(Value: Integer): Boolean;
    procedure FindClose(Destroying: Boolean = False);
    function GetCurrentItem: TSearchRec;
    function GetPath: string;
    function CheckFileExt(const FileName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    { Last result of search [translated] }
    property CurrentItem: TSearchRec read GetCurrentItem; // последний результат поиска
    { Path to search in [translated] }
    property Path: string read GetPath; // заданный для поиска путь
    { And attributes [translated] }
    property Attr: Integer read FAttr; // и атрибуты
    property Recurse: Boolean read FRecurse;

    { Windows Error Code [translated] }
    property ErrorCode: Integer read FLastSearchResult; // код ошибки Windows
    procedure First(const FilePath, FileExt: string; FileAttr: Integer; RecurseSearch: Boolean);
    procedure Next;
    function IsDone: Boolean;
  end;

implementation

uses
  JvJCLUtils;

constructor TJvgFileIterator.Create;
begin
  inherited Create;
  FLSearchRecs := TList.Create;
  FSLFileExt := TStringList.Create;
end;

destructor TJvgFileIterator.Destroy;
begin
  while FLSearchRecs.Count > 0 do
    FindClose(True);
  FLSearchRecs.Free;
  FSLFileExt.Free;
  inherited Destroy;
end;

procedure TJvgFileIterator.First(const FilePath, FileExt: string; FileAttr: Integer; RecurseSearch: Boolean);
begin
  if FileExt <> '' then
    FSLFileExt.CommaText := LowerCase(FileExt);

  FPath := ExtractFilePath(FilePath);
  FAttr := FileAttr;
  FRecurse := RecurseSearch;

  New(FPCurrentItem);
  FLSearchRecs.Add(FPCurrentItem);

  FPCurrentItem^.Path := ExtractFilePath(FilePath);
  try
    FLastSearchResult := SysUtils.FindFirst(FPath + '*.*', FileAttr, FPCurrentItem^.sr);
    FFindOpened := CheckResult(FLastSearchResult);
    if not FFindOpened then
      FindClose
    else
    if not CheckFileExt(FPCurrentItem^.sr.Name) then
      Next;
  except
    FindClose;
  end;

end;

function TJvgFileIterator.CheckResult(Value: Integer): Boolean;
begin
  Result := True;
  case Value of
    0:
      Result := True;
    ERROR_NO_MORE_FILES:
      begin
        FindClose;
        Result := False;
      end;
  else
    RaiseLastOSError;
  end;
end;

function TJvgFileIterator.IsDone: Boolean;
begin
  Result := FLastSearchResult <> 0;
end;

procedure TJvgFileIterator.Next;
begin
  //if FFindOpened then
  begin
    FLastSearchResult := FindNext(FPCurrentItem^.sr);
    FFindOpened := CheckResult(FLastSearchResult);

    if not FFindOpened then
      Exit;

    if FRecurse and (FPCurrentItem^.sr.Attr and faDirectory = faDirectory) and
      (FPCurrentItem^.sr.Name <> '.') and (FPCurrentItem^.sr.Name <> '..') then
      First(ExtractFilePath(FPCurrentItem^.Path) + FPCurrentItem^.sr.Name + '\', '', FAttr, True)
    else
    if not CheckFileExt(FPCurrentItem^.sr.Name) then
      Next;

  end;
  // else
  //   raise Exception.Create('Call Next method after First method');
end;

function TJvgFileIterator.CheckFileExt(const FileName: string): Boolean;
begin
  Result := not ((FileName = '.') or (FileName = '..'));
  if Result then
    Result := (Trim(FSLFileExt.Text) = '*') or
      (FSLFileExt.IndexOf(LowerCase(ExtractFileExt(FileName))) <> -1);
end;

procedure TJvgFileIterator.FindClose(Destroying: Boolean = False);
begin
  if FLSearchRecs.Count = 0 then
    Exit;
  SysUtils.FindClose(FPCurrentItem^.sr);
  Dispose(FLSearchRecs[FLSearchRecs.Count - 1]);

  FLSearchRecs.Count := FLSearchRecs.Count - 1;

  if not Destroying and (FLSearchRecs.Count > 0) then
  begin
    FPCurrentItem := FLSearchRecs[FLSearchRecs.Count - 1];
    Next;
  end;
end;

function TJvgFileIterator.GetCurrentItem: TSearchRec;
begin
  Result := FPCurrentItem^.sr;
end;

function TJvgFileIterator.GetPath: string;
begin
  Result := PSearchData(FLSearchRecs[FLSearchRecs.Count - 1])^.Path;
end;

end.

