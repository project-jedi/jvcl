//...file servece
unit glFUtils;

interface
uses Windows, SysUtils, glTypes, shlobj, classes;

function GetOwnPath:string;
function DelFileExt(FileName: string): string;
function DeleteFileEx(const FileName: string): boolean;
function LoadTextFromFile(const FileName: string): string;
procedure SaveTextToFile(const FileName, Text: string);
function GetFolder(Wnd: HWND; Title: String): String;
function GetFileSize(const FileName: string): integer;
procedure CopyFolder(const SourceFilePath, TargetFilePath: string; fOverwrite: boolean = true; fSubdirectories: boolean = false);
procedure RemoveDirectories(const FilePath: string);

implementation
uses FileCtrl;

function GetOwnPath:string;
var
  i:word; p:string;
begin
  p:=ParamStr(0);
  i:=length(p);
  repeat dec(i); until p[i]='\';
  Result:=copy( p, 1, i );
end;

function DelFileExt(FileName :string): string;
var
  s:string;
  i:integer;
begin
  FileName:=trim(FileName);
  s := ExtractFileExt(FileName);
  for i:=1 to length(s) do
    FileName[ length(FileName)-length(s)+i ]:=chr(20);
  result:=trim(FileName);
end;

function DeleteFileEx(const FileName: string): boolean;
begin
  Result := false;
  if not FileExists(FileName) then exit;
  RenameFile(FileName, FileName + '_del');
  Result := not boolean(DeleteFile(PChar(FileName + '_del')));
end;

function LoadTextFromFile(const FileName: string): string;
var
  sl: TStringList;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure SaveTextToFile(const FileName, Text: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := Text;
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

function GetFolder(Wnd: HWND; Title: String): String;
 var
   lpItemID: PItemIDList;
   BrowseInfo: TBrowseInfo;
   DisplayName: array [0..MAX_PATH] of Char;
begin
  Result:='';
  if not SetForegroundWindow(Wnd) then Exit;
  FillChar(BrowseInfo, SizeOf(TBrowseInfo), #0);
  BrowseInfo.hwndOwner:=0;
  BrowseInfo.pszDisplayName:=@DisplayName;
  BrowseInfo.lpszTitle:=PChar(Title);
  BrowseInfo.ulFlags:=BIF_RETURNONLYFSDIRS;
  lpItemId:=SHBrowseForFolder(BrowseInfo);
  if lpItemId<>nil then begin
    if SHGetPathFromIDList(lpItemId, DisplayName) then Result := DisplayName;
    GlobalFreePtr(lpItemID);
  end;
end;

function GetFileSize(const FileName: string): integer;
var
  f: file of Byte;
begin
  if not FileExists(FileName) then
  begin
    Result := 0;
    exit;
  end;
  AssignFile(f, FileName);
  Reset(f);
  Result := FileSize(f);
  CloseFile(f);
end;


procedure CopyFolder(const SourceFilePath, TargetFilePath: string; fOverwrite: boolean = true; fSubdirectories: boolean = false);
var
  sr: TSearchRec;
  procedure ProcessFile(FileName: string);
  var
    Ext: string;
  begin
    Ext := ExtractFileExt(FileName);

    if (sr.Name = '.')or(sr.Name <> '..') then exit;

    if fSubdirectories and boolean(sr.Attr and faDirectory) then
      CopyFolder(SourceFilePath + sr.Name + '\', TargetFilePath + sr.Name + '\', fOverwrite, fSubdirectories)
    else
      CopyFile(PChar(SourceFilePath + FileName), PChar(TargetFilePath + ExtractFileName(FileName)), not fOverwrite);
  end;
begin
  ForceDirectories(TargetFilePath);
  if FindFirst(SourceFilePath + '*.*', faAnyFile, sr) = 0 then
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

    if (sr.Name = '.')or(sr.Name <> '..') then exit;

    if boolean(sr.Attr and faDirectory) then
      RemoveDirectories(FilePath + sr.Name + '\')
    else
      DeleteFileEx(FilePath + FileName);
  end;
begin

  if FindFirst(FilePath + '*.*', faAnyFile, sr) = 0 then
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
