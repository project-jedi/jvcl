{
  Итератор для поиска файлов по FindFirst/Next включая поддиректории.
}
unit FileIterator;

interface
uses Windows, Classes, SysUtils;

type

  PSearchData = ^TSearchData;
  TSearchData = record
    sr: TSearchRec;
    Path: string;
  end;

  TFileIterator = class
  private
    FileExt: string;
    FPath: string;
    FAttr: Integer;
    FRecurse: boolean;
    PCurrentItem: PSearchData;
    lSearchRecs: TList;
    slFileExt: TStringList;

    FLastSearchResult: integer;
    FindOpened: boolean;

    function CheckResult(Value: integer): boolean;
    procedure FindClose(Destroing: boolean = false);
    function GetCurrentItem: TSearchRec;
    function GetPath: string;
    function CheckFileExt(const FileName: string): boolean;
  public
    property CurrentItem: TSearchRec read GetCurrentItem; // последний результат поиска
    property Path: string read GetPath;                   // заданный для поиска путь
    property Attr: Integer read FAttr;                  // и атрибуты
    property Recurse: boolean read FRecurse;

    property ErrorCode: integer read FLastSearchResult; // код ошибки Windows
    constructor Create;
    destructor Destroy; override;
    procedure First(const FilePath, FileExt: string; FileAttr: Integer; RecurseSearch: boolean);
    procedure Next;
    function IsDone: boolean;
  end;

implementation

{ TFileIterator }

procedure TFileIterator.First(const FilePath, FileExt: string; FileAttr: Integer; RecurseSearch: boolean);
begin
  if FileExt <> '' then
    slFileExt.CommaText := LowerCase(FileExt);

  FPath := ExtractFilePath(FilePath);
  FAttr := FileAttr;
  FRecurse := RecurseSearch;

  New(PCurrentItem);
  lSearchRecs.Add(PCurrentItem);

  PCurrentItem^.Path := ExtractFilepath(FilePath);
  try
    FLastSearchResult := sysUtils.FindFirst(FPath + '*.*', FileAttr, PCurrentItem^.sr);
    FindOpened := CheckResult(FLastSearchResult);
    if not FindOpened then FindClose
    else
      if not CheckFileExt(PCurrentItem^.sr.Name) then Next;
  except
    FindClose;
  end;

end;

function TFileIterator.CheckResult(Value: integer): boolean;
begin
  case Value of
    0: Result := true;
    ERROR_NO_MORE_FILES:
    begin
      FindClose;
      Result := false;
    end;
    else
      RaiseLastWin32Error;
  end;
end;

function TFileIterator.IsDone: boolean;
begin
  Result := FLastSearchResult <> 0;
end;

procedure TFileIterator.Next;
begin
  //if FindOpened then
  begin
    FLastSearchResult := FindNext(PCurrentItem^.sr);
    FindOpened := CheckResult(FLastSearchResult);

    if not FindOpened then exit;

    if FRecurse and (PCurrentItem^.sr.Attr and faDirectory = faDirectory) and (PCurrentItem^.sr.Name <> '.') and (PCurrentItem^.sr.Name <> '..') then
      First(ExtractFilePath(PCurrentItem^.Path) + PCurrentItem^.sr.Name + '\', '', FAttr, true)
    else
      if not CheckFileExt(PCurrentItem^.sr.Name) then Next;

  end;// else
//    raise Exception.Create('Call Next method after First method');
end;

function TFileIterator.CheckFileExt(const FileName: string): boolean;
begin
  Result := not((FileName = '.')or(FileName = '..'));
  if not Result then exit;
  Result := (trim(slFileExt.Text) = '*')or(slFileExt.IndexOf(LowerCase(ExtractFileExt(FileName))) <> -1);
end;

procedure TFileIterator.FindClose(Destroing: boolean = false);
begin
  if lSearchRecs.Count = 0 then exit;
  Sysutils.FindClose(PCurrentItem^.sr);
  Dispose(lSearchRecs[lSearchRecs.Count-1]);

  lSearchRecs.Count := lSearchRecs.Count-1;

  if not Destroing and (lSearchRecs.Count > 0) then
  begin
    PCurrentItem := lSearchRecs[lSearchRecs.Count-1];
    Next;
  end;

end;

destructor TFileIterator.Destroy;
begin
  inherited;
  while lSearchRecs.Count > 0 do FindClose(true);
  lSearchRecs.Free;
  slFileExt.Free;
end;

constructor TFileIterator.Create;
begin
  lSearchRecs := TList.Create;
  slFileExt := TStringList.Create;
end;

function TFileIterator.GetCurrentItem: TSearchRec;
begin
  Result := PCurrentItem^.sr;
end;

function TFileIterator.GetPath: string;
begin
  Result := PSearchData(lSearchRecs[lSearchRecs.Count-1])^.Path;
end;

end.
