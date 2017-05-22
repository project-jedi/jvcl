unit JFileSearch;
interface
uses
  SysUtils, Classes;

{$IFNDEF MSWINDOWS} // Delphi 1-5
 {$IFNDEF LINUX}
const
  PathDelim = '\';
 {$ENDIF} 
{$ENDIF}

type
  TSearchTypes = Set of (stFileMask);

  PFileParams = ^TFileParams;
  TFileParams = record
    FileMask: string;
    SearchTypes: TSearchTypes;
  end;

  TSearchOptions = Set of (soStripDirs);
  TDirOption = (doIncludeSubDirs);

  TJvSearchFiles = class(TComponent)
  private
    FFiles: TStrings;
    FFileParams: TFileParams;
    FOptions: TSearchOptions;
    FDirOption: TDirOption;
    FRootDirectory: string;
    function GetFileParams: PFileParams;
    procedure SearchDirs(const Dir: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Search;

    property RootDirectory: string read FRootDirectory write FRootDirectory;
    property FileParams: PFileParams read GetFileParams;
    property Options: TSearchOptions read FOptions write FOptions;
    property DirOption: TDirOption read FDirOption write FDirOption;
    property Files: TStrings read FFiles;
  end;

implementation

{ TJvSearchFiles }

constructor TJvSearchFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFiles := TStringList.Create;
  FOptions := [soStripDirs];
  FDirOption := doIncludeSubDirs;
end;

destructor TJvSearchFiles.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

function TJvSearchFiles.GetFileParams: PFileParams;
begin
  Result := @FFileParams;
end;

procedure TJvSearchFiles.Search;
begin
  if (FRootDirectory <> '') and (FRootDirectory[Length(FRootDirectory)] = PathDelim) then
    Delete(FRootDirectory, Length(FRootDirectory), 1);
  FFiles.Clear;
  SearchDirs(FRootDirectory);
end;

procedure TJvSearchFiles.SearchDirs(const Dir: string);
var
  sr: TSearchRec;
begin
  if FindFirst(Dir + PathDelim + FFileParams.FileMask,
       faAnyFile and not faDirectory, sr) = 0 then
  try
    repeat
      if (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        if soStripDirs in FOptions then
          FFiles.Add(sr.Name)
        else
          FFiles.Add(Dir + PathDelim + sr.Name);
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;

  if DirOption = doIncludeSubDirs then
  begin
    if FindFirst(Dir + PathDelim + '*.*', faDirectory, sr) = 0 then
    try
      repeat
        if sr.Attr and faDirectory <> 0 then
          if (sr.Name <> '.') and (sr.Name <> '..') then
            SearchDirs(Dir + PathDelim + sr.Name)
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

end.