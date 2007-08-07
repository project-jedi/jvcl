unit DirCleaner;

interface

uses
  Classes,
  JVCLHelpUtils;

type
  TDirCleaner = class(TTask)
  private
    FDirs: TStrings;
    FFiles: TStrings;
    procedure HandleFindFile(Sender: TObject; const AName: string);
  protected
    function ScanDir(const ADir: string; const IncludeSubDirs: Boolean): Boolean;
    function Scan: Boolean;
    function Delete: Boolean;

    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;
    procedure AddDir(const ADir: string; const IncludeSubDirs: Boolean);
  end;

implementation

uses
  SysUtils,
  JvSearchFiles;

//=== { TDirCleaner } ========================================================

constructor TDirCleaner.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FDirs := TStringList.Create;
  FFiles := TStringList.Create;
end;

destructor TDirCleaner.Destroy;
begin
  FDirs.Free;
  FFiles.Free;
  inherited Destroy;
end;

procedure TDirCleaner.AddDir(const ADir: string;
  const IncludeSubDirs: Boolean);
begin
  FDirs.AddObject(ADir, TObject(IncludeSubDirs));
end;

function TDirCleaner.Delete: Boolean;
var
  I: Integer;
begin
  StatusMsg('Deleting..');

  Result := True;
  for I := 0 to FFiles.Count - 1 do
  begin
    Progress(I, FFiles.Count);
    Result := DeleteFile(FFiles[i]);
    if not Result then
    begin
      ErrorMsgFmt('Error deleting file %s: %s', [
        FFiles[i], SysErrorMessage(GetLastError)]);
      Exit;
    end;
  end;
end;

function TDirCleaner.DoExecute: Boolean;
begin
  Result := Scan and Delete;
end;

function TDirCleaner.GetTaskDescription: string;
begin
  Result := 'Cleaning directories..';
end;

procedure TDirCleaner.HandleFindFile(Sender: TObject; const AName: string);
begin
  FFiles.Add(AName);
end;

function TDirCleaner.Scan: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FDirs.Count - 1 do
  begin
    StatusMsgFmt('Scanning %s..', [FDirs[i]]);
    Result := ScanDir(FDirs[i], Boolean(FDirs.Objects[i]));
    if not Result then
      Exit;
  end;
end;

function TDirCleaner.ScanDir(const ADir: string;
  const IncludeSubDirs: Boolean): Boolean;
begin
  with TJvSearchFiles.Create(nil) do
  try
    RootDirectory := ADir;
    Options := [soSearchFiles, soOwnerData];
    ErrorResponse := erIgnore;
    if IncludeSubDirs then
      DirOption := doIncludeSubDirs
    else
      DirOption := doExcludeSubDirs;
    FileParams.Attributes.ReadOnly := tsMustBeUnSet;
    FileParams.Attributes.Hidden := tsMustBeUnSet;
    FileParams.Attributes.System := tsMustBeUnSet;
    FileParams.SearchTypes := [stAttribute];
    OnFindFile := HandleFindFile;
    Result := Search;
    if not Result then
      Exit;
  finally
    Free;
  end;
end;

end.
