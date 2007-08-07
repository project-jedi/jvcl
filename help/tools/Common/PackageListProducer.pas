unit PackageListProducer;

interface

uses
  Classes, JVCLHelpUtils;

type
  TPackageListProducer = class(TTask)
  private
    FPackageDir: string;
    FPackageListFileName: string;
    FRuntimePasDir: string;

    function ProcessFile(const AFileName: string; List, AllList: TStrings): Boolean;
    procedure ReportFileDiff(AllFilesInPackages: TStrings);
    procedure UpdatePackageList(NewList: TStrings);
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    property PackageDir: string read FPackageDir write FPackageDir;
    property PackageListFileName: string read FPackageListFileName write FPackageListFileName;
    property RuntimePasDir: string read FRuntimePasDir write FRuntimePasDir;
  end;

implementation

uses
  SysUtils,
  DelphiParser;

//=== Local procedures =======================================================

function DpkNameToNiceName(const S: string): string;
begin
  { 1234567890123
    JvCoreD7R.dpk ->

    Core
  }
  Result := Copy(S, 3, Length(S) - 9);
end;

//=== { TPackageListProducer } ===============================================

function TPackageListProducer.CanStart: Boolean;
begin
  Result := CheckDir(RuntimePasDir) and CheckDir(PackageDir) and
    not IsNullStr(PackageListFileName);
end;

function TPackageListProducer.DoExecute: Boolean;
var
  RunTimeDpkFiles: TStringList;
  FilesInPackages, AllFilesInPackages: TStringList;
  I: Integer;
begin
  Result := True;

  RunTimeDpkFiles := TStringList.Create;
  FilesInPackages := TStringList.Create;
  AllFilesInPackages := TStringList.Create;
  try
    FilesInPackages.Sorted := True;
    RunTimeDpkFiles.Sorted := True;
    AllFilesInPackages.Sorted := True;

    StatusMsg('Retrieving files from package directory..');
    GetAllFilesFrom(PackageDir, '*r.dpk', RunTimeDpkFiles);

    for I := 0 to RunTimeDpkFiles.Count - 1 do
    begin
      Progress(I, RunTimeDpkFiles.Count);
      ProcessFile(RunTimeDpkFiles[I], FilesInPackages, AllFilesInPackages);
    end;

    ReportFileDiff(AllFilesInPackages);
    UpdatePackageList(FilesInPackages);
  finally
    RunTimeDpkFiles.Free;
    FilesInPackages.Free;
    AllFilesInPackages.Free;
  end;
end;

function TPackageListProducer.GetTaskDescription: string;
begin
  Result := 'Generating package file..';
end;

function TPackageListProducer.ProcessFile(const AFileName: string; List,
  AllList: TStrings): Boolean;
var
  DpkParser: TDpkParser;
  I: Integer;
begin
  DpkParser := TDpkParser.Create;
  try
    Result := DpkParser.ExecuteFile(AFileName);
    if Result then
    begin
      for I := 0 to DpkParser.List.Count - 1 do
        List.Add(Format('%s=%s', [DpkParser.List[I], DpkNameToNiceName(ExtractFileName(AFileName))]));
      AllList.AddStrings(DpkParser.List);
    end
    else
      ErrorMsgFmt('Parse error %s - %s', [AFileName, DpkParser.ErrorMsg]);
  finally
    DpkParser.Free;
  end;
end;

procedure TPackageListProducer.ReportFileDiff(AllFilesInPackages: TStrings);
var
  NotInRuntimePasDir: TStringList;
  NotInDpk: TStringList;
  AllPasFiles: TStringList;
  I: Integer;
begin
  AllPasFiles := TStringList.Create;
  NotInRuntimePasDir := TStringList.Create;
  NotInDpk := TStringList.Create;
  try
    AllPasFiles.Sorted := True;
    StatusMsg('Retrieving files from run-time directory..');
    GetAllFilesFrom(RuntimePasDir, '*.pas', AllPasFiles, False, True);

    StatusMsg('Comparing..');
    DiffLists(AllPasFiles, AllFilesInPackages, nil, NotInRuntimePasDir, NotInDpk);
    if NotInRuntimePasDir.Count > 0 then
    begin
      HintMsg('Files in .dpk''s, but not in .pas dir');
      for I := 0 to NotInRuntimePasDir.Count - 1 do
        HintMsg('  ' + NotInRuntimePasDir[i]);
    end;
    if NotInDpk.Count > 0 then
    begin
      HintMsg('Files in .pas dir, but not in any .dpk file');
      for I := 0 to NotInDpk.Count - 1 do
        HintMsg('  ' + NotInDpk[i]);
    end;
  finally
    NotInDpk.Free;
    NotInRuntimePasDir.Free;
    AllPasFiles.Free;
  end;
end;

procedure TPackageListProducer.UpdatePackageList(
  NewList: TStrings);
var
  I: Integer;
  OldList: TStringList;
  Added: TStringList;
  Removed: TStringList;
begin
  OldList := TStringList.Create;
  Added := TStringList.Create;
  Removed := TStringList.Create;
  try
    StatusMsg('Loading..');
    OldList.Sorted := True;
    OldList.LoadFromFile(PackageListFileName);
    Added.Sorted := True;
    Removed.Sorted := True;

    StatusMsg('Comparing..');
    DiffLists(OldList, NewList, nil, Added, Removed);
    if (Added.Count = 0) and (Removed.Count = 0) then
      HintMsg('Nothing changed');
    if Removed.Count > 0 then
      for I := 0 to Removed.Count - 1 do
        HintMsgFmt('Removed <%s>', [Removed[i]]);
    if Added.Count > 0 then
      for I := 0 to Added.Count - 1 do
        HintMsgFmt('Added <%s>', [Added[i]]);

    StatusMsg('Saving..');
    NewList.SaveToFile(PackageListFileName);
  finally
    Added.Free;
    Removed.Free;
    OldList.Free;
  end;
end;

end.
