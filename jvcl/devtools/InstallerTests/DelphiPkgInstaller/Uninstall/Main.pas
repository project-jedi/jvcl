unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DelphiData, ShellAPI;

type
  TFormMain = class(TForm)
    ProgressBar: TProgressBar;
    LblStatus: TLabel;
    BtnUninstall: TButton;
    BtnCancel: TButton;
    procedure BtnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnUninstallClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private-Deklarationen }
    FLog: TStrings;
    FTargetList: TCompileTargetList;
    FTarget: TCompileTarget;
  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;

implementation

uses
  JvGnugettext;

{$R *.dfm}

var
  Uninstalled: Boolean = False;
  DeleteUninstallerDir: Boolean = False;

procedure TFormMain.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Filename: string;
  Title: string;
  Version: string;
  i: Integer;
begin
  Filename := ExtractFilePath(ParamStr(0)) + 'install.log';
  if not FileExists(Filename) then
  begin
    Application.ShowMainForm := False;
    MessageDlg(_('No install.log found.'), mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;
  FLog := TStringList.Create;
  try
    FLog.LoadFromFile(Filename);
  except
    Application.ShowMainForm := False;
    Application.HandleException(Self);
    Application.Terminate;
    Exit;
  end;

  Version := Copy(FLog[0], 9, MaxInt);
  Title := Copy(FLog[1], 7, MaxInt);

  FTargetList := TCompileTargetList.Create;
  for i := 0 to FTargetList.Count - 1 do
    if FTargetList[i].Name + ' ' + FTargetList[i].VersionStr = Version then
    begin
      FTarget := FTargetList[i];
      Break;
    end;

  if FTarget = nil then
  begin
    Application.ShowMainForm := False;
    MessageDlg(Format(_('No valid %s installation found.'), [Version]), mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;
  LblStatus.Caption := Format(LblStatus.Caption, [Title]);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FTargetList.Free;
  FLog.Free;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := BtnCancel.Enabled;
end;

procedure TFormMain.BtnUninstallClick(Sender: TObject);
var
  i: Integer;
  Action, Kind, Filename: string;
  Percentage, NewPercentage: Integer;
  S: string;
  Index: Integer;
  List: TStrings;
begin
  ProgressBar.Max := 100;
  ProgressBar.Position := 0;
  ProgressBar.Visible := True;
  BtnUninstall.Enabled := False;
  BtnCancel.Enabled := False;
  try
    LblStatus.Caption := 'Uninstalling...';
    Percentage := 0;
    for i := FLog.Count - 1 downto 2 do
    begin
      S := FLog[i];
      Action := Copy(S, 1, Pos(':', S) - 1);
      Delete(S, 1, Length(Action) + 1);
      if SameText(Action, 'FileAdd') then
      begin
        Filename := S;
        SetFileAttributes(PChar(Filename), 0);
        DeleteFile(Filename);
      end
      else
      if SameText(Action, 'DirAdd') then
      begin
        Filename := S;
        if not RemoveDir(Filename) then
        begin
          if SameText(Filename, ExtractFileDir(ParamStr(0))) then
            DeleteUninstallerDir := True;
        end;
      end
      else
      if SameText(Action, 'PathListAdd') then
      begin
        Kind := Copy(S, 1, Pos(',', S) - 1);
        Delete(S, 1, Length(Kind) + 1);
        List := nil;
        if SameText(Kind, 'Search') then
          List := FTarget.SearchPaths
        else if SameText(Kind, 'Browse') then
          List := FTarget.BrowsingPaths
        else if SameText(Kind, 'Debug') then
          List := FTarget.DebugDcuPaths;
        if List <> nil then
        begin
          Index := List.IndexOf(S);
          if Index = -1 then
            Index := List.IndexOf(FTarget.InsertDirMacros(S));
          if Index = -1 then
            Index := List.IndexOf(FTarget.ExpandDirMacros(S));
          if Index <> -1 then
            List.Delete(Index);
        end;
      end
      else if SameText(Action, 'PackageAdd') then
      begin
        Index := FTarget.KnownPackages.IndexOfFilename(S);
        if Index = -1 then
          Index := FTarget.KnownPackages.IndexOfFilename(FTarget.InsertDirMacros(S));
        if Index = -1 then
          Index := FTarget.KnownPackages.IndexOfFilename(FTarget.ExpandDirMacros(S));
        if Index <> -1 then
          FTarget.KnownPackages.Delete(Index);
      end;

      NewPercentage := Int64(FLog.Count - i) * 100 div FLog.Count;
      if NewPercentage <> Percentage then
      begin
        Percentage := NewPercentage;
        ProgressBar.Position := Percentage;
        Application.ProcessMessages;
      end;
    end;
    FTarget.SavePaths;
    FTarget.SavePackagesLists;
  finally
    LblStatus.Caption := 'Finished.';
    BtnCancel.Enabled := True;
    BtnCancel.Caption := _('Quit');
  end;
  Uninstalled := True;
end;

function GetTempDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetTempPath(Length(Result), PChar(Result)));
end;

procedure Finalize;
var
  f: TextFile;
  DeleteFileName: string;
begin
  DeleteFileName := IncludeTrailingPathDelimiter(ExtractShortPathName(GetTempDir)) + 'DeleteDelphiPkgUninstall.bat';

  DeleteFile(ExtractFilePath(ParamStr(0)) + 'install.log');
  SetFileAttributes(PChar(ParamStr(0)), 0);

  AssignFile(f, DeleteFileName);
  Rewrite(f);

  WriteLn(f, '@cd \');
  WriteLn(f, '@del ', ParamStr(0));
  if DeleteUninstallerDir then
    WriteLn(f, '@rd ', ExtractFileDir(ParamStr(0)));
  Write(f, '@del ', DeleteFileName);
  CloseFile(f);
  Application.ProcessMessages;

  ShellExecute(0, 'open', PChar(DeleteFileName), nil, nil, SW_HIDE);
  Halt(0);
end;

initialization

finalization
  if Uninstalled then
    Finalize;
end.
