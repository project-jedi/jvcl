{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmMake.pas, released on 2003-11-27.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-11-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{.$I JVCL.INC}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$IFDEF VER150}
 // Delphi 7 .NET preview warnings
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

unit FrmMake;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, CoreData, BuildHelpers, FileCtrl, ShellAPI;

type
  TFormMake = class(TForm)
    BtnAbort: TButton;
    PageControl: TPageControl;
    TabSheetProgress: TTabSheet;
    TabSheetLog: TTabSheet;
    LblAction: TLabel;
    ProgressBar: TProgressBar;
    LblTarget: TLabel;
    ProgressBarTargets: TProgressBar;
    MemoLog: TRichEdit;
    LblOpenFile: TLabel;
    procedure BtnAbortClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TabSheetProgressShow(Sender: TObject);
    procedure TabSheetLogShow(Sender: TObject);
    procedure MemoLogSelectionChange(Sender: TObject);
    procedure LblOpenFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MemoLogKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
    FAborted: Boolean;
    FFinished: Boolean;
  public
    { Public-Deklarationen }
    procedure Execute;
  end;

var
  FormMake: TFormMake;

implementation
uses
  CapExec;

{$R *.dfm}

function Has(Text: string; const Values: array of string): Boolean;
var
  i: Integer;
begin
  Result := True;
  Text := AnsiLowerCase(Text);
  for i := 0 to High(Values) do
    if Pos(Values[i], Text) > 0 then
      Exit;
  Result := False;
end;

type
  TMakeThread = class(TThread)
  private
    FTarget: TTargetInfo;
    FError: Boolean;
    FAction: string;
    FCurLine: string;
    FMaxPackages: Integer;

    function GetAborted: Boolean;

    procedure CaptureLine(const Line: string; var Aborted: Boolean);
    function MakeTarget: Boolean;

    procedure ExceptionError;
    procedure Finished;
    procedure UpdateTarget;
    procedure UpdateAction;
    procedure UpdateCurLine;
    procedure IncTargetProgress;
    procedure InitPkgProgressBar;
    procedure IncPkgProgressBar;
  protected
    procedure Execute; override;

    property Aborted: Boolean read GetAborted;
  end;

{ TMakeThread }

function TMakeThread.GetAborted: Boolean;
begin
  Result := FormMake.FAborted;
end;

procedure TMakeThread.Finished;
begin
  FormMake.FFinished := True;
  FormMake.BtnAbort.Cancel := True;
  FormMake.BtnAbort.Caption := '&Close';
  if not FError then
  begin
    FormMake.LblTarget.Caption := 'JVCL 3 package installation';
    FormMake.LblAction.Caption := 'Finished.';
    MessageDlg('JVCL 3 packages were successfully installed.', mtInformation, [mbOk], 0);
  end
  else if Aborted then
  begin
    FormMake.LblTarget.Caption := FormMake.LblTarget.Caption + ' aborted.';
    FormMake.LblAction.Caption := FormMake.LblAction.Caption + ' aborted.';
    MessageDlg('Installation aborted.', mtWarning, [mbOk], 0);
  end
  else
  begin
    FormMake.LblTarget.Caption := FormMake.LblTarget.Caption + ' failed.';
    FormMake.LblAction.Caption := FormMake.LblAction.Caption + ' failed.';
    MessageDlg('Installation failed.', mtError, [mbOk], 0);
  end;
  FormMake.TabSheetLog.Show;
end;

procedure TMakeThread.UpdateTarget;
begin
  if FTarget <> nil then
    FormMake.LblTarget.Caption := FTarget.DisplayName
  else
    FormMake.LblTarget.Caption := '';
  UpdateAction;
end;

procedure TMakeThread.UpdateAction;
begin
  FormMake.LblAction.Caption := FAction;
end;

procedure TMakeThread.UpdateCurLine;
begin
  UpdateAction;
  if StartsWith(FCurLine, 'MAKE ') then
    if FormMake.MemoLog.Lines.Count > 0 then
      FormMake.MemoLog.Lines.Add(''); // otherwise only the first MAKE is printed
  FormMake.MemoLog.Lines.Add(FCurLine);

  if StartsWith(FCurLine, 'Compiling package: ') then
  begin
    IncPkgProgressBar;
    FormMake.MemoLog.SelStart := FormMake.MemoLog.SelStart - Length(FCurLine) - 2;
    FormMake.MemoLog.SelLength := Length(FCurLine);
    FormMake.MemoLog.SelAttributes.Style := [fsBold];
    FormMake.MemoLog.SelLength := 0;
  end
  else if Has(FCurLine, ['hint: ', 'hinweis: ', 'suggérer: ']) then
  begin
    FormMake.MemoLog.SelStart := FormMake.MemoLog.SelStart - Length(FCurLine) - 2;
    FormMake.MemoLog.SelLength := Length(FCurLine);
    FormMake.MemoLog.SelAttributes.Color := clGreen;
    FormMake.MemoLog.SelLength := 0;
  end
  else if Has(FCurLine, ['warning: ', 'warnung: ', 'avertissement: ']) then
  begin
    FormMake.MemoLog.SelStart := FormMake.MemoLog.SelStart - Length(FCurLine) - 2;
    FormMake.MemoLog.SelLength := Length(FCurLine);
    FormMake.MemoLog.SelAttributes.Color := clMaroon;
    FormMake.MemoLog.SelLength := 0;
  end
  else if Has(FCurLine, ['error: ', 'fehler: ', 'erreur: ']) then
  begin
    FormMake.MemoLog.SelStart := FormMake.MemoLog.SelStart - Length(FCurLine) - 2;
    FormMake.MemoLog.SelLength := Length(FCurLine);
    FormMake.MemoLog.SelAttributes.Color := clRed;
    FormMake.MemoLog.SelLength := 0;
  end
  else if Has(FCurLine, ['fatal: ']) then
  begin
    FormMake.MemoLog.SelStart := FormMake.MemoLog.SelStart - Length(FCurLine) - 2;
    FormMake.MemoLog.SelLength := Length(FCurLine);
    FormMake.MemoLog.SelAttributes.Color := clRed;
    FormMake.MemoLog.SelAttributes.Style := [fsBold];
    FormMake.MemoLog.SelLength := 0;
  end;
end;

procedure TMakeThread.ExceptionError;
begin
  Application.HandleException(Self);
end;

procedure TMakeThread.Execute;
var i: Integer;
begin
  if Terminated then Exit;
  try
    for i := 0 to TargetList.Count - 1 do
    begin
      FTarget := TargetList[i];
      if FTarget.CompileFor then
      begin
        FAction := 'Compiling packages...';
        Synchronize(UpdateTarget);
        FError := not MakeTarget;
        if (not FError) and (not Aborted) then
        begin
          FAction := 'Installing packages...';
          Synchronize(UpdateAction);
          FTarget.RegistryInstall;
          Synchronize(IncTargetProgress);
        end;
      end;
      FTarget := nil;
      if (Aborted) or (FError) then
        Break;
    end;
  except
    on E: Exception do
    begin
      FError := True;
      Synchronize(ExceptionError);
    end;
  end;
  Terminate;
  Synchronize(Finished);
end;

function TMakeThread.MakeTarget: Boolean;
var
  PrepareBpgData: TPrepareBpgData;
begin
  Result := False;
  if Aborted then Exit;
 // create make file
  PrepareBpgData := PrepareBpg(JVCLPackageDir + '\' + FTarget.BpgName,
    FTarget.SearchPaths, FTarget.LibDir, FTarget.BplDir, FTarget.DcpDir);
  try
    FMaxPackages := PrepareBpgData.Make.Projects.Count;
    Synchronize(InitPkgProgressBar);

    if FTarget.Build then
      SetEnvironmentVariable('DCCOPT', '-B')
    else
      SetEnvironmentVariable('DCCOPT', nil);

    SetEnvironmentVariable('ROOT', PChar(FTarget.RootDir));
    SetEnvironmentVariable('DCPDIR', Pointer(FTarget.DcpDir));
    SetEnvironmentVariable('BPLDIR', Pointer(FTarget.BplDir));
    Result := CaptureExecute('"' + FTarget.RootDir + '\Bin\make.exe"', '-f"' + ChangeFileExt(FTarget.BpgName, '.mak') + '"', JVCLPackageDir,
      CaptureLine) = 0;
  finally
    PrepareBpgData.Cleaning := Result;
    PrepareBpgData.Free;
  end;
end;

procedure TMakeThread.CaptureLine(const Line: string; var Aborted: Boolean);
begin
  Aborted := Self.Aborted;
  FCurLine := Line;
  if StartsWith(Line, 'Compiling package: ') then
    FAction := 'Compiling ' + ChangeFileExt(Copy(Line, 20, MaxInt), '');
  Synchronize(UpdateCurLine);
end;

procedure TMakeThread.IncTargetProgress;
begin
  FormMake.ProgressBarTargets.StepBy(1);
end;

procedure TMakeThread.IncPkgProgressBar;
begin
  FormMake.ProgressBar.StepBy(1);
end;

procedure TMakeThread.InitPkgProgressBar;
begin
  FormMake.ProgressBar.Position := 0;
  FormMake.ProgressBar.Max := FMaxPackages;
end;

{ TFormMake }

procedure TFormMake.Execute;
var
  i: Integer;
  BuildTargetCount: Integer;
  Thread: TMakeThread;
begin
  FFinished := False;
  FAborted := False;

  BtnAbort.Cancel := False;
  BtnAbort.Caption := '&Abort';
  MemoLog.Lines.Clear;
  BtnAbort.Enabled := True;
  TabSheetProgress.Show;
  PageControl.ActivePageIndex := 0;
  LblOpenFile.Visible := False;

  ProgressBar.Position := 0;
  ProgressBarTargets.Position := 0;

  LblAction.Caption := 'Initializing...';
  LblTarget.Caption := '';

  ActiveControl := PageControl;

  BuildTargetCount := 0;
  for i := 0 to TargetList.Count - 1 do
    if TargetList[i].CompileFor then
      Inc(BuildTargetCount);
  if BuildTargetCount > 0 then
  begin
    ProgressBarTargets.Max := BuildTargetCount;
    ProgressBarTargets.Visible := BuildTargetCount > 1;
    Thread := TMakeThread.Create(True);
    Thread.FreeOnTerminate := True;
    Thread.Resume;
    ShowModal;
    FFinished := True;
  end;
end;

procedure TFormMake.BtnAbortClick(Sender: TObject);
begin
  if FFinished then
    Close
  else
    FAborted := True;
end;

procedure TFormMake.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (FFinished) or (FAborted);
end;

procedure TFormMake.TabSheetProgressShow(Sender: TObject);
begin
  Height := 195;
end;

procedure TFormMake.TabSheetLogShow(Sender: TObject);
begin
  Height := 400;
  MemoLog.SetFocus;
end;

procedure TFormMake.MemoLogSelectionChange(Sender: TObject);
var
  S: string;
  ps: Integer;
begin
  if not FFinished then
    Exit;
  if (MemoLog.CaretPos.Y > 0) and (MemoLog.CaretPos.Y < MemoLog.Lines.Count) then
  begin
    S := MemoLog.Lines[MemoLog.CaretPos.Y];
    if StartsWith(S, JVCLDir + '\') then
    begin
      ps := Pos('(', S);
      if ps > 0 then
      begin
        Delete(S, ps, MaxInt);
        if FileExists(S) then
        begin
          LblOpenFile.Hint := S;
          LblOpenFile.Caption := MinimizeName(LblOpenFile.Hint, Canvas, LblOpenFile.Width);
          LblOpenFile.Visible := True;
        end
        else
          LblOpenFile.Visible := False;
      end;
    end
    else
      LblOpenFile.Visible := False;
  end;
end;

procedure TFormMake.LblOpenFileClick(Sender: TObject);
begin
  if ShellExecute(Handle, 'open', PChar(LblOpenFile.Hint), nil, nil, SW_SHOW) < 32 then
    MessageDlg('Error opening the file.', mtError, [mbOk], 0);
end;

procedure TFormMake.FormCreate(Sender: TObject);
begin
  BtnAbort.Anchors := [akRight, akBottom];
  LblOpenFile.Anchors := [akLeft, akBottom];
  PageControl.Anchors := [akLeft, akTop, akRight, akBottom];
end;

procedure TFormMake.MemoLogKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FFinished then
    if (Shift = []) and (Key = VK_ESCAPE) then
      BtnAbort.Click;
end;

end.
