{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmMake.pas, released on 2003-11-27.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2003-2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}

{$IFDEF COMPILER6_UP}
  {$WARN UNIT_PLATFORM OFF}
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
    procedure MemoLogKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    FException: Exception;

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
    function CompilePackageGroup(const BpgFilename, IncludePaths,
      UnitPaths, SourcePaths, OutDir, LibOutDir: string;
      IsJcl: Boolean): Boolean;
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
    FormMake.LblTarget.Caption := 'JVCL3 package installation';
    FormMake.LblAction.Caption := 'Finished.';
    MessageDlg('JVCL3 packages were successfully installed.', mtInformation, [mbOk], 0);
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
  else if StartsWith(FCurLine, '** ') then
  begin
    IncPkgProgressBar;
    FormMake.MemoLog.SelStart := FormMake.MemoLog.SelStart - Length(FCurLine) - 2;
    FormMake.MemoLog.SelLength := Length(FCurLine);
    FormMake.MemoLog.SelAttributes.Style := [fsBold];
    FormMake.MemoLog.SelLength := 0;
  end
  else if Has(FCurLine, ['hint: ', 'hinweis: ', 'suggestion: ']) then
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
  FCurLine := '';
  UpdateCurLine;
  FCurLine := '---------------------------------------------------------------';
  UpdateCurLine;
  FCurLine := Format('JVCL 3 Package Installer: Exception: [%s] %s',
    [Exception(FException).ClassName, Exception(FException).Message]);
  UpdateCurLine;
  Application.ShowException(FException);
end;

procedure TMakeThread.Execute;
var
  i: Integer;
begin
  if Terminated then Exit;
  try
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
        FException := E;
        FError := True;
        Synchronize(ExceptionError);
      end;
    end;
  finally
    Terminate;
    Synchronize(Finished);
  end;
end;

function TMakeThread.CompilePackageGroup(const BpgFilename, IncludePaths,
  UnitPaths, SourcePaths, OutDir, LibOutDir: string; IsJcl: Boolean): Boolean;
var
  PrepareBpgData: TPrepareBpgData;
  Build, Options, DcpOptions, StartDir: string;
  // JclMakeFilename: string;
  // Files: TStrings;
  // PackageList: TPackageList;
  // i: Integer;
  b: Boolean;
begin
  Result := True;

  StartDir := ExtractFileDir(BpgFilename);
  if FTarget.Build then Build := '-B ' else Build := '';

  Options := Build;
  if IncludePaths <> '' then Options := Options + Format('-I"%s" ', [IncludePaths]);
  if UnitPaths <> '' then Options := Options + Format('-U"%s" ', [UnitPaths]);
  if OutDir <> '' then Options := Options + Format('-N"%s" ', [OutDir]);

  DcpOptions := Options;
 // Add source paths to "Options" but not to "DcpOptions"
  if SourcePaths <> '' then Options := Options + Format('-U"%s" ', [SourcePaths]);

  SetEnvironmentVariable('JCLROOT', Pointer(FTarget.JCLDir));
  SetEnvironmentVariable('DCCOPT', nil); // used by Delphi
  SetEnvironmentVariable('DCC32', nil); // used by BCB
  SetEnvironmentVariable('ROOT', PChar(FTarget.RootDir));
  SetEnvironmentVariable('DCPDIR', Pointer(FTarget.DcpDir));
  if FTarget.IsBCB then
    SetEnvironmentVariable('BPLDIR', PChar(FTarget.DcpDir)) // corrected by MoveBCBFiles
  else
    SetEnvironmentVariable('BPLDIR', Pointer(FTarget.BplDir));


  if FTarget.IsDelphi then
    SetEnvironmentVariable('DCCOPT', PChar('-M ' + Options))
  else
    SetEnvironmentVariable('DCC32', PChar('dcc32 -Q -M ' + Options));

  if (IsJCL) and (FTarget.IsBCB) and (FTarget.InstallJcl) and (FTarget.Build) then
  begin
    b := False;
    CaptureLine('** Deleting dcu files...', b);
    DeleteJclDcuFiles(OutDir, StartDir + '\xyz');
    DeleteJclDcuFiles(OutDir + '\obj', StartDir + '\xyz');
  end;

  // generate resources if necessary
  b := False;
  CaptureLine('** Generating resources...', b);
  CaptureExecute('"' + FTarget.RootDir + '\Bin\make.exe"',
    '-f makefile.mak', JVCLDir + '\images', CaptureLine);

  if not FTarget.IsBCB then
  begin
    if (not IsJCL) or (FTarget.InstallJcl {or FTarget.IsBCB}) then
    begin
     // create make file
      PrepareBpgData := PrepareBpg(BpgFilename, FTarget, IsJCL);
      try
        FMaxPackages := PrepareBpgData.Make.Projects.Count;
        if (FTarget.IsBCB) and (IsJcl) then
          FMaxPackages := FMaxPackages * 2; // include DCP creation steps
        Synchronize(InitPkgProgressBar);

       // compile
        Result := CaptureExecute('"' + FTarget.RootDir + '\Bin\make.exe"',
          '-B -f"' + ChangeFileExt(BpgFilename, '.mak') + '"', StartDir,
          CaptureLine) = 0;
        if FTarget.IsBCB then
        begin
          MoveBCBFiles(LibOutDir, FTarget); // move .lib, .bpi to DcpDir, .bpl to BplDir and deletes .tds
          if FTarget.MoveHppFiles then
            MoveHPPFiles(SourcePaths, StartDir + '\xyz', FTarget);
        end;
      finally
        SetEnvironmentVariable('DCCOPT', nil);
        SetEnvironmentVariable('DCC32', nil);
        SetEnvironmentVariable('JCLROOT', nil);

        PrepareBpgData.Cleaning := Result;
        PrepareBpgData.Free;
      end;
    end;
  end
  else
  begin
    // Call the batch file if the target is BCB because it's easier
    // to maintain.
    {i := }CaptureExecute(
                'makebcb.bat',
                '"' + ChangeFileExt(BpgFileName, '') + '" '+
                '"' + FTarget.JVCLDirName + '" ' +
                '"' + FTarget.RootDir + '" ',
                JVCLPackageDir,
                CaptureLine);
    //Result := i=1;     // Note: the batch file returns 1...
    // But if everything went ok, the makefile shouldn't be there
    Result := not FileExists(JVCLPackageDir + ChangeFileExt(BpgFilename, '.mak'));
  end;

  if Result and FTarget.IsBCB and IsJcl then
  begin
   // *****
   // C++BUILDER .dcp creation
   // *****
    { The batch file creates a Error.dat file in the package directory. This
      is better than checking the errorlevel because under Win9x/ME the
      errorlevel is allways 0. }
    DeleteFile(JVCLPackageDir + '\Error.dat');
    {i := }CaptureExecute(
                'makejcldcp4bcb.bat',
                IntToStr(FTarget.MajorVersion)+ ' '+
                '"' + FTarget.RootDir + '" '+
                '"' + FTarget.JCLDir + '"',
                JVCLPackageDir,
                CaptureLine);
    //Result := i=1;     // Note: the batch file returns 1...
    Result := not FileExists(JVCLPackageDir + '\Error.dat');
    DeleteFile(JVCLPackageDir + '\Error.dat');
    
{    // create Delphi packages for BCB .dcp compilation
    Files := TStringList.Create; // files that were created
    try
      //if IsJcl then
        PackageList := CreateJclPackageList(FTarget);
      //else
      //  PackageList := FTarget.Packages; // WRONG. contains only design-time packages and have generic names instead of BCB x.0 names
      try
        for i := 0 to PackageList.Count - 1 do
          CreateDelphiPackageForBCB(PackageList[i], Files, IsJcl);
      finally
        if isJcl then
          PackageList.Free;
      end;

      JclMakeFilename := ChangeFileExt(BpgFilename, '') + 'dcp.mak';
     // create make file
      PrepareBpgData := PrepareDcpBpg(JclMakeFilename, Files, FTarget, IsJcl);
      try
        if IsJcl and not FTarget.InstallJcl then
        begin
          FMaxPackages := PrepareBpgData.Make.Projects.Count;
          Synchronize(InitPkgProgressBar);
        end;

        PrepareBpgData.CreatedFiles.AddStrings(Files);
        SetEnvironmentVariable('DCCOPT', Pointer(DcpOptions));

       // compile
        Result := CaptureExecute('"' + FTarget.RootDir + '\Bin\make.exe"',
          '-B -f"' + JclMakeFilename + '"', StartDir, // hard coded "-B"
          CaptureLine) = 0;
{       (ahuser) Do not delete the DCUs after the compilation. Delete them before a build.
        if Result then
        begin
          DeleteDcuFiles(OutDir, StartDir + '\xyz');
          DeleteDcuFiles(OutDir + '\obj', StartDir + '\xyz');
        end;}
{      finally
        SetEnvironmentVariable('DCCOPT', nil);
        PrepareBpgData.Cleaning := Result;
        PrepareBpgData.Free;
      end;
    finally
      Files.Free;
    end;}
  end;

end;

function TMakeThread.MakeTarget: Boolean;
var
  JclBpgFilename, Prefix, ObjDirIfNecessary: string;
begin
  Result := False;
  if Aborted then Exit;
  Result := True;

  if (FTarget.InstallJcl) or (FTarget.JCLNeedsDcp) then
  begin
    if FTarget.IsDelphi then
    begin
      Prefix := 'D';
      ObjDirIfNecessary := '';
    end
    else
    begin
      Prefix := 'C';
      ObjDirIfNecessary := '\obj';
    end;
    JclBpgFilename := 'JclPackages' + Prefix + IntToStr(FTarget.MajorVersion) + '0.bpg';

    Result := CompilePackageGroup(
      FTarget.JCLPackageDir + '\' + JclBpgFilename,
      JclIncludePaths,
      JclLibDir + '\' + FTarget.JclDirName + ';' + FTarget.DcpDir,
      JclSourcePaths,
      JclLibDir + '\' + FTarget.JclDirName + ObjDirIfNecessary,
      FTarget.JCLPackageDir + '\' + FTarget.JclDirName,
      True);
    if Result then
      FTarget.JclRegistryInstall;
  end;

  if Result then
  begin
    Result := CompilePackageGroup(
      JVCLPackageDir + '\' + FTarget.BpgName,
      JVCLIncludePaths,
      JVCLDir + '\' + FTarget.LibDir + ';' + FTarget.DcpDir + ';' + FTarget.BplDir,
      JVCLSourcePaths,
      JVCLDir + '\' + FTarget.LibDir,
      FTarget.DcpDir,
      False);
  end;
end;

procedure TMakeThread.CaptureLine(const Line: string; var Aborted: Boolean);
begin
  Aborted := Self.Aborted;
  FCurLine := TrimRight(Line);
  if StartsWith(Line, 'Compiling package: ') then
    FAction := 'Compiling ' + ChangeFileExt(Copy(Line, 20, MaxInt), '')
  else if StartsWith(Line, '** ') and not StartsWith(Line, '** error') then
    FAction := Copy(Line, 4, MaxInt);
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
  i: Integer;
  ShowDirLbl: Boolean;
begin
  if not FFinished then
    Exit;
  if (MemoLog.CaretPos.Y > 0) and (MemoLog.CaretPos.Y < MemoLog.Lines.Count) then
  begin
    S := MemoLog.Lines[MemoLog.CaretPos.Y];

   // find the directory
    if StartsWith(S, JVCLDir + '\') then
      ShowDirLbl := True
    else
    begin
      ShowDirLbl := False;
      for i := 0 to TargetList.Count - 1 do
        if StartsWith(S, TargetList[i].JCLDir + '\') then
        begin
          ShowDirLbl := True;
          Break;
        end;
    end;

    if ShowDirLbl then
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
