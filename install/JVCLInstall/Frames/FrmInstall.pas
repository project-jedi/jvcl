{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmInstall.pas, released on 2004-04-06.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit FrmInstall;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls,
  ShellAPI,
  JVCL3Install, JVCLData, PackageUtils, Compile, Utils;

type
  TFrameInstall = class(TFrame)
    LblTarget: TLabel;
    ProgressBarTarget: TProgressBar;
    ProgressBarCompile: TProgressBar;
    LblInfo: TLabel;
    RichEditLog: TRichEdit;
    LblOpenFile: TLabel;
    BtnDetails: TButton;
    procedure RichEditLogSelectionChange(Sender: TObject);
    procedure BtnDetailsClick(Sender: TObject);
    procedure LblOpenFileClick(Sender: TObject);
  private
    FInitializing: Boolean;
    FInstaller: TInstaller;
    procedure Init;
  protected
    property Installer: TInstaller read FInstaller;
  private
    FPositionTarget: Integer;
    FPositionProject: Integer;
    FFinished: Boolean;
    FAborted: Boolean;
    procedure EvProgress(Sender: TObject; const Text: string;
      Position, Max: Integer; Kind: TProgressKind);
    procedure EvCaptureLine(const Text: string; var Aborted: Boolean);
    procedure EvIdle(Sender: TObject);
  public
    class function Build(Installer: TInstaller; Client: TWinControl): TFrameInstall;
    procedure Execute;

    property Aborted: Boolean read FAborted write FAborted;
  end;

implementation

{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF COMPILER6_UP}

uses
  InstallerConsts, FileCtrl;

{$R *.dfm}

{ TFrameInstall }

class function TFrameInstall.Build(Installer: TInstaller;
  Client: TWinControl): TFrameInstall;
begin
  Result := TFrameInstall.Create(Client);
  Installer.PackageInstaller.Translate(Result);
  Result.FInstaller := Installer;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameInstall.EvProgress(Sender: TObject; const Text: string;
  Position, Max: Integer; Kind: TProgressKind);
begin
  case Kind of
    pkTarget:
      begin
        FPositionTarget := Position;

        LblTarget.Caption := Text;
        LblTarget.Hint := Text;
        ProgressBarTarget.Max := Max * ProjectMax;
        ProgressBarTarget.Position := {FPositionProject +} (FPositionTarget * ProjectMax);
      end;

    pkProject:
      begin
        FPositionProject := Position;
        ProgressBarTarget.Position := FPositionProject + (FPositionTarget * ProjectMax);
        LblTarget.Caption := LblTarget.Hint + ' - ' + Text;
        ProgressBarCompile.Position := 0;
      end;

    pkResource,
    pkPackage:
      begin
        if Text <> '' then
          LblInfo.Caption := Format(RsCompiling, [Text])
        else
          LblInfo.Caption := '';
        ProgressBarCompile.Max := Max;
        ProgressBarCompile.Position := Position;
      end;

    pkOther:
      begin
        LblInfo.Caption := Text;
        ProgressBarCompile.Max := Max;
        ProgressBarCompile.Position := Position;
      end;
  end;

  Application.ProcessMessages;
end;

procedure TFrameInstall.EvCaptureLine(const Text: string;
  var Aborted: Boolean);

var
  Line: string;

  procedure SetFont(Styles: TFontStyles; Color: TColor = clNone);
  begin
    RichEditLog.SelStart := RichEditLog.SelStart - Length(Line) - 2;
    RichEditLog.SelLength := Length(Line);
    if Color <> clNone then
      RichEditLog.SelAttributes.Color := Color;
    RichEditLog.SelAttributes.Style := Styles;
    RichEditLog.SelLength := 0;
  end;

var
  LText: string;
begin
  Aborted := FAborted;
  Line := Text;
  if (Text <> '') and (Text[1] = #1) then
    Delete(Line, 1, 1);
  RichEditLog.Lines.Add(Line);
  if Text <> '' then
  begin
    if Text[1] = #1 then
      SetFont([fsBold], clGray)
    else
    if (Text[1] = #9) and not StartsWith(Text, #9'Loaded ') then
      SetFont([], clGray)
    else if Text[1] = '[' then
      SetFont([fsBold])
    else if Text = RsPackagesAreUpToDate then
      SetFont([], clTeal)
    else if HasText(Text, ['hint: ', 'hinweis: ', 'suggestion: ']) then // do not localize
      SetFont([], clGreen)
    else if HasText(Text, ['warning: ', 'warnung: ', 'avertissement: ']) then // do not localize
      SetFont([], clMaroon)
    else if HasText(Text, ['error: ', 'fehler: ', 'erreur: ']) then // do not localize
      SetFont([], clRed)
    else if HasText(Text, ['fatal: ']) then // do not localize
      SetFont([fsBold], clRed)
    else
    begin
      LText := TrimLeft(Text);
      if StartsWith(LText, 'MAKE version', True) or // do not localize
         StartsWith(LText, 'Borland ', True) or // do not localize
         StartsWith(LText, 'Copyright ', True) then // do not localize
      begin
        SetFont([fsItalic]);
      end
    end;
  end;
end;

procedure TFrameInstall.EvIdle(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TFrameInstall.RichEditLogSelectionChange(Sender: TObject);
var
  S: string;
  ps: Integer;
  i: Integer;
  ShowDirLbl: Boolean;
begin
  if not FFinished then
    Exit;
  if (RichEditLog.CaretPos.Y > 0) and (RichEditLog.CaretPos.Y < RichEditLog.Lines.Count) then
  begin
    S := RichEditLog.Lines[RichEditLog.CaretPos.Y];

   // find the directory
    if StartsWith(S, Installer.JVCLDir + '\') then
      ShowDirLbl := True
    else
    begin
      ShowDirLbl := False;
      for i := 0 to Installer.Data.Targets.Count - 1 do
        if StartsWith(S, Installer.Data.TargetConfig[i].JCLDir + '\') then
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
          LblOpenFile.Caption := MinimizeName(LblOpenFile.Hint, LblOpenFile.Canvas, LblOpenFile.Width);
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

procedure TFrameInstall.Init;
begin
  FInitializing := True;
  try
    RichEditLog.Visible := False;
    LblOpenFile.Visible := False;
    BtnDetails.Visible := True;

    LblTarget.Caption := '';
    LblTarget.Hint := '';
    ProgressBarTarget.Max := 100;
    ProgressBarTarget.Position := 0;

    LblInfo.Caption := '';
    ProgressBarCompile.Max := 100;
    ProgressBarCompile.Position := 0;
  finally
    FInitializing := False;
  end;
end;

procedure TFrameInstall.Execute;
var
  Success: Boolean;
  i: Integer;
  AbortReason: string;
begin
  Aborted := False;

  Installer.Data.SaveTargetConfigs;
  FPositionTarget := 0;
  FPositionProject := 0;

  FFinished := False;

  Compiler := TJVCLCompiler.Create(Installer.Data);
  try
    Compiler.OnProgress := EvProgress;
    Compiler.OnCaptureLine := EvCaptureLine;
    Compiler.OnIdle := EvIdle;
    Success := Compiler.Compile;
    AbortReason := Compiler.AbortReason;
  finally
    Compiler.Free;
  end;

  if Success then
  begin
    // register packages
    with Installer do
      for i := 0 to SelTargetCount - 1 do
        if SelTargets[i].InstallJVCL and (not SelTargets[i].CompileOnly) then
        begin
          if SelTargets[i].CleanPalettes then
            SelTargets[i].CleanJVCLPalette(False);
          Success := SelTargets[i].RegisterToIDE;
          if not Success then
            Break;
        end;
  end;

  if not Success then
  begin
    LblTarget.Caption := Format(RsError, [LblTarget.Hint]);
    BtnDetails.Visible := False;
    RichEditLog.Visible := True;
    if AbortReason <> '' then
      AbortReason := RsInstallError + #10#10 + AbortReason
    else
      AbortReason := RsInstallError;
    MessageDlg(AbortReason, mtError, [mbOk], 0);
  end
  else
    LblTarget.Caption := RsComplete;

  FFinished := True;
  if Success then
    Installer.PackageInstaller.ForcedFinish; // this is the last page so we want the installer to show the finished state
end;

procedure TFrameInstall.BtnDetailsClick(Sender: TObject);
begin
  BtnDetails.Visible := False;
  RichEditLog.Visible := True;
  RichEditLog.SetFocus;
end;

procedure TFrameInstall.LblOpenFileClick(Sender: TObject);
begin
  if ShellExecute(Handle, 'open', PChar(LblOpenFile.Hint), nil, nil, SW_SHOW) < 32 then // do not localize
    MessageDlg(RsErrorOpeningFile, mtError, [mbOk], 0);
end;

end.
