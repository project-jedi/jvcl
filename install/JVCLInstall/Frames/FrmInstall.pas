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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

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
  Result.FInstaller := Installer;
  Result.Parent := Client;
  Result.Align := alClient;
  Result.Init;
end;

procedure TFrameInstall.EvProgress(Sender: TObject; const Text: string;
  Position, Max: Integer; Kind: TProgressKind);
begin
  case Kind of
    tkTarget:
      begin
        FPositionTarget := Position;

        LblTarget.Caption := Text;
        LblTarget.Hint := Text;
        ProgressBarTarget.Max := Max * ProjectMax;
        ProgressBarTarget.Position := {FPositionProject +} (FPositionTarget * ProjectMax);
      end;

    tkProject:
      begin
        FPositionProject := Position;
        ProgressBarTarget.Position := FPositionProject + (FPositionTarget * ProjectMax);
        LblTarget.Caption := LblTarget.Hint + ' - ' + Text;
        ProgressBarCompile.Position := 0;
      end;

    tkResource,
    tkPackage:
      begin
        if Text <> '' then
          LblInfo.Caption := Format(RsCompiling, [Text])
        else
          LblInfo.Caption := '';
        ProgressBarCompile.Max := Max;
        ProgressBarCompile.Position := Position;
      end;
  end;

  Application.ProcessMessages;
end;

procedure TFrameInstall.EvCaptureLine(const Text: string;
  var Aborted: Boolean);
var
  LText: string;
begin
  Aborted := FAborted;
  RichEditLog.Lines.Add(Text);
  if Text <> '' then
  begin
    if Text[1] = '[' then
    begin
      RichEditLog.SelStart := RichEditLog.SelStart - Length(Text) - 2;
      RichEditLog.SelLength := Length(Text);
      RichEditLog.SelAttributes.Style := [fsBold];
      RichEditLog.SelLength := 0;
    end
    else if HasText(Text, ['hint: ', 'hinweis: ', 'suggestion: ']) then // do not localize
    begin
      RichEditLog.SelStart := RichEditLog.SelStart - Length(Text) - 2;
      RichEditLog.SelLength := Length(Text);
      RichEditLog.SelAttributes.Color := clGreen;
      RichEditLog.SelLength := 0;
    end
    else if HasText(Text, ['warning: ', 'warnung: ', 'avertissement: ']) then // do not localize
    begin
      RichEditLog.SelStart := RichEditLog.SelStart - Length(Text) - 2;
      RichEditLog.SelLength := Length(Text);
      RichEditLog.SelAttributes.Color := clMaroon;
      RichEditLog.SelLength := 0;
    end
    else if HasText(Text, ['error: ', 'fehler: ', 'erreur: ']) then // do not localize
    begin
      RichEditLog.SelStart := RichEditLog.SelStart - Length(Text) - 2;
      RichEditLog.SelLength := Length(Text);
      RichEditLog.SelAttributes.Color := clRed;
      RichEditLog.SelLength := 0;
    end
    else if HasText(Text, ['fatal: ']) then // do not localize
    begin
      RichEditLog.SelStart := RichEditLog.SelStart - Length(Text) - 2;
      RichEditLog.SelLength := Length(Text);
      RichEditLog.SelAttributes.Color := clRed;
      RichEditLog.SelAttributes.Style := [fsBold];
      RichEditLog.SelLength := 0;
    end
    else
    begin
      LText := TrimLeft(Text);
      if StartsWith(LText, 'MAKE version', True) or // do not localize
         StartsWith(LText, 'Borland ', True) or // do not localize
         StartsWith(LText, 'Copyright ', True) then // do not localize
      begin
        RichEditLog.SelStart := RichEditLog.SelStart - Length(Text) - 2;
        RichEditLog.SelLength := Length(Text);
        RichEditLog.SelAttributes.Style := [fsItalic];
        RichEditLog.SelLength := 0;
      end
    end;
  end;
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
    Success := Compiler.Compile;
  finally
    Compiler.Free;
  end;

  if Success then
  begin
    // register packages to the IDEs
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
    MessageDlg(RsCompileError, mtError, [mbOk], 0);
  end
  else
    LblTarget.Caption := RsComplete;

  FFinished := True;
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
