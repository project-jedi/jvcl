{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmCompile.pas, released on 2004-12-13.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit FrmCompile;

{$I jvcl.inc}

interface

uses
  JVCLData,
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  Consts;

type
  TCompileLineType = (clText, clFileProgress, clHint, clWarning, clError, clFatal);

  ICompileMessages = interface
    ['{C932390B-8DB6-4CAE-89D0-7BAB8A2E640B}']
    procedure Clear;

    procedure AddHint(const Text: string);
    procedure AddWarning(const Text: string);
    procedure AddError(const Text: string);
    procedure AddFatal(const Text: string);
    procedure AddText(const Msg: string);

    procedure SetCurrentTarget(ATarget: TTargetConfig);

      { Text is the line that the compiler outputs. The ICompileMessages
        implementor must parse the line itself. }
  end;

  TFormCompile = class(TForm)
    PanelClient: TPanel;
    BtnOk: TButton;
    BevelProject: TBevel;
    BevelStatus: TBevel;
    BevelCurrentLine: TBevel;
    BevelHints: TBevel;
    LblProject: TLabel;
    LblStatusCaption: TLabel;
    BevelTotalLines: TBevel;
    LblCurrentLineCaption: TLabel;
    LblCurrentLine: TLabel;
    LblTotalLinesCaption: TLabel;
    LblTotalLines: TLabel;
    BevelWarnings: TBevel;
    BevelErrors: TBevel;
    LblHintsCaption: TLabel;
    LblHints: TLabel;
    LblWarningsCaption: TLabel;
    LblWarnings: TLabel;
    LblErrorsCaption: TLabel;
    LblErrors: TLabel;
    LblProjectCaption: TLabel;
    LblStatus: TLabel;
    LblErrorReason: TLabel;
    procedure BtnOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FAborted: Boolean;
    FHints: Cardinal;
    FWarnings: Cardinal;
    FErrors: Cardinal;
    FCurrentLine: Cardinal;
    FTotalLines: Cardinal;
    FCurFilename: string;
    FCompileMessages: ICompileMessages;
    FAutoClearCompileMessages: Boolean;
    FCurrentTarget: TTargetConfig;
    procedure SetCurrentLine(Line: Cardinal);
    function IsCompileFileLine(const Line: string): Boolean;
  public
    procedure Init(const ProjectName: string; Clear: Boolean = True);
    procedure Compiling(const Filename: string);
    procedure Linking(const Filename: string);
    procedure Done(const ErrorReason: string = '');
    procedure SetCurrentTarget(ATarget: TTargetConfig);

    function HandleLine(const Line: string): TCompileLineType;

    procedure BringToFront;

    procedure IncHint;
    procedure IncWarning;
    procedure IncError;

    property Aborted: Boolean read FAborted;

    property Hints: Cardinal read FHints;
    property Warnings: Cardinal read FWarnings;
    property Errors: Cardinal read FErrors;
    property CurrentLine: Cardinal read FCurrentLine write SetCurrentLine;

    property AutoClearCompileMessages: Boolean read FAutoClearCompileMessages write FAutoClearCompileMessages default False;
    property CompileMessages: ICompileMessages read FCompileMessages write FCompileMessages;
  end;

var
  FormCompile: TFormCompile;

implementation

{$IFDEF MSWINDOWS}
{$I windowsonly.inc}
uses
  CmdLineUtils, FileCtrl;
{$ELSE}
uses
  CmdLineUtils;
{$ENDIF MSWINDOWS}

{$R *.dfm}

resourcestring
  RsPreparing = 'Preparing...';
  RsCompiling = 'Compiling';
  RsLinking = 'Linking';
  RsDone = 'Done';
  RsCompileAborted = 'Aborted.';
  RsThereAreErrors = 'There are errors.';
  RsThereAreWarnings = 'There are warnings.';
  RsThereAreHints = 'There are hints.';
  RsCompiled = 'compiled.';

{ TFormCompile }

procedure TFormCompile.BringToFront;
begin
  if Visible and Application.Active then
    SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
end;

procedure TFormCompile.BtnOkClick(Sender: TObject);
begin
  if BtnOk.Caption = SOKButton then
  begin
    Tag := 1;
    Close;
  end
  else
    FAborted := True;
end;

function TFormCompile.HandleLine(const Line: string): TCompileLineType;

  function HasText(Text: string; const Values: array of string): Boolean;
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

begin
  Result := clText;
  if Line = '' then
    Exit;

  if Assigned(FCompileMessages) then
    FCompileMessages.SetCurrentTarget(FCurrentTarget);

  if IsCompileFileLine(Line) then
    Result := clFileProgress
  else
  if HasText(Line, ['hint: ', 'hinweis: ', 'suggestion: ']) then // do not localize
  begin
    Result := clHint;
    IncHint;
    if Assigned(FCompileMessages) then
      FCompileMessages.AddHint(Line);
  end
  else if HasText(Line, ['warning: ', 'warnung: ', 'avertissement: ']) then // do not localize
  begin
    Result := clWarning;
    IncWarning;
    if Assigned(FCompileMessages) then
      FCompileMessages.AddWarning(Line);
  end
  else if HasText(Line, ['error: ', 'fehler: ', 'erreur: ']) then // do not localize
  begin
    Result := clError;
    IncError;
    if Assigned(FCompileMessages) then
      FCompileMessages.AddError(Line);
  end
  else if HasText(Line, ['fatal: ', 'schwerwiegend: ', 'fatale: ']) then // do not localize
  begin
    Result := clFatal;
    IncError;
    if Assigned(FCompileMessages) then
      FCompileMessages.AddFatal(Line);
  end;
end;

function TFormCompile.IsCompileFileLine(const Line: string): Boolean;

  function PosLast(Ch: Char; const S: string): Integer;
  begin
    for Result := Length(S) downto 1 do
      if S[Result] = Ch then
        Exit;
    Result := 0;
  end;

var
  ps, psEnd, LineNum, Err: Integer;
  Filename: string;
begin
  Result := False;
  ps := PosLast('(', Line);
  if (ps > 0) and (Pos(': ', Line) = 0) and (Pos('.', Line) > 0) then
  begin
    psEnd := PosLast(')', Line);
    if psEnd < ps then
      Exit;

    Filename := Copy(Line, 1, ps - 1);
    if (Filename <> '') and (Filename[Length(Filename)] > #32) then
    begin
      Val(Copy(Line, ps + 1, psEnd - ps - 1), LineNum, Err);
      if Err = 0 then
      begin
        Compiling(Filename);
        CurrentLine := LineNum;
        Result := True;
      end;
    end;
  end;
end;

procedure TFormCompile.Init(const ProjectName: string; Clear: Boolean);
begin
  Tag := 0;
  FAborted := False;
  LblProject.Caption := MinimizeName(ProjectName, LblProject.Canvas, LblProject.ClientWidth);

  LblStatusCaption.Font.Style := [];
  LblStatus.Font.Style := [];

  if Clear then
  begin
    if Assigned(FCompileMessages) and AutoClearCompileMessages then
      FCompileMessages.Clear;
    FHints := 0;
    FErrors := 0;
    FWarnings := 0;
    FTotalLines := 0;
  end;
  FCurrentLine := 0;
  FCurFilename := '';

  LblHints.Caption := IntToStr(FHints);
  LblWarnings.Caption := IntToStr(FWarnings);
  LblErrors.Caption := IntToStr(FErrors);
  LblCurrentLine.Caption := IntToStr(FCurrentLine);
  LblTotalLines.Caption := IntToStr(FTotalLines);
  LblStatusCaption.Caption := RsPreparing;
  LblStatus.Caption := '';

  BtnOk.Enabled := True;
  BtnOk.Caption := SCancelButton;
  if not Visible then
    Show
  else
    BringToFront;
end;

procedure TFormCompile.Compiling(const Filename: string);
begin
  if Filename <> FCurFilename then
  begin
    FCurFilename := Filename;
    FTotalLines := FTotalLines + FCurrentLine;
    CurrentLine := 0; // updates total lines and current lines
    LblStatusCaption.Font.Style := [];
    LblStatus.Font.Style := [];
    LblStatusCaption.Caption := RsCompiling + ':';
    LblStatus.Caption := ExtractFileName(Filename);
    BringToFront;
    Application.ProcessMessages;
  end;
end;

procedure TFormCompile.Linking(const Filename: string);
begin
  FTotalLines := FTotalLines + FCurrentLine;
  CurrentLine := 0;

  LblStatusCaption.Font.Style := [];
  LblStatus.Font.Style := [];
  LblStatusCaption.Caption := RsLinking + ':';
  LblStatus.Caption := ExtractFileName(Filename);
  BringToFront;
  Application.ProcessMessages;
end;

procedure TFormCompile.Done(const ErrorReason: string);
begin
  FCurFilename := '';
  FTotalLines := FTotalLines + FCurrentLine;
  CurrentLine := 0;

  LblErrorReason.Caption := ErrorReason;
  LblErrorReason.Visible := ErrorReason <> '';
  LblStatusCaption.Font.Style := [fsBold];
  LblStatus.Font.Style := [fsBold];
  LblStatusCaption.Caption := RsDone + ':';

  if Aborted then
    LblStatus.Caption := RsCompileAborted
  else if FErrors > 0 then
    LblStatus.Caption := RsThereAreErrors
  else if FWarnings > 0 then
    LblStatus.Caption := RsThereAreWarnings
  else if FHints > 0 then
    LblStatus.Caption := RsThereAreHints
  else
    LblStatus.Caption := RsCompiled;
  //BtnOk.Enabled := ErrorReason <> '';
  BtnOk.Caption := SOKButton;
  if ((ErrorReason <> '') or (LblStatus.Caption = RsCompileAborted)) and not CmdOptions.ContinueOnError then
  begin
    Hide;
    ShowModal;
  end;
end;

procedure TFormCompile.IncError;
begin
  Inc(FErrors);
  LblErrors.Caption := IntToStr(FErrors);
  Application.ProcessMessages;
end;

procedure TFormCompile.IncHint;
begin
  Inc(FHints);
  LblHints.Caption := IntToStr(FHints);
  Application.ProcessMessages;
end;

procedure TFormCompile.IncWarning;
begin
  Inc(FWarnings);
  LblWarnings.Caption := IntToStr(FWarnings);
  Application.ProcessMessages;
end;

procedure TFormCompile.SetCurrentLine(Line: Cardinal);
begin
  FCurrentLine := Line;
  LblCurrentLine.Caption := IntToStr(Line);
  LblTotalLines.Caption := IntToStr(FTotalLines + FCurrentLine);
  BringToFront;
  Application.ProcessMessages;
end;

procedure TFormCompile.SetCurrentTarget(ATarget: TTargetConfig);
begin
  FCurrentTarget := ATarget;
end;

procedure TFormCompile.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Tag = 1;
end;

end.
