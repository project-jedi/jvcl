{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Main.pas, released on 2003-10-04.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-10-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit Main;
interface
uses
  QWindows, QMessages, SysUtils, Variants, Classes, Types, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QComCtrls, Parser, QFileCtrls;

type
  TFormMain = class(TForm)
    BtnParse: TButton;
    ProgressBar: TProgressBar;
    LblStatus: TLabel;
    CheckBoxSingleResFile: TCheckBox;
    BtnQuit: TButton;
    LblDir: TLabel;
    EditDirectory: TEdit;
    BtnBrowse: TButton;
    CheckBoxSilent: TCheckBox;
    CheckBoxSubDirs: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnParseClick(Sender: TObject);
    procedure BtnQuitClick(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
  private
    procedure DoProgress(Sender: TObject; const Text: string; Percentage: Integer);
    procedure DoGetResName(Sender: TObject; var Name: string; const Value: string);
    procedure ParseCmdLine;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.xfm}

procedure TFormMain.ParseCmdLine;
var
  i: Integer;
  S: string;
begin
  CheckBoxSubDirs.Checked := False;
  CheckBoxSilent.Checked := False;
  CheckBoxSingleResFile.Checked := False;

  for i := 1 to ParamCount - 1 do
  begin
    S := ParamStr(i);
    if S[1] = '-' then
    begin
      case UpCase(S[2]) of
        'S': CheckBoxSubDirs.Checked := True;
        'Q': CheckBoxSilent.Checked := True;
        '1': CheckBoxSingleResFile.Checked := True;
      end;
    end
    else if DirectoryExists(S) then
      EditDirectory.Text := S;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  LblStatus.Caption := '';

  ParseCmdLine;
  if CheckBoxSilent.Checked then
  begin
   // silent mode
    Application.ShowMainForm := False;
    BtnParse.Click;
    Application.Terminate;
  end;
end;

procedure TFormMain.BtnParseClick(Sender: TObject);
var
  Parser: TParser;
begin
  if not DirectoryExists(EditDirectory.Text) then
  begin
    MessageDlg('Directory does not exist', mtError, [mbOK], 0);
    Exit;
  end;
  Parser := TParser.Create;
  BtnParse.Enabled := False;
  try
    Parser.OnProgress := DoProgress;
    Parser.OnGetResName := DoGetResName;
    Parser.SingleResFile := CheckBoxSingleResFile.Checked;
    Parser.SubDirs := CheckBoxSubDirs.Checked;
    Parser.ParseFiles(EditDirectory.Text);
  finally
    BtnParse.Enabled := True;
    Parser.Free;
  end;
end;

procedure TFormMain.DoGetResName(Sender: TObject; var Name: string;
  const Value: string);
begin
  if CheckBoxSilent.Checked then
    Exit;

  if not InputQuery('Resourcestring Name', Value, Name) then
    Name := '';
end;

procedure TFormMain.DoProgress(Sender: TObject; const Text: string;
  Percentage: Integer);
begin
  ProgressBar.Position := Percentage;
  LblStatus.Caption := Text;
  Application.ProcessMessages;
end;

procedure TFormMain.BtnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.BtnBrowseClick(Sender: TObject);
var Dir: widestring;
begin
  Dir := EditDirectory.Text;
  if SelectDirectory('Select the source directory', '', Dir) then
    EditDirectory.Text := Dir;
end;

end.
