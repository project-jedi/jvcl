{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit JvQLogFileMainFormU;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms,
  QDialogs, JvQComponent, JvQLogFile, QStdCtrls;

type
  TJvLogFileMainForm = class(TForm)
    JvLogFile1: TJvLogFile;
    btnStart: TButton;
    btnShow: TButton;
    lblActive: TLabel;
    btnReset: TButton;
    lblInactive: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure btnResetClick(Sender: TObject);
  private
    FLogFileName: string;
    procedure StartLogging;
    procedure StopLogging;
    procedure ResetLogging;
  end;

var
  JvLogFileMainForm: TJvLogFileMainForm;

implementation

{$R *.xfm}

procedure TJvLogFileMainForm.FormCreate(Sender: TObject);
begin
  FLogFileName := ChangeFileExt(Application.ExeName, '.log');
end;

procedure TJvLogFileMainForm.btnStartClick(Sender: TObject);
begin
  if btnStart.Tag = 0 then
    StartLogging
  else
    StopLogging;
end;

procedure TJvLogFileMainForm.btnShowClick(Sender: TObject);
begin
  if btnStart.Tag = 1 then
    btnStart.Click; // stop logging
  JvLogFile1.ShowLog('Mouse Move Log');
end;

procedure TJvLogFileMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if btnStart.Tag = 1 then
    JvLogFile1.Add(DateTimeToStr(Now), 'Mouse Move', Format('X:%d, Y:%d', [X, Y]));
  Caption := Format('JvLogFile Demo - X:%d, Y:%d', [X, Y]);
end;

procedure TJvLogFileMainForm.ResetLogging;
begin
  StopLogging;
  DeleteFile(FLogFileName);
  JvLogFile1.Clear;
end;

procedure TJvLogFileMainForm.StartLogging;
begin
  if FileExists(FLogFileName) then
    JvLogFile1.LoadFromFile(FLogFileName);
  btnStart.Caption := '&Stop';
  btnStart.Tag := 1;
  lblActive.Visible := true;
  lblInactive.Visible := false;
end;

procedure TJvLogFileMainForm.StopLogging;
begin
  btnStart.Tag := 0;
  lblActive.Visible := false;
  lblInactive.Visible := true;
  btnStart.Caption := '&Start';
  JvLogFile1.SaveToFile(FLogFileName);
end;

procedure TJvLogFileMainForm.btnResetClick(Sender: TObject);
begin
  ResetLogging;
end;

end.

