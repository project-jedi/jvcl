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

unit CreateProcessExampleMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, JvComponent, JvCreateProcess;

type
  TCreateProcessExampleMainForm = class(TForm)
    Panel1: TPanel;
    RichEdit1: TRichEdit;
    RunBtn: TButton;
    QuitBtn: TButton;
    CloseBtn: TButton;
    TerminateBtn: TButton;
    JvCreateProcess1: TJvCreateProcess;
    StopBtn: TButton;
    procedure RunBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure TerminateBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvCreateProcess1Terminate(Sender: TObject; ExitCode: Cardinal);
    procedure StopBtnClick(Sender: TObject);
  public
    procedure AddLogMsg(const Text: string);
    procedure UpdateButtons;
  end;

var
  CreateProcessExampleMainForm: TCreateProcessExampleMainForm;

implementation

{$R *.DFM}

resourcestring
  sProcessStarted = 'Process "%s" started...';
  sProcessTerminated = 'Process "%s" terminated, ExitCode: %.8x';

procedure TCreateProcessExampleMainForm.AddLogMsg(const Text: string);
begin
  RichEdit1.Lines.Add(FormatDateTime('DD.MM.YYYY HH:NN:SS  ', Now) + Text);
end;

procedure TCreateProcessExampleMainForm.RunBtnClick(Sender: TObject);
begin
  JvCreateProcess1.Run;
  UpdateButtons;
  AddLogMsg(Format(sProcessStarted, [JvCreateProcess1.CommandLine]));
end;

procedure TCreateProcessExampleMainForm.CloseBtnClick(Sender: TObject);
begin
  JvCreateProcess1.CloseApplication(False);
end;

procedure TCreateProcessExampleMainForm.QuitBtnClick(Sender: TObject);
begin
  JvCreateProcess1.CloseApplication(True);
end;

procedure TCreateProcessExampleMainForm.TerminateBtnClick(Sender: TObject);
begin
  JvCreateProcess1.Terminate;
end;

procedure TCreateProcessExampleMainForm.UpdateButtons;
begin
  RunBtn.Enabled := JvCreateProcess1.State = psReady;
  CloseBtn.Enabled := JvCreateProcess1.State <> psReady;
  QuitBtn.Enabled := JvCreateProcess1.State <> psReady;
  TerminateBtn.Enabled := JvCreateProcess1.State <> psReady;
  StopBtn.Enabled := JvCreateProcess1.State <> psReady;
end;

procedure TCreateProcessExampleMainForm.FormCreate(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TCreateProcessExampleMainForm.JvCreateProcess1Terminate(Sender: TObject; ExitCode: Cardinal);
begin
  UpdateButtons;
  AddLogMsg(Format(sProcessTerminated, [JvCreateProcess1.CommandLine, ExitCode]));
end;

procedure TCreateProcessExampleMainForm.StopBtnClick(Sender: TObject);
begin
  JvCreateProcess1.StopWaiting;
  UpdateButtons;
end;

end.

