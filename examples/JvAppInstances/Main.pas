{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

{$I jvcl.inc}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvAppInst, Mask, JvMaskEdit, JvSpin;

type
  TFormMain = class(TForm)
    MemoCmdLine: TMemo;
    MemoLog: TMemo;
    BtnQuit: TButton;
    JvAppInstances: TJvAppInstances;
    Button1: TButton;
    CheckBoxActive: TCheckBox;
    BtnInstanceCount: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnQuitClick(Sender: TObject);
    procedure CheckBoxActiveClick(Sender: TObject);
    procedure EvCmdLineReceived(Sender: TObject;
      CmdLine: TStrings);
    procedure EvInstanceCreated(Sender: TObject;
      ProcessId: Cardinal);
    procedure EvInstanceDestroyed(Sender: TObject;
      ProcessId: Cardinal);
    procedure JvAppInstancesRejected(Sender: TObject);
    procedure JvAppInstancesUserNotify(Sender: TObject; Param: Integer);
    procedure Button1Click(Sender: TObject);
    procedure BtnInstanceCountClick(Sender: TObject);
  private
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := 'pid: ' + IntToStr(GetCurrentProcessId);
  CheckBoxActive.Checked := JvAppInstances.Active;
end;

procedure TFormMain.BtnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.CheckBoxActiveClick(Sender: TObject);
begin
  JvAppInstances.Active := CheckBoxActive.Checked;
end;

procedure TFormMain.EvCmdLineReceived(Sender: TObject; CmdLine: TStrings);
begin
  MemoCmdLine.Lines.Assign(CmdLine);
end;

procedure TFormMain.EvInstanceCreated(Sender: TObject;
  ProcessId: Cardinal);
begin
  MemoLog.Lines.Add('Instance created (pid: ' + IntToStr(ProcessId) + ')');
end;

procedure TFormMain.EvInstanceDestroyed(Sender: TObject;
  ProcessId: Cardinal);
begin
  MemoLog.Lines.Add('Instance destroyed (pid: ' + IntToStr(ProcessId) + ')');
end;

procedure TFormMain.JvAppInstancesRejected(Sender: TObject);
begin
  ShowMessage('I was rejected (pid: ' + IntToStr(GetCurrentProcessId) + ').'#10 +
              'Now I call UserNotify(100)');
  JvAppInstances.UserNotify(100);
end;

procedure TFormMain.JvAppInstancesUserNotify(Sender: TObject;
  Param: Integer);
begin
  MemoLog.Lines.Add('UserNotify: ' + IntToStr(Param));
end;

procedure TFormMain.Button1Click(Sender: TObject);
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  StartupInfo.cb := SizeOf(StartupInfo);
  GetStartupInfo(StartupInfo);

  if CreateProcess(nil, PChar(ParamStr(0) + ' "1st Argument" /second /third'),
    nil, nil, False, 0, nil, nil, StartupInfo, ProcessInfo) then
  begin
   // we do not need them
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    MessageDlg('Error starting new process instance.', mtError, [mbOk], 0);
end;

procedure TFormMain.BtnInstanceCountClick(Sender: TObject);
begin
  ShowMessage('Active instances: ' + IntToStr(JvAppInstances.AppInstances.InstanceCount));
end;

end.
