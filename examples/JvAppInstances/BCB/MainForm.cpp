/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones@meloo.com)

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

******************************************************************/
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvAppInst"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  Caption = "Pid: " + IntToStr(GetCurrentProcessId());
  CheckBoxActive->Checked = JvAppInstances->Active;
  MaxInstances->Caption = Format("%d", ARRAYOFCONST(((int)(JvAppInstances->MaxInstances))));
  RunningInstances->Caption = Format("%d", ARRAYOFCONST(((int)(JvAppInstances->AppInstances->InstanceCount))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::BtnQuitClick(TObject *Sender)
{
  Close();  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::CheckBoxActiveClick(TObject *Sender)
{
  JvAppInstances->Active = CheckBoxActive->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvAppInstancesCmdLineReceived(TObject *Sender,
      TStrings *CmdLine)
{
  MemoCmdLine->Lines->Assign(CmdLine);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvAppInstancesInstanceCreated(TObject *Sender,
      DWORD ProcessId)
{
  MemoLog->Lines->Add("Instance created (Pid: " + IntToStr(ProcessId) + ")");
  MaxInstances->Caption = Format("%d", ARRAYOFCONST(((int)(JvAppInstances->MaxInstances))));
  RunningInstances->Caption = Format("%d", ARRAYOFCONST(((int)(JvAppInstances->AppInstances->InstanceCount))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvAppInstancesInstanceDestroyed(TObject *Sender,
      DWORD ProcessId)
{
  MemoLog->Lines->Add("Instance destroyed (Pid: " + IntToStr(ProcessId) + ")");
  MaxInstances->Caption = Format("%d", ARRAYOFCONST(((int)(JvAppInstances->MaxInstances))));
  RunningInstances->Caption = Format("%d", ARRAYOFCONST(((int)(JvAppInstances->AppInstances->InstanceCount))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvAppInstancesRejected(TObject *Sender)
{
  ShowMessage("I was rejected (Pid: " + IntToStr(GetCurrentProcessId()) + ").\n" +
    "Now I call UserNotify(100)");
  JvAppInstances->UserNotify(100);
  MaxInstances->Caption = Format("%d", ARRAYOFCONST(((int)(JvAppInstances->MaxInstances))));
  RunningInstances->Caption = Format("%d", ARRAYOFCONST(((int)(JvAppInstances->AppInstances->InstanceCount))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvAppInstancesUserNotify(TObject *Sender,
      int Param)
{
  MemoLog->Lines->Add("UserNotify: " + IntToStr(Param));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button1Click(TObject *Sender)
{
  TProcessInformation ProcessInfo;
  TStartupInfo StartupInfo;

  StartupInfo.cb = sizeof(StartupInfo);
  GetStartupInfo(&StartupInfo);

  if (CreateProcess(NULL, (AnsiString(ParamStr(0)) + AnsiString(" \"1st Argument\" /second /third")).c_str(),
    NULL, NULL, false, 0, NULL, NULL, &StartupInfo, &ProcessInfo))
  {
    // we do not need them
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  }
  else
    MessageDlg("Error starting new process instance.", mtError, TMsgDlgButtons() << mbOK, 0);

}
//---------------------------------------------------------------------------
