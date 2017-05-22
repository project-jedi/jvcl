/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones att altern dott org)

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

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvAppInst.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *RunningInstances;
  TMemo *MemoLog;
  TMemo *MemoCmdLine;
  TLabel *MaxInstances;
  TLabel *Label2;
  TLabel *Label1;
  TJvAppInstances *JvAppInstances;
  TCheckBox *CheckBoxActive;
  TButton *Button1;
  TButton *BtnQuit;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall BtnQuitClick(TObject *Sender);
  void __fastcall CheckBoxActiveClick(TObject *Sender);
  void __fastcall JvAppInstancesCmdLineReceived(TObject *Sender,
          TStrings *CmdLine);
  void __fastcall JvAppInstancesInstanceCreated(TObject *Sender,
          DWORD ProcessId);
  void __fastcall JvAppInstancesInstanceDestroyed(TObject *Sender,
          DWORD ProcessId);
  void __fastcall JvAppInstancesRejected(TObject *Sender);
  void __fastcall JvAppInstancesUserNotify(TObject *Sender, int Param);
  void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
