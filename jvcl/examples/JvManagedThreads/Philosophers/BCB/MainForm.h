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
// $Id$
//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvMTComponents.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>

enum TPhilosopherState {psThinking, psHungry, psEating};

class TPerson : public TObject
{
public:
  int Nr;
  TPerson(int ANumber):Nr(ANumber){;};
};

class TMyMsg : public TObject
{
public:
  AnsiString Msg;
  TMyMsg(AnsiString AMsg):Msg(AMsg){;};
};

//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *PhilLabel1;
  TLabel *PhilLabel2;
  TLabel *PhilLabel3;
  TLabel *PhilLabel4;
  TLabel *PhilLabel5;
  TShape *Shape1;
  TLabel *LblSpeed;
  TMemo *Memo;
  TButton *BtnStart;
  TButton *BtnTerminate;
  TTrackBar *SpeedBar;
  TButton *BtnClose;
  TJvMTManager *PhilosopherManager;
  TJvMTThread *PhilosopherThread;
  TJvMTMonitorSection *MonitorSection;
  TJvMTVCLToThread *PersonBuffer;
  TJvMTThreadToVCL *MsgToVCL;
  void __fastcall BtnStartClick(TObject *Sender);
  void __fastcall BtnTerminateClick(TObject *Sender);
  void __fastcall PersonBufferCanWrite(TObject *Sender);
  void __fastcall MsgToVCLCanRead(TObject *Sender);
  void __fastcall SpeedBarChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall BtnCloseClick(TObject *Sender);
  void __fastcall PhilosopherThreadExecute(TJvMTThread *Sender,
          TJvMTSingleThread *MTThread);
private:	// User declarations
  int FSpeed;
  int FNrCycle;
  TPhilosopherState FState[5];
  void __fastcall SetPhilLabelsEnabled(bool Value);
  void __fastcall TerminatePhilosophers();
  void __fastcall ProcessMsg(AnsiString M);
  void __fastcall WaitRandom();

  void __fastcall Msg(AnsiString S);
  void __fastcall Test(int Nr);
  void __fastcall PickupChopsticks(int Nr);
  void __fastcall PutdownChopsticks(int Nr);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
