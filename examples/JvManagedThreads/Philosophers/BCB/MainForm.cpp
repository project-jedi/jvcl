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

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvMTComponents"
#pragma resource "*.dfm"
TfrmMain *frmMain;

void __fastcall TfrmMain::SetPhilLabelsEnabled(bool Value)
{
  PhilLabel1->Enabled = Value;
  PhilLabel2->Enabled = Value;
  PhilLabel3->Enabled = Value;
  PhilLabel4->Enabled = Value;
  PhilLabel5->Enabled = Value;
}

void __fastcall TfrmMain::TerminatePhilosophers()
{
  // terminate any philosphers
  PhilosopherManager->TerminateThreads();
  SetPhilLabelsEnabled(false);
}

void __fastcall TfrmMain::ProcessMsg(AnsiString M)
{
  int Nr = StrToInt(M.SubString(1,1))+1;
  TLabel* Lbl = dynamic_cast<TLabel*>(FindComponent("PhilLabel"+IntToStr(Nr)));
  if (Lbl != NULL)
    Lbl->Caption = M.SubString(2,255);
}

void __fastcall TfrmMain::WaitRandom()
{
  randomize();
  for (int I = 0; I <= 5+random(15); I++)
  {
    Sleep(FSpeed);
    CurrentMTThread()->CheckTerminate();
  }
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::BtnStartClick(TObject *Sender)
{
  Memo->Lines->Add("Terminating previous philosophers...");
  TerminatePhilosophers();
  PhilosopherManager->WaitThreads();

  Memo->Lines->Add("Initiating five new philosophers...");

  // reset the states
  for (int I = 0; I <= 4; I++)
    FState[I] = psThinking;

  // make 5 philosophers
  for (int I = 0; I <= 4; I++)
    PhilosopherThread->RunCopy();

  SetPhilLabelsEnabled(True);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::BtnTerminateClick(TObject *Sender)
{
  Memo->Lines->Add("Terminating all philosophers...");
  TerminatePhilosophers();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::PersonBufferCanWrite(TObject *Sender)
{
  PersonBuffer->Write(new TPerson(FNrCycle));
  FNrCycle = (FNrCycle+1) % 5;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::MsgToVCLCanRead(TObject *Sender)
{
  TMyMsg* M = dynamic_cast<TMyMsg*>(MsgToVCL->Read());
  try
  {
    ProcessMsg(M->Msg);
  }
  __finally
  {
    delete M;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SpeedBarChange(TObject *Sender)
{
  FSpeed = SpeedBar->Position;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormShow(TObject *Sender)
{
  FSpeed = SpeedBar->Position;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::BtnCloseClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Msg(AnsiString S)
{
  //OutputDebugString(PChar(S));
  MsgToVCL->Write(new TMyMsg(S));
}

void __fastcall TfrmMain::Test(int Nr)
{
  if ((FState[(Nr+4) % 5] != psEating) && (FState[Nr] == psHungry) &&
    (FState[(Nr+1) % 5] != psEating))
  {
    FState[Nr] = psEating;
    MonitorSection->Condition[Nr]->Signal();
  }
}

void __fastcall TfrmMain::PickupChopsticks(int Nr)
{
  MonitorSection->Enter();
  try
  {
    FState[Nr] = psHungry;
    Test(Nr);
    if (FState[Nr] != psEating)
      MonitorSection->Condition[Nr]->Wait();
  }
  __finally
  {
    MonitorSection->Leave();
  }
}

void __fastcall TfrmMain::PutdownChopsticks(int Nr)
{
  MonitorSection->Enter();
  try
  {
    FState[Nr] = psThinking;
    Test((Nr+4) % 5);
    Test((Nr+1) % 5);
  }
  __finally
  {
    MonitorSection->Leave();
  }
}

void __fastcall TfrmMain::PhilosopherThreadExecute(TJvMTThread *Sender,
      TJvMTSingleThread *MTThread)
{
  OutputDebugString("Philosopher is waiting for personality...");
  TPerson* Person = dynamic_cast<TPerson*>(PersonBuffer->Read());
  try
  {
    Msg(IntToStr(Person->Nr)+" Acquired personality");

    while (true)
    {
      // philosopher is thinking
      Msg(IntToStr(Person->Nr)+" Thinking");
      WaitRandom();

      // philosopher is hungry
      Msg(IntToStr(Person->Nr)+" Pickup chopsticks");
      PickupChopsticks(Person->Nr);

      // philosopher is eating
      Msg(IntToStr(Person->Nr)+" Eating");
      WaitRandom();

      // philosopher is finished eating
      //Msg(IntToStr(Person.Nr)+' Putdown chopsticks');
      PutdownChopsticks(Person->Nr);
    }
  }
  __finally
  {
    delete Person;
  }
}
//---------------------------------------------------------------------------
