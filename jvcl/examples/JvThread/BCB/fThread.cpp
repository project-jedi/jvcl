/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):
   korecek: translation from Delphi to BCB

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
#include <stdlib.h>

#include "fThread.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvThread"
#pragma link "JvThreadDialog"
#pragma link "JvComponentBase"
#pragma link "JvExMask"
#pragma link "JvSpin"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
bool SuspendRandomThread=false;
bool RaiseExceptInThread=false;
//---------------------------------------------------------------------------
typedef struct SJobData
{
 int Dummy[10];
} SJobData;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
 randomize();

 cbExclusive1->Checked=JvThread1->Exclusive;
 cbExclusive2->Checked=JvThread2->Exclusive;

 seMaxCount1->AsInteger=JvThread1->MaxCount;
 seMaxCount2->AsInteger=JvThread2->MaxCount;

 cbDeferredExecution1->Checked=!JvThread1->RunOnCreate;
 cbDeferredExecution2->Checked=!JvThread2->RunOnCreate;

 cbDeferredDeletion1->Checked=!JvThread1->FreeOnTerminate;
 cbDeferredDeletion2->Checked=!JvThread2->FreeOnTerminate;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvThread1Execute(TObject *Sender, Pointer Params)
{
  TJvBaseThread* ThisThread=dynamic_cast<TJvBaseThread*>(Sender);

  if(JvThread1->Terminated) // flag 'Terminated' for current thread; slower then direct ThisThread->Terminated
  { // terminated before resume
   JvThread1->ReturnValue=-1; // for current thread; slower then direct ThisThread->ReturnValue
   exit;
  }

  int i, j, k;
  bool FL_Break=false;

  JvThread1->ReturnValue=0; // for current thread; slower then direct ThisThread->ReturnValue

  //Do the job here
  k = 0;
  for(i=0;(i<1000) && !FL_Break;++i)
  {
    for(j=0;j<10;++j)
    {
      ++k;
      // I included here ::Sleep(0) unlike the original Delphi code to
      // allow the other threads to process as well. (c)VK
      ::Sleep(0);
      //To use global variable/objects, you have to synchronize (to avoid conflicts)
      Form1->ThreadID1 = ThisThread->ThreadID;
      Form1->Value1 = k;
      JvThread1->Synchronize(Form1->Stats1); // slower then direct ThisThread->Synchronize(Form1->Stats1)

      if(SuspendRandomThread)
      {
       SuspendRandomThread=false;
       JvThread1->Suspend(); // itself
      }

      if(RaiseExceptInThread)
      {
       RaiseExceptInThread=false;
       JvThread1->ReturnValue=666; // for current thread; slower then direct ThisThread->ReturnValue
       throw Exception("Exception in Job 1, ThreadID="+IntToHex((int)ThisThread->ThreadID, 8));
      }

      if(JvThread1->Terminated) // flag 'Terminated' for current thread; slower then direct ThisThread->Terminated
      {
        FL_Break = true;
        JvThread1->ReturnValue=1;
        break;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvThread2Execute(TObject *Sender, Pointer Params)
{
// old TJvThread* Container=dynamic_cast<TJvThread*>(Sender);

  TJvBaseThread* ThisThread=dynamic_cast<TJvBaseThread*>(Sender);
  if(ThisThread==NULL)
   throw Exception("Execution error: 'Sender' is not TJvBaseThread object");

  if(ThisThread->Terminated)
  {
   ThisThread->ReturnValue=-1;  // terminated before resume
   return;
  }

  SJobData* Data=reinterpret_cast<SJobData*>(ThisThread->Params); // Data==Params

  TJvThread* Container=dynamic_cast<TJvThread*>(ThisThread->Container);
  if(Container==NULL)
   throw Exception("Execution error: container is not TJvThread component");

  int i, j, k=0;
  //Do the job here
  for(i=0; i<1000 && !ThisThread->Terminated; ++i)
  {
    for(j=0; j<10 && !ThisThread->Terminated; ++j)
    {
      k+=5; //This is the only difference with the other thread
      ::Sleep(Data->Dummy[1]);
      //To use global variable/objects, you have to synchronize (to avoid conflicts)
      Form1->ThreadID2 = ThisThread->ThreadID;
      Form1->Value2=k;
      ThisThread->Synchronize(Form1->Stats2);
// or Container->Synchronize(Form1->Stats2,); // slower then direct ThisThread->Synchronize

      if(SuspendRandomThread)
      {
       SuspendRandomThread=false;
       JvThread2->Suspend(); // itself
      }

      if(RaiseExceptInThread)
      {
       RaiseExceptInThread=false;
       ThisThread->ReturnValue=666;
       throw Exception("Exception in Job 2, ThreadID="+IntToHex((int)ThisThread->ThreadID, 8));
      }
    }
  }

  AnsiString s;
  if(ThisThread->Terminated)
  {
   ThisThread->ReturnValue=1;
   s="Job 2: Terminated";
  }
  else
  {
   ThisThread->ReturnValue=0;
   s="Job 2: Normal finish";
  }

 if(Data->Dummy[0])
  ThisThread->SynchMessageDlg(s+": ID=0x"+IntToHex((int)ThisThread->ThreadID, 8), mtInformation, TMsgDlgButtons()<<mbOK, 0);
// or Container->SynchMessageDlg(s+": ID=0x"+IntToHex((int)ThisThread->ThreadID, 8), mtInformation, TMsgDlgButtons()<<mbOK, 0);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Stats1(void)
{
   lbStats1->Caption = "ID:"+IntToHex(ThreadID1, 8)+ " V:"+IntToStr(Value1);
}

void __fastcall TForm1::Stats2(void)
{
   lbStats2->Caption = "ID:"+IntToHex(ThreadID2, 8)+ " V:"+IntToStr(Value2);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnStartJob1Click(TObject *Sender)
{
  JvThread1->ThreadDialog = NULL;
  TJvBaseThread* t=JvThread1->Execute(NULL);
  if(t)
  {
   int id=t->ThreadID;
   Memo->Lines->Add("Started Job 1: "+IntToHex(id, 8));
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnStartJob1SimpleDlgClick(TObject *Sender)
{
  JvThread1->ThreadDialog = JvThreadSimpleDialog1;
  TJvBaseThread* t=JvThread1->Execute(NULL);
  if(t)
  {
   int id=t->ThreadID;
   Memo->Lines->Add("Started Job 1 with Simple Dlg: "+IntToHex(id, 8));
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnStartJob2Click(TObject *Sender)
{
  JvThread2->ThreadDialog = NULL;
  SJobData* Data=new SJobData;
  Data->Dummy[0]=cbShowMsgBeforeExit2->Checked;
  Data->Dummy[1]=rand()%10;
  TJvBaseThread* t=JvThread2->Execute(Data);
  if(t)
  {
   int id=t->ThreadID;
   Memo->Lines->Add("Started Job 2: "+IntToHex(id, 8));
  }
  else
   delete Data; // thread was not started
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnStartJob2AnimatedDlgClick(TObject *Sender)
{
  JvThread2->ThreadDialog = JvThreadAnimateDialog1;
  SJobData* Data=new SJobData;
  Data->Dummy[0]=cbShowMsgBeforeExit2->Checked;
  Data->Dummy[1]=rand()%10;
  TJvBaseThread* t=JvThread2->Execute(Data);
  if(t)
  {
   int id=t->ThreadID;
   Memo->Lines->Add("Started Job 2 with Animated Dlg: "+IntToHex(id, 8));
  }
  else
   delete Data; // thread was not started
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbExclusive1Click(TObject *Sender)
{
 JvThread1->Exclusive=cbExclusive1->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbExclusive2Click(TObject *Sender)
{
 JvThread2->Exclusive=cbExclusive2->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbDeferredExecution1Click(TObject *Sender)
{
 JvThread1->RunOnCreate=!cbDeferredExecution1->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbDeferredExecution2Click(TObject *Sender)
{
 JvThread2->RunOnCreate=!cbDeferredExecution2->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbDeferredDeletion1Click(TObject *Sender)
{
  JvThread1->FreeOnTerminate=!cbDeferredDeletion1->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbDeferredDeletion2Click(TObject *Sender)
{
  JvThread2->FreeOnTerminate=!cbDeferredDeletion2->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::seMaxCount1Change(TObject *Sender)
{
  JvThread1->MaxCount=seMaxCount1->AsInteger;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::seMaxCount2Change(TObject *Sender)
{
  JvThread2->MaxCount=seMaxCount2->AsInteger;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbPriority1Change(TObject *Sender)
{
 int p=cbPriority1->ItemIndex;
 if(p>=0)
 {
  int old=JvThread1->Priority;
  JvThread1->SetPriority((TThreadPriority)p); // all threads
  if(p>old) Memo->Lines->Add("Job 1 priority boosted");
  if(p<old) Memo->Lines->Add("Job 1 priority reduced");
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbPriority2Change(TObject *Sender)
{
 int p=cbPriority2->ItemIndex;
 if(p>=0)
 {
  int old=JvThread2->Priority;
  JvThread2->SetPriority((TThreadPriority)p); // all threads
  if(p>old) Memo->Lines->Add("Job 2 priority boosted");
  if(p<old) Memo->Lines->Add("Job 2 priority reduced");
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSuspendAll1Click(TObject *Sender)
{
 cbAutoStart1->Checked=false;
 JvThread1->Suspend(); // all threads
 Memo->Lines->Add("Job 1 suspended");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSuspendAll2Click(TObject *Sender)
{
 cbAutoStart2->Checked=false;
 JvThread2->Suspend(); // all threads
 Memo->Lines->Add("Job 2 suspended");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnResumeAll1Click(TObject *Sender)
{
 JvThread1->Resume(); // all threads
 Memo->Lines->Add("Job 1 resumed");
 cbAutoStart1->Enabled=true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnResumeAll2Click(TObject *Sender)
{
 JvThread2->Resume(); // all threads
 Memo->Lines->Add("Job 2 resumed");
 cbAutoStart2->Enabled=true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnTerminate1Click(TObject *Sender)
{
 cbAutoStart1->Checked=false;
 JvThread1->Terminate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnTerminate2Click(TObject *Sender)
{
 cbAutoStart2->Checked=false;
 JvThread2->Terminate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  Memo->Lines->Add("Application shutdown");

  cbAutoStart1->Checked=false;
  cbAutoStart2->Checked=false;

  JvThread1->Terminate();
  JvThread2->Terminate();

  Memo->Lines->Add("Waiting for Job1 completion ...");
  JvThread1->WaitFor();
  Memo->Lines->Add("Done");

  Memo->Lines->Add("Waiting for Job2 completion ...");
  JvThread2->WaitFor();
  Memo->Lines->Add("Done");

  if(JvThread1->Count)
  {
   Memo->Lines->Add("Removing zombies in Job 1 ...");
   JvThread1->RemoveZombie();
   Memo->Lines->Add("Done");
  }

  if(JvThread2->Count)
  {
   Memo->Lines->Add("Removing zombies in Job 2 ...");
   JvThread2->RemoveZombie();
   Memo->Lines->Add("Done");
  }

  Memo->Lines->Add("Finished");
  ::Sleep(2000);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvThreadBegin(TObject *Sender)
{
 if(rbExOnBegin->Checked)
 {
  cbAutoStart1->Checked=false;
  cbAutoStart2->Checked=false;
  throw Exception("OnBegin");
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvThreadBeforeResume(TObject *Sender)
{
 if(rbExOnBeforeResume->Checked)
  throw Exception("OnBeforeResume");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvThread1Finish(TObject *Sender)
{
 TJvBaseThread* t=dynamic_cast<TJvBaseThread*>(Sender);
 if(t)
 {
  int id=t->ThreadID;
  int rv=t->ReturnValue;
  TJvThread* Container=dynamic_cast<TJvThread*>(t->Container);

  Memo->Lines->Add("Job 1 Finished: "+IntToHex(id, 8)+" result: "+IntToStr(rv));

  if(rbExOnFinish->Checked)
  {
   cbAutoStart2->Checked=false; // for preventing endless exception generation
   throw Exception("OnFinish");
  }
 }
 else
 {
  Memo->Lines->Add("Job 1 Finished: unknown object 'Sender'");
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvThread2Finish(TObject *Sender)
{
 TJvBaseThread* t=dynamic_cast<TJvBaseThread*>(Sender);
 if(t)
 {
  int id=t->ThreadID;
  int rv=t->ReturnValue;
  TJvThread* Container=dynamic_cast<TJvThread*>(t->Container);

  Memo->Lines->Add("Job2 Finished: "+IntToHex(id, 8)+" result: "+IntToStr(rv));
  SJobData* data=(SJobData*)t->Params;
  delete data;

  if(rbExOnFinish->Checked)
  {
   cbAutoStart2->Checked=false; // for preventing endless exception generation
   throw Exception("OnFinish");
  }
 }
 else
 {
  Memo->Lines->Add("Finished: unknown object 'Sender'");
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvThread1FinishAll(TObject *Sender)
{
 TJvThread* Container=dynamic_cast<TJvThread*>(Sender);
 Memo->Lines->Add("Finished all Job 1");
 if(rbExOnFinishAll->Checked)
 {
  cbAutoStart1->Checked=false; // for preventing endless exception generation
  throw Exception("OnFinishAll");
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvThread2FinishAll(TObject *Sender)
{
 TJvThread* Container=dynamic_cast<TJvThread*>(Sender);
 Memo->Lines->Add("Finished all Job 2");
 if(rbExOnFinishAll->Checked)
 {
  cbAutoStart2->Checked=false; // for preventing endless exception generation
  throw Exception("OnFinishAll");
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MemoDblClick(TObject *Sender)
{
 Memo->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::tmrStatusTimer(TObject *Sender)
{
 lbCount1->Caption="Job 1 count: "+IntToStr(JvThread1->Count);
 lbCount2->Caption="Job 2 count: "+IntToStr(JvThread2->Count);

 if(JvThread1->Count==0) lbStats1->Caption = "ID:-------- V:-";
 if(JvThread2->Count==0) lbStats2->Caption = "ID:-------- V:-";

 cbJobTerminated1->Checked=JvThread1->Terminated;
 cbJobTerminated2->Checked=JvThread2->Terminated;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::tmrAutoTimer(TObject *Sender)
{
 if(cbAutoStart1->Checked)
 {
  if(rand()&1) btnStartJob1->Click();
  else btnStartJob1SimpleDlg->Click();
 }

 if(cbAutoStart2->Checked)
 {
  if(rand()&1) btnStartJob2->Click();
  else btnStartJob2AnimatedDlg->Click();
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSuspendItselfClick(TObject *Sender)
{
 SuspendRandomThread=true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnRaiseExceptionClick(TObject *Sender)
{
 RaiseExceptInThread=true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnRemoveZombieClick(TObject *Sender)
{
 JvThread1->RemoveZombie();
 JvThread2->RemoveZombie();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnShowStateClick(TObject *Sender)
{
 AnsiString s;
 char c;

 JvThread1->Lock();   // cleaning up threads with FreeOnTerminate=false
 try
 {
  if(JvThread1->Count)
  {
   Memo->Lines->Add("Job 1 list");
   Memo->Lines->Add("ID       Active Finished Return Value");
   for(int i=0; i<JvThread1->Count; ++i)
   {
    TJvBaseThread* T=JvThread1->Threads[i];
    s=IntToHex((int)T->ThreadID, 8)+" ";
    if(T->ExecuteIsActive) c='+'; else c='-'; s+=c; s+="      ";
    if(T->Finished) c='+'; else c='-'; s+=c; s+="        ";
    s+=IntToStr(T->ReturnValue);
    Memo->Lines->Add(s);
   }
  }
  else
   Memo->Lines->Add("Job 1 list is empty");
 }
 __finally
 {
  JvThread1->Unlock();
 }

 JvThread2->Lock();   // cleaning up threads with FreeOnTerminate=false
 try
 {
  if(JvThread2->Count)
  {
   Memo->Lines->Add("Job 2 list");
   Memo->Lines->Add("ID       Active Finished Return Value");
   for(int i=0; i<JvThread2->Count; ++i)
   {
    TJvBaseThread* T=JvThread2->Threads[i];
    s=IntToHex((int)T->ThreadID, 8)+" ";
    if(T->ExecuteIsActive) c='+'; else c='-'; s+=c; s+="      ";
    if(T->Finished) c='+'; else c='-'; s+=c; s+="        ";
    s+=IntToStr(T->ReturnValue);
    Memo->Lines->Add(s);
   }
  }
  else
   Memo->Lines->Add("Job 2 list is empty");
 }
 __finally
 {
  JvThread2->Unlock();
 }
}
//---------------------------------------------------------------------------

