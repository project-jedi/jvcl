//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "p_QThrd.h"
#include "main.h"
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Important: Methods and properties of objects in VCL can only be
//   used in a method called using Synchronize, for example:
//
//      Synchronize(UpdateCaption);
//
//   where UpdateCaption could look like:
//
//      void __fastcall TMyThread::UpdateCaption()
//      {
//        Form1->Caption = "Updated in a thread";
//      }
//---------------------------------------------------------------------------

__fastcall TMyThread::TMyThread(bool CreateSuspended)
        : TThread(CreateSuspended)
{
}

__fastcall TMyThread::~TMyThread(void)
{
  Beep();
}
//---------------------------------------------------------------------------
void __fastcall TMyThread::Execute()
{
  TJvUIBQuery *Query;

  FreeOnTerminate = true;
  //Form1.DataBase.Lock; //simulate single thread
  try
  {
    Query = new TJvUIBQuery(NULL);
    try
    {
      Query->Transaction = frmMain->Transaction;
      Query->FetchBlobs = true;
      Query->SQL->Text = "select * from project";
      Query->Open();
      while( !Query->Eof )
      {
        Query->Next();
        ::Sleep(30); // simulate activity
      }
    }
    __finally
    {
      delete Query;
    }
  }
  __finally
  {
    //Form1.DataBase.UnLock; //simulate single thread
  }
}
//---------------------------------------------------------------------------
