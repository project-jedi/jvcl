//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "TestForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmTest *frmTest;
//---------------------------------------------------------------------------
__fastcall TfrmTest::TfrmTest(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmTest::Edit1Change1(TObject *Sender)
{
  mmChanges->Lines->Add("Edit1Change1 event");
}
//---------------------------------------------------------------------------
