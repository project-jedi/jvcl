//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvUIB"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::goClick(TObject *Sender)
{
  TARecord Datas[10];
  /* init Datas */
  for(int i=0;i<10;++i)
  {
    Datas[i].COUNTRY ="blabla"+IntToStr(i);
    Datas[i].CURRENCY="blabla";
  }

  for(int i = 0;i<10;++i)
  {
    Query->Params->AsString[0] = Datas[i].COUNTRY;
    Query->Params->AsString[1] = Datas[i].CURRENCY;
    Query->Execute();
    // for better performance commit every 1000 records
    // Transaction.Commit;
  }
  Query->Close(etmRollback); // change to etmCommit to apply.

}
//---------------------------------------------------------------------------

