//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvDockControlForm"
#pragma resource "*.dfm"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm2::lbDockClient1FormHide(TObject *Sender)
{
  ((TMenuItem*)Tag)->Checked = FALSE;
}
//---------------------------------------------------------------------------

void __fastcall TForm2::lbDockClient1FormShow(TObject *Sender)
{
  ((TMenuItem*)Tag)->Checked = TRUE;
}
//---------------------------------------------------------------------------

