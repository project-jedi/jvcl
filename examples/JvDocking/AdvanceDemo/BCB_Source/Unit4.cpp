//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit4.h"
#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvDockControlForm"
#pragma resource "*.dfm"
TForm4 *Form4;
//---------------------------------------------------------------------------
__fastcall TForm4::TForm4(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm4::lbDockClient1FormHide(TObject *Sender)
{
  ((TMenuItem*)Tag)->Checked = FALSE;
}
//---------------------------------------------------------------------------

void __fastcall TForm4::lbDockClient1FormShow(TObject *Sender)
{
  ((TMenuItem*)Tag)->Checked = TRUE;
}
//---------------------------------------------------------------------------

