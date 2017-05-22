//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit3.h"
#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvDockControlForm"
#pragma resource "*.dfm"
TForm3 *Form3;
//---------------------------------------------------------------------------
__fastcall TForm3::TForm3(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm3::lbDockClient1FormHide(TObject *Sender)
{
  ((TMenuItem*)Tag)->Checked = FALSE;
}
//---------------------------------------------------------------------------

void __fastcall TForm3::lbDockClient1FormShow(TObject *Sender)
{
  ((TMenuItem*)Tag)->Checked = TRUE;
}
//---------------------------------------------------------------------------

