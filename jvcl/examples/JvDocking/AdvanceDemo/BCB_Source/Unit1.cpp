//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvDockControlForm"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::lbDockClient1FormHide(TObject *Sender)
{
  ((TMenuItem*)Tag)->Checked = FALSE;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::lbDockClient1FormShow(TObject *Sender)
{
  ((TMenuItem*)Tag)->Checked = TRUE;
}
//---------------------------------------------------------------------------

