//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvBmpAnimator"
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::OnOffClick(TObject *Sender)
{
  BmpAnimator1->Active = !BmpAnimator1->Active;
  if (!BmpAnimator1->Active)
    BmpAnimator1->Position = 0;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Edit1Change(TObject *Sender)
{
  if (!BmpAnimator1->Active)
     BmpAnimator1->Position = UpDown1->Position;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Edit2Change(TObject *Sender)
{
  try
  {
    BmpAnimator1->Speed = StrToInt(Edit2->Text);
  }
  catch(...)
  {
    BmpAnimator1->Speed = 15;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::TransparentClick(TObject *Sender)
{
  BmpAnimator1->Transparent = Transparent->Checked;
}
//---------------------------------------------------------------------------
