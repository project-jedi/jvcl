//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "TransparentForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma link "JvFormTransparent"
#pragma link "JvLabel"
#pragma resource "*.dfm"
TfrmTransparent *frmTransparent;
//---------------------------------------------------------------------------
__fastcall TfrmTransparent::TfrmTransparent(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
