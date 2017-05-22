//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DockWindowForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvDockControlForm"
#pragma resource "*.dfm"
TfrmDockWindow *frmDockWindow;
//---------------------------------------------------------------------------
__fastcall TfrmDockWindow::TfrmDockWindow(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
