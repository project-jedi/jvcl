//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma link "JvZoom"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TForm(Owner)
{
  JvZoom1->Active = true;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::CheckBox1Click(TObject *Sender)
{
  JvZoom1->Active = CheckBox1->Checked;        
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::Button1Click(TObject *Sender)
{
  JvZoom1->ZoomLevel = JvZoom1->ZoomLevel/2;        
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::Button2Click(TObject *Sender)
{
  JvZoom1->ZoomLevel = JvZoom1->ZoomLevel*2;        
}
//---------------------------------------------------------------------------

