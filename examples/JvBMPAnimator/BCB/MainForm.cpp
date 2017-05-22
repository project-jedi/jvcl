/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones att altern dott org)

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************/
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
