/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones@meloo.com)

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
// $Id$
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvExComCtrls"
#pragma link "JvMenus"
#pragma link "JvToolBar"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Exit1Click(TObject *Sender)
{
  Application->Terminate();  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Try1Click(TObject *Sender)
{
  ShowMessage("Nice try !");
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnAddItemsClick(TObject *Sender)
{
  TMenuItem* item;

  item = new TMenuItem(this);
  item->Caption = "Add";
  jmnMain->Items->Add(item);
  item = new TMenuItem(this);
  item->Caption = "Below Add";
  jmnMain->Items->Items[3]->Add(item);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnChangeCaptionClick(TObject *Sender)
{
  // Put a tab in it to test its correct expansion
  Try1->Caption = "1\tReFile";
}
//---------------------------------------------------------------------------
