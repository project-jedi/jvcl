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
#include "JvAppHotKey.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
#pragma link "JvAppHotKey"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}

void _fastcall TfrmMain::DoHotKey(TObject* Sender)
{
  Application->BringToFront();
  ShowMessage(
    Format("HotKey \"%s\" pressed!",
    ARRAYOFCONST(((AnsiString)ShortCutToText(dynamic_cast<TJvApplicationHotKey *>(Sender)->HotKey))))
    );
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::btnAddClick(TObject *Sender)
{
  AnsiString S;

  S = ShortCutToText(HotKey1->HotKey);
  if (lbHotKeys->Items->IndexOf(S) > -1)
  {
    ShowMessage("Hot key already assigned!");
    return;
  }

  TJvApplicationHotKey* jah = new TJvApplicationHotKey(this);

  jah->HotKey = HotKey1->HotKey;
  jah->Active = true;
  jah->OnHotKey = DoHotKey;
  lbHotKeys->Items->Add(S);
}
//---------------------------------------------------------------------------

