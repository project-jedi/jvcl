/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):
   korecek: translation from Delphi to BCB

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

#include "JvTreeViewAsMenuMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComCtrls"
#pragma link "JvExComCtrls"
#pragma resource "*.dfm"
TJvTreeViewAsMenuMainForm *JvTreeViewAsMenuMainForm;
//---------------------------------------------------------------------------
__fastcall TJvTreeViewAsMenuMainForm::TJvTreeViewAsMenuMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TJvTreeViewAsMenuMainForm::JvTreeView1PageChanged(
      TObject *Sender, TTreeNode *Item, TTabSheet *Page)
{
   Caption = Item->Text+" - "+Page->Caption;
}
//---------------------------------------------------------------------------
