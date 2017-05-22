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

#include "JvPageListTreeViewMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvButton"
#pragma link "JvColorCombo"
#pragma link "JvCombobox"
#pragma link "JvCtrls"
#pragma link "JvExComCtrls"
#pragma link "JvExControls"
#pragma link "JvExExtCtrls"
#pragma link "JvExStdCtrls"
#pragma link "JvExtComponent"
#pragma link "JvFooter"
#pragma link "JvGroupHeader"
#pragma link "JvPageList"
#pragma link "JvPageListTreeView"
#pragma resource "*.dfm"
TJvPageListTreeViewMainFrm *JvPageListTreeViewMainFrm;
//---------------------------------------------------------------------------
__fastcall TJvPageListTreeViewMainFrm::TJvPageListTreeViewMainFrm(TComponent* Owner)
        : TForm(Owner)
{
  JvColorComboBox1->InsertColor(0, clBlack, "Automatic");
  JvColorComboBox2->InsertColor(0, clWhite, "Automatic");
}
//---------------------------------------------------------------------------
void __fastcall TJvPageListTreeViewMainFrm::JvFooterBtn2Click(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
