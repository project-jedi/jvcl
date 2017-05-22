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

#include "MainFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvUninstallControls"
#pragma link "ExtCtrls"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvUninstallListBox1Click(TObject *Sender)
{

   memListInfo->Lines = JvUninstallListBox1->Properties;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkListShowAllClick(TObject *Sender)
{
  JvUninstallListBox1->ShowAll = chkListShowAll->Checked;
  JvUninstallListBox1->RefreshItem();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkListSortedClick(TObject *Sender)
{
  JvUninstallListBox1->Sorted = chkListSorted->Checked;
  JvUninstallListBox1->RefreshItem();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkShowEmptyClick(TObject *Sender)
{
  JvUninstallListBox1->ShowEmptyValues = chkShowEmpty->Checked;
  JvUninstallListBox1->RefreshItem();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::JvUninstallComboBox1Click(TObject *Sender)
{
    memComboInfo->Lines = JvUninstallComboBox1->Properties;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkComboShowAllClick(TObject *Sender)
{
  JvUninstallComboBox1->ShowAll = chkComboShowAll->Checked;
  JvUninstallComboBox1->RefreshItem();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkComboSortedClick(TObject *Sender)
{
  JvUninstallComboBox1->Sorted = chkComboSorted->Checked;
  JvUninstallComboBox1->RefreshItem();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::chkComboEmptyValuesClick(TObject *Sender)
{
  JvUninstallComboBox1->ShowEmptyValues = chkComboEmptyValues->Checked;
  JvUninstallComboBox1->RefreshItem();

}
//---------------------------------------------------------------------------
