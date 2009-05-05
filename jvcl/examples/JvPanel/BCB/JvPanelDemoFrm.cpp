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

#include <vcl.h>
#pragma hdrstop

#include "JvPanelDemoFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponentBase"
#pragma link "JvExExtCtrls"
#pragma link "JvExMask"
#pragma link "JvExtComponent"
#pragma link "JvFormPlacement"
#pragma link "JvPanel"
#pragma link "JvToolEdit"
#pragma resource "*.dfm"
TJvPanelDemoMainFrm *JvPanelDemoMainFrm;
//---------------------------------------------------------------------------
__fastcall TJvPanelDemoMainFrm::TJvPanelDemoMainFrm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TJvPanelDemoMainFrm::CheckBox2Click(TObject *Sender)
{
  JvPanel1->ArrangeSettings->AutoArrange = CheckBox2->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TJvPanelDemoMainFrm::CheckBox1Click(TObject *Sender)
{
  JvPanel1->Transparent = CheckBox1->Checked;
}
//---------------------------------------------------------------------------
