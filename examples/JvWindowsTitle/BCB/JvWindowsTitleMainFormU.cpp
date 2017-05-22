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

#include "JvWindowsTitleMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
#pragma link "JvJCLUtils"
TJvWindowsTitleMainForm *JvWindowsTitleMainForm;
//---------------------------------------------------------------------------
__fastcall TJvWindowsTitleMainForm::TJvWindowsTitleMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TJvWindowsTitleMainForm::Button1Click(TObject *Sender)
{

  TStringList* S;
  int i;

  S = new TStringList();
  ListBox1->Items->BeginUpdate();
  try
  {
    ListBox1->Items->Clear();
    GetVisibleWindows(S);
    for( i = 0;i < S->Count;++i)
    { 
      ListBox1->Items->Add( Format( "%s (%d)", OPENARRAY(TVarRec, (S->Strings[i],reinterpret_cast<int >(S->Objects[i]) ) ) ) );
    }
  }
  __finally
  {
    ListBox1->Items->EndUpdate();
    delete S;
  }

}
//---------------------------------------------------------------------------
