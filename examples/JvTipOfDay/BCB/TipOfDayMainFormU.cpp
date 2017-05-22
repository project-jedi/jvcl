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

#include "TipOfDayMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvBaseDlg"
#pragma link "JvComponent"
#pragma link "JvTipOfDay"
#pragma resource "*.dfm"
TTipOfDayMainForm *TipOfDayMainForm;
//---------------------------------------------------------------------------
__fastcall TTipOfDayMainForm::TTipOfDayMainForm(TComponent* Owner)
        : TForm(Owner)
{
  FileDirectory = ExtractFileDir(Application->ExeName);
  cbStyle->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TTipOfDayMainForm::Button2Click(TObject *Sender)
{
  JvTip->Style = TJvTipOfDayStyle(cbStyle->ItemIndex);
  JvTip->Execute();
  if( JvTip->Options.Contains( toShowOnStartUp) )
  {
    ShowMessage("You want to see tips the next time the application is started");
  }
  else
  {
    ShowMessage("You don''t want to see tips the next time the application is started");
  }

}
//---------------------------------------------------------------------------
void __fastcall TTipOfDayMainForm::Button1Click(TObject *Sender)
{
  OpenDialog1->InitialDir = FileDirectory;

  if( OpenDialog1->Execute() )
  {
     JvTip->LoadFromFile(OpenDialog1->FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTipOfDayMainForm::Button3Click(TObject *Sender)
{
  SaveDialog1->InitialDir = FileDirectory;

  if( SaveDialog1->Execute() )
  {
     JvTip->SaveToFile(SaveDialog1->FileName);
  }
}
//---------------------------------------------------------------------------
