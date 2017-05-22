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

#include "JvSystemPopup2MainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvSystemPopup"
#pragma resource "*.dfm"
TJvSystemPopup2MainForm *JvSystemPopup2MainForm;
//---------------------------------------------------------------------------
__fastcall TJvSystemPopup2MainForm::TJvSystemPopup2MainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TJvSystemPopup2MainForm::actClickToCheckExecute(TObject *Sender)
{
  TAction *pTA;
  pTA = dynamic_cast<TAction *>(Sender);
  if(pTA!=NULL)
  {
      pTA->Checked = !pTA->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TJvSystemPopup2MainForm::actClickToSwitchIconExecute(
      TObject *Sender)
{
  TAction *pTA;
  pTA = dynamic_cast<TAction *>(Sender);
  if(pTA!=NULL)
  {
    pTA->ImageIndex = ((pTA->ImageIndex + 2) % 4) - 1;
  }
}
//---------------------------------------------------------------------------
void __fastcall TJvSystemPopup2MainForm::actHideableItemExecute(
      TObject *Sender)
{
  chbHideHideableItem->Checked = true;
}
//---------------------------------------------------------------------------
void __fastcall TJvSystemPopup2MainForm::actShortCutExecute(
      TObject *Sender)
{
  ShowMessage("ShortCut");
}
//---------------------------------------------------------------------------
void __fastcall TJvSystemPopup2MainForm::Action21Click(TObject *Sender)
{
  TMenuItem *pMI;

  pMI = dynamic_cast<TMenuItem *>(Sender);
  if(pMI!=NULL)
  {
     pMI->Checked = !pMI->Checked;
  }
}
//---------------------------------------------------------------------------
void __fastcall TJvSystemPopup2MainForm::btnSwitchClick(TObject *Sender)
{

    if(JvSystemPopup1->Popup == PopupMenu1)
    {
      JvSystemPopup1->Popup = PopupMenu2;
    }
    else
    {
      JvSystemPopup1->Popup = PopupMenu1;
    }
}
//---------------------------------------------------------------------------
void __fastcall TJvSystemPopup2MainForm::chbHideHideableItemClick(
      TObject *Sender)
{
  actHideableItem->Visible = !chbHideHideableItem->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TJvSystemPopup2MainForm::rgrPositionClick(TObject *Sender)
{

  JvSystemPopup1->Position = static_cast<TJvPopupPosition >(rgrPosition->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TJvSystemPopup2MainForm::rgrPositionInMenuClick(
      TObject *Sender)
{
  JvSystemPopup1->PositionInMenu = static_cast<TJvPositionInMenu >(rgrPositionInMenu->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TJvSystemPopup2MainForm::btnAddClick(TObject *Sender)
{
  TMenuItem *MenuItem;

  MenuItem = new TMenuItem(NULL);
//  with MenuItem do
  MenuItem->Caption = edtAdd->Text;

  PopupMenu1->Items->Items[0]->Add(MenuItem);
  if( JvSystemPopup1->Popup == PopupMenu1 )
  {
    JvSystemPopup1->Refresh(False);
  }
}
//---------------------------------------------------------------------------
