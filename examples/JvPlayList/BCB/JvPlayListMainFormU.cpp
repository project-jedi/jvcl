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

#include "JvPlayListMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvExStdCtrls"
#pragma link "JvListBox"
#pragma link "JvPlaylist"
#pragma resource "*.dfm"
TJvPlayListMainForm *JvPlayListMainForm;
//---------------------------------------------------------------------------
__fastcall TJvPlayListMainForm::TJvPlayListMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::JvPlayList1Click(TObject *Sender)
{
  if( JvPlayList1->ItemIndex!=-1 )
  {
    Label1->Caption = JvPlayList1->Items->Strings[JvPlayList1->ItemIndex];
  }
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::OpenExecute(TObject *Sender)
{
  if( OpenDialog1->Execute() == true )
  {
    JvPlayList1->AddItems(OpenDialog1->Files);
  }
}
//---------------------------------------------------------------------------

void __fastcall TJvPlayListMainForm::DeleteExecute(TObject *Sender)
{
  JvPlayList1->DeleteSelected();
}
//---------------------------------------------------------------------------

void __fastcall TJvPlayListMainForm::ExitExecute(TObject *Sender)
{
  Application->Terminate();
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::DeleteDeadExecute(TObject *Sender)
{
  JvPlayList1->DeleteDeadFiles();
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::SortSongExecute(TObject *Sender)
{
  JvPlayList1->SortBySongName();
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::ShowDrives1Click(TObject *Sender)
{
 TMenuItem *pMI;

  pMI = dynamic_cast<TMenuItem *>(Sender);
  if( pMI != NULL )
  {
    pMI->Checked = !pMI->Checked;

    JvPlayList1->ShowDrive = pMI->Checked;
  }

}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::SortPahExecute(TObject *Sender)
{
  JvPlayList1->SortByPathInverted();
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::ShowNumbers1Click(TObject *Sender)
{
 TMenuItem *pMI;

  pMI = dynamic_cast<TMenuItem *>(Sender);
  if( pMI != NULL )
  {
    pMI->Checked = !pMI->Checked;
    JvPlayList1->ShowNumbers = pMI->Checked;
  }
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::ShowExtensions1Click(TObject *Sender)
{
 TMenuItem *pMI;

  pMI = dynamic_cast<TMenuItem *>(Sender);
  if( pMI != NULL )
  {
    pMI->Checked = !pMI->Checked;
    JvPlayList1->ShowExtension = pMI->Checked;
  }
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::SortSongNameInvertedExecute(TObject *Sender)
{
  JvPlayList1->SortBySongNameInverted();
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::RandomOrderExecute(TObject *Sender)
{
  JvPlayList1->RandomOrder();
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::ReverseExecute(TObject *Sender)
{
  JvPlayList1->ReverseOrder();
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::SelectAllExecute(TObject *Sender)
{
  JvPlayList1->SelectAll();

}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::UnselectAllExecute(TObject *Sender)
{
  JvPlayList1->UnselectAll();
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::InvSelectExecute(TObject *Sender)
{
  JvPlayList1->InvertSelection();

}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::MoveUpExecute(TObject *Sender)
{
  JvPlayList1->MoveSelectedUp();        
}
//---------------------------------------------------------------------------
void __fastcall TJvPlayListMainForm::MoveDownExecute(TObject *Sender)
{
  JvPlayList1->MoveSelectedDown();        
}
//---------------------------------------------------------------------------

