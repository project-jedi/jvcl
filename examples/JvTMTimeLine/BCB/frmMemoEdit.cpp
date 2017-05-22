/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

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

#include "frmMemoEdit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TMemoEditFrm *MemoEditFrm;
//---------------------------------------------------------------------------
__fastcall TMemoEditFrm::TMemoEditFrm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMemoEditFrm::reLinesKeyUp(TObject *Sender, WORD &Key,
      TShiftState Shift)
{
  if( Key == VK_ESCAPE)
  {
    Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TMemoEditFrm::Load1Click(TObject *Sender)
{
  TOpenDialog* pOD;

  pOD = new TOpenDialog(NULL);

  try
  {
   if( pOD->Execute() )
   {
      reLines->Lines->LoadFromFile(pOD->FileName);
   }
  } 
  __finally
  {
    delete pOD;
  }

}
//---------------------------------------------------------------------------
void __fastcall TMemoEditFrm::Save1Click(TObject *Sender)
{
  TSaveDialog* pSD;
  pSD = new TSaveDialog(NULL);


  try
  {
    if( pSD->Execute() )
    {
      reLines->Lines->SaveToFile(pSD->FileName);
    }
  }
  __finally
  {
    delete pSD;
  }

}
//---------------------------------------------------------------------------
void __fastcall TMemoEditFrm::Cut1Click(TObject *Sender)
{
  reLines->CutToClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TMemoEditFrm::Copy1Click(TObject *Sender)
{
  reLines->CopyToClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TMemoEditFrm::Paste1Click(TObject *Sender)
{
  reLines->PasteFromClipboard();
}
//---------------------------------------------------------------------------
void __fastcall TMemoEditFrm::Selectall1Click(TObject *Sender)
{
  reLines->SelectAll();
}
//---------------------------------------------------------------------------

bool __fastcall TMemoEditFrm_Edit(TStrings* Lines, TDateTime ADate, TIcon* Icon)
{
 TMemoEditFrm* f;
 bool flResult;

  f = new TMemoEditFrm(Application);
  try
  {
    f->Icon = Icon;
    f->Caption = Format(f->Caption,OPENARRAY(TVarRec,(DateToStr(ADate) ) ) );
    f->reLines->Lines = Lines;
    flResult = (f->ShowModal() == mrOk);
    if( flResult )
    {
      Lines->Assign(f->reLines->Lines);
    }
  }
  __finally
  {
    delete  f;
  }

  return flResult;
}
