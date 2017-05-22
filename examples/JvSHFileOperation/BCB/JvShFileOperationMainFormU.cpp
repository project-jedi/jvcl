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

#include "JvShFileOperationMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvBaseDlg"
#pragma link "JvSHFileOperation"
#pragma resource "*.dfm"
TJvShFileOperationMainForm *JvShFileOperationMainForm;
//---------------------------------------------------------------------------
__fastcall TJvShFileOperationMainForm::TJvShFileOperationMainForm(TComponent* Owner)
        : TForm(Owner)
{
   Application->HintHidePause = 5000;
}
//---------------------------------------------------------------------------

void TJvShFileOperationMainForm::DoIt(TJvSHFileOpType AType, const AnsiString OKMsg)
{
 TJvSHFileOptions AOptions;

  memMessages->Lines->Clear();
  JvSHFileOperation1->Operation   = AType;
  JvSHFileOperation1->SourceFiles = memSource->Lines;
  JvSHFileOperation1->DestFiles   = memDest->Lines;
  JvSHFileOperation1->Title       = edTitle->Text;

  AOptions.Clear();// = NULL;
  if( chkUndo->Checked )
  {
    AOptions = (AOptions << fofAllowUndo);
    //Include(AOptions,fofAllowUndo);
  }
  if( chkFiles->Checked )
  {
    AOptions = (AOptions << fofFilesOnly);
    //Include(AOptions,fofFilesOnly);
  }
  if( chkMulti->Checked )
  {
    AOptions = (AOptions << fofMultiDestFiles);
    //Include(AOptions,fofMultiDestFiles);
  }
  if( chkNoConfirm->Checked )
  {
    AOptions = (AOptions << fofNoConfirmation);
    //Include(AOptions,fofNoConfirmation);
  }
  if( chkNoDirCreate->Checked )
  {
    AOptions = (AOptions << fofNoConfirmMkDir);
    //Include(AOptions,fofNoConfirmMkDir);
  }
  if( chkRename->Checked )
  {
    AOptions = (AOptions << fofRenameOnCollision);
    //Include(AOptions,fofRenameOnCollision);
  }
  if( chkSilent->Checked )
  {
    AOptions = (AOptions << fofSilent);
    //Include(AOptions,fofSilent);
  }
  if( chkSimple->Checked )
  {
    AOptions = (AOptions << fofSimpleProgress);
    //Include(AOptions,fofSimpleProgress);
  }
  if( chkMappings->Checked )
  {
    AOptions = (AOptions << fofWantMappingHandle);
    //Include(AOptions,fofWantMappingHandle);
  }
  if( chkNoErrors->Checked )
  {
    AOptions = (AOptions << fofNoErrorUI);
    //Include(AOptions,fofNoErrorUI);
  }
  if( chkNoSecAttrs->Checked )
  {
    AOptions = (AOptions << fofNoCopySecurityAttributes);
    //Include(AOptions,fofNoCopySecurityAttributes);
  }
  if( chkNoRecurse->Checked )
  {
    AOptions = (AOptions << fofNoRecursion);
    //Include(AOptions,fofNoRecursion);
  }
  if( chkNoConElem->Checked )
  {
    AOptions = (AOptions << fofNoConnectedElements);
    //Include(AOptions,fofNoConnectedElements);
  }
  if( chkNoParse->Checked )
  {
    AOptions = (AOptions << fofNoRecurseParse);
    //Include(AOptions,fofNoRecurseParse);
  }
  if( chkWantNukes->Checked )
  {
    AOptions = (AOptions << fofWantNukeWarning);
    //Include(AOptions,fofWantNukeWarning);
  }
  JvSHFileOperation1->Options = AOptions;
  if( !JvSHFileOperation1->Execute() )
  {
    memMessages->Lines->Add(SysErrorMessage(GetLastError()));
  }
  else
  {
    memMessages->Lines->Add(OKMsg);
  }
}

void __fastcall TJvShFileOperationMainForm::JvSHFileOperation1FileMapping(
      TObject *Sender, const AnsiString OldFileName,
      const AnsiString NewFileName)
{
  memMessages->Lines->Add(Format("%s renamed as %s",ARRAYOFCONST( (OldFileName, NewFileName) ) ) );
}
//---------------------------------------------------------------------------

void __fastcall TJvShFileOperationMainForm::btnCopyClick(TObject *Sender)
{
  DoIt(foCopy,"Copy finished");
}
//---------------------------------------------------------------------------

void __fastcall TJvShFileOperationMainForm::btnMoveClick(TObject *Sender)
{
  DoIt(foMove,"Move finished");
}
//---------------------------------------------------------------------------

void __fastcall TJvShFileOperationMainForm::btnRenameClick(TObject *Sender)
{
  DoIt(foRename,"Rename finished");
}
//---------------------------------------------------------------------------

void __fastcall TJvShFileOperationMainForm::btnDeleteClick(TObject *Sender)
{
  DoIt(foDelete,"Delete finished");
}
//---------------------------------------------------------------------------

