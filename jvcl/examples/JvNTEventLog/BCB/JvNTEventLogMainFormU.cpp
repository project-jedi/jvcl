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

#include "JvNTEventLogMainFormU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponentBase"
#pragma link "JvNTEventLog"
#pragma resource "*.dfm"
TJvNTEventLogMainForm *JvNTEventLogMainForm;
//---------------------------------------------------------------------------
__fastcall TJvNTEventLogMainForm::TJvNTEventLogMainForm(TComponent* Owner)
        : TForm(Owner)
{
  JvNTEventLog1->ReadEventLogs(ListBox1->Items);
  JvNTEventLog1->Active = true;
  PostMessage(Handle, CM_CHECKOSVERSION,0,0);
}
//---------------------------------------------------------------------------

void __fastcall TJvNTEventLogMainForm::CMCHECKOSVERSION(TMessage & Msg)
{
  if( Win32Platform != VER_PLATFORM_WIN32_NT)
  {
    ShowMessage("This demo only works on NT based OS's (NT 4, Win2k and WinXP).");
  }
}

void __fastcall TJvNTEventLogMainForm::ListBox1Click(TObject *Sender)
{
  if( ListBox1->ItemIndex > -1)
  {
    JvNTEventLog1->Active = false;
    JvNTEventLog1->Log    = ListBox1->Items->Strings[ListBox1->ItemIndex];
    JvNTEventLog1->Source = ListBox1->Items->Strings[ListBox1->ItemIndex];
    JvNTEventLog1->Active = true;
    ReadEvents();
  }

}
//---------------------------------------------------------------------------

void TJvNTEventLogMainForm::ReadEvents(void)
{
  TListItem *pItem;

  ListView1->Items->BeginUpdate();
  Screen->Cursor = crHourGlass;
  __try
  {
    ListView1->Items->Clear();

    JvNTEventLog1->First();
    while( !JvNTEventLog1->Eof() )
    {
      pItem = ListView1->Items->Add();
      pItem->Caption = JvNTEventLog1->EventRecord->EventType;
      pItem->SubItems->Add(DateToStr(JvNTEventLog1->EventRecord->DateTime));
      pItem->SubItems->Add(TimeToStr(JvNTEventLog1->EventRecord->DateTime));
      pItem->SubItems->Add(JvNTEventLog1->EventRecord->Source);
      pItem->SubItems->Add(IntToStr(JvNTEventLog1->EventRecord->Category));
      pItem->SubItems->Add(IntToStr(JvNTEventLog1->EventRecord->ID & 0x0FFFFFFF));
      pItem->SubItems->Add(JvNTEventLog1->EventRecord->UserName);
      pItem->SubItems->Add(JvNTEventLog1->EventRecord->Computer);
      JvNTEventLog1->Next();
    }
  }
  __finally
  {
    ListView1->Items->EndUpdate();
    Screen->Cursor = crDefault;
  }
}

void __fastcall TJvNTEventLogMainForm::btnRefreshClick(TObject *Sender)
{
  if( ListBox1->ItemIndex > -1)
  {
    ReadEvents();
  }
}
//---------------------------------------------------------------------------

