/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones att altern dott org)

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
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvCombobox"
#pragma link "JvDriveCtrls"
#pragma link "JvExStdCtrls"
#pragma link "JvListBox"
#pragma link "JvAni"

#pragma resource "*.dfm"

TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TForm(Owner)
{
  Activated = false;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormActivate(TObject *Sender)
{
  if (!Activated)
  {
    Activated = true;
    char buffer[MAX_PATH+1];
    GetWindowsDirectory(buffer, MAX_PATH);
    strcat(buffer, "\\Cursors");
    JvDirectoryListBox1->Directory = buffer;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvFileListBox1Click(TObject *Sender)
{
  Image1->Picture->LoadFromFile(JvFileListBox1->FileName);

  if (Image1->Picture != NULL &&
      Image1->Picture->Graphic != NULL &&
      dynamic_cast<TJvAni *>(Image1->Picture->Graphic) != NULL)
  {
    TJvAni* ani = dynamic_cast<TJvAni *>(Image1->Picture->Graphic);
    ani->Animated = true;
    ani->AssignIconsToBitmap(ImageIcons->Picture->Bitmap, clBtnFace, false, false);
    ani->AssignToBitmap(ImageFrames->Picture->Bitmap, clBtnFace, false, false);
    Memo1->Clear();
    Memo1->Lines->Add("Author: " + ani->Author);
    Memo1->Lines->Add("Title: " + ani->Title);
    Memo1->Lines->Add("Icons: " + IntToStr(ani->IconCount));
    Memo1->Lines->Add("Frames: " + IntToStr(ani->FrameCount));

  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnSaveClick(TObject *Sender)
{
  SaveDialog1->InitialDir = JvDirectoryListBox1->Directory;
  if (SaveDialog1->Execute())
    dynamic_cast<TJvAni *>(Image1->Picture->Graphic)->SaveToFile(SaveDialog1->FileName);
}
//---------------------------------------------------------------------------
