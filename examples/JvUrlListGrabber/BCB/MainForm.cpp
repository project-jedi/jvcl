/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Olivier Sannier (obones@meloo.com)

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

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvUrlListGrabber"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::DoProgressEvent(TObject* Sender, __int64 Position, __int64 TotalSize, AnsiString Url, bool& Continue)
{
  memExplanation->Lines->Add(Format("Url: %s, Position: %d", ARRAYOFCONST((Url, static_cast<int>(Position)))));
}

void __fastcall TfrmMain::DoHandleError(TObject* Sender, AnsiString ErrorMsg)
{
  memExplanation->Lines->Add(Format("Error: %s", ARRAYOFCONST((ErrorMsg))));
}

void __fastcall TfrmMain::grabberConnectionClosed(TJvUrlListGrabber* Sender, TJvCustomUrlGrabber* Grabber)
{
  Application->MessageBox("Finished", "", 0);
  delete grabber;
}

void __fastcall TfrmMain::btnClearClick(TObject *Sender)
{
  memUrls->Lines->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnStopClick(TObject *Sender)
{
  julGrabber->StopAll();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  memExplanation->WordWrap = true;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnGoDynamicClick(TObject *Sender)
{
  grabber = new TJvUrlListGrabber(this);
  grabber->URLs->Add(InputBox("Url to grab", "Please give a url to grab", "http://jvcl.sf.net/"));
  memExplanation->Lines->Clear();

  TJvCustomUrlGrabber* urlGrabber = grabber->Grabbers[0];

  urlGrabber->OutputMode = omFile;
  urlGrabber->OnError = DoHandleError;
  urlGrabber->OnProgress = DoProgressEvent;
  urlGrabber->FileName = ExtractFilePath(Application->ExeName)+"\test.txt";
  urlGrabber->Start();

  grabber->OnConnectionClosed = grabberConnectionClosed;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnGoDesignClick(TObject *Sender)
{
  int i;

  if (memUrls->Lines->Count == 0)
  {
    Application->MessageBox("Url List cannot be empty",
                           "Error in Url List",
                           MB_ICONERROR);
  }
  else
  {
    memExplanation->Lines->Clear();
    julGrabber->URLs->Clear();
    julGrabber->Cleanup();
    for (i = 0; i < memUrls->Lines->Count; i++)
      if (memUrls->Lines->operator [](i) != "")
        julGrabber->URLs->Add((*memUrls->Lines)[i]);
    for (i = 0; i < julGrabber->URLs->Count; i++)
    {
      TJvCustomUrlGrabber* urlGrabber = julGrabber->Grabbers[i];
      urlGrabber->OnError = DoHandleError;
      urlGrabber->OnProgress = DoProgressEvent;
      urlGrabber->OutputMode = omFile;
      urlGrabber->FileName = ExtractFilePath(Application->ExeName) + "\result" + IntToStr(i) + ".txt";
    }
    julGrabber->StartAll();
  }
}
//---------------------------------------------------------------------------

