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
      if (memUrls->Lines->Strings[i] != "")
        julGrabber->URLs->Add(memUrls->Lines->Strings[i]);
    for (i = 0; i < julGrabber->URLs->Count; i++)
    {
      TJvCustomUrlGrabber* urlGrabber = julGrabber->Grabbers[i];
      urlGrabber->OutputMode = omFile;
      urlGrabber->FileName = ExtractFilePath(Application->ExeName) + "\\result" + IntToStr(i) + ".txt";
    }
    julGrabber->StartAll();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::julGrabberProgress(TJvUrlListGrabber *Sender,
      TJvCustomUrlGrabber *Grabber, __int64 Position, __int64 TotalSize,
      AnsiString Url, bool &Continue)
{
  memExplanation->Lines->Add(Format("Grabber %d: Url: %s, Position: %d of %d", ARRAYOFCONST((static_cast<int>(Grabber->Id), Url, static_cast<int>(Position), static_cast<int>(TotalSize)))));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::julGrabberError(TJvUrlListGrabber *Sender,
      TJvCustomUrlGrabber *Grabber, AnsiString ErrorMsg)
{
  memExplanation->Lines->Add(Format("Grabber %d: Error: %s", ARRAYOFCONST((static_cast<int>(Grabber->Id), ErrorMsg))));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::julGrabberConnectedToServer(
      TJvUrlListGrabber *Sender, TJvCustomUrlGrabber *Grabber)
{
  memExplanation->Lines->Add(Format("Grabber %d: Connected to server", ARRAYOFCONST((static_cast<int>(Grabber->Id)))));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::julGrabberConnectionClosed(
      TJvUrlListGrabber *Sender, TJvCustomUrlGrabber *Grabber)
{
  memExplanation->Lines->Add(Format("Grabber %d: Connection closed", ARRAYOFCONST((static_cast<int>(Grabber->Id)))));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::julGrabberDoneFile(TJvUrlListGrabber *Sender,
      TJvCustomUrlGrabber *Grabber, AnsiString FileName, int FileSize,
      AnsiString Url)
{
  memExplanation->Lines->Add(Format("Grabber %d: Done file %s of size %d from Url %s", ARRAYOFCONST((static_cast<int>(Grabber->Id), FileName, static_cast<int>(FileSize), Url))));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::julGrabberRequestSent(TJvUrlListGrabber *Sender,
      TJvCustomUrlGrabber *Grabber)
{
  memExplanation->Lines->Add(Format("Grabber %d: Request sent", ARRAYOFCONST((static_cast<int>(Grabber->Id)))));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::julGrabberSendingRequest(
      TJvUrlListGrabber *Sender, TJvCustomUrlGrabber *Grabber)
{
  memExplanation->Lines->Add(Format("Grabber %d: Sending request", ARRAYOFCONST((static_cast<int>(Grabber->Id)))));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::julGrabberStatusChange(TJvUrlListGrabber *Sender,
      TJvCustomUrlGrabber *Grabber)
{
  memExplanation->Lines->Add(Format("Grabber %d: Status change", ARRAYOFCONST((static_cast<int>(Grabber->Id)))));
}
//---------------------------------------------------------------------------

