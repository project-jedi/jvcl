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
#pragma link "JvAVICapture"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnConnectClick(TObject *Sender)
{
  try
  {
    AviCap->Connect(0);
  }
  catch(EInvalidDriverIndexError &e)
  {
      ShowMessage("No device found. Verify your connection and configuration.");
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnStartPreviewClick(TObject *Sender)
{
  AviCap->StartPreview();  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnStopPreviewClick(TObject *Sender)
{
  AviCap->StopPreview();  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCaptureClick(TObject *Sender)
{
  AviCap->CaptureSettings->ConfirmCapture = true;
  AviCap->CaptureSettings->LimitEnabled = true;
  AviCap->CaptureSettings->TimeLimit = 3;
  AviCap->StartCapture();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnSourceClick(TObject *Sender)
{
  if (!AviCap->ShowDialog(vdSource)) 
    Application->MessageBox("Your driver doesn''t provide this dialog "
      "or you are not connected to a driver",
      "Unable to show dialog",
      MB_ICONINFORMATION);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnFormatClick(TObject *Sender)
{
  if (!AviCap->ShowDialog(vdFormat))
    Application->MessageBox("Your driver doesn''t provide this dialog "
      "or you are not connected to a driver",
      "Unable to show dialog",
      MB_ICONINFORMATION);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnDisplayClick(TObject *Sender)
{
  if (!AviCap->ShowDialog(vdDisplay)) 
    Application->MessageBox("Your driver doesn''t provide this dialog "
      "or you are not connected to a driver",
      "Unable to show dialog",
      MB_ICONINFORMATION);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCompressionClick(TObject *Sender)
{
  if (!AviCap->ShowDialog(vdCompression)) 
    Application->MessageBox("Your driver doesn''t provide this dialog "
      "or you are not connected to a driver",
      "Unable to show dialog",
      MB_ICONINFORMATION);
}
//---------------------------------------------------------------------------
