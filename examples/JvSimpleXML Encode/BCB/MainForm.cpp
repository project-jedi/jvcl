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
#include <JvJVCLUtils.hpp>
#include <Clipbrd.hpp>
#include <JvSimpleXML.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvJVCLUtils"
#pragma link "JvSimpleXML"
#pragma link "Clipbrd"
#pragma resource "*.dfm"
TfrmMain *frmMain;

AnsiString StringFromFile(const AnsiString FileName)
{
  TFileStream* fs = new TFileStream(FileName, fmOpenRead);

  AnsiString Result;
  try
  {
    Result.SetLength(fs->Size);
    if (fs->Size > 0)
      fs->Read(Result.c_str(), fs->Size);
  }
  __finally
  {
    delete fs;
  }
  return Result;
}

void __fastcall TfrmMain::WMDropFiles(TWMDropFiles& Message)
{
  int Count = DragQueryFile(reinterpret_cast<void*>(Message.Drop), 0xFFFFFFFF, NULL, 0);
  try
  {
    char FileBuf[MAX_PATH+1];
    if (Count > 0)
    {
      TStringList* FileList = new TStringList();
      try
      {
        for (int i = 0; i < Count; i++)
        {
          DragQueryFile(reinterpret_cast<void*>(Message.Drop), i, FileBuf, sizeof(FileBuf));
          FileList->Add(FileBuf);
        }
        FileList->Sort();
        reSource->Lines->BeginUpdate();
        try
        {
          reSource->Lines->Clear();
          for (int i = 0; i < FileList->Count; i++)
            reSource->Lines->Add(StringFromFile((*FileList)[i]));
        }
        __finally
        {
          reSource->Lines->EndUpdate();
        }
      }
      __finally
      {
        delete FileList;
      }
    }
  } __finally {
    DragFinish(reinterpret_cast<void*>(Message.Drop));
  }
}

void __fastcall TfrmMain::DisplayTime(int MS)
{
  int kB = reSource->GetTextLen() / 1024;
  if (kB == 0) return;
  if (MS == 0) MS = 1;
  (*StatusBar1->Panels)[0]->Text = Format("Conversion of %dkB took %d msecs -> %f kB/sec", ARRAYOFCONST((kB, MS, kB / MS * 1000.0)));
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnEncodeClick(TObject *Sender)
{
  AnsiString S;
  Cardinal FStartValue;

  WaitCursor();
  reSource->Lines->BeginUpdate();
  reResult->Lines->BeginUpdate();
  try {
  // assign to S to take the visual control out of the equation
#if __BORLANDC__ >= 0x560
    if (chkUseUTF8->Checked)
    {
      if (chkUseClipboard->Checked)
      {
        reSource->CopyToClipboard();
        S = Clipboard()->AsText;
      }
      else
        S = reSource->Lines->Text;
      FStartValue = GetTickCount();
      S = UTF8Encode(S);
      FStartValue = GetTickCount() - FStartValue;
      if (chkUseClipboard->Checked)
      {
        Clipboard()->AsText = S;
        reSource->PasteFromClipboard();
      }
      else
        reResult->Lines->Text = S;
    }
    else
#endif
    {
      if (chkUseClipboard->Checked)
      {
        reSource->CopyToClipboard();
        S = Clipboard()->AsText;
      }
      else
        S = reSource->Lines->Text;
      FStartValue = GetTickCount();
      S = XMLEncode(S);
      FStartValue = GetTickCount() - FStartValue;
      if (chkUseClipboard->Checked)
      {
        Clipboard()->AsText = S;
        reSource->PasteFromClipboard();
      }
      else
        reResult->Lines->Text = S;
    }
    DisplayTime(FStartValue);
  }
  __finally
  {
    reSource->Lines->EndUpdate();
    reResult->Lines->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnDecodeClick(TObject *Sender)
{
  AnsiString S;
  Cardinal FStartValue;

  WaitCursor();
  // assign to S to take the visual control out of the equation
  reSource->Lines->BeginUpdate();
  reResult->Lines->BeginUpdate();
  try
  {
#if __BORLANDC__ >= 0x560
    if (chkUseUTF8->Checked)
    {
      S = reSource->Lines->Text;
      FStartValue = GetTickCount();
      S = UTF8Decode(S);
      FStartValue = GetTickCount() - FStartValue;
      reResult->Lines->Text = S;
    }
    else
#endif
    {
      S = reSource->Lines->Text;
      FStartValue = GetTickCount();
      SimpleXMLDecode(S, chkTrim->Checked);
      FStartValue = GetTickCount() - FStartValue;
      reResult->Lines->Text = S;
    }
    DisplayTime(FStartValue);
  }
  __finally
  {
    reSource->Lines->EndUpdate();
    reResult->Lines->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCopyClick(TObject *Sender)
{
  reSource->Lines = reResult->Lines;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  DragAcceptFiles(Handle, true);
#if __BORLANDC__ < 0x0560
  chkUseUTF8->Enabled = false;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormResize(TObject *Sender)
{
  pnlBottom->Height = ClientHeight / 2;
}
//---------------------------------------------------------------------------
