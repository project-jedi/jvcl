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

#include "MainSpeedForm.h"
#include <math.h>
#include <JvJVCLUtils.hpp>
#include <JvSimpleXML.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvExGrids"
#pragma link "JvStringGrid"
#pragma link "JvJVCLUtils"
#pragma link "JvSimpleXML"
#pragma resource "*.dfm"
TfrmSpeedTest *frmSpeedTest;

AnsiString StringFromFile(const AnsiString FileName, __int64& FileSize)
{
  TFileStream* fs = new TFileStream(FileName, fmOpenRead);
  AnsiString Result;
  try
  {
    FileSize = fs->Size;
    Result.SetLength(FileSize);
    if (fs->Size > 0)
      fs->Read(Result.c_str(), FileSize);
  }
  __finally
  {
    delete fs;
  }
  return Result;
}

void __fastcall TfrmSpeedTest::Test(bool Decode)
{
  WaitCursor();
  for (int i = 0; i < memFiles->Lines->Count; i++)
    if (FileExists((*memFiles->Lines)[i]))
    {
      __int64 FStringLength;
      AnsiString S = StringFromFile((*memFiles->Lines)[i], FStringLength);
      Cardinal FStartTime = GetTickCount();
      if (Decode)
        SimpleXMLDecode(S, false);
      else
        SimpleXMLEncode(S);
      AddInfo((*memFiles->Lines)[i], Decode, FStringLength, GetTickCount() - FStartTime, sgResults->Cells[0][1] != "");
    }
}

void __fastcall TfrmSpeedTest::AddInfo(const AnsiString FileName, bool Decode,
  __int64 FileSize, Cardinal MSecs, bool AddRow)
{
  const char* cDecoded[2] = {"Encoded", "Decoded"};

  Extended Speed, KBSpeed;

  if (MSecs == 0)
  {
    Speed = FileSize;
    KBSpeed = FileSize / 1024.0;
  }
  else
  {
    Speed = FileSize / static_cast<Extended>(MSecs);
    KBSpeed = (FileSize / 1024.0) / (MSecs / 1000.0);
  }
  if (AddRow)
    sgResults->RowCount = sgResults->RowCount + 1;
  int i = sgResults->RowCount - 1;
  sgResults->Cells[0][i] = ExtractFileName(FileName);
  sgResults->Cells[1][i] = IntToStr(FileSize);
  sgResults->Cells[2][i] = cDecoded[Decode];
  sgResults->Cells[3][i] = IntToStr(MSecs);
  sgResults->Cells[4][i] = IntToStr(static_cast<__int64>(floor(Speed)));
  sgResults->Cells[5][i] = Format("%.*f", ARRAYOFCONST((1 + (KBSpeed < 1)?1:0, KBSpeed)));
}

void __fastcall TfrmSpeedTest::WMDropFiles(TWMDropFiles& Message)
{
  char buf[MAX_PATH-1];

  WaitCursor();
  int Count = DragQueryFile(reinterpret_cast<void*>(Message.Drop), 0xFFFFFFFF, NULL, 0);
  try
  {
    for (int i = 0; i < Count; i++)
    {
      DragQueryFile(reinterpret_cast<void*>(Message.Drop), i, buf, sizeof(buf));
      if (FileExists(buf))
        memFiles->Lines->Add(buf);
    }
  }
  __finally
  {
    DragFinish(reinterpret_cast<void*>(Message.Drop));
  }
}

//---------------------------------------------------------------------------
__fastcall TfrmSpeedTest::TfrmSpeedTest(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmSpeedTest::btnEncodeClick(TObject *Sender)
{
  Test(false);  
}
//---------------------------------------------------------------------------
void __fastcall TfrmSpeedTest::btnDecodeClick(TObject *Sender)
{
  Test(true);  
}
//---------------------------------------------------------------------------
void __fastcall TfrmSpeedTest::FormCreate(TObject *Sender)
{
  DragAcceptFiles(Handle, true);
  sgResults->Cells[0][0] = "Filename";
  sgResults->Cells[1][0] = "Size (bytes)";
  sgResults->Cells[2][0] = "Action";
  sgResults->Cells[3][0] = "Time (msecs)";
  sgResults->Cells[4][0] = "byte/msec";
  sgResults->Cells[5][0] = "kB/sec";
}
//---------------------------------------------------------------------------
void __fastcall TfrmSpeedTest::FormDestroy(TObject *Sender)
{
  DragAcceptFiles(Handle, false);
}
//---------------------------------------------------------------------------
void __fastcall TfrmSpeedTest::FormResize(TObject *Sender)
{
  sgResults->DefaultColWidth = (sgResults->ClientWidth - sgResults->ColCount + 1) / sgResults->ColCount;
}
//---------------------------------------------------------------------------
void __fastcall TfrmSpeedTest::sgResultsCaptionClick(TJvStringGrid *Sender,
      int AColumn, int ARow)
{
  const TJvSortType SortType[6] = {stClassic, stNumeric, stClassic, stNumeric, stNumeric, stNumeric};

  sgResults->SortGrid(AColumn, FDescending, false, SortType[AColumn]);
  FDescending = !FDescending;
}
//---------------------------------------------------------------------------
void __fastcall TfrmSpeedTest::btnClearClick(TObject *Sender)
{
  sgResults->RowCount = 2;
  sgResults->Rows[1]->Clear();
}
//---------------------------------------------------------------------------
