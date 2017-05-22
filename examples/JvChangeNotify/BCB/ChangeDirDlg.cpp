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

#include "ChangeDirDlg.h"
#include "FileCtrl.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "FileCtrl"
#pragma resource "*.dfm"
TfrmChangeNotificationDirDlg *frmChangeNotificationDirDlg;

bool TfrmChangeNotificationDirDlg::Execute(AnsiString& Directory, TJvChangeActions& Options, bool& IncludeSubDirs)
{
  TfrmChangeNotificationDirDlg *f;
  bool Result;

  f = new TfrmChangeNotificationDirDlg(Application);
  try
  {
    f->Edit1->Text = Directory;
    f->cbFileNames->Checked  = Options.Contains(caChangeFileName);
    f->cbAttributes->Checked = Options.Contains(caChangeAttributes);
    f->cbDirNames->Checked   = Options.Contains(caChangeDirName);
    f->cbSize->Checked       = Options.Contains(caChangeSize);
    f->cbWrite->Checked      = Options.Contains(caChangeLastWrite);
    f->cbSubTrees->Checked   = IncludeSubDirs;
    Result = (f->ShowModal() == mrOk);
    if (Result)
    {
      Directory = f->Edit1->Text;
      Options.Clear();
      if (f->cbFileNames->Checked)
        Options << caChangeFileName;
      if (f->cbAttributes->Checked)
        Options << caChangeAttributes;
      if (f->cbDirNames->Checked)
        Options << caChangeDirName;
      if (f->cbSize->Checked)
        Options << caChangeSize;
      if (f->cbWrite->Checked)
        Options << caChangeLastWrite;
      IncludeSubDirs = f->cbSubTrees->Checked;
    }
  }
  __finally
  {
    delete f;
  }
  return Result;
}


//---------------------------------------------------------------------------
__fastcall TfrmChangeNotificationDirDlg::TfrmChangeNotificationDirDlg(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmChangeNotificationDirDlg::Button1Click(TObject *Sender)
{
  AnsiString S;

  S = GetCurrentDir();
  TSelectDirOpts options;

  options << sdAllowCreate << sdPerformCreate << sdPrompt;

  if (SelectDirectory(S, options, 0))
    Edit1->Text = S;
}
//---------------------------------------------------------------------------
