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
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
#include "ChangeDirDlg.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvChangeNotify"
#pragma link "JvComponent"
#pragma resource "*.dfm"
TfrmMain *frmMain;

void __fastcall TfrmMain::WMGetMinMaxInfo(TWMGetMinMaxInfo &Msg)
{
  Msg.MinMaxInfo->ptMinTrackSize.x = 392;
  Msg.MinMaxInfo->ptMinTrackSize.y = 295;
  Msg.Result = 0;
}

void __fastcall TfrmMain::ResetCaptions(bool Invert)
{
  const AnsiString aCap[2] = {"TJvChangeNotification demo", "Checking..."};

  if (Invert)
    Caption = aCap[JvChangeNotify1->Active?0:1];
  else
    Caption = aCap[JvChangeNotify1->Active?1:0];
  Application->Title = Caption;
}

AnsiString OptionsToStr(TJvChangeActions &Options)
{
  AnsiString Result = "";
  if (Options.Contains(caChangeFileName))
    Result += "Rename Files,";
  if (Options.Contains(caChangeDirName))
    Result += "Rename Folders,";
  if (Options.Contains(caChangeAttributes))
    Result += "Change Attributes,";
  if (Options.Contains(caChangeSize))
    Result += "Change Size,";
  if (Options.Contains(caChangeLastWrite))
    Result += "Change Content,";
  if (Options.Contains(caChangeSecurity))
    Result += "Change Security,";
  if (Result.Length() > 0)
  {
    Result.SetLength(Result.Length() - 1);
    Result = "(" + Result + ")";
  }
  return Result;
}

void __fastcall TfrmMain::DeleteItem(TListItem* li)
{
  if (li == NULL)
    return;
  if (li->Data != NULL)
    JvChangeNotify1->Notifications->Delete(li->Index);
  li->Delete();
}

void __fastcall TfrmMain::EditItem(TListItem* li)
{
  AnsiString ADirectory;
  TJvChangeActions AOptions;
  bool AIncludeSubDirs;

  if ((li == NULL) || (li->Data == NULL))
  {
    ADirectory = GetCurrentDir();
    AIncludeSubDirs = true;
    AOptions.Clear();
    AOptions << caChangeFileName << caChangeDirName;
  }
  else
  {
    TJvChangeItem* jci = static_cast<TJvChangeItem*>(li->Data);
    ADirectory = jci->Directory;
    AIncludeSubDirs = jci->IncludeSubTrees;
    AOptions = jci->Actions;
  }

  if (TfrmChangeNotificationDirDlg::Execute(ADirectory, AOptions, AIncludeSubDirs))
  {
    if (li == NULL)
    {
      li = ListView1->Items->Add();
      li->Caption = ADirectory;
      if (AIncludeSubDirs && (Win32Platform == VER_PLATFORM_WIN32_NT))
        li->SubItems->Add("Yes");
      else
        li->SubItems->Add("No");
      li->SubItems->Add(OptionsToStr(AOptions));
    }
    else
    {
      li->Caption = ADirectory;
      if (AIncludeSubDirs && (Win32Platform == VER_PLATFORM_WIN32_NT))
        li->SubItems->Strings[0] = "Yes";
      else
        li->SubItems->Strings[0] = "No";
      li->SubItems->Strings[1]= OptionsToStr(AOptions);
    }
    if (li->Data == NULL)
      li->Data = JvChangeNotify1->Notifications->Add();

    TJvChangeItem* jci = static_cast<TJvChangeItem*>(li->Data);
    jci->IncludeSubTrees = (AIncludeSubDirs && (Win32Platform == VER_PLATFORM_WIN32_NT));
    jci->Directory = ADirectory;
    jci->Actions = AOptions;
  }
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  JvChangeNotify1->Active = false;  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnDeleteClick(TObject *Sender)
{
  DeleteItem(ListView1->Selected);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnAddClick(TObject *Sender)
{
  EditItem(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnStartClick(TObject *Sender)
{
  bool b;

  if (JvChangeNotify1->Notifications->Count == 0)
  {
    ShowMessage("No notifications to monitor!");
    btnStart->Down = false;
    return;
  }

  b = btnStart->Down;
  btnAdd->Enabled = !b;
  btnDelete->Enabled = !b;
  ResetCaptions(true);
  // do this *after* setting buttons
  JvChangeNotify1->Active = b;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnClearClick(TObject *Sender)
{
  ListBox2->Clear();
  ResetCaptions(false);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvChangeNotify1ChangeNotify(TObject *Sender,
      AnsiString Dir, TJvChangeActions Actions)
{
  Application->Title = Format("Change in %s (%s)", ARRAYOFCONST((Dir, ActionsToString(Actions))));
  ListBox2->Items->Add(Application->Title);
  FlashWindow(frmMain->Handle, true);
  MessageBeep(static_cast<DWORD>(-1));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ListView1DblClick(TObject *Sender)
{
  EditItem(ListView1->Selected);
}
//---------------------------------------------------------------------------
