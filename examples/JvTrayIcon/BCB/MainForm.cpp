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
#pragma link "JvTrayIcon"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnUpdateClick(TObject *Sender)
{
  TTrayVisibilities Options;

//  JvTrayIcon1->

  JvTrayIcon1->Active = false;
  JvTrayIcon1->Animated = chkAnimated->Checked;
  JvTrayIcon1->IconIndex = -1;
  JvTrayIcon1->Hint = edHint->Text;
  JvTrayIcon1->Snap = chkSnap->Checked;
  if (chkPopUp->Checked)
    JvTrayIcon1->PopupMenu = popTrayIcon;
  else
    JvTrayIcon1->PopupMenu = NULL;
  if (chkDropDown->Checked)
    JvTrayIcon1->DropDownMenu = popTrayIcon;
  else
    JvTrayIcon1->DropDownMenu = NULL;
  Options.Clear();
  if (chkTaskBar->Checked)
    Options << tvVisibleTaskBar;
  if (chkTaskList->Checked)
    Options << tvVisibleTaskList;
  if (chkAutoHide->Checked)
    Options << tvAutoHide;
  if (chkAutoHideIcon->Checked)
    Options << tvAutoHideIcon;
  if (chkRestoreClick->Checked && chkRestoreClick->Enabled)
    Options << tvRestoreClick;
  if (chkRestoreDblClick->Checked && chkRestoreDblClick->Enabled)
    Options << tvRestoreDbClick;
//    if (chkMinClick->Checked && chkMinClick->Enabled)
//      Options << tvMinimizeClick;
//    if (chkMinDblClick->Checked && chkMinDblClick->Enabled)
//      Options << tvMinimizeDbClick;
  JvTrayIcon1->Visibility = Options;
  JvTrayIcon1->Active = chkActive->Checked;
  btnBalloon->Enabled = JvTrayIcon1->Active;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuShowHideClick(TObject *Sender)
{
  if (IsWindowVisible(Handle))
    JvTrayIcon1->HideApplication();
  else
    JvTrayIcon1->ShowApplication();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkRestoreClickClick(TObject *Sender)
{
  chkRestoreDblClick->Enabled = !chkRestoreClick->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkRestoreDblClickClick(TObject *Sender)
{
  chkRestoreClick->Enabled = !chkRestoreDblClick->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkMinClickClick(TObject *Sender)
{
  chkMinDblClick->Enabled = !chkMinClick->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkMinDblClickClick(TObject *Sender)
{
  chkMinClick->Enabled = !chkMinDblClick->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::RestoreTimerTimer(TObject *Sender)
{
  if (!IsWindowVisible(Handle))
    JvTrayIcon1->ShowApplication();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkAutoRestoreClick(TObject *Sender)
{
  RestoreTimer->Enabled = !chkAutoRestore->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnBalloonClick(TObject *Sender)
{
  JvTrayIcon1->BalloonHint(edBalloonTitle->Text,
                           edBalloonText->Text,
                           static_cast<TBalloonType>(cbBalloonType->ItemIndex),
                           5000,
                           true);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkActiveClick(TObject *Sender)
{
  JvTrayIcon1->Active = chkActive->Checked;
  btnBalloon->Enabled = JvTrayIcon1->Active;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormShow(TObject *Sender)
{
  cbBalloonType->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkAnimatedClick(TObject *Sender)
{
  JvTrayIcon1->Animated = chkAnimated->Checked;
}
//---------------------------------------------------------------------------
