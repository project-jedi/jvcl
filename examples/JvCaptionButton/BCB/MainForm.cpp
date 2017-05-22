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
#pragma link "JvCaptionButton"
#pragma link "JvComponent"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvCaptionButton1Click(TObject *Sender)
{
  TPoint P;

  if (chkLogEvents->Checked)
  {
    GetCursorPos(&P);
    int x = P.x;
    int y = P.y;
    meEvents->Lines->Add(Format("Click at %d, %d", ARRAYOFCONST((x, y))));
  }
  chkDown->Checked = JvCaptionButton1->Down;
  // show "copyright" when showing special image
  if ((JvCaptionButton1->Standard == tsbNone) && (chkShowImage->Checked))
    MessageBox(Handle,"JvCaptionButton Demo. Copyright (c) 2003 by JEDI VCL; all rights reserved.",
      "About this demo...",MB_OK | MB_ICONINFORMATION);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvCaptionButton1MouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  if (chkLogEvents->Checked)
    meEvents->Lines->Add(Format("MouseDown at %d, %d", ARRAYOFCONST((X, Y))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvCaptionButton1MouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  if (chkLogEvents->Checked)
    meEvents->Lines->Add(Format("MouseUp at %d, %d", ARRAYOFCONST((X, Y))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvCaptionButton1MouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  if (chkLogEvents->Checked)
    meEvents->Lines->Add(Format("MouseMove at %d, %d", ARRAYOFCONST((X, Y))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkVisibleClick(TObject *Sender)
{
  JvCaptionButton1->Visible = chkVisible->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkToggleClick(TObject *Sender)
{
  JvCaptionButton1->Toggle = chkToggle->Checked;
  chkDown->Checked = JvCaptionButton1->Down;
  chkDown->Enabled = chkToggle->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbStandardChange(TObject *Sender)
{
  JvCaptionButton1->Standard = static_cast<TJvStandardButton>(cbStandard->ItemIndex);
  chkShowImage->Enabled = (JvCaptionButton1->Standard == tsbNone);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  udPosition->Position = JvCaptionButton1->Position;
  udPositionClick(Sender, btNext);
  cbStandard->ItemIndex = static_cast<int>(JvCaptionButton1->Standard);
  cbStandardChange(Sender);
  chkToggle->Checked = JvCaptionButton1->Toggle;
  chkVisible->Checked = JvCaptionButton1->Visible;
  edCaption->Text = JvCaptionButton1->Caption;
  edHint->Text = JvCaptionButton1->Hint;
  chkSysMenu->Checked = BorderIcons.Contains(biSystemMenu);
  chkMax->Checked = BorderIcons.Contains(biMaximize);
  chkMin->Checked = BorderIcons.Contains(biMinimize);
  chkHelp->Checked = BorderIcons.Contains(biHelp);
  chkEnabled->Checked = JvCaptionButton1->Enabled;
  cbBorderStyle->ItemIndex = static_cast<int>(BorderStyle);
  cbBorderStyleChange(Sender);
  chkShowHints->Checked = ShowHint;
  tbBtnWidth->Min = JvCaptionButton1->ButtonWidth;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::udPositionClick(TObject *Sender,
      TUDBtnType Button)
{
  JvCaptionButton1->Position = udPosition->Position;
  lblPos->Caption = Format("(%d)", ARRAYOFCONST((JvCaptionButton1->Position)));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkDownClick(TObject *Sender)
{
  JvCaptionButton1->Down = chkDown->Checked;
  chkDown->Checked = JvCaptionButton1->Down;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::edCaptionChange(TObject *Sender)
{
  JvCaptionButton1->Caption = edCaption->Text;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkSysMenuClick(TObject *Sender)
{
  if (chkSysMenu->Checked)
    BorderIcons << biSystemMenu;
  else
    BorderIcons >> biSystemMenu;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkMaxClick(TObject *Sender)
{
  if (chkMax->Checked)
    BorderIcons << biMaximize;
  else
    BorderIcons >> biMaximize;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkMinClick(TObject *Sender)
{
  if (chkMin->Checked)
    BorderIcons << biMinimize;
  else
    BorderIcons >> biMinimize;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkHelpClick(TObject *Sender)
{
  if (chkHelp->Checked)
    BorderIcons << biHelp;
  else
    BorderIcons >> biHelp;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbBorderStyleChange(TObject *Sender)
{
  BorderStyle = static_cast<TBorderStyle>(cbBorderStyle->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkShowHintsClick(TObject *Sender)
{
  ShowHint = chkShowHints->Checked;
  Application->ShowHint = ShowHint;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::edHintClick(TObject *Sender)
{
  JvCaptionButton1->Hint = edHint->Text;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkEnabledClick(TObject *Sender)
{
  JvCaptionButton1->Enabled = chkEnabled->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::tbBtnWidthChange(TObject *Sender)
{
  TPoint P;

  JvCaptionButton1->ButtonWidth = tbBtnWidth->Position;
  tbBtnWidth->Hint = Format("(%d)", ARRAYOFCONST((tbBtnWidth->Position)));
  GetCursorPos(&P);
  Application->ActivateHint(P);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnAddClick(TObject *Sender)
{
  int i,j;

  j = 0;
  // find next position
  for (i = 0; i < ComponentCount; i++)
    if ((dynamic_cast<TJvCaptionButton*>(Components[i])) &&
        (dynamic_cast<TJvCaptionButton*>(Components[i])->Position >= j))
      j = dynamic_cast<TJvCaptionButton*>(Components[i])->Position + 1;

  TJvCaptionButton* jcb = new TJvCaptionButton(this);

  jcb->Assign(JvCaptionButton1);
  jcb->Position = j;
  jcb->OnClick = JvCaptionButton1Click;
  jcb->OnMouseMove = JvCaptionButton1MouseMove;
  jcb->OnMouseUp = JvCaptionButton1MouseUp;
  jcb->OnMouseDown = JvCaptionButton1MouseDown;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnDeleteClick(TObject *Sender)
{
  int i;

  for (i = ComponentCount - 1; i >= 0; i--)
    if ((dynamic_cast<TJvCaptionButton*>(Components[i])) &&
        (Components[i] != JvCaptionButton1))
    {
      delete Components[i];
      return;
    }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkShowImageClick(TObject *Sender)
{
  if (chkShowImage->Checked)
  {
    JvCaptionButton1->Caption = "";
    JvCaptionButton1->ImageIndex = 3;
  }
  else
  {
    JvCaptionButton1->Caption = edCaption->Text;
    JvCaptionButton1->ImageIndex = -1;
  }
}
//---------------------------------------------------------------------------
