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
#pragma link "JvBalloonHint"
#pragma link "JvComponent"
#pragma link "JvExStdCtrls"
#pragma link "JvListComb"
#pragma resource "*.dfm"
TfrmMain *frmMain;

void TfrmMain::InitValues()
{
  chbShowHeaderInHint->Checked            = JvBalloonHint1->ApplicationHintOptions.Contains(ahShowHeaderInHint);
  chbShowIconInHint->Checked              = JvBalloonHint1->ApplicationHintOptions.Contains(ahShowIconInHint);
  chbPlaySound->Checked                   = JvBalloonHint1->ApplicationHintOptions.Contains(ahPlaySound);
  chbUseBalloonAsApplicationHint->Checked = JvBalloonHint1->UseBalloonAsApplicationHint;
  rgrDefaultIcon->ItemIndex               = static_cast<Integer>(JvBalloonHint1->DefaultIcon);
  rgrDefaultBalloonPosition->ItemIndex    = static_cast<Integer>(JvBalloonHint1->DefaultBalloonPosition);

  chbUseDefaultHeader->Checked       =  JvBalloonHint1->Options.Contains(boUseDefaultHeader);
  chbUseDefaultIcon->Checked         =  JvBalloonHint1->Options.Contains(boUseDefaultIcon);
  chbUseDefaultImageIndex->Checked   =  JvBalloonHint1->Options.Contains(boUseDefaultImageIndex);
  chbShowCloseBtn->Checked           =  JvBalloonHint1->Options.Contains(boShowCloseBtn);
  chbCustomAnimation->Checked        =  JvBalloonHint1->Options.Contains(boCustomAnimation);
  chbO_PlaySound->Checked            =  JvBalloonHint1->Options.Contains(boPlaySound);
  rgrCustomAnimationStyle->ItemIndex = static_cast<Integer>(JvBalloonHint1->CustomAnimationStyle);
  edtCustomAnimationTime->Text       = IntToStr(JvBalloonHint1->CustomAnimationTime);

  edtHeader->Text        = "Header";
  edtDefaultHeader->Text = JvBalloonHint1->DefaultHeader;
  memMessage->Text       = "Message";
  edtVisibleTime->Text   = "10000";

  FillAnchors(cmbAnchorCtrl->Items);
  cmbAnchorCtrl->ItemIndex = random(cmbAnchorCtrl->Items->Count);
}

void AddCtrl(TControl* ACtrl, TStrings* Strings)
{
  if (ACtrl->Name != "")
    Strings->AddObject(ACtrl->Name, ACtrl);
}

void AddControls(TWinControl* AWinControl, TStrings* Strings)
{
  for(int i=0; i < AWinControl->ControlCount; i++)
  {
    if (dynamic_cast<TWinControl*>(AWinControl->Controls[i]))
      AddControls(dynamic_cast<TWinControl*>(AWinControl->Controls[i]), Strings);
    else
      AddCtrl(AWinControl->Controls[i], Strings);
    AddCtrl(AWinControl, Strings);
  }
}

void TfrmMain::FillAnchors(TStrings* Strings)
{
  Strings->BeginUpdate();
  try
  {
    Strings->Clear();
    AddControls(this, Strings);
  }
  __finally
  {
    Strings->EndUpdate();
  }
}



//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  /* xp problem, with sizable forms;
    This form is sizable so you can test what happens if you
    size the window and a balloon is showing
  */
  ClientHeight = 505;
  ClientWidth = 665;

  Randomize();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormShow(TObject *Sender)
{
  InitValues();  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::edtDefaultHeaderChange(TObject *Sender)
{
  JvBalloonHint1->DefaultHeader = edtDefaultHeader->Text;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbShowHeaderInHintClick(TObject *Sender)
{
  if (chbShowHeaderInHint->Checked)
    JvBalloonHint1->ApplicationHintOptions << ahShowHeaderInHint;
  else
    JvBalloonHint1->ApplicationHintOptions >> ahShowHeaderInHint;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbShowIconInHintClick(TObject *Sender)
{
  if (chbShowIconInHint->Checked)
    JvBalloonHint1->ApplicationHintOptions << ahShowIconInHint;
  else
    JvBalloonHint1->ApplicationHintOptions >> ahShowIconInHint;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbPlaySoundClick(TObject *Sender)
{
  if (chbPlaySound->Checked)
    JvBalloonHint1->ApplicationHintOptions << ahPlaySound;
  else
    JvBalloonHint1->ApplicationHintOptions >> ahPlaySound;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::rgrDefaultIconClick(TObject *Sender)
{
  if (rgrDefaultIcon->ItemIndex >= 0)
    JvBalloonHint1->DefaultIcon = static_cast<TJvIconKind>(rgrDefaultIcon->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ilbDefaultImageIndexClick(TObject *Sender)
{
  JvBalloonHint1->DefaultImageIndex = ilbDefaultImageIndex->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::rgrDefaultBalloonPositionClick(TObject *Sender)
{
  if (rgrDefaultBalloonPosition->ItemIndex >= 0)
    JvBalloonHint1->DefaultBalloonPosition = static_cast<TJvBalloonPosition>(rgrDefaultBalloonPosition->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnLaunchClick(TObject *Sender)
{
  TJvIconKind LIcon;
  int LImageIndex;
  int LVisibleTime;
  TControl * LCtrl;

  if (rgrDefaultIcon->ItemIndex >= 0)
    LIcon = static_cast<TJvIconKind>(rgrDefaultIcon->ItemIndex);
  else
    LIcon = ikNone;
  LImageIndex = -1;

  if (LIcon == ikCustom)
  {
    LImageIndex = ilbDefaultImageIndex->ItemIndex;
    if (LImageIndex < 0)
      LIcon = ikNone;
  }

  if (cmbAnchorCtrl->ItemIndex >= 0)
    LCtrl = dynamic_cast<TControl*>(cmbAnchorCtrl->Items->Objects[cmbAnchorCtrl->ItemIndex]);
  else
    LCtrl = NULL;

  LVisibleTime = StrToIntDef(edtVisibleTime->Text, 5000);

  switch(LIcon)
  {
    case ikNone:
      JvBalloonHint1->ActivateHint(LCtrl, memMessage->Text, edtHeader->Text, LVisibleTime);
    case ikCustom:
      JvBalloonHint1->ActivateHint(LCtrl, memMessage->Text, LImageIndex, edtHeader->Text,
        LVisibleTime);
    default:
      JvBalloonHint1->ActivateHint(LCtrl, memMessage->Text, LIcon, edtHeader->Text, LVisibleTime);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::rgrCustomAnimationStyleClick(TObject *Sender)
{
  if (rgrCustomAnimationStyle->ItemIndex >= 0)
    JvBalloonHint1->CustomAnimationStyle = static_cast<TJvAnimationStyle>(rgrCustomAnimationStyle->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::edtCustomAnimationTimeChange(TObject *Sender)
{
  JvBalloonHint1->CustomAnimationTime = StrToIntDef(edtCustomAnimationTime->Text, 0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbUseDefaultHeaderClick(TObject *Sender)
{
  if (chbUseDefaultHeader->Checked)
    JvBalloonHint1->Options << boUseDefaultHeader;
  else
    JvBalloonHint1->Options >> boUseDefaultHeader;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbUseDefaultIconClick(TObject *Sender)
{
  if (chbUseDefaultIcon->Checked)
    JvBalloonHint1->Options << boUseDefaultIcon;
  else
    JvBalloonHint1->Options >> boUseDefaultIcon;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbUseDefaultImageIndexClick(TObject *Sender)
{
  if (chbUseDefaultImageIndex->Checked)
    JvBalloonHint1->Options << boUseDefaultImageIndex;
  else
    JvBalloonHint1->Options >> boUseDefaultImageIndex;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbShowCloseBtnClick(TObject *Sender)
{
  if (chbShowCloseBtn->Checked)
    JvBalloonHint1->Options << boShowCloseBtn;
  else
    JvBalloonHint1->Options >> boShowCloseBtn;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbCustomAnimationClick(TObject *Sender)
{
  if (chbCustomAnimation->Checked)
    JvBalloonHint1->Options << boCustomAnimation;
  else
    JvBalloonHint1->Options >> boCustomAnimation;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbO_PlaySoundClick(TObject *Sender)
{
  if (chbO_PlaySound->Checked)
    JvBalloonHint1->Options << boPlaySound;
  else
    JvBalloonHint1->Options >> boPlaySound;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chbUseBalloonAsApplicationHintClick(
      TObject *Sender)
{
  JvBalloonHint1->UseBalloonAsApplicationHint = chbUseBalloonAsApplicationHint->Checked;
}
//---------------------------------------------------------------------------
