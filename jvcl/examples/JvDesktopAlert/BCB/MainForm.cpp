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
#include "JvDesktopAlert.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvAppIniStorage"
#pragma link "JvAppStorage"
#pragma link "JvComponent"
#pragma link "JvFormPlacement"
#pragma link "JvDesktopAlert"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
  FCount = 0;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  cbLocation->ItemIndex = 3;  
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnBrowseClick(TObject *Sender)
{
  if (OpenPictureDialog1->Execute())
    Image1->Picture->LoadFromFile(OpenPictureDialog1->FileName);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Clickme1Click(TObject *Sender)
{
  ShowMessage("You clicked the menu!");
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnPreviewClick(TObject *Sender)
{
  int i,j;
  TJvDesktopAlert* DA;
  TJvDesktopAlertOptions FOptions;

  for (i = 0; i < udWindowCount->Position; i++)
  {
    DA = new TJvDesktopAlert(this);
    DA->Images = imlCustBtns;
    DA->HeaderText = Format("%s (%d)", ARRAYOFCONST((edHeader->Text, FCount)));
    DA->MessageText = edMessage->Text;
    DA->Image = Image1->Picture;
    DA->OnMessageClick = DoMessageClick;
    DA->OnShow = DoAlertShow;
    DA->OnClose = DoAlertClose;
    FOptions = TJvDesktopAlertOptions() << daoCanFade;
    DA->Options = FOptions;
    DA->Location->AlwaysResetPosition = false;
    DA->Location->Position = static_cast<TJvDesktopAlertPosition>(cbLocation->ItemIndex);
    DA->Location->Width = StrToIntDef(edWidth->Text,0);
    DA->Location->Height = StrToIntDef(edHeight->Text,0);
    if (DA->Location->Position == dapCustom)
    {
      DA->Location->Left = random(Screen->Width - 200);
      DA->Location->Top =  random(Screen->Height - 100);
    }
    DA->FadeInTime = udFadeIn->Position;
    DA->WaitTime  = udWait->Position; //  + Random(WaitTime);
    DA->FadeOutTime = udFadeOut->Position;
    if (chkClickable->Checked)
      FOptions << daoCanClick;
    if (chkMovable->Checked)
      FOptions << daoCanMove;
    if (chkClose->Checked)
      FOptions << daoCanClose;
    DA->Options = FOptions;
    if (chkShowDropDown->Checked)
      DA->DropDownMenu = PopupMenu1;
    for (j = 0; j < udButtons->Position; j++)
    {
      TJvDesktopAlertButtonItem* btn = DA->Buttons->Add();

      btn->ImageIndex = random(imlCustBtns->Count);
      btn->Tag = j;
      btn->OnClick = DoButtonClick;
    }
    DA->Execute();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::DoMessageClick(TObject* Sender)
{
  ShowMessage("You clicked the message!");
}

void __fastcall TfrmMain::DoButtonClick(TObject* Sender)
{
  ShowMessageFmt("You clicked button %d!", ARRAYOFCONST((dynamic_cast<TControl*>(Sender)->Tag)));
}

void __fastcall TfrmMain::DoAlertShow(TObject* Sender)
{
  FCount++;
  Caption = Format("JvDesktopAlert Demo: showing %d alerts", ARRAYOFCONST((FCount)));
}

void _fastcall TfrmMain::DoAlertClose(TObject* Sender)
{                   
  FCount--;
  Caption = Format("JvDesktopAlert Demo: showing %d alerts",ARRAYOFCONST((FCount)));
}


