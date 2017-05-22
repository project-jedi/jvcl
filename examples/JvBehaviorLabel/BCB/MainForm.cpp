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
#pragma link "JvBehaviorLabel"
#pragma link "JvExStdCtrls"
#pragma resource "*.dfm"
TfrmMain *frmMain;

void __fastcall TfrmMain::DoCodeBreakStart(TObject* Sender)
{
  lblCodeBreaker->Caption = "BREAK THE CODE";
}

void __fastcall TfrmMain::DoCodeBreakStop(TObject* Sender)
{
  ShowMessage("Congratulations! You've hacked the system!");
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCodeBreakClick(TObject *Sender)
{
  Randomize;
  lblCodeBreaker->OnStart = NULL;
  lblCodeBreaker->OnStop = NULL;
  if (lblCodeBreaker->Caption == dynamic_cast<TJvLabelCodeBreaker* >(lblCodeBreaker->BehaviorOptions)->DecodedText)
    lblCodeBreaker->Caption = "x6/yhjSkhHHDski\"=90sd";
  // this might trigger the OnStart/OnStop events, so set to nil
  lblCodeBreaker->BehaviorOptions->Active = !lblCodeBreaker->BehaviorOptions->Active;
  lblCodeBreaker->OnStart = DoCodeBreakStart;
  lblCodeBreaker->OnStop = DoCodeBreakStop;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnAppearClick(TObject *Sender)
{
  lblAppearing->Alignment = taCenter;
  lblAppearing->BehaviorOptions->Active = !lblAppearing->BehaviorOptions->Active;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnBlinkClick(TObject *Sender)
{
  lblBlinking->BehaviorOptions->Active = !lblBlinking->BehaviorOptions->Active;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnBounceClick(TObject *Sender)
{
  lblBouncing->BehaviorOptions->Active = !lblBouncing->BehaviorOptions->Active;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnScrollClick(TObject *Sender)
{
  lblScrolling->BehaviorOptions->Active = !lblScrolling->BehaviorOptions->Active;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnSpecialClick(TObject *Sender)
{
  lblSpecial->BehaviorOptions->Active = !lblSpecial->BehaviorOptions->Active;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnTypeClick(TObject *Sender)
{
  lblTyping->BehaviorOptions->Active = !lblTyping->BehaviorOptions->Active;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnAllClick(TObject *Sender)
{
  for (int i= 0; i <  ControlCount; i++)
    if (Controls[i] != btnAll && dynamic_cast<TButton *>(Controls[i]))
      dynamic_cast<TButton*>(Controls[i])->Click();
}
//---------------------------------------------------------------------------
