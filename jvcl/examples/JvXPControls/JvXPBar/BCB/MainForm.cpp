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
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvComponent"
#pragma link "JvExControls"
#pragma link "JvXPBar"
#pragma link "JvXPContainer"
#pragma link "JvXPCore"
#pragma resource "*.dfm"

const AnsiString SClickEvent = "You have clicked the action \"%s\"...";

TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::DoGrouped(TControl *Control)
{
  if (dynamic_cast<TJvXPBar*>(Control))
    dynamic_cast<TJvXPBar*>(Control)->Grouped = chkGrouped->Checked;
}

void __fastcall TfrmMain::DoExpandAll(TControl *Control)
{
  if (dynamic_cast<TJvXPBar*>(Control))
    dynamic_cast<TJvXPBar*>(Control)->Collapsed = false;
}

void __fastcall TfrmMain::IterateControls(TProcControl Proc)
{
  for(int i = 0; i < ComponentCount - 1; i++)
    if (dynamic_cast<TControl*>(Components[i]))
      Proc(dynamic_cast<TControl*>(Components[i]));
}

void __fastcall TfrmMain::DoCollapseAll(TControl *Control)
{
  if (dynamic_cast<TJvXPBar*>(Control))
    dynamic_cast<TJvXPBar*>(Control)->Collapsed = true;
}

void __fastcall TfrmMain::DoEnableToggle(TControl *Control)
{
  if (dynamic_cast<TJvXPBar*>(Control))
    for(int i = 0; i < dynamic_cast<TJvXPBar*>(Control)->Items->Count; i++)
      if ((i % 2) == 1)
        dynamic_cast<TJvXPBar*>(Control)->Items->Items[i]->Enabled = !dynamic_cast<TJvXPBar*>(Control)->Items->Items[i]->Enabled;
}

void __fastcall TfrmMain::DoVisibleToggle(TControl *Control)
{
  if (dynamic_cast<TJvXPBar*>(Control))
    for (int i = 0; i < dynamic_cast<TJvXPBar*>(Control)->Items->Count; i++)
      if ((i % 2) == 1)
        dynamic_cast<TJvXPBar*>(Control)->Items->Items[i]->Visible = !dynamic_cast<TJvXPBar*>(Control)->Items->Items[i]->Visible;
}

void __fastcall TfrmMain::BuildStructure()
{
  tvSelfView->Items->Clear();
  for(int i = ComponentCount - 1; i >=0; i--)
    if (dynamic_cast<TJvXPBar*>(Components[i]))
    {
      TJvXPBar* ABar = dynamic_cast<TJvXPBar*>(Components[i]);
      TTreeNode* Parent = tvSelfView->Items->AddChild(NULL, ABar->Caption);
      if (ABar->ControlCount == 0)
      for(int j = 0; j < ABar->Items->Count; j++)
      {
        TTreeNode* Child = tvSelfView->Items->AddChild(Parent,ABar->Items->Items[j]->Caption);
        Child->ImageIndex = ABar->Items->Items[j]->ImageIndex;
        Child->SelectedIndex = Child->ImageIndex;
      }
    }
  tvSelfView->FullExpand();
}

void __fastcall TfrmMain::acConnectRemoteServerExecute(TObject *Sender)
{
  MessageBox(Application->Handle,
            (Format(SClickEvent, ARRAYOFCONST((dynamic_cast<TAction*>(Sender)->Name)))).c_str(),
            "Click",
            MB_ICONINFORMATION);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  cntDetails->Align = alClient;
  BuildStructure();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCollapseAllClick(TObject *Sender)
{
  IterateControls(DoCollapseAll);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnExpandAllClick(TObject *Sender)
{
  IterateControls(DoExpandAll);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnToggleEnableModeClick(TObject *Sender)
{
  IterateControls(DoEnableToggle);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnToggleVisibleModeClick(TObject *Sender)
{
  IterateControls(DoVisibleToggle);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkGroupedClick(TObject *Sender)
{
  IterateControls(DoGrouped);
  btnExpandAll->Enabled = !chkGrouped->Checked;
}
//---------------------------------------------------------------------------
