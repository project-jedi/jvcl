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
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvCheckTreeView"
#pragma link "JvComCtrls"
#pragma link "JvExComCtrls"
#pragma resource "*.dfm"
TfrmMain *frmMain;

const PChar cOnOff[2] = {"off", "on"};

void __fastcall TfrmMain::SetupNode(TTreeNode* Node)
{
  Node->ImageIndex = udImageIndex->Position;
  Node->SelectedIndex = Node->ImageIndex;
  if (udImageIndex->Position != -1)
  {
    if (udImageIndex->Position < udImageIndex->Max)
      udImageIndex->Position = udImageIndex->Position + 1;
    else
      udImageIndex->Position = 0;
  }
  switch (cbNodeType->ItemIndex)
  {
    case 1: // check box
      JvCheckTreeView1->CheckBox[Node] = True;
      JvCheckTreeView1->Checked[Node] = chkChecked->Checked;
      break;
    case 2: // radio item
      JvCheckTreeView1->RadioItem[Node] = True;
      JvCheckTreeView1->Checked[Node] = chkChecked->Checked;
      break;
  }
  Node->MakeVisible();
  JvCheckTreeView1->Selected = Node;
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  udImageIndex->Max = ilStandard->Count - 1;
  cbNodeType->ItemIndex = 1; // checkboxes
  cbNodeTypeChange(Sender);

  cbStyle->ItemIndex = 2; //cbsJVCL
  cbStyleChange(Sender);

  cbCascadeLevels->ItemIndex = 0;
  cbCascadeLevelsChange(Sender);

  cbCascadeOptions->ItemIndex = 2; // cascade all
  cbCascadeOptionsChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnAddClick(TObject *Sender)
{
  SetupNode(JvCheckTreeView1->Items->Add(JvCheckTreeView1->Selected, edText->Text));
  edText->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnAddChildClick(TObject *Sender)
{
  SetupNode(JvCheckTreeView1->Items->AddChild(JvCheckTreeView1->Selected, edText->Text));
  edText->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbNodeTypeChange(TObject *Sender)
{
  chkChecked->Enabled = (cbNodeType->ItemIndex > 0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuNormalClick(TObject *Sender)
{
  TTreeNode* N;

  N = reinterpret_cast<TTreeNode*>(popTree->Tag);
  if (N != NULL)
  {
    JvCheckTreeView1->CheckBox[N] = false;
    JvCheckTreeView1->RadioItem[N] = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuCheckBoxClick(TObject *Sender)
{
  TTreeNode* N;

  N = reinterpret_cast<TTreeNode*>(popTree->Tag);
  if (N != NULL)
    JvCheckTreeView1->CheckBox[N] = true;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuRadioItemClick(TObject *Sender)
{
  TTreeNode* N;

  N = reinterpret_cast<TTreeNode*>(popTree->Tag);
  if (N != NULL)
    JvCheckTreeView1->RadioItem[N] = true;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuCheckedClick(TObject *Sender)
{
  TTreeNode* N;
  TMenuItem* M;

  N = reinterpret_cast<TTreeNode*>(popTree->Tag);
  if (N != NULL)
  {
    M = dynamic_cast<TMenuItem*>(Sender);
    M->Checked = !M->Checked;
    JvCheckTreeView1->Checked[N] = M->Checked;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::mnuDeleteClick(TObject *Sender)
{
  TTreeNode* N;

  N = reinterpret_cast<TTreeNode*>(popTree->Tag);
  if (N != NULL)
    JvCheckTreeView1->Items->Delete(N);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkFlatClick(TObject *Sender)
{
  if (chkFlat->Checked)
    JvCheckTreeView1->StateImages = ilFlatChecks;
  else
    JvCheckTreeView1->StateImages = ilChecks;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnRandomClick(TObject *Sender)
{
  int i, aIndex;
  TTreeNode* N;

  randomize();
  JvCheckTreeView1->Items->BeginUpdate();
  JvCheckTreeView1->Items->Clear();
  for(i = 0; i <= 200; i++)
  {
    if (JvCheckTreeView1->Items->Count == 0)
      N = JvCheckTreeView1->Items->AddChild(NULL, edText->Text);
    else
    {
      aIndex = random(JvCheckTreeView1->Items->Count - 1);
      if (aIndex < JvCheckTreeView1->Items->Count % 2)
        N = JvCheckTreeView1->Items->AddChild(NULL, edText->Text);
      else
        N = JvCheckTreeView1->Items->AddChild(JvCheckTreeView1->Items->Item[aIndex], edText->Text);
    }
    N->ImageIndex = random(ilStandard->Count - 1);
    N->SelectedIndex = N->ImageIndex;
    switch (random(8))
    {
      case 2:
      case 3: // check box
        JvCheckTreeView1->CheckBox[N] = true;
        JvCheckTreeView1->Checked[N] = (random(2) != 0);
        break;
      case 5:
      case 6:
      case 7: // radio item
        JvCheckTreeView1->RadioItem[N] = true;
        JvCheckTreeView1->Checked[N] = (random(5) != 0);
    }
  }
  JvCheckTreeView1->FullExpand();
  JvCheckTreeView1->Items->EndUpdate();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbStyleChange(TObject *Sender)
{
  JvCheckTreeView1->CheckBoxOptions->Style = static_cast<TJvTVCheckBoxStyle>(cbStyle->ItemIndex);
  switch (JvCheckTreeView1->CheckBoxOptions->Style)
  {
    case cbsNative:
    case Jvchecktreeview::cbsNone:
      JvCheckTreeView1->StateImages = NULL;
      break;
    case cbsJVCL:
      if (chkFlat->Checked)
        JvCheckTreeView1->StateImages = ilFlatChecks;
      else
        JvCheckTreeView1->StateImages = ilChecks;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbCascadeLevelsChange(TObject *Sender)
{
  JvCheckTreeView1->CheckBoxOptions->CascadeLevels = cbCascadeLevels->ItemIndex - 1;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Clear1Click(TObject *Sender)
{
  JvCheckTreeView1->Items->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbCascadeOptionsChange(TObject *Sender)
{
  TJvTVCascadeOptions F;
  F.Clear();
  switch (cbCascadeOptions->ItemIndex)
  {
    case 0:
      F << poOnCheck;
      break;
    case 1:
      F << poOnUnCheck;
      break;
    case 2:
      F << poOnCheck << poOnUnCheck;
      break;
  }
  JvCheckTreeView1->CheckBoxOptions->CascadeOptions = F;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvCheckTreeView1ContextPopup(TObject *Sender,
      TPoint &MousePos, bool &Handled)
{
  TTreeNode* N;

  N = JvCheckTreeView1->GetNodeAt(MousePos.x, MousePos.y);
  if (N != NULL)
  {
    popTree->Tag = reinterpret_cast<int>(N);
    mnuChecked->Enabled = true;
    mnuChecked->Checked = JvCheckTreeView1->Checked[N];
    if (JvCheckTreeView1->CheckBox[N])
      mnuCheckBox->Checked = true;
    else
    if (JvCheckTreeView1->RadioItem[N])
      mnuRadioItem->Checked = true;
    else
    {
      mnuChecked->Enabled = false;
      mnuNormal->Checked = true;
      mnuChecked->Checked = false;
    }
  }
  else
  {
    popTree->Tag = 0;
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvCheckTreeView1Toggling(TObject *Sender,
      TTreeNode *Node, bool &AllowChange)
{
  StatusBar1->Panels->Items[0]->Text = Format("Node %s about to be toggled %s", ARRAYOFCONST((Node->Text, cOnOff[!JvCheckTreeView1->Checked[Node]])));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvCheckTreeView1Toggled(TObject *Sender,
      TTreeNode *Node)
{
  StatusBar1->Panels->Items[0]->Text = Format("Node %s toggled %s", ARRAYOFCONST((Node->Text, cOnOff[JvCheckTreeView1->Checked[Node]])));
}
//---------------------------------------------------------------------------
