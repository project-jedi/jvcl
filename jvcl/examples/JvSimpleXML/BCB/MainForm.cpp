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
#pragma link "JvComCtrls"
#pragma link "JvDialogs"
#pragma link "JvExComCtrls"
#pragma link "JvSimpleXml"
#pragma resource "*.dfm"
TfrmMain *frmMain;

void __fastcall TfrmMain::LoadFromFile(const AnsiString Filename)
{
  Screen->Cursor = crHourGlass;
  Enabled = false;
  try
  {
    JvTreeView1->Items->BeginUpdate();
    try
    {
      JvSimpleXml1->LoadFromFile(Filename);
      JvTreeView1->Items->Clear();
      ParseIntoTreeView(JvSimpleXml1->Root, JvTreeView1->Items->Add(NULL, ExtractFileName(Filename)));
    }
    __finally
    {
      JvTreeView1->Items->EndUpdate();
    }
    JvTreeView1->FullExpand();
  }
  __finally
  {
    Screen->Cursor = crDefault;
    Enabled = true;
  }

}

void __fastcall TfrmMain::ParseIntoTreeView(TJvSimpleXMLElem* AnXMLNode, TTreeNode* ATreeNode)
{
  if (AnXMLNode != NULL)
  {
    AnsiString S;
    if (AnXMLNode->Value != "")
      S = AnXMLNode->Name + "=" + AnXMLNode->Value;
    else
      S = AnXMLNode->Name;
    AnsiString T = "";
    for (int j = 0; j < AnXMLNode->Properties->Count; j++)
      T = T + " " + (*AnXMLNode->Properties)[j]->Name + "=\"" + (*AnXMLNode->Properties)[j]->Value + "\"";
    ATreeNode = JvTreeView1->Items->AddChild(ATreeNode, S + " (" + Trim(T) + ")");
    for (int i = 0; i < AnXMLNode->Items->Count; i++)
      ParseIntoTreeView((*AnXMLNode->Items)[i], ATreeNode);
  }
}

//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button1Click(TObject *Sender)
{
  JvOpenDialog1->FileName = edXMLFile->Text;
  if (JvOpenDialog1->Execute())
  {
    LoadFromFile(JvOpenDialog1->FileName);
    edXMLFile->Text = JvOpenDialog1->FileName;
  }
}
//---------------------------------------------------------------------------
