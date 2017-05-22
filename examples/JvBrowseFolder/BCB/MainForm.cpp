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
#pragma link "JvBaseDlg"
#pragma link "JvBrowseFolder"
#pragma link "JvComponent"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button1Click(TObject *Sender)
{
  //Set options
  JvBrowseFolder1->Title = Edit4->Text;
  JvBrowseFolder1->Options.Clear();
  if (CheckBox1->Checked)
    JvBrowseFolder1->Options << odNewDialogStyle;
  if (CheckBox2->Checked)
    JvBrowseFolder1->Options << odBrowseForComputer;
  if (CheckBox3->Checked)
    JvBrowseFolder1->Options << odOnlyDirectory;
  if (CheckBox4->Checked)
    JvBrowseFolder1->Options << odOnlyPrinters;
  if (CheckBox5->Checked)
    JvBrowseFolder1->Options << odIncludeFiles;
  if (CheckBox6->Checked)
    JvBrowseFolder1->Options << odIncludeUrls;
  if (CheckBox7->Checked)
    JvBrowseFolder1->Options << odEditBox;
  if (CheckBox8->Checked)
    JvBrowseFolder1->Options << odShareable;

  switch(ComboBox1->ItemIndex)
  {
    case 1:  JvBrowseFolder1->RootDirectory = fdControlPanel;
             break;
    case 2:  JvBrowseFolder1->RootDirectory = fdRecycleBin;
             break;
    case 3:  JvBrowseFolder1->RootDirectory = fdDesktop;
             break;
    case 4:  JvBrowseFolder1->RootDirectory = fdDesktopDirectory;
             break;
    case 5:  JvBrowseFolder1->RootDirectory = fdMyComputer;
             break;
    case 6:  JvBrowseFolder1->RootDirectory = fdFonts;
             break;
    case 7:  JvBrowseFolder1->RootDirectory = fdNetHood;
             break;
    case 8:  JvBrowseFolder1->RootDirectory = fdNetwork;
             break;
    case 9:  JvBrowseFolder1->RootDirectory = fdPersonal;
             break;
    case 10: JvBrowseFolder1->RootDirectory = fdPrinters;
             break;
    case 11: JvBrowseFolder1->RootDirectory = fdPrograms;
             break;
    case 12: JvBrowseFolder1->RootDirectory = fdRecent;
             break;
    case 13: JvBrowseFolder1->RootDirectory = fdSendTo;
             break;
    case 14: JvBrowseFolder1->RootDirectory = fdStartMenu;
             break;
    case 15: JvBrowseFolder1->RootDirectory = fdStartup;
             break;
    case 16: JvBrowseFolder1->RootDirectory = fdTemplates;
             break;
    default: JvBrowseFolder1->RootDirectory = fdRootFolder;
  }

  //Execute and get back the directory
  if (JvBrowseFolder1->Execute())
  {
    Edit1->Text = JvBrowseFolder1->Directory;
    Edit2->Text = JvBrowseFolder1->DisplayName;

    //If it's a printer or anything else other than a directory, you have to use
    //JvBrowseFolder1.LastPidl
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvBrowseFolder1Change(TObject *Sender,
      const AnsiString Directory)
{
  Edit3->Text = Directory;
}
//---------------------------------------------------------------------------
