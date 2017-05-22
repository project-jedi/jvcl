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
#include "IniFiles.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "JvColorCombo"
#pragma link "JvCombobox"
#pragma link "JvExStdCtrls"
#pragma link "IniFiles"
#pragma resource "*.dfm"
TfrmMain *frmMain;


void __fastcall TfrmMain::LoadSettings()
{
  TColor CurColor;
  AnsiString S;
  TStringList *SL;
  int i;

  // load color name map and saved custom colors
  CurColor = JvColorComboBox1->ColorValue;
  try
  {
    S = ChangeFileExt(Application->ExeName, ".ini");
    if (FileExists(S))
    {
      SL = new TStringList();
      try
      {
        TIniFile *ini = new TIniFile(S);
        try
        {
          edNameTemplate->Text = ini->ReadString("Settings", "Custom Template", edNameTemplate->Text);
          ini->ReadSectionValues("Color Names", JvColorComboBox1->ColorNameMap);
          ini->ReadSection("Custom Colors", SL);
          for (i = 0; i< SL->Count; i++)
            JvColorComboBox1->AddColor(StringToColor(SL->Strings[i]),"");
        }
        __finally
        {
          delete ini;
        }
      }
      __finally
      {
        delete SL;
      }
    }
  }
  __finally
  {
    JvColorComboBox1->ColorValue = CurColor;
    cbDisplayStyleChange(NULL);
  }
}

void __fastcall TfrmMain::SaveSettings()
{
  int i;
  TList *AList;

  // save color name map and current custom colors
  JvColorComboBox1->Options >> coCustomColors;
  try
  {
    TIniFile *ini = new TIniFile(ChangeFileExt(Application->ExeName, ".ini"));
    try
    {
      AList = new TList;
      try
      {
        JvColorComboBox1->GetCustomColors(AList);
        ini->EraseSection("Custom Colors");
        for (i = 0; i < AList->Count; i++)
          ini->WriteString("Custom Colors", ColorToString(reinterpret_cast<TColor>(AList->Items[i])), "");
      }
      __finally
      {
        delete AList;
      }
      ini->WriteString("Settings", "Custom Template", edNameTemplate->Text);
      // save color map so users can translate them if they wish
      // no need to save the custom names since they are
      // set dynamically at load time (see JvColorComboBox1NewColor)
      ini->EraseSection("Color Names");
      TStrings *cnm = JvColorComboBox1->ColorNameMap;
      for (i = 0; i < cnm->Count; i++)
        ini->WriteString("Color Names", cnm->Names[i], cnm->Values[cnm->Names[i]]);
    }
    __finally
    {
      delete ini;
    }
  }
  __finally
  {
    cbDisplayStyleChange(NULL);
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
  LoadSettings();
  cbDisplayStyle->ItemIndex = 1;
  cbDisplayStyleChange(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnColorNamesClick(TObject *Sender)
{
  memInfo->Lines = JvColorComboBox1->ColorNameMap;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvColorComboBox1NewColor(TObject *Sender,
      TColor Color, AnsiString &DisplayName, bool &AllowAdd)
{
  // make sure the new color isn't already in the list
  AllowAdd = JvColorComboBox1->FindColor(Color) < 0;
  if (AllowAdd)
  {
    if (edNameTemplate->Text != "")
    // CustomColorCount isn't incremented until *after* this event has finished with AllowAdd = true, so add 1 here:
      DisplayName = Format(edNameTemplate->Text, ARRAYOFCONST((JvColorComboBox1->CustomColorCount + 1)));
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  SaveSettings();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::btnCustColorsClick(TObject *Sender)
{
  TList *AList;
  int i;

  AList = new TList();
  memInfo->Lines->Clear();
  try
  {
    // the returned TList contains a list of TColor items
    JvColorComboBox1->GetCustomColors(AList);
    for (i = 0; i < AList->Count; i++)
      memInfo->Lines->Add(ColorToString(reinterpret_cast<TColor>(AList->Items[i])));
  }
  __finally
  {
    delete AList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::cbDisplayStyleChange(TObject *Sender)
{
  TJvColorComboOptions O;

  O = JvColorComboBox1->Options;
  O >> coText >> coHex >> coRGB >> coCustomColors;
  if (chkAllowCustom->Checked)
    O << coCustomColors;
  switch(cbDisplayStyle->ItemIndex)
  {
    case 1:
      O << coText;
      break;
    case 2:
      O << coHex;
      break;
    case 3:
      O << coRGB;
      break;
  }
  JvColorComboBox1->Options = O;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvColorComboBox1Change(TObject *Sender)
{
  Caption = Format("Color: %s", ARRAYOFCONST((ColorToString(JvColorComboBox1->ColorValue))));
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::chkAllowCustomClick(TObject *Sender)
{
  if (chkAllowCustom->Checked)
    JvColorComboBox1->Options << coCustomColors;
  else
    JvColorComboBox1->Options >> coCustomColors;
}
//---------------------------------------------------------------------------
