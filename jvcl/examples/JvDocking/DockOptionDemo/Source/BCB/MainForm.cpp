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
#define USEJVCL

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CSPIN"
#pragma link "JvComponent"
#pragma link "JvDockControlForm"
#pragma link "JvDockVIDStyle"

#ifdef USEJVCL
#pragma link "JvAppIniStorage"
#endif

#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TForm(Owner)
{
  ConjoinOption = NULL;
  TabOption = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormClose(TObject *Sender, TCloseAction &Action)
{
#ifdef USEJVCL
  JvAppStorage->FileName = ExtractFilePath(Application->ExeName) + "DockInfo.ini";
  SaveDockTreeToAppStorage(JvAppStorage);
#else
  SaveDockTreeToFile(ExtractFilePath(Application->ExeName) + "DockInfo.ini");
#endif
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ActivePanelFont_ButtonClick(TObject *Sender)
{
  FontDialog1->Font->Assign(ConjoinOption->ActiveFont);
  if (FontDialog1->Execute())
    ConjoinOption->ActiveFont = FontDialog1->Font;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ActivePanelStartColor_ButtonClick(
      TObject *Sender)
{
  ColorDialog1->Color = ConjoinOption->ActiveTitleStartColor;
  if (ColorDialog1->Execute())
    ConjoinOption->ActiveTitleStartColor = ColorDialog1->Color;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ActivePanelEndColor_ButtonClick(TObject *Sender)
{
  ColorDialog1->Color = ConjoinOption->ActiveTitleEndColor;
  if (ColorDialog1->Execute())
    ConjoinOption->ActiveTitleEndColor = ColorDialog1->Color;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::InactivePanelFont_ButtonClick(TObject *Sender)
{
  FontDialog1->Font->Assign(ConjoinOption->InactiveFont);
  if (FontDialog1->Execute())
    ConjoinOption->InactiveFont = FontDialog1->Font;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::InactivePanelStartColor_ButtonClick(
      TObject *Sender)
{
  ColorDialog1->Color = ConjoinOption->ActiveTitleStartColor;
  if (ColorDialog1->Execute())
    ConjoinOption->ActiveTitleStartColor = ColorDialog1->Color;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::InactivePanelEndColor_ButtonClick(
      TObject *Sender)
{
  ColorDialog1->Color = ConjoinOption->InactiveTitleEndColor;
  if (ColorDialog1->Execute())
    ConjoinOption->InactiveTitleEndColor = ColorDialog1->Color;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::GrabbersSize_SpinEditChange(TObject *Sender)
{
  ConjoinOption->GrabbersSize = GrabbersSize_SpinEdit->Value;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SplitterWidth_SpinEditChange(TObject *Sender)
{
  ConjoinOption->SplitterWidth = SplitterWidth_SpinEdit->Value;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::TextAlignment_ComboBoxChange(TObject *Sender)
{
  ConjoinOption->TextAlignment = static_cast<TAlignment>(TextAlignment_ComboBox->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::SystemInfo_CheckBoxClick(TObject *Sender)
{
  if (ConjoinOption != NULL)
    ConjoinOption->SystemInfo = SystemInfo_CheckBox->Checked;
  DoReadConjoinOption();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::TextEllipsis_CheckBoxClick(TObject *Sender)
{
  ConjoinOption->TextEllipsis = TextEllipsis_CheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ActiveTabFont_ButtonClick(TObject *Sender)
{
  FontDialog1->Font->Assign(TabOption->ActiveFont);
  if (FontDialog1->Execute())
    TabOption->ActiveFont = FontDialog1->Font;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::InactiveTabFont_ButtonClick(TObject *Sender)
{
  FontDialog1->Font->Assign(TabOption->InactiveFont);
  if (FontDialog1->Execute())
    TabOption->InactiveFont = FontDialog1->Font;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ActiveTabColor_ButtonClick(TObject *Sender)
{
  ColorDialog1->Color = TabOption->ActiveSheetColor;
  if (ColorDialog1->Execute())
    TabOption->ActiveSheetColor = ColorDialog1->Color;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::InctiveTabColor_ButtonClick(TObject *Sender)
{
  ColorDialog1->Color = TabOption->InactiveSheetColor;
  if (ColorDialog1->Execute())
    TabOption->InactiveSheetColor = ColorDialog1->Color;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::HotTrack_CheckBoxClick(TObject *Sender)
{
  TabOption->HotTrack = HotTrack_CheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::ShowIcon_CheckBoxClick(TObject *Sender)
{
  TabOption->ShowTabImages = ShowIcon_CheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::TabPosition_ComboBoxChange(TObject *Sender)
{
  switch (TabPosition_ComboBox->ItemIndex)
  {
    case 0:
      TabOption->TabPosition = tpTop;
      break;
    case 1:
      TabOption->TabPosition = tpBottom;
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::JvDockVIDStyle1SystemInfoChange(bool Value)
{
  SystemInfo_CheckBox->Checked = Value;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::TrackColor_ButtonClick(TObject *Sender)
{
  ColorDialog1->Color = TabOption->HotTrackColor;
  if (ColorDialog1->Execute())
    TabOption->HotTrackColor = ColorDialog1->Color;
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
#ifdef USEJVCL
  JvAppStorage = new TJvAppIniFileStorage(this);
#endif
  CreateDockWindow();
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::DoReadOption()
{
  DoReadConjoinOption();
  DoReadTabOption();
}

void __fastcall TfrmMain::DoReadConjoinOption()
{
  ConjoinOption = dynamic_cast<TJvDockVIDConjoinServerOption*>(JvDockVIDStyle1->ConjoinServerOption);

  GrabbersSize_SpinEdit->Value      = ConjoinOption->GrabbersSize;
  SplitterWidth_SpinEdit->Value     = ConjoinOption->SplitterWidth;
  TextAlignment_ComboBox->ItemIndex = static_cast<Integer>(ConjoinOption->TextAlignment);
  SystemInfo_CheckBox->Checked      = ConjoinOption->SystemInfo;
  TextEllipsis_CheckBox->Checked    = ConjoinOption->TextEllipsis;
}

void __fastcall TfrmMain::DoReadTabOption()
{
  TabOption = dynamic_cast<TJvDockVIDTabServerOption*>(JvDockVIDStyle1->TabServerOption);

  HotTrack_CheckBox->Checked = TabOption->HotTrack;
  ShowIcon_CheckBox->Checked = TabOption->ShowTabImages;
  switch(TabOption->TabPosition)
  {
    case tpTop:
      TabPosition_ComboBox->ItemIndex = 0;
      break;
    case tpBottom:
      TabPosition_ComboBox->ItemIndex = 1;
      break;
  }
}

void __fastcall TfrmMain::CreateDockWindow()
{
  int i;

  for (i = 0; i < DockFormCount; i++)
  {
    DockForms[i] = new TfrmDockWindow(NULL);
    DockForms[i]->Caption = DockForms[i]->Caption + IntToStr(i+1);
    DockForms[i]->Show();

    ImageList1->GetIcon(i, DockForms[i]->Icon);
  }

#ifdef USEJVCL
  JvAppStorage->FileName = ExtractFilePath(Application->ExeName) + "DockInfo.ini";
#else
  LoadDockTreeFromFile(ExtractFilePath(Application->ExeName) + "DockInfo.ini");
#endif
  DoReadOption();
}

