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
// $Id$
//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "CSPIN.h"
#include "JvComponent.hpp"
#include "JvDockControlForm.hpp"
#include "JvDockVIDStyle.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include "DockWindowForm.h"

#ifdef USEJVCL
#include <JvAppIniStorage.hpp>
#endif
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TJvDockServer *lbDockServer1;
  TJvDockVIDStyle *JvDockVIDStyle1;
  TFontDialog *FontDialog1;
  TColorDialog *ColorDialog1;
  TImageList *ImageList1;
  TPageControl *PageControl1;
  TTabSheet *TabSheet1;
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TGroupBox *GroupBox2;
  TButton *ActivePanelFont_Button;
  TButton *ActivePanelStartColor_Button;
  TButton *ActivePanelEndColor_Button;
  TGroupBox *GroupBox3;
  TButton *InactivePanelFont_Button;
  TButton *InactivePanelStartColor_Button;
  TButton *InactivePanelEndColor_Button;
  TComboBox *TextAlignment_ComboBox;
  TCheckBox *SystemInfo_CheckBox;
  TCheckBox *TextEllipsis_CheckBox;
  TTabSheet *TabSheet2;
  TLabel *Label6;
  TGroupBox *GroupBox5;
  TButton *ActiveTabFont_Button;
  TButton *ActiveTabColor_Button;
  TGroupBox *GroupBox6;
  TButton *InactiveTabFont_Button;
  TButton *InctiveTabColor_Button;
  TComboBox *TabPosition_ComboBox;
  TCheckBox *HotTrack_CheckBox;
  TCheckBox *ShowIcon_CheckBox;
  TButton *TrackColor_Button;
  TCSpinEdit *GrabbersSize_SpinEdit;
  TCSpinEdit *SplitterWidth_SpinEdit;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall ActivePanelFont_ButtonClick(TObject *Sender);
  void __fastcall ActivePanelStartColor_ButtonClick(TObject *Sender);
  void __fastcall ActivePanelEndColor_ButtonClick(TObject *Sender);
  void __fastcall InactivePanelFont_ButtonClick(TObject *Sender);
  void __fastcall InactivePanelStartColor_ButtonClick(TObject *Sender);
  void __fastcall InactivePanelEndColor_ButtonClick(TObject *Sender);
  void __fastcall GrabbersSize_SpinEditChange(TObject *Sender);
  void __fastcall SplitterWidth_SpinEditChange(TObject *Sender);
  void __fastcall TextAlignment_ComboBoxChange(TObject *Sender);
  void __fastcall SystemInfo_CheckBoxClick(TObject *Sender);
  void __fastcall TextEllipsis_CheckBoxClick(TObject *Sender);
  void __fastcall ActiveTabFont_ButtonClick(TObject *Sender);
  void __fastcall InactiveTabFont_ButtonClick(TObject *Sender);
  void __fastcall ActiveTabColor_ButtonClick(TObject *Sender);
  void __fastcall InctiveTabColor_ButtonClick(TObject *Sender);
  void __fastcall HotTrack_CheckBoxClick(TObject *Sender);
  void __fastcall ShowIcon_CheckBoxClick(TObject *Sender);
  void __fastcall TabPosition_ComboBoxChange(TObject *Sender);
  void __fastcall JvDockVIDStyle1SystemInfoChange(bool Value);
  void __fastcall TrackColor_ButtonClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
#ifdef USEJVCL
  TJvAppIniFileStorage* JvAppStorage;
#endif
  void __fastcall DoReadOption();
  void __fastcall DoReadConjoinOption();
  void __fastcall DoReadTabOption();

  static const int DockFormCount = 10;

  TfrmDockWindow* DockForms[DockFormCount];
  TJvDockVIDConjoinServerOption* ConjoinOption;
  TJvDockVIDTabServerOption* TabOption;

public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
  void __fastcall CreateDockWindow();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
