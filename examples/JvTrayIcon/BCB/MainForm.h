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
#include "JvComponent.hpp"
#include "JvTrayIcon.hpp"
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TGroupBox *GroupBox1;
  TLabel *Label1;
  TCheckBox *chkActive;
  TEdit *edHint;
  TCheckBox *chkSnap;
  TCheckBox *chkTaskBar;
  TCheckBox *chkTaskList;
  TCheckBox *chkAutoHide;
  TCheckBox *chkRestoreClick;
  TCheckBox *chkRestoreDblClick;
  TCheckBox *chkMinClick;
  TCheckBox *chkMinDblClick;
  TCheckBox *chkPopUp;
  TButton *btnUpdate;
  TCheckBox *chkDropDown;
  TGroupBox *GroupBox2;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TEdit *edBalloonTitle;
  TEdit *edBalloonText;
  TButton *btnBalloon;
  TComboBox *cbBalloonType;
  TCheckBox *chkAutoHideIcon;
  TCheckBox *chkAnimated;
  TCheckBox *chkAutoRestore;
  TJvTrayIcon *JvTrayIcon1;
  TPopupMenu *popTrayIcon;
  TMenuItem *mnuShowHide;
  TTimer *RestoreTimer;
  TImageList *ImageList1;
  void __fastcall btnUpdateClick(TObject *Sender);
  void __fastcall mnuShowHideClick(TObject *Sender);
  void __fastcall chkRestoreClickClick(TObject *Sender);
  void __fastcall chkRestoreDblClickClick(TObject *Sender);
  void __fastcall chkMinClickClick(TObject *Sender);
  void __fastcall chkMinDblClickClick(TObject *Sender);
  void __fastcall RestoreTimerTimer(TObject *Sender);
  void __fastcall chkAutoRestoreClick(TObject *Sender);
  void __fastcall btnBalloonClick(TObject *Sender);
  void __fastcall chkActiveClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall chkAnimatedClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
