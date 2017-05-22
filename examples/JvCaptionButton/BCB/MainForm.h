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

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvCaptionButton.hpp"
#include "JvComponent.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TUpDown *udPosition;
  TTrackBar *tbBtnWidth;
  TMemo *meEvents;
  TLabel *lblPos;
  TLabel *Label7;
  TLabel *Label6;
  TLabel *Label5;
  TLabel *Label4;
  TLabel *Label3;
  TLabel *Label2;
  TLabel *Label1;
  TJvCaptionButton *JvCaptionButton1;
  TImageList *ImageList1;
  TGroupBox *gbButtons;
  TCheckBox *chkSysMenu;
  TCheckBox *chkMax;
  TCheckBox *chkMin;
  TCheckBox *chkHelp;
  TEdit *edHint;
  TEdit *edCaption;
  TCheckBox *chkVisible;
  TCheckBox *chkToggle;
  TCheckBox *chkShowImage;
  TCheckBox *chkShowHints;
  TCheckBox *chkLogEvents;
  TCheckBox *chkEnabled;
  TCheckBox *chkDown;
  TComboBox *cbStandard;
  TComboBox *cbBorderStyle;
  TButton *btnDelete;
  TButton *btnAdd;
  TBevel *Bevel1;
  void __fastcall JvCaptionButton1Click(TObject *Sender);
  void __fastcall JvCaptionButton1MouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall JvCaptionButton1MouseUp(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall JvCaptionButton1MouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
  void __fastcall chkVisibleClick(TObject *Sender);
  void __fastcall chkToggleClick(TObject *Sender);
  void __fastcall cbStandardChange(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall udPositionClick(TObject *Sender, TUDBtnType Button);
  void __fastcall chkDownClick(TObject *Sender);
  void __fastcall edCaptionChange(TObject *Sender);
  void __fastcall chkSysMenuClick(TObject *Sender);
  void __fastcall chkMaxClick(TObject *Sender);
  void __fastcall chkMinClick(TObject *Sender);
  void __fastcall chkHelpClick(TObject *Sender);
  void __fastcall cbBorderStyleChange(TObject *Sender);
  void __fastcall chkShowHintsClick(TObject *Sender);
  void __fastcall edHintClick(TObject *Sender);
  void __fastcall chkEnabledClick(TObject *Sender);
  void __fastcall tbBtnWidthChange(TObject *Sender);
  void __fastcall btnAddClick(TObject *Sender);
  void __fastcall btnDeleteClick(TObject *Sender);
  void __fastcall chkShowImageClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
