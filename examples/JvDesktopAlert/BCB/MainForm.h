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
#include "JvAppIniStorage.hpp"
#include "JvAppStorage.hpp"
#include "JvComponent.hpp"
#include "JvFormPlacement.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
#include <Graphics.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TLabel *Label5;
  TLabel *Label6;
  TLabel *Label7;
  TLabel *Label8;
  TLabel *Label9;
  TLabel *Label12;
  TLabel *Label13;
  TLabel *Label14;
  TButton *btnPreview;
  TEdit *edHeader;
  TEdit *edMessage;
  TButton *btnBrowse;
  TPanel *Panel1;
  TImage *Image1;
  TEdit *Edit1;
  TUpDown *udButtons;
  TCheckBox *chkClickable;
  TCheckBox *chkMovable;
  TCheckBox *chkClose;
  TEdit *Edit2;
  TUpDown *udWindowCount;
  TCheckBox *chkShowDropDown;
  TEdit *Edit3;
  TUpDown *udFadeIn;
  TEdit *Edit4;
  TUpDown *udWait;
  TEdit *Edit5;
  TUpDown *udFadeOut;
  TComboBox *cbLocation;
  TGroupBox *GroupBox1;
  TLabel *Label10;
  TLabel *Label11;
  TEdit *edWidth;
  TEdit *edHeight;
  TEdit *Edit6;
  TUpDown *udEndSteps;
  TEdit *Edit7;
  TUpDown *udStartSteps;
  TComboBox *cmbStyle;
  TPopupMenu *PopupMenu1;
  TMenuItem *Examplemenu1;
  TMenuItem *N1;
  TMenuItem *Clickme1;
  TImageList *imlCustBtns;
  TOpenPictureDialog *OpenPictureDialog1;
  TJvFormStorage *JvFormStorage1;
  TJvAppIniFileStorage *JvAppIniFileStorage1;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnBrowseClick(TObject *Sender);
  void __fastcall Clickme1Click(TObject *Sender);
  void __fastcall btnPreviewClick(TObject *Sender);
private:	// User declarations
  int FCount;
  void __fastcall DoButtonClick(TObject* Sender);
  void __fastcall DoMessageClick(TObject* Sender);
  void __fastcall DoAlertClose(TObject* Sender);
  void __fastcall DoAlertShow(TObject* Sender);
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
