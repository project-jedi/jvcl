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

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvExComCtrls.hpp"
#include "JvMenus.hpp"
#include "JvToolBar.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TPanel *pnlPopup;
  TJvToolBar *jtbMenus;
  TButton *btnAddItems;
  TButton *btnChangeCaption;
  TPanel *pnlMarginPopup;
  TMemo *memExplanation;
  TJvMainMenu *jmnMain;
  TMenuItem *File1;
  TMenuItem *Try1;
  TMenuItem *N1;
  TMenuItem *Exit1;
  TMenuItem *Other1;
  TMenuItem *Sub11;
  TMenuItem *Hello1;
  TMenuItem *Plaf1;
  TMenuItem *Yop1;
  TMenuItem *Nice1;
  TMenuItem *Checked1;
  TMenuItem *N11;
  TMenuItem *Radio11;
  TMenuItem *Radio21;
  TMenuItem *Radio31;
  TMenuItem *N2;
  TMenuItem *N3;
  TMenuItem *SUb1;
  TMenuItem *SubAgain1;
  TImageList *imlImages;
  TJvPopupMenu *jpmPopup;
  TMenuItem *Popup11;
  TMenuItem *Popup21;
  TMenuItem *Checked2;
  TMenuItem *PopupSub1;
  TMenuItem *Yop2;
  TMenuItem *CheckedInSub1;
  TMenuItem *Yip1;
  TMenuItem *AfterPSub1;
  TJvPopupMenu *jpmMarginPopup;
  TMenuItem *Test1;
  TMenuItem *Testagain1;
  TJvStandardMenuItemPainter *jipMarginPainter;
  void __fastcall Exit1Click(TObject *Sender);
  void __fastcall Try1Click(TObject *Sender);
  void __fastcall btnAddItemsClick(TObject *Sender);
  void __fastcall btnChangeCaptionClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
