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
#include "JvComponent.hpp"
#include "JvExControls.hpp"
#include "JvXPButtons.hpp"
#include "JvXPCheckCtrls.hpp"
#include "JvXPContainer.hpp"
#include "JvXPCore.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TJvXPContainer *cntHeader;
  TJvXPToolButton *btnClose;
  TJvXPContainer *cntNetPanel;
  TLabel *lbBrowse;
  TShape *shpSeperator;
  TLabel *lbInternalPage;
  TLabel *lbWebEditor;
  TLabel *lbConfigure;
  TImage *imgConfigure;
  TJvXPContainer *cntNetHeader;
  TJvXPToolButton *btnLeft;
  TJvXPToolButton *btnRight;
  TJvXPButton *btnOK;
  TJvXPButton *btnCancel;
  TJvXPButton *btn1;
  TJvXPButton *btn2;
  TJvXPButton *btn4;
  TJvXPButton *btn3;
  TJvXPCheckbox *chkToggleEnable;
  TJvXPCheckbox *chkOfficeStyle;
  TJvXPCheckbox *chk1;
  TJvXPCheckbox *chk2;
  TJvXPToolButton *dxToolButton1;
  TJvXPToolButton *dxToolButton2;
  TJvXPToolButton *dxToolButton3;
  TJvXPToolButton *dxToolButton4;
  TJvXPToolButton *dxToolButton5;
  TJvXPStyleManager *styleOffice;
  TImageList *imlMain;
  TActionList *aclMain;
  TAction *acBtn1;
  TAction *acBtn2;
  TAction *acBtn3;
  TAction *acBtn4;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall acBtn1Execute(TObject *Sender);
  void __fastcall acBtn3Execute(TObject *Sender);
  void __fastcall btnCloseClick(TObject *Sender);
  void __fastcall chkOfficeStyleClick(TObject *Sender);
  void __fastcall chkToggleEnableClick(TObject *Sender);
  void __fastcall cntHeaderMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
  void __fastcall cntHeaderPaint(TObject *Sender, TRect &Rect,
          TCanvas *ACanvas, TFont *AFont);
  void __fastcall cntNetPanelPaint(TObject *Sender, TRect &Rect,
          TCanvas *ACanvas, TFont *AFont);
private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
