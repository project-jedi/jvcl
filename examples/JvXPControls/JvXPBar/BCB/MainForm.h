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
#include "JvXPBar.hpp"
#include "JvXPContainer.hpp"
#include "JvXPCore.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------

typedef void __fastcall (__closure *TProcControl)(TControl* Control);

class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TSplitter *spltMain;
  TJvXPContainer *cntWinXPBar;
  TScrollBox *sbxWinXPBar;
  TJvXPContainer *JvXPContainer1;
  TJvXPBar *dxWinXPBar4;
  TTreeView *tvSelfView;
  TJvXPBar *JvXPBar1;
  TJvXPBar *dxWinXPBar3;
  TJvXPBar *dxWinXPBar2;
  TJvXPBar *dxWinXPBar1;
  TJvXPContainer *cntDetails;
  TLabel *lbWelcome;
  TButton *btnCollapseAll;
  TButton *btnExpandAll;
  TButton *btnToggleEnableMode;
  TButton *btnToggleVisibleMode;
  TCheckBox *chkGrouped;
  TImageList *imlWinXPBar;
  TActionList *aclWinXPBar;
  TAction *acConnectRemoteServer;
  TAction *acConnectLocalServer;
  TAction *acConnectAdministrator;
  TAction *acSettingsUsers;
  TAction *acSettingsStatistics;
  TAction *acSettingsDatabase;
  TAction *acSettingsDownloads;
  TAction *acSynchronizeUnknown;
  TAction *acSynchronizeWeb;
  TAction *acGettingStarted;
  TAction *acHelp;
  TAction *acHowDoI;
  TAction *acCommonQuestions;
  TImageList *ilOldButtons;
  TImageList *ilWhiteButtons;
  TImageList *ilRedButtons;
  TImageList *ilBlackButtons;
  void __fastcall acConnectRemoteServerExecute(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnCollapseAllClick(TObject *Sender);
  void __fastcall btnExpandAllClick(TObject *Sender);
  void __fastcall btnToggleEnableModeClick(TObject *Sender);
  void __fastcall btnToggleVisibleModeClick(TObject *Sender);
  void __fastcall chkGroupedClick(TObject *Sender);
private:	// User declarations
  void __fastcall DoGrouped(TControl* Control);
  void __fastcall DoExpandAll(TControl* Control);
  void __fastcall DoCollapseAll(TControl* Control);
  void __fastcall DoEnableToggle(TControl* Control);
  void __fastcall DoVisibleToggle(TControl* Control);
  void __fastcall IterateControls(TProcControl Proc);
  void __fastcall BuildStructure();
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
