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
#include "JvBalloonHint.hpp"
#include "JvComponent.hpp"
#include "JvExStdCtrls.hpp"
#include "JvListComb.hpp"
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TPanel *pnlDefaultValues;
  TLabel *lblDefaultHeader;
  TEdit *edtDefaultHeader;
  TRadioGroup *rgrDefaultIcon;
  TGroupBox *grbDefaultImageIndex;
  TJvImageListBox *ilbDefaultImageIndex;
  TRadioGroup *rgrDefaultBalloonPosition;
  TStaticText *sttDefaultValues;
  TPanel *pnlBalloon;
  TLabel *lblMessage;
  TLabel *lblHeader;
  TLabel *lblVisibleTime;
  TLabel *lblAnchorCtrl;
  TGroupBox *grbOptions;
  TCheckBox *chbUseDefaultHeader;
  TCheckBox *chbUseDefaultIcon;
  TCheckBox *chbUseDefaultImageIndex;
  TCheckBox *chbShowCloseBtn;
  TCheckBox *chbCustomAnimation;
  TCheckBox *chbO_PlaySound;
  TButton *btnLaunch;
  TGroupBox *grbCustomAnimation;
  TLabel *lblCustomAnimationTime;
  TEdit *edtCustomAnimationTime;
  TRadioGroup *rgrCustomAnimationStyle;
  TEdit *edtHeader;
  TEdit *edtVisibleTime;
  TComboBox *cmbAnchorCtrl;
  TMemo *memMessage;
  TStaticText *sttBalloon;
  TPanel *pnlApplicationHint;
  TCheckBox *chbShowHeaderInHint;
  TCheckBox *chbShowIconInHint;
  TCheckBox *chbPlaySound;
  TCheckBox *chbUseBalloonAsApplicationHint;
  TStaticText *sttApplicationHint;
  TJvBalloonHint *JvBalloonHint1;
  TImageList *ImageList2;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall edtDefaultHeaderChange(TObject *Sender);
  void __fastcall chbShowHeaderInHintClick(TObject *Sender);
  void __fastcall chbShowIconInHintClick(TObject *Sender);
  void __fastcall chbPlaySoundClick(TObject *Sender);
  void __fastcall rgrDefaultIconClick(TObject *Sender);
  void __fastcall ilbDefaultImageIndexClick(TObject *Sender);
  void __fastcall rgrDefaultBalloonPositionClick(TObject *Sender);
  void __fastcall btnLaunchClick(TObject *Sender);
  void __fastcall rgrCustomAnimationStyleClick(TObject *Sender);
  void __fastcall edtCustomAnimationTimeChange(TObject *Sender);
  void __fastcall chbUseDefaultHeaderClick(TObject *Sender);
  void __fastcall chbUseDefaultIconClick(TObject *Sender);
  void __fastcall chbUseDefaultImageIndexClick(TObject *Sender);
  void __fastcall chbShowCloseBtnClick(TObject *Sender);
  void __fastcall chbCustomAnimationClick(TObject *Sender);
  void __fastcall chbO_PlaySoundClick(TObject *Sender);
  void __fastcall chbUseBalloonAsApplicationHintClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  void InitValues();
  void FillAnchors(TStrings* Strings);

  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
