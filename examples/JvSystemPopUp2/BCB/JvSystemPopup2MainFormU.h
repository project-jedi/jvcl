/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):
   korecek: translation from Delphi to BCB

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

#ifndef JvSystemPopup2MainFormUH
#define JvSystemPopup2MainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvSystemPopup.hpp"
#include <ActnList.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TJvSystemPopup2MainForm : public TForm
{
__published:	// IDE-managed Components
        TButton *btnSwitch;
        TRadioGroup *rgrPosition;
        TRadioGroup *rgrPositionInMenu;
        TEdit *edtAdd;
        TButton *btnAdd;
        TCheckBox *chbHideHideableItem;
        TPopupMenu *PopupMenu1;
        TMenuItem *est1;
        TMenuItem *Action11;
        TMenuItem *DisabledItem1;
        TMenuItem *ShortCut2;
        TMenuItem *Menu21;
        TMenuItem *ClickToSwitchIcon1;
        TMenuItem *HideableItem2;
        TActionList *ActionList1;
        TAction *actClickToCheck;
        TAction *actClickToSwitchIcon;
        TAction *actHideableItem;
        TAction *actShortCut;
        TPopupMenu *PopupMenu2;
        TMenuItem *Action21;
        TMenuItem *Radio21;
        TMenuItem *Radio31;
        TMenuItem *HideableItem1;
        TMenuItem *ShortCut1;
        TImageList *ImageList1;
        TJvSystemPopup *JvSystemPopup1;
        void __fastcall actClickToCheckExecute(TObject *Sender);
        void __fastcall actClickToSwitchIconExecute(TObject *Sender);
        void __fastcall actHideableItemExecute(TObject *Sender);
        void __fastcall actShortCutExecute(TObject *Sender);
        void __fastcall Action21Click(TObject *Sender);
        void __fastcall btnSwitchClick(TObject *Sender);
        void __fastcall chbHideHideableItemClick(TObject *Sender);
        void __fastcall rgrPositionClick(TObject *Sender);
        void __fastcall rgrPositionInMenuClick(TObject *Sender);
        void __fastcall btnAddClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TJvSystemPopup2MainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvSystemPopup2MainForm *JvSystemPopup2MainForm;
//---------------------------------------------------------------------------
#endif
