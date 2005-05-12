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

#ifndef JvSystemPopupMainFormUH
#define JvSystemPopupMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvComponent.hpp"
#include "JvSystemPopup.hpp"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TJvSystemPopupMainForm : public TForm
{
__published:	// IDE-managed Components
        TRadioGroup *RadioGroup1;
        TJvSystemPopup *JvSystemPopup1;
        TPopupMenu *PopupMenu1;
        TMenuItem *firstone1;
        TMenuItem *secondone1;
        TMenuItem *thirdone1;
        TMenuItem *N1;
        TMenuItem *SubMenu11;
        TMenuItem *SubMenu12;
        TMenuItem *SubMenu21;
        TMenuItem *SubMenu31;
        TMenuItem *SubMenu22;
        TMenuItem *SubMenuItem11;
        TMenuItem *SubMenuItem21;
        TMenuItem *SubMenuItem31;
        TMenuItem *SubMenuItem41;
        TMenuItem *SubMenuItem51;
        TMenuItem *SubMenuItem1;
        TMenuItem *SubMenuItem71;
        TMenuItem *SubMenuItem81;
        TMenuItem *SubMenuItem91;
        void __fastcall firstone1Click(TObject *Sender);
        void __fastcall RadioGroup1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TJvSystemPopupMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvSystemPopupMainForm *JvSystemPopupMainForm;
//---------------------------------------------------------------------------
#endif
