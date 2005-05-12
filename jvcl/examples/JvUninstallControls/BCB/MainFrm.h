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

#ifndef MainFrmH
#define MainFrmH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvUninstallControls.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TPanel *Panel2;
        TJvUninstallComboBox *JvUninstallComboBox1;
        TCheckBox *chkComboShowAll;
        TCheckBox *chkComboSorted;
        TMemo *memComboInfo;
        TCheckBox *chkComboEmptyValues;
        TPanel *Panel1;
        TJvUninstallListBox *JvUninstallListBox1;
        TCheckBox *chkListShowAll;
        TCheckBox *chkListSorted;
        TCheckBox *chkShowEmpty;
        TMemo *memListInfo;
        TSplitter *Splitter1;
        void __fastcall JvUninstallListBox1Click(TObject *Sender);
        void __fastcall chkListShowAllClick(TObject *Sender);
        void __fastcall chkListSortedClick(TObject *Sender);
        void __fastcall chkShowEmptyClick(TObject *Sender);
        void __fastcall JvUninstallComboBox1Click(TObject *Sender);
        void __fastcall chkComboShowAllClick(TObject *Sender);
        void __fastcall chkComboSortedClick(TObject *Sender);
        void __fastcall chkComboEmptyValuesClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
