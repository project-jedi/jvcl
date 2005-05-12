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

#ifndef JvWindowsTitleMainFormUH
#define JvWindowsTitleMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <JvJCLUtils.hpp>
//---------------------------------------------------------------------------
class TJvWindowsTitleMainForm : public TForm
{
__published:	// IDE-managed Components
        TButton *Button1;
        TListBox *ListBox1;
        void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TJvWindowsTitleMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvWindowsTitleMainForm *JvWindowsTitleMainForm;
//---------------------------------------------------------------------------
#endif
