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

#ifndef TipOfDayMainFormUH
#define TipOfDayMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvBaseDlg.hpp"
#include "JvComponent.hpp"
#include "JvTipOfDay.hpp"
#include <Dialogs.hpp>
//---------------------------------------------------------------------------
class TTipOfDayMainForm : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TButton *Button1;
        TButton *Button2;
        TButton *Button3;
        TComboBox *cbStyle;
        TJvTipOfDay *JvTip;
        TOpenDialog *OpenDialog1;
        TSaveDialog *SaveDialog1;
        void __fastcall Button2Click(TObject *Sender);
        void __fastcall Button1Click(TObject *Sender);
        void __fastcall Button3Click(TObject *Sender);
private:        // User declarations
        AnsiString FileDirectory;
public:         // User declarations
        __fastcall TTipOfDayMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTipOfDayMainForm *TipOfDayMainForm;
//---------------------------------------------------------------------------
#endif
