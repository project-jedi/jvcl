/******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

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

#ifndef frmMemoEditH
#define frmMemoEditH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
bool __fastcall TMemoEditFrm_Edit(TStrings* Lines, TDateTime ADate, TIcon* Icon=NULL);
//---------------------------------------------------------------------------
class TMemoEditFrm : public TForm
{
__published:	// IDE-managed Components
        TPanel *Panel1;
        TButton *btnOK;
        TButton *btnCancel;
        TRichEdit *reLines;
        TPopupMenu *popMemo;
        TMenuItem *Load1;
        TMenuItem *Save1;
        TMenuItem *N1;
        TMenuItem *Cut1;
        TMenuItem *Copy1;
        TMenuItem *Paste1;
        TMenuItem *N2;
        TMenuItem *Selectall1;
        void __fastcall reLinesKeyUp(TObject *Sender, WORD &Key,
          TShiftState Shift);
        void __fastcall Load1Click(TObject *Sender);
        void __fastcall Save1Click(TObject *Sender);
        void __fastcall Cut1Click(TObject *Sender);
        void __fastcall Copy1Click(TObject *Sender);
        void __fastcall Paste1Click(TObject *Sender);
        void __fastcall Selectall1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TMemoEditFrm(TComponent* Owner);
};
//---------------------------------------------------------------------------
#endif
