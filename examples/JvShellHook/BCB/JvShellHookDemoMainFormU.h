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

#ifndef JvShellHookDemoMainFormUH
#define JvShellHookDemoMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <JvShellHook.hpp>
//---------------------------------------------------------------------------
/* comment out this define if you have no such compiler version */
#define COMPILER6_UP

class TJvShellHookDemoMainForm : public TForm
{
__published:    // IDE-managed Components
        TLabel *Label1;
        TButton *btnClear;
        TCheckBox *chkActive;
        TCheckBox *chkNoRedraw;
        TListView *lvMessages;
        void __fastcall chkActiveClick(TObject *Sender);
        void __fastcall btnClearClick(TObject *Sender);
        void __fastcall lvMessagesResize(TObject *Sender);
private:  // User declarations
        void __fastcall DoShellMessage(TObject * Sender, TMessage &Message);
        AnsiString GetAppCommand(int LParam);

public:   // User declarations
        __fastcall TJvShellHookDemoMainForm(TComponent* Owner);
        __fastcall ~TJvShellHookDemoMainForm(void);
        TJvShellHook *SH;
};
//---------------------------------------------------------------------------
extern PACKAGE TJvShellHookDemoMainForm *JvShellHookDemoMainForm;
//---------------------------------------------------------------------------
#endif
